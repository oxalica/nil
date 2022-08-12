//! Liveness check of names.
//! It locates uncessary or inaccessible bindings and expressions, based on name resolution.
//!
//! Our goals are,
//! - Applicatable.
//!   Removing ALL unused items will work and be semantically identical.
//! - Closed.
//!   If there is an unused binding, it either has no references,
//!   or each of its reference is included by some reported unused binding.
//! - Self-contained.
//!   Warnings inside a sub-expression should not be influenced by if the sub-expression itself
//!   is reachable from root.
//!   So one unreachable binding should not cause ALL deep bindings to be spammed.
//!
//! We now identifies,
//! - Unused `let` bindings.
//! - Unused `with` expressions.
//! - Unnecessary `rec` attrsets.
use super::{BindingKey, BindingValue, DefDatabase, Expr, ExprId, NameDefId, ResolveResult};
use crate::{Diagnostic, DiagnosticKind, FileId};
use la_arena::ArenaMap;
use rowan::ast::AstNode;
use rowan::TextRange;
use std::collections::HashMap;
use std::sync::Arc;
use syntax::ast;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct LivenessCheckResult {
    name_defs: Box<[NameDefId]>,
    withs: Box<[ExprId]>,
    rec_attrsets: Box<[ExprId]>,
}

impl LivenessCheckResult {
    pub fn to_diagnostics<'a>(
        &'a self,
        db: &dyn DefDatabase,
        file: FileId,
    ) -> impl Iterator<Item = Diagnostic> + 'a {
        let source_map = db.source_map(file);
        let root = db.parse(file).syntax_node();
        let mut diags = Vec::new();
        diags.extend(self.name_defs.iter().map(|&def| {
            let ptr = source_map.name_def_node(def).unwrap();
            Diagnostic::new(ptr.text_range(), DiagnosticKind::UnusedBinding)
        }));
        diags.extend(self.withs.iter().map(|&expr| {
            let ptr = source_map.expr_node(expr).unwrap();
            let node = ast::With::cast(ptr.to_node(&root)).unwrap();
            let header_range = match (node.with_token(), node.semicolon_token()) {
                (Some(start), Some(end)) => start.text_range().cover(end.text_range()),
                _ => TextRange::empty(ptr.text_range().start()),
            };
            Diagnostic::new(header_range, DiagnosticKind::UnusedWith)
        }));
        diags.extend(self.rec_attrsets.iter().map(|&expr| {
            let ptr = source_map.expr_node(expr).unwrap();
            let node = ast::AttrSet::cast(ptr.to_node(&root)).unwrap();
            let range = node.rec_token().map_or_else(
                || TextRange::empty(ptr.text_range().start()),
                |tok| tok.text_range(),
            );
            Diagnostic::new(range, DiagnosticKind::UnusedRec)
        }));
        diags.into_iter()
    }
}

pub(crate) fn liveness_check_query(
    db: &dyn DefDatabase,
    file_id: FileId,
) -> Arc<LivenessCheckResult> {
    let module = db.module(file_id);
    let scopes = db.scopes(file_id);

    // Unused let-bindings are eagerly collected into this.
    let mut unused_defs = Vec::new();

    // For rec-attrset check.
    let mut visited_defs: ArenaMap<NameDefId, ()> = ArenaMap::default();
    // For traversal of let-in bindings.
    let mut visited_def_rhs = ArenaMap::default();
    let mut visited_withs = ArenaMap::default();
    let mut stack = vec![module.entry_expr];

    while !stack.is_empty() {
        // N.B. This should be dropped in every loop.
        // Or it will make this whole check cost quadratic time!
        let mut discovered_let_rhs: HashMap<NameDefId, ExprId> = HashMap::new();

        // Traverse all reachable Exprs from roots.
        while let Some(expr) = stack.pop() {
            match &module[expr] {
                Expr::Reference(name) => match scopes.resolve_name(expr, name) {
                    Some(ResolveResult::NameDef(def)) => {
                        visited_defs.insert(def, ());
                        if let Some(rhs) = discovered_let_rhs.remove(&def) {
                            // Dedup inherit-from expressions.
                            if visited_def_rhs.get(rhs).is_none() {
                                visited_def_rhs.insert(rhs, ());
                                stack.push(rhs);
                            }
                        }
                    }
                    Some(ResolveResult::WithExprs(exprs)) => {
                        for expr in exprs {
                            visited_withs.insert(expr, ());
                        }
                    }
                    Some(ResolveResult::Builtin(_)) | None => {}
                },
                Expr::LetIn(bindings, body) => {
                    // Pre-mark all let-binding.
                    bindings.walk_child_defs(|def, value| {
                        let (BindingValue::Inherit(e)
                        | BindingValue::InheritFrom(e)
                        | BindingValue::Expr(e)) = *value;
                        discovered_let_rhs.insert(def, e);
                    });

                    // Traverse the body as root.
                    stack.push(*body);
                }
                e => e.walk_child_exprs(|e| stack.push(e)),
            }
        }

        // Record unused let-bindings and continue traversal inside them,
        // as if themselves are reachable.
        unused_defs.extend(discovered_let_rhs.iter().map(|(&def, _)| def));
        stack.extend(discovered_let_rhs.iter().map(|(_, &rhs)| rhs));
    }

    // Finally, collect unused lambda parameters, "with" expressions and "rec" attrsets.
    // It's intended to not reporting them if they are only referenced by some unused let-bindings.
    // Unused let-bindings may be caused by unfinished codes or typos,
    // situation may be changed when user tries to fixing them.
    let mut unused_withs = Vec::new();
    let mut unused_recs = Vec::new();
    for (expr, kind) in module.exprs() {
        match kind {
            &Expr::Lambda(Some(param), Some(_), _) if visited_defs.get(param).is_none() => {
                unused_defs.push(param);
            }
            &Expr::With(..) if visited_withs.get(expr).is_none() => {
                unused_withs.push(expr);
            }
            Expr::RecAttrset(bindings)
                if bindings.statics.iter().all(|(key, _)| match key {
                    &BindingKey::NameDef(def) => visited_defs.get(def).is_none(),
                    BindingKey::Name(_) => unreachable!(),
                }) =>
            {
                unused_recs.push(expr);
            }
            _ => {}
        }
    }

    Arc::new(LivenessCheckResult {
        name_defs: unused_defs.into(),
        withs: unused_withs.into(),
        rec_attrsets: unused_recs.into(),
    })
}

#[cfg(test)]
mod tests {
    use crate::tests::TestDB;
    use crate::DefDatabase;

    #[track_caller]
    fn check(fixture: &str) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.files().len(), 1);
        let file = f.files()[0];
        assert_eq!(db.module(file).diagnostics(), Vec::new(), "Lower error");
        let expect = f.markers().iter().map(|p| p.pos).collect::<Vec<_>>();
        let mut got = db
            .liveness_check(file)
            .to_diagnostics(&db, file)
            .map(|diag| diag.range.start())
            .collect::<Vec<_>>();
        got.sort_unstable();
        assert_eq!(got, expect);
    }

    #[test]
    fn let_in() {
        // Transitive references.
        check("let   a = b; b = 1;   c = a; in c");
        check("let   a = b; b = 1; $0c = a; in a");
        // Shadowing.
        check("let $0a = 1; in let a = 2; in a");
        check("let $0a = 1; in let a = a/*self*/; in a");
        // Not shadowing.
        check("let   a = 1; in let inherit a; in a");
        // Mutual references.
        check("let $0a = b; $1b = a; in 1");
    }

    #[test]
    fn lambda() {
        check("a: { b }: $0c@{}: 0");
    }

    #[test]
    fn with() {
        check("a: $0with 1; a");
        check("a: with 1; with 2; b");
    }

    #[test]
    fn rec_attrset() {
        check("$0rec { a = 1; b = 1; c = 1; }");
        check("$0rec { }");
        check("  rec { a = 1; b = a; c = 1; }");
        check("  rec { a = b; b = a; }");
        check("  rec { a = a/*self*/; }");
    }

    #[test]
    fn let_and_rec() {
        check("let   a = 1; in $0rec { inherit a; }");
        check("let $0a = 1; in   rec { a = a/*self*/; }");
        check("let $0a =   rec { a = a/*self*/; }; in 1");
        check("let $0a = $1rec { inherit a; }; in 1");
        check("rec { a = 1; b = let $0c = a; in 1; }");
    }

    #[test]
    fn nested_let() {
        check("let   a = let   b = 1; in b; in a");
        check("let $0a = let   b = 1; in b; in 1");
        check("let $0a = let $1b = 1; in 1; in 1");
        check("let   a = let $0b = 1; in 1; in a");

        check("let   a = 1;   b = let   c = a; in c; in b");
        check("let $0a = 1;   b = let $1c = a; in 1; in b");
        check("let $0a = 1; $1b = let   c = a; in c; in 1");
        check("let $0a = 1; $1b = let $2c = a; in 1; in 1");
    }

    #[test]
    fn with_used_by_unused_let() {
        check("with 1; let $0a = from_with; in 1");
    }
}
