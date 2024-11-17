//! Liveness check of names.
//! It locates unnecessary or inaccessible bindings and expressions, based on name resolution.
//!
//! Our goals are,
//! - Applicable.
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
//! - Unused parameters of a package.
//!
//! Notes:
//! - All identifiers starting with `_` are skipped from warnings. This also includes Nix internals
//!   starting with `__`, eg. `__findFile` <https://github.com/oxalica/nil/pull/109>.
use super::{BindingValue, DefDatabase, Expr, ExprId, NameId, ResolveResult};
use crate::{Diagnostic, DiagnosticKind, FileId, ModuleKind};
use la_arena::ArenaMap;
use std::collections::BTreeMap;
use std::sync::Arc;
use syntax::ast::{self, AstNode};
use syntax::TextRange;

fn should_ignore(name: &str) -> bool {
    name.starts_with('_')
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct LivenessCheckResult {
    names: Box<[NameId]>,
    withs: Box<[ExprId]>,
    rec_attrsets: Box<[ExprId]>,
}

impl LivenessCheckResult {
    /// Turn the liveness check results into DB Diagnostics.
    pub fn to_diagnostics<'a>(
        &'a self,
        db: &dyn DefDatabase,
        file: FileId,
    ) -> impl Iterator<Item = Diagnostic> + 'a {
        let source_map = db.source_map(file);
        let root = db.parse(file).syntax_node();
        let mut diags = Vec::new();
        diags.extend(
            self.names
                .iter()
                .flat_map(|&def| source_map.nodes_for_name(def))
                .map(|ptr| Diagnostic::new(ptr.text_range(), DiagnosticKind::UnusedBinding)),
        );
        diags.extend(self.withs.iter().map(|&expr| {
            let ptr = source_map.node_for_expr(expr).unwrap();
            let node = ast::With::cast(ptr.to_node(&root)).unwrap();
            let header_range = match (node.with_token(), node.semicolon_token()) {
                (Some(start), Some(end)) => start.text_range().cover(end.text_range()),
                _ => TextRange::empty(ptr.text_range().start()),
            };
            Diagnostic::new(header_range, DiagnosticKind::UnusedWith)
        }));
        diags.extend(self.rec_attrsets.iter().map(|&expr| {
            let ptr = source_map.node_for_expr(expr).unwrap();
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

/// Query to the DB to get liveness faults.
pub(crate) fn liveness_check_query(
    db: &dyn DefDatabase,
    file_id: FileId,
) -> Arc<LivenessCheckResult> {
    let module = db.module(file_id);
    let name_res = db.name_resolution(file_id);

    let (must_use_params_expr, is_flake_outputs) = match &*db.module_kind(file_id) {
        ModuleKind::Package { lambda_expr }
        | ModuleKind::ConfigModule { lambda_expr }
        | ModuleKind::Config { lambda_expr } => (Some(*lambda_expr), false),
        ModuleKind::FlakeNix {
            outputs_expr: Some(outputs_expr),
            ..
        } => (Some(*outputs_expr), true),
        _ => (None, false),
    };

    // Unused let-bindings are eagerly collected into this.
    let mut unused_defs = Vec::new();

    let name_cnt = module.names.len();
    let expr_cnt = module.exprs.len();

    // For rec-attrset check.
    let mut visited_defs = ArenaMap::<NameId, ()>::with_capacity(name_cnt);
    // For traversal of let-in bindings.
    let mut visited_def_rhs = ArenaMap::with_capacity(expr_cnt);
    let mut visited_withs = ArenaMap::with_capacity(expr_cnt);
    let mut stack = vec![module.entry_expr];
    let mut discovered_let_rhs: BTreeMap<NameId, ExprId> = BTreeMap::new();

    while !stack.is_empty() {
        // N.B. This should be dropped in every loop.
        // Or it will make this whole check cost quadratic time!
        discovered_let_rhs.clear();

        // Traverse all reachable Exprs from roots.
        while let Some(expr) = stack.pop() {
            match &module[expr] {
                Expr::Reference(_) => match name_res.get(expr) {
                    Some(&ResolveResult::Definition(name)) => {
                        visited_defs.insert(name, ());
                        if let Some(rhs) = discovered_let_rhs.remove(&name) {
                            // Dedup inherit-from expressions.
                            if visited_def_rhs.get(rhs).is_none() {
                                visited_def_rhs.insert(rhs, ());
                                stack.push(rhs);
                            }
                        }
                    }
                    Some(ResolveResult::WithExprs(exprs)) => {
                        for &expr in exprs {
                            visited_withs.insert(expr, ());
                        }
                    }
                    Some(ResolveResult::Builtin(_)) | None => {}
                },
                Expr::LetIn(bindings, body) => {
                    // Pre-mark all let-binding.
                    for &(name, value) in bindings.statics.iter() {
                        let e = match value {
                            BindingValue::Expr(e) | BindingValue::Inherit(e) => e,
                            BindingValue::InheritFrom(i) => bindings.inherit_froms[i],
                        };
                        discovered_let_rhs.insert(name, e);
                    }

                    // Traverse the body as root.
                    stack.push(*body);
                }
                e => e.walk_child_exprs(|e| stack.push(e)),
            }
        }

        // Record unused let-bindings and continue traversal inside them,
        // as if they are reachable.
        unused_defs.extend(discovered_let_rhs.iter().map(|(&name, _)| name));
        stack.extend(discovered_let_rhs.iter().map(|(_, &rhs)| rhs));
    }

    // Finally, collect unused lambda parameters, "with" expressions and "rec" attrsets.
    // It's intended to not reporting them if they are only referenced by some unused let-bindings.
    // Unused let-bindings may be caused by unfinished codes or typos,
    // situation may be changed when user tries to fix them.
    let mut unused_withs = Vec::new();
    let mut unused_recs = Vec::new();
    for (expr, kind) in module.exprs() {
        match kind {
            Expr::Lambda(param, pat, _) => {
                // `{ ... }@bar: ...`
                //          ^ Unused and removable.
                if let Some(param) = *param {
                    if pat.is_some() && visited_defs.get(param).is_none() {
                        unused_defs.push(param);
                    }
                }
                // `{ foo [, ...] }[@bar]: ...`
                //    ^ Unused and removable, only for packages and configurations.
                if let Some(pat) = pat {
                    if must_use_params_expr == Some(expr)
                    // For flakes,
                    // `outputs = { foo [, ...] }@bar: ...`
                    //              ^ Always considered used.
                    // It causes Nix to add inputs from registry automatically,
                    // and user can access it via `bar` elsewhere.
                    // Tested in `flake_output_with_universal`.
                        && (!is_flake_outputs || param.is_none())
                    {
                        unused_defs.extend(pat.fields.iter().filter_map(|&(name, _)| {
                            let name = name?;
                            if visited_defs.get(name).is_some() {
                                return None;
                            }
                            // `self` is not removable without ellipsis for flake outputs.
                            if is_flake_outputs && !pat.ellipsis && module[name].text == "self" {
                                return None;
                            }
                            Some(name)
                        }));
                    }
                }
            }
            &Expr::With(..) if visited_withs.get(expr).is_none() => {
                unused_withs.push(expr);
            }
            Expr::RecAttrset(bindings)
                if bindings
                    .statics
                    .iter()
                    .all(|&(name, _)| visited_defs.get(name).is_none()) =>
            {
                unused_recs.push(expr);
            }
            _ => {}
        }
    }

    unused_defs.retain(|name| !should_ignore(&module[*name].text));

    Arc::new(LivenessCheckResult {
        names: unused_defs.into(),
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

        let source_map = db.source_map(file);
        let lower_diags = source_map.diagnostics();
        assert!(
            !lower_diags.iter().any(|diag| diag.severity().is_fatal()),
            "Lower error: {lower_diags:?}",
        );

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

    #[test]
    fn unused_pat_param_package() {
        check("{ stdenv, $0hello }: stdenv.mkDerivation { }");
    }

    #[test]
    fn unused_pat_param_config() {
        check("{ $0lib, pkgs, ... }: { foo = with pkgs; [ bar ]; }");
    }

    #[test]
    fn flake_output_self() {
        // Warn `self`.
        check(
            "
#- /flake.nix
{
    outputs = { $0self, foo, $1nixpkgs, ... }: foo.lib.foo { };
}
            ",
        );

        // Don't warn `self`.
        check(
            "
#- /flake.nix
{
    outputs = { self, $0nixpkgs }: { };
}
            ",
        );
    }

    // https://github.com/oxalica/nil/issues/73
    #[test]
    fn flake_output_with_universal() {
        // Don't warn pat parameters for automatic inputs.
        check(
            "
#- /flake.nix
{
    outputs = { self, nixpkgs, ... }@inputs:
        import ./foo.nix inputs;
}
            ",
        );

        // The universal parameter itself is still warned.
        check(
            "
#- /flake.nix
{
    outputs = { self, nixpkgs, ... }@$0inputs: { };
}
            ",
        );
    }

    #[test]
    fn underscore_names() {
        check("x: _y: x");
        check("let __findFile = 42; in <nixpkgs>");
    }

    // Issue #114
    #[test]
    fn merged_rec_attrset() {
        check("{ test = $0rec { }; test.bar = 42; }");
        check("{ test = rec { foo = bar; }; test.bar = 42; }");
        check("{ test = rec { foo = 42; }; test.bar = foo; }");
        check("{ test = rec { }; test.foo = bar; test.bar = 42; }");

        check("{ test = $0rec { }; test = rec { };}");
    }
}
