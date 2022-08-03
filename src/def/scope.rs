use super::{BindingKey, BindingValue, Bindings, DefDatabase, Expr, ExprId, Module, NameDefId};
use crate::{builtin, FileId};
use la_arena::{Arena, ArenaMap, Idx};
use smol_str::SmolStr;
use std::collections::HashMap;
use std::sync::Arc;
use std::{iter, ops};

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct ModuleScopes {
    scopes: Arena<ScopeData>,
    scope_by_expr: ArenaMap<ExprId, ScopeId>,
}

pub type ScopeId = Idx<ScopeData>;

impl ops::Index<ScopeId> for ModuleScopes {
    type Output = ScopeData;
    fn index(&self, index: ScopeId) -> &Self::Output {
        &self.scopes[index]
    }
}

impl ModuleScopes {
    pub(crate) fn module_scopes_query(db: &dyn DefDatabase, file_id: FileId) -> Arc<Self> {
        let module = db.module(file_id);
        let mut this = Self::default();
        let root_scope = this.scopes.alloc(ScopeData {
            parent: None,
            kind: ScopeKind::NameDefs(Default::default()),
        });
        this.traverse_expr(&*module, module.entry_expr, root_scope);
        Arc::new(this)
    }

    pub(crate) fn resolve_name_query(
        db: &dyn DefDatabase,
        file_id: FileId,
        expr_id: ExprId,
    ) -> Option<ResolveResult> {
        let module = db.module(file_id);
        let name = match &module[expr_id] {
            Expr::Reference(name) => name,
            _ => return None,
        };
        db.scopes(file_id).resolve_name(expr_id, name)
    }

    pub fn scope_by_expr(&self, expr_id: ExprId) -> Option<ScopeId> {
        self.scope_by_expr.get(expr_id).copied()
    }

    pub fn ancestors(&self, scope_id: ScopeId) -> impl Iterator<Item = &'_ ScopeData> + '_ {
        iter::successors(Some(scope_id), |&i| self[i].parent).map(|i| &self[i])
    }

    pub fn resolve_name(&self, expr_id: ExprId, name: &SmolStr) -> Option<ResolveResult> {
        let scope = self.scope_by_expr(expr_id)?;
        // 1. Local defs.
        if let Some(def) = self
            .ancestors(scope)
            .find_map(|data| data.as_name_defs()?.get(name))
        {
            return Some(ResolveResult::NameDef(*def));
        }
        // 2. Builtin names.
        if let Some(name) = builtin::NAMES.get_key(name) {
            return Some(ResolveResult::Builtin(name));
        }
        // 3. "with" exprs.
        let withs = self
            .ancestors(scope)
            .filter_map(|data| data.as_with())
            .collect::<Vec<_>>();
        if !withs.is_empty() {
            return Some(ResolveResult::WithExprs(withs));
        }
        None
    }

    fn traverse_expr(&mut self, module: &Module, expr: ExprId, scope: ScopeId) {
        self.scope_by_expr.insert(expr, scope);

        match &module[expr] {
            Expr::Lambda(param, pat, body) => {
                let mut defs = HashMap::default();
                if let &Some(name_id) = param {
                    defs.insert(module[name_id].name.clone(), name_id);
                }
                if let Some(pat) = pat {
                    for name_id in pat.fields.iter().filter_map(|(opt_id, _)| *opt_id) {
                        defs.insert(module[name_id].name.clone(), name_id);
                    }
                }

                let scope = if !defs.is_empty() {
                    self.scopes.alloc(ScopeData {
                        parent: Some(scope),
                        kind: ScopeKind::NameDefs(defs),
                    })
                } else {
                    scope
                };

                if let Some(pat) = pat {
                    for default_expr in pat.fields.iter().filter_map(|(_, e)| *e) {
                        self.traverse_expr(module, default_expr, scope);
                    }
                }
                self.traverse_expr(module, *body, scope);
            }
            Expr::With(env, body) => {
                self.traverse_expr(module, *env, scope);
                let scope = self.scopes.alloc(ScopeData {
                    parent: Some(scope),
                    kind: ScopeKind::WithExpr(expr),
                });
                self.traverse_expr(module, *body, scope);
            }
            Expr::Attrset(bindings) | Expr::LetAttrset(bindings) => {
                self.traverse_bindings(module, bindings, scope);
            }
            Expr::LetIn(bindings, body) => {
                let scope = self.traverse_bindings(module, bindings, scope);
                self.traverse_expr(module, *body, scope);
            }
            e => e.walk_child_exprs(|e| self.traverse_expr(module, e, scope)),
        }
    }

    fn traverse_bindings(
        &mut self,
        module: &Module,
        bindings: &Bindings,
        scope: ScopeId,
    ) -> ScopeId {
        let mut defs = HashMap::default();

        for (k, v) in bindings.entries.iter() {
            if let &BindingKey::NameDef(def) = k {
                defs.insert(module[def].name.clone(), def);
            }

            // Inherited attrs are resolved in the outer scope.
            if let &BindingValue::Inherit(expr) = v {
                assert!(matches!(&module[expr], Expr::Reference(_)));
                self.traverse_expr(module, expr, scope);
            }
        }

        let scope = if defs.is_empty() {
            scope
        } else {
            self.scopes.alloc(ScopeData {
                parent: Some(scope),
                kind: ScopeKind::NameDefs(defs),
            })
        };

        for (k, v) in bindings.entries.iter() {
            if let &BindingKey::Dynamic(expr) = k {
                self.traverse_expr(module, expr, scope);
            }
            if let &BindingValue::Expr(expr) = v {
                self.traverse_expr(module, expr, scope);
            }
        }
        for &e in bindings.inherit_froms.iter() {
            self.traverse_expr(module, e, scope);
        }
        scope
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolveResult {
    NameDef(NameDefId),
    Builtin(&'static str),
    WithExprs(Vec<ExprId>),
}

impl ResolveResult {
    pub fn as_name_def(&self) -> Option<NameDefId> {
        match self {
            Self::NameDef(v) => Some(*v),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScopeData {
    parent: Option<ScopeId>,
    kind: ScopeKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ScopeKind {
    NameDefs(HashMap<SmolStr, NameDefId>),
    WithExpr(ExprId),
}

impl ScopeData {
    pub fn as_name_defs(&self) -> Option<&HashMap<SmolStr, NameDefId>> {
        match &self.kind {
            ScopeKind::NameDefs(defs) => Some(defs),
            _ => None,
        }
    }

    pub fn as_with(&self) -> Option<ExprId> {
        match self.kind {
            ScopeKind::WithExpr(expr) => Some(expr),
            _ => None,
        }
    }
}

/// The map of reverse name resolution, or name references.
/// It is used for name references lookup, but requires resolution of all names.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct NameReferenceMap {
    // Assume almost all defs are referenced somewhere.
    map: ArenaMap<NameDefId, Vec<ExprId>>,
}

impl NameReferenceMap {
    pub(crate) fn name_reference_map_query(db: &dyn DefDatabase, file_id: FileId) -> Arc<Self> {
        let module = db.module(file_id);
        let mut this = Self::default();
        module
            .exprs()
            // N.B. Inherited attrs are also translated into Expr::References.
            // This should cover all cases.
            .filter(|(_, kind)| matches!(kind, Expr::Reference(_)))
            .map(|(expr, _)| expr)
            .filter_map(|expr| Some((expr, db.resolve_name(file_id, expr)?.as_name_def()?)))
            .for_each(|(expr, def)| match this.map.get_mut(def) {
                Some(refs) => refs.push(expr),
                None => this.map.insert(def, vec![expr]),
            });
        Arc::new(this)
    }

    pub fn references(&self, def: NameDefId) -> Option<&[ExprId]> {
        Some(&**self.map.get(def)?)
    }
}

#[cfg(test)]
mod tests {
    use super::ScopeKind;
    use crate::def::{AstPtr, DefDatabase, ResolveResult, SourceDatabase};
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};
    use rowan::ast::AstNode;
    use syntax::{ast, match_ast};

    #[track_caller]
    fn check_scopes(fixture: &str, expect: Expect) {
        let (db, file_id, [pos]) = TestDB::single_file(fixture).unwrap();
        let ptr = AstPtr::new(
            db.node_at::<ast::Expr>(file_id, pos)
                .expect("No Expr node")
                .syntax(),
        );

        let source_map = db.source_map(file_id);
        let expr_id = source_map.expr_map[&ptr];
        let scopes = db.scopes(file_id);

        // "innermost@pos var@pos | middle@pos | outmost@pos"
        let scope_id = scopes.scope_by_expr(expr_id).expect("No scope data");
        let scope_defs = scopes
            .ancestors(scope_id)
            .map(|scope| match &scope.kind {
                ScopeKind::NameDefs(defs) => {
                    let mut names = defs
                        .iter()
                        .map(|(name, def)| {
                            let pos = source_map.name_def_node(*def).unwrap().text_range().start();
                            format!("{}@{}", name, u32::from(pos))
                        })
                        .collect::<Vec<_>>();
                    names.sort();
                    names.join(" ")
                }
                &ScopeKind::WithExpr(expr) => {
                    let pos = source_map.expr_node(expr).unwrap().text_range().start();
                    format!("with@{}", u32::from(pos))
                }
            })
            .collect::<Vec<_>>();
        // The last one is the empty root.
        let got = scope_defs[..scope_defs.len() - 1].join(" | ");
        expect.assert_eq(&got);
    }

    #[track_caller]
    fn check_resolve(fixture: &str) {
        let (db, file_id, [pos, def_pos]) = TestDB::single_file(fixture).unwrap();

        // Inherit(Attr(Name)) or Expr(Ref(Name))
        let ptr = db
            .find_node(file_id, pos, |n| {
                match_ast! {
                    match n {
                        ast::Expr(e) => Some(AstPtr::new(e.syntax())),
                        ast::Attr(e) => Some(AstPtr::new(e.syntax())),
                        _ => None,
                    }
                }
            })
            .expect("No Attr or Expr found");

        let parse = db.parse(file_id).value;
        let source_map = db.source_map(file_id);
        let expr_id = source_map.expr_map[&ptr];
        let got = db.resolve_name(file_id, expr_id).map(|ret| match ret {
            ResolveResult::NameDef(def) => source_map
                .name_def_node(def)
                .unwrap()
                .to_node(&parse.syntax_node())
                .text_range()
                .start(),
            ResolveResult::WithExprs(expr) => source_map
                // Test the innermost one.
                .expr_node(expr[0])
                .unwrap()
                .to_node(&parse.syntax_node())
                .text_range()
                .start(),
            // Same pos for builtin names.
            ResolveResult::Builtin(_) => pos,
        });
        assert_eq!(got, Some(def_pos));
    }

    #[track_caller]
    fn check_refs(fixture: &str, expect: &[u32]) {
        let (db, file_id, [pos]) = TestDB::single_file(fixture).unwrap();
        let ptr = AstPtr::new(
            db.node_at::<ast::Attr>(file_id, pos)
                .expect("No Attr node")
                .syntax(),
        );
        let source_map = db.source_map(file_id);
        let def = source_map.node_name_def(ptr).expect("Not a name def");

        let ref_map = db.name_reference_map(file_id);
        let got = ref_map.references(def).map_or_else(Vec::new, |exprs| {
            exprs
                .iter()
                .map(|&expr| {
                    let pos = source_map.expr_node(expr).unwrap().text_range().start();
                    u32::from(pos)
                })
                .collect::<Vec<_>>()
        });
        assert_eq!(got, expect);
    }

    #[test]
    fn top_level() {
        check_scopes(r"$0a", expect![[""]]);
    }

    #[test]
    fn lambda() {
        check_scopes(r"(a: b: (c: 0) $0a (d: 0)) (e: 0)", expect!["b@4 | a@1"]);
        check_scopes(r"{ a, b ? c, ... }@d: $0x (y: x)", expect!["a@2 b@5 d@18"]);
        check_scopes(
            r"a: { a, b ? $0c, ... }@d: y: a",
            expect!["a@5 b@8 d@21 | a@0"],
        );
        check_resolve("{} @ $1y: $0y");
    }

    #[test]
    fn with() {
        check_scopes(
            r"a: with b; c: with c; $0a (d: with e; a)",
            expect!["with@14 | c@11 | with@3 | a@0"],
        );

        check_resolve(r"$1a: with b; c: $0a");
        check_resolve(r"a: with b; $1c: $0c");
        check_resolve(r"a: $1with b; c: $0x");
        check_resolve(r"$1x: with a; with b; $0x");
        check_resolve(r"x: with a; $1with b; $0y");
    }

    #[test]
    fn attrset_non_rec() {
        check_scopes(
            "a: { inherit a; b = c: $0a; e = 1; inherit (a) f; }",
            expect!["c@20 | a@0"],
        );
        check_scopes(
            "a: { inherit a; b = c: a; e = 1; inherit ($0a) f; }",
            expect!["a@0"],
        );
    }

    #[test]
    fn attrset_rec() {
        check_scopes(
            "a: rec { inherit a; b = c: $0a; e = 1; inherit (a) f; }",
            expect!["c@24 | a@17 b@20 e@30 f@49 | a@0"],
        );
        check_scopes(
            "a: rec { inherit a; b = c: a; e = 1; inherit ($0a) f; }",
            expect!["a@17 b@20 e@30 f@49 | a@0"],
        );
        check_scopes(
            "a: rec { inherit $0a; b = c: a; e = 1; inherit (a) f; }",
            expect!["a@0"],
        );
    }

    #[test]
    fn let_in() {
        check_scopes(r#"let a.b = 1; "c+d" = a; in $0a"#, expect!["a@4 c+d@13"]);
        check_scopes(r#"let a.b = 1; "b+c" = $0a; in a"#, expect!["a@4 b+c@13"]);
    }

    #[test]
    fn shadowing() {
        check_scopes(
            "let a = 1; b = 2; in let a = 2; inherit b; in $0a",
            expect!["a@25 b@40 | a@4 b@11"],
        );

        check_resolve("let a = 1; b = 2; in let $1a = 2; inherit b; in $0a");
        check_resolve("let a = 1; b = 2; in let a = 2; inherit $1b; in $0b");
        check_resolve("let a = 1; $1b = 2; in let a = 2; inherit $0b; in b");
        check_resolve("let a = 1; in let $1a = $0a; in a");
    }

    #[test]
    fn builtin() {
        check_resolve("let $1true = 1; in with x; $0true + false + falsie");
        check_resolve("let true = 1; in with x; true + $0$1false + falsie");
        check_resolve("let true = 1; in $1with x; true + false + $0falsie");
    }

    #[test]
    fn references() {
        check_refs("let $0a = 1; b = 1; in a", &[21]);
        check_refs("let a = 1; $0b = 1; in a", &[]);

        check_refs("rec { inherit (b) $0a; b = a; }", &[25]);
        check_refs("rec { inherit (b) a; $0b = a; }", &[15]);

        check_refs(r#"let $0" " = 1; in { inherit " "; }"#, &[26]);
        check_refs(
            r#"let " " = 1; in rec { inherit $0" "; x = { inherit ${" "}; }; }"#,
            &[49],
        );
    }
}
