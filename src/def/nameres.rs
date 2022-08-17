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

    pub fn scope_by_expr(&self, expr_id: ExprId) -> Option<ScopeId> {
        self.scope_by_expr.get(expr_id).copied()
    }

    pub fn ancestors(&self, scope_id: ScopeId) -> impl Iterator<Item = &'_ ScopeData> + '_ {
        iter::successors(Some(scope_id), |&i| self[i].parent).map(|i| &self[i])
    }

    /// Resolve a name in the scope of an Expr.
    fn resolve_name(&self, expr_id: ExprId, name: &SmolStr) -> Option<ResolveResult> {
        let scope = self.scope_by_expr(expr_id)?;
        // 1. Local defs.
        if let Some(def) = self
            .ancestors(scope)
            .find_map(|data| data.as_name_defs()?.get(name))
        {
            return Some(ResolveResult::NameDef(*def));
        }
        // 2. Builtin names.
        if let Some(name) = builtin::BUILTINS.get_key(name) {
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
            Expr::Attrset(bindings) | Expr::RecAttrset(bindings) | Expr::LetAttrset(bindings) => {
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

        for (key, value) in bindings.statics.iter() {
            if let &BindingKey::NameDef(def) = key {
                defs.insert(module[def].name.clone(), def);
            }

            // Inherited attrs are resolved in the outer scope.
            if let &BindingValue::Inherit(expr) = value {
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

        for (_, value) in bindings.statics.iter() {
            match value {
                // Traversed before.
                BindingValue::Inherit(_) |
                // Traversed later.
                BindingValue::InheritFrom(_) => {},
                &BindingValue::Expr(e) => {
                    self.traverse_expr(module, e, scope);
                }
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

/// Name resolution of all references.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct NameResolution {
    resolve_map: HashMap<ExprId, ResolveResult>,
}

impl NameResolution {
    pub(crate) fn name_resolution_query(db: &dyn DefDatabase, file_id: FileId) -> Arc<Self> {
        let module = db.module(file_id);
        let scopes = db.scopes(file_id);
        let resolve_map = module
            .exprs()
            .filter_map(|(e, kind)| {
                match kind {
                    // Inherited attrs are also translated into Expr::References.
                    Expr::Reference(name) => Some((e, scopes.resolve_name(e, name)?)),
                    _ => None,
                }
            })
            .collect();
        Arc::new(Self { resolve_map })
    }

    pub fn get(&self, expr: ExprId) -> Option<&ResolveResult> {
        self.resolve_map.get(&expr)
    }

    pub fn iter(&self) -> impl Iterator<Item = (ExprId, &'_ ResolveResult)> + '_ {
        self.resolve_map.iter().map(|(e, res)| (*e, res))
    }
}

/// The map of reverse name resolution, or name references.
/// It is used for name references lookup, but requires resolution of all names.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct NameReference {
    // Assume almost all defs are referenced somewhere.
    def_refs: ArenaMap<NameDefId, Vec<ExprId>>,
    // But there are just some "with"s.
    with_refs: HashMap<ExprId, Vec<ExprId>>,
}

impl NameReference {
    pub(crate) fn name_reference_query(db: &dyn DefDatabase, file_id: FileId) -> Arc<Self> {
        let name_res = db.name_resolution(file_id);
        let mut this = Self::default();
        for (expr, resolved) in name_res.iter() {
            match resolved {
                ResolveResult::Builtin(_) => {}
                &ResolveResult::NameDef(def) => match this.def_refs.get_mut(def) {
                    Some(refs) => refs.push(expr),
                    None => this.def_refs.insert(def, vec![expr]),
                },
                ResolveResult::WithExprs(withs) => withs
                    .iter()
                    .for_each(|&with_expr| this.with_refs.entry(with_expr).or_default().push(expr)),
            }
        }
        Arc::new(this)
    }

    pub fn def_references(&self, def: NameDefId) -> Option<&[ExprId]> {
        Some(&**self.def_refs.get(def)?)
    }

    pub fn with_references(&self, with_expr: ExprId) -> Option<&[ExprId]> {
        Some(&**self.with_refs.get(&with_expr)?)
    }
}

#[cfg(test)]
mod tests {
    use super::ScopeKind;
    use crate::def::{AstPtr, DefDatabase, ResolveResult};
    use crate::tests::TestDB;
    use rowan::ast::AstNode;
    use syntax::{ast, match_ast};

    #[track_caller]
    fn check_scopes(fixture: &str) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        let expect = f.markers()[1..].iter().map(|p| p.pos).collect::<Vec<_>>();
        let ptr = AstPtr::new(
            db.node_at::<ast::Expr>(f[0])
                .expect("No Expr node")
                .syntax(),
        );

        let source_map = db.source_map(f[0].file_id);
        let expr_id = source_map.expr_map[&ptr];
        let scopes = db.scopes(f[0].file_id);

        // "innermost@pos var@pos | middle@pos | outmost@pos"
        let scope_id = scopes.scope_by_expr(expr_id).expect("No scope data");
        let def_poses = scopes
            .ancestors(scope_id)
            .flat_map(|scope| match &scope.kind {
                ScopeKind::NameDefs(defs) => {
                    let mut poses = defs
                        .iter()
                        .map(|(_, def)| {
                            source_map.name_def_node(*def).unwrap().text_range().start()
                        })
                        .collect::<Vec<_>>();
                    poses.sort_unstable();
                    poses
                }
                &ScopeKind::WithExpr(expr) => {
                    vec![source_map.expr_node(expr).unwrap().text_range().start()]
                }
            })
            .collect::<Vec<_>>();
        assert_eq!(def_poses, expect);
    }

    #[track_caller]
    fn check_resolve(fixture: &str) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        let file_id = f[0].file_id;
        assert!((1..=2).contains(&f.markers().len()));
        let expect = f.markers().get(1).map(|p| p.pos);

        // Inherit(Attr(Name)) or Expr(Ref(Name))
        let ptr = db
            .find_node(f[0], |n| {
                match_ast! {
                    match n {
                        ast::Expr(e) => Some(AstPtr::new(e.syntax())),
                        ast::Attr(e) => Some(AstPtr::new(e.syntax())),
                        _ => None,
                    }
                }
            })
            .expect("No Attr or Expr found");

        let parse = db.parse(file_id);
        let source_map = db.source_map(file_id);
        let name_res = db.name_resolution(file_id);
        let expr_id = source_map.expr_map[&ptr];
        let got = name_res.get(expr_id).map(|ret| match ret {
            &ResolveResult::NameDef(def) => source_map
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
            ResolveResult::Builtin(_) => f[0].pos,
        });
        assert_eq!(got, expect);
    }

    #[test]
    fn top_level() {
        check_scopes(r"$0a");
    }

    #[test]
    fn lambda() {
        check_scopes(r"($2a: $1b: (c: 0) $0a (d: 0)) (e: 0)");
        check_scopes(r"{ $1a, $2b ? c, ... }@$3d: $0x (y: x)");
        check_scopes(r"$4a: { $1a, $2b ? $0c, ... }@$3d: y: a");

        check_resolve("{} @ $1y: $0y");
    }

    #[test]
    fn with() {
        check_scopes(r"$4a: $3with b; $2c: $1with c; $0a (d: with e; a)");

        check_resolve(r"$1a: with b; c: $0a");
        check_resolve(r"a: with b; $1c: $0c");
        check_resolve(r"a: $1with b; c: $0x");
        check_resolve(r"$1x: with a; with b; $0x");
        check_resolve(r"x: with a; $1with b; $0y");
    }

    #[test]
    fn attrset_non_rec() {
        check_scopes("$2a: { inherit a; b = $1c: $0a; e = 1; inherit (a) f; }");
        check_scopes("$1a: { inherit a; b = c: a; e = 1; inherit ($0a) f; }");
    }

    #[test]
    fn attrset_rec() {
        check_scopes("$6a: rec { inherit $2a; $3b = $1c: $0a; $4e = 1; inherit (a) $5f; }");
        check_scopes("$5a: rec { inherit $1a; $2b = c: a; $3e = 1; inherit ($0a) $4f; }");
        check_scopes("$1a: rec { inherit $0a; b = c: a; e = 1; inherit (a) f; }");
    }

    #[test]
    fn let_in() {
        check_scopes(r#"let $1a.b = 1; $2"c+d" = a; in $0a"#);
        check_scopes(r#"let $1a.b = 1; $2"b+c" = $0a; in a"#);
    }

    #[test]
    fn shadowing() {
        check_scopes("let $3a = 1; $4b = 2; in let $1a = 2; inherit $2b; in $0a");

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
}
