use super::{DefDatabase, Expr, ExprId, Module, NameDefId};
use crate::base::FileId;
use la_arena::{Arena, ArenaMap, Idx};
use smol_str::SmolStr;
use std::{collections::HashMap, iter, ops, sync::Arc};

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
        let mut inner_env = None;
        self.ancestors(self.scope_by_expr(expr_id)?)
            .find_map(|data| match &data.kind {
                ScopeKind::NameDefs(defs) => defs.get(name).copied(),
                ScopeKind::WithEnv(env) => {
                    inner_env = inner_env.or(Some(*env));
                    None
                }
            })
            .map(ResolveResult::NameDef)
            .or_else(|| inner_env.map(ResolveResult::WithEnv))
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
                    kind: ScopeKind::WithEnv(*env),
                });
                self.traverse_expr(module, *body, scope);
            }
            e => e.walk_child_exprs(|e| self.traverse_expr(module, e, scope)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolveResult {
    NameDef(NameDefId),
    WithEnv(ExprId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScopeData {
    parent: Option<ScopeId>,
    kind: ScopeKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ScopeKind {
    NameDefs(HashMap<SmolStr, NameDefId>),
    WithEnv(ExprId),
}

impl ScopeData {
    pub fn name_defs(&self) -> impl Iterator<Item = (&SmolStr, NameDefId)> + '_ {
        match &self.kind {
            ScopeKind::NameDefs(defs) => Some(defs),
            ScopeKind::WithEnv(_) => None,
        }
        .into_iter()
        .flatten()
        .map(|(name, &def)| (name, def))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        base::SourceDatabase,
        def::{AstPtr, DefDatabase, ResolveResult},
        tests::TestDB,
    };
    use rowan::ast::AstNode;
    use syntax::ast;

    #[track_caller]
    fn check_scopes(src: &str, expect: &str) {
        let (db, file_id, pos) = TestDB::from_file_with_pos(src);
        let ptr = AstPtr::new(db.node_at::<ast::Expr>(file_id, pos).syntax());

        let source_map = db.source_map(file_id);
        let expr_id = source_map.expr_map[&ptr];
        let scopes = db.scopes(file_id);

        // "innermost var abc | middle var | outmost var"
        let scope_id = scopes.scope_by_expr(expr_id).expect("No scope data");
        let scope_defs = scopes
            .ancestors(scope_id)
            .filter_map(|scope| {
                let mut names = scope
                    .name_defs()
                    .map(|(name, _)| &**name)
                    .collect::<Vec<_>>();
                if names.is_empty() {
                    return None;
                }
                names.sort();
                Some(names.join(" "))
            })
            .collect::<Vec<_>>();
        let got = scope_defs.join(" | ");
        assert_eq!(got, expect);
    }

    #[track_caller]
    fn check_resolve(src: &str, expect: Option<u32>) {
        let (db, file_id, pos) = TestDB::from_file_with_pos(src);
        let ptr = AstPtr::new(db.node_at::<ast::Expr>(file_id, pos).syntax());
        let parse = db.parse(file_id).value;
        let source_map = db.source_map(file_id);
        let expr_id = source_map.expr_map[&ptr];
        let got = db
            .resolve_name(file_id, expr_id)
            .map(|ret| match ret {
                ResolveResult::NameDef(def) => source_map
                    .name_def_node(def)
                    .unwrap()
                    .to_node(&parse.syntax_node()),
                ResolveResult::WithEnv(env) => source_map
                    .expr_node(env)
                    .unwrap()
                    .to_node(&parse.syntax_node()),
            })
            .map(|n| u32::from(n.text_range().start()));
        assert_eq!(got, expect);
    }

    #[test]
    fn top_level() {
        check_scopes(r"$0a", "");

        check_resolve(r"$0a", None);
    }

    #[test]
    fn lambda() {
        check_scopes(r"(a: b: (c: 0) $0a (d: 0)) (e: 0)", "b | a");
        check_scopes(r"{ a, b ? c, ... }@d: $0x (y: x)", "a b d");
        check_scopes(r"a: { a, b ? $0c, ... }@d: y: a", "a b d | a");

        check_resolve(r"a: b: b$0", Some(3));
        check_resolve(r"a: { a, b ? $0d, ... }@d: y: a", Some(21));
        check_resolve(r"a: { a, b ? $0y, ... }@d: y: a", None);
        check_resolve(r"a: { a, b ? $0a, ... }@d: y: a", Some(5));
        check_resolve(r"a: { x, b ? $0a, ... }@d: y: a", Some(0));
    }

    #[test]
    fn with() {
        check_scopes(r"a: with b; c: $0a", "c | a");

        check_resolve(r"a: with b; c: $0a", Some(0));
        check_resolve(r"a: with b; c: $0c", Some(11));
        check_resolve(r"a: with b; c: $0x", Some(8));
        check_resolve(r"x: with a; with b; $0x", Some(0));
        check_resolve(r"x: with a; with b; $0y", Some(16));
    }
}
