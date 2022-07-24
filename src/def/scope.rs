use super::{DefDatabase, Expr, ExprId, Module, Name, NameDefId};
use crate::base::FileId;
use la_arena::{Arena, ArenaMap, Idx};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;
use std::sync::Arc;
use std::{iter, ops};

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct ModuleScopes {
    scopes: Arena<ScopeData>,
    scope_by_expr: ArenaMap<ExprId, ScopeId>,
}

pub type ScopeId = Idx<ScopeData>;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct ScopeData {
    parent: Option<ScopeId>,
    name_defs: HashMap<Name, NameDefId>,
}

impl ops::Index<ScopeId> for ModuleScopes {
    type Output = ScopeData;
    fn index(&self, index: ScopeId) -> &Self::Output {
        &self.scopes[index]
    }
}

impl ModuleScopes {
    pub(crate) fn module_scopes_query(db: &dyn DefDatabase, file_id: FileId) -> Arc<Self> {
        let module = db.module(file_id);
        let mut this = ModuleScopes::default();
        let root = this.new_root_scope();
        this.traverse_expr(&*module, root, module.entry_expr);
        Arc::new(this)
    }

    pub(crate) fn lookup_name_query(
        db: &dyn DefDatabase,
        file_id: FileId,
        expr_id: ExprId,
    ) -> Option<NameDefId> {
        let module = db.module(file_id);
        let name = match &module[expr_id] {
            Expr::Ident(name) => name,
            _ => return None,
        };
        db.scopes(file_id).lookup(expr_id, name)
    }

    pub fn scope_by_expr(&self, expr_id: ExprId) -> Option<ScopeId> {
        self.scope_by_expr.get(expr_id).copied()
    }

    pub fn ancestors(&self, scope_id: ScopeId) -> impl Iterator<Item = &'_ ScopeData> + '_ {
        iter::successors(Some(scope_id), |&i| self[i].parent).map(|i| &self[i])
    }

    pub fn lookup(&self, expr_id: ExprId, name: &Name) -> Option<NameDefId> {
        self.ancestors(self.scope_by_expr(expr_id)?)
            .find_map(|scope| scope.get(name))
    }

    fn new_root_scope(&mut self) -> ScopeId {
        self.scopes.alloc(ScopeData {
            parent: None,
            name_defs: HashMap::new(),
        })
    }

    fn new_scope(&mut self, parent: ScopeId, name_defs: HashMap<Name, NameDefId>) -> ScopeId {
        self.scopes.alloc(ScopeData {
            parent: Some(parent),
            name_defs,
        })
    }

    fn traverse_expr(&mut self, module: &Module, scope_id: ScopeId, expr_id: ExprId) {
        self.scope_by_expr.insert(expr_id, scope_id);
        match module[expr_id] {
            Expr::Missing | Expr::Ident(_) | Expr::Literal(_) => {}
            Expr::Apply(func, arg) => {
                self.traverse_expr(module, scope_id, func);
                self.traverse_expr(module, scope_id, arg);
            }
            Expr::Lambda(param_opt, ref pat_opt, body) => {
                let mut name_defs = HashMap::default();
                if let Some(name_id) = param_opt {
                    name_defs.insert(module[name_id].name.clone(), name_id);
                }
                if let Some(pat) = pat_opt {
                    for name_id in pat.fields.iter().filter_map(|(opt_id, _)| *opt_id) {
                        name_defs.insert(module[name_id].name.clone(), name_id);
                    }
                }
                let scope_id = self.new_scope(scope_id, name_defs);

                if let Some(pat) = pat_opt {
                    for expr_id in pat
                        .fields
                        .iter()
                        .filter_map(|(_, default_expr_id)| *default_expr_id)
                    {
                        self.traverse_expr(module, scope_id, expr_id);
                    }
                }
                self.traverse_expr(module, scope_id, body);
            }
        }
    }
}

impl ScopeData {
    pub fn name_defs(&self) -> impl Iterator<Item = NameDefId> + '_ {
        self.name_defs.values().copied()
    }

    pub fn get<Q>(&self, name: &Q) -> Option<NameDefId>
    where
        Q: Hash + Eq,
        Name: Borrow<Q>,
    {
        self.name_defs.get(name).copied()
    }
}
