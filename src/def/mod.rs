mod lower;
mod scope;

#[cfg(test)]
mod tests;

use crate::base::{FileId, SourceDatabase};
use la_arena::{Arena, ArenaMap, Idx};
use ordered_float::OrderedFloat;
use smol_str::SmolStr;
use std::{collections::HashMap, ops, sync::Arc};

pub use self::scope::{ModuleScopes, ScopeData, ScopeId};

#[salsa::query_group(DefDatabaseStorage)]
pub trait DefDatabase: SourceDatabase {
    fn module_with_source_map(&self, file_id: FileId) -> (Arc<Module>, Arc<ModuleSourceMap>);

    fn module(&self, file_id: FileId) -> Arc<Module>;

    fn source_map(&self, file_id: FileId) -> Arc<ModuleSourceMap>;

    #[salsa::invoke(ModuleScopes::module_scopes_query)]
    fn scopes(&self, file_id: FileId) -> Arc<ModuleScopes>;

    #[salsa::invoke(ModuleScopes::resolve_name_query)]
    fn resolve_name(&self, file_id: FileId, expr_id: ExprId) -> Option<NameDefId>;
}

fn module_with_source_map(
    db: &dyn DefDatabase,
    file_id: FileId,
) -> (Arc<Module>, Arc<ModuleSourceMap>) {
    let parse = db.parse(file_id);
    let (module, source_map) = lower::lower(parse.map(|p| p.root()));
    (Arc::new(module), Arc::new(source_map))
}

fn module(db: &dyn DefDatabase, file_id: FileId) -> Arc<Module> {
    db.module_with_source_map(file_id).0
}

fn source_map(db: &dyn DefDatabase, file_id: FileId) -> Arc<ModuleSourceMap> {
    db.module_with_source_map(file_id).1
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    exprs: Arena<Expr>,
    name_defs: Arena<NameDef>,
    entry_expr: ExprId,
}

pub type ExprId = Idx<Expr>;
pub type NameDefId = Idx<NameDef>;

impl ops::Index<ExprId> for Module {
    type Output = Expr;
    fn index(&self, index: ExprId) -> &Self::Output {
        &self.exprs[index]
    }
}

impl ops::Index<NameDefId> for Module {
    type Output = NameDef;
    fn index(&self, index: NameDefId) -> &Self::Output {
        &self.name_defs[index]
    }
}

pub type AstPtr = rowan::ast::SyntaxNodePtr<syntax::NixLanguage>;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct ModuleSourceMap {
    expr_map: HashMap<AstPtr, ExprId>,
    expr_map_rev: HashMap<ExprId, AstPtr>,
    name_def_map: HashMap<AstPtr, NameDefId>,
    name_def_map_rev: ArenaMap<NameDefId, AstPtr>,
}

impl ModuleSourceMap {
    pub fn node_expr(&self, node: AstPtr) -> Option<ExprId> {
        self.expr_map.get(&node).copied()
    }

    pub fn expr_node(&self, expr_id: ExprId) -> Option<AstPtr> {
        self.expr_map_rev.get(&expr_id).cloned()
    }

    pub fn name_def_node(&self, def_id: NameDefId) -> Option<AstPtr> {
        self.name_def_map_rev.get(def_id).cloned()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Missing,
    Reference(SmolStr),
    Literal(Literal),
    Apply(ExprId, ExprId),
    Lambda(Option<NameDefId>, Option<Pat>, ExprId),
    // TODO
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NameDef {
    pub name: SmolStr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Int(i64),
    Float(OrderedFloat<f64>),
    String(SmolStr),
    Path(Path),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub anchor: PathAnchor,
    pub supers: usize,
    // Normalized path separated by `/`, with no `.` or `..` segments.
    pub raw_segments: SmolStr,
}

impl Path {
    pub fn segments(&self) -> impl Iterator<Item = &str> + '_ {
        self.raw_segments.split(' ').filter(|s| !s.is_empty())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PathAnchor {
    Relative(FileId),
    Absolute,
    Home,
    Search(SmolStr),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pat {
    pub fields: Box<[(Option<NameDefId>, Option<ExprId>)]>,
    pub ellipsis: bool,
}
