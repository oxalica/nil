mod lower;
mod scope;

#[cfg(test)]
mod tests;

use crate::source::{FileId, SourceDatabase};
use la_arena::{Arena, Idx};
use ordered_float::OrderedFloat;
use smol_str::SmolStr;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::ops;
use std::sync::Arc;

pub use self::scope::{ModuleScopes, ScopeData, ScopeId};

#[salsa::query_group(DefDatabaseStorage)]
pub trait DefDatabase: SourceDatabase {
    fn module_with_source_map(&self, file_id: FileId) -> (Arc<Module>, Arc<ModuleSourceMap>);

    fn module(&self, file_id: FileId) -> Arc<Module>;

    fn source_map(&self, file_id: FileId) -> Arc<ModuleSourceMap>;

    #[salsa::invoke(ModuleScopes::module_scopes_query)]
    fn scopes(&self, file_id: FileId) -> Arc<ModuleScopes>;
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Missing,
    Ident(Name),
    Literal(Literal),
    Apply(ExprId, ExprId),
    Lambda(Option<NameDefId>, Option<Pat>, ExprId),
    // TODO
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NameDef {
    pub name: Name,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Int(i64),
    Float(OrderedFloat<f64>),
    String(Box<str>),
    Path(Path),
    // TODO
}

#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(SmolStr);

impl From<&'_ str> for Name {
    fn from(s: &'_ str) -> Self {
        Self(s.into())
    }
}

impl From<String> for Name {
    fn from(s: String) -> Self {
        Self(s.into())
    }
}

impl Borrow<str> for Name {
    fn borrow(&self) -> &str {
        &*self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub anchor: PathAnchor,
    pub supers: usize,
    // Normalized path separated by `/`, with no `.` or `..` segments.
    pub raw_segments: Box<str>,
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
    Search(Box<str>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pat {
    pub fields: Box<[(Option<NameDefId>, Option<ExprId>)]>,
    pub ellipsis: bool,
}
