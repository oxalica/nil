mod lower;

#[cfg(test)]
mod tests;

use crate::source::{FileId, SourceDatabase};
use la_arena::{Arena, Idx};
use std::collections::HashMap;
use std::ops;
use std::sync::Arc;

#[salsa::query_group(DefDatabaseStorage)]
pub trait DefDatabase: SourceDatabase {
    fn module_with_source_map(&self, file_id: FileId) -> (Arc<Module>, Arc<ModuleSourceMap>);

    fn module(&self, file_id: FileId) -> Arc<Module>;

    fn source_map(&self, file_id: FileId) -> Arc<ModuleSourceMap>;
}

fn module_with_source_map(
    db: &dyn DefDatabase,
    file_id: FileId,
) -> (Arc<Module>, Arc<ModuleSourceMap>) {
    let root = db.parse(file_id);
    let (module, source_map) = lower::lower(root);
    (Arc::new(module), Arc::new(source_map))
}

fn module(db: &dyn DefDatabase, file_id: FileId) -> Arc<Module> {
    db.module_with_source_map(file_id).0
}

fn source_map(db: &dyn DefDatabase, file_id: FileId) -> Arc<ModuleSourceMap> {
    db.module_with_source_map(file_id).1
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Module {
    exprs: Arena<Expr>,
}

impl ops::Index<ExprId> for Module {
    type Output = Expr;
    fn index(&self, index: ExprId) -> &Self::Output {
        &self.exprs[index]
    }
}

pub type AstPtr = rowan::ast::SyntaxNodePtr<rnix::NixLanguage>;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct ModuleSourceMap {
    expr_map: HashMap<AstPtr, ExprId>,
    expr_map_rev: HashMap<ExprId, AstPtr>,
}

pub type ExprId = Idx<Expr>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Missing,
    Ident(Box<str>),
    Literal(Literal),
    Apply(ExprId, ExprId),
    // TODO
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Int(i64),
    String(Box<str>),
    Path(Path),
    // TODO
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub anchor: PathAnchor,
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
