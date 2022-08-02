mod lower;
mod scope;

#[cfg(test)]
mod tests;

use crate::base::SourceDatabase;
use crate::{Diagnostic, FileId};
use la_arena::{Arena, ArenaMap, Idx};
use ordered_float::OrderedFloat;
use smol_str::SmolStr;
use std::collections::HashMap;
use std::ops;
use std::sync::Arc;

pub use self::scope::{ModuleScopes, NameReferenceMap, ResolveResult, ScopeData, ScopeId};
pub use syntax::ast::{BinaryOpKind as BinaryOp, UnaryOpKind as UnaryOp};

#[salsa::query_group(DefDatabaseStorage)]
pub trait DefDatabase: SourceDatabase {
    fn module_with_source_map(&self, file_id: FileId) -> (Arc<Module>, Arc<ModuleSourceMap>);

    fn module(&self, file_id: FileId) -> Arc<Module>;

    fn source_map(&self, file_id: FileId) -> Arc<ModuleSourceMap>;

    #[salsa::invoke(ModuleScopes::module_scopes_query)]
    fn scopes(&self, file_id: FileId) -> Arc<ModuleScopes>;

    #[salsa::invoke(ModuleScopes::resolve_name_query)]
    fn resolve_name(&self, file_id: FileId, expr_id: ExprId) -> Option<ResolveResult>;

    #[salsa::invoke(NameReferenceMap::name_reference_map_query)]
    fn name_reference_map(&self, file_id: FileId) -> Arc<NameReferenceMap>;
}

fn module_with_source_map(
    db: &dyn DefDatabase,
    file_id: FileId,
) -> (Arc<Module>, Arc<ModuleSourceMap>) {
    let parse = db.parse(file_id);
    let (module, source_map) = lower::lower(parse);
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
    diagnostics: Vec<Diagnostic>,
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

impl Module {
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn exprs(&self) -> impl Iterator<Item = (ExprId, &'_ Expr)> + ExactSizeIterator + '_ {
        self.exprs.iter()
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

    pub fn node_name_def(&self, node: AstPtr) -> Option<NameDefId> {
        self.name_def_map.get(&node).copied()
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
    Lambda(Option<NameDefId>, Option<Pat>, ExprId),
    With(ExprId, ExprId),
    Assert(ExprId, ExprId),
    IfThenElse(ExprId, ExprId, ExprId),
    Binary(Option<BinaryOp>, ExprId, ExprId),
    Apply(ExprId, ExprId),
    Unary(Option<UnaryOp>, ExprId),
    HasAttr(ExprId, Attrpath),
    Select(ExprId, Attrpath, Option<ExprId>),
    StringInterpolation(Box<[ExprId]>),
    List(Box<[ExprId]>),
    LetIn(Bindings, ExprId),
    Attrset(Bindings),
    LetAttrset(Bindings),
}

impl Expr {
    pub(crate) fn walk_child_exprs(&self, mut f: impl FnMut(ExprId)) {
        match self {
            Self::Missing | Self::Reference(_) | Self::Literal(_) => {}
            Self::Lambda(_, pat, body) => {
                if let Some(p) = pat {
                    p.fields
                        .iter()
                        .filter_map(|&(_, default_expr)| default_expr)
                        .for_each(&mut f);
                }
                f(*body);
            }
            Self::Unary(_, a) => f(*a),
            Self::Assert(a, b) | Self::With(a, b) | Self::Binary(_, a, b) | Self::Apply(a, b) => {
                f(*a);
                f(*b);
            }
            Self::IfThenElse(a, b, c) => {
                f(*a);
                f(*b);
                f(*c);
            }
            Self::HasAttr(set, path) => {
                f(*set);
                path.iter().copied().for_each(f);
            }
            Self::Select(set, path, default_expr) => {
                f(*set);
                path.iter().copied().for_each(&mut f);
                if let &Some(e) = default_expr {
                    f(e);
                }
            }
            Self::StringInterpolation(xs) | Self::List(xs) => xs.iter().copied().for_each(f),
            Self::LetIn(bindings, body) => {
                bindings.walk_child_exprs(&mut f);
                f(*body);
            }
            Self::Attrset(bindings) | Self::LetAttrset(bindings) => {
                bindings.walk_child_exprs(f);
            }
        }
    }
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

pub type Attrpath = Box<[ExprId]>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Bindings {
    pub entries: Box<[(BindingKey, BindingValue)]>,
    pub inherit_froms: Box<[ExprId]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BindingKey {
    NameDef(NameDefId),
    Name(SmolStr),
    Dynamic(ExprId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BindingValue {
    Inherit(ExprId),
    InheritFrom(u32),
    Expr(ExprId),
}

impl Bindings {
    pub(crate) fn walk_child_exprs(&self, mut f: impl FnMut(ExprId)) {
        for (key, kind) in self.entries.iter() {
            match key {
                BindingKey::NameDef(_) | BindingKey::Name(_) => {}
                &BindingKey::Dynamic(expr) => f(expr),
            }
            match *kind {
                BindingValue::Inherit(e) | BindingValue::Expr(e) => f(e),
                BindingValue::InheritFrom(_) => {}
            }
        }
    }
}
