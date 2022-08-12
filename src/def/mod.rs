mod liveness;
mod lower;
mod path;
mod scope;

#[cfg(test)]
mod tests;

use crate::base::SourceDatabase;
use crate::{Diagnostic, FileId, SourceRootId};
use la_arena::{Arena, ArenaMap, Idx};
use ordered_float::OrderedFloat;
use smol_str::SmolStr;
use std::collections::{HashMap, HashSet};
use std::ops;
use std::sync::Arc;
use syntax::Parse;

pub use self::liveness::LivenessCheckResult;
pub use self::path::{Path, PathAnchor, PathData};
pub use self::scope::{ModuleScopes, NameReferenceMap, ResolveResult, ScopeData, ScopeId};
pub use syntax::ast::{BinaryOpKind as BinaryOp, UnaryOpKind as UnaryOp};

#[salsa::query_group(DefDatabaseStorage)]
pub trait DefDatabase: SourceDatabase {
    #[salsa::interned]
    fn intern_path(&self, path_data: PathData) -> Path;

    fn parse(&self, file_id: FileId) -> Parse;

    fn module_with_source_map(&self, file_id: FileId) -> (Arc<Module>, Arc<ModuleSourceMap>);

    fn module(&self, file_id: FileId) -> Arc<Module>;

    fn source_map(&self, file_id: FileId) -> Arc<ModuleSourceMap>;

    #[salsa::invoke(Module::module_references_query)]
    fn module_references(&self, file_id: FileId) -> Arc<HashSet<FileId>>;

    fn source_root_closure(&self, id: SourceRootId) -> Arc<HashSet<FileId>>;

    #[salsa::invoke(Path::resolve_path_query)]
    fn resolve_path(&self, path: Path) -> Option<FileId>;

    #[salsa::invoke(ModuleScopes::module_scopes_query)]
    fn scopes(&self, file_id: FileId) -> Arc<ModuleScopes>;

    #[salsa::invoke(ModuleScopes::resolve_name_query)]
    fn resolve_name(&self, file_id: FileId, expr_id: ExprId) -> Option<ResolveResult>;

    #[salsa::invoke(NameReferenceMap::name_reference_map_query)]
    fn name_reference_map(&self, file_id: FileId) -> Arc<NameReferenceMap>;

    #[salsa::invoke(liveness::liveness_check_query)]
    fn liveness_check(&self, file_id: FileId) -> Arc<LivenessCheckResult>;
}

fn parse(db: &dyn DefDatabase, file_id: FileId) -> Parse {
    let content = db.file_content(file_id);
    syntax::parse_file(&content)
}

fn module_with_source_map(
    db: &dyn DefDatabase,
    file_id: FileId,
) -> (Arc<Module>, Arc<ModuleSourceMap>) {
    let parse = db.parse(file_id);
    let (module, source_map) = lower::lower(db, file_id, parse);
    (Arc::new(module), Arc::new(source_map))
}

fn module(db: &dyn DefDatabase, file_id: FileId) -> Arc<Module> {
    db.module_with_source_map(file_id).0
}

fn source_map(db: &dyn DefDatabase, file_id: FileId) -> Arc<ModuleSourceMap> {
    db.module_with_source_map(file_id).1
}

fn source_root_closure(db: &dyn DefDatabase, id: SourceRootId) -> Arc<HashSet<FileId>> {
    let entry = match db.source_root(id).entry() {
        Some(file) => file,
        None => return Default::default(),
    };
    let mut closure = HashSet::new();
    closure.insert(entry);
    let mut queue = vec![entry];
    while let Some(file) = queue.pop() {
        for &target in db.module_references(file).iter() {
            if closure.insert(target) {
                queue.push(target);
            }
        }
    }
    Arc::new(closure)
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

    pub fn name_defs(
        &self,
    ) -> impl Iterator<Item = (NameDefId, &'_ NameDef)> + ExactSizeIterator + '_ {
        self.name_defs.iter()
    }

    pub(crate) fn module_references_query(
        db: &dyn DefDatabase,
        file_id: FileId,
    ) -> Arc<HashSet<FileId>> {
        let refs = db
            .module(file_id)
            .exprs()
            .filter_map(|(_, kind)| match kind {
                &Expr::Literal(Literal::Path(p)) => Some(p),
                _ => None,
            })
            .filter_map(|path| path.resolve(db))
            .collect();
        Arc::new(refs)
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
    PathInterpolation(Box<[ExprId]>),
    List(Box<[ExprId]>),
    LetIn(Bindings, ExprId),
    Attrset(Bindings),
    LetAttrset(Bindings),
    RecAttrset(Bindings),
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
            Self::StringInterpolation(xs) | Self::List(xs) | Self::PathInterpolation(xs) => {
                xs.iter().copied().for_each(f)
            }
            Self::LetIn(bindings, body) => {
                bindings.walk_child_exprs(&mut f);
                f(*body);
            }
            Self::Attrset(bindings) | Self::RecAttrset(bindings) | Self::LetAttrset(bindings) => {
                bindings.walk_child_exprs(f);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NameDef {
    pub name: SmolStr,
    pub kind: NameDefKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NameDefKind {
    LetIn,
    RecAttrset,
    Param,
    PatField,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Int(i64),
    Float(OrderedFloat<f64>),
    String(SmolStr),
    Path(Path),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pat {
    pub fields: Box<[(Option<NameDefId>, Option<ExprId>)]>,
    pub ellipsis: bool,
}

pub type Attrpath = Box<[ExprId]>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Bindings {
    pub statics: Box<[(BindingKey, BindingValue)]>,
    pub inherit_froms: Box<[ExprId]>,
    pub dynamics: Box<[(ExprId, ExprId)]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BindingKey {
    NameDef(NameDefId),
    Name(SmolStr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BindingValue {
    Inherit(ExprId),
    InheritFrom(ExprId),
    Expr(ExprId),
}

impl Bindings {
    pub(crate) fn walk_child_exprs(&self, mut f: impl FnMut(ExprId)) {
        for (_, value) in self.statics.iter() {
            match value {
                BindingValue::Inherit(e) | BindingValue::Expr(e) => f(*e),
                // Walking here would be redundant, we traverse them outside `entries` loop.
                BindingValue::InheritFrom(_) => {}
            }
        }
        for &e in self.inherit_froms.iter() {
            f(e);
        }
    }

    pub(crate) fn walk_child_defs(&self, mut f: impl FnMut(NameDefId, &BindingValue)) {
        for (key, value) in self.statics.iter() {
            if let &BindingKey::NameDef(def) = key {
                f(def, value);
            }
        }
    }
}
