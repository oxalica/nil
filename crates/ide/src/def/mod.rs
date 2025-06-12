mod kind;
mod liveness;
mod lower;
mod nameres;
mod path;

#[cfg(test)]
mod tests;

use crate::base::SourceDatabase;
use crate::{Diagnostic, FileId, SourceRootId, VfsPath};
use la_arena::{Arena, ArenaMap, Idx};
use nix_interop::DEFAULT_IMPORT_FILE;
use ordered_float::OrderedFloat;
use smallvec::SmallVec;
use smol_str::SmolStr;
use std::collections::{HashMap, HashSet};
use std::ops;
use std::sync::Arc;
use syntax::Parse;

pub use self::kind::ModuleKind;
pub use self::liveness::LivenessCheckResult;
pub use self::nameres::{ModuleScopes, NameReference, NameResolution, ResolveResult};
pub use self::path::{Path, PathAnchor, PathData};
pub use syntax::ast::{BinaryOpKind as BinaryOp, UnaryOpKind as UnaryOp};

#[salsa::query_group(DefDatabaseStorage)]
pub trait DefDatabase: SourceDatabase {
    #[salsa::interned]
    fn intern_path(&self, path_data: PathData) -> Path;

    fn parse(&self, file_id: FileId) -> Parse;

    fn module_with_source_map(&self, file_id: FileId) -> (Arc<Module>, Arc<ModuleSourceMap>);

    fn module(&self, file_id: FileId) -> Arc<Module>;

    fn source_map(&self, file_id: FileId) -> Arc<ModuleSourceMap>;

    #[salsa::invoke(ModuleKind::module_kind_query)]
    fn module_kind(&self, file_id: FileId) -> Arc<ModuleKind>;

    #[salsa::invoke(Module::module_references_query)]
    fn module_references(&self, file_id: FileId) -> Arc<HashSet<FileId>>;

    fn source_root_referrer_graph(
        &self,
        sid: SourceRootId,
    ) -> Arc<HashMap<FileId, ModuleReferrers>>;

    fn source_root_closure(&self, id: SourceRootId) -> Arc<HashSet<FileId>>;

    // The result is not wrapped in Arc. Typically, the number of referrers is just 1 or 0.
    // And also this method is not call so often.
    fn module_referrers(&self, file_id: FileId) -> ModuleReferrers;

    #[salsa::invoke(Path::resolve_path_query)]
    fn resolve_path(&self, path: Path) -> Option<VfsPath>;

    #[salsa::invoke(ModuleScopes::module_scopes_query)]
    fn scopes(&self, file_id: FileId) -> Arc<ModuleScopes>;

    #[salsa::invoke(NameResolution::name_resolution_query)]
    fn name_resolution(&self, file_id: FileId) -> Arc<NameResolution>;

    #[salsa::invoke(NameReference::name_reference_query)]
    fn name_reference(&self, file_id: FileId) -> Arc<NameReference>;

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
    let (mut module, mut source_map) = lower::lower(db, file_id, parse);
    module.shrink_to_fit();
    source_map.shrink_to_fit();
    (Arc::new(module), Arc::new(source_map))
}

fn module(db: &dyn DefDatabase, file_id: FileId) -> Arc<Module> {
    db.module_with_source_map(file_id).0
}

fn source_map(db: &dyn DefDatabase, file_id: FileId) -> Arc<ModuleSourceMap> {
    db.module_with_source_map(file_id).1
}

pub type ModuleReferrers = SmallVec<[FileId; 2]>;

fn source_root_referrer_graph(
    db: &dyn DefDatabase,
    sid: SourceRootId,
) -> Arc<HashMap<FileId, ModuleReferrers>> {
    // Assert our inline threshould costs no extra memory.
    const _: [(); std::mem::size_of::<Vec<FileId>>()] =
        [(); std::mem::size_of::<ModuleReferrers>()];

    let source_root = db.source_root(sid);
    let mut graph = HashMap::<FileId, ModuleReferrers>::with_capacity(source_root.files().len());
    for (file, _) in source_root.files() {
        for &referrer in &*db.module_references(file) {
            // This never duplicates, since `module_references` returns a `HashSet`.
            graph.entry(referrer).or_default().push(file);
        }
    }

    // Keep the order deterministic.
    for referees in graph.values_mut() {
        referees.sort();
    }

    graph.shrink_to_fit();
    Arc::new(graph)
}

fn module_referrers(db: &dyn DefDatabase, file_id: FileId) -> ModuleReferrers {
    let sid = db.file_source_root(file_id);
    let graph = db.source_root_referrer_graph(sid);
    graph.get(&file_id).cloned().unwrap_or_default()
}

fn source_root_closure(db: &dyn DefDatabase, id: SourceRootId) -> Arc<HashSet<FileId>> {
    let Some(entry) = db.source_root(id).entry() else {
        return Arc::default();
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
    closure.shrink_to_fit();
    Arc::new(closure)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    exprs: Arena<Expr>,
    names: Arena<Name>,
    entry_expr: ExprId,
}

pub type ExprId = Idx<Expr>;
pub type NameId = Idx<Name>;

impl ops::Index<ExprId> for Module {
    type Output = Expr;
    fn index(&self, index: ExprId) -> &Self::Output {
        &self.exprs[index]
    }
}

impl ops::Index<NameId> for Module {
    type Output = Name;
    fn index(&self, index: NameId) -> &Self::Output {
        &self.names[index]
    }
}

impl Module {
    pub fn shrink_to_fit(&mut self) {
        self.exprs.shrink_to_fit();
        self.names.shrink_to_fit();
    }

    pub fn entry_expr(&self) -> ExprId {
        self.entry_expr
    }

    pub fn exprs(&self) -> impl ExactSizeIterator<Item = (ExprId, &'_ Expr)> + '_ {
        self.exprs.iter()
    }

    pub fn names(&self) -> impl ExactSizeIterator<Item = (NameId, &'_ Name)> + '_ {
        self.names.iter()
    }

    pub(crate) fn module_references_query(
        db: &dyn DefDatabase,
        file_id: FileId,
    ) -> Arc<HashSet<FileId>> {
        let source_root = db.source_root(db.file_source_root(file_id));
        let mut refs = db
            .module(file_id)
            .exprs()
            .filter_map(|(_, kind)| {
                let &Expr::Literal(Literal::Path(path)) = kind else {
                    return None;
                };
                let mut vpath = path.resolve(db)?;
                source_root.file_for_path(&vpath).or_else(|| {
                    vpath.push(DEFAULT_IMPORT_FILE)?;
                    source_root.file_for_path(&vpath)
                })
            })
            .collect::<HashSet<_>>();
        refs.shrink_to_fit();
        Arc::new(refs)
    }
}

pub type AstPtr = syntax::SyntaxNodePtr;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct ModuleSourceMap {
    expr_map: HashMap<AstPtr, ExprId>,
    expr_map_rev: HashMap<ExprId, AstPtr>,
    name_map: HashMap<AstPtr, NameId>,
    name_map_rev: ArenaMap<NameId, Vec<AstPtr>>,

    // This contains locations, thus is quite volatile.
    diagnostics: Vec<Diagnostic>,
}

impl ModuleSourceMap {
    pub fn shrink_to_fit(&mut self) {
        self.expr_map.shrink_to_fit();
        self.expr_map_rev.shrink_to_fit();
        self.name_map.shrink_to_fit();
        self.name_map_rev.shrink_to_fit();
        self.diagnostics.shrink_to_fit();
    }

    pub fn expr_for_node(&self, node: AstPtr) -> Option<ExprId> {
        self.expr_map.get(&node).copied()
    }

    pub fn node_for_expr(&self, expr_id: ExprId) -> Option<AstPtr> {
        self.expr_map_rev.get(&expr_id).cloned()
    }

    pub fn name_for_node(&self, node: AstPtr) -> Option<NameId> {
        self.name_map.get(&node).copied()
    }

    pub fn nodes_for_name(&self, name_id: NameId) -> impl Iterator<Item = AstPtr> + '_ {
        self.name_map_rev
            .get(name_id)
            .into_iter()
            .flatten()
            .cloned()
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Missing,
    Reference(SmolStr),
    Literal(Literal),
    Lambda(Option<NameId>, Option<Pat>, ExprId),
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
    CurPos,
}

impl Expr {
    pub fn walk_child_exprs(&self, mut f: impl FnMut(ExprId)) {
        match self {
            Self::Missing | Self::Reference(_) | Self::Literal(_) | Self::CurPos => {}
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
                xs.iter().copied().for_each(f);
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
pub struct Name {
    pub text: SmolStr,
    pub kind: NameKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NameKind {
    LetIn,
    PlainAttrset,
    RecAttrset,
    Param,
    PatField,
}

impl NameKind {
    pub fn is_definition(self) -> bool {
        !matches!(self, Self::PlainAttrset)
    }
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
    pub fields: Box<[(Option<NameId>, Option<ExprId>)]>,
    pub ellipsis: bool,
}

pub type Attrpath = Box<[ExprId]>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Bindings {
    pub statics: Box<[(NameId, BindingValue)]>,
    pub inherit_froms: Box<[ExprId]>,
    pub dynamics: Box<[(ExprId, ExprId)]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BindingValue {
    Expr(ExprId),
    Inherit(ExprId),
    InheritFrom(usize),
}

impl Bindings {
    pub fn walk_child_exprs(&self, mut f: impl FnMut(ExprId)) {
        for (_, value) in self.statics.iter() {
            match value {
                BindingValue::Inherit(e) | BindingValue::Expr(e) => f(*e),
                BindingValue::InheritFrom(_idx) => {}
            }
        }
        for &e in self.inherit_froms.iter() {
            f(e);
        }
        for &(k, v) in self.dynamics.iter() {
            f(k);
            f(v);
        }
    }

    // FIXME: This is currently O(n).
    pub fn get(&self, name: &str, module: &Module) -> Option<BindingValue> {
        self.statics
            .iter()
            .find_map(|&(name_id, value)| (module[name_id].text == name).then_some(value))
    }
}
