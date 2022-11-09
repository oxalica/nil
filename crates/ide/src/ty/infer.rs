use super::union_find::UnionFind;
use super::{TyDatabase, TyDisplay};
use crate::def::{
    BindingValue, Bindings, Expr, ExprId, Literal, NameId, NameResolution, ResolveResult,
};
use crate::{FileId, Module};
use la_arena::ArenaMap;
use smol_str::SmolStr;
use std::collections::btree_map::{BTreeMap, Entry};
use std::mem;
use std::sync::Arc;
use syntax::ast::{BinaryOpKind, UnaryOpKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ty(u32);

impl Ty {
    pub fn kind(self, infer: &InferenceResult) -> &TyKind {
        infer.kind(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyKind {
    Unknown,

    // We won't wanna infer to `null` before supporting union types.
    // It would contain no information.
    // Null,
    Bool,
    Int,
    Float,
    String,
    Path,

    List(Ty),
    Lambda(Ty, Ty),
    Attrset(Attrset),
}

impl TyKind {
    fn intern(self, ctx: &mut InferCtx<'_>) -> Ty {
        Ty(ctx.table.push(self))
    }

    pub fn as_attrset(&self) -> Option<&Attrset> {
        match self {
            Self::Attrset(v) => Some(v),
            _ => None,
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Attrset(BTreeMap<SmolStr, Ty>);

impl Attrset {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get(&self, field: &str) -> Option<Ty> {
        self.0.get(field).copied()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&SmolStr, Ty)> + '_ {
        self.0.iter().map(|(k, &v)| (k, v))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InferenceResult {
    name_ty_map: ArenaMap<NameId, Ty>,
    expr_ty_map: ArenaMap<ExprId, Ty>,
    arena: Vec<TyKind>,
}

impl InferenceResult {
    pub fn ty_for_name(&self, name: NameId) -> Ty {
        self.name_ty_map[name]
    }

    pub fn ty_for_expr(&self, expr: ExprId) -> Ty {
        self.expr_ty_map[expr]
    }

    pub fn kind(&self, ty: Ty) -> &TyKind {
        &self.arena[ty.0 as usize]
    }

    pub fn display_ty(&self, ty: Ty) -> TyDisplay<'_> {
        TyDisplay::new(ty, self, 2)
    }

    pub fn debug_ty(&self, ty: Ty) -> TyDisplay<'_> {
        TyDisplay::new(ty, self, usize::MAX)
    }
}

pub(crate) fn infer_query(db: &dyn TyDatabase, file: FileId) -> Arc<InferenceResult> {
    let module = db.module(file);
    let nameres = db.name_resolution(file);
    let table = UnionFind::new(module.names().len() + module.exprs().len(), |_| {
        TyKind::Unknown
    });
    let mut ctx = InferCtx {
        module: &module,
        nameres: &nameres,
        table,
    };
    ctx.infer_expr(module.entry_expr());
    let mut ret = ctx.finish();
    ret.arena.shrink_to_fit();
    Arc::new(ret)
}

struct InferCtx<'db> {
    module: &'db Module,
    nameres: &'db NameResolution,

    /// The arena for both unification and interning.
    /// First `module.names().len() + module.exprs().len()` elements are types of each names and
    /// exprs, to allow recursive definition.
    table: UnionFind<TyKind>,
}

impl<'db> InferCtx<'db> {
    fn new_ty_var(&mut self) -> Ty {
        Ty(self.table.push(TyKind::Unknown))
    }

    fn ty_for_name(&self, i: NameId) -> Ty {
        Ty(u32::from(i.into_raw()))
    }

    fn ty_for_expr(&self, i: ExprId) -> Ty {
        Ty(self.module.names().len() as u32 + u32::from(i.into_raw()))
    }

    fn infer_expr(&mut self, e: ExprId) -> Ty {
        let ty = self.infer_expr_inner(e);
        let placeholder_ty = self.ty_for_expr(e);
        self.unify(placeholder_ty, ty);
        ty
    }

    fn infer_expr_inner(&mut self, e: ExprId) -> Ty {
        match &self.module[e] {
            Expr::Missing => self.new_ty_var(),
            Expr::Reference(_) => match self.nameres.get(e) {
                None => self.new_ty_var(),
                Some(res) => match res {
                    &ResolveResult::Definition(name) => self.ty_for_name(name),
                    ResolveResult::WithExprs(_) => {
                        // TODO: With names.
                        self.new_ty_var()
                    }
                    // TODO: Builtin types.
                    ResolveResult::Builtin(_) => self.new_ty_var(),
                },
            },
            Expr::Literal(lit) => match lit {
                Literal::Int(_) => TyKind::Int,
                Literal::Float(_) => TyKind::Float,
                Literal::String(_) => TyKind::String,
                Literal::Path(_) => TyKind::Path,
            }
            .intern(self),
            Expr::Lambda(name, pat, body) => {
                let param_ty = self.new_ty_var();

                if let Some(name) = *name {
                    self.unify(param_ty, self.ty_for_name(name));
                }

                if let Some(pat) = pat {
                    self.unify_kind(param_ty, TyKind::Attrset(Attrset::default()));
                    for &(name, default_expr) in pat.fields.iter() {
                        // Always infer default_expr.
                        let default_ty = default_expr.map(|e| self.infer_expr(e));
                        let name = match name {
                            Some(name) => name,
                            None => continue,
                        };
                        let name_ty = self.ty_for_name(name);
                        if let Some(default_ty) = default_ty {
                            self.unify(name_ty, default_ty);
                        }
                        let field_text = self.module[name].text.clone();
                        let param_field_ty = self.infer_set_field(param_ty, field_text);
                        self.unify(param_field_ty, name_ty);
                    }
                }

                let body_ty = self.infer_expr(*body);
                TyKind::Lambda(param_ty, body_ty).intern(self)
            }
            &Expr::With(env, body) => {
                self.infer_expr(env);
                self.infer_expr(body)
            }
            &Expr::Assert(cond, body) => {
                self.infer_expr(cond);
                self.infer_expr(body)
            }
            &Expr::IfThenElse(cond, then, else_) => {
                let cond_ty = self.infer_expr(cond);
                self.unify_kind(cond_ty, TyKind::Bool);
                let then_ty = self.infer_expr(then);
                let else_ty = self.infer_expr(else_);
                self.unify(then_ty, else_ty);
                then_ty
            }
            &Expr::Binary(op, lhs, rhs) => {
                let lhs_ty = self.infer_expr(lhs);
                let rhs_ty = self.infer_expr(rhs);

                let op = match op {
                    None => return self.new_ty_var(),
                    Some(op) => op,
                };

                match op {
                    BinaryOpKind::Equal | BinaryOpKind::NotEqual => TyKind::Bool.intern(self),
                    BinaryOpKind::Imply | BinaryOpKind::Or | BinaryOpKind::And => {
                        self.unify_kind(lhs_ty, TyKind::Bool);
                        self.unify_kind(rhs_ty, TyKind::Bool);
                        TyKind::Bool.intern(self)
                    }
                    BinaryOpKind::Less
                    | BinaryOpKind::Greater
                    | BinaryOpKind::LessEqual
                    | BinaryOpKind::GreaterEqual => {
                        self.unify(lhs_ty, rhs_ty);
                        TyKind::Bool.intern(self)
                    }
                    // TODO: Polymorphism.
                    BinaryOpKind::Add
                    | BinaryOpKind::Sub
                    | BinaryOpKind::Mul
                    | BinaryOpKind::Div => {
                        // TODO: Arguments have type: int | float.
                        self.unify(lhs_ty, rhs_ty);
                        lhs_ty
                    }
                    BinaryOpKind::Update => {
                        self.unify_kind(lhs_ty, TyKind::Attrset(Attrset::default()));
                        self.unify_kind(rhs_ty, TyKind::Attrset(Attrset::default()));
                        self.unify(lhs_ty, rhs_ty);
                        lhs_ty
                    }
                    BinaryOpKind::Concat => {
                        let ret_ty = TyKind::List(self.new_ty_var()).intern(self);
                        self.unify(lhs_ty, ret_ty);
                        self.unify(rhs_ty, ret_ty);
                        ret_ty
                    }
                }
            }
            &Expr::Unary(op, arg) => {
                let arg_ty = self.infer_expr(arg);
                match op {
                    None => self.new_ty_var(),
                    Some(UnaryOpKind::Not) => {
                        self.unify_kind(arg_ty, TyKind::Bool);
                        TyKind::Bool.intern(self)
                    }
                    // TODO: The argument is int | bool.
                    Some(UnaryOpKind::Negate) => arg_ty,
                }
            }
            &Expr::Apply(lam, arg) => {
                let param_ty = self.new_ty_var();
                let ret_ty = self.new_ty_var();
                let lam_ty = self.infer_expr(lam);
                self.unify_kind(lam_ty, TyKind::Lambda(param_ty, ret_ty));
                let arg_ty = self.infer_expr(arg);
                self.unify(arg_ty, param_ty);
                ret_ty
            }
            Expr::HasAttr(set_expr, path) => {
                // TODO: Store the information of referenced paths somehow.
                self.infer_expr(*set_expr);
                for &attr in path.iter() {
                    let attr_ty = self.infer_expr(attr);
                    self.unify_kind(attr_ty, TyKind::String);
                }
                TyKind::Bool.intern(self)
            }
            Expr::Select(set_expr, path, default_expr) => {
                let set_ty = self.infer_expr(*set_expr);
                let ret_ty = path.iter().fold(set_ty, |set_ty, &attr| {
                    let attr_ty = self.infer_expr(attr);
                    self.unify_kind(attr_ty, TyKind::String);
                    match &self.module[attr] {
                        Expr::Literal(Literal::String(key)) => {
                            self.infer_set_field(set_ty, key.clone())
                        }
                        _ => {
                            self.unify_kind(set_ty, TyKind::Attrset(Attrset::default()));
                            self.new_ty_var()
                        }
                    }
                });
                if let Some(default_expr) = *default_expr {
                    let default_ty = self.infer_expr(default_expr);
                    self.unify(ret_ty, default_ty);
                }
                ret_ty
            }
            Expr::PathInterpolation(parts) => {
                for &part in parts.iter() {
                    let ty = self.infer_expr(part);
                    // FIXME: Parts are coerce-able to string.
                    self.unify_kind(ty, TyKind::String);
                }
                TyKind::Path.intern(self)
            }
            Expr::StringInterpolation(parts) => {
                for &part in parts.iter() {
                    let ty = self.infer_expr(part);
                    // FIXME: Parts are coerce-able to string.
                    self.unify_kind(ty, TyKind::String);
                }
                TyKind::String.intern(self)
            }
            Expr::List(elems) => {
                let expect_elem_ty = self.new_ty_var();
                let ret_ty = TyKind::List(expect_elem_ty).intern(self);
                for &elem in elems.iter() {
                    let elem_ty = self.infer_expr(elem);
                    self.unify(elem_ty, expect_elem_ty);
                }
                ret_ty
            }
            Expr::LetIn(bindings, body) => {
                self.infer_bindings(bindings);
                self.infer_expr(*body)
            }
            Expr::Attrset(bindings) | Expr::RecAttrset(bindings) => {
                let set = self.infer_bindings(bindings);
                TyKind::Attrset(set).intern(self)
            }
            Expr::LetAttrset(bindings) => {
                let set = self.infer_bindings(bindings);
                let set_ty = TyKind::Attrset(set).intern(self);
                self.infer_set_field(set_ty, "body".into())
            }
        }
    }

    fn infer_bindings(&mut self, bindings: &Bindings) -> Attrset {
        for &from_expr in bindings.inherit_froms.iter() {
            self.infer_expr(from_expr);
        }

        let mut fields = BTreeMap::new();
        for &(name, value) in bindings.statics.iter() {
            let name_ty = self.ty_for_name(name);
            let name_text = self.module[name].text.clone();
            let value_ty = match value {
                BindingValue::Inherit(e) | BindingValue::Expr(e) => self.infer_expr(e),
                BindingValue::InheritFrom(from_expr) => {
                    let from_ty = self.ty_for_expr(from_expr);
                    self.infer_set_field(from_ty, name_text.clone())
                }
            };
            self.unify(name_ty, value_ty);
            fields.insert(name_text, value_ty);
        }

        for &(k, v) in bindings.dynamics.iter() {
            let name_ty = self.infer_expr(k);
            self.unify_kind(name_ty, TyKind::String);
            self.infer_expr(v);
        }

        Attrset(fields)
    }

    fn infer_set_field(&mut self, set_ty: Ty, field: SmolStr) -> Ty {
        let next_ty = Ty(self.table.len() as u32);
        match self.table.get_mut(set_ty.0) {
            TyKind::Attrset(set) => match set.0.entry(field) {
                Entry::Occupied(ent) => return *ent.get(),
                Entry::Vacant(ent) => {
                    ent.insert(next_ty);
                }
            },
            k @ TyKind::Unknown => {
                *k = TyKind::Attrset(Attrset([(field, next_ty)].into_iter().collect()));
            }
            TyKind::Bool
            | TyKind::Int
            | TyKind::Float
            | TyKind::String
            | TyKind::Path
            | TyKind::List(_)
            | TyKind::Lambda(_, _) => {}
        }
        self.new_ty_var()
    }

    /// Unify a type in table with an expected kind.
    fn unify_kind(&mut self, a: Ty, b: TyKind) {
        match (self.table.get_mut(a.0), b) {
            (a @ TyKind::Unknown, b) => *a = b,
            (&mut TyKind::List(a), TyKind::List(b)) => self.unify(a, b),
            (&mut TyKind::Lambda(a1, a2), TyKind::Lambda(b1, b2)) => {
                self.unify(a1, b1);
                self.unify(a2, b2);
            }
            (TyKind::Attrset(_), TyKind::Attrset(b)) => {
                assert!(b.0.is_empty(), "Never unify_kind an non-empty set");
            }
            _ => {}
        }
    }

    fn unify(&mut self, a: Ty, b: Ty) {
        let (i, other) = self.table.unify(a.0, b.0);
        let other = match other {
            Some(other) => other,
            None => return,
        };
        let mut a = mem::replace(self.table.get_mut(i), TyKind::Unknown);
        match (&mut a, other) {
            (a @ TyKind::Unknown, b) => *a = b,
            (&mut TyKind::List(a), TyKind::List(b)) => {
                self.unify(a, b);
            }
            (&mut TyKind::Lambda(a1, a2), TyKind::Lambda(b1, b2)) => {
                self.unify(a1, b1);
                self.unify(a2, b2);
            }
            (TyKind::Attrset(a), TyKind::Attrset(b)) => {
                for (field, ty) in b.0 {
                    match a.0.entry(field) {
                        Entry::Vacant(ent) => {
                            ent.insert(ty);
                        }
                        Entry::Occupied(ent) => {
                            self.unify(*ent.get(), ty);
                        }
                    }
                }
            }
            _ => {}
        }
        *self.table.get_mut(i) = a;
    }

    fn finish(mut self) -> InferenceResult {
        let mut comp = TableCompresser::new(&mut self.table);

        let name_cnt = self.module.names().len();
        let mut name_ty_map = ArenaMap::default();
        let mut expr_ty_map = ArenaMap::default();
        for (name, _) in self.module.names() {
            let ty = Ty(u32::from(name.into_raw()));
            name_ty_map.insert(name, comp.intern(ty));
        }
        for (expr, _) in self.module.exprs() {
            let ty = Ty(name_cnt as u32 + u32::from(expr.into_raw()));
            expr_ty_map.insert(expr, comp.intern(ty));
        }

        InferenceResult {
            name_ty_map,
            expr_ty_map,
            arena: comp.arena,
        }
    }
}

struct TableCompresser<'a> {
    arena: Vec<TyKind>,
    cache: Vec<Option<Ty>>,
    table: &'a mut UnionFind<TyKind>,
}

impl<'a> TableCompresser<'a> {
    fn new(table: &'a mut UnionFind<TyKind>) -> Self {
        let mut arena = Vec::with_capacity(table.len());
        // Primitives.
        arena.extend([
            TyKind::Unknown,
            TyKind::Bool,
            TyKind::Int,
            TyKind::Float,
            TyKind::String,
            TyKind::Path,
        ]);
        Self {
            arena,
            cache: vec![None; table.len()],
            table,
        }
    }

    fn intern(&mut self, ty: Ty) -> Ty {
        let i = self.table.find(ty.0);
        if let Some(ty) = self.cache[i as usize] {
            return ty;
        }

        // Prevent cycles.
        self.cache[i as usize] = Some(Ty(0));
        let ret = self.intern_uncached(i);
        self.cache[i as usize] = Some(ret);
        ret
    }

    fn intern_uncached(&mut self, i: u32) -> Ty {
        let mut k = mem::replace(self.table.get_mut(i), TyKind::Unknown);
        match &mut k {
            // Matches `Self::new`.
            TyKind::Unknown => return Ty(0),
            TyKind::Bool => return Ty(1),
            TyKind::Int => return Ty(2),
            TyKind::Float => return Ty(3),
            TyKind::String => return Ty(4),
            TyKind::Path => return Ty(5),
            TyKind::List(ty) => {
                *ty = self.intern(*ty);
            }
            TyKind::Lambda(a, b) => {
                *a = self.intern(*a);
                *b = self.intern(*b);
            }
            TyKind::Attrset(set) => {
                for ty in set.0.values_mut() {
                    *ty = self.intern(*ty);
                }
            }
        };
        let ret = Ty(self.arena.len() as u32);
        self.arena.push(k);
        ret
    }
}
