use super::union_find::UnionFind;
use super::{known, AttrSource, TyDatabase};
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

impl AttrSource {
    fn unify(&mut self, rhs: Self) {
        if *self == Self::Unknown {
            *self = rhs;
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TyVar(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
enum Ty {
    Unknown,

    // We won't wanna infer to `null` before supporting union types.
    // It would contain no information.
    // Null,
    Bool,
    Int,
    Float,
    String,
    Path,

    List(TyVar),
    Lambda(TyVar, TyVar),
    // TODO: Add support for `rest` similar to super::Attrset.
    Attrset(Attrset),

    External(super::Ty),
}

impl Ty {
    fn intern(self, ctx: &mut InferCtx<'_>) -> TyVar {
        TyVar(ctx.table.push(self))
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
struct Attrset {
    fields: BTreeMap<SmolStr, (TyVar, AttrSource)>,
    // This is the type for all non-static fields.
    // Is this really the same as `super::Attrset::rest`?
    dyn_ty: Option<TyVar>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InferenceResult {
    name_ty_map: ArenaMap<NameId, super::Ty>,
    expr_ty_map: ArenaMap<ExprId, super::Ty>,
}

impl InferenceResult {
    pub fn ty_for_name(&self, name: NameId) -> super::Ty {
        self.name_ty_map[name].clone()
    }

    pub fn ty_for_expr(&self, expr: ExprId) -> super::Ty {
        self.expr_ty_map[expr].clone()
    }
}

pub(crate) fn infer_query(db: &dyn TyDatabase, file: FileId) -> Arc<InferenceResult> {
    let expect_ty = db.module_expected_ty(file);
    infer_with(db, file, expect_ty)
}

pub(crate) fn infer_with(
    db: &dyn TyDatabase,
    file: FileId,
    expect_ty: Option<super::Ty>,
) -> Arc<InferenceResult> {
    let module = db.module(file);
    let nameres = db.name_resolution(file);
    let table = UnionFind::new(module.names().len() + module.exprs().len(), |_| Ty::Unknown);
    let mut ctx = InferCtx {
        module: &module,
        nameres: &nameres,
        table,
    };
    let ty = ctx.infer_expr(module.entry_expr());
    if let Some(expect_ty) = expect_ty {
        ctx.unify_var_ty(ty, Ty::External(expect_ty));
    }
    Arc::new(ctx.finish())
}

struct InferCtx<'db> {
    module: &'db Module,
    nameres: &'db NameResolution,

    /// The arena for both unification and interning.
    /// First `module.names().len() + module.exprs().len()` elements are types of each names and
    /// exprs, to allow recursive definition.
    table: UnionFind<Ty>,
}

impl<'db> InferCtx<'db> {
    fn new_ty_var(&mut self) -> TyVar {
        TyVar(self.table.push(Ty::Unknown))
    }

    fn ty_for_name(&self, i: NameId) -> TyVar {
        TyVar(u32::from(i.into_raw()))
    }

    fn ty_for_expr(&self, i: ExprId) -> TyVar {
        TyVar(self.module.names().len() as u32 + u32::from(i.into_raw()))
    }

    fn import_external(&mut self, ty: super::Ty) -> TyVar {
        let ty = match ty {
            super::Ty::Unknown => Ty::Unknown,
            super::Ty::Bool => Ty::Bool,
            super::Ty::Int => Ty::Int,
            super::Ty::Float => Ty::Float,
            super::Ty::String => Ty::String,
            super::Ty::Path => Ty::Path,
            super::Ty::List(_) | super::Ty::Lambda(..) | super::Ty::Attrset(_) => Ty::External(ty),
        };
        TyVar(self.table.push(ty))
    }

    fn infer_expr(&mut self, e: ExprId) -> TyVar {
        let ty = self.infer_expr_inner(e);
        let placeholder_ty = self.ty_for_expr(e);
        self.unify_var(placeholder_ty, ty);
        ty
    }

    fn infer_expr_inner(&mut self, e: ExprId) -> TyVar {
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
                    ResolveResult::Builtin(name) => {
                        match known::BUILTINS.as_attrset().unwrap().get(name) {
                            None => self.new_ty_var(),
                            Some(ty) => self.import_external(ty.clone()),
                        }
                    }
                },
            },
            Expr::Literal(lit) => match lit {
                Literal::Int(_) => Ty::Int,
                Literal::Float(_) => Ty::Float,
                Literal::String(_) => Ty::String,
                Literal::Path(_) => Ty::Path,
            }
            .intern(self),
            Expr::Lambda(name, pat, body) => {
                let param_ty = self.new_ty_var();

                if let Some(name) = *name {
                    self.unify_var(param_ty, self.ty_for_name(name));
                }

                if let Some(pat) = pat {
                    self.unify_var_ty(param_ty, Ty::Attrset(Attrset::default()));
                    for &(name, default_expr) in pat.fields.iter() {
                        // Always infer default_expr.
                        let default_ty = default_expr.map(|e| self.infer_expr(e));
                        let Some(name) = name else { continue };
                        let name_ty = self.ty_for_name(name);
                        if let Some(default_ty) = default_ty {
                            self.unify_var(name_ty, default_ty);
                        }
                        let field_text = self.module[name].text.clone();
                        let param_field_ty = self.infer_set_field(
                            param_ty,
                            Some(field_text),
                            AttrSource::Name(name),
                        );
                        self.unify_var(param_field_ty, name_ty);
                    }
                }

                let body_ty = self.infer_expr(*body);
                Ty::Lambda(param_ty, body_ty).intern(self)
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
                self.unify_var_ty(cond_ty, Ty::Bool);
                let then_ty = self.infer_expr(then);
                let else_ty = self.infer_expr(else_);
                self.unify_var(then_ty, else_ty);
                then_ty
            }
            &Expr::Binary(op, lhs, rhs) => {
                let lhs_ty = self.infer_expr(lhs);
                let rhs_ty = self.infer_expr(rhs);

                let Some(op) = op else {
                    return self.new_ty_var();
                };

                match op {
                    BinaryOpKind::Equal | BinaryOpKind::NotEqual => Ty::Bool.intern(self),
                    BinaryOpKind::Imply | BinaryOpKind::Or | BinaryOpKind::And => {
                        self.unify_var_ty(lhs_ty, Ty::Bool);
                        self.unify_var_ty(rhs_ty, Ty::Bool);
                        Ty::Bool.intern(self)
                    }
                    BinaryOpKind::Less
                    | BinaryOpKind::Greater
                    | BinaryOpKind::LessEqual
                    | BinaryOpKind::GreaterEqual => {
                        self.unify_var(lhs_ty, rhs_ty);
                        Ty::Bool.intern(self)
                    }
                    // TODO: Polymorphism.
                    BinaryOpKind::Add
                    | BinaryOpKind::Sub
                    | BinaryOpKind::Mul
                    | BinaryOpKind::Div => {
                        // TODO: Arguments have type: int | float.
                        self.unify_var(lhs_ty, rhs_ty);
                        lhs_ty
                    }
                    BinaryOpKind::Update => {
                        self.unify_var_ty(lhs_ty, Ty::Attrset(Attrset::default()));
                        self.unify_var_ty(rhs_ty, Ty::Attrset(Attrset::default()));
                        self.unify_var(lhs_ty, rhs_ty);
                        lhs_ty
                    }
                    BinaryOpKind::Concat | BinaryOpKind::Pipe => {
                        let ret_ty = Ty::List(self.new_ty_var()).intern(self);
                        self.unify_var(lhs_ty, ret_ty);
                        self.unify_var(rhs_ty, ret_ty);
                        ret_ty
                    }
                }
            }
            &Expr::Unary(op, arg) => {
                let arg_ty = self.infer_expr(arg);
                match op {
                    None => self.new_ty_var(),
                    Some(UnaryOpKind::Not) => {
                        self.unify_var_ty(arg_ty, Ty::Bool);
                        Ty::Bool.intern(self)
                    }
                    // TODO: The argument is int | bool.
                    Some(UnaryOpKind::Negate) => arg_ty,
                }
            }
            &Expr::Apply(lam, arg) => {
                let param_ty = self.new_ty_var();
                let ret_ty = self.new_ty_var();
                let lam_ty = self.infer_expr(lam);
                self.unify_var_ty(lam_ty, Ty::Lambda(param_ty, ret_ty));
                let arg_ty = self.infer_expr(arg);
                self.unify_var(arg_ty, param_ty);
                ret_ty
            }
            Expr::HasAttr(set_expr, path) => {
                // TODO: Store the information of referenced paths somehow.
                self.infer_expr(*set_expr);
                for &attr in path.iter() {
                    let attr_ty = self.infer_expr(attr);
                    self.unify_var_ty(attr_ty, Ty::String);
                }
                Ty::Bool.intern(self)
            }
            Expr::Select(set_expr, path, default_expr) => {
                let set_ty = self.infer_expr(*set_expr);
                let ret_ty = path.iter().fold(set_ty, |set_ty, &attr| {
                    let attr_ty = self.infer_expr(attr);
                    self.unify_var_ty(attr_ty, Ty::String);
                    let opt_key = match &self.module[attr] {
                        Expr::Literal(Literal::String(key)) => Some(key.clone()),
                        _ => None,
                    };
                    self.infer_set_field(set_ty, opt_key, AttrSource::Unknown)
                });
                if let Some(default_expr) = *default_expr {
                    let default_ty = self.infer_expr(default_expr);
                    self.unify_var(ret_ty, default_ty);
                }
                ret_ty
            }
            Expr::PathInterpolation(parts) => {
                for &part in parts.iter() {
                    let ty = self.infer_expr(part);
                    // FIXME: Parts are coerce-able to string.
                    self.unify_var_ty(ty, Ty::String);
                }
                Ty::Path.intern(self)
            }
            Expr::StringInterpolation(parts) => {
                for &part in parts.iter() {
                    let ty = self.infer_expr(part);
                    // FIXME: Parts are coerce-able to string.
                    self.unify_var_ty(ty, Ty::String);
                }
                Ty::String.intern(self)
            }
            Expr::List(elems) => {
                let expect_elem_ty = self.new_ty_var();
                let ret_ty = Ty::List(expect_elem_ty).intern(self);
                for &elem in elems.iter() {
                    let elem_ty = self.infer_expr(elem);
                    self.unify_var(elem_ty, expect_elem_ty);
                }
                ret_ty
            }
            Expr::LetIn(bindings, body) => {
                self.infer_bindings(bindings);
                self.infer_expr(*body)
            }
            Expr::Attrset(bindings) | Expr::RecAttrset(bindings) => {
                let set = self.infer_bindings(bindings);
                Ty::Attrset(set).intern(self)
            }
            Expr::LetAttrset(bindings) => {
                let set = self.infer_bindings(bindings);
                let set_ty = Ty::Attrset(set).intern(self);
                self.infer_set_field(set_ty, Some("body".into()), AttrSource::Unknown)
            }
        }
    }

    fn infer_bindings(&mut self, bindings: &Bindings) -> Attrset {
        let inherit_from_tys = bindings
            .inherit_froms
            .iter()
            .map(|&from_expr| self.infer_expr(from_expr))
            .collect::<Vec<_>>();

        let mut fields = BTreeMap::new();
        for &(name, value) in bindings.statics.iter() {
            let name_ty = self.ty_for_name(name);
            let name_text = self.module[name].text.clone();
            let value_ty = match value {
                BindingValue::Inherit(e) | BindingValue::Expr(e) => self.infer_expr(e),
                BindingValue::InheritFrom(i) => self.infer_set_field(
                    inherit_from_tys[i],
                    Some(name_text.clone()),
                    AttrSource::Name(name),
                ),
            };
            self.unify_var(name_ty, value_ty);
            let src = AttrSource::Name(name);
            fields.insert(name_text, (value_ty, src));
        }

        let dyn_ty = (!bindings.dynamics.is_empty()).then(|| {
            let dyn_ty = self.new_ty_var();
            for &(k, v) in bindings.dynamics.iter() {
                let name_ty = self.infer_expr(k);
                self.unify_var_ty(name_ty, Ty::String);
                let value_ty = self.infer_expr(v);
                self.unify_var(value_ty, dyn_ty);
            }
            dyn_ty
        });

        Attrset { fields, dyn_ty }
    }

    /// `field` is `None` for dynamic fields.
    fn infer_set_field(&mut self, set_ty: TyVar, field: Option<SmolStr>, src: AttrSource) -> TyVar {
        let next_ty = TyVar(self.table.len() as u32);
        match self.table.get_mut(set_ty.0) {
            Ty::Attrset(set) => match field {
                Some(field) => match set.fields.entry(field) {
                    Entry::Occupied(mut ent) => {
                        let (ty, prev_src) = ent.get_mut();
                        prev_src.unify(src);
                        return *ty;
                    }
                    Entry::Vacant(ent) => {
                        ent.insert((next_ty, src));
                    }
                },
                None => match set.dyn_ty {
                    Some(dyn_ty) => return dyn_ty,
                    None => set.dyn_ty = Some(next_ty),
                },
            },
            Ty::External(super::Ty::Attrset(set)) => match field {
                Some(field) => {
                    if let Some(ty) = set.get(&field).cloned() {
                        return self.import_external(ty);
                    }
                }
                None => {
                    if let Some(rest) = &set.rest {
                        let rest_ty = rest.0.clone();
                        return self.import_external(rest_ty);
                    }
                }
            },
            k @ Ty::Unknown => {
                *k = Ty::Attrset(match field {
                    Some(field) => Attrset {
                        fields: [(field, (next_ty, src))].into_iter().collect(),
                        dyn_ty: None,
                    },
                    None => Attrset {
                        fields: BTreeMap::new(),
                        dyn_ty: Some(next_ty),
                    },
                });
            }
            _ => {}
        }
        self.new_ty_var()
    }

    fn unify_var_ty(&mut self, var: TyVar, rhs: Ty) {
        let lhs = mem::replace(self.table.get_mut(var.0), Ty::Unknown);
        let ret = self.unify(lhs, rhs);
        *self.table.get_mut(var.0) = ret;
    }

    fn unify_var(&mut self, lhs: TyVar, rhs: TyVar) {
        let (var, rhs) = self.table.unify(lhs.0, rhs.0);
        let Some(rhs) = rhs else { return };
        self.unify_var_ty(TyVar(var), rhs);
    }

    fn unify(&mut self, lhs: Ty, rhs: Ty) -> Ty {
        match (lhs, rhs) {
            (Ty::Unknown, other) | (other, Ty::Unknown) => other,
            (Ty::List(a), Ty::List(b)) => {
                self.unify_var(a, b);
                Ty::List(a)
            }
            (Ty::Lambda(arg1, ret1), Ty::Lambda(arg2, ret2)) => {
                self.unify_var(arg1, arg2);
                self.unify_var(ret1, ret2);
                Ty::Lambda(arg1, ret1)
            }
            (Ty::Attrset(mut a), Ty::Attrset(b)) => {
                for (field, (ty2, src2)) in b.fields {
                    match a.fields.entry(field) {
                        Entry::Vacant(ent) => {
                            ent.insert((ty2, src2));
                        }
                        Entry::Occupied(mut ent) => {
                            let (ty1, src1) = ent.get_mut();
                            src1.unify(src2);
                            self.unify_var(*ty1, ty2);
                        }
                    }
                }
                Ty::Attrset(a)
            }
            (Ty::External(external), local) | (local, Ty::External(external)) => {
                match (local, &external) {
                    (Ty::Lambda(arg1, ret1), super::Ty::Lambda(arg2, ret2)) => {
                        let arg2 = self.import_external(super::Ty::clone(arg2));
                        let ret2 = self.import_external(super::Ty::clone(ret2));
                        self.unify_var(arg1, arg2);
                        self.unify_var(ret1, ret2);
                    }
                    (Ty::Attrset(a), super::Ty::Attrset(b)) => {
                        let rest_ty_var = b
                            .rest
                            .as_ref()
                            .map(|rest| self.import_external(rest.0.clone()));
                        for (field, (ty, _)) in &a.fields {
                            if let Some(field_ty) = b.get(field) {
                                let var = self.import_external(field_ty.clone());
                                self.unify_var(*ty, var);
                            } else if let Some(var) = rest_ty_var {
                                self.unify_var(*ty, var);
                            }
                        }
                        if let (Some(dyn_ty_var), Some(var)) = (a.dyn_ty, rest_ty_var) {
                            self.unify_var(dyn_ty_var, var);
                        }
                    }
                    _ => {}
                }
                Ty::External(external)
            }
            (lhs, _) => lhs,
        }
    }

    fn finish(mut self) -> InferenceResult {
        let mut i = Collector::new(&mut self.table);

        let name_cnt = self.module.names().len();
        let expr_cnt = self.module.exprs().len();
        let mut name_ty_map = ArenaMap::with_capacity(name_cnt);
        let mut expr_ty_map = ArenaMap::with_capacity(expr_cnt);
        for (name, _) in self.module.names() {
            let ty = TyVar(u32::from(name.into_raw()));
            name_ty_map.insert(name, i.collect(ty));
        }
        for (expr, _) in self.module.exprs() {
            let ty = TyVar(name_cnt as u32 + u32::from(expr.into_raw()));
            expr_ty_map.insert(expr, i.collect(ty));
        }

        InferenceResult {
            name_ty_map,
            expr_ty_map,
        }
    }
}

/// Traverse the table and freeze all `Ty`s into immutable ones.
struct Collector<'a> {
    cache: Vec<Option<super::Ty>>,
    table: &'a mut UnionFind<Ty>,
}

impl<'a> Collector<'a> {
    fn new(table: &'a mut UnionFind<Ty>) -> Self {
        Self {
            cache: vec![None; table.len()],
            table,
        }
    }

    fn collect(&mut self, ty: TyVar) -> super::Ty {
        let i = self.table.find(ty.0);
        if let Some(ty) = self.cache[i as usize].clone() {
            return ty;
        }

        // Prevent cycles.
        self.cache[i as usize] = Some(super::Ty::Unknown);
        let ret = self.collect_uncached(i);
        self.cache[i as usize] = Some(ret.clone());
        ret
    }

    fn collect_uncached(&mut self, i: u32) -> super::Ty {
        let ty = mem::replace(self.table.get_mut(i), Ty::Unknown);
        match ty {
            Ty::Unknown => super::Ty::Unknown,
            Ty::Bool => super::Ty::Bool,
            Ty::Int => super::Ty::Int,
            Ty::Float => super::Ty::Float,
            Ty::String => super::Ty::String,
            Ty::Path => super::Ty::Path,
            Ty::List(a) => super::Ty::List(self.collect(a).into()),
            Ty::Lambda(a, b) => {
                let a = self.collect(a);
                let b = self.collect(b);
                super::Ty::Lambda(a.into(), b.into())
            }
            Ty::Attrset(fields) => {
                let fields = fields
                    .fields
                    .into_iter()
                    .map(|(name, (ty, src))| (name, self.collect(ty), src))
                    .collect();
                super::Ty::Attrset(super::Attrset { fields, rest: None })
            }
            Ty::External(ty) => ty,
        }
    }
}
