use super::{
    AstPtr, Attrpath, BindingValue, Bindings, DefDatabase, Expr, ExprId, Literal, Module,
    ModuleSourceMap, Name, NameId, NameKind, Pat, PathAnchor, PathData,
};
use crate::{Diagnostic, DiagnosticKind, FileId, FileRange};
use indexmap::IndexMap;
use la_arena::Arena;
use rowan::ast::AstNode;
use smol_str::SmolStr;
use std::collections::HashMap;
use syntax::ast::{self, HasStringParts, LiteralKind};
use syntax::semantic::{
    unescape_string_literal, AttrKind, BindingDesugar, BindingValueKind, HasBindingsDesugar,
};
use syntax::Parse;

pub(super) fn lower(
    db: &dyn DefDatabase,
    file_id: FileId,
    parse: Parse,
) -> (Module, ModuleSourceMap) {
    let mut ctx = LowerCtx {
        db,
        file_id,
        module: Module {
            exprs: Arena::new(),
            names: Arena::new(),
            // Placeholder.
            entry_expr: ExprId::from_raw(0.into()),
            diagnostics: Vec::new(),
        },
        source_map: ModuleSourceMap::default(),
    };

    let entry = ctx.lower_expr_opt(parse.root().expr());
    let mut module = ctx.module;
    module.entry_expr = entry;
    (module, ctx.source_map)
}

struct LowerCtx<'a> {
    db: &'a dyn DefDatabase,
    file_id: FileId,
    module: Module,
    source_map: ModuleSourceMap,
}

impl LowerCtx<'_> {
    fn alloc_expr(&mut self, expr: Expr, ptr: AstPtr) -> ExprId {
        let id = self.module.exprs.alloc(expr);
        self.source_map.expr_map.insert(ptr.clone(), id);
        self.source_map.expr_map_rev.insert(id, ptr);
        id
    }

    fn alloc_name(&mut self, text: SmolStr, kind: NameKind, ptr: AstPtr) -> NameId {
        let id = self.module.names.alloc(Name { text, kind });
        self.source_map.name_map.insert(ptr.clone(), id);
        self.source_map.name_map_rev.insert(id, vec![ptr]);
        id
    }

    fn diagnostic(&mut self, diag: Diagnostic) {
        self.module.diagnostics.push(diag);
    }

    fn lower_expr_opt(&mut self, expr: Option<ast::Expr>) -> ExprId {
        if let Some(expr) = expr {
            return self.lower_expr(expr);
        }
        // Synthetic syntax has no coresponding text.
        self.module.exprs.alloc(Expr::Missing)
    }

    fn lower_expr(&mut self, expr: ast::Expr) -> ExprId {
        let ptr = AstPtr::new(expr.syntax());
        match expr {
            ast::Expr::Literal(e) => {
                let lit = self.lower_literal(e);
                self.alloc_expr(lit.map_or(Expr::Missing, Expr::Literal), ptr)
            }
            ast::Expr::Ref(e) => {
                let name = e
                    .token()
                    .map_or_else(Default::default, |tok| tok.text().into());
                self.alloc_expr(Expr::Reference(name), ptr)
            }
            ast::Expr::Apply(e) => {
                let func = self.lower_expr_opt(e.function());
                let arg = self.lower_expr_opt(e.argument());
                self.alloc_expr(Expr::Apply(func, arg), ptr)
            }
            ast::Expr::Paren(e) => self.lower_expr_opt(e.expr()),
            ast::Expr::Lambda(e) => self.lower_lambda(e, ptr),
            ast::Expr::Assert(e) => {
                let cond = self.lower_expr_opt(e.condition());
                let body = self.lower_expr_opt(e.body());
                self.alloc_expr(Expr::Assert(cond, body), ptr)
            }
            ast::Expr::IfThenElse(e) => {
                let cond = self.lower_expr_opt(e.condition());
                let then_body = self.lower_expr_opt(e.then_body());
                let else_body = self.lower_expr_opt(e.else_body());
                self.alloc_expr(Expr::IfThenElse(cond, then_body, else_body), ptr)
            }
            ast::Expr::With(e) => {
                let env = self.lower_expr_opt(e.environment());
                let body = self.lower_expr_opt(e.body());
                self.alloc_expr(Expr::With(env, body), ptr)
            }
            ast::Expr::BinaryOp(e) => {
                let lhs = self.lower_expr_opt(e.lhs());
                let op = e.op_kind();
                let rhs = self.lower_expr_opt(e.rhs());
                self.alloc_expr(Expr::Binary(op, lhs, rhs), ptr)
            }
            ast::Expr::UnaryOp(e) => {
                let op = e.op_kind();
                let arg = self.lower_expr_opt(e.arg());
                self.alloc_expr(Expr::Unary(op, arg), ptr)
            }
            ast::Expr::HasAttr(e) => {
                let set = self.lower_expr_opt(e.set());
                let attrpath = self.lower_attrpath_opt(e.attrpath());
                self.alloc_expr(Expr::HasAttr(set, attrpath), ptr)
            }
            ast::Expr::Select(e) => {
                let set = self.lower_expr_opt(e.set());
                let attrpath = self.lower_attrpath_opt(e.attrpath());
                let default_expr = e.default_expr().map(|e| self.lower_expr(e));
                self.alloc_expr(Expr::Select(set, attrpath, default_expr), ptr)
            }
            ast::Expr::String(s) => self.lower_string(&s),
            ast::Expr::IndentString(s) => self.lower_string_interpolation(&s),
            ast::Expr::List(e) => {
                let elements = e.elements().map(|e| self.lower_expr(e)).collect();
                self.alloc_expr(Expr::List(elements), ptr)
            }
            ast::Expr::LetIn(e) => {
                let bindings = MergingSet::desugar(self, NameKind::LetIn, &e).finish(self);
                if bindings.statics.is_empty() && bindings.inherit_froms.is_empty() {
                    let let_tok_range = e
                        .let_token()
                        .map_or(e.syntax().text_range(), |tok| tok.text_range());
                    let let_in_header = e
                        .in_token()
                        .map_or(let_tok_range, |tok| tok.text_range().cover(let_tok_range));
                    self.diagnostic(Diagnostic::new(let_in_header, DiagnosticKind::EmptyLetIn));
                }
                let body = self.lower_expr_opt(e.body());
                self.alloc_expr(Expr::LetIn(bindings, body), ptr)
            }
            ast::Expr::AttrSet(e) => {
                let ctor = if e.rec_token().is_some() {
                    Expr::RecAttrset
                } else if e.let_token().is_some() {
                    self.diagnostic(Diagnostic::new(
                        e.syntax().text_range(),
                        DiagnosticKind::LetAttrset,
                    ));
                    Expr::LetAttrset
                } else {
                    Expr::Attrset
                };
                let bindings = MergingSet::desugar(self, name_kind_of_set(&e), &e).finish(self);
                self.alloc_expr(ctor(bindings), ptr)
            }
            ast::Expr::PathInterpolation(e) => {
                let parts = e
                    .path_parts()
                    .filter_map(|part| match part {
                        ast::PathPart::Fragment(_) => None,
                        ast::PathPart::Dynamic(d) => Some(self.lower_expr_opt(d.expr())),
                    })
                    .collect();
                self.alloc_expr(Expr::PathInterpolation(parts), ptr)
            }
        }
    }

    fn lower_lambda(&mut self, lam: ast::Lambda, ptr: AstPtr) -> ExprId {
        let mut param_locs = HashMap::new();
        let mut lower_name = |this: &mut Self, node: ast::Name, kind: NameKind| -> NameId {
            let ptr = AstPtr::new(node.syntax());
            let text = match node.token() {
                None => "".into(),
                Some(tok) => {
                    let text: SmolStr = tok.text().into();
                    if let Some(prev_loc) = param_locs.insert(text.clone(), ptr.text_range()) {
                        this.diagnostic(
                            Diagnostic::new(ptr.text_range(), DiagnosticKind::DuplicatedParam)
                                .with_note(
                                    FileRange::new(this.file_id, prev_loc),
                                    "Previously defined here",
                                ),
                        );
                    }
                    text
                }
            };
            this.alloc_name(text, kind, ptr)
        };

        let (param, pat) = lam.param().map_or((None, None), |param| {
            let name = param.name().map(|n| lower_name(self, n, NameKind::Param));
            let pat = param.pat().map(|pat| {
                let fields = pat
                    .fields()
                    .map(|field| {
                        let field_name = field
                            .name()
                            .map(|n| lower_name(self, n, NameKind::PatField));
                        let default_expr = field.default_expr().map(|e| self.lower_expr(e));
                        (field_name, default_expr)
                    })
                    .collect();
                let ellipsis = pat.ellipsis_token().is_some();
                Pat { fields, ellipsis }
            });
            (name, pat)
        });
        let body = self.lower_expr_opt(lam.body());
        self.alloc_expr(Expr::Lambda(param, pat, body), ptr)
    }

    fn lower_literal(&mut self, lit: ast::Literal) -> Option<Literal> {
        let kind = lit.kind()?;
        let tok = lit.token().unwrap();
        let mut text = tok.text();

        Some(match kind {
            LiteralKind::Int => Literal::Int(text.parse::<i64>().ok()?),
            LiteralKind::Float => Literal::Float(text.parse::<f64>().unwrap().into()),
            LiteralKind::Uri => {
                self.diagnostic(Diagnostic::new(
                    lit.syntax().text_range(),
                    DiagnosticKind::UriLiteral,
                ));
                Literal::String(text.into())
            }
            LiteralKind::SearchPath => {
                text = &text[1..text.len() - 1]; // Strip '<' and '>'.
                let (search_name, text) = text.split_once('/').unwrap_or((text, ""));
                let anchor = PathAnchor::Search(search_name.into());
                let path = self.db.intern_path(PathData::normalize(anchor, text));
                Literal::Path(path)
            }
            LiteralKind::Path => {
                let anchor = match text.as_bytes()[0] {
                    b'/' => PathAnchor::Absolute,
                    b'~' => {
                        text = text.strip_prefix('~').unwrap();
                        PathAnchor::Home
                    }
                    _ => PathAnchor::Relative(self.file_id),
                };
                let path = self.db.intern_path(PathData::normalize(anchor, text));
                Literal::Path(path)
            }
        })
    }

    fn lower_attrpath_opt(&mut self, attrpath: Option<ast::Attrpath>) -> Attrpath {
        attrpath
            .into_iter()
            .flat_map(|attrpath| attrpath.attrs())
            .map(|attr| match attr {
                ast::Attr::Dynamic(d) => self.lower_expr_opt(d.expr()),
                ast::Attr::Name(n) => {
                    let name = n
                        .token()
                        .map_or_else(Default::default, |tok| tok.text().into());
                    let ptr = AstPtr::new(n.syntax());
                    self.alloc_expr(Expr::Literal(Literal::String(name)), ptr)
                }
                ast::Attr::String(s) => self.lower_string(&s),
            })
            .collect()
    }

    fn lower_string(&mut self, n: &ast::String) -> ExprId {
        let ptr = AstPtr::new(n.syntax());
        // Special case for literal strings.
        // They are subjects to URI detection.
        match unescape_string_literal(n) {
            Some(lit) => self.alloc_expr(Expr::Literal(Literal::String(lit.into())), ptr),
            None => self.lower_string_interpolation(n),
        }
    }

    fn lower_string_interpolation(&mut self, n: &impl HasStringParts) -> ExprId {
        let ptr = AstPtr::new(n.syntax());
        let parts = n
            .string_parts()
            .filter_map(|part| {
                match part {
                    ast::StringPart::Dynamic(d) => Some(self.lower_expr_opt(d.expr())),
                    // Currently we don't encode literal fragments.
                    ast::StringPart::Fragment(_) | ast::StringPart::Escape(_) => None,
                }
            })
            .collect();
        self.alloc_expr(Expr::StringInterpolation(parts), ptr)
    }
}

fn name_kind_of_set(set: &ast::AttrSet) -> NameKind {
    if set.rec_token().is_some() || set.let_token().is_some() {
        NameKind::RecAttrset
    } else {
        NameKind::PlainAttrset
    }
}

#[derive(Debug)]
struct MergingSet {
    name_kind: NameKind,
    /// The span of this set if it's from an {Rec,}Attrset literal.
    /// Otherwise, it's None.
    ptr: Option<AstPtr>,
    statics: IndexMap<SmolStr, MergingEntry>,
    inherit_froms: Vec<ExprId>,
    dynamics: Vec<(ExprId, ExprId)>,
}

#[derive(Debug)]
struct MergingEntry {
    /// The key of this entry.
    /// Used for tracking source map.
    name: NameId,
    /// The RHS if it is implicit or explicit set.
    /// We stores both `set` and `value` components to prevent information loss
    /// when handling duplicated keys.
    set: Option<MergingSet>,
    /// The RHS if it is not merge-able.
    /// The source location is for error reporting.
    value: Option<(AstPtr, BindingValue)>,
}

impl MergingSet {
    fn new(name_kind: NameKind, ptr: Option<AstPtr>) -> Self {
        Self {
            name_kind,
            ptr,
            statics: IndexMap::default(),
            inherit_froms: Vec::new(),
            dynamics: Vec::new(),
        }
    }

    fn desugar(ctx: &mut LowerCtx, name_kind: NameKind, n: &impl HasBindingsDesugar) -> Self {
        let ptr = n.expr_syntax().map(AstPtr::new);
        let mut this = Self::new(name_kind, ptr);
        this.merge_bindings(ctx, n);
        this
    }

    fn merge_bindings(&mut self, ctx: &mut LowerCtx, n: &impl HasBindingsDesugar) {
        for b in n.desugar_bindings() {
            match b {
                BindingDesugar::Inherit(i) => self.merge_inherit(ctx, i),
                BindingDesugar::AttrValue(Some(attr), value) => {
                    self.merge_attr_value(ctx, attr, value);
                }
                // Just recover. The error is already reported by the parser.
                BindingDesugar::AttrValue(None, value) => {
                    self.push_dynamic(ctx, None, value);
                }
            }
        }
    }

    fn merge_inherit(&mut self, ctx: &mut LowerCtx, i: ast::Inherit) {
        let from_expr = i.from_expr().map(|e| {
            let expr = ctx.lower_expr_opt(e.expr());
            self.inherit_froms.push(expr);
            expr
        });

        if i.attrs().next().is_none() {
            ctx.diagnostic(Diagnostic::new(
                i.syntax().text_range(),
                DiagnosticKind::EmptyInherit,
            ));
            return;
        }

        for attr in i.attrs() {
            let attr_ptr = AstPtr::new(attr.syntax());
            let key = match AttrKind::of(attr) {
                AttrKind::Static(key) => SmolStr::from(key.unwrap_or_default()),
                // `inherit ${expr}` or `inherit (expr) ${expr}` is invalid.
                AttrKind::Dynamic(expr) => {
                    ctx.diagnostic(Diagnostic::new(
                        attr_ptr.text_range(),
                        DiagnosticKind::InvalidDynamic,
                    ));
                    self.push_dynamic(ctx, None, BindingValueKind::Expr(expr));
                    continue;
                }
            };

            let value = match from_expr {
                Some(e) => BindingValue::InheritFrom(e),
                None => {
                    let ref_expr = ctx.alloc_expr(Expr::Reference(key.clone()), attr_ptr.clone());
                    BindingValue::Inherit(ref_expr)
                }
            };
            self.merge_static_value(ctx, key, attr_ptr, value);
        }
    }

    /// Push a dynamic Attr. This is also used for error recovery,
    /// so InvalidDynamic is not checked here.
    fn push_dynamic(
        &mut self,
        ctx: &mut LowerCtx,
        key_expr: Option<ast::Expr>,
        value: BindingValueKind,
    ) {
        let key_expr = ctx.lower_expr_opt(key_expr);
        let value_expr = match value {
            BindingValueKind::Expr(e) => ctx.lower_expr_opt(e),
            BindingValueKind::ImplicitSet(set) => {
                Self::desugar(ctx, NameKind::PlainAttrset, &set).finish_expr(ctx)
            }
            BindingValueKind::ExplicitSet(set) => {
                Self::desugar(ctx, name_kind_of_set(&set), &set).finish_expr(ctx)
            }
        };
        self.dynamics.push((key_expr, value_expr));
    }

    fn merge_attr_value(&mut self, ctx: &mut LowerCtx, attr: ast::Attr, value: BindingValueKind) {
        let attr_ptr = AstPtr::new(attr.syntax());
        match AttrKind::of(attr) {
            AttrKind::Static(key) => {
                let key = SmolStr::new(key.unwrap_or_default());
                match value {
                    BindingValueKind::Expr(e) => {
                        let e = ctx.lower_expr_opt(e);
                        self.merge_static_value(ctx, key, attr_ptr, BindingValue::Expr(e));
                    }
                    BindingValueKind::ImplicitSet(set) => {
                        self.merge_static_set(ctx, key, attr_ptr, &set, NameKind::PlainAttrset);
                    }
                    BindingValueKind::ExplicitSet(set) => {
                        self.merge_static_set(ctx, key, attr_ptr, &set, name_kind_of_set(&set));
                    }
                }
            }
            AttrKind::Dynamic(key_expr) => {
                if self.name_kind == NameKind::LetIn {
                    ctx.diagnostic(Diagnostic::new(
                        attr_ptr.text_range(),
                        DiagnosticKind::InvalidDynamic,
                    ));
                    // We don't skip the RHS but still process it as a recovery.
                }
                self.push_dynamic(ctx, key_expr, value)
            }
        }
    }

    fn merge_static_value(
        &mut self,
        ctx: &mut LowerCtx,
        key: SmolStr,
        attr_ptr: AstPtr,
        value: BindingValue,
    ) {
        self.statics
            .entry(key.clone())
            // Set-value or value-value collision.
            .and_modify(|ent| {
                // Append this location to the existing name.
                ctx.source_map.name_map.insert(attr_ptr.clone(), ent.name);
                ctx.source_map.name_map_rev[ent.name].push(attr_ptr.clone());

                let prev_ptr = ctx.source_map.nodes_for_name(ent.name).next().unwrap();
                ctx.diagnostic(
                    Diagnostic::new(attr_ptr.text_range(), DiagnosticKind::DuplicatedKey)
                        .with_note(
                            FileRange::new(ctx.file_id, prev_ptr.text_range()),
                            "Previously defined here",
                        ),
                );
            })
            .or_insert_with(|| MergingEntry {
                name: ctx.alloc_name(key, self.name_kind, attr_ptr.clone()),
                set: None,
                value: Some((attr_ptr, value)),
            });
    }

    fn merge_static_set(
        &mut self,
        ctx: &mut LowerCtx,
        key: SmolStr,
        attr_ptr: AstPtr,
        n: &impl HasBindingsDesugar,
        child_name_kind: NameKind,
    ) {
        self.statics
            .entry(key.clone())
            .and_modify(|ent| {
                // Append this location to the existing name.
                ctx.source_map.name_map.insert(attr_ptr.clone(), ent.name);
                ctx.source_map.name_map_rev[ent.name].push(attr_ptr.clone());

                if let Some(prev_set) = &mut ent.set {
                    // Erase the source information when merging occurs.
                    // Since the previous definition is not exhaustive now.
                    // `{ a = { b = 1; }; a.c = 2; }`
                    prev_set.ptr = None;

                    if prev_set.name_kind.is_definition() {
                        ctx.diagnostic(Diagnostic::new(
                            attr_ptr.text_range(),
                            DiagnosticKind::MergeRecAttrset,
                        ));
                    } else if child_name_kind.is_definition() {
                        ctx.diagnostic(Diagnostic::new(
                            attr_ptr.text_range(),
                            DiagnosticKind::MergePlainRecAttrset,
                        ));
                    }
                }

                // Value-set collision.
                // N.B. Report the previous value's definition, not the first one.
                // `{ a.b = 1; a = 2; a.c = 3; }`.
                //                      ^ Collides with `a =`, not `a.b =`
                if let Some((prev_ptr, _)) = &ent.value {
                    ctx.diagnostic(
                        Diagnostic::new(attr_ptr.text_range(), DiagnosticKind::DuplicatedKey)
                            .with_note(
                                FileRange::new(ctx.file_id, prev_ptr.text_range()),
                                "Previously defined here",
                            ),
                    );
                }
            })
            // Otherwise, insert a new entry.
            .or_insert_with(|| MergingEntry {
                name: ctx.alloc_name(key, self.name_kind, attr_ptr),
                set: None,
                value: None,
            })
            .set
            // If the previous entry is not a set, make it to be.
            .get_or_insert_with(|| Self::new(child_name_kind, n.expr_syntax().map(AstPtr::new)))
            .merge_bindings(ctx, n);
    }

    fn finish(self, ctx: &mut LowerCtx) -> Bindings {
        Bindings {
            statics: self
                .statics
                .into_values()
                .map(|entry| {
                    let value = match entry.set {
                        Some(set) => BindingValue::Expr(set.finish_expr(ctx)),
                        None => entry.value.unwrap().1,
                    };
                    (entry.name, value)
                })
                .collect(),
            inherit_froms: self.inherit_froms.into(),
            dynamics: self.dynamics.into(),
        }
    }

    fn finish_expr(mut self, ctx: &mut LowerCtx) -> ExprId {
        let ctor = match self.name_kind {
            // Implicit Attrsets can only be one of these two.
            NameKind::PlainAttrset => Expr::Attrset,
            NameKind::RecAttrset => Expr::RecAttrset,
            _ => unreachable!(),
        };
        let ptr = self.ptr.take();
        let e = ctor(self.finish(ctx));
        match ptr {
            Some(ptr) => ctx.alloc_expr(e, ptr),
            // For implicit Attrset produced by merging, there's no "source" for it.
            None => ctx.module.exprs.alloc(e),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::lower;
    use crate::def::{Expr, Literal};
    use crate::tests::TestDB;
    use crate::{DefDatabase, FileId};
    use expect_test::{expect, Expect};
    use std::fmt::Write;
    use syntax::parse_file;

    #[track_caller]
    fn check_lower(src: &str, expect: Expect) {
        let (db, file_id) = TestDB::single_file(src).unwrap();
        let module = db.module(file_id);
        let mut got = String::new();
        for diag in module.diagnostics() {
            writeln!(got, "{}", diag.debug_display()).unwrap();
        }
        if !module.diagnostics.is_empty() {
            writeln!(got).unwrap();
        }
        for (i, e) in module.exprs.iter() {
            writeln!(got, "{}: {:?}", i.into_raw(), e).unwrap();
        }
        if !module.names.is_empty() {
            writeln!(got).unwrap();
        }
        for (i, name) in module.names.iter() {
            writeln!(got, "{}: {:?}", i.into_raw(), name).unwrap();
        }
        expect.assert_eq(&got);
    }

    #[track_caller]
    fn check_path(src: &str, expect: Expect) {
        let (db, file_id) = TestDB::single_file(src).unwrap();
        let module = db.module(file_id);
        let got = module
            .exprs()
            .filter_map(|(_, kind)| match kind {
                Expr::Literal(Literal::Path(path)) => Some(path),
                _ => None,
            })
            .map(|path| format!("{:?}\n", path.data(&db)))
            .collect::<String>();
        expect.assert_eq(&got);
    }

    #[track_caller]
    fn check_error(src: &str, expect: Expect) {
        let (db, file_id) = TestDB::single_file(src).unwrap();
        let module = db.module(file_id);
        let mut got = String::new();
        for diag in module.diagnostics() {
            writeln!(got, "{}", diag.debug_display()).unwrap();
        }
        expect.assert_eq(&got);
    }

    #[test]
    fn literal() {
        check_lower(
            "42",
            expect![[r#"
                0: Literal(Int(42))
            "#]],
        );
        check_lower(
            "1.2e3",
            expect![[r#"
                0: Literal(Float(OrderedFloat(1200.0)))
            "#]],
        );
        check_lower(
            "a:b",
            expect![[r#"
                0..3: UriLiteral

                0: Literal(String("a:b"))
            "#]],
        );
    }

    #[test]
    fn path() {
        check_path(
            "./.",
            expect![[r#"
                PathData { anchor: Relative(FileId(0)), supers: 0, relative: VfsPath("") }
            "#]],
        );
        check_path(
            "../.",
            expect![[r#"
                PathData { anchor: Relative(FileId(0)), supers: 1, relative: VfsPath("") }
            "#]],
        );
        check_path(
            "../a/../../.b/./c",
            expect![[r#"
                PathData { anchor: Relative(FileId(0)), supers: 2, relative: VfsPath("/.b/c") }
            "#]],
        );
        check_path(
            "/../a/../../.b/./c",
            expect![[r#"
                PathData { anchor: Absolute, supers: 0, relative: VfsPath("/.b/c") }
            "#]],
        );
        check_path(
            "~/../a/../../.b/./c",
            expect![[r#"
                PathData { anchor: Home, supers: 2, relative: VfsPath("/.b/c") }
            "#]],
        );
        check_path(
            "<p/../a/../../.b/./c>",
            expect![[r#"
                PathData { anchor: Search("p"), supers: 2, relative: VfsPath("/.b/c") }
            "#]],
        );
    }

    #[test]
    fn lambda() {
        check_lower(
            "a: 0",
            expect![[r#"
                0: Literal(Int(0))
                1: Lambda(Some(Idx::<Name>(0)), None, Idx::<Expr>(0))

                0: Name { text: "a", kind: Param }
            "#]],
        );
        check_lower(
            "{ }: 0",
            expect![[r#"
                0: Literal(Int(0))
                1: Lambda(None, Some(Pat { fields: [], ellipsis: false }), Idx::<Expr>(0))
            "#]],
        );
        check_lower(
            "a@{ ... }: 0",
            expect![[r#"
                0: Literal(Int(0))
                1: Lambda(Some(Idx::<Name>(0)), Some(Pat { fields: [], ellipsis: true }), Idx::<Expr>(0))

                0: Name { text: "a", kind: Param }
            "#]],
        );
        check_lower(
            "{ } @ a: 0",
            expect![[r#"
                0: Literal(Int(0))
                1: Lambda(Some(Idx::<Name>(0)), Some(Pat { fields: [], ellipsis: false }), Idx::<Expr>(0))

                0: Name { text: "a", kind: Param }
            "#]],
        );
        check_lower(
            "{ a, b ? 0, ... }: 0",
            expect![[r#"
                0: Literal(Int(0))
                1: Literal(Int(0))
                2: Lambda(None, Some(Pat { fields: [(Some(Idx::<Name>(0)), None), (Some(Idx::<Name>(1)), Some(Idx::<Expr>(0)))], ellipsis: true }), Idx::<Expr>(1))

                0: Name { text: "a", kind: PatField }
                1: Name { text: "b", kind: PatField }
            "#]],
        );
        check_lower(
            "{ a ? 0, b }@c: 0",
            expect![[r#"
                0: Literal(Int(0))
                1: Literal(Int(0))
                2: Lambda(Some(Idx::<Name>(0)), Some(Pat { fields: [(Some(Idx::<Name>(1)), Some(Idx::<Expr>(0))), (Some(Idx::<Name>(2)), None)], ellipsis: false }), Idx::<Expr>(1))

                0: Name { text: "c", kind: Param }
                1: Name { text: "a", kind: PatField }
                2: Name { text: "b", kind: PatField }
            "#]],
        );
    }

    #[test]
    fn string() {
        check_lower(
            r#"" fo\no ""#,
            expect![[r#"
                0: Literal(String(" fo\no "))
            "#]],
        );
        check_lower(
            r#"'' fo'''o ''"#,
            expect![[r#"
                0: StringInterpolation([])
            "#]],
        );

        check_lower(
            r#"" fo${1}o\n$${${42}\💗""#,
            expect![[r#"
                0: Literal(Int(1))
                1: Literal(Int(42))
                2: StringInterpolation([Idx::<Expr>(0), Idx::<Expr>(1)])
            "#]],
        );
        check_lower(
            r#"'' ''$${1} $${}${42} ''"#,
            expect![[r#"
                0: Literal(Int(1))
                1: Literal(Int(42))
                2: StringInterpolation([Idx::<Expr>(0), Idx::<Expr>(1)])
            "#]],
        );
    }

    #[test]
    fn trivial_expr() {
        check_lower(
            "(1 + 2) 3 * -4",
            expect![[r#"
                0: Literal(Int(1))
                1: Literal(Int(2))
                2: Binary(Some(Add), Idx::<Expr>(0), Idx::<Expr>(1))
                3: Literal(Int(3))
                4: Apply(Idx::<Expr>(2), Idx::<Expr>(3))
                5: Literal(Int(4))
                6: Unary(Some(Negate), Idx::<Expr>(5))
                7: Binary(Some(Mul), Idx::<Expr>(4), Idx::<Expr>(6))
            "#]],
        );
        check_lower(
            "assert 1; 2",
            expect![[r#"
                0: Literal(Int(1))
                1: Literal(Int(2))
                2: Assert(Idx::<Expr>(0), Idx::<Expr>(1))
            "#]],
        );
        check_lower(
            "if 1 then 2 else 3",
            expect![[r#"
                0: Literal(Int(1))
                1: Literal(Int(2))
                2: Literal(Int(3))
                3: IfThenElse(Idx::<Expr>(0), Idx::<Expr>(1), Idx::<Expr>(2))
            "#]],
        );
        check_lower(
            "with 1; 2",
            expect![[r#"
                0: Literal(Int(1))
                1: Literal(Int(2))
                2: With(Idx::<Expr>(0), Idx::<Expr>(1))
            "#]],
        );
        check_lower(
            "[ 1 2 ]",
            expect![[r#"
                0: Literal(Int(1))
                1: Literal(Int(2))
                2: List([Idx::<Expr>(0), Idx::<Expr>(1)])
            "#]],
        );
    }

    #[test]
    fn uri() {
        check_lower(
            "foo:bar",
            expect![[r#"
                0..7: UriLiteral

                0: Literal(String("foo:bar"))
            "#]],
        );
    }

    #[test]
    fn attrpath() {
        check_lower(
            r#"a.b."c".${d} or e"#,
            expect![[r#"
                0: Reference("a")
                1: Literal(String("b"))
                2: Literal(String("c"))
                3: Reference("d")
                4: Reference("e")
                5: Select(Idx::<Expr>(0), [Idx::<Expr>(1), Idx::<Expr>(2), Idx::<Expr>(3)], Some(Idx::<Expr>(4)))
            "#]],
        );
        check_lower(
            r#"a?b."c".${d}"#,
            expect![[r#"
                0: Reference("a")
                1: Literal(String("b"))
                2: Literal(String("c"))
                3: Reference("d")
                4: HasAttr(Idx::<Expr>(0), [Idx::<Expr>(1), Idx::<Expr>(2), Idx::<Expr>(3)])
            "#]],
        );
    }

    #[test]
    fn attrset_key_kind() {
        check_lower(
            r#"{ a = 1; ${b} = 2; "c" = 3; ${(("\n"))} = 4; }"#,
            expect![[r#"
                0: Literal(Int(1))
                1: Reference("b")
                2: Literal(Int(2))
                3: Literal(Int(3))
                4: Literal(Int(4))
                5: Attrset(Bindings { statics: [(Idx::<Name>(0), Expr(Idx::<Expr>(0))), (Idx::<Name>(1), Expr(Idx::<Expr>(3))), (Idx::<Name>(2), Expr(Idx::<Expr>(4)))], inherit_froms: [], dynamics: [(Idx::<Expr>(1), Idx::<Expr>(2))] })

                0: Name { text: "a", kind: PlainAttrset }
                1: Name { text: "c", kind: PlainAttrset }
                2: Name { text: "\n", kind: PlainAttrset }
            "#]],
        );
    }

    #[test]
    fn attrset_inherit() {
        check_lower(
            r#"{ inherit; inherit a "b" ${("c")}; inherit (d); inherit (e) f g; }"#,
            expect![[r#"
                2..10: EmptyInherit
                35..47: EmptyInherit

                0: Reference("a")
                1: Reference("b")
                2: Reference("c")
                3: Reference("d")
                4: Reference("e")
                5: Attrset(Bindings { statics: [(Idx::<Name>(0), Inherit(Idx::<Expr>(0))), (Idx::<Name>(1), Inherit(Idx::<Expr>(1))), (Idx::<Name>(2), Inherit(Idx::<Expr>(2))), (Idx::<Name>(3), InheritFrom(Idx::<Expr>(4))), (Idx::<Name>(4), InheritFrom(Idx::<Expr>(4)))], inherit_froms: [Idx::<Expr>(3), Idx::<Expr>(4)], dynamics: [] })

                0: Name { text: "a", kind: PlainAttrset }
                1: Name { text: "b", kind: PlainAttrset }
                2: Name { text: "c", kind: PlainAttrset }
                3: Name { text: "f", kind: PlainAttrset }
                4: Name { text: "g", kind: PlainAttrset }
            "#]],
        );
    }

    #[test]
    fn attrset_merge_non_rec() {
        // Path and path.
        check_lower(
            "{ a.b = 1; a.c = 2; }",
            expect![[r#"
                0: Literal(Int(1))
                1: Literal(Int(2))
                2: Attrset(Bindings { statics: [(Idx::<Name>(1), Expr(Idx::<Expr>(0))), (Idx::<Name>(2), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Idx::<Name>(0), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })

                0: Name { text: "a", kind: PlainAttrset }
                1: Name { text: "b", kind: PlainAttrset }
                2: Name { text: "c", kind: PlainAttrset }
            "#]],
        );
        // Path and attrset.
        check_lower(
            "{ a.b = 1; a = { c = 2; }; }",
            expect![[r#"
                0: Literal(Int(1))
                1: Literal(Int(2))
                2: Attrset(Bindings { statics: [(Idx::<Name>(1), Expr(Idx::<Expr>(0))), (Idx::<Name>(2), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Idx::<Name>(0), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })

                0: Name { text: "a", kind: PlainAttrset }
                1: Name { text: "b", kind: PlainAttrset }
                2: Name { text: "c", kind: PlainAttrset }
            "#]],
        );
        // Attrset and path.
        check_lower(
            "{ a = { b = 1; }; a.c = 2; }",
            expect![[r#"
                0: Literal(Int(1))
                1: Literal(Int(2))
                2: Attrset(Bindings { statics: [(Idx::<Name>(1), Expr(Idx::<Expr>(0))), (Idx::<Name>(2), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Idx::<Name>(0), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })

                0: Name { text: "a", kind: PlainAttrset }
                1: Name { text: "b", kind: PlainAttrset }
                2: Name { text: "c", kind: PlainAttrset }
            "#]],
        );
        // Attrset and attrset.
        check_lower(
            "{ a = { b = 1; }; a = { c = 2; }; }",
            expect![[r#"
                0: Literal(Int(1))
                1: Literal(Int(2))
                2: Attrset(Bindings { statics: [(Idx::<Name>(1), Expr(Idx::<Expr>(0))), (Idx::<Name>(2), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Idx::<Name>(0), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })

                0: Name { text: "a", kind: PlainAttrset }
                1: Name { text: "b", kind: PlainAttrset }
                2: Name { text: "c", kind: PlainAttrset }
            "#]],
        );
    }

    #[test]
    fn attrset_merge_rec() {
        // Rec and non-rec.
        check_lower(
            "{ a = rec { b = 1; }; a = { c = 2; }; }",
            expect![[r#"
                22..23: MergeRecAttrset

                0: Literal(Int(1))
                1: Literal(Int(2))
                2: RecAttrset(Bindings { statics: [(Idx::<Name>(1), Expr(Idx::<Expr>(0))), (Idx::<Name>(2), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Idx::<Name>(0), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })

                0: Name { text: "a", kind: PlainAttrset }
                1: Name { text: "b", kind: RecAttrset }
                2: Name { text: "c", kind: RecAttrset }
            "#]],
        );

        // Rec and path.
        check_lower(
            "{ a = rec { b = 1; }; a.c = 2; }",
            expect![[r#"
                22..23: MergeRecAttrset

                0: Literal(Int(1))
                1: Literal(Int(2))
                2: RecAttrset(Bindings { statics: [(Idx::<Name>(1), Expr(Idx::<Expr>(0))), (Idx::<Name>(2), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Idx::<Name>(0), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })

                0: Name { text: "a", kind: PlainAttrset }
                1: Name { text: "b", kind: RecAttrset }
                2: Name { text: "c", kind: RecAttrset }
            "#]],
        );

        // Non-rec and rec.
        check_lower(
            "{ a = { b = 1; }; a = rec { c = 2; }; }",
            expect![[r#"
                18..19: MergePlainRecAttrset

                0: Literal(Int(1))
                1: Literal(Int(2))
                2: Attrset(Bindings { statics: [(Idx::<Name>(1), Expr(Idx::<Expr>(0))), (Idx::<Name>(2), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Idx::<Name>(0), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })

                0: Name { text: "a", kind: PlainAttrset }
                1: Name { text: "b", kind: PlainAttrset }
                2: Name { text: "c", kind: PlainAttrset }
            "#]],
        );

        // Path and rec.
        check_lower(
            "{ a.b = 1; a = rec { c = 2; }; }",
            expect![[r#"
                11..12: MergePlainRecAttrset

                0: Literal(Int(1))
                1: Literal(Int(2))
                2: Attrset(Bindings { statics: [(Idx::<Name>(1), Expr(Idx::<Expr>(0))), (Idx::<Name>(2), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Idx::<Name>(0), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })

                0: Name { text: "a", kind: PlainAttrset }
                1: Name { text: "b", kind: PlainAttrset }
                2: Name { text: "c", kind: PlainAttrset }
            "#]],
        );

        // Rec and rec.
        check_lower(
            "{ a = rec { b = 1; }; a = rec { c = 2; }; }",
            expect![[r#"
                22..23: MergeRecAttrset

                0: Literal(Int(1))
                1: Literal(Int(2))
                2: RecAttrset(Bindings { statics: [(Idx::<Name>(1), Expr(Idx::<Expr>(0))), (Idx::<Name>(2), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Idx::<Name>(0), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })

                0: Name { text: "a", kind: PlainAttrset }
                1: Name { text: "b", kind: RecAttrset }
                2: Name { text: "c", kind: RecAttrset }
            "#]],
        );
    }

    #[test]
    fn attrset_rec_dup() {
        check_lower(
            "rec { a = 1; a = 2; }",
            expect![[r#"
                13..14: DuplicatedKey
                    6..7: Previously defined here

                0: Literal(Int(1))
                1: Literal(Int(2))
                2: RecAttrset(Bindings { statics: [(Idx::<Name>(0), Expr(Idx::<Expr>(0)))], inherit_froms: [], dynamics: [] })

                0: Name { text: "a", kind: RecAttrset }
            "#]],
        );
    }

    #[test]
    fn attrset_rec_deep() {
        check_lower(
            "rec { a.b = 1; }",
            expect![[r#"
                0: Literal(Int(1))
                1: Attrset(Bindings { statics: [(Idx::<Name>(1), Expr(Idx::<Expr>(0)))], inherit_froms: [], dynamics: [] })
                2: RecAttrset(Bindings { statics: [(Idx::<Name>(0), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })

                0: Name { text: "a", kind: RecAttrset }
                1: Name { text: "b", kind: PlainAttrset }
            "#]],
        );
    }

    #[test]
    fn attrset_rec_single() {
        // This should not warn.
        check_lower(
            "{ a.b = rec { c = 1; }; }",
            expect![[r#"
                0: Literal(Int(1))
                1: RecAttrset(Bindings { statics: [(Idx::<Name>(2), Expr(Idx::<Expr>(0)))], inherit_froms: [], dynamics: [] })
                2: Attrset(Bindings { statics: [(Idx::<Name>(1), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Idx::<Name>(0), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })

                0: Name { text: "a", kind: PlainAttrset }
                1: Name { text: "b", kind: PlainAttrset }
                2: Name { text: "c", kind: RecAttrset }
            "#]],
        );
    }

    #[test]
    fn attrset_let() {
        check_lower(
            "{ a.b = let { c.d = 1; }; }",
            expect![[r#"
                8..24: LetAttrset

                0: Literal(Int(1))
                1: Attrset(Bindings { statics: [(Idx::<Name>(2), Expr(Idx::<Expr>(0)))], inherit_froms: [], dynamics: [] })
                2: LetAttrset(Bindings { statics: [(Idx::<Name>(1), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Idx::<Name>(3), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })
                4: Attrset(Bindings { statics: [(Idx::<Name>(0), Expr(Idx::<Expr>(3)))], inherit_froms: [], dynamics: [] })

                0: Name { text: "a", kind: PlainAttrset }
                1: Name { text: "c", kind: RecAttrset }
                2: Name { text: "d", kind: PlainAttrset }
                3: Name { text: "b", kind: PlainAttrset }
            "#]],
        );
    }

    #[test]
    fn attrset_dynamic_no_merge() {
        check_lower(
            "{ ${a}.b = 1; ${a}.b = 2; }",
            expect![[r#"
                0: Reference("a")
                1: Literal(Int(1))
                2: Attrset(Bindings { statics: [(Idx::<Name>(0), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Reference("a")
                4: Literal(Int(2))
                5: Attrset(Bindings { statics: [(Idx::<Name>(1), Expr(Idx::<Expr>(4)))], inherit_froms: [], dynamics: [] })
                6: Attrset(Bindings { statics: [], inherit_froms: [], dynamics: [(Idx::<Expr>(0), Idx::<Expr>(2)), (Idx::<Expr>(3), Idx::<Expr>(5))] })

                0: Name { text: "b", kind: PlainAttrset }
                1: Name { text: "b", kind: PlainAttrset }
            "#]],
        );
    }

    #[test]
    fn let_empty() {
        check_lower(
            "let in 1",
            expect![[r#"
                0..6: EmptyLetIn

                0: Literal(Int(1))
                1: LetIn(Bindings { statics: [], inherit_froms: [], dynamics: [] }, Idx::<Expr>(0))
            "#]],
        );
    }

    #[test]
    fn let_dynamic_deep() {
        check_lower(
            "let a.${a} = 1; in 1",
            expect![[r#"
                0: Reference("a")
                1: Literal(Int(1))
                2: Attrset(Bindings { statics: [], inherit_froms: [], dynamics: [(Idx::<Expr>(0), Idx::<Expr>(1))] })
                3: Literal(Int(1))
                4: LetIn(Bindings { statics: [(Idx::<Name>(0), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] }, Idx::<Expr>(3))

                0: Name { text: "a", kind: LetIn }
            "#]],
        );
    }

    #[test]
    fn invalid_dynamic_error() {
        check_error(
            "let ${a} = 1; in 1",
            expect![[r#"
                4..8: InvalidDynamic
                0..16: EmptyLetIn
            "#]],
        );
        check_error(
            "{ inherit ${a}; }",
            expect![[r#"
                10..14: InvalidDynamic
            "#]],
        );
        check_error(
            "{ inherit (a) ${a}; }",
            expect![[r#"
                14..18: InvalidDynamic
            "#]],
        );
    }

    #[test]
    fn attrset_merge_error() {
        // Value and value.
        check_error(
            "{ a = 1; a = 2; }",
            expect![[r#"
                9..10: DuplicatedKey
                    2..3: Previously defined here
            "#]],
        );
        // Set and value.
        check_error(
            "{ a.b = 1; a = 2; }",
            expect![[r#"
                11..12: DuplicatedKey
                    2..3: Previously defined here
            "#]],
        );
        // Value and set.
        check_error(
            "{ a = 1; a.b = 2; }",
            expect![[r#"
                9..10: DuplicatedKey
                    2..3: Previously defined here
            "#]],
        );
        // Inherit and value.
        check_error(
            "{ inherit a; a = 1; }",
            expect![[r#"
                13..14: DuplicatedKey
                    10..11: Previously defined here
            "#]],
        );
        // Inherit-from and value.
        check_error(
            "{ inherit (1) a; a = 1; }",
            expect![[r#"
                17..18: DuplicatedKey
                    14..15: Previously defined here
            "#]],
        );
    }

    #[test]
    fn attrset_no_duplicated_duplicated_error() {
        check_error(
            "{ a = 1; a = 2; a = 3; }",
            expect![[r#"
                9..10: DuplicatedKey
                    2..3: Previously defined here
                16..17: DuplicatedKey
                    2..3: Previously defined here
            "#]],
        );
    }

    #[test]
    fn lambda_duplicated_param() {
        check_error(
            "{ a, a }: 1",
            expect![[r#"
                5..6: DuplicatedParam
                    2..3: Previously defined here
            "#]],
        );
        check_error(
            "{ a }@a: 1",
            expect![[r#"
                2..3: DuplicatedParam
                    6..7: Previously defined here
            "#]],
        );
        check_error(
            "a@{ a }: 1",
            expect![[r#"
                4..5: DuplicatedParam
                    0..1: Previously defined here
            "#]],
        );
    }

    #[test]
    fn attrset_malformed_no_panic() {
        let src = "{ } @ y: y { cc, extraPackages ? optional (cc.isGNU) }: 1";
        let parse = parse_file(src);
        let db = TestDB::default();
        let _ = lower(&db, FileId(0), parse);
    }
}
