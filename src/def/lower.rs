use super::{
    AstPtr, Attrpath, BindingKey, BindingValue, Bindings, DefDatabase, Expr, ExprId, Literal,
    Module, ModuleSourceMap, NameDef, NameDefId, NameDefKind, Pat, PathAnchor, PathData,
};
use crate::{Diagnostic, DiagnosticKind, FileId, FileRange};
use indexmap::IndexMap;
use la_arena::Arena;
use rowan::ast::AstNode;
use smol_str::SmolStr;
use std::str;
use syntax::ast::{self, HasBindings, HasStringParts, LiteralKind};
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
            name_defs: Arena::new(),
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

    fn alloc_name_def(&mut self, name: SmolStr, kind: NameDefKind, ptr: AstPtr) -> NameDefId {
        let id = self.module.name_defs.alloc(NameDef { name, kind });
        self.source_map.name_def_map.insert(ptr.clone(), id);
        self.source_map.name_def_map_rev.insert(id, vec![ptr]);
        id
    }

    fn diagnostic(&mut self, diag: Diagnostic) {
        self.module.diagnostics.push(diag);
    }

    fn lower_name(&mut self, node: ast::Name, kind: NameDefKind) -> NameDefId {
        let name = node
            .token()
            .map_or_else(Default::default, |tok| tok.text().into());
        let ptr = AstPtr::new(node.syntax());
        self.alloc_name_def(name, kind, ptr)
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
            ast::Expr::Lambda(e) => {
                let (param, pat) = e.param().map_or((None, None), |param| {
                    let name = param.name().map(|n| self.lower_name(n, NameDefKind::Param));
                    let pat = param.pat().map(|pat| {
                        let fields = pat
                            .fields()
                            .map(|field| {
                                let field_name = field
                                    .name()
                                    .map(|n| self.lower_name(n, NameDefKind::PatField));
                                let default_expr = field.default_expr().map(|e| self.lower_expr(e));
                                (field_name, default_expr)
                            })
                            .collect();
                        let ellipsis = pat.ellipsis_token().is_some();
                        Pat { fields, ellipsis }
                    });
                    (name, pat)
                });
                let body = self.lower_expr_opt(e.body());
                self.alloc_expr(Expr::Lambda(param, pat, body), ptr)
            }
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
            ast::Expr::IndentString(s) => self.lower_string(&s),
            ast::Expr::List(e) => {
                let elements = e.elements().map(|e| self.lower_expr(e)).collect();
                self.alloc_expr(Expr::List(elements), ptr)
            }
            ast::Expr::LetIn(e) => {
                let mut set = MergingSet::new(Some(NameDefKind::LetIn));
                set.merge_bindings(self, &e);
                let bindings = set.finish(self);
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
                // RecAttrset is popular than LetAttrset, and is preferred.
                let (def_kind, ctor): (_, fn(_) -> _) = if e.rec_token().is_some() {
                    (Some(NameDefKind::RecAttrset), Expr::RecAttrset)
                } else if e.let_token().is_some() {
                    self.diagnostic(Diagnostic::new(
                        e.syntax().text_range(),
                        DiagnosticKind::LetAttrset,
                    ));
                    (Some(NameDefKind::RecAttrset), Expr::LetAttrset)
                } else {
                    (None, Expr::Attrset)
                };
                let mut set = MergingSet::new(def_kind);
                set.merge_bindings(self, &e);
                let bindings = set.finish(self);
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

    fn lower_string(&mut self, n: &impl HasStringParts) -> ExprId {
        let ptr = AstPtr::new(n.syntax());
        // Here we don't need to special case literal strings.
        // They would simply become `Expr::StringInterpolation([])`.
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

enum AttrKind {
    Static(SmolStr, AstPtr),
    Dynamic(Option<ast::Expr>),
}

impl AttrKind {
    fn classify(attr: ast::Attr) -> Self {
        let string_node = match attr {
            ast::Attr::String(s) => s,
            ast::Attr::Name(n) => {
                let name = n
                    .token()
                    .map_or_else(Default::default, |tok| tok.text().into());
                return Self::Static(name, AstPtr::new(n.syntax()));
            }
            // Unwrap parenthesis around string literals.
            // `{ ${(("foo"))} = 1; }` => `{ "foo" = 1; }`
            ast::Attr::Dynamic(d) => {
                let mut e = d.expr();
                loop {
                    match e {
                        Some(ast::Expr::String(s)) => break s,
                        Some(ast::Expr::Paren(p)) => e = p.expr(),
                        _ => return Self::Dynamic(e),
                    }
                }
            }
        };

        if string_node
            .string_parts()
            .any(|part| matches!(part, ast::StringPart::Dynamic(_)))
        {
            return Self::Dynamic(Some(ast::Expr::String(string_node)));
        }

        let content = string_node
            .string_parts()
            .fold(String::new(), |prev, part| match part {
                ast::StringPart::Dynamic(_) => unreachable!(),
                ast::StringPart::Fragment(tok) => prev + tok.text(),
                ast::StringPart::Escape(tok) => {
                    prev + match tok.text().as_bytes() {
                        b"\\n" => "\n",
                        b"\\r" => "\r",
                        b"\\t" => "\t",
                        [b'\\', bytes @ ..] => {
                            str::from_utf8(bytes).expect("Verified by the lexer")
                        }
                        _ => unreachable!("Verified by the lexer"),
                    }
                }
            });
        let ptr = AstPtr::new(string_node.syntax());
        Self::Static(content.into(), ptr)
    }
}

struct MergingSet {
    def_kind: Option<NameDefKind>,
    statics: IndexMap<BindingKey, MergingEntry>,
    inherit_froms: Vec<ExprId>,
    dynamics: Vec<(ExprId, MergingEntry)>,
}

struct MergingEntry {
    /// The LHS range of the definition's Attr node, for error reporting.
    def_ptr: AstPtr,
    value: MergingValue,
}

enum MergingValue {
    Placeholder,
    Attrset(Option<AstPtr>, MergingSet),
    Final(BindingValue),
}

impl From<BindingValue> for MergingValue {
    fn from(value: BindingValue) -> Self {
        Self::Final(value)
    }
}

impl MergingSet {
    fn new(def_kind: Option<NameDefKind>) -> Self {
        Self {
            def_kind,
            statics: IndexMap::default(),
            inherit_froms: Vec::new(),
            dynamics: Vec::new(),
        }
    }

    // Place an orphaned Expr as dynamic-attrs for error recovery.
    fn recover_error(&mut self, ctx: &mut LowerCtx, expr: ExprId, expr_ptr: AstPtr) {
        let key = ctx.alloc_expr(Expr::Missing, expr_ptr.clone());
        let entry = MergingEntry {
            def_ptr: expr_ptr,
            value: BindingValue::Expr(expr).into(),
        };
        self.dynamics.push((key, entry));
    }

    fn merge_bindings(&mut self, ctx: &mut LowerCtx, n: &impl HasBindings) {
        for b in n.bindings() {
            match b {
                ast::Binding::Inherit(i) => self.merge_inherit(ctx, i),
                ast::Binding::AttrpathValue(entry) => self.merge_path_value(ctx, entry),
            }
        }
    }

    fn merge_inherit(&mut self, ctx: &mut LowerCtx, i: ast::Inherit) {
        let from_expr = i.from_expr().map(|e| {
            let expr = ctx.lower_expr_opt(e.expr());
            self.inherit_froms.push(expr);
            expr
        });

        let mut no_attrs = true;
        for attr in i.attrs() {
            no_attrs = false;

            let ptr = AstPtr::new(attr.syntax());
            let (name, ptr) = match AttrKind::classify(attr) {
                AttrKind::Static(name, ptr) => (name, ptr),
                // `inherit ${expr}` or `inherit (expr) ${expr}` is invalid.
                AttrKind::Dynamic(expr) => {
                    ctx.diagnostic(Diagnostic::new(
                        ptr.text_range(),
                        DiagnosticKind::InvalidDynamic,
                    ));
                    let expr = ctx.lower_expr_opt(expr);
                    self.recover_error(ctx, expr, ptr.clone());
                    continue;
                }
            };
            let key = match self.def_kind {
                Some(kind) => {
                    BindingKey::NameDef(ctx.alloc_name_def(name.clone(), kind, ptr.clone()))
                }
                None => BindingKey::Name(name.clone()),
            };

            // Inherited names never merge other values. It must be an error.
            if let Some(v) = self.statics.get_mut(&key) {
                v.emit_duplicated_key(ctx, ptr);
                continue;
            }

            let value = match from_expr {
                Some(e) => BindingValue::InheritFrom(e),
                None => {
                    let ref_expr = ctx.alloc_expr(Expr::Reference(name), ptr.clone());
                    BindingValue::Inherit(ref_expr)
                }
            };
            let entry = MergingEntry {
                def_ptr: ptr,
                value: value.into(),
            };
            self.statics.insert(key, entry);
        }

        if no_attrs {
            ctx.diagnostic(Diagnostic::new(
                i.syntax().text_range(),
                DiagnosticKind::EmptyInherit,
            ));
        }
    }

    fn merge_path_value(mut self: &mut Self, ctx: &mut LowerCtx, path_value: ast::AttrpathValue) {
        let mut attrs = path_value
            .attrpath()
            .into_iter()
            .flat_map(|path| path.attrs());
        let mut next_attr = match attrs.next() {
            Some(first_attr) => first_attr,
            // Recover from missing Attrpath. This is already a syntax error,
            // don't report it again.
            None => {
                let value_expr = ctx.lower_expr_opt(path_value.value());
                self.recover_error(ctx, value_expr, AstPtr::new(path_value.syntax()));
                return;
            }
        };

        loop {
            let mut attr_ptr = AstPtr::new(next_attr.syntax());
            let entry = match AttrKind::classify(next_attr) {
                AttrKind::Static(name, ptr) => {
                    attr_ptr = ptr;
                    let key = match self.def_kind {
                        Some(kind) => {
                            BindingKey::NameDef(ctx.alloc_name_def(name, kind, attr_ptr.clone()))
                        }
                        None => BindingKey::Name(name),
                    };
                    self.statics
                        .entry(key)
                        .or_insert_with(|| MergingEntry::new(attr_ptr.clone()))
                }
                AttrKind::Dynamic(expr) => {
                    // LetIn doesn't allow dynamic attrs.
                    if self.def_kind == Some(NameDefKind::LetIn) {
                        ctx.diagnostic(Diagnostic::new(
                            attr_ptr.text_range(),
                            DiagnosticKind::InvalidDynamic,
                        ));
                        // We don't skip the RHS but still process it as a recovery.
                    }
                    let expr = ctx.lower_expr_opt(expr);
                    self.dynamics
                        .push((expr, MergingEntry::new(attr_ptr.clone())));
                    &mut self.dynamics.last_mut().unwrap().1
                }
            };
            match attrs.next() {
                Some(attr) => {
                    // Deeper attrsets created via attrpath is not `rec`.
                    self = entry.make_attrset(ctx, None, attr_ptr, None);
                    next_attr = attr;
                }
                None => {
                    entry.merge_ast(ctx, attr_ptr, path_value.value());
                    return;
                }
            }
        }
    }

    fn finish(self, ctx: &mut LowerCtx) -> Bindings {
        Bindings {
            statics: self
                .statics
                .into_iter()
                .map(|(key, entry)| (key, entry.finish(ctx)))
                .collect(),
            inherit_froms: self.inherit_froms.into(),
            dynamics: self
                .dynamics
                .into_iter()
                .map(|(key_expr, entry)| {
                    let expr = match entry.finish(ctx) {
                        BindingValue::Inherit(_) | BindingValue::InheritFrom(_) => unreachable!(),
                        BindingValue::Expr(expr) => expr,
                    };
                    (key_expr, expr)
                })
                .collect(),
        }
    }
}

impl MergingEntry {
    fn new(def_ptr: AstPtr) -> Self {
        Self {
            def_ptr,
            value: MergingValue::Placeholder,
        }
    }

    fn make_attrset(
        &mut self,
        ctx: &mut LowerCtx,
        def_kind: Option<NameDefKind>,
        def_ptr: AstPtr,
        value_ptr: Option<AstPtr>,
    ) -> &mut MergingSet {
        match &mut self.value {
            MergingValue::Placeholder => {
                self.value = MergingValue::Attrset(value_ptr, MergingSet::new(def_kind));
            }
            MergingValue::Attrset(_, prev_set) => {
                let prev_is_rec = prev_set.def_kind.is_some();
                let this_is_rec = def_kind.is_some();
                if prev_is_rec {
                    ctx.diagnostic(Diagnostic::new(
                        def_ptr.text_range(),
                        DiagnosticKind::MergeRecAttrset,
                    ));
                } else if this_is_rec {
                    ctx.diagnostic(Diagnostic::new(
                        def_ptr.text_range(),
                        DiagnosticKind::MergePlainRecAttrset,
                    ));
                }
            }
            // We prefer to become a Attrset as a guess, which allows further merging.
            MergingValue::Final(value) => {
                let mut set = MergingSet::new(def_kind);
                if let BindingValue::Expr(expr) = value {
                    set.recover_error(ctx, *expr, self.def_ptr.clone());
                }
                self.emit_duplicated_key(ctx, def_ptr.clone());
                self.def_ptr = def_ptr;
                self.value = MergingValue::Attrset(value_ptr, MergingSet::new(def_kind));
            }
        }
        match &mut self.value {
            MergingValue::Attrset(_, set) => set,
            _ => unreachable!(),
        }
    }

    fn merge_ast(&mut self, ctx: &mut LowerCtx, def_ptr: AstPtr, mut e: Option<ast::Expr>) {
        loop {
            match &e {
                Some(ast::Expr::Paren(p)) => e = p.expr(),
                Some(ast::Expr::AttrSet(e)) => {
                    // RecAttrset is popular than LetAttrset, and is preferred.
                    let def_kind = if e.rec_token().is_some() {
                        Some(NameDefKind::RecAttrset)
                    } else if e.let_token().is_some() {
                        break;
                    } else {
                        None
                    };
                    let value_ptr = AstPtr::new(e.syntax());
                    return self
                        .make_attrset(ctx, def_kind, def_ptr, Some(value_ptr))
                        .merge_bindings(ctx, e);
                }
                _ => break,
            }
        }
        // Here we got an unmergable Expr.
        match &mut self.value {
            MergingValue::Placeholder => {
                self.value = BindingValue::Expr(ctx.lower_expr_opt(e)).into();
            }
            MergingValue::Attrset(..) | MergingValue::Final { .. } => {
                // Suppress errors when there is no RHS, which happens during typing.
                if e.is_some() {
                    self.emit_duplicated_key(ctx, def_ptr);
                }
            }
        }
    }

    fn emit_duplicated_key(&mut self, ctx: &mut LowerCtx, ptr: AstPtr) {
        ctx.diagnostic(
            Diagnostic::new(ptr.text_range(), DiagnosticKind::DuplicatedKey).with_note(
                FileRange::new(ctx.file_id, self.def_ptr.text_range()),
                "Previously defined here",
            ),
        );
    }

    fn finish(self, ctx: &mut LowerCtx) -> BindingValue {
        match self.value {
            MergingValue::Placeholder => unreachable!(),
            MergingValue::Final(value) => value,
            MergingValue::Attrset(value_ptr, set) => {
                let is_rec = set.def_kind.is_some();
                let bindings = set.finish(ctx);
                let expr = if is_rec {
                    Expr::RecAttrset(bindings)
                } else {
                    Expr::Attrset(bindings)
                };
                let expr = match value_ptr {
                    Some(ptr) => ctx.alloc_expr(expr, ptr),
                    // Implicit attrsets have no source!
                    None => ctx.module.exprs.alloc(expr),
                };
                BindingValue::Expr(expr)
            }
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
        if !module.name_defs.is_empty() {
            writeln!(got).unwrap();
        }
        for (i, def) in module.name_defs.iter() {
            writeln!(got, "{}: {:?}", i.into_raw(), def).unwrap();
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
                0..3: URL literal is confusing and deprecated. Use strings instead

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
                1: Lambda(Some(Idx::<NameDef>(0)), None, Idx::<Expr>(0))

                0: NameDef { name: "a", kind: Param }
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
                1: Lambda(Some(Idx::<NameDef>(0)), Some(Pat { fields: [], ellipsis: true }), Idx::<Expr>(0))

                0: NameDef { name: "a", kind: Param }
            "#]],
        );
        check_lower(
            "{ } @ a: 0",
            expect![[r#"
                0: Literal(Int(0))
                1: Lambda(Some(Idx::<NameDef>(0)), Some(Pat { fields: [], ellipsis: false }), Idx::<Expr>(0))

                0: NameDef { name: "a", kind: Param }
            "#]],
        );
        check_lower(
            "{ a, b ? 0, ... }: 0",
            expect![[r#"
                0: Literal(Int(0))
                1: Literal(Int(0))
                2: Lambda(None, Some(Pat { fields: [(Some(Idx::<NameDef>(0)), None), (Some(Idx::<NameDef>(1)), Some(Idx::<Expr>(0)))], ellipsis: true }), Idx::<Expr>(1))

                0: NameDef { name: "a", kind: PatField }
                1: NameDef { name: "b", kind: PatField }
            "#]],
        );
        check_lower(
            "{ a ? 0, b }@c: 0",
            expect![[r#"
                0: Literal(Int(0))
                1: Literal(Int(0))
                2: Lambda(Some(Idx::<NameDef>(0)), Some(Pat { fields: [(Some(Idx::<NameDef>(1)), Some(Idx::<Expr>(0))), (Some(Idx::<NameDef>(2)), None)], ellipsis: false }), Idx::<Expr>(1))

                0: NameDef { name: "c", kind: Param }
                1: NameDef { name: "a", kind: PatField }
                2: NameDef { name: "b", kind: PatField }
            "#]],
        );
    }

    #[test]
    fn string() {
        check_lower(
            r#"" fo\no ""#,
            expect![[r#"
                0: StringInterpolation([])
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
                0..7: URL literal is confusing and deprecated. Use strings instead

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
                2: StringInterpolation([])
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
                2: StringInterpolation([])
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
                5: Attrset(Bindings { statics: [(Name("a"), Expr(Idx::<Expr>(0))), (Name("c"), Expr(Idx::<Expr>(3))), (Name("\n"), Expr(Idx::<Expr>(4)))], inherit_froms: [], dynamics: [(Idx::<Expr>(1), Idx::<Expr>(2))] })
            "#]],
        );
    }

    #[test]
    fn attrset_inherit() {
        check_lower(
            r#"{ inherit; inherit a "b" ${("c")}; inherit (d); inherit (e) f g; }"#,
            expect![[r#"
                2..10: Nothing inherited
                35..47: Nothing inherited

                0: Reference("a")
                1: Reference("b")
                2: Reference("c")
                3: Reference("d")
                4: Reference("e")
                5: Attrset(Bindings { statics: [(Name("a"), Inherit(Idx::<Expr>(0))), (Name("b"), Inherit(Idx::<Expr>(1))), (Name("c"), Inherit(Idx::<Expr>(2))), (Name("f"), InheritFrom(Idx::<Expr>(4))), (Name("g"), InheritFrom(Idx::<Expr>(4)))], inherit_froms: [Idx::<Expr>(3), Idx::<Expr>(4)], dynamics: [] })
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
                2: Attrset(Bindings { statics: [(Name("b"), Expr(Idx::<Expr>(0))), (Name("c"), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Name("a"), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })
            "#]],
        );
        // Path and attrset.
        check_lower(
            "{ a.b = 1; a = { c = 2; }; }",
            expect![[r#"
                0: Literal(Int(1))
                1: Literal(Int(2))
                2: Attrset(Bindings { statics: [(Name("b"), Expr(Idx::<Expr>(0))), (Name("c"), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Name("a"), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })
            "#]],
        );
        // Attrset and path.
        check_lower(
            "{ a = { b = 1; }; a.c = 2; }",
            expect![[r#"
                0: Literal(Int(1))
                1: Literal(Int(2))
                2: Attrset(Bindings { statics: [(Name("b"), Expr(Idx::<Expr>(0))), (Name("c"), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Name("a"), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })
            "#]],
        );
        // Attrset and attrset.
        check_lower(
            "{ a = { b = 1; }; a = { c = 2; }; }",
            expect![[r#"
                0: Literal(Int(1))
                1: Literal(Int(2))
                2: Attrset(Bindings { statics: [(Name("b"), Expr(Idx::<Expr>(0))), (Name("c"), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Name("a"), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })
            "#]],
        );
    }

    #[test]
    fn attrset_merge_rec() {
        // Rec and non-rec.
        check_lower(
            "{ a = rec { b = 1; }; a = { c = 2; }; }",
            expect![[r#"
                22..23: Merging rec-attrset with other attrsets or attrpath. Merged values can unexpectedly reference each other remotely as in a single `rec { ... }`

                0: Literal(Int(1))
                1: Literal(Int(2))
                2: RecAttrset(Bindings { statics: [(NameDef(Idx::<NameDef>(0)), Expr(Idx::<Expr>(0))), (NameDef(Idx::<NameDef>(1)), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Name("a"), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })

                0: NameDef { name: "b", kind: RecAttrset }
                1: NameDef { name: "c", kind: RecAttrset }
            "#]],
        );

        // Rec and path.
        check_lower(
            "{ a = rec { b = 1; }; a.c = 2; }",
            expect![[r#"
                22..23: Merging rec-attrset with other attrsets or attrpath. Merged values can unexpectedly reference each other remotely as in a single `rec { ... }`

                0: Literal(Int(1))
                1: Literal(Int(2))
                2: RecAttrset(Bindings { statics: [(NameDef(Idx::<NameDef>(0)), Expr(Idx::<Expr>(0))), (NameDef(Idx::<NameDef>(1)), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Name("a"), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })

                0: NameDef { name: "b", kind: RecAttrset }
                1: NameDef { name: "c", kind: RecAttrset }
            "#]],
        );

        // Non-rec and rec.
        check_lower(
            "{ a = { b = 1; }; a = rec { c = 2; }; }",
            expect![[r#"
                18..19: Merging non-rec-attrset with rec-attrset, the latter `rec` is implicitly ignored

                0: Literal(Int(1))
                1: Literal(Int(2))
                2: Attrset(Bindings { statics: [(Name("b"), Expr(Idx::<Expr>(0))), (Name("c"), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Name("a"), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })
            "#]],
        );

        // Path and rec.
        check_lower(
            "{ a.b = 1; a = rec { c = 2; }; }",
            expect![[r#"
                11..12: Merging non-rec-attrset with rec-attrset, the latter `rec` is implicitly ignored

                0: Literal(Int(1))
                1: Literal(Int(2))
                2: Attrset(Bindings { statics: [(Name("b"), Expr(Idx::<Expr>(0))), (Name("c"), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Name("a"), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })
            "#]],
        );

        // Rec and rec.
        check_lower(
            "{ a = rec { b = 1; }; a = rec { c = 2; }; }",
            expect![[r#"
                22..23: Merging rec-attrset with other attrsets or attrpath. Merged values can unexpectedly reference each other remotely as in a single `rec { ... }`

                0: Literal(Int(1))
                1: Literal(Int(2))
                2: RecAttrset(Bindings { statics: [(NameDef(Idx::<NameDef>(0)), Expr(Idx::<Expr>(0))), (NameDef(Idx::<NameDef>(1)), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Name("a"), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })

                0: NameDef { name: "b", kind: RecAttrset }
                1: NameDef { name: "c", kind: RecAttrset }
            "#]],
        );
    }

    #[test]
    fn attrset_rec_deep() {
        check_lower(
            "rec { a.b = 1; }",
            expect![[r#"
                0: Literal(Int(1))
                1: Attrset(Bindings { statics: [(Name("b"), Expr(Idx::<Expr>(0)))], inherit_froms: [], dynamics: [] })
                2: RecAttrset(Bindings { statics: [(NameDef(Idx::<NameDef>(0)), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })

                0: NameDef { name: "a", kind: RecAttrset }
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
                1: RecAttrset(Bindings { statics: [(NameDef(Idx::<NameDef>(0)), Expr(Idx::<Expr>(0)))], inherit_froms: [], dynamics: [] })
                2: Attrset(Bindings { statics: [(Name("b"), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Name("a"), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })

                0: NameDef { name: "c", kind: RecAttrset }
            "#]],
        );
    }

    #[test]
    fn attrset_let() {
        check_lower(
            "{ a.b = let { c.d = 1; }; }",
            expect![[r#"
                8..24: `let { ... }` is deprecated. Use `let ... in ...` instead

                0: Literal(Int(1))
                1: Attrset(Bindings { statics: [(Name("d"), Expr(Idx::<Expr>(0)))], inherit_froms: [], dynamics: [] })
                2: LetAttrset(Bindings { statics: [(NameDef(Idx::<NameDef>(0)), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                3: Attrset(Bindings { statics: [(Name("b"), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] })
                4: Attrset(Bindings { statics: [(Name("a"), Expr(Idx::<Expr>(3)))], inherit_froms: [], dynamics: [] })

                0: NameDef { name: "c", kind: RecAttrset }
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
                2: Reference("a")
                3: Literal(Int(2))
                4: Attrset(Bindings { statics: [(Name("b"), Expr(Idx::<Expr>(1)))], inherit_froms: [], dynamics: [] })
                5: Attrset(Bindings { statics: [(Name("b"), Expr(Idx::<Expr>(3)))], inherit_froms: [], dynamics: [] })
                6: Attrset(Bindings { statics: [], inherit_froms: [], dynamics: [(Idx::<Expr>(0), Idx::<Expr>(4)), (Idx::<Expr>(2), Idx::<Expr>(5))] })
            "#]],
        );
    }

    #[test]
    fn let_empty() {
        check_lower(
            "let in 1",
            expect![[r#"
                0..6: Empty let-in

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
                4: LetIn(Bindings { statics: [(NameDef(Idx::<NameDef>(0)), Expr(Idx::<Expr>(2)))], inherit_froms: [], dynamics: [] }, Idx::<Expr>(3))

                0: NameDef { name: "a", kind: LetIn }
            "#]],
        );
    }

    #[test]
    fn invalid_dynamic_error() {
        check_error(
            "let ${a} = 1; in 1",
            expect![[r#"
                4..8: Invalid location of dynamic attribute
                0..16: Empty let-in
            "#]],
        );
        check_error(
            "{ inherit ${a}; }",
            expect![[r#"
                10..14: Invalid location of dynamic attribute
            "#]],
        );
        check_error(
            "{ inherit (a) ${a}; }",
            expect![[r#"
                14..18: Invalid location of dynamic attribute
            "#]],
        );
    }

    #[test]
    fn attrset_merge_error() {
        // Value and value.
        check_error(
            "{ a = 1; a = 2; }",
            expect![[r#"
                9..10: Duplicated name definition
                  2..3: Previously defined here
            "#]],
        );
        // Set and value.
        check_error(
            "{ a.b = 1; a = 2; }",
            expect![[r#"
                11..12: Duplicated name definition
                  2..3: Previously defined here
            "#]],
        );
        // Value and set.
        check_error(
            "{ a = 1; a.b = 2; }",
            expect![[r#"
                9..10: Duplicated name definition
                  2..3: Previously defined here
            "#]],
        );
        // Inherit and value.
        check_error(
            "{ inherit a; a = 1; }",
            expect![[r#"
                13..14: Duplicated name definition
                  10..11: Previously defined here
            "#]],
        );
        // Inherit-from and value.
        check_error(
            "{ inherit (1) a; a = 1; }",
            expect![[r#"
                17..18: Duplicated name definition
                  14..15: Previously defined here
            "#]],
        );
    }

    #[test]
    fn attrset_no_duplicated_duplicated_error() {
        check_error(
            "{ a = 1; a = 2; a = 3; }",
            expect![[r#"
                9..10: Duplicated name definition
                  2..3: Previously defined here
                16..17: Duplicated name definition
                  2..3: Previously defined here
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
