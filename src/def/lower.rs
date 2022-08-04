use super::{
    AstPtr, Attrpath, BindingKey, BindingValue, Bindings, Expr, ExprId, Literal, Module,
    ModuleSourceMap, NameDef, NameDefId, Pat, Path, PathAnchor,
};
use crate::{Diagnostic, FileId, FileRange, InFile};
use indexmap::IndexMap;
use la_arena::Arena;
use rowan::ast::AstNode;
use rowan::TextRange;
use smol_str::SmolStr;
use std::str;
use syntax::ast::{self, HasBindings, HasStringParts, LiteralKind};
use syntax::Parse;

pub(super) fn lower(parse: InFile<Parse>) -> (Module, ModuleSourceMap) {
    let diagnostics = parse
        .value
        .errors()
        .iter()
        .map(|&(err, pos)| {
            Diagnostic::SyntaxError(InFile::new(parse.file_id, TextRange::empty(pos)), err)
        })
        .collect();

    let mut ctx = LowerCtx {
        file_id: parse.file_id,
        module: Module {
            exprs: Arena::new(),
            name_defs: Arena::new(),
            // Placeholder.
            entry_expr: ExprId::from_raw(0.into()),
            diagnostics,
        },
        source_map: ModuleSourceMap::default(),
    };

    let entry = ctx.lower_expr_opt(parse.value.root().expr());
    let mut module = ctx.module;
    module.entry_expr = entry;
    (module, ctx.source_map)
}

struct LowerCtx {
    file_id: FileId,
    module: Module,
    source_map: ModuleSourceMap,
}

impl LowerCtx {
    fn alloc_expr(&mut self, expr: Expr, ptr: AstPtr) -> ExprId {
        let id = self.module.exprs.alloc(expr);
        self.source_map.expr_map.insert(ptr.clone(), id);
        self.source_map.expr_map_rev.insert(id, ptr);
        id
    }

    fn alloc_name_def(&mut self, name: SmolStr, ptr: AstPtr) -> NameDefId {
        let id = self.module.name_defs.alloc(NameDef { name });
        self.source_map.name_def_map.insert(ptr.clone(), id);
        self.source_map.name_def_map_rev.insert(id, ptr);
        id
    }

    fn file_range(&self, ptr: &AstPtr) -> FileRange {
        InFile::new(self.file_id, ptr.text_range())
    }

    fn push_diagnostic(&mut self, diag: Diagnostic) {
        self.module.diagnostics.push(diag);
    }

    fn lower_name(&mut self, node: ast::Name) -> NameDefId {
        let name = node
            .token()
            .map_or_else(Default::default, |tok| tok.text().into());
        let ptr = AstPtr::new(node.syntax());
        self.alloc_name_def(name, ptr)
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
                    let name = param.name().map(|n| self.lower_name(n));
                    let pat = param.pat().map(|pat| {
                        let fields = pat
                            .fields()
                            .map(|field| {
                                let field_name = field.name().map(|n| self.lower_name(n));
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
                let mut set = MergingSet::new(true, ptr.clone());
                set.merge_bindings(self, &e);
                let bindings = set.finish(self);
                for (key, _) in bindings.entries.iter() {
                    if let BindingKey::Dynamic(expr) = key {
                        if let Some(ptr) = self.source_map.expr_node(*expr) {
                            let range = self.file_range(&ptr);
                            self.push_diagnostic(Diagnostic::InvalidDynamic(range));
                        }
                    }
                }
                let body = self.lower_expr_opt(e.body());
                self.alloc_expr(Expr::LetIn(bindings, body), ptr)
            }
            ast::Expr::AttrSet(e) => {
                let (is_rec, ctor): (bool, fn(_) -> _) = if e.rec_token().is_some() {
                    (true, Expr::Attrset)
                } else if e.let_token().is_some() {
                    (true, Expr::LetAttrset)
                } else {
                    (false, Expr::Attrset)
                };
                let mut set = MergingSet::new(is_rec, ptr.clone());
                set.merge_bindings(self, &e);
                let bindings = set.finish(self);
                self.alloc_expr(ctor(bindings), ptr)
            }
        }
    }

    fn lower_literal(&mut self, lit: ast::Literal) -> Option<Literal> {
        let kind = lit.kind()?;
        let tok = lit.token().unwrap();
        let mut text = tok.text();

        fn normalize_path(path: &str) -> (usize, SmolStr) {
            let mut ret = String::new();
            let mut supers = 0usize;
            for seg in path.split('/').filter(|&seg| !seg.is_empty() && seg != ".") {
                if seg != ".." {
                    if !ret.is_empty() {
                        ret.push('/');
                    }
                    ret.push_str(seg);
                } else if ret.is_empty() {
                    supers += 1;
                } else {
                    let last_slash = ret.bytes().rposition(|c| c != b'/').unwrap_or(0);
                    ret.truncate(last_slash);
                }
            }
            (supers, ret.into())
        }

        Some(match kind {
            LiteralKind::Int => Literal::Int(text.parse::<i64>().ok()?),
            LiteralKind::Float => Literal::Float(text.parse::<f64>().unwrap().into()),
            LiteralKind::Uri => Literal::String(text.into()),
            LiteralKind::SearchPath => {
                text = &text[1..text.len() - 1]; // Strip '<' and '>'.
                let (search_name, relative_path) = text.split_once('/').unwrap_or((text, ""));
                let anchor = PathAnchor::Search(search_name.into());
                let (supers, raw_segments) = normalize_path(relative_path);
                Literal::Path(Path {
                    anchor,
                    supers,
                    raw_segments,
                })
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
                let (mut supers, raw_segments) = normalize_path(text);
                // Extra ".." has no effect for absolute path.
                if anchor == PathAnchor::Absolute {
                    supers = 0;
                }
                Literal::Path(Path {
                    anchor,
                    supers,
                    raw_segments,
                })
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

    fn lower_key(&mut self, is_rec: bool, attr: ast::Attr) -> BindingKey {
        let ast_string = match attr {
            ast::Attr::Name(n) if is_rec => return BindingKey::NameDef(self.lower_name(n)),
            ast::Attr::Name(n) => {
                return BindingKey::Name(
                    n.token()
                        .map_or_else(Default::default, |tok| tok.text().into()),
                )
            }
            ast::Attr::String(s) => s,
            ast::Attr::Dynamic(d) => {
                let mut e = d.expr();
                loop {
                    match e {
                        Some(ast::Expr::String(s)) => break s,
                        Some(ast::Expr::Paren(p)) => e = p.expr(),
                        _ => return BindingKey::Dynamic(self.lower_expr_opt(e)),
                    }
                }
            }
        };

        if ast_string
            .string_parts()
            .all(|part| !matches!(part, ast::StringPart::Dynamic(_)))
        {
            let ptr = AstPtr::new(ast_string.syntax());
            let content = ast_string
                .string_parts()
                .fold(String::new(), |prev, part| match part {
                    ast::StringPart::Dynamic(_) => unreachable!(),
                    ast::StringPart::Fragment(tok) => prev + tok.text(),
                    ast::StringPart::Escape(tok) => match tok.text().as_bytes() {
                        b"\\n" => prev + "\n",
                        b"\\r" => prev + "\r",
                        b"\\t" => prev + "\t",
                        [b'\\', bytes @ ..] => {
                            prev + str::from_utf8(bytes).expect("Verified by the lexer")
                        }
                        _ => unreachable!("Verified by the lexer"),
                    },
                });
            if is_rec {
                return BindingKey::NameDef(self.alloc_name_def(content.into(), ptr));
            } else {
                return BindingKey::Name(content.into());
            }
        }

        BindingKey::Dynamic(self.lower_string(&ast_string))
    }
}

struct MergingSet {
    is_rec: bool,
    ptr: AstPtr,
    entries: IndexMap<BindingKey, MergingValue>,
    inherit_froms: Vec<ExprId>,
}

#[derive(Default)]
enum MergingValue {
    #[default]
    Placeholder,
    Attrset(MergingSet),
    Final(BindingValue),
}

impl MergingSet {
    fn new(is_rec: bool, ptr: AstPtr) -> Self {
        Self {
            is_rec,
            ptr,
            entries: Default::default(),
            inherit_froms: Vec::new(),
        }
    }

    // Place an orphaned Expr as dynamic-attrs for error recovery.
    fn recover_error(&mut self, ctx: &mut LowerCtx, expr: ExprId, ptr: AstPtr) {
        let k = BindingKey::Dynamic(ctx.alloc_expr(Expr::Missing, ptr));
        let v = MergingValue::Final(BindingValue::Expr(expr));
        self.entries.insert(k, v);
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
            let from_id = self.inherit_froms.len() as u32;
            self.inherit_froms.push(expr);
            from_id
        });

        for attr in i.attrs() {
            let ptr = AstPtr::new(attr.syntax());
            let key = match ctx.lower_key(self.is_rec, attr) {
                // `inherit ${expr}` or `inherit (expr) ${expr}` is invalid.
                BindingKey::Dynamic(expr) => {
                    ctx.push_diagnostic(Diagnostic::InvalidDynamic(ctx.file_range(&ptr)));
                    self.recover_error(ctx, expr, ptr.clone());
                    continue;
                }
                key => key,
            };

            // Inherited names never merge other values. It must be an error.
            if let Some(v) = self.entries.get(&key) {
                v.emit_duplicated_key(ctx, &ptr);
                continue;
            }

            let value = match from_expr {
                Some(id) => MergingValue::Final(BindingValue::InheritFrom(id)),
                None => {
                    let name = match &key {
                        BindingKey::NameDef(def) => ctx.module[*def].name.clone(),
                        BindingKey::Name(s) => s.clone(),
                        BindingKey::Dynamic(_) => unreachable!(),
                    };
                    let ref_expr = ctx.alloc_expr(Expr::Reference(name), ptr);
                    MergingValue::Final(BindingValue::Inherit(ref_expr))
                }
            };
            self.entries.insert(key, value);
        }
    }

    fn merge_path_value(
        mut self: &mut Self,
        // &mut self,
        ctx: &mut LowerCtx,
        entry: ast::AttrpathValue,
    ) {
        let mut attrs = entry.attrpath().into_iter().flat_map(|path| path.attrs());
        let mut next_attr = match attrs.next() {
            Some(first_attr) => first_attr,
            // Recover from missing Attrpath. This is already a syntax error,
            // don't report it again.
            None => {
                let value_expr = ctx.lower_expr_opt(entry.value());
                self.recover_error(ctx, value_expr, AstPtr::new(entry.syntax()));
                return;
            }
        };

        loop {
            let key = ctx.lower_key(self.is_rec, next_attr);
            let deep = self.entries.entry(key).or_default();
            match attrs.next() {
                Some(attr) => {
                    self = deep.make_attrset(ctx, false, AstPtr::new(attr.syntax()));
                    next_attr = attr;
                }
                None => {
                    deep.merge_ast(ctx, entry.value());
                    return;
                }
            }
        }
    }

    fn finish(self, ctx: &mut LowerCtx) -> Bindings {
        Bindings {
            entries: self
                .entries
                .into_iter()
                .map(|(k, v)| (k, v.finish(ctx)))
                .collect(),
            inherit_froms: self.inherit_froms.into(),
        }
    }
}

impl MergingValue {
    fn make_attrset(&mut self, ctx: &mut LowerCtx, is_rec: bool, ptr: AstPtr) -> &mut MergingSet {
        match self {
            // TODO: Report warnings if set.is_rec
            Self::Attrset(set) => return set,
            Self::Placeholder => *self = Self::Attrset(MergingSet::new(is_rec, ptr)),
            // We prefer to become a Attrset as a guess, which supports further merging.
            Self::Final(v) => {
                let mut set = MergingSet::new(is_rec, ptr.clone());
                if let BindingValue::Expr(expr) = *v {
                    if let Some(prev_ptr) = ctx.source_map.expr_node(expr) {
                        set.recover_error(ctx, expr, prev_ptr);
                    }
                }
                self.emit_duplicated_key(ctx, &ptr);
                *self = Self::Attrset(set);
            }
        }
        match self {
            Self::Attrset(set) => set,
            _ => unreachable!(),
        }
    }

    fn merge_ast(&mut self, ctx: &mut LowerCtx, mut e: Option<ast::Expr>) {
        loop {
            match e {
                Some(ast::Expr::Paren(p)) => e = p.expr(),
                Some(ast::Expr::AttrSet(e)) if e.let_token().is_none() => {
                    return self
                        .make_attrset(ctx, e.rec_token().is_some(), AstPtr::new(e.syntax()))
                        .merge_bindings(ctx, &e);
                }
                _ => break,
            }
        }
        // Here we got an unmergable non-Attrset Expr.
        match self {
            Self::Placeholder => *self = Self::Final(BindingValue::Expr(ctx.lower_expr_opt(e))),
            Self::Attrset(_) | Self::Final(_) => {
                // Only emit errors if there is RHS.
                if let Some(e) = e {
                    self.emit_duplicated_key(ctx, &AstPtr::new(e.syntax()));
                }
            }
        }
    }

    fn emit_duplicated_key(&self, ctx: &mut LowerCtx, ptr: &AstPtr) {
        let cur_range = ctx.file_range(ptr);
        let prev_range = match self {
            Self::Placeholder => unreachable!(),
            Self::Attrset(set) => ctx.file_range(&set.ptr),
            Self::Final(BindingValue::Inherit(prev_expr) | BindingValue::Expr(prev_expr)) => {
                match ctx.source_map.expr_node(*prev_expr) {
                    Some(ptr) => ctx.file_range(&ptr),
                    None => return,
                }
            }
            Self::Final(BindingValue::InheritFrom(_)) => {
                // FIXME: Cannot get inherit from attrs.
                cur_range
            }
        };
        ctx.push_diagnostic(Diagnostic::DuplicatedKey(prev_range, cur_range));
    }

    fn finish(self, ctx: &mut LowerCtx) -> BindingValue {
        match self {
            Self::Placeholder => unreachable!("Should be processed"),
            Self::Final(k) => k,
            Self::Attrset(set) => {
                let ptr = set.ptr.clone();
                let bindings = set.finish(ctx);
                let expr = ctx.alloc_expr(Expr::Attrset(bindings), ptr);
                BindingValue::Expr(expr)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::lower;
    use crate::base::{FileId, InFile};
    use expect_test::{expect, Expect};
    use std::fmt::Write;
    use syntax::parse_file;

    fn check_lower(src: &str, expect: Expect) {
        let parse = parse_file(src);
        let (module, _source_map) = lower(InFile::new(FileId(0), parse));
        let mut got = String::new();
        for (i, e) in module.exprs.iter() {
            writeln!(got, "{}: {:?}", i.into_raw(), e).unwrap();
        }
        if !module.name_defs.is_empty() {
            writeln!(got).unwrap();
        }
        for (i, def) in module.name_defs.iter() {
            writeln!(got, "{}: {:?}", i.into_raw(), def).unwrap();
        }
        assert!(module.diagnostics().is_empty());
        expect.assert_eq(&got);
    }

    fn check_error(src: &str, expect: Expect) {
        let parse = parse_file(src);
        let (module, _source_map) = lower(InFile::new(FileId(0), parse));
        let mut got = String::new();
        for diag in module.diagnostics() {
            writeln!(got, "{:?}", diag).unwrap();
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
                0: Literal(String("a:b"))
            "#]],
        );
    }

    #[test]
    fn path() {
        check_lower(
            "./.",
            expect![[r#"
                0: Literal(Path(Path { anchor: Relative(FileId(0)), supers: 0, raw_segments: "" }))
            "#]],
        );
        check_lower(
            "../.",
            expect![[r#"
                0: Literal(Path(Path { anchor: Relative(FileId(0)), supers: 1, raw_segments: "" }))
            "#]],
        );
        check_lower(
            "../a/../../.b/./c",
            expect![[r#"
                0: Literal(Path(Path { anchor: Relative(FileId(0)), supers: 2, raw_segments: ".b/c" }))
            "#]],
        );
        check_lower(
            "/../a/../../.b/./c",
            expect![[r#"
                0: Literal(Path(Path { anchor: Absolute, supers: 0, raw_segments: ".b/c" }))
            "#]],
        );
        check_lower(
            "~/../a/../../.b/./c",
            expect![[r#"
                0: Literal(Path(Path { anchor: Home, supers: 2, raw_segments: ".b/c" }))
            "#]],
        );
        check_lower(
            "<p/../a/../../.b/./c>",
            expect![[r#"
                0: Literal(Path(Path { anchor: Search("p"), supers: 2, raw_segments: ".b/c" }))
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

                0: NameDef { name: "a" }
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

                0: NameDef { name: "a" }
            "#]],
        );
        check_lower(
            "{ } @ a: 0",
            expect![[r#"
                0: Literal(Int(0))
                1: Lambda(Some(Idx::<NameDef>(0)), Some(Pat { fields: [], ellipsis: false }), Idx::<Expr>(0))

                0: NameDef { name: "a" }
            "#]],
        );
        check_lower(
            "{ a, b ? 0, ... }: 0",
            expect![[r#"
                0: Literal(Int(0))
                1: Literal(Int(0))
                2: Lambda(None, Some(Pat { fields: [(Some(Idx::<NameDef>(0)), None), (Some(Idx::<NameDef>(1)), Some(Idx::<Expr>(0)))], ellipsis: true }), Idx::<Expr>(1))

                0: NameDef { name: "a" }
                1: NameDef { name: "b" }
            "#]],
        );
        check_lower(
            "{ a ? 0, b }@c: 0",
            expect![[r#"
                0: Literal(Int(0))
                1: Literal(Int(0))
                2: Lambda(Some(Idx::<NameDef>(0)), Some(Pat { fields: [(Some(Idx::<NameDef>(1)), Some(Idx::<Expr>(0))), (Some(Idx::<NameDef>(2)), None)], ellipsis: false }), Idx::<Expr>(1))

                0: NameDef { name: "c" }
                1: NameDef { name: "a" }
                2: NameDef { name: "b" }
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
                5: Attrset(Bindings { entries: [(Name("a"), Expr(Idx::<Expr>(0))), (Dynamic(Idx::<Expr>(1)), Expr(Idx::<Expr>(2))), (Name("c"), Expr(Idx::<Expr>(3))), (Name("\n"), Expr(Idx::<Expr>(4)))], inherit_froms: [] })
            "#]],
        );
    }

    #[test]
    fn attrset_inherit() {
        check_lower(
            r#"{ inherit; inherit a "b" ${("c")}; inherit (d); inherit (e) f g; }"#,
            expect![[r#"
                0: Reference("a")
                1: Reference("b")
                2: Reference("c")
                3: Reference("d")
                4: Reference("e")
                5: Attrset(Bindings { entries: [(Name("a"), Inherit(Idx::<Expr>(0))), (Name("b"), Inherit(Idx::<Expr>(1))), (Name("c"), Inherit(Idx::<Expr>(2))), (Name("f"), InheritFrom(1)), (Name("g"), InheritFrom(1))], inherit_froms: [Idx::<Expr>(3), Idx::<Expr>(4)] })
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
                2: Attrset(Bindings { entries: [(Name("b"), Expr(Idx::<Expr>(0))), (Name("c"), Expr(Idx::<Expr>(1)))], inherit_froms: [] })
                3: Attrset(Bindings { entries: [(Name("a"), Expr(Idx::<Expr>(2)))], inherit_froms: [] })
            "#]],
        );
        // Path and attrset.
        check_lower(
            "{ a.b = 1; a = { c = 2; }; }",
            expect![[r#"
                0: Literal(Int(1))
                1: Literal(Int(2))
                2: Attrset(Bindings { entries: [(Name("b"), Expr(Idx::<Expr>(0))), (Name("c"), Expr(Idx::<Expr>(1)))], inherit_froms: [] })
                3: Attrset(Bindings { entries: [(Name("a"), Expr(Idx::<Expr>(2)))], inherit_froms: [] })
            "#]],
        );
        // Attrset and path.
        check_lower(
            "{ a = { b = 1; }; a.c = 2; }",
            expect![[r#"
                0: Literal(Int(1))
                1: Literal(Int(2))
                2: Attrset(Bindings { entries: [(Name("b"), Expr(Idx::<Expr>(0))), (Name("c"), Expr(Idx::<Expr>(1)))], inherit_froms: [] })
                3: Attrset(Bindings { entries: [(Name("a"), Expr(Idx::<Expr>(2)))], inherit_froms: [] })
            "#]],
        );
        // Attrset and attrset.
        check_lower(
            "{ a = { b = 1; }; a = { c = 2; }; }",
            expect![[r#"
                0: Literal(Int(1))
                1: Literal(Int(2))
                2: Attrset(Bindings { entries: [(Name("b"), Expr(Idx::<Expr>(0))), (Name("c"), Expr(Idx::<Expr>(1)))], inherit_froms: [] })
                3: Attrset(Bindings { entries: [(Name("a"), Expr(Idx::<Expr>(2)))], inherit_froms: [] })
            "#]],
        );
    }

    #[test]
    fn attrset_merge_rec() {
        // Rec and non-rec.
        check_lower(
            "{ a = rec { b = 1; }; a.c = 2; }",
            expect![[r#"
                0: Literal(Int(1))
                1: Literal(Int(2))
                2: Attrset(Bindings { entries: [(NameDef(Idx::<NameDef>(0)), Expr(Idx::<Expr>(0))), (NameDef(Idx::<NameDef>(1)), Expr(Idx::<Expr>(1)))], inherit_froms: [] })
                3: Attrset(Bindings { entries: [(Name("a"), Expr(Idx::<Expr>(2)))], inherit_froms: [] })

                0: NameDef { name: "b" }
                1: NameDef { name: "c" }
            "#]],
        );
        // Non-rec and rec.
        check_lower(
            "{ a = { b = 1; }; a = rec { c = 2; }; }",
            expect![[r#"
                0: Literal(Int(1))
                1: Literal(Int(2))
                2: Attrset(Bindings { entries: [(Name("b"), Expr(Idx::<Expr>(0))), (Name("c"), Expr(Idx::<Expr>(1)))], inherit_froms: [] })
                3: Attrset(Bindings { entries: [(Name("a"), Expr(Idx::<Expr>(2)))], inherit_froms: [] })
            "#]],
        );
    }

    #[test]
    fn attrset_rec_deep() {
        check_lower(
            "rec { a.b = 1; }",
            expect![[r#"
                0: Literal(Int(1))
                1: Attrset(Bindings { entries: [(Name("b"), Expr(Idx::<Expr>(0)))], inherit_froms: [] })
                2: Attrset(Bindings { entries: [(NameDef(Idx::<NameDef>(0)), Expr(Idx::<Expr>(1)))], inherit_froms: [] })

                0: NameDef { name: "a" }
            "#]],
        );
    }

    #[test]
    fn attrset_let() {
        check_lower(
            "{ a.b = let { c.d = 1; }; }",
            expect![[r#"
                0: Literal(Int(1))
                1: Attrset(Bindings { entries: [(Name("d"), Expr(Idx::<Expr>(0)))], inherit_froms: [] })
                2: LetAttrset(Bindings { entries: [(NameDef(Idx::<NameDef>(0)), Expr(Idx::<Expr>(1)))], inherit_froms: [] })
                3: Attrset(Bindings { entries: [(Name("b"), Expr(Idx::<Expr>(2)))], inherit_froms: [] })
                4: Attrset(Bindings { entries: [(Name("a"), Expr(Idx::<Expr>(3)))], inherit_froms: [] })

                0: NameDef { name: "c" }
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
                4: Attrset(Bindings { entries: [(Name("b"), Expr(Idx::<Expr>(1)))], inherit_froms: [] })
                5: Attrset(Bindings { entries: [(Name("b"), Expr(Idx::<Expr>(3)))], inherit_froms: [] })
                6: Attrset(Bindings { entries: [(Dynamic(Idx::<Expr>(0)), Expr(Idx::<Expr>(4))), (Dynamic(Idx::<Expr>(2)), Expr(Idx::<Expr>(5)))], inherit_froms: [] })
            "#]],
        );
    }

    #[test]
    fn invalid_dynamic_error() {
        check_error(
            "let ${a} = 1; in 1",
            expect![[r#"
                InvalidDynamic(InFile { file_id: FileId(0), value: 6..7 })
            "#]],
        );
        check_error(
            "{ inherit ${a}; }",
            expect![[r#"
                InvalidDynamic(InFile { file_id: FileId(0), value: 10..14 })
            "#]],
        );
        check_error(
            "{ inherit (a) ${a}; }",
            expect![[r#"
                InvalidDynamic(InFile { file_id: FileId(0), value: 14..18 })
            "#]],
        );
    }

    // FIXME: The location is not quite right currently.
    #[test]
    fn attrset_merge_error() {
        // Value and value.
        check_error(
            "{ a = 1; a = 2; }",
            expect![[r#"
                DuplicatedKey(InFile { file_id: FileId(0), value: 6..7 }, InFile { file_id: FileId(0), value: 13..14 })
            "#]],
        );
        // Set and value.
        check_error(
            "{ a.b = 1; a = 2; }",
            expect![[r#"
                DuplicatedKey(InFile { file_id: FileId(0), value: 4..5 }, InFile { file_id: FileId(0), value: 15..16 })
            "#]],
        );
        // Value and set.
        check_error(
            "{ a = 1; a.b = 2; }",
            expect![[r#"
                DuplicatedKey(InFile { file_id: FileId(0), value: 6..7 }, InFile { file_id: FileId(0), value: 11..12 })
            "#]],
        );
        // Inherit and value.
        check_error(
            "{ inherit a; a = 1; }",
            expect![[r#"
                DuplicatedKey(InFile { file_id: FileId(0), value: 10..11 }, InFile { file_id: FileId(0), value: 17..18 })
            "#]],
        );
    }

    #[test]
    fn attrset_malformed_no_panic() {
        let src = "{ } @ y: y { cc, extraPackages ? optional (cc.isGNU) }: 1";
        let parse = parse_file(src);
        let (_module, _source_map) = lower(InFile::new(FileId(0), parse));
    }
}
