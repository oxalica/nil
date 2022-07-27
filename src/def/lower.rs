use super::{
    AstPtr, Attrpath, Expr, ExprId, Literal, Module, ModuleSourceMap, NameDef, NameDefId, Pat,
    Path, PathAnchor,
};
use crate::base::{FileId, InFile};
use la_arena::Arena;
use rowan::ast::AstNode;
use smol_str::SmolStr;
use std::str;
use syntax::ast::{self, HasStringParts, LiteralKind};

pub(super) fn lower(root: InFile<ast::SourceFile>) -> (Module, ModuleSourceMap) {
    let mut ctx = LowerCtx {
        file_id: root.file_id,
        module: Module {
            exprs: Arena::new(),
            name_defs: Arena::new(),
            // Placeholder.
            entry_expr: ExprId::from_raw(0.into()),
        },
        source_map: ModuleSourceMap::default(),
    };
    let entry = ctx.lower_expr_opt(root.value.expr());
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
            ast::Expr::AttrSet(_) => todo!(),
            ast::Expr::LetIn(_) => todo!(),
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
            LiteralKind::RelativePath
            | LiteralKind::AbsolutePath
            | LiteralKind::HomePath
            | LiteralKind::SearchPath => {
                let anchor = match kind {
                    LiteralKind::RelativePath => PathAnchor::Relative(self.file_id),
                    LiteralKind::AbsolutePath => PathAnchor::Absolute,
                    LiteralKind::HomePath => {
                        text = &text[2..]; // Strip "~/".
                        PathAnchor::Home
                    }
                    LiteralKind::SearchPath => {
                        text = &text[1..text.len() - 1]; // Strip '<' and '>'.
                        let (search_name, path) = text.split_once('/').unwrap();
                        text = path;
                        PathAnchor::Search(search_name.into())
                    }
                    _ => unreachable!(),
                };
                let (mut supers, raw_segments) = normalize_path(text);
                if kind == LiteralKind::AbsolutePath {
                    // Extra ".." has no effect for absolute path.
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
        let (module, _source_map) = lower(InFile::new(FileId(0), parse.root()));
        let mut got = String::new();
        for (i, e) in module.exprs.iter() {
            writeln!(got, "{}: {:?}", i.into_raw(), e).unwrap();
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
            "#]],
        );
        check_lower(
            "{ }@a: 0",
            expect![[r#"
                0: Literal(Int(0))
                1: Lambda(Some(Idx::<NameDef>(0)), Some(Pat { fields: [], ellipsis: false }), Idx::<Expr>(0))
            "#]],
        );
        check_lower(
            "{ a, b ? 0, ... }: 0",
            expect![[r#"
                0: Literal(Int(0))
                1: Literal(Int(0))
                2: Lambda(None, Some(Pat { fields: [(Some(Idx::<NameDef>(0)), None), (Some(Idx::<NameDef>(1)), Some(Idx::<Expr>(0)))], ellipsis: true }), Idx::<Expr>(1))
            "#]],
        );
        check_lower(
            "{ a ? 0, b }@c: 0",
            expect![[r#"
                0: Literal(Int(0))
                1: Literal(Int(0))
                2: Lambda(Some(Idx::<NameDef>(0)), Some(Pat { fields: [(Some(Idx::<NameDef>(1)), Some(Idx::<Expr>(0))), (Some(Idx::<NameDef>(2)), None)], ellipsis: false }), Idx::<Expr>(1))
            "#]],
        );
    }
}
