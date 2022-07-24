use super::{
    AstPtr, Expr, ExprId, Literal, Module, ModuleSourceMap, NameDef, NameDefId, Pat, Path,
    PathAnchor,
};
use crate::source::{FileId, InFile};
use la_arena::Arena;
use rowan::ast::AstNode;
use syntax::ast::{self, LiteralKind};

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

    fn alloc_name_def(&mut self, node: ast::Name) -> NameDefId {
        let name = node
            .token()
            .map_or_else(Default::default, |tok| tok.text().into());
        let id = self.module.name_defs.alloc(NameDef { name });
        let ptr = AstPtr::new(node.syntax());
        self.source_map.name_def_map.insert(ptr, id);
        id
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
                self.alloc_expr(Expr::Ident(name), ptr)
            }
            ast::Expr::Apply(e) => {
                let func = self.lower_expr_opt(e.function());
                let arg = self.lower_expr_opt(e.argument());
                self.alloc_expr(Expr::Apply(func, arg), ptr)
            }
            ast::Expr::Paren(e) => self.lower_expr_opt(e.expr()),
            ast::Expr::Lambda(e) => {
                let (param, pat) = e.param().map_or((None, None), |param| {
                    let name = param.name().map(|n| self.alloc_name_def(n));
                    let pat = param.pat().map(|pat| {
                        let fields = pat
                            .fields()
                            .map(|field| {
                                let field_name = field.name().map(|n| self.alloc_name_def(n));
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
            ast::Expr::Assert(_) => todo!(),
            ast::Expr::AttrSet(_) => todo!(),
            ast::Expr::BinaryOp(_) => todo!(),
            ast::Expr::HasAttr(_) => todo!(),
            ast::Expr::IfThenElse(_) => todo!(),
            ast::Expr::IndentString(_) => todo!(),
            ast::Expr::LetIn(_) => todo!(),
            ast::Expr::List(_) => todo!(),
            ast::Expr::Select(_) => todo!(),
            ast::Expr::String(_) => todo!(),
            ast::Expr::UnaryOp(_) => todo!(),
            ast::Expr::With(_) => todo!(),
        }
    }

    fn lower_literal(&mut self, lit: ast::Literal) -> Option<Literal> {
        let kind = lit.kind()?;
        let tok = lit.token().unwrap();
        let mut text = tok.text();

        fn normalize_path(path: &str) -> (usize, Box<str>) {
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
}
