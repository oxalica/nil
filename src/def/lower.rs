use super::{AstPtr, Expr, ExprId, Literal, Module, ModuleSourceMap, Path, PathAnchor};
use crate::source::{FileId, InFile};
use rnix::types::{ParsedType, Root, TokenWrapper, TypedNode, Wrapper};
use rnix::value::Anchor;
use rnix::{NixValue, SyntaxNode, AST};

pub(super) fn lower(ast: InFile<AST>) -> (Module, ModuleSourceMap) {
    let mut ctx = LowerCtx {
        file_id: ast.file_id,
        module: Module::default(),
        source_map: ModuleSourceMap::default(),
    };
    let node = Root::cast(ast.value.node()).map(|root| root.node().clone());
    ctx.lower_node_opt(node);
    (ctx.module, ctx.source_map)
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

    fn lower_node_opt(&mut self, node: Option<SyntaxNode>) -> ExprId {
        if let Some(node) = node {
            if let Ok(expr) = ParsedType::try_from(node) {
                return self.lower_expr(expr);
            }
        }
        // Synthetic syntax has no coresponding text.
        self.module.exprs.alloc(Expr::Missing)
    }

    fn lower_path(&mut self, anchor: Anchor, segments: &str, ptr: AstPtr) -> ExprId {
        let anchor = match anchor {
            Anchor::Relative => PathAnchor::Relative(self.file_id),
            Anchor::Absolute => todo!(),
            Anchor::Home => todo!(),
            Anchor::Store => todo!(),
        };
        let path = Path {
            anchor,
            raw_segments: segments.into(),
        };
        self.alloc_expr(Expr::Literal(Literal::Path(path)), ptr)
    }

    fn lower_expr(&mut self, expr: ParsedType) -> ExprId {
        let ptr = AstPtr::new(expr.node());
        let expr = match expr {
            ParsedType::Root(e) => return self.lower_node_opt(e.inner()),
            ParsedType::Paren(e) => return self.lower_node_opt(e.inner()),
            ParsedType::Ident(e) => {
                let ident = e.to_inner_string();
                Expr::Ident(ident.into())
            }
            ParsedType::Value(e) => match e.to_value() {
                Ok(v) => {
                    let lit = match v {
                        NixValue::Integer(x) => Literal::Int(x),
                        NixValue::Float(_) => todo!(),
                        NixValue::String(s) => Literal::String(s.into()),
                        NixValue::Path(anchor, path) => return self.lower_path(anchor, &path, ptr),
                    };
                    Expr::Literal(lit)
                }
                Err(_) => Expr::Missing,
            },
            ParsedType::Apply(e) => {
                let lam = self.lower_node_opt(e.lambda());
                let arg = self.lower_node_opt(e.value());
                Expr::Apply(lam, arg)
            }
            ParsedType::Assert(_) => todo!(),
            ParsedType::IfElse(_) => todo!(),
            ParsedType::Select(_) => todo!(),
            ParsedType::Inherit(_) => todo!(),
            ParsedType::InheritFrom(_) => todo!(),
            ParsedType::Lambda(_) => todo!(),
            ParsedType::LegacyLet(_) => todo!(),
            ParsedType::LetIn(_) => todo!(),
            ParsedType::List(_) => todo!(),
            ParsedType::BinOp(_) => todo!(),
            ParsedType::OrDefault(_) => todo!(),
            ParsedType::AttrSet(_) => todo!(),
            ParsedType::KeyValue(_) => todo!(),
            ParsedType::Str(_) => todo!(),
            ParsedType::StrInterpol(_) => todo!(),
            ParsedType::UnaryOp(_) => todo!(),
            ParsedType::With(_) => todo!(),
            ParsedType::PathWithInterpol(_) => todo!(),

            // Invalid nodes in expression location.
            ParsedType::Pattern(_)
            | ParsedType::PatBind(_)
            | ParsedType::PatEntry(_)
            | ParsedType::Key(_)
            | ParsedType::Dynamic(_)
            | ParsedType::Error(_) => Expr::Missing,
        };
        self.alloc_expr(expr, ptr)
    }
}
