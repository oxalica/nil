use std::collections::HashSet;

use super::{Expr, ExprId, Literal, Module, ModuleSourceMap, Path, PathAnchor};
use crate::source::{FileId, InFile};
use rnix::types::{self as ast, ParsedType, TokenWrapper, TypedNode, Wrapper};
use rnix::value::Anchor;
use rnix::{NixValue, SyntaxNode};

pub(super) fn lower(root: InFile<ast::Root>) -> (Module, ModuleSourceMap) {
    let mut ctx = LowerCtx {
        file_id: root.file_id,
        module: Module::default(),
        paths: HashSet::default(),
    };
    ctx.lower_node_opt(Some(root.value.node().clone()));
    (ctx.module, ModuleSourceMap::default())
}

struct LowerCtx {
    file_id: FileId,
    module: Module,
    paths: HashSet<Path>,
}

impl LowerCtx {
    fn alloc_expr(&mut self, expr: Expr) -> ExprId {
        self.module.exprs.alloc(expr)
    }

    fn alloc_missing(&mut self) -> ExprId {
        self.alloc_expr(Expr::Missing)
    }

    fn lower_node_opt(&mut self, node: Option<SyntaxNode>) -> ExprId {
        if let Some(node) = node {
            if let Ok(expr) = ParsedType::try_from(node) {
                return self.lower_expr(expr);
            }
        }
        self.alloc_missing()
    }

    fn lower_path(&mut self, anchor: Anchor, segments: &str) -> ExprId {
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
        self.paths.insert(path.clone());
        self.alloc_expr(Expr::Literal(Literal::Path(path)))
    }

    fn lower_expr(&mut self, expr: ParsedType) -> ExprId {
        match expr {
            ParsedType::Root(e) => self.lower_node_opt(e.inner()),
            ParsedType::Paren(e) => self.lower_node_opt(e.inner()),
            ParsedType::Ident(e) => {
                let ident = e.to_inner_string();
                self.alloc_expr(Expr::Ident(ident.into()))
            }
            ParsedType::Value(e) => match e.to_value() {
                Ok(v) => {
                    let lit = match v {
                        NixValue::Integer(x) => Literal::Int(x),
                        NixValue::Float(_) => todo!(),
                        NixValue::String(s) => Literal::String(s.into()),
                        NixValue::Path(anchor, path) => return self.lower_path(anchor, &path),
                    };
                    self.alloc_expr(Expr::Literal(lit))
                }
                Err(_) => self.alloc_missing(),
            },
            ParsedType::Apply(e) => {
                let lam = self.lower_node_opt(e.lambda());
                let arg = self.lower_node_opt(e.value());
                self.alloc_expr(Expr::Apply(lam, arg))
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
            | ParsedType::Error(_) => self.alloc_missing(),
        }
    }
}
