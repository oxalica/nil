use super::{AstPtr, Expr, ExprId, Literal, Module, ModuleSourceMap};
use crate::source::{FileId, InFile};
use rowan::ast::AstNode;
use syntax::ast::{self, LiteralKind};

pub(super) fn lower(root: InFile<ast::SourceFile>) -> (Module, ModuleSourceMap) {
    let mut ctx = LowerCtx {
        file_id: root.file_id,
        module: Module::default(),
        source_map: ModuleSourceMap::default(),
    };
    ctx.lower_expr_opt(root.value.expr());
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
            ast::Expr::Name(e) => {
                let name = e.token().map_or_else(|| "".into(), |tok| tok.text().into());
                self.alloc_expr(Expr::Ident(name), ptr)
            }
            ast::Expr::Apply(e) => {
                let func = self.lower_expr_opt(e.function());
                let arg = self.lower_expr_opt(e.argument());
                self.alloc_expr(Expr::Apply(func, arg), ptr)
            }
            ast::Expr::Paren(e) => self.lower_expr_opt(e.expr()),
            ast::Expr::Assert(_) => todo!(),
            ast::Expr::AttrSet(_) => todo!(),
            ast::Expr::BinaryOp(_) => todo!(),
            ast::Expr::HasAttr(_) => todo!(),
            ast::Expr::IfThenElse(_) => todo!(),
            ast::Expr::IndentString(_) => todo!(),
            ast::Expr::Lambda(_) => todo!(),
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
        let text = tok.text();

        Some(match kind {
            LiteralKind::Int => Literal::Int(text.parse::<i64>().ok()?),
            LiteralKind::Float => todo!(),
            LiteralKind::Uri => Literal::String(text.into()),
            LiteralKind::RelativePath => todo!(),
            LiteralKind::AbsolutePath => todo!(),
            LiteralKind::HomePath => todo!(),
            LiteralKind::SearchPath => todo!(),
        })
    }
}
