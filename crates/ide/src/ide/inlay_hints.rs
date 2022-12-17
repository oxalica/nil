use syntax::TextSize;

use crate::def::{Expr, ResolveResult};
use crate::{DefDatabase, FileRange};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InlayHint {
    pub pos: TextSize,
    pub kind: InlayKind,
    pub label: String,
    pub tooltip: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InlayKind {
    WithHint,
}

pub(crate) fn inlay_hints(
    db: &dyn DefDatabase,
    FileRange { file_id, range }: FileRange,
) -> Vec<InlayHint> {
    let src = db.file_content(file_id);
    let module = db.module(file_id);
    let source_map = db.source_map(file_id);
    let nameres = db.name_resolution(file_id);
    nameres
        .iter()
        .filter_map(|(expr, res)| {
            let ResolveResult::WithExprs(withs) = res else {
                return None;
            };
            let ident_range = source_map.node_for_expr(expr)?.text_range();
            ident_range.intersect(range)?;

            let label = "^".repeat(withs.len());
            let tooltip = withs
                .iter()
                .rev()
                .filter_map(|&with_expr| {
                    let Expr::With(env_expr, _) = module[with_expr] else { unreachable!() };
                    let range = source_map.node_for_expr(env_expr)?.text_range();
                    Some(format!("with {};\n", &src[range]))
                })
                .collect::<String>();
            Some(InlayHint {
                pos: ident_range.start(),
                kind: InlayKind::WithHint,
                label,
                tooltip: format!("```\n{tooltip}\n```"),
            })
        })
        .collect()
}
