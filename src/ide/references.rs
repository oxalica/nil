use crate::def::{AstPtr, DefDatabase};
use crate::{FileId, FileRange, InFile};
use rowan::ast::AstNode;
use rowan::TextSize;
use syntax::{ast, match_ast, SyntaxKind};

pub(crate) fn references(
    db: &dyn DefDatabase,
    file_id: FileId,
    pos: TextSize,
) -> Option<Vec<FileRange>> {
    let parse = db.parse(file_id).value;
    let tok = parse.syntax_node().token_at_offset(pos).right_biased()?;
    if !matches!(tok.kind(), SyntaxKind::IDENT | SyntaxKind::STRING) {
        return None;
    }
    let ptr = tok.parent_ancestors().find_map(|node| {
        match_ast! {
            match node {
                ast::Attr(n) => Some(AstPtr::new(n.syntax())),
                _ => None,
            }
        }
    })?;

    let source_map = db.source_map(file_id);
    let def = source_map.node_name_def(ptr)?;

    let ref_map = db.name_reference_map(file_id);
    let refs = ref_map.references(def).map_or_else(Vec::new, |exprs| {
        exprs
            .iter()
            .map(|&expr| {
                let ptr = source_map.expr_node(expr).expect("Id must be valid");
                InFile::new(file_id, ptr.text_range())
            })
            .collect()
    });
    Some(refs)
}
