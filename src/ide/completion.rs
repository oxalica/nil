use crate::def::{AstPtr, DefDatabase};
use crate::{builtin, FileId};
use rowan::ast::AstNode;
use smol_str::SmolStr;
use syntax::{ast, match_ast, SyntaxKind, TextRange, TextSize, T};

/// A single completion variant in the editor pop-up.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompletionItem {
    /// The label to show in the completion menu.
    pub label: SmolStr,
    /// Range of identifier that is being completed.
    pub source_range: TextRange,
    /// What content replaces the source range when user selects this item.
    pub replace: SmolStr,
    /// What item (struct, function, etc) are we completing.
    pub kind: CompletionItemKind,
}

/// The type of the completion item.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CompletionItemKind {
    Builtin,
    Binding,
}

pub(crate) fn completions(
    db: &dyn DefDatabase,
    file_id: FileId,
    pos: TextSize,
) -> Option<Vec<CompletionItem>> {
    let parse = db.parse(file_id);

    let tok = parse.syntax_node().token_at_offset(pos).left_biased()?;
    let source_range = match tok.kind() {
        T![.] => TextRange::empty(pos),
        SyntaxKind::IDENT => tok.text_range(),
        _ => return None,
    };

    let ref_node = tok.parent_ancestors().find_map(|node| {
        match_ast! {
            match node {
                ast::Ref(n) => Some(n),
                _ => None,
            }
        }
    })?;
    let source_map = db.source_map(file_id);
    let expr_id = source_map.node_expr(AstPtr::new(ref_node.syntax()))?;
    let scopes = db.scopes(file_id);
    let scope_id = scopes.scope_by_expr(expr_id)?;

    // TODO: Better sorting.
    let mut items = scopes
        .ancestors(scope_id)
        .filter_map(|scope| scope.as_name_defs())
        .flat_map(|scope| scope.keys())
        .map(|name| CompletionItem {
            label: name.clone(),
            source_range,
            replace: name.clone(),
            kind: CompletionItemKind::Binding,
        })
        .chain(builtin::NAMES.iter().map(|name| CompletionItem {
            label: name.into(),
            source_range,
            replace: name.into(),
            kind: CompletionItemKind::Builtin,
        }))
        .collect::<Vec<_>>();
    items.sort_by(|lhs, rhs| lhs.label.cmp(&rhs.label));
    items.dedup_by(|lhs, rhs| lhs.label == rhs.label);

    Some(items)
}
