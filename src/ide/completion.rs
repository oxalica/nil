use crate::builtin::BuiltinKind;
use crate::def::{AstPtr, DefDatabase, NameDefKind};
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
    Param,
    LetBinding,
    Field,
    BuiltinConst,
    BuiltinFunction,
    BuiltinAttrset,
}

impl From<BuiltinKind> for CompletionItemKind {
    fn from(k: BuiltinKind) -> Self {
        match k {
            BuiltinKind::Const => Self::BuiltinConst,
            BuiltinKind::Function => Self::BuiltinFunction,
            BuiltinKind::Attrset => Self::BuiltinAttrset,
        }
    }
}

impl From<NameDefKind> for CompletionItemKind {
    fn from(k: NameDefKind) -> Self {
        match k {
            NameDefKind::LetIn => Self::LetBinding,
            NameDefKind::RecAttrset => Self::Field,
            NameDefKind::Param | NameDefKind::PatField => Self::Param,
        }
    }
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
    let module = db.module(file_id);
    let source_map = db.source_map(file_id);
    let expr_id = source_map.node_expr(AstPtr::new(ref_node.syntax()))?;
    let scopes = db.scopes(file_id);
    let scope_id = scopes.scope_by_expr(expr_id)?;

    // TODO: Better sorting.
    let mut items = scopes
        .ancestors(scope_id)
        .filter_map(|scope| scope.as_name_defs())
        .flatten()
        .map(|(name, &def)| CompletionItem {
            label: name.clone(),
            source_range,
            replace: name.clone(),
            kind: module[def].kind.into(),
        })
        .chain(
            builtin::BUILTINS
                .values()
                .filter(|b| !b.is_hidden)
                .map(|b| CompletionItem {
                    label: b.name.into(),
                    source_range,
                    replace: b.name.into(),
                    kind: b.kind.into(),
                }),
        )
        .collect::<Vec<_>>();
    items.sort_by(|lhs, rhs| lhs.label.cmp(&rhs.label));
    items.dedup_by(|lhs, rhs| lhs.label == rhs.label);

    Some(items)
}
