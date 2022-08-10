use crate::{LineMap, StateSnapshot, Vfs};
use lsp_types::{
    self as lsp, DiagnosticSeverity, DiagnosticTag, Location, Position, Range,
    TextDocumentPositionParams,
};
use nil::{CompletionItem, CompletionItemKind, Diagnostic, FilePos, FileRange, Severity};
use text_size::TextRange;

pub(crate) fn from_file_pos(
    snap: &StateSnapshot,
    params: &TextDocumentPositionParams,
) -> Option<FilePos> {
    let vfs = snap.vfs.read().unwrap();
    let file = vfs.get_file_for_uri(&params.text_document.uri)?;
    let line_map = vfs.get_line_map(file)?;
    let pos = line_map.pos(params.position.line, params.position.character);
    Some(FilePos::new(file, pos))
}

pub(crate) fn to_location(vfs: &Vfs, frange: FileRange) -> Option<Location> {
    let uri = vfs.get_uri_for_file(frange.file_id)?;
    let line_map = vfs.get_line_map(frange.file_id)?;
    Some(Location::new(uri, to_range(line_map, frange.range)))
}

pub(crate) fn to_range(line_map: &LineMap, range: TextRange) -> Range {
    let (line1, col1) = line_map.line_col(range.start());
    let (line2, col2) = line_map.line_col(range.end());
    Range::new(Position::new(line1, col1), Position::new(line2, col2))
}

pub(crate) fn to_diagnostic(line_map: &LineMap, diag: Diagnostic) -> Option<lsp::Diagnostic> {
    Some(lsp::Diagnostic {
        severity: match diag.severity() {
            Severity::Error => Some(DiagnosticSeverity::ERROR),
            Severity::Warning => Some(DiagnosticSeverity::WARNING),
            Severity::IncompleteSyntax => return None,
        },
        range: to_range(line_map, diag.range),
        code: None,
        code_description: None,
        source: None,
        message: diag.message(),
        related_information: None,
        tags: {
            let mut tags = Vec::new();
            if diag.is_deprecated() {
                tags.push(DiagnosticTag::DEPRECATED);
            }
            if diag.is_unnecessary() {
                tags.push(DiagnosticTag::UNNECESSARY);
            }
            Some(tags)
        },
        data: None,
    })
}

pub(crate) fn to_completion_item(
    line_map: &LineMap,
    item: CompletionItem,
) -> Option<lsp::CompletionItem> {
    let kind = match item.kind {
        CompletionItemKind::Keyword => lsp::CompletionItemKind::KEYWORD,
        CompletionItemKind::Param => lsp::CompletionItemKind::VARIABLE,
        CompletionItemKind::LetBinding => lsp::CompletionItemKind::VARIABLE,
        CompletionItemKind::Field => lsp::CompletionItemKind::FIELD,
        CompletionItemKind::BuiltinConst => lsp::CompletionItemKind::CONSTANT,
        CompletionItemKind::BuiltinFunction => lsp::CompletionItemKind::FUNCTION,
        CompletionItemKind::BuiltinAttrset => lsp::CompletionItemKind::CLASS,
    };
    Some(lsp::CompletionItem {
        label: item.label.into(),
        kind: Some(kind),
        insert_text: None,
        insert_text_format: Some(lsp::InsertTextFormat::PLAIN_TEXT),
        // We don't support indentation yet.
        insert_text_mode: Some(lsp::InsertTextMode::ADJUST_INDENTATION),
        text_edit: Some(lsp::CompletionTextEdit::Edit(lsp::TextEdit {
            range: to_range(line_map, item.source_range),
            new_text: item.replace.into(),
        })),
        // TODO
        ..Default::default()
    })
}
