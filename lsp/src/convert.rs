use crate::{LineMap, StateSnapshot, Vfs};
use lsp_types::{
    self as lsp, DiagnosticRelatedInformation, DiagnosticSeverity, DiagnosticTag, Location,
    Position, Range, TextDocumentIdentifier, TextDocumentPositionParams,
};
use nil::{CompletionItem, CompletionItemKind, Diagnostic, FileId, FilePos, FileRange, Severity};
use text_size::{TextRange, TextSize};

pub(crate) fn from_file(snap: &StateSnapshot, doc: &TextDocumentIdentifier) -> Option<FileId> {
    let vfs = snap.vfs.read().unwrap();
    vfs.get_file_for_uri(&doc.uri)
}

pub(crate) fn from_pos(snap: &StateSnapshot, file: FileId, pos: Position) -> Option<TextSize> {
    let vfs = snap.vfs.read().unwrap();
    let line_map = vfs.get_line_map(file)?;
    let pos = line_map.pos(pos.line, pos.character);
    Some(pos)
}

pub(crate) fn from_file_pos(
    snap: &StateSnapshot,
    params: &TextDocumentPositionParams,
) -> Option<FilePos> {
    let file = from_file(snap, &params.text_document)?;
    let pos = from_pos(snap, file, params.position)?;
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

pub(crate) fn to_diagnostics(
    vfs: &Vfs,
    file: FileId,
    diags: &[Diagnostic],
) -> Option<Vec<lsp::Diagnostic>> {
    let line_map = vfs.get_line_map(file)?;
    let mut ret = Vec::with_capacity(diags.len() * 2);
    for diag in diags {
        let primary_diag = lsp::Diagnostic {
            severity: match diag.severity() {
                Severity::Error => Some(DiagnosticSeverity::ERROR),
                Severity::Warning => Some(DiagnosticSeverity::WARNING),
                Severity::IncompleteSyntax => continue,
            },
            range: to_range(line_map, diag.range),
            code: None,
            code_description: None,
            source: None,
            message: diag.message(),
            related_information: {
                Some(
                    diag.notes
                        .iter()
                        .filter_map(|(frange, msg)| {
                            Some(DiagnosticRelatedInformation {
                                location: to_location(vfs, *frange)?,
                                message: msg.to_owned(),
                            })
                        })
                        .collect(),
                )
            },
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
        };

        // Hoist related information to top-level Hints.
        for (frange, msg) in &diag.notes {
            // We cannot handle cross-file diagnostics here.
            if frange.file_id != file {
                continue;
            }

            ret.push(lsp::Diagnostic {
                severity: Some(DiagnosticSeverity::HINT),
                range: to_range(line_map, frange.range),
                code: primary_diag.code.clone(),
                code_description: primary_diag.code_description.clone(),
                source: primary_diag.source.clone(),
                message: msg.into(),
                related_information: Some(vec![DiagnosticRelatedInformation {
                    location: to_location(vfs, FileRange::new(file, diag.range))
                        .expect("Checked by get_line_map"),
                    message: "original diagnostic".into(),
                }]),
                tags: None,
                data: None,
            });
        }

        ret.push(primary_diag);
    }
    Some(ret)
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
