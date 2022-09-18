use std::sync::Arc;

use crate::{semantic_tokens, LineMap, LspError, Result, Vfs};
use ide::{
    CompletionItem, CompletionItemKind, Diagnostic, FileId, FilePos, FileRange, HlRange, Severity,
    TextEdit, WorkspaceEdit,
};
use lsp::SemanticToken;
use lsp_server::ErrorCode;
use lsp_types::{
    self as lsp, DiagnosticRelatedInformation, DiagnosticSeverity, DiagnosticTag, Location,
    Position, PrepareRenameResponse, Range, TextDocumentIdentifier, TextDocumentPositionParams,
};
use text_size::{TextRange, TextSize};

pub(crate) fn from_file(vfs: &Vfs, doc: &TextDocumentIdentifier) -> Result<FileId> {
    vfs.get_file_for_uri(&doc.uri)
}

pub(crate) fn from_pos(line_map: &LineMap, pos: Position) -> Result<TextSize> {
    Ok(line_map.pos(pos.line, pos.character))
}

pub(crate) fn from_file_pos(
    vfs: &Vfs,
    params: &TextDocumentPositionParams,
) -> Result<(Arc<LineMap>, FilePos)> {
    let file = from_file(vfs, &params.text_document)?;
    let line_map = vfs.file_line_map(file);
    let pos = from_pos(&line_map, params.position)?;
    Ok((line_map, FilePos::new(file, pos)))
}

pub(crate) fn from_range(
    vfs: &Vfs,
    file: FileId,
    range: Range,
) -> Result<(Arc<LineMap>, TextRange)> {
    let line_map = vfs.file_line_map(file);
    let start = from_pos(&line_map, range.start)?;
    let end = from_pos(&line_map, range.end)?;
    Ok((line_map, TextRange::new(start, end)))
}

pub(crate) fn to_location(vfs: &Vfs, frange: FileRange) -> Location {
    let uri = vfs.uri_for_file(frange.file_id);
    let line_map = vfs.file_line_map(frange.file_id);
    Location::new(uri, to_range(&line_map, frange.range))
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
) -> Vec<lsp::Diagnostic> {
    let line_map = vfs.file_line_map(file);
    let mut ret = Vec::with_capacity(diags.len() * 2);
    for diag in diags {
        let primary_diag = lsp::Diagnostic {
            severity: match diag.severity() {
                Severity::Error => Some(DiagnosticSeverity::ERROR),
                Severity::Warning => Some(DiagnosticSeverity::WARNING),
                Severity::IncompleteSyntax => continue,
            },
            range: to_range(&line_map, diag.range),
            code: None,
            code_description: None,
            source: None,
            message: diag.message(),
            related_information: {
                Some(
                    diag.notes
                        .iter()
                        .map(|(frange, msg)| DiagnosticRelatedInformation {
                            location: to_location(vfs, *frange),
                            message: msg.to_owned(),
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
                range: to_range(&line_map, frange.range),
                code: primary_diag.code.clone(),
                code_description: primary_diag.code_description.clone(),
                source: primary_diag.source.clone(),
                message: msg.into(),
                related_information: Some(vec![DiagnosticRelatedInformation {
                    location: to_location(vfs, FileRange::new(file, diag.range)),
                    message: "original diagnostic".into(),
                }]),
                tags: None,
                data: None,
            });
        }

        ret.push(primary_diag);
    }

    ret
}

pub(crate) fn to_completion_item(line_map: &LineMap, item: CompletionItem) -> lsp::CompletionItem {
    let kind = match item.kind {
        CompletionItemKind::Keyword => lsp::CompletionItemKind::KEYWORD,
        CompletionItemKind::Param => lsp::CompletionItemKind::VARIABLE,
        CompletionItemKind::LetBinding => lsp::CompletionItemKind::VARIABLE,
        CompletionItemKind::Field => lsp::CompletionItemKind::FIELD,
        CompletionItemKind::BuiltinConst => lsp::CompletionItemKind::CONSTANT,
        CompletionItemKind::BuiltinFunction => lsp::CompletionItemKind::FUNCTION,
        CompletionItemKind::BuiltinAttrset => lsp::CompletionItemKind::CLASS,
    };
    lsp::CompletionItem {
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
    }
}

pub(crate) fn to_rename_error(message: String) -> LspError {
    LspError {
        code: ErrorCode::InvalidRequest,
        message,
    }
}

pub(crate) fn to_prepare_rename_response(
    line_map: &LineMap,
    range: TextRange,
    text: String,
) -> PrepareRenameResponse {
    let range = to_range(line_map, range);
    PrepareRenameResponse::RangeWithPlaceholder {
        range,
        placeholder: text,
    }
}

pub(crate) fn to_workspace_edit(vfs: &Vfs, ws_edit: WorkspaceEdit) -> lsp::WorkspaceEdit {
    let content_edits = ws_edit
        .content_edits
        .into_iter()
        .map(|(file, edits)| {
            let uri = vfs.uri_for_file(file);
            let edits = edits
                .into_iter()
                .map(|edit| {
                    let line_map = vfs.file_line_map(file);
                    to_text_edit(&line_map, edit)
                })
                .collect();
            (uri, edits)
        })
        .collect();
    lsp::WorkspaceEdit {
        changes: Some(content_edits),
        document_changes: None,
        change_annotations: None,
    }
}

pub(crate) fn to_text_edit(line_map: &LineMap, edit: TextEdit) -> lsp::TextEdit {
    lsp::TextEdit {
        range: to_range(line_map, edit.delete),
        new_text: edit.insert.into(),
    }
}

pub(crate) fn to_semantic_tokens(line_map: &LineMap, hls: &[HlRange]) -> Vec<SemanticToken> {
    // We must now exceed the last line.
    let line_count = line_map.line_count();
    if line_count == 0 {
        return Vec::new();
    }
    let last_line = line_count - 1;

    let mut toks = Vec::with_capacity(hls.len());
    let (mut prev_line, mut prev_start) = (0, 0);
    for hl in hls {
        let (ty_idx, mod_set) = semantic_tokens::to_semantic_type_and_modifiers(hl.tag);
        let range = to_range(line_map, hl.range);
        for line in range.start.line..=range.end.line.min(last_line) {
            // N.B. For relative encoding, column offset is relative to
            // the previous token *in the same line*.
            if line != prev_line {
                prev_start = 0;
            }

            let mut start = 0;
            let mut end = line_map.line_end_col(line);
            if line == range.start.line {
                start = start.max(range.start.character);
            }
            if line == range.end.line {
                end = end.min(range.end.character);
            }

            // Don't emit empty token.
            // N.B. When there is a newline at EOF and a unterminated string,
            // the newline itself is in highlighted range.
            // We must ignore this case since newlines cannot be highlighted in LSP.
            if start == end {
                continue;
            }

            toks.push(SemanticToken {
                delta_line: line - prev_line,
                delta_start: start - prev_start,
                length: end - start,
                token_type: ty_idx as u32,
                token_modifiers_bitset: mod_set.0,
            });

            (prev_line, prev_start) = (line, start);
        }
    }

    toks
}
