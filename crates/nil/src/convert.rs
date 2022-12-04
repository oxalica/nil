use crate::{semantic_tokens, LineMap, LspError, Result, Vfs};
use ide::{
    Assist, AssistKind, CompletionItem, CompletionItemKind, Diagnostic, FileId, FilePos, FileRange,
    HlRange, HlRelated, HoverResult, NameKind, Severity, SymbolTree, TextEdit, WorkspaceEdit,
};
use lsp_server::ErrorCode;
use lsp_types::{
    self as lsp, CodeAction, CodeActionKind, CodeActionOrCommand, DiagnosticRelatedInformation,
    DiagnosticSeverity, DiagnosticTag, DocumentHighlight, DocumentHighlightKind, DocumentSymbol,
    Documentation, Hover, Location, MarkupContent, MarkupKind, NumberOrString, Position,
    PrepareRenameResponse, Range, SemanticToken, SymbolKind, TextDocumentIdentifier,
    TextDocumentPositionParams,
};
use std::sync::Arc;
use text_size::{TextRange, TextSize};

pub(crate) fn from_file(vfs: &Vfs, doc: &TextDocumentIdentifier) -> Result<(FileId, Arc<LineMap>)> {
    let file = vfs.file_for_uri(&doc.uri)?;
    let line_map = vfs.line_map_for_file(file);
    Ok((file, line_map))
}

pub(crate) fn from_pos(line_map: &LineMap, pos: Position) -> Result<TextSize> {
    Ok(line_map.pos_for_line_col(pos.line, pos.character))
}

pub(crate) fn from_file_pos(
    vfs: &Vfs,
    params: &TextDocumentPositionParams,
) -> Result<(FilePos, Arc<LineMap>)> {
    let (file, line_map) = from_file(vfs, &params.text_document)?;
    let pos = from_pos(&line_map, params.position)?;
    Ok((FilePos::new(file, pos), line_map))
}

pub(crate) fn from_range(
    vfs: &Vfs,
    file: FileId,
    range: Range,
) -> Result<(Arc<LineMap>, TextRange)> {
    let line_map = vfs.line_map_for_file(file);
    let start = from_pos(&line_map, range.start)?;
    let end = from_pos(&line_map, range.end)?;
    Ok((line_map, TextRange::new(start, end)))
}

pub(crate) fn to_location(vfs: &Vfs, frange: FileRange) -> Location {
    let uri = vfs.uri_for_file(frange.file_id);
    let line_map = vfs.line_map_for_file(frange.file_id);
    Location::new(uri, to_range(&line_map, frange.range))
}

pub(crate) fn to_range(line_map: &LineMap, range: TextRange) -> Range {
    let (line1, col1) = line_map.line_col_for_pos(range.start());
    let (line2, col2) = line_map.line_col_for_pos(range.end());
    Range::new(Position::new(line1, col1), Position::new(line2, col2))
}

pub(crate) fn to_diagnostics(
    vfs: &Vfs,
    file: FileId,
    diags: &[Diagnostic],
) -> Vec<lsp::Diagnostic> {
    let line_map = vfs.line_map_for_file(file);
    let mut ret = Vec::with_capacity(diags.len() * 2);
    for diag in diags {
        let primary_diag = lsp::Diagnostic {
            severity: match diag.severity() {
                Severity::Error | Severity::IncompleteSyntax => Some(DiagnosticSeverity::ERROR),
                Severity::Warning => Some(DiagnosticSeverity::WARNING),
            },
            range: to_range(&line_map, diag.range),
            code: Some(NumberOrString::String(diag.code().into())),
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
        detail: item.brief,
        documentation: item.doc.map(|doc| {
            Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: doc,
            })
        }),

        // TODO
        deprecated: None,
        preselect: None,
        sort_text: None,
        filter_text: None,
        additional_text_edits: None,
        command: None,
        commit_characters: None,
        data: None,
        tags: None,
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
                    let line_map = vfs.line_map_for_file(file);
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
    // We must not exceed the last line.
    let last_line = line_map.last_line();

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
            let mut end = line_map.end_col_for_line(line);
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

pub(crate) fn to_hover(line_map: &LineMap, hover: HoverResult) -> Hover {
    Hover {
        range: Some(to_range(line_map, hover.range)),
        contents: lsp::HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: hover.markup,
        }),
    }
}

pub(crate) fn to_document_symbols(
    line_map: &LineMap,
    syms: Vec<SymbolTree>,
) -> Vec<DocumentSymbol> {
    syms.into_iter()
        .map(|sym| to_document_symbol(line_map, sym))
        .collect()
}

fn to_document_symbol(line_map: &LineMap, sym: SymbolTree) -> DocumentSymbol {
    #[allow(deprecated)]
    DocumentSymbol {
        name: sym.name.into(),
        detail: None,
        kind: match sym.kind {
            NameKind::PlainAttrset | NameKind::RecAttrset => SymbolKind::FIELD,
            NameKind::LetIn | NameKind::Param | NameKind::PatField => SymbolKind::VARIABLE,
        },
        tags: None,
        deprecated: None,
        range: to_range(line_map, sym.full_range),
        selection_range: to_range(line_map, sym.focus_range),
        children: Some(to_document_symbols(line_map, sym.children)),
    }
}

pub(crate) fn to_code_action(vfs: &Vfs, assist: Assist) -> CodeActionOrCommand {
    CodeActionOrCommand::CodeAction(CodeAction {
        title: assist.label,
        kind: Some(match assist.kind {
            AssistKind::QuickFix => CodeActionKind::QUICKFIX,
            AssistKind::RefactorRewrite => CodeActionKind::REFACTOR_REWRITE,
        }),
        diagnostics: None,
        edit: Some(to_workspace_edit(vfs, assist.edits)),
        command: None,
        is_preferred: None,
        disabled: None,
        data: None,
    })
}

pub(crate) fn to_document_highlight(
    line_map: &LineMap,
    hls: &[HlRelated],
) -> Vec<DocumentHighlight> {
    hls.iter()
        .map(|hl| DocumentHighlight {
            range: to_range(line_map, hl.range),
            kind: Some(if hl.is_definition {
                DocumentHighlightKind::WRITE
            } else {
                DocumentHighlightKind::READ
            }),
        })
        .collect()
}
