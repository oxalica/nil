use crate::{convert, StateSnapshot};
use lsp_types::{
    self as lsp, CompletionItem, CompletionOptions, CompletionParams, CompletionResponse,
    GotoDefinitionParams, GotoDefinitionResponse, Location, OneOf, ReferenceParams,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
};
use nil::{CompletionItemKind, InFile};

pub(crate) fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                ..Default::default()
            },
        )),
        definition_provider: Some(OneOf::Left(true)),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".into()]),
            ..Default::default()
        }),
        references_provider: Some(OneOf::Left(true)),
        ..Default::default()
    }
}

pub(crate) fn goto_definition(
    snap: StateSnapshot,
    params: GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let fpos = convert::from_file_pos(&snap, &params.text_document_position_params)?;
    let targets = snap.analysis.goto_definition(fpos).ok()??;
    let vfs = snap.vfs.read().unwrap();
    let targets = targets
        .into_iter()
        .filter_map(|target| {
            convert::to_location(&vfs, InFile::new(target.file_id, target.focus_range))
        })
        .collect::<Vec<_>>();
    Some(GotoDefinitionResponse::Array(targets))
}

pub(crate) fn references(snap: StateSnapshot, params: ReferenceParams) -> Option<Vec<Location>> {
    let fpos = convert::from_file_pos(&snap, &params.text_document_position)?;
    let refs = snap.analysis.references(fpos).ok()??;
    let vfs = snap.vfs.read().unwrap();
    let locs = refs
        .iter()
        .filter_map(|&frange| convert::to_location(&vfs, frange))
        .collect::<Vec<_>>();
    Some(locs)
}

pub(crate) fn completion(
    snap: StateSnapshot,
    params: CompletionParams,
) -> Option<CompletionResponse> {
    let fpos = convert::from_file_pos(&snap, &params.text_document_position)?;
    let items = snap.analysis.completions(fpos).ok()??;
    let vfs = snap.vfs.read().unwrap();
    let line_map = vfs.file_line_map(fpos.file_id)?;
    let items = items
        .into_iter()
        .map(|item| {
            let kind = match item.kind {
                CompletionItemKind::Keyword => lsp::CompletionItemKind::KEYWORD,
                CompletionItemKind::Param => lsp::CompletionItemKind::VARIABLE,
                CompletionItemKind::LetBinding => lsp::CompletionItemKind::VARIABLE,
                CompletionItemKind::Field => lsp::CompletionItemKind::FIELD,
                CompletionItemKind::BuiltinConst => lsp::CompletionItemKind::CONSTANT,
                CompletionItemKind::BuiltinFunction => lsp::CompletionItemKind::FUNCTION,
                CompletionItemKind::BuiltinAttrset => lsp::CompletionItemKind::CLASS,
            };
            CompletionItem {
                label: item.label.into(),
                kind: Some(kind),
                insert_text: None,
                insert_text_format: Some(lsp::InsertTextFormat::PLAIN_TEXT),
                // We don't support indentation yet.
                insert_text_mode: Some(lsp::InsertTextMode::ADJUST_INDENTATION),
                text_edit: Some(lsp::CompletionTextEdit::Edit(lsp::TextEdit {
                    range: convert::to_range(line_map, item.source_range),
                    new_text: item.replace.into(),
                })),
                // TODO
                ..Default::default()
            }
        })
        .collect::<Vec<_>>();
    Some(CompletionResponse::Array(items))
}
