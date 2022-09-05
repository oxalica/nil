use crate::{convert, StateSnapshot};
use lsp_types::{
    CompletionOptions, CompletionParams, CompletionResponse, GotoDefinitionParams,
    GotoDefinitionResponse, Location, OneOf, ReferenceParams, SelectionRange, SelectionRangeParams,
    SelectionRangeProviderCapability, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextDocumentSyncOptions,
};
use nil::FileRange;
use text_size::TextRange;

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
        selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
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
            convert::to_location(&vfs, FileRange::new(target.file_id, target.focus_range))
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
    let line_map = vfs.get_line_map(fpos.file_id)?;
    let items = items
        .into_iter()
        .filter_map(|item| convert::to_completion_item(line_map, item))
        .collect::<Vec<_>>();
    Some(CompletionResponse::Array(items))
}

pub(crate) fn selection_range(
    snap: StateSnapshot,
    params: SelectionRangeParams,
) -> Option<Vec<SelectionRange>> {
    let file = convert::from_file(&snap, &params.text_document)?;
    params
        .positions
        .iter()
        .map(|&pos| {
            let pos = convert::from_pos(&snap, file, pos)?;
            let frange = FileRange::new(file, TextRange::empty(pos));

            let mut ranges = snap.analysis.expand_selection(frange).ok()??;
            if ranges.is_empty() {
                ranges.push(TextRange::empty(pos));
            }

            // FIXME: Use Arc for LineMap.
            let vfs = snap.vfs.read().unwrap();
            let line_map = vfs.get_line_map(file)?;
            let mut ret = SelectionRange {
                range: convert::to_range(line_map, *ranges.last().unwrap()),
                parent: None,
            };
            for &r in ranges.iter().rev().skip(1) {
                ret = SelectionRange {
                    range: convert::to_range(line_map, r),
                    parent: Some(ret.into()),
                };
            }

            Some(ret)
        })
        .collect()
}
