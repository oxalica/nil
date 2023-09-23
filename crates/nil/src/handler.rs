use crate::{convert, StateSnapshot};
use anyhow::{ensure, Context, Result};
use async_lsp::{ErrorCode, ResponseError};
use ide::{FileRange, GotoDefinitionResult};
use lsp_types::{
    CodeActionParams, CodeActionResponse, CompletionParams, CompletionResponse,
    DocumentFormattingParams, DocumentHighlight, DocumentHighlightParams, DocumentLink,
    DocumentLinkParams, DocumentSymbolParams, DocumentSymbolResponse, GotoDefinitionParams,
    GotoDefinitionResponse, Hover, HoverParams, Location, Position, PrepareRenameResponse, Range,
    ReferenceParams, RenameParams, SelectionRange, SelectionRangeParams, SemanticTokens,
    SemanticTokensParams, SemanticTokensRangeParams, SemanticTokensRangeResult,
    SemanticTokensResult, TextDocumentPositionParams, TextEdit, Url, WorkspaceEdit,
};
use nix_interop::DEFAULT_IMPORT_FILE;
use std::process;
use std::sync::Arc;
use text_size::TextRange;

pub(crate) fn goto_definition(
    snap: StateSnapshot,
    params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>> {
    let (fpos, _) = convert::from_file_pos(&snap.vfs(), &params.text_document_position_params)?;
    let ret = snap.analysis.goto_definition(fpos)?;
    let vfs = snap.vfs();
    let targets = match ret {
        None => return Ok(None),
        Some(GotoDefinitionResult::Path(vpath)) => {
            let Some(path) = vpath.as_path() else {
                return Ok(None);
            };
            let default_child = path.join(DEFAULT_IMPORT_FILE);
            let target_path = if path.is_file() {
                path
            } else if default_child.is_file() {
                &default_child
            } else {
                return Ok(None);
            };
            vec![Location {
                uri: Url::from_file_path(target_path).unwrap(),
                range: Range::default(),
            }]
        }
        Some(GotoDefinitionResult::Targets(targets)) => targets
            .into_iter()
            .map(|target| {
                convert::to_location(&vfs, FileRange::new(target.file_id, target.focus_range))
            })
            .collect(),
    };
    Ok(Some(GotoDefinitionResponse::Array(targets)))
}

pub(crate) fn references(
    snap: StateSnapshot,
    params: ReferenceParams,
) -> Result<Option<Vec<Location>>> {
    let (fpos, _) = convert::from_file_pos(&snap.vfs(), &params.text_document_position)?;
    let Some(refs) = snap.analysis.references(fpos)? else {
        return Ok(None);
    };
    let vfs = snap.vfs();
    let locs = refs
        .into_iter()
        .map(|frange| convert::to_location(&vfs, frange))
        .collect::<Vec<_>>();
    Ok(Some(locs))
}

pub(crate) fn completion(
    snap: StateSnapshot,
    params: CompletionParams,
) -> Result<Option<CompletionResponse>> {
    let (fpos, line_map) = convert::from_file_pos(&snap.vfs(), &params.text_document_position)?;
    let trigger_char = params
        .context
        .and_then(|ctx| ctx.trigger_character?.chars().next());
    let Some(items) = snap.analysis.completions(fpos, trigger_char)? else {
        return Ok(None);
    };
    let items = items
        .into_iter()
        .map(|item| convert::to_completion_item(&line_map, item))
        .collect::<Vec<_>>();
    Ok(Some(CompletionResponse::Array(items)))
}

pub(crate) fn selection_range(
    snap: StateSnapshot,
    params: SelectionRangeParams,
) -> Result<Option<Vec<SelectionRange>>> {
    let (file, line_map) = convert::from_file(&snap.vfs(), &params.text_document)?;
    let ret = params
        .positions
        .iter()
        .map(|&pos| {
            let pos = convert::from_pos(&line_map, pos)?;
            let frange = FileRange::new(file, TextRange::empty(pos));

            let mut ranges = snap.analysis.expand_selection(frange)?.unwrap_or_default();
            if ranges.is_empty() {
                ranges.push(TextRange::empty(pos));
            }

            let mut ret = SelectionRange {
                range: convert::to_range(&line_map, *ranges.last().unwrap()),
                parent: None,
            };
            for &r in ranges.iter().rev().skip(1) {
                ret = SelectionRange {
                    range: convert::to_range(&line_map, r),
                    parent: Some(ret.into()),
                };
            }

            Ok(ret)
        })
        .collect::<Result<Vec<_>>>();
    ret.map(Some)
}

pub(crate) fn prepare_rename(
    snap: StateSnapshot,
    params: TextDocumentPositionParams,
) -> Result<Option<PrepareRenameResponse>> {
    let (fpos, line_map) = convert::from_file_pos(&snap.vfs(), &params)?;
    let (range, text) = snap
        .analysis
        .prepare_rename(fpos)?
        .map_err(convert::to_rename_error)?;
    let resp = convert::to_prepare_rename_response(&line_map, range, text.into());
    Ok(Some(resp))
}

pub(crate) fn rename(snap: StateSnapshot, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
    let (fpos, _) = convert::from_file_pos(&snap.vfs(), &params.text_document_position)?;
    let ws_edit = snap
        .analysis
        .rename(fpos, &params.new_name)?
        .map_err(convert::to_rename_error)?;
    let resp = convert::to_workspace_edit(&snap.vfs(), ws_edit);
    Ok(Some(resp))
}

pub(crate) fn semantic_token_full(
    snap: StateSnapshot,
    params: SemanticTokensParams,
) -> Result<Option<SemanticTokensResult>> {
    let (file, line_map) = convert::from_file(&snap.vfs(), &params.text_document)?;
    let hls = snap.analysis.syntax_highlight(file, None)?;
    let toks = convert::to_semantic_tokens(&line_map, &hls);
    Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data: toks,
    })))
}

pub(crate) fn semantic_token_range(
    snap: StateSnapshot,
    params: SemanticTokensRangeParams,
) -> Result<Option<SemanticTokensRangeResult>> {
    let (file, range, line_map) = {
        let vfs = snap.vfs();
        let (file, line_map) = convert::from_file(&vfs, &params.text_document)?;
        let (_, range) = convert::from_range(&vfs, file, params.range)?;
        (file, range, line_map)
    };
    let hls = snap.analysis.syntax_highlight(file, Some(range))?;
    let toks = convert::to_semantic_tokens(&line_map, &hls);
    Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
        result_id: None,
        data: toks,
    })))
}

pub(crate) fn hover(snap: StateSnapshot, params: HoverParams) -> Result<Option<Hover>> {
    let (fpos, line_map) =
        convert::from_file_pos(&snap.vfs(), &params.text_document_position_params)?;
    let ret = snap.analysis.hover(fpos)?;
    Ok(ret.map(|hover| convert::to_hover(&line_map, hover)))
}

pub(crate) fn document_symbol(
    snap: StateSnapshot,
    params: DocumentSymbolParams,
) -> Result<Option<DocumentSymbolResponse>> {
    let (file, line_map) = convert::from_file(&snap.vfs(), &params.text_document)?;
    let syms = snap.analysis.symbol_hierarchy(file)?;
    let syms = convert::to_document_symbols(&line_map, syms);
    Ok(Some(DocumentSymbolResponse::Nested(syms)))
}

// FIXME: This is sync now.
pub(crate) fn formatting(
    snap: StateSnapshot,
    params: DocumentFormattingParams,
) -> Result<Option<Vec<TextEdit>>> {
    fn run_with_stdin(
        cmd: &[String],
        stdin_data: impl AsRef<[u8]> + Send + 'static,
    ) -> Result<String> {
        let mut child = process::Command::new(&cmd[0])
            .args(&cmd[1..])
            .stdin(process::Stdio::piped())
            .stdout(process::Stdio::piped())
            .stderr(process::Stdio::piped())
            .spawn()?;
        let mut stdin = child.stdin.take().unwrap();
        std::thread::spawn(move || {
            let _ = std::io::copy(&mut stdin_data.as_ref(), &mut stdin);
        });
        let output = child.wait_with_output()?;
        ensure!(
            output.status.success(),
            "Formatter exited with {}, stderr: {}",
            output.status,
            String::from_utf8_lossy(&output.stderr),
        );
        let stdout = String::from_utf8(output.stdout)?;
        Ok(stdout)
    }

    let cmd =
        snap.config.formatting_command.as_ref().context(
            "No formatter configured. Set the nil.formatting.command LSP server setting.",
        )?;

    let (file_content, line_map) = {
        let vfs = snap.vfs();
        let (file, line_map) = convert::from_file(&vfs, &params.text_document)?;
        (vfs.content_for_file(file), line_map)
    };

    let new_content = run_with_stdin(cmd, <Arc<[u8]>>::from(file_content.clone()))
        .with_context(|| format!("Failed to run formatter {cmd:?}"))?;

    if new_content == *file_content {
        return Ok(None);
    }

    // Replace the whole file.
    let last_line = line_map.last_line();
    Ok(Some(vec![TextEdit {
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: last_line,
                character: line_map.end_col_for_line(last_line),
            },
        },
        new_text: new_content,
    }]))
}

pub(crate) fn document_links(
    snap: StateSnapshot,
    params: DocumentLinkParams,
) -> Result<Option<Vec<DocumentLink>>> {
    let (file, line_map) = convert::from_file(&snap.vfs(), &params.text_document)?;
    let links = snap.analysis.links(file)?;
    let links = links
        .into_iter()
        .filter_map(|link| convert::to_document_link(&line_map, &params.text_document.uri, link))
        .collect::<Vec<_>>();
    Ok(Some(links))
}

pub(crate) fn document_link_resolve(
    snap: StateSnapshot,
    params: DocumentLink,
) -> Result<DocumentLink> {
    let (uri, frange, line_map) = convert::from_document_link(&snap.vfs(), &params)?;
    snap.analysis
        .link_resolve(frange)?
        .and_then(|link| convert::to_document_link(&line_map, &uri, link))
        .ok_or_else(|| {
            anyhow::Error::new(ResponseError::new(
                ErrorCode::INVALID_PARAMS,
                "invalid unresolved link",
            ))
        })
}

pub(crate) fn code_action(
    snap: StateSnapshot,
    params: CodeActionParams,
) -> Result<Option<CodeActionResponse>> {
    let (file_id, _) = convert::from_file(&snap.vfs(), &params.text_document)?;
    let (_, range) = convert::from_range(&snap.vfs(), file_id, params.range)?;
    let assists = snap.analysis.assists(FileRange { file_id, range })?;
    let vfs = snap.vfs();
    let actions = assists
        .into_iter()
        .map(|assist| convert::to_code_action(&vfs, assist))
        .collect();
    Ok(Some(actions))
}

pub(crate) fn document_highlight(
    snap: StateSnapshot,
    params: DocumentHighlightParams,
) -> Result<Option<Vec<DocumentHighlight>>> {
    let (fpos, line_map) =
        convert::from_file_pos(&snap.vfs(), &params.text_document_position_params)?;
    let ret = snap.analysis.highlight_related(fpos)?;
    let ret = convert::to_document_highlight(&line_map, &ret);
    Ok(Some(ret))
}

pub(crate) fn parent_module(
    snap: StateSnapshot,
    params: TextDocumentPositionParams,
) -> Result<Option<GotoDefinitionResponse>> {
    let (file, _) = convert::from_file(&snap.vfs(), &params.text_document)?;
    let files = snap.analysis.file_referrers(file)?;
    let vfs = snap.vfs();
    let locs = files
        .into_iter()
        .map(|file| convert::to_location(&vfs, FileRange::new(file, TextRange::default())))
        .collect();
    Ok(Some(GotoDefinitionResponse::Array(locs)))
}
