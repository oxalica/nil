use crate::{convert, Result, StateSnapshot};
use ide::FileRange;
use lsp_types::{
    CompletionParams, CompletionResponse, Diagnostic, DocumentFormattingParams,
    DocumentSymbolParams, DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse,
    Hover, HoverParams, Location, Position, PrepareRenameResponse, Range, ReferenceParams,
    RenameParams, SelectionRange, SelectionRangeParams, SemanticTokens, SemanticTokensParams,
    SemanticTokensRangeParams, SemanticTokensRangeResult, SemanticTokensResult,
    TextDocumentPositionParams, TextEdit, Url, WorkspaceEdit,
};
use std::process;
use std::sync::Arc;
use text_size::TextRange;

const MAX_DIAGNOSTICS_CNT: usize = 128;

pub(crate) fn diagnostics(snap: StateSnapshot, uri: &Url) -> Result<Vec<Diagnostic>> {
    let file = snap.vfs().file_for_uri(uri)?;
    let mut diags = snap.analysis.diagnostics(file)?;
    diags.retain(|diag| !snap.config.diagnostics_ignored.contains(diag.code()));
    diags.truncate(MAX_DIAGNOSTICS_CNT);
    Ok(convert::to_diagnostics(&snap.vfs(), file, &diags))
}

pub(crate) fn goto_definition(
    snap: StateSnapshot,
    params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>> {
    let (_, fpos) = convert::from_file_pos(&snap.vfs(), &params.text_document_position_params)?;
    let targets = match snap.analysis.goto_definition(fpos)? {
        None => return Ok(None),
        Some(targets) => targets,
    };
    let vfs = snap.vfs();
    let targets = targets
        .into_iter()
        .map(|target| {
            convert::to_location(&vfs, FileRange::new(target.file_id, target.focus_range))
        })
        .collect::<Vec<_>>();
    Ok(Some(GotoDefinitionResponse::Array(targets)))
}

pub(crate) fn references(
    snap: StateSnapshot,
    params: ReferenceParams,
) -> Result<Option<Vec<Location>>> {
    let (_, fpos) = convert::from_file_pos(&snap.vfs(), &params.text_document_position)?;
    let refs = match snap.analysis.references(fpos)? {
        None => return Ok(None),
        Some(refs) => refs,
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
    let (line_map, fpos) = convert::from_file_pos(&snap.vfs(), &params.text_document_position)?;
    let items = match snap.analysis.completions(fpos)? {
        None => return Ok(None),
        Some(items) => items,
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
    let file = convert::from_file(&snap.vfs(), &params.text_document)?;
    let line_map = snap.vfs().line_map_for_file(file);
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
    let (line_map, fpos) = convert::from_file_pos(&snap.vfs(), &params)?;
    let (range, text) = snap
        .analysis
        .prepare_rename(fpos)?
        .map_err(convert::to_rename_error)?;
    let resp = convert::to_prepare_rename_response(&line_map, range, text.into());
    Ok(Some(resp))
}

pub(crate) fn rename(snap: StateSnapshot, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
    let (_, fpos) = convert::from_file_pos(&snap.vfs(), &params.text_document_position)?;
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
    let file = convert::from_file(&snap.vfs(), &params.text_document)?;
    let line_map = snap.vfs().line_map_for_file(file);
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
    let file = convert::from_file(&snap.vfs(), &params.text_document)?;
    let (line_map, range) = convert::from_range(&snap.vfs(), file, params.range)?;
    let hls = snap.analysis.syntax_highlight(file, Some(range))?;
    let toks = convert::to_semantic_tokens(&line_map, &hls);
    Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
        result_id: None,
        data: toks,
    })))
}

pub(crate) fn hover(snap: StateSnapshot, params: HoverParams) -> Result<Option<Hover>> {
    let (line_map, fpos) =
        convert::from_file_pos(&snap.vfs(), &params.text_document_position_params)?;
    let ret = snap.analysis.hover(fpos)?;
    Ok(ret.map(|hover| convert::to_hover(&line_map, hover)))
}

pub(crate) fn document_symbol(
    snap: StateSnapshot,
    params: DocumentSymbolParams,
) -> Result<Option<DocumentSymbolResponse>> {
    let file = convert::from_file(&snap.vfs(), &params.text_document)?;
    let line_map = snap.vfs().line_map_for_file(file);
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
        if !output.status.success() {
            return Err(format!(
                "Process exited with {}.\n{}",
                output.status,
                String::from_utf8_lossy(&output.stderr)
            )
            .into());
        }
        let stdout = String::from_utf8(output.stdout)?;
        Ok(stdout)
    }

    let cmd = match &snap.config.formatting_command {
        Some(cmd) => cmd,
        None => return Ok(None),
    };

    let (file_content, line_map) = {
        let vfs = snap.vfs();
        let file = convert::from_file(&vfs, &params.text_document)?;
        (vfs.content_for_file(file), vfs.line_map_for_file(file))
    };

    let new_content = run_with_stdin(cmd, <Arc<[u8]>>::from(file_content.clone()))
        .map_err(|err| format!("Failed to run formatter: {err}"))?;

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
