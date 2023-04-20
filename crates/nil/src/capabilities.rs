use crate::semantic_tokens::{SEMANTIC_TOKEN_MODIFIERS, SEMANTIC_TOKEN_TYPES};
use lsp_types::{
    ClientCapabilities, CodeActionProviderCapability, CompletionOptions, DocumentLinkOptions,
    HoverProviderCapability, OneOf, RenameOptions, SelectionRangeProviderCapability,
    SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
    SemanticTokensServerCapabilities, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextDocumentSyncOptions, WorkDoneProgressOptions,
};

macro_rules! test {
    ($lhs:ident $(.$field:ident)*) => {
        Some($lhs)
            $(.and_then(|opt| opt.$field.as_ref()))*
        == Some(&true)
    };
}

pub(crate) fn negotiate_capabilities(
    client_caps: &ClientCapabilities,
) -> (ServerCapabilities, NegotiatedCapabilities) {
    let final_caps = NegotiatedCapabilities {
        client_show_message_request: test!(
            client_caps
                .window
                .show_message
                .message_action_item
                // This is required for knowing which action is performed.
                .additional_properties_support
        ),
        server_initiated_progress: test!(client_caps.window.work_done_progress),
    };

    let server_caps = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::INCREMENTAL),
                will_save: None,
                will_save_wait_until: None,
                save: None,
            },
        )),
        definition_provider: Some(OneOf::Left(true)),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".into(), "?".into()]),
            ..Default::default()
        }),
        references_provider: Some(OneOf::Left(true)),
        selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
        rename_provider: Some(OneOf::Right(RenameOptions {
            prepare_provider: Some(true),
            work_done_progress_options: WorkDoneProgressOptions::default(),
        })),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                work_done_progress_options: WorkDoneProgressOptions::default(),
                legend: SemanticTokensLegend {
                    token_types: SEMANTIC_TOKEN_TYPES.to_vec(),
                    token_modifiers: SEMANTIC_TOKEN_MODIFIERS.to_vec(),
                },
                range: Some(true),
                full: Some(SemanticTokensFullOptions::Delta { delta: Some(false) }),
            },
        )),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        document_formatting_provider: Some(OneOf::Left(true)),
        document_link_provider: Some(DocumentLinkOptions {
            resolve_provider: Some(false),
            work_done_progress_options: WorkDoneProgressOptions::default(),
        }),
        code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
        document_highlight_provider: Some(OneOf::Left(true)),
        ..Default::default()
    };

    (server_caps, final_caps)
}

#[derive(Clone, Debug, Default)]
pub(crate) struct NegotiatedCapabilities {
    pub client_show_message_request: bool,
    pub server_initiated_progress: bool,
}
