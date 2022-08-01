use crate::vfs::{Vfs, VfsPath};
use crossbeam_channel::Sender;
use lsp_types::{self as lsp, notification as notif, request as req, OneOf, Url};
use nil::{Analysis, AnalysisHost, Change, CompletionItemKind, FilePos, InFile};
use std::sync::{Arc, RwLock};
use text_size::TextRange;

pub fn server_capabilities() -> lsp::ServerCapabilities {
    lsp::ServerCapabilities {
        text_document_sync: Some(lsp::TextDocumentSyncCapability::Options(
            lsp::TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(lsp::TextDocumentSyncKind::FULL),
                ..Default::default()
            },
        )),
        definition_provider: Some(OneOf::Left(true)),
        completion_provider: Some(lsp::CompletionOptions {
            trigger_characters: Some(vec![".".into()]),
            ..Default::default()
        }),
        ..Default::default()
    }
}

pub struct State {
    host: AnalysisHost,
    vfs: Arc<RwLock<Vfs>>,
    responder: Sender<lsp_server::Message>,
}

impl State {
    pub fn new(responder: Sender<lsp_server::Message>) -> Self {
        Self {
            host: Default::default(),
            vfs: Default::default(),
            responder,
        }
    }

    pub fn snapshot(&self) -> StateSnapshot {
        StateSnapshot {
            analysis: self.host.snapshot(),
            vfs: Arc::clone(&self.vfs),
        }
    }

    pub fn apply_change(&mut self, change: Change) {
        if !change.is_empty() {
            log::debug!("Files changed: {:?}", change);
            self.host.apply_change(change);
        }
    }

    pub fn set_vfs_file_content(&mut self, uri: &Url, text: Option<String>) {
        if let Ok(path) = VfsPath::try_from(uri) {
            self.apply_change({
                let mut vfs = self.vfs.write().unwrap();
                vfs.set_file_content(path, text);
                vfs.take_change()
            });
        }
    }

    pub fn dispatch_request(&mut self, req: lsp_server::Request) {
        RequestDispatcher(self, Some(req))
            .on::<req::GotoDefinition>(|st, params| {
                match (|| -> Option<_> {
                    let pos = get_file_pos(
                        &st.vfs.read().unwrap(),
                        &params.text_document_position_params,
                    )?;
                    let targets = st.analysis.goto_definition(pos).ok()??;
                    let vfs = st.vfs.read().unwrap();
                    let targets = targets
                        .into_iter()
                        .filter_map(|target| {
                            to_lsp_location(&vfs, InFile::new(target.file_id, target.focus_range))
                        })
                        .collect::<Vec<_>>();
                    Some(targets)
                })() {
                    Some(locs) => Some(lsp::GotoDefinitionResponse::Array(locs)),
                    None => Some(lsp::GotoDefinitionResponse::Array(Vec::new())),
                }
            })
            .on::<req::Completion>(|st, params| {
                let pos = get_file_pos(&st.vfs.read().unwrap(), &params.text_document_position)?;
                let items = st.analysis.completions(pos).ok()??;
                let vfs = st.vfs.read().unwrap();
                let items = items
                    .into_iter()
                    .filter_map(|item| {
                        let kind = match item.kind {
                            // FIXME: More specific?
                            CompletionItemKind::Builtin => lsp::CompletionItemKind::KEYWORD,
                            CompletionItemKind::Binding => lsp::CompletionItemKind::VARIABLE,
                        };
                        Some(lsp::CompletionItem {
                            label: item.label.into(),
                            kind: Some(kind),
                            insert_text: None,
                            insert_text_format: Some(lsp::InsertTextFormat::PLAIN_TEXT),
                            // We don't support indentation yet.
                            insert_text_mode: Some(lsp::InsertTextMode::ADJUST_INDENTATION),
                            text_edit: Some(lsp::CompletionTextEdit::Edit(lsp::TextEdit {
                                range: to_lsp_range(&vfs, pos.map(|_| item.source_range))?,
                                new_text: item.replace.into(),
                            })),
                            // TODO
                            ..Default::default()
                        })
                    })
                    .collect::<Vec<_>>();
                Some(lsp::CompletionResponse::Array(items))
            })
            .finish();
    }

    pub fn dispatch_notification(&mut self, notif: lsp_server::Notification) {
        NotificationDispatcher(self, Some(notif))
            .on_mut::<notif::DidOpenTextDocument>(|st, params| {
                st.set_vfs_file_content(&params.text_document.uri, Some(params.text_document.text));
            })
            .on_mut::<notif::DidCloseTextDocument>(|st, params| {
                st.set_vfs_file_content(&params.text_document.uri, None);
            })
            .on_mut::<notif::DidChangeTextDocument>(|st, params| {
                if let Some(chg) = params.content_changes.into_iter().next() {
                    st.set_vfs_file_content(&params.text_document.uri, Some(chg.text));
                }
            })
            .finish();
    }
}

#[must_use = "RequestDispatcher::finish not called"]
struct RequestDispatcher<'s>(&'s mut State, Option<lsp_server::Request>);

impl<'s> RequestDispatcher<'s> {
    // TODO: Error handling?
    fn on<R: req::Request>(mut self, f: fn(StateSnapshot, R::Params) -> R::Result) -> Self {
        if matches!(&self.1, Some(notif) if notif.method == R::METHOD) {
            let req = self.1.take().unwrap();
            let params = serde_json::from_value::<R::Params>(req.params).unwrap();
            let resp = f(self.0.snapshot(), params);
            let resp = lsp_server::Response::new_ok(req.id, serde_json::to_value(resp).unwrap());
            self.0.responder.send(resp.into()).unwrap();
        }
        self
    }

    fn finish(self) {
        if let Some(req) = self.1 {
            let resp = lsp_server::Response::new_err(
                req.id,
                lsp_server::ErrorCode::MethodNotFound as _,
                String::new(),
            );
            self.0.responder.send(resp.into()).unwrap();
        }
    }
}

#[must_use = "NotificationDispatcher::finish not called"]
struct NotificationDispatcher<'s>(&'s mut State, Option<lsp_server::Notification>);

impl<'s> NotificationDispatcher<'s> {
    fn on_mut<N: notif::Notification>(mut self, f: fn(&mut State, N::Params)) -> Self {
        if matches!(&self.1, Some(notif) if notif.method == N::METHOD) {
            let params =
                serde_json::from_value::<N::Params>(self.1.take().unwrap().params).unwrap();
            f(self.0, params);
        }
        self
    }

    fn finish(self) {
        let _ = self;
    }
}

#[derive(Debug)]
pub struct StateSnapshot {
    analysis: Analysis,
    vfs: Arc<RwLock<Vfs>>,
}

fn get_file_pos(vfs: &Vfs, params: &lsp::TextDocumentPositionParams) -> Option<FilePos> {
    let path = VfsPath::try_from(&params.text_document.uri).ok()?;
    vfs.get_file_pos(&path, params.position.line, params.position.character)
}

fn to_lsp_location(vfs: &Vfs, pos: InFile<TextRange>) -> Option<lsp::Location> {
    let url = vfs.file_path(pos.file_id)?.try_into().ok()?;
    Some(lsp::Location::new(url, to_lsp_range(vfs, pos)?))
}

fn to_lsp_range(vfs: &Vfs, pos: InFile<TextRange>) -> Option<lsp::Range> {
    let (_, line1, col1) = vfs.get_file_line_col(pos.map(|range| range.start()))?;
    let (_, line2, col2) = vfs.get_file_line_col(pos.map(|range| range.end()))?;
    Some(lsp::Range::new(
        lsp::Position::new(line1, col1),
        lsp::Position::new(line2, col2),
    ))
}
