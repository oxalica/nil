use crate::{convert, handler, Vfs, VfsPath};
use anyhow::{bail, Result};
use crossbeam_channel::{Receiver, Sender};
use lsp_server::{ErrorCode, Message, Notification, Request, Response};
use lsp_types::notification::Notification as _;
use lsp_types::{notification as notif, request as req, PublishDiagnosticsParams, Url};
use nil::{Analysis, AnalysisHost};
use std::sync::{Arc, RwLock};

const MAX_DIAGNOSTICS_CNT: usize = 128;

pub struct State {
    host: AnalysisHost,
    vfs: Arc<RwLock<Vfs>>,
    sender: Sender<Message>,
    is_shutdown: bool,
}

impl State {
    pub fn new(responder: Sender<Message>) -> Self {
        Self {
            host: Default::default(),
            vfs: Default::default(),
            sender: responder,
            is_shutdown: false,
        }
    }

    pub fn run(&mut self, lsp_receiver: Receiver<Message>) -> Result<()> {
        for msg in &lsp_receiver {
            match msg {
                Message::Request(req) => self.dispatch_request(req),
                Message::Notification(notif) => {
                    if notif.method == notif::Exit::METHOD {
                        return Ok(());
                    }
                    self.dispatch_notification(notif)
                }
                Message::Response(_) => {}
            }
        }
        bail!("Channel closed")
    }

    fn dispatch_request(&mut self, req: Request) {
        if self.is_shutdown {
            let resp = Response::new_err(
                req.id,
                ErrorCode::InvalidRequest as i32,
                "Shutdown already requested.".into(),
            );
            self.sender.send(resp.into()).unwrap();
            return;
        }

        RequestDispatcher(self, Some(req))
            .on_sync_mut::<req::Shutdown>(|st, ()| {
                st.is_shutdown = true;
            })
            .on::<req::GotoDefinition>(handler::goto_definition)
            .on::<req::References>(handler::references)
            .on::<req::Completion>(handler::completion)
            .finish();
    }

    fn dispatch_notification(&mut self, notif: Notification) {
        NotificationDispatcher(self, Some(notif))
            .on_sync_mut::<notif::DidOpenTextDocument>(|st, params| {
                st.set_vfs_file_content(&params.text_document.uri, Some(params.text_document.text));
            })
            .on_sync_mut::<notif::DidCloseTextDocument>(|st, params| {
                st.set_vfs_file_content(&params.text_document.uri, None);
            })
            .on_sync_mut::<notif::DidChangeTextDocument>(|st, params| {
                if let Some(chg) = params.content_changes.into_iter().next() {
                    st.set_vfs_file_content(&params.text_document.uri, Some(chg.text));
                }
            })
            .finish();
    }

    fn send_notification<N: notif::Notification>(&self, params: N::Params) {
        self.sender
            .send(Notification::new(N::METHOD.into(), params).into())
            .unwrap();
    }

    fn snapshot(&self) -> StateSnapshot {
        StateSnapshot {
            analysis: self.host.snapshot(),
            vfs: Arc::clone(&self.vfs),
        }
    }

    fn set_vfs_file_content(&mut self, uri: &Url, text: Option<String>) {
        if let Ok(path) = VfsPath::try_from(uri) {
            let mut vfs = self.vfs.write().unwrap();
            vfs.set_file_content(path, text);

            let change = vfs.take_change();
            log::debug!("Files changed: {:?}", change);
            self.host.apply_change(change.clone());

            // Currently we push down changes immediately.
            assert_eq!(change.file_changes.len(), 1);
            let (file, text) = &change.file_changes[0];
            let diagnostics = vfs
                .file_line_map(*file)
                .and_then(|line_map| {
                    let _ = text.as_deref()?;
                    let diags = self.host.snapshot().diagnostics(*file).ok()?;
                    let diags = diags
                        .into_iter()
                        .take(MAX_DIAGNOSTICS_CNT)
                        .filter_map(|diag| convert::to_diagnostic(line_map, diag))
                        .collect::<Vec<_>>();
                    Some(diags)
                })
                .unwrap_or_default();
            self.send_notification::<notif::PublishDiagnostics>(PublishDiagnosticsParams {
                uri: uri.clone(),
                diagnostics,
                version: None,
            });
        }
    }
}

#[must_use = "RequestDispatcher::finish not called"]
struct RequestDispatcher<'s>(&'s mut State, Option<Request>);

impl<'s> RequestDispatcher<'s> {
    fn on_sync_mut<R: req::Request>(mut self, f: fn(&mut State, R::Params) -> R::Result) -> Self {
        if matches!(&self.1, Some(notif) if notif.method == R::METHOD) {
            let req = self.1.take().unwrap();
            let params = serde_json::from_value::<R::Params>(req.params).unwrap();
            let resp = f(self.0, params);
            let resp = Response::new_ok(req.id, serde_json::to_value(resp).unwrap());
            self.0.sender.send(resp.into()).unwrap();
        }
        self
    }

    // TODO: Error handling?
    fn on<R: req::Request>(mut self, f: fn(StateSnapshot, R::Params) -> R::Result) -> Self {
        if matches!(&self.1, Some(notif) if notif.method == R::METHOD) {
            let req = self.1.take().unwrap();
            let params = serde_json::from_value::<R::Params>(req.params).unwrap();
            let resp = f(self.0.snapshot(), params);
            let resp = Response::new_ok(req.id, serde_json::to_value(resp).unwrap());
            self.0.sender.send(resp.into()).unwrap();
        }
        self
    }

    fn finish(self) {
        if let Some(req) = self.1 {
            let resp = Response::new_err(req.id, ErrorCode::MethodNotFound as _, String::new());
            self.0.sender.send(resp.into()).unwrap();
        }
    }
}

#[must_use = "NotificationDispatcher::finish not called"]
struct NotificationDispatcher<'s>(&'s mut State, Option<Notification>);

impl<'s> NotificationDispatcher<'s> {
    fn on_sync_mut<N: notif::Notification>(mut self, f: fn(&mut State, N::Params)) -> Self {
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
    pub(crate) analysis: Analysis,
    pub(crate) vfs: Arc<RwLock<Vfs>>,
}
