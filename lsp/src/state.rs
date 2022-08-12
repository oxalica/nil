use crate::{convert, handler, Vfs};
use anyhow::{bail, Result};
use crossbeam_channel::{Receiver, Sender};
use lsp_server::{ErrorCode, Message, Notification, Request, Response};
use lsp_types::notification::Notification as _;
use lsp_types::{notification as notif, request as req, PublishDiagnosticsParams, Url};
use nil::{Analysis, AnalysisHost, VfsPath};
use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

const MAX_DIAGNOSTICS_CNT: usize = 128;

pub struct State {
    host: AnalysisHost,
    vfs: Arc<RwLock<Vfs>>,
    opened_files: Arc<RwLock<HashSet<Url>>>,
    workspace_root: Option<PathBuf>,
    sender: Sender<Message>,
    is_shutdown: bool,
}

impl State {
    pub fn new(responder: Sender<Message>, workspace_root: Option<PathBuf>) -> Self {
        let vfs = Vfs::new(workspace_root.clone().unwrap_or_else(|| PathBuf::from("/")));
        Self {
            host: Default::default(),
            vfs: Arc::new(RwLock::new(vfs)),
            opened_files: Default::default(),
            workspace_root,
            sender: responder,
            is_shutdown: false,
        }
    }

    pub fn run(&mut self, lsp_receiver: Receiver<Message>) -> Result<()> {
        if let Some(root) = &self.workspace_root {
            let mut vfs = self.vfs.write().unwrap();
            for entry in ignore::WalkBuilder::new(root).follow_links(false).build() {
                (|| -> Option<()> {
                    let entry = entry.ok()?;
                    let relative_path = entry.path().strip_prefix(root).ok()?;
                    let vpath = VfsPath::from_path(relative_path)?;
                    let text = fs::read_to_string(entry.path()).ok();
                    vfs.set_path_content(vpath, text);
                    Some(())
                })();
            }
            drop(vfs);
            self.apply_vfs_change();
        }

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
                let uri = &params.text_document.uri;
                st.opened_files.write().unwrap().insert(uri.clone());
                st.set_vfs_file_content(uri, Some(params.text_document.text));
            })
            .on_sync_mut::<notif::DidCloseTextDocument>(|st, params| {
                // N.B. Don't clear text here.
                st.opened_files
                    .write()
                    .unwrap()
                    .remove(&params.text_document.uri);
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
        self.vfs.write().unwrap().set_uri_content(uri, text);
        self.apply_vfs_change();
    }

    fn apply_vfs_change(&mut self) {
        let mut vfs = self.vfs.write().unwrap();
        let change = vfs.take_change();
        let file_changes = change
            .file_changes
            .iter()
            .map(|(file, text)| (*file, text.is_some()))
            .collect::<Vec<_>>();
        log::debug!("Change: {:?}", change);
        self.host.apply_change(change);

        let snap = self.host.snapshot();
        let opened_files = self.opened_files.read().unwrap();
        for (file, has_text) in file_changes {
            let uri = vfs.get_uri_for_file(file).unwrap();
            if !opened_files.contains(&uri) {
                continue;
            }

            let diagnostics = has_text
                .then(|| {
                    let line_map = vfs.get_line_map(file)?;
                    let diags = snap.diagnostics(file).ok()?;
                    let diags = diags
                        .into_iter()
                        .take(MAX_DIAGNOSTICS_CNT)
                        .filter_map(|diag| convert::to_diagnostic(&vfs, line_map, diag))
                        .collect::<Vec<_>>();
                    Some(diags)
                })
                .flatten()
                .unwrap_or_default();
            self.send_notification::<notif::PublishDiagnostics>(PublishDiagnosticsParams {
                uri,
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
