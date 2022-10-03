use crate::{convert, handler, Result, Vfs};
use crossbeam_channel::{Receiver, Sender};
use ide::{Analysis, AnalysisHost, Cancelled};
use lsp_server::{ErrorCode, Message, Notification, ReqQueue, Request, RequestId, Response};
use lsp_types::notification::Notification as _;
use lsp_types::{
    notification as notif, request as req, ConfigurationItem, ConfigurationParams, Diagnostic,
    InitializeParams, MessageType, NumberOrString, PublishDiagnosticsParams, ShowMessageParams,
    Url,
};
use serde::Deserialize;
use std::cell::Cell;
use std::collections::HashSet;
use std::panic::UnwindSafe;
use std::sync::{Arc, Once, RwLock};
use std::{panic, thread};

const CONFIG_KEY: &str = "nil";

#[derive(Debug, Clone, Default, Deserialize)]
pub struct Config {
    pub(crate) diagnostics_ignored: HashSet<String>,
    pub(crate) formatting_command: Option<Vec<String>>,
}

type ReqHandler = fn(&mut Server, Response);

type Task = Box<dyn FnOnce() -> Event + Send>;

enum Event {
    Response(Response),
    Diagnostics(Url, Vec<Diagnostic>),
}

pub struct Server {
    // States.
    host: AnalysisHost,
    vfs: Arc<RwLock<Vfs>>,
    opened_files: HashSet<Url>,
    config: Arc<Config>,
    is_shutdown: bool,

    // Message passing.
    req_queue: ReqQueue<(), ReqHandler>,
    lsp_tx: Sender<Message>,
    task_tx: Sender<Task>,
    event_tx: Sender<Event>,
    event_rx: Receiver<Event>,
}

impl Server {
    pub fn new(lsp_tx: Sender<Message>) -> Self {
        let (task_tx, task_rx) = crossbeam_channel::unbounded();
        let (event_tx, event_rx) = crossbeam_channel::unbounded();
        let worker_cnt = thread::available_parallelism().map_or(1, |n| n.get());
        for _ in 0..worker_cnt {
            let task_rx = task_rx.clone();
            let event_tx = event_tx.clone();
            thread::Builder::new()
                .name("Worker".into())
                .spawn(move || Self::worker(task_rx, event_tx))
                .expect("Failed to spawn worker threads");
        }
        tracing::info!("Started {worker_cnt} workers");

        Self {
            host: Default::default(),
            vfs: Arc::new(RwLock::new(Vfs::new())),
            opened_files: Default::default(),
            config: Arc::new(Config::default()),
            is_shutdown: false,

            req_queue: ReqQueue::default(),
            lsp_tx,
            task_tx,
            event_tx,
            event_rx,
        }
    }

    fn worker(task_rx: Receiver<Task>, event_tx: Sender<Event>) {
        while let Ok(task) = task_rx.recv() {
            if event_tx.send(task()).is_err() {
                break;
            }
        }
    }

    pub fn run(&mut self, lsp_rx: Receiver<Message>, _init_params: InitializeParams) -> Result<()> {
        loop {
            crossbeam_channel::select! {
                recv(lsp_rx) -> msg => {
                    match msg.map_err(|_| "Channel closed")? {
                        Message::Request(req) => self.dispatch_request(req),
                        Message::Notification(notif) => {
                            if notif.method == notif::Exit::METHOD {
                                return Ok(());
                            }
                            self.dispatch_notification(notif)?;
                        }
                        Message::Response(resp) => {
                            if let Some(callback) = self.req_queue.outgoing.complete(resp.id.clone()) {
                                callback(self, resp);
                            }
                        }
                    }
                }
                recv(self.event_rx) -> event => {
                    self.dispatch_event(event.map_err(|_| "Worker panicked")?);
                }
            }
        }
    }

    fn dispatch_event(&mut self, event: Event) {
        match event {
            Event::Response(resp) => {
                if let Some(()) = self.req_queue.incoming.complete(resp.id.clone()) {
                    self.lsp_tx.send(resp.into()).unwrap();
                }
            }
            Event::Diagnostics(uri, diagnostics) => {
                self.send_notification::<notif::PublishDiagnostics>(PublishDiagnosticsParams {
                    uri,
                    diagnostics,
                    version: None,
                });
            }
        }
    }

    fn dispatch_request(&mut self, req: Request) {
        if self.is_shutdown {
            let resp = Response::new_err(
                req.id,
                ErrorCode::InvalidRequest as i32,
                "Shutdown already requested.".into(),
            );
            self.lsp_tx.send(resp.into()).unwrap();
            return;
        }

        RequestDispatcher(self, Some(req))
            .on_sync_mut::<req::Shutdown>(|st, ()| {
                st.is_shutdown = true;
                Ok(())
            })
            .on::<req::GotoDefinition>(handler::goto_definition)
            .on::<req::References>(handler::references)
            .on::<req::Completion>(handler::completion)
            .on::<req::SelectionRangeRequest>(handler::selection_range)
            .on::<req::PrepareRenameRequest>(handler::prepare_rename)
            .on::<req::Rename>(handler::rename)
            .on::<req::SemanticTokensFullRequest>(handler::semantic_token_full)
            .on::<req::SemanticTokensRangeRequest>(handler::semantic_token_range)
            .on::<req::HoverRequest>(handler::hover)
            .on::<req::DocumentSymbolRequest>(handler::document_symbol)
            .on::<req::Formatting>(handler::formatting)
            .finish();
    }

    fn dispatch_notification(&mut self, notif: Notification) -> Result<()> {
        NotificationDispatcher(self, Some(notif))
            .on_sync_mut::<notif::Cancel>(|st, params| {
                let id: RequestId = match params.id {
                    NumberOrString::Number(id) => id.into(),
                    NumberOrString::String(id) => id.into(),
                };
                if let Some(resp) = st.req_queue.incoming.cancel(id) {
                    st.lsp_tx.send(resp.into()).unwrap();
                }
                Ok(())
            })?
            .on_sync_mut::<notif::DidOpenTextDocument>(|st, params| {
                let uri = &params.text_document.uri;
                st.opened_files.insert(uri.clone());
                st.set_vfs_file_content(uri, params.text_document.text)?;
                Ok(())
            })?
            .on_sync_mut::<notif::DidCloseTextDocument>(|st, params| {
                // N.B. Don't clear text here.
                st.opened_files.remove(&params.text_document.uri);
                Ok(())
            })?
            .on_sync_mut::<notif::DidChangeTextDocument>(|st, params| {
                let mut vfs = st.vfs.write().unwrap();
                if let Ok(file) = vfs.file_for_uri(&params.text_document.uri) {
                    for change in params.content_changes {
                        if let Some((_, range)) = change
                            .range
                            .and_then(|range| convert::from_range(&vfs, file, range).ok())
                        {
                            let _ = vfs.change_file_content(file, range, &change.text);
                        }
                    }
                    drop(vfs);
                    st.apply_vfs_change();
                }
                Ok(())
            })?
            .on_sync_mut::<notif::DidChangeConfiguration>(|st, _params| {
                // As stated in https://github.com/microsoft/language-server-protocol/issues/676,
                // this notification's parameters should be ignored and the actual config queried separately.
                st.send_request::<req::WorkspaceConfiguration>(
                    ConfigurationParams {
                        items: vec![ConfigurationItem {
                            scope_uri: None,
                            section: Some(CONFIG_KEY.into()),
                        }],
                    },
                    |st, resp| {
                        let ret = match resp.error {
                            None => Ok(resp
                                .result
                                .and_then(|mut v| Some(v.get_mut(0)?.take()))
                                .unwrap_or_default()),
                            Some(err) => Err(format!(
                                "LSP error {}: {}, data: {:?}",
                                err.code, err.message, err.data
                            )),
                        };
                        match ret {
                            Ok(v) => {
                                tracing::debug!("Updating config: {:?}", v);
                                st.update_config(v);
                            }
                            Err(err) => tracing::error!("Failed to update config: {}", err),
                        }
                    },
                );
                Ok(())
            })?
            .on_sync_mut::<notif::DidChangeWatchedFiles>(|_st, _params| {
                // Workaround:
                // > In former implementations clients pushed file events without the server actively asking for it.
                // Ref: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWatchedFiles
                Ok(())
            })?
            .finish()
    }

    fn send_request<R: req::Request>(
        &mut self,
        params: R::Params,
        callback: fn(&mut Self, Response),
    ) {
        let req = self
            .req_queue
            .outgoing
            .register(R::METHOD.into(), params, callback);
        self.lsp_tx.send(req.into()).unwrap();
    }

    fn send_notification<N: notif::Notification>(&self, params: N::Params) {
        self.lsp_tx
            .send(Notification::new(N::METHOD.into(), params).into())
            .unwrap();
    }

    fn show_message(&self, typ: MessageType, msg: impl Into<String>) {
        self.send_notification::<notif::ShowMessage>(ShowMessageParams {
            typ,
            message: msg.into(),
        });
    }

    fn update_config(&mut self, mut v: serde_json::Value) {
        let mut updated_diagnostics = false;
        let mut config = Config::clone(&self.config);
        let mut errors = Vec::new();
        if let Some(v) = v.pointer_mut("/diagnostics/ignored") {
            match serde_json::from_value(v.take()) {
                Ok(v) => {
                    config.diagnostics_ignored = v;
                    updated_diagnostics = true;
                }
                Err(e) => {
                    errors.push(format!("Invalid value of `diagnostics.ignored`: {e}"));
                }
            }
        }
        if let Some(v) = v.pointer_mut("/formatting/command") {
            match serde_json::from_value::<Option<Vec<String>>>(v.take()) {
                Ok(Some(v)) if v.is_empty() => {
                    errors.push("`formatting.command` must not be an empty list".into());
                }
                Ok(v) => {
                    config.formatting_command = v;
                }
                Err(e) => {
                    errors.push(format!("Invalid value of `formatting.command`: {e}"));
                }
            }
        }
        tracing::debug!("Updated config, errors: {errors:?}, config: {config:?}");
        self.config = Arc::new(config);

        if !errors.is_empty() {
            let msg = ["Failed to apply some settings:"]
                .into_iter()
                .chain(errors.iter().flat_map(|s| ["\n- ", s]))
                .collect::<String>();
            self.show_message(MessageType::ERROR, msg);
        }

        // Refresh all diagnostics since the filter may be changed.
        if updated_diagnostics {
            for uri in &self.opened_files {
                tracing::debug!("Recalculate diagnostics of {uri}");
                self.update_diagnostics(uri.clone());
            }
        }
    }

    fn update_diagnostics(&self, uri: Url) {
        let snap = self.snapshot();
        let task = move || {
            let diags = with_catch_unwind("diagnostics", || handler::diagnostics(snap, &uri))
                .unwrap_or_else(|err| {
                    tracing::error!("Failed to calculate diagnostics: {err}");
                    Vec::new()
                });
            Event::Diagnostics(uri, diags)
        };
        self.task_tx.send(Box::new(task)).unwrap();
    }

    fn snapshot(&self) -> StateSnapshot {
        StateSnapshot {
            analysis: self.host.snapshot(),
            vfs: Arc::clone(&self.vfs),
            config: Arc::clone(&self.config),
        }
    }

    fn set_vfs_file_content(&mut self, uri: &Url, text: String) -> Result<()> {
        self.vfs.write().unwrap().set_uri_content(uri, text)?;
        self.apply_vfs_change();
        Ok(())
    }

    fn apply_vfs_change(&mut self) {
        let file_changes = {
            let mut vfs = self.vfs.write().unwrap();
            let change = vfs.take_change();
            let file_changes = change
                .file_changes
                .iter()
                .map(|(file, text)| (vfs.uri_for_file(*file), !text.is_empty()))
                .collect::<Vec<_>>();
            tracing::debug!("Change: {:?}", change);
            self.host.apply_change(change);
            file_changes
        };

        for (uri, has_text) in file_changes {
            if !self.opened_files.contains(&uri) {
                continue;
            }
            if has_text {
                self.update_diagnostics(uri);
            } else {
                self.event_tx
                    .send(Event::Diagnostics(uri, Vec::new()))
                    .unwrap();
            }
        }
    }
}

#[must_use = "RequestDispatcher::finish not called"]
struct RequestDispatcher<'s>(&'s mut Server, Option<Request>);

impl<'s> RequestDispatcher<'s> {
    fn on_sync_mut<R: req::Request>(
        mut self,
        f: fn(&mut Server, R::Params) -> Result<R::Result>,
    ) -> Self {
        if matches!(&self.1, Some(notif) if notif.method == R::METHOD) {
            let req = self.1.take().unwrap();
            let ret = (|| {
                let params = serde_json::from_value::<R::Params>(req.params)?;
                let v = f(self.0, params)?;
                Ok(serde_json::to_value(v).unwrap())
            })();
            let resp = result_to_response(req.id, ret);
            self.0.lsp_tx.send(resp.into()).unwrap();
        }
        self
    }

    fn on<R>(mut self, f: fn(StateSnapshot, R::Params) -> Result<R::Result>) -> Self
    where
        R: req::Request,
        R::Params: 'static,
        R::Result: 'static,
    {
        if matches!(&self.1, Some(notif) if notif.method == R::METHOD) {
            let req = self.1.take().unwrap();
            let snap = self.0.snapshot();
            self.0.req_queue.incoming.register(req.id.clone(), ());
            let task = move || {
                let ret = with_catch_unwind(R::METHOD, || {
                    let params = serde_json::from_value::<R::Params>(req.params)?;
                    let resp = f(snap, params)?;
                    Ok(serde_json::to_value(resp)?)
                });
                Event::Response(result_to_response(req.id, ret))
            };
            self.0.task_tx.send(Box::new(task)).unwrap();
        }
        self
    }

    fn finish(self) {
        if let Some(req) = self.1 {
            let resp = Response::new_err(req.id, ErrorCode::MethodNotFound as _, String::new());
            self.0.lsp_tx.send(resp.into()).unwrap();
        }
    }
}

#[must_use = "NotificationDispatcher::finish not called"]
struct NotificationDispatcher<'s>(&'s mut Server, Option<Notification>);

impl<'s> NotificationDispatcher<'s> {
    fn on_sync_mut<N: notif::Notification>(
        mut self,
        f: fn(&mut Server, N::Params) -> Result<()>,
    ) -> Result<Self> {
        if matches!(&self.1, Some(notif) if notif.method == N::METHOD) {
            match serde_json::from_value::<N::Params>(self.1.take().unwrap().params) {
                Ok(params) => {
                    f(self.0, params)?;
                }
                Err(err) => {
                    tracing::error!("Failed to parse notification {}: {}", N::METHOD, err)
                }
            }
        }
        Ok(self)
    }

    fn finish(self) -> Result<()> {
        if let Some(notif) = self.1 {
            if !notif.method.starts_with("$/") {
                tracing::error!("Unhandled notification: {:?}", notif);
            }
        }
        Ok(())
    }
}

fn with_catch_unwind<T>(ctx: &str, f: impl FnOnce() -> Result<T> + UnwindSafe) -> Result<T> {
    static INSTALL_PANIC_HOOK: Once = Once::new();
    thread_local! {
        static PANIC_LOCATION: Cell<String> = Cell::new(String::new());
    }

    INSTALL_PANIC_HOOK.call_once(|| {
        let old_hook = panic::take_hook();
        panic::set_hook(Box::new(move |info| {
            if let Some(loc) = info.location() {
                PANIC_LOCATION.with(|inner| {
                    inner.set(loc.to_string());
                });
            }
            old_hook(info);
        }))
    });

    match panic::catch_unwind(f) {
        Ok(ret) => ret,
        Err(payload) => {
            let reason = payload
                .downcast_ref::<String>()
                .map(|s| &**s)
                .or_else(|| payload.downcast_ref::<&str>().map(|s| &**s))
                .unwrap_or("unknown");
            let mut loc = PANIC_LOCATION.with(|inner| inner.take());
            if loc.is_empty() {
                loc = "unknown".into();
            }
            let msg = format!("In {}, panicked at {}: {}", ctx, loc, reason);
            Err(msg.into())
        }
    }
}

fn result_to_response(id: RequestId, ret: Result<serde_json::Value>) -> Response {
    let err = match ret {
        Ok(v) => {
            return Response {
                id,
                result: Some(v),
                error: None,
            }
        }
        Err(err) => err,
    };

    if err.is::<Cancelled>() {
        // When client cancelled a request, a response is immediately sent back,
        // and this will be ignored.
        return Response::new_err(id, ErrorCode::ServerCancelled as i32, "Cancelled".into());
    }
    if let Some(err) = err.downcast_ref::<serde_json::Error>() {
        return Response::new_err(id, ErrorCode::InvalidParams as i32, err.to_string());
    }
    Response::new_err(id, ErrorCode::InternalError as i32, err.to_string())
}

#[derive(Debug)]
pub struct StateSnapshot {
    pub(crate) analysis: Analysis,
    vfs: Arc<RwLock<Vfs>>,
    pub(crate) config: Arc<Config>,
}

impl StateSnapshot {
    pub(crate) fn vfs(&self) -> impl std::ops::Deref<Target = Vfs> + '_ {
        self.vfs.read().unwrap()
    }
}
