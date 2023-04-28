use crate::capabilities::{negotiate_capabilities, NegotiatedCapabilities};
use crate::config::{Config, CONFIG_KEY};
use crate::{convert, handler, lsp_ext, UrlExt, Vfs, MAX_FILE_LEN};
use anyhow::{bail, ensure, Context, Result};
use async_lsp::router::Router;
use async_lsp::{ClientSocket, ErrorCode, LanguageClient, ResponseError};
use ide::{Analysis, AnalysisHost, Cancelled, FlakeInfo, VfsPath};
use lsp_types::notification::Notification;
use lsp_types::request::{self as req, Request};
use lsp_types::{
    notification as notif, ConfigurationItem, ConfigurationParams, DidChangeConfigurationParams,
    DidChangeTextDocumentParams, DidChangeWatchedFilesParams,
    DidChangeWatchedFilesRegistrationOptions, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, FileChangeType, FileEvent, FileSystemWatcher, GlobPattern,
    InitializeParams, InitializeResult, InitializedParams, MessageActionItem,
    MessageActionItemProperty, MessageType, NumberOrString, OneOf, ProgressParams,
    ProgressParamsValue, PublishDiagnosticsParams, Registration, RegistrationParams,
    RelativePattern, ServerInfo, ShowMessageParams, ShowMessageRequestParams, Url,
    WorkDoneProgress, WorkDoneProgressBegin, WorkDoneProgressCreateParams, WorkDoneProgressEnd,
    WorkDoneProgressReport,
};
use nix_interop::nixos_options::{self, NixosOptions};
use nix_interop::{flake_lock, flake_output, FlakeUrl, FLAKE_FILE, FLAKE_LOCK_FILE};
use std::backtrace::Backtrace;
use std::borrow::BorrowMut;
use std::cell::Cell;
use std::collections::HashMap;
use std::future::{ready, Future};
use std::ops::ControlFlow;
use std::panic::UnwindSafe;
use std::path::Path;
use std::pin::pin;
use std::sync::{Arc, Once, RwLock};
use std::time::Duration;
use std::{fmt, panic};
use tokio::sync::watch;
use tokio::task;
use tokio::task::{AbortHandle, JoinHandle};

const LSP_SERVER_NAME: &str = "nil";
const FLAKE_ARCHIVE_PROGRESS_TOKEN: &str = "nil/flakeArchiveProgress";
const LOAD_INPUT_FLAKE_PROGRESS_TOKEN: &str = "nil/loadInputFlakeProgress";

const NIXOS_OPTIONS_FLAKE_INPUT: &str = "nixpkgs";

const PROGRESS_REPORT_PERIOD: Duration = Duration::from_millis(100);
const LOAD_FLAKE_WORKSPACE_DEBOUNCE_DURATION: Duration = Duration::from_millis(100);

type NotifyResult = ControlFlow<async_lsp::Result<()>>;

struct UpdateConfigEvent(serde_json::Value);
struct SetFlakeInfoEvent(Option<FlakeInfo>);
struct SetNixosOptionsEvent(NixosOptions);

pub struct Server {
    // States.
    /// This contains an internal RWLock and must not lock together with `vfs`.
    host: AnalysisHost,
    vfs: Arc<RwLock<Vfs>>,
    opened_files: HashMap<Url, FileData>,
    config: Arc<Config>,
    /// Tried to load flake?
    /// This is used to reload flake only once after the configuration is first loaded.
    tried_flake_load: bool,
    /// Is this workspace a flake?
    workspace_is_flake: bool,

    // Ongoing tasks.
    load_flake_workspace_fut: Option<JoinHandle<()>>,

    // Immutable (mostly).
    client: ClientSocket,
    capabilities: NegotiatedCapabilities,
    /// Messages to show once initialized.
    init_messages: Vec<ShowMessageParams>,
}

#[derive(Debug, Default)]
struct FileData {
    diagnostics_task: Option<AbortHandle>,
}

impl Server {
    pub fn new_router(client: ClientSocket, init_messages: Vec<ShowMessageParams>) -> Router<Self> {
        let this = Self::new(client, init_messages);
        let mut router = Router::new(this);
        router
            //// Lifecycle ////
            .request::<req::Initialize, _>(Self::on_initialize)
            .notification::<notif::Initialized>(Self::on_initialized)
            .request::<req::Shutdown, _>(|_, _| ready(Ok(())))
            .notification::<notif::Exit>(|_, _| ControlFlow::Break(Ok(())))
            //// Notifications ////
            .notification::<notif::DidOpenTextDocument>(Self::on_did_open)
            .notification::<notif::DidCloseTextDocument>(Self::on_did_close)
            .notification::<notif::DidChangeTextDocument>(Self::on_did_change)
            .notification::<notif::DidChangeConfiguration>(Self::on_did_change_configuration)
            // NB. This handler is mandatory.
            // > In former implementations clients pushed file events without the server actively asking for it.
            // Ref: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWatchedFiles
            .notification::<notif::DidChangeWatchedFiles>(Self::on_did_change_watched_files)
            .notification::<lsp_ext::ReloadFlake>(Self::on_reload_flake)
            //// Requests ////
            .request_snap::<req::GotoDefinition>(handler::goto_definition)
            .request_snap::<req::References>(handler::references)
            .request_snap::<req::Completion>(handler::completion)
            .request_snap::<req::SelectionRangeRequest>(handler::selection_range)
            .request_snap::<req::PrepareRenameRequest>(handler::prepare_rename)
            .request_snap::<req::Rename>(handler::rename)
            .request_snap::<req::SemanticTokensFullRequest>(handler::semantic_token_full)
            .request_snap::<req::SemanticTokensRangeRequest>(handler::semantic_token_range)
            .request_snap::<req::HoverRequest>(handler::hover)
            .request_snap::<req::DocumentSymbolRequest>(handler::document_symbol)
            .request_snap::<req::Formatting>(handler::formatting)
            .request_snap::<req::DocumentLinkRequest>(handler::document_links)
            .request_snap::<req::CodeActionRequest>(handler::code_action)
            .request_snap::<req::DocumentHighlightRequest>(handler::document_highlight)
            .request_snap::<lsp_ext::ParentModule>(handler::parent_module)
            //// Events ////
            .event(Self::on_set_flake_info)
            .event(Self::on_set_nixos_options)
            .event(Self::on_update_config)
            // Loopback event.
            .event(Self::on_did_change_watched_files);
        router
    }

    pub fn new(client: ClientSocket, init_messages: Vec<ShowMessageParams>) -> Self {
        Self {
            host: AnalysisHost::default(),
            vfs: Arc::new(RwLock::new(Vfs::new())),
            opened_files: HashMap::default(),
            // Will be set during initialization.
            config: Arc::new(Config::new("/non-existing-path".into())),
            tried_flake_load: false,
            workspace_is_flake: false,

            load_flake_workspace_fut: None,

            client,
            // Will be set during initialization.
            capabilities: NegotiatedCapabilities::default(),
            init_messages,
        }
    }

    fn on_initialize(
        &mut self,
        params: InitializeParams,
    ) -> impl Future<Output = Result<InitializeResult, ResponseError>> {
        tracing::info!("Init params: {params:?}");

        let (server_caps, final_caps) = negotiate_capabilities(&params);
        self.capabilities = final_caps;

        // TODO: Use `workspaceFolders`.
        let root_path = match params
            .root_uri
            .as_ref()
            .and_then(|uri| uri.to_file_path().ok())
        {
            Some(path) => path,
            None => std::env::current_dir().expect("Failed to the current directory"),
        };

        *Arc::get_mut(&mut self.config).expect("No concurrent access yet") = Config::new(root_path);

        ready(Ok(InitializeResult {
            capabilities: server_caps,
            server_info: Some(ServerInfo {
                name: LSP_SERVER_NAME.into(),
                version: option_env!("CFG_RELEASE").map(Into::into),
            }),
        }))
    }

    fn on_initialized(&mut self, _params: InitializedParams) -> NotifyResult {
        for msg in std::mem::take(&mut self.init_messages) {
            tracing::warn!("Init message ({:?}): {}", msg.typ, msg.message);
            let _: Result<_, _> = self.client.show_message(msg);
        }

        // Load configurations before loading flake.
        // The latter depends on `nix.binary`.
        // FIXME: This is still racy since `on_did_open` can also trigger flake reloading and would
        // read uninitialized configs.
        self.spawn_reload_config();

        // Make a virtual event to trigger loading of flake files for flake info.
        let flake_files_changed_event = DidChangeWatchedFilesParams {
            changes: [FLAKE_LOCK_FILE, FLAKE_FILE]
                .into_iter()
                .map(|name| {
                    let uri = Url::from_file_path(self.config.root_path.join(name))
                        .expect("Root must be absolute");
                    let typ = FileChangeType::CREATED;
                    FileEvent { uri, typ }
                })
                .collect(),
        };
        if self.capabilities.watch_files {
            tokio::spawn({
                let config = self.config.clone();
                let caps = self.capabilities.clone();
                let mut client = self.client.clone();
                async move {
                    Self::register_watched_files(&config, &caps, &mut client).await;
                    let _: Result<_, _> = client.emit(flake_files_changed_event);
                }
            });
        } else {
            self.on_did_change_watched_files(flake_files_changed_event)?;
        }

        ControlFlow::Continue(())
    }

    async fn register_watched_files(
        config: &Config,
        caps: &NegotiatedCapabilities,
        client: &mut ClientSocket,
    ) {
        let to_watcher = |pat: &str| FileSystemWatcher {
            glob_pattern: if caps.watch_files_relative_pattern {
                let root_uri = Url::from_file_path(&config.root_path).expect("Must be absolute");
                GlobPattern::Relative(RelativePattern {
                    base_uri: OneOf::Right(root_uri),
                    pattern: pat.into(),
                })
            } else {
                GlobPattern::String(format!("{}/{}", config.root_path.display(), pat))
            },
            // All events.
            kind: None,
        };
        let register_options = DidChangeWatchedFilesRegistrationOptions {
            watchers: [FLAKE_LOCK_FILE, FLAKE_FILE].map(to_watcher).into(),
        };
        let params = RegistrationParams {
            registrations: vec![Registration {
                id: notif::DidChangeWatchedFiles::METHOD.into(),
                method: notif::DidChangeWatchedFiles::METHOD.into(),
                register_options: Some(serde_json::to_value(register_options).unwrap()),
            }],
        };
        if let Err(err) = client.register_capability(params).await {
            client.show_message_ext(
                MessageType::ERROR,
                format!("Failed to watch flake files: {err:#}"),
            );
        }
        tracing::info!("Registered file watching for flake files");
    }

    fn on_did_open(&mut self, params: DidOpenTextDocumentParams) -> NotifyResult {
        // Ignore the open event for unsupported files, thus all following interactions
        // will error due to unopened files.
        let len = params.text_document.text.len();
        if len > MAX_FILE_LEN {
            self.client.show_message_ext(
                MessageType::WARNING,
                "Disable LSP functionalities for too large file ({len} > {MAX_FILE_LEN})",
            );
            return ControlFlow::Continue(());
        }

        let uri = params.text_document.uri;
        self.opened_files.insert(uri.clone(), FileData::default());
        self.set_vfs_file_content(&uri, params.text_document.text);

        // We created a new flake.nix
        if !self.workspace_is_flake
            && uri
                .to_file_path()
                .ok()
                .as_ref()
                .and_then(|path| path.file_name())
                .map_or(false, |name| name == FLAKE_FILE)
        {
            self.spawn_load_flake_workspace();
        }

        self.spawn_update_diagnostics(uri);

        ControlFlow::Continue(())
    }

    fn on_did_close(&mut self, params: DidCloseTextDocumentParams) -> NotifyResult {
        // N.B. Don't clear text here.
        // `DidCloseTextDocument` means the client ends its maintainance to a file but
        // not deletes it.
        self.opened_files.remove(&params.text_document.uri);

        // Clear diagnostics for closed files.
        let _: Result<_, _> =
            self.client
                .notify::<notif::PublishDiagnostics>(PublishDiagnosticsParams {
                    uri: params.text_document.uri,
                    diagnostics: Vec::new(),
                    version: None,
                });

        ControlFlow::Continue(())
    }

    fn on_did_change(&mut self, params: DidChangeTextDocumentParams) -> NotifyResult {
        let mut vfs = self.vfs.write().unwrap();
        let uri = params.text_document.uri;
        // Ignore files not maintained in Vfs.
        let Ok(file) = vfs.file_for_uri(&uri) else { return ControlFlow::Continue(()) };
        for change in params.content_changes {
            let ret = (|| {
                let del_range = match change.range {
                    None => None,
                    Some(range) => Some(convert::from_range(&vfs, file, range).ok()?.1),
                };
                vfs.change_file_content(file, del_range, &change.text)
                    .ok()?;
                Some(())
            })();
            if ret.is_none() {
                tracing::error!(
                    "File is out of sync! Failed to apply change for {uri}: {change:?}"
                );

                // Clear file states to minimize pollution of the broken state.
                self.opened_files.remove(&uri);
                let _: Result<_, _> = vfs.remove_uri(&uri);
            }
        }
        drop(vfs);

        // FIXME: This blocks.
        self.apply_vfs_change();

        self.spawn_update_diagnostics(uri);

        ControlFlow::Continue(())
    }

    fn on_did_change_configuration(
        &mut self,
        _params: DidChangeConfigurationParams,
    ) -> NotifyResult {
        // As stated in https://github.com/microsoft/language-server-protocol/issues/676,
        // this notification's parameters should be ignored and the actual config queried separately.
        self.spawn_reload_config();
        ControlFlow::Continue(())
    }

    fn on_did_change_watched_files(&mut self, params: DidChangeWatchedFilesParams) -> NotifyResult {
        tracing::debug!("Watched files changed: {params:?}");

        let mut flake_files_changed = true;
        for FileEvent { uri, typ } in &params.changes {
            // Don't reload files maintained by the client.
            if self.opened_files.contains_key(uri) {
                continue;
            }
            let Ok(path) = uri.to_file_path() else { continue };
            match *typ {
                FileChangeType::CREATED | FileChangeType::CHANGED => {
                    if let Ok(text) = std::fs::read_to_string(&path) {
                        self.set_vfs_file_content(uri, text);
                    }
                }
                FileChangeType::DELETED => {
                    let _: Result<_> = self.vfs.write().unwrap().remove_uri(uri);
                }
                _ => continue,
            }
            if let Ok(relative) = path.strip_prefix(&self.config.root_path) {
                if relative == Path::new(FLAKE_FILE) || relative == Path::new(FLAKE_LOCK_FILE) {
                    flake_files_changed = true;
                }
            }
        }

        if flake_files_changed {
            self.spawn_load_flake_workspace();
        }

        ControlFlow::Continue(())
    }

    fn on_reload_flake(&mut self, (): ()) -> NotifyResult {
        self.spawn_load_flake_workspace();
        ControlFlow::Continue(())
    }

    /// Spawn a task to (re)load the flake workspace via `flake.{nix,lock}`, including flake info,
    /// NixOS options and outputs (TODO).
    fn spawn_load_flake_workspace(&mut self) {
        let fut = task::spawn(Self::load_flake_workspace(
            self.vfs.clone(),
            self.config.clone(),
            self.capabilities.clone(),
            self.client.clone(),
        ));
        if let Some(prev_fut) = self.load_flake_workspace_fut.replace(fut) {
            prev_fut.abort();
        }
    }

    async fn load_flake_workspace(
        vfs: Arc<RwLock<Vfs>>,
        config: Arc<Config>,
        caps: NegotiatedCapabilities,
        mut client: ClientSocket,
    ) {
        // Delay the loading to debounce. Later triggers will cancel previous tasks at here.
        tokio::time::sleep(LOAD_FLAKE_WORKSPACE_DEBOUNCE_DURATION).await;

        tracing::info!("Loading flake workspace");

        let flake_info = match Self::load_flake_info(&vfs, &config).await {
            Ok(ret) => {
                let _: Result<_, _> = client.emit(SetFlakeInfoEvent(ret.clone()));
                ret
            }
            Err(err) => {
                client.show_message_ext(
                    MessageType::ERROR,
                    format!("Failed to load flake workspace: {err:#}"),
                );
                return;
            }
        };
        let Some(flake_info) = flake_info else { return };

        let missing_paths = || {
            flake_info
                .input_store_paths
                .iter()
                .filter(|(_, path)| !path.as_path().expect("Must be real paths").exists())
        };

        if missing_paths().next().is_some() {
            tracing::debug!(
                "Missing flake inputs: {:?}",
                missing_paths().collect::<Vec<_>>()
            );

            let do_fetch = if !caps.client_show_message_request {
                client.show_message_ext(
                    MessageType::WARNING,
                    "\
                    Some flake inputs are not available, please run `nix flake archive` to fetch them. \n\
                    Your LSP client doesn't support confirmation. You can enable auto-fetch in configurations.\
                    ",
                );
                false
            } else {
                let ret = client
                    .show_message_request(ShowMessageRequestParams {
                        typ: MessageType::INFO,
                        message: "\
                            Some flake inputs are not available. Fetch them now? \n\
                            You can enable auto-fetch in configurations.\
                        "
                        .into(),
                        actions: Some(vec![
                            MessageActionItem {
                                title: "Fetch".into(),
                                properties: [(
                                    // Matches below.
                                    "ok".into(),
                                    MessageActionItemProperty::Boolean(true),
                                )]
                                .into(),
                            },
                            MessageActionItem {
                                title: "Ignore missing ones".into(),
                                properties: HashMap::new(),
                            },
                        ]),
                    })
                    .await;
                // Matches above.
                matches!(ret, Ok(Some(item)) if item.properties.contains_key("ok"))
            };

            if do_fetch {
                tracing::info!("Archiving flake");
                let progress = Progress::new(
                    &client,
                    &caps,
                    FLAKE_ARCHIVE_PROGRESS_TOKEN,
                    "Fetching flake with inputs",
                    "nix flake archive".to_owned(),
                )
                .await;
                let flake_url = FlakeUrl::new_path(&config.root_path);
                let ret = flake_lock::archive(&config.nix_binary, &flake_url)
                    .await
                    .and_then(|()| {
                        let missing = missing_paths().collect::<Vec<_>>();
                        ensure!(
                            missing.is_empty(),
                            "command succeeded but some paths are still missing: {missing:?}"
                        );
                        Ok(())
                    });
                progress.done(None);

                if let Err(err) = ret {
                    client.show_message_ext(
                        MessageType::ERROR,
                        format_args!("Failed to archiving flake: {err:#}"),
                    );
                    // Fallthrough and load the rest if possible.
                }
            }
        }

        // TODO: A better way to retrieve the nixpkgs for options?
        if let Some(nixpkgs_path) = flake_info
            .input_store_paths
            .get(NIXOS_OPTIONS_FLAKE_INPUT)
            .and_then(VfsPath::as_path)
            .filter(|path| path.exists())
        {
            tracing::info!("Evaluating NixOS options from {}", nixpkgs_path.display());

            let ret = nixos_options::eval_all_options(&config.nix_binary, nixpkgs_path)
                .await
                .context("Failed to evaluate NixOS options");
            match ret {
                // Sanity check.
                Ok(opts) if !opts.is_empty() => {
                    tracing::info!("Loaded NixOS options ({} top-level options)", opts.len());
                    let _: Result<_, _> = client.emit(SetNixosOptionsEvent(opts));
                }
                Ok(_) => tracing::error!("Empty NixOS options?"),
                Err(err) => {
                    client.show_message_ext(MessageType::ERROR, format_args!("{err:#}"));
                }
            }
        }

        Self::load_input_flakes(flake_info, &config, &caps, &mut client).await;
    }

    async fn load_input_flakes(
        mut flake_info: FlakeInfo,
        config: &Config,
        caps: &NegotiatedCapabilities,
        client: &mut ClientSocket,
    ) {
        // Filter out missing paths.
        let mut input_paths = flake_info
            .input_store_paths
            .iter()
            .filter_map(|(input_name, path)| {
                let path = path.as_path().expect("Must be real paths");
                // FIXME: Filter `flake = true` inputs.
                path.join(FLAKE_FILE).exists().then_some((input_name, path))
            })
            .collect::<Vec<_>>();

        // Fast path.
        if input_paths.is_empty() {
            return;
        }

        // Sort by input names to keep evaluation order stable.
        input_paths.sort_by_key(|&(name, _)| name);

        let input_cnt = input_paths.len();
        tracing::info!("Evaluating {input_cnt} flake inputs");

        let progress = Progress::new(
            client,
            caps,
            LOAD_INPUT_FLAKE_PROGRESS_TOKEN,
            "Evaluating input flakes",
            format!("[0/{input_cnt}]"),
        )
        .await;

        let include_legacy = match nix_interop::info::get(&config.nix_binary).await {
            Ok(info) => {
                tracing::debug!("Nix info: {info:?}");
                info.flake_show_filter_systems
            }
            Err(err) => {
                client.show_message_ext(
                    MessageType::ERROR,
                    format!("Failed to get information about Nix: {err:#}"),
                );
                false
            }
        };

        let mut error_cnt = 0;
        for (i, (input_name, path)) in input_paths.iter().copied().enumerate() {
            let report = |path: &str| {
                let dot = if path.is_empty() { "" } else { "." };
                progress.report(
                    (i * 100 / input_cnt) as u32,
                    format!("[{i}/{input_cnt}] {input_name}{dot}{path}"),
                );
            };
            report("");

            tracing::info!("Evaluating flake input {input_name:?}");

            let (watcher_tx, watcher_rx) = watch::channel(String::new());
            let flake_url = FlakeUrl::new_path(path);
            let mut eval_fut = pin!(flake_output::eval_flake_output(
                &config.nix_binary,
                &flake_url,
                Some(watcher_tx),
                include_legacy,
            ));
            let ret = loop {
                match tokio::time::timeout(PROGRESS_REPORT_PERIOD, eval_fut.as_mut()).await {
                    Ok(ret) => break ret,
                    Err(_) => report(&watcher_rx.borrow()),
                }
            };

            let output = match ret {
                Ok(output) => output,
                Err(err) => {
                    // Don't spam on configuration errors (eg. bad Nix path).
                    if error_cnt == 0 {
                        client.show_message_ext(
                            MessageType::ERROR,
                            format!("Flake input {input_name:?} cannot be evaluated: {err:#}"),
                        );
                    }
                    error_cnt += 1;
                    continue;
                }
            };
            flake_info
                .input_flake_outputs
                .insert(input_name.clone(), output);
            let _: Result<_, _> = client.emit(SetFlakeInfoEvent(Some(flake_info.clone())));
        }

        tracing::info!("Finished loading flake inputs. {error_cnt}/{input_cnt} failed");
        let msg =
            (error_cnt != 0).then(|| format!("{error_cnt}/{input_cnt} input(s) failed to load"));
        progress.done(msg);
    }

    async fn load_flake_info(vfs: &RwLock<Vfs>, config: &Config) -> Result<Option<FlakeInfo>> {
        tracing::info!("Loading flake info");

        let (flake_file, lock_src) = {
            let vfs = vfs.read().unwrap();

            let flake_vpath = VfsPath::new(config.root_path.join(FLAKE_FILE));
            // We always load flake.nix when initialized. If there's none in Vfs, there's none.
            let Ok(flake_file) = vfs.file_for_path(&flake_vpath) else { return Ok(None) };

            let lock_vpath = VfsPath::new(config.root_path.join(FLAKE_LOCK_FILE));
            let Ok(lock_file) = vfs.file_for_path(&lock_vpath)
            else {
                return Ok(Some(FlakeInfo {
                    flake_file,
                    input_store_paths: HashMap::new(),
                    input_flake_outputs: HashMap::new(),
                }));
            };
            let lock_src = vfs.content_for_file(lock_file);
            (flake_file, lock_src)
        };

        let inputs =
            flake_lock::resolve_flake_locked_inputs(&config.nix_binary, lock_src.as_bytes())
                .await
                .context("Failed to resolve flake inputs from lock file")?;

        // We only need the map for input -> store path.
        let input_store_paths = inputs
            .into_iter()
            .map(|(key, input)| (key, VfsPath::new(input.store_path)))
            .collect();
        Ok(Some(FlakeInfo {
            flake_file,
            input_store_paths,
            input_flake_outputs: HashMap::new(),
        }))
    }

    fn on_set_flake_info(&mut self, info: SetFlakeInfoEvent) -> NotifyResult {
        tracing::debug!("Set flake info: {:?}", info.0);
        self.workspace_is_flake = info.0.is_some();
        self.vfs.write().unwrap().set_flake_info(info.0);
        self.apply_vfs_change();
        ControlFlow::Continue(())
    }

    fn on_set_nixos_options(&mut self, opts: SetNixosOptionsEvent) -> NotifyResult {
        tracing::debug!("Set NixOS options ({:?} top-levels)", opts.0.len());
        self.vfs.write().unwrap().set_nixos_options(opts.0);
        self.apply_vfs_change();
        ControlFlow::Continue(())
    }

    fn spawn_reload_config(&self) {
        let mut client = self.client.clone();
        tokio::spawn(async move {
            let ret = client
                .configuration(ConfigurationParams {
                    items: vec![ConfigurationItem {
                        scope_uri: None,
                        section: Some(CONFIG_KEY.into()),
                    }],
                })
                .await;
            let mut v = match ret {
                Ok(v) => v,
                Err(err) => {
                    client.show_message_ext(
                        MessageType::ERROR,
                        format_args!("Failed to update config: {err}"),
                    );
                    return;
                }
            };
            tracing::debug!("Updating config: {:?}", v);
            let v = v.pop().unwrap_or_default();
            let _: Result<_, _> = client.emit(UpdateConfigEvent(v));
        });
    }

    fn on_update_config(&mut self, value: UpdateConfigEvent) -> NotifyResult {
        let mut config = Config::clone(&self.config);
        let (errors, updated_diagnostics) = config.update(value.0);
        tracing::debug!("Updated config, errors: {errors:?}, config: {config:?}");
        self.config = Arc::new(config);

        if !errors.is_empty() {
            let msg = ["Failed to apply some settings:"]
                .into_iter()
                .chain(errors.iter().flat_map(|s| ["\n- ", s]))
                .collect::<String>();
            self.client.show_message_ext(MessageType::ERROR, msg);
        }

        // Refresh all diagnostics since the filter may be changed.
        if updated_diagnostics {
            // Pre-collect to avoid mutability violation.
            let uris = self.opened_files.keys().cloned().collect::<Vec<_>>();
            for uri in uris {
                tracing::trace!("Recalculate diagnostics of {uri}");
                self.spawn_update_diagnostics(uri.clone());
            }
        }

        // If this is the first load, load the flake workspace, which depends on `nix.binary`.
        if !self.tried_flake_load {
            self.tried_flake_load = true;
            self.spawn_load_flake_workspace();
        }

        ControlFlow::Continue(())
    }

    fn spawn_update_diagnostics(&mut self, uri: Url) {
        let task = self.spawn_with_snapshot({
            let uri = uri.clone();
            move |snap| {
                // Return empty diagnostics for ignored files.
                (!snap.config.diagnostics_excluded_files.contains(&uri))
                    .then(|| {
                        with_catch_unwind("diagnostics", || handler::diagnostics(snap, &uri))
                            .unwrap_or_else(|err| {
                                tracing::error!("Failed to calculate diagnostics: {err}");
                                Vec::new()
                            })
                    })
                    .unwrap_or_default()
            }
        });

        // Can this really fail?
        let Some(f) = self.opened_files.get_mut(&uri) else { task.abort(); return; };
        if let Some(prev_task) = f.diagnostics_task.replace(task.abort_handle()) {
            prev_task.abort();
        }

        let mut client = self.client.clone();
        task::spawn(async move {
            if let Ok(diagnostics) = task.await {
                tracing::debug!("Publish {} diagnostics for {}", diagnostics.len(), uri);
                let _: Result<_, _> = client.publish_diagnostics(PublishDiagnosticsParams {
                    uri,
                    diagnostics,
                    version: None,
                });
            } else {
                // Task cancelled, then there must be another task queued already. Do nothing.
            }
        });
    }

    /// Create a blocking task with a database snapshot as the input.
    // NB. `spawn_blocking` must be called immediately after snapshotting, so that the read guard
    // held in `Analysis` is sent out of the async runtime worker. Otherwise, the read guard
    // is held by the async runtime, and the next `apply_change` acquiring the write guard would
    // deadlock.
    fn spawn_with_snapshot<T: Send + 'static>(
        &self,
        f: impl FnOnce(StateSnapshot) -> T + Send + 'static,
    ) -> JoinHandle<T> {
        let snap = StateSnapshot {
            analysis: self.host.snapshot(),
            vfs: Arc::clone(&self.vfs),
            config: Arc::clone(&self.config),
        };
        task::spawn_blocking(move || f(snap))
    }

    fn set_vfs_file_content(&mut self, uri: &Url, text: String) {
        let vpath = uri.to_vfs_path();
        self.vfs.write().unwrap().set_path_content(vpath, text);
        self.apply_vfs_change();
    }

    fn apply_vfs_change(&mut self) {
        let changes = self.vfs.write().unwrap().take_change();
        tracing::trace!("Apply VFS changes: {:?}", changes);

        // N.B. This acquires the internal write lock.
        // Must be called without holding the lock of `vfs`.
        self.host.apply_change(changes);
    }
}

trait RouterExt: BorrowMut<Router<Server>> {
    fn request_snap<R: Request>(
        &mut self,
        f: impl Fn(StateSnapshot, R::Params) -> Result<R::Result> + Send + Copy + UnwindSafe + 'static,
    ) -> &mut Self
    where
        R::Params: Send + UnwindSafe + 'static,
        R::Result: Send + 'static,
    {
        self.borrow_mut().request::<R, _>(move |this, params| {
            let task = this.spawn_with_snapshot(move |snap| {
                with_catch_unwind(R::METHOD, move || f(snap, params))
            });
            async move {
                task.await
                    .expect("Already catch_unwind")
                    .map_err(error_to_response)
            }
        });
        self
    }
}

impl RouterExt for Router<Server> {}

trait ClientExt: BorrowMut<ClientSocket> {
    fn show_message_ext(&mut self, typ: MessageType, msg: impl fmt::Display) {
        // Maybe connect all tracing::* to LSP ShowMessage?
        let _: Result<_, _> = self.borrow_mut().show_message(ShowMessageParams {
            typ,
            message: msg.to_string(),
        });
    }
}

impl ClientExt for ClientSocket {}

struct Progress {
    client: ClientSocket,
    token: Option<String>,
}

impl Progress {
    async fn new(
        client: &ClientSocket,
        caps: &NegotiatedCapabilities,
        token: impl fmt::Display,
        title: impl fmt::Display,
        message: impl Into<Option<String>>,
    ) -> Self {
        let token = token.to_string();
        let created = caps.server_initiated_progress
            && client
                .request::<req::WorkDoneProgressCreate>(WorkDoneProgressCreateParams {
                    token: NumberOrString::String(token.clone()),
                })
                .await
                .is_ok();
        let this = Self {
            client: client.clone(),
            token: created.then_some(token),
        };
        this.notify(WorkDoneProgress::Begin(WorkDoneProgressBegin {
            title: title.to_string(),
            cancellable: None,
            message: message.into(),
            percentage: None,
        }));
        this
    }

    fn notify(&self, progress: WorkDoneProgress) {
        let Some(token) = &self.token else { return };
        let _: Result<_, _> = self.client.notify::<notif::Progress>(ProgressParams {
            token: NumberOrString::String(token.clone()),
            value: ProgressParamsValue::WorkDone(progress),
        });
    }

    fn report(&self, percentage: u32, message: String) {
        assert!((0..=100).contains(&percentage));
        self.notify(WorkDoneProgress::Report(WorkDoneProgressReport {
            cancellable: None,
            message: Some(message),
            percentage: Some(percentage),
        }));
    }

    fn done(mut self, message: Option<String>) {
        self.notify(WorkDoneProgress::End(WorkDoneProgressEnd { message }));
        // Don't drop again.
        self.token = None;
    }
}

impl Drop for Progress {
    fn drop(&mut self) {
        self.notify(WorkDoneProgress::End(WorkDoneProgressEnd { message: None }));
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
            let loc = info
                .location()
                .map(|loc| loc.to_string())
                .unwrap_or_default();
            let backtrace = Backtrace::force_capture();
            PANIC_LOCATION.with(|inner| {
                inner.set(format!("Location: {loc:#}\nBacktrace: {backtrace:#}"));
            });
            old_hook(info);
        }));
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
                loc = "Location: unknown".into();
            }
            tracing::error!("Panicked in {ctx}: {reason}\n{loc}");
            bail!("Panicked in {ctx}: {reason}\n{loc}");
        }
    }
}

fn error_to_response(err: anyhow::Error) -> ResponseError {
    if err.is::<Cancelled>() {
        return ResponseError::new(ErrorCode::REQUEST_CANCELLED, "Client cancelled");
    }
    match err.downcast::<ResponseError>() {
        Ok(resp) => resp,
        Err(err) => ResponseError::new(ErrorCode::INTERNAL_ERROR, err),
    }
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
