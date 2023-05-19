mod capabilities;
mod config;
mod convert;
mod handler;
mod lsp_ext;
mod meter;
mod semantic_tokens;
mod server;
mod vfs;

use anyhow::Result;
use async_lsp::client_monitor::ClientProcessMonitorLayer;
use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::server::LifecycleLayer;
use async_lsp::stdio::{PipeStdin, PipeStdout};
use async_lsp::tracing::TracingLayer;
use ide::VfsPath;
use lsp_types::{MessageType, ShowMessageParams, Url};
use tokio::io::BufReader;
use tower::ServiceBuilder;

pub(crate) use server::{Server, StateSnapshot};
pub(crate) use vfs::{LineMap, Vfs};

use crate::meter::MeterLayer;

/// The file length limit. Files larger than this will be rejected from all interactions.
/// The hard limit is `u32::MAX` due to following conditions.
/// - The parser and the `rowan` library uses `u32` based indices.
/// - `vfs::LineMap` uses `u32` based indices.
///
/// Since large files can cause significant performance issues, also to
/// be away from off-by-n errors, here's an arbitrary chosen limit: 128MiB.
///
/// If you have any real world usages for files larger than this, please file an issue.
pub const MAX_FILE_LEN: usize = 128 << 20;

pub(crate) trait UrlExt: Sized {
    fn to_vfs_path(&self) -> VfsPath;
    fn from_vfs_path(path: &VfsPath) -> Self;
}

impl UrlExt for Url {
    fn to_vfs_path(&self) -> VfsPath {
        // `Url::to_file_path` doesn't do schema check.
        if self.scheme() == "file" {
            if let Ok(path) = self.to_file_path() {
                return path.into();
            }
            tracing::warn!("Ignore invalid file URI: {self}");
        }
        VfsPath::Virtual(self.as_str().to_owned())
    }

    fn from_vfs_path(vpath: &VfsPath) -> Self {
        match vpath {
            VfsPath::Path(path) => Url::from_file_path(path).expect("VfsPath must be absolute"),
            VfsPath::Virtual(uri) => uri.parse().expect("Virtual path must be an URI"),
        }
    }
}

pub async fn run_server_stdio() -> Result<()> {
    let concurrency = match std::thread::available_parallelism() {
        Ok(n) => n,
        Err(err) => {
            tracing::error!("Failed to get available parallelism: {err}");
            1.try_into().expect("1 is not 0")
        }
    };
    tracing::info!("Max concurrent requests: {concurrency}");

    let mut init_messages = Vec::new();
    if let Some(err) = PipeStdin::lock().err().or_else(|| PipeStdout::lock().err()) {
        init_messages.push(ShowMessageParams {
            typ: MessageType::WARNING,
            message: format!(
                "\
                Invalid stdin/stdout fd mode: {err}. \n\
                This will become a hard error in the future. \n\
                Please file an issue with your editor configurations: \n\
                https://github.com/oxalica/nil/issues
                ",
            ),
        });
    }

    let (frontend, _) = async_lsp::Frontend::new_server(|client| {
        ServiceBuilder::new()
            .layer(TracingLayer::default())
            .layer(MeterLayer::default())
            .layer(LifecycleLayer::default())
            // TODO: Use `CatchUnwindLayer`.
            .layer(ConcurrencyLayer::new(concurrency))
            .layer(ClientProcessMonitorLayer::new(client.clone()))
            .service(Server::new_router(client, init_messages))
    });

    let input = BufReader::new(tokio::io::stdin());
    let output = tokio::io::stdout();
    Ok(frontend.run(input, output).await?)
}
