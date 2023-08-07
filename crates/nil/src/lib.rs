mod capabilities;
mod config;
mod convert;
mod handler;
mod lsp_ext;
mod meter;
mod semantic_tokens;
mod server;
mod vfs;

use anyhow::{Context, Result};
use async_lsp::client_monitor::ClientProcessMonitorLayer;
use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::server::LifecycleLayer;
use async_lsp::stdio::{PipeStdin, PipeStdout};
use async_lsp::tracing::TracingLayer;
use ide::VfsPath;
use lsp_types::Url;
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
        // Double the concurrency limit since many handlers are blocking anyway.
        Ok(n) => n.saturating_mul(2.try_into().expect("2 is not 0")),
        Err(err) => {
            tracing::error!("Failed to get available parallelism: {err}");
            2.try_into().expect("2 is not 0")
        }
    };
    tracing::info!("Max concurrent requests: {concurrency}");

    let init_messages = Vec::new();

    let stdin = PipeStdin::lock_tokio().context("stdin is not pipe-like")?;
    let stdout = PipeStdout::lock_tokio().context("stdout is not pipe-like")?;

    let (mainloop, _) = async_lsp::MainLoop::new_server(|client| {
        ServiceBuilder::new()
            .layer(
                TracingLayer::new()
                    .request(|r| tracing::info_span!("request", method = r.method))
                    .notification(|n| tracing::info_span!("notification", method = n.method))
                    .event(|e| tracing::info_span!("event", method = e.type_name())),
            )
            .layer(MeterLayer)
            .layer(LifecycleLayer::default())
            // TODO: Use `CatchUnwindLayer`.
            .layer(ConcurrencyLayer::new(concurrency))
            .layer(ClientProcessMonitorLayer::new(client.clone()))
            .service(Server::new_router(client, init_messages))
    });

    Ok(mainloop.run_buffered(stdin, stdout).await?)
}
