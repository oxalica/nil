mod capabilities;
mod config;
mod convert;
mod handler;
mod lsp_ext;
mod semantic_tokens;
mod server;
mod vfs;

use anyhow::Result;
use ide::VfsPath;
use lsp_server::{Connection, ErrorCode};
use lsp_types::Url;
use std::fmt;

pub(crate) use server::{Server, StateSnapshot};
pub(crate) use vfs::{LineMap, Vfs};

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

#[derive(Debug)]
pub(crate) struct LspError {
    code: ErrorCode,
    message: String,
}

impl fmt::Display for LspError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // NB. This will be displayed in the editor.
        self.message.fmt(f)
    }
}

impl std::error::Error for LspError {}

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

pub fn run_server_stdio() -> Result<()> {
    let (conn, io_threads) = Connection::stdio();

    let server = Server::new(conn.sender, conn.receiver);
    server.run()?;

    tracing::info!("Leaving main loop");

    io_threads.join()?;
    Ok(())
}
