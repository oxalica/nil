mod capabilities;
mod convert;
mod handler;
mod semantic_tokens;
mod server;
mod vfs;

use lsp_server::{Connection, ErrorCode};
use lsp_types::InitializeParams;
use std::path::PathBuf;
use std::{env, fmt};

pub(crate) use server::{Server, StateSnapshot};
pub(crate) use vfs::{LineMap, Vfs};

#[derive(Debug)]
pub(crate) struct LspError {
    code: ErrorCode,
    message: String,
}

impl fmt::Display for LspError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {}", self.code, self.message)
    }
}

impl std::error::Error for LspError {}

pub type Error = Box<dyn std::error::Error + Send + Sync>;
pub type Result<T, E = Error> = std::result::Result<T, E>;

pub fn main_loop(conn: Connection) -> Result<()> {
    let init_params =
        conn.initialize(serde_json::to_value(&capabilities::server_capabilities()).unwrap())?;
    tracing::info!("Init params: {}", init_params);

    let init_params = serde_json::from_value::<InitializeParams>(init_params)?;
    let workspace_path = (|| -> Option<PathBuf> {
        if let Some(folders) = &init_params.workspace_folders {
            return folders.get(0)?.uri.to_file_path().ok();
        }
        if let Some(uri) = &init_params.root_uri {
            return uri.to_file_path().ok();
        }
        env::current_dir().ok()
    })();

    let mut state = Server::new(conn.sender.clone(), workspace_path);
    state.run(conn.receiver)?;

    tracing::info!("Leaving main loop");
    Ok(())
}
