mod convert;
mod handler;
mod state;
mod vfs;

use lsp_server::Connection;
use lsp_types::InitializeParams;
use std::env;
use std::path::PathBuf;

pub(crate) use state::{State, StateSnapshot};
pub(crate) use vfs::{LineMap, Vfs};

pub type Error = Box<dyn std::error::Error + Send + Sync>;
pub type Result<T, E = Error> = std::result::Result<T, E>;

pub fn main_loop(conn: Connection) -> Result<()> {
    let init_params =
        conn.initialize(serde_json::to_value(&handler::server_capabilities()).unwrap())?;
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

    let mut state = State::new(conn.sender.clone(), workspace_path);
    state.run(conn.receiver)?;

    tracing::info!("Leaving main loop");
    Ok(())
}
