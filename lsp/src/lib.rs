mod convert;
mod handler;
mod state;
mod vfs;

pub(crate) use state::{State, StateSnapshot};
pub(crate) use vfs::{LineMap, Vfs, VfsPath};

use anyhow::Result;
use lsp_server::Connection;

pub fn main_loop(conn: Connection) -> Result<()> {
    let init_params =
        conn.initialize(serde_json::to_value(&handler::server_capabilities()).unwrap())?;
    log::info!("Init params: {}", init_params);

    let mut state = State::new(conn.sender.clone());
    state.run(conn.receiver)?;

    log::info!("Leaving main loop");
    Ok(())
}
