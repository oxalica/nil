mod capabilities;
mod convert;
mod handler;
mod semantic_tokens;
mod server;
mod vfs;

use lsp_server::{Connection, ErrorCode};
use lsp_types::InitializeParams;
use std::fmt;

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

    let mut server = Server::new(conn.sender.clone());
    server.run(conn.receiver, init_params)?;

    tracing::info!("Leaving main loop");
    Ok(())
}
