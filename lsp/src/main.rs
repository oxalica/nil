use anyhow::Result;
use lsp_server::Connection;

fn main() -> Result<()> {
    env_logger::Builder::from_env("NIL_LOG").init();
    let (conn, io_threads) = Connection::stdio();
    lsp::main_loop(conn)?;
    io_threads.join()?;
    Ok(())
}
