use anyhow::Result;
use lsp_server::Connection;
use std::env;

fn main() -> Result<()> {
    if env::var("RUST_BACKTRACE").is_err() {
        env::set_var("RUST_BACKTRACE", "short");
    }

    env_logger::Builder::from_env("NIL_LOG").init();

    let (conn, io_threads) = Connection::stdio();
    lsp::main_loop(conn)?;
    io_threads.join()?;
    Ok(())
}
