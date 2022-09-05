use anyhow::Result;
use lsp_server::Connection;
use std::path::PathBuf;
use std::sync::Arc;
use std::{env, fs, io};
use tracing_subscriber::fmt::writer::BoxMakeWriter;
use tracing_subscriber::EnvFilter;

const LOG_FILTER_ENV: &str = "NIL_LOG";
const LOG_PATH_ENV: &str = "NIL_LOG_PATH";
const BACKTRACE_ENV: &str = "RUST_BACKTRACE";

fn main() -> Result<()> {
    if env::var(BACKTRACE_ENV).is_err() {
        env::set_var(BACKTRACE_ENV, "short");
    }

    setup_logger();

    let (conn, io_threads) = Connection::stdio();
    nil::main_loop(conn)?;
    io_threads.join()?;
    Ok(())
}

fn setup_logger() {
    let file = env::var_os(LOG_PATH_ENV).and_then(|path| {
        let path = PathBuf::from(path);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(&parent).ok()?;
        }
        fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .ok()
    });

    let writer = match file {
        Some(file) => BoxMakeWriter::new(Arc::new(file)),
        None => BoxMakeWriter::new(io::stderr),
    };

    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_env(LOG_FILTER_ENV))
        .with_writer(writer)
        .with_ansi(false)
        .init();
}
