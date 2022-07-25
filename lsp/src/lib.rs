mod state;
mod vfs;

use crate::state::{server_capabilities, State};
use anyhow::Result;
use lsp_server::{Connection, Message};

pub fn main_loop(conn: Connection) -> Result<()> {
    let init_params = conn.initialize(serde_json::to_value(&server_capabilities()).unwrap())?;
    eprintln!("Init params: {}", init_params);

    let mut state = State::new(conn.sender.clone());

    eprintln!("Entering main loop");

    for msg in &conn.receiver {
        eprintln!("Got message: {:?}", msg);
        match msg {
            Message::Request(req) => {
                if conn.handle_shutdown(&req)? {
                    break;
                }
                state.dispatch_request(req);
            }
            Message::Notification(notif) => {
                state.dispatch_notification(notif);
            }
            Message::Response(_) => {}
        }
    }

    // TODO: Force shutdown of pendign requests?
    eprintln!("Leaving main loop");

    Ok(())
}
