// SPDX-License-Identifier: GPL-3.0-only

use smithay::reexports::calloop::{Interest, LoopHandle, Mode, PostAction, generic::Generic};

use anyhow::{Context, Result, anyhow};
use std::collections::HashMap;
use tracing::{error, info, warn};

use crate::state::{Common, State};
use cosmic_comp_ipc::Message;

pub fn get_env(common: &Common) -> Result<HashMap<String, String>> {
    let mut env = HashMap::new();
    env.insert(
        String::from("WAYLAND_DISPLAY"),
        common
            .socket
            .clone()
            .into_string()
            .map_err(|_| anyhow!("wayland socket is no valid utf-8 string?"))?,
    );
    if let Some(display) = common.xwayland_state.as_ref().map(|s| s.display) {
        env.insert(String::from("DISPLAY"), format!(":{}", display));
    }
    Ok(env)
}

pub fn run_socket(handle: LoopHandle<State>, common: &mut Common) -> Result<()> {
    let Some(mut socket) = common.session_sock.take() else {
        return Ok(());
    };
    info!("Found cosmic-comp ipc socket");

    let env = get_env(common)?;
    let message = Message::SetEnv { variables: env };
    cosmic_comp_ipc::write_msg_blocking(&mut socket, &message)
        .context("Failed to send session ipc message")?;

    handle
        .insert_source(
            Generic::new(socket, Interest::READ, Mode::Level),
            move |_, stream, _state| {
                // SAFETY: We don't drop the stream!
                let stream = unsafe { stream.get_mut() };

                match cosmic_comp_ipc::read_msg_blocking(stream) {
                    Ok(msg) => match msg {
                        Message::SetEnv { .. } => {
                            warn!("Got SetEnv from session? What is this?");
                            Ok(PostAction::Continue)
                        }
                    },
                    Err(cosmic_comp_ipc::Error::IO(err)) => {
                        error!(?err, "Error reading from session socket");
                        Ok(PostAction::Remove)
                    }
                    Err(err) => {
                        warn!(?err, "Invalid message from session sock");
                        Ok(PostAction::Continue)
                    }
                }
            },
        )
        .with_context(|| "Failed to init the cosmic session socket source")?;

    Ok(())
}
