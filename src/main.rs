// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    reexports::{
        calloop::{generic::Generic, EventLoop, Interest, Mode, PostAction},
        wayland_server::{Display, DisplayHandle},
    },
    wayland::socket::ListeningSocketSource,
};

use anyhow::{Context, Result};
use std::{ffi::OsString, sync::Arc};
use tracing::{error, info, warn};

use crate::wayland::handlers::compositor::client_compositor_state;

pub mod backend;
pub mod config;
#[cfg(feature = "debug")]
pub mod debug;
pub mod input;
mod logger;
pub mod session;
pub mod shell;
pub mod state;
#[cfg(feature = "systemd")]
pub mod systemd;
pub mod utils;
pub mod wayland;
pub mod xwayland;

fn main() -> Result<()> {
    // setup logger
    logger::init_logger()?;
    info!("Cosmic starting up!");

    // init event loop
    let mut event_loop = EventLoop::try_new().with_context(|| "Failed to initialize event loop")?;
    // init wayland
    let (display, socket) = init_wayland_display(&mut event_loop)?;
    // init state
    let mut state = state::State::new(
        &display,
        socket,
        event_loop.handle(),
        event_loop.get_signal(),
    );
    // init backend
    backend::init_backend_auto(&display, &mut event_loop, &mut state)?;
    // potentially tell systemd we are setup now
    #[cfg(feature = "systemd")]
    if let state::BackendData::Kms(_) = &state.backend {
        systemd::ready(&state);
    }
    // potentially tell the session we are setup now
    session::setup_socket(event_loop.handle(), &state)?;

    // run the event loop
    event_loop.run(None, &mut state, |state| {
        // shall we shut down?
        if state.common.shell.outputs().next().is_none() || state.common.should_stop {
            info!("Shutting down");
            state.common.event_loop_signal.stop();
            state.common.event_loop_signal.wakeup();
            return;
        }

        // trigger routines
        let clients = state.common.shell.update_animations();
        {
            let dh = state.common.display_handle.clone();
            for client in clients.values() {
                client_compositor_state(&client).blocker_cleared(state, &dh);
            }
        }
        state.common.shell.refresh();
        state::Common::refresh_focus(state);

        // send out events
        let _ = state.common.display_handle.flush_clients();
    })?;

    // drop eventloop & state before logger
    std::mem::drop(event_loop);
    std::mem::drop(state);

    Ok(())
}

fn init_wayland_display(
    event_loop: &mut EventLoop<state::State>,
) -> Result<(DisplayHandle, OsString)> {
    let display = Display::new().unwrap();
    let handle = display.handle();

    let source = ListeningSocketSource::new_auto().unwrap();
    let socket_name = source.socket_name().to_os_string();
    info!("Listening on {:?}", socket_name);

    event_loop
        .handle()
        .insert_source(source, |client_stream, _, state| {
            if let Err(err) = state.common.display_handle.insert_client(
                client_stream,
                Arc::new(if cfg!(debug_assertions) {
                    state.new_privileged_client_state()
                } else {
                    state.new_client_state()
                }),
            ) {
                warn!(?err, "Error adding wayland client");
            };
        })
        .with_context(|| "Failed to init the wayland socket source.")?;
    event_loop
        .handle()
        .insert_source(
            Generic::new(display, Interest::READ, Mode::Level),
            move |_, display, state| {
                // SAFETY: We don't drop the display
                match unsafe { display.get_mut().dispatch_clients(state) } {
                    Ok(_) => Ok(PostAction::Continue),
                    Err(err) => {
                        error!(?err, "I/O error on the Wayland display");
                        state.common.should_stop = true;
                        Err(err)
                    }
                }
            },
        )
        .with_context(|| "Failed to init the wayland event source.")?;

    Ok((handle, socket_name))
}
