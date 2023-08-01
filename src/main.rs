// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    reexports::{
        calloop::{generic::Generic, EventLoop, Interest, Mode, PostAction},
        wayland_server::Display,
    },
    wayland::socket::ListeningSocketSource,
};

use anyhow::{Context, Result};
use std::{ffi::OsString, os::unix::prelude::AsRawFd, sync::Arc};
use tracing::{error, info, warn};

use crate::wayland::{
    handlers::{compositor::client_compositor_state, screencopy::PendingScreencopyBuffers},
    protocols::screencopy::SessionType,
};

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
    let mut event_loop =
        EventLoop::try_new_high_precision().with_context(|| "Failed to initialize event loop")?;
    // init wayland
    let (display, socket) = init_wayland_display(&mut event_loop)?;
    // init state
    let mut state = state::State::new(
        &display.handle(),
        socket,
        event_loop.handle(),
        event_loop.get_signal(),
    );
    // init backend
    backend::init_backend_auto(&display.handle(), &mut event_loop, &mut state)?;
    // potentially tell systemd we are setup now
    #[cfg(feature = "systemd")]
    if let state::BackendData::Kms(_) = &state.backend {
        systemd::ready(&state);
    }
    // potentially tell the session we are setup now
    session::setup_socket(event_loop.handle(), &state)?;

    let mut data = state::Data { display, state };
    // run the event loop
    event_loop.run(None, &mut data, |data| {
        // shall we shut down?
        if data.state.common.shell.outputs().next().is_none() || data.state.common.should_stop {
            info!("Shutting down");
            data.state.common.event_loop_signal.stop();
            data.state.common.event_loop_signal.wakeup();
            return;
        }

        // trigger routines
        let clients = data.state.common.shell.update_animations();
        {
            let dh = data.display.handle();
            for client in clients.values() {
                client_compositor_state(&client).blocker_cleared(&mut data.state, &dh);
            }
        }
        data.state.common.shell.refresh();
        if data.state.common.shell.animations_going() {
            for output in data.state.common.shell.outputs() {
                let mut scheduled_sessions = None;
                if let Some(sessions) = output.user_data().get::<PendingScreencopyBuffers>() {
                    scheduled_sessions
                        .get_or_insert_with(Vec::new)
                        .extend(sessions.borrow_mut().drain(..));
                }

                data.state.backend.schedule_render(
                    &data.state.common.event_loop_handle,
                    &output,
                    scheduled_sessions.as_ref().map(|sessions| {
                        sessions
                            .iter()
                            .filter(|(s, _)| match s.session_type() {
                                SessionType::Output(o) | SessionType::Workspace(o, _)
                                    if &o == output =>
                                {
                                    true
                                }
                                _ => false,
                            })
                            .cloned()
                            .collect::<Vec<_>>()
                    }),
                );
            }
        }
        state::Common::refresh_focus(&mut data.state);

        // send out events
        let _ = data.display.flush_clients();
    })?;

    // drop eventloop & state before logger
    std::mem::drop(event_loop);
    std::mem::drop(data);

    Ok(())
}

fn init_wayland_display(
    event_loop: &mut EventLoop<state::Data>,
) -> Result<(Display<state::State>, OsString)> {
    let mut display = Display::new().unwrap();

    let source = ListeningSocketSource::new_auto().unwrap();
    let socket_name = source.socket_name().to_os_string();
    info!("Listening on {:?}", socket_name);

    event_loop
        .handle()
        .insert_source(source, |client_stream, _, data| {
            if let Err(err) = data.display.handle().insert_client(
                client_stream,
                Arc::new(if cfg!(debug_assertions) {
                    data.state.new_privileged_client_state()
                } else {
                    data.state.new_client_state()
                }),
            ) {
                warn!(?err, "Error adding wayland client");
            };
        })
        .with_context(|| "Failed to init the wayland socket source.")?;
    event_loop
        .handle()
        .insert_source(
            Generic::new(
                display.backend().poll_fd().as_raw_fd(),
                Interest::READ,
                Mode::Level,
            ),
            move |_, _, data: &mut state::Data| match data.display.dispatch_clients(&mut data.state)
            {
                Ok(_) => Ok(PostAction::Continue),
                Err(err) => {
                    error!(?err, "I/O error on the Wayland display");
                    data.state.common.should_stop = true;
                    Err(err)
                }
            },
        )
        .with_context(|| "Failed to init the wayland event source.")?;

    Ok((display, socket_name))
}
