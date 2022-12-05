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

pub mod backend;
pub mod config;
pub mod input;
mod logger;
pub mod session;
pub mod shell;
pub mod state;
pub mod systemd;
pub mod utils;
pub mod wayland;

#[cfg(feature = "debug")]
pub mod debug;

fn main() -> Result<()> {
    // setup logger
    let _log = logger::init_logger()?;
    slog_scope::info!("Cosmic starting up!");

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
            slog_scope::info!("Shutting down");
            data.state.common.event_loop_signal.stop();
            data.state.common.event_loop_signal.wakeup();
            return;
        }

        // trigger routines
        data.state.common.shell.refresh();
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

    let source = ListeningSocketSource::new_auto(None).unwrap();
    let socket_name = source.socket_name().to_os_string();
    slog_scope::info!("Listening on {:?}", socket_name);

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
                slog_scope::warn!("Error adding wayland client: {}", err);
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
                Err(e) => {
                    slog_scope::error!("I/O error on the Wayland display: {}", e);
                    data.state.common.should_stop = true;
                    Err(e)
                }
            },
        )
        .with_context(|| "Failed to init the wayland event source.")?;

    Ok((display, socket_name))
}
