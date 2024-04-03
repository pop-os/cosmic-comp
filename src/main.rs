// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    reexports::{
        calloop::{generic::Generic, EventLoop, Interest, Mode, PostAction},
        wayland_server::{Display, DisplayHandle},
    },
    wayland::socket::ListeningSocketSource,
};

use anyhow::{Context, Result};
use std::{env, ffi::OsString, os::unix::process::CommandExt, process, sync::Arc};
use tracing::{error, info, warn};

use crate::{
    state::BackendData, utils::prelude::SeatExt,
    wayland::handlers::compositor::client_compositor_state,
};

pub mod backend;
pub mod config;
pub mod dbus;
#[cfg(feature = "debug")]
pub mod debug;
pub mod input;
mod logger;
pub mod session;
pub mod shell;
pub mod state;
#[cfg(feature = "systemd")]
pub mod systemd;
pub mod theme;
pub mod utils;
pub mod wayland;
pub mod xwayland;

#[cfg(feature = "profile-with-tracy")]
#[global_allocator]
static GLOBAL: profiling::tracy_client::ProfiledAllocator<std::alloc::System> =
    profiling::tracy_client::ProfiledAllocator::new(std::alloc::System, 10);

fn main() -> Result<()> {
    // setup logger
    logger::init_logger()?;
    info!("Cosmic starting up!");

    utils::rlimit::increase_nofile_limit();

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

    let mut args = env::args().skip(1);
    let mut child_opt = if let Some(exec) = args.next() {
        // Run command in kiosk mode
        let mut command = process::Command::new(&exec);
        command.args(args);
        command.envs(session::get_env(&state)?);
        unsafe { command.pre_exec(|| Ok(utils::rlimit::restore_nofile_limit())) };

        info!("Running {:?}", exec);
        Some(command.spawn()?)
    } else {
        None
    };

    if let Err(err) = theme::watch_theme(event_loop.handle()) {
        warn!(?err, "Failed to watch theme");
    }

    #[cfg(feature = "profile-with-tracy")]
    profiling::tracy_client::Client::start();

    profiling::register_thread!("Main Thread");

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
        state.common.update_x11_stacking_order();

        if state.common.shell.animations_going() {
            for output in state
                .common
                .shell
                .outputs()
                .cloned()
                .collect::<Vec<_>>()
                .into_iter()
            {
                state
                    .backend
                    .schedule_render(&state.common.event_loop_handle, &output);
            }
        }

        // send out events
        let _ = state.common.display_handle.flush_clients();

        // check if kiosk child is running
        if let Some(ref mut child) = child_opt {
            match child.try_wait() {
                // Kiosk child exited with status
                Ok(Some(exit_status)) => {
                    info!("Command exited with status {:?}", exit_status);
                    match exit_status.code() {
                        // Exiting with the same status as the kiosk child
                        Some(code) => process::exit(code),
                        // The kiosk child exited with signal, exiting with error
                        None => process::exit(1),
                    }
                }
                // Command still running
                Ok(None) => {}
                // Kiosk child disappeared, exiting with error
                Err(err) => {
                    warn!(?err, "Failed to wait for command");
                    process::exit(1);
                }
            }
        }
    })?;

    // kill kiosk child if loop exited
    if let Some(mut child) = child_opt {
        let _ = child.kill();
    }

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
            let node = match &state.backend {
                BackendData::Kms(kms_state) if kms_state.auto_assign => kms_state
                    .target_node_for_output(&state.common.last_active_seat().active_output()),
                _ => None,
            };

            if let Err(err) = state.common.display_handle.insert_client(
                client_stream,
                Arc::new(if cfg!(debug_assertions) {
                    state.new_privileged_client_state()
                } else if let Some(node) = node {
                    state.new_client_state_with_node(node)
                } else {
                    state.new_client_state()
                }),
            ) {
                warn!(?err, "Error adding wayland client")
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
