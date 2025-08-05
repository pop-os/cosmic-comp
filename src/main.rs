// SPDX-License-Identifier: GPL-3.0-only

use calloop::timer::{TimeoutAction, Timer};
use smithay::{
    reexports::{
        calloop::{generic::Generic, EventLoop, Interest, Mode, PostAction},
        wayland_server::{Display, DisplayHandle},
    },
    wayland::socket::ListeningSocketSource,
};

use anyhow::{Context, Result};
use state::{LastRefresh, State};
use std::{
    env,
    ffi::OsString,
    os::unix::process::CommandExt,
    process,
    sync::Arc,
    time::{Duration, Instant},
};
use tracing::{error, info, warn};
use wayland::protocols::overlap_notify::OverlapNotifyState;

use crate::wayland::handlers::compositor::client_compositor_state;

use clap_lex::RawArgs;

use std::error::Error;

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

// called by the Xwayland source, either after starting or failing
impl State {
    fn notify_ready(&mut self) {
        // TODO: Don't notify again, but potentially import updated env-variables
        // into systemd and the session?
        self.ready.call_once(|| {
            // potentially tell systemd we are setup now
            if let state::BackendData::Kms(_) = &self.backend {
                #[cfg(feature = "systemd")]
                systemd::ready(&self.common);
                #[cfg(not(feature = "systemd"))]
                if let Err(err) = dbus::ready(&self.common) {
                    error!(?err, "Failed to update the D-Bus activation environment");
                }
            }

            // potentially tell the session we are setup now
            if let Err(err) =
                session::setup_socket(self.common.event_loop_handle.clone(), &self.common)
            {
                warn!(?err, "Failed to setup cosmic-session communication");
            }

            let mut args = env::args().skip(1);
            self.common.kiosk_child = if let Some(exec) = args.next() {
                // Run command in kiosk mode
                let mut command = process::Command::new(&exec);
                command.args(args);
                command.envs(
                    session::get_env(&self.common).expect("WAYLAND_DISPLAY should be valid UTF-8"),
                );
                unsafe { command.pre_exec(|| Ok(utils::rlimit::restore_nofile_limit())) };

                info!("Running {:?}", exec);
                command
                    .spawn()
                    .map_err(|err| {
                        // TODO: replace with `inspect_err` once stable
                        error!(?err, "Error running kiosk child.");
                        err
                    })
                    .ok()
            } else {
                None
            };
        });
    }
}

fn main() {
    if let Err(err) = main_inner() {
        error!("Error occured in main(): {}", err);
        process::exit(1);
    }
}

fn main_inner() -> Result<(), Box<dyn Error>> {
    let raw_args = RawArgs::from_args();
    let mut cursor = raw_args.cursor();
    let git_hash = option_env!("GIT_HASH").unwrap_or("unknown");

    // Parse the arguments
    while let Some(arg) = raw_args.next_os(&mut cursor) {
        match arg.to_str() {
            Some("--help") | Some("-h") => {
                print_help(env!("CARGO_PKG_VERSION"), git_hash);
                return Ok(());
            }
            Some("--version") | Some("-V") => {
                println!(
                    "cosmic-comp {} (git commit {})",
                    env!("CARGO_PKG_VERSION"),
                    git_hash
                );
                return Ok(());
            }
            _ => {}
        }
    }

    // setup logger
    logger::init_logger()?;
    info!("Cosmic starting up!");

    profiling::register_thread!("Main Thread");
    #[cfg(feature = "profile-with-tracy")]
    tracy_client::Client::start();

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

    if let Err(err) = theme::watch_theme(event_loop.handle()) {
        warn!(?err, "Failed to watch theme");
    }

    // run the event loop
    event_loop.run(None, &mut state, |state| {
        // shall we shut down?
        if state.common.should_stop {
            info!("Shutting down");
            state.common.event_loop_signal.stop();
            state.common.event_loop_signal.wakeup();
            return;
        }

        // trigger routines
        let clients = state.common.shell.write().update_animations();
        {
            let dh = state.common.display_handle.clone();
            for client in clients.values() {
                client_compositor_state(&client).blocker_cleared(state, &dh);
            }
        }

        refresh(state);

        {
            let shell = state.common.shell.read();
            if shell.animations_going() {
                for output in shell.outputs().cloned().collect::<Vec<_>>().into_iter() {
                    state.backend.schedule_render(&output);
                }
            }
        }

        // send out events
        let _ = state.common.display_handle.flush_clients();

        // check if kiosk child is running
        if let Some(child) = state.common.kiosk_child.as_mut() {
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
    if let Some(mut child) = state.common.kiosk_child.take() {
        let _ = child.kill();
    }

    // drop eventloop & state before logger
    std::mem::drop(event_loop);
    std::mem::drop(state);

    Ok(())
}

fn print_help(version: &str, git_rev: &str) {
    println!(
        r#"cosmic-comp {version} (git commit {git_rev})
System76 <info@system76.com>
	    
Designed for the COSMIC™ desktop environment, cosmic-comp is a Wayland Compositor.
	    
Project home page: https://github.com/pop-os/cosmic-comp
	    
Options:
  -h, --help     Show this message
  -v, --version  Show the version of cosmic-comp"#
    );
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
            let client_state = state.new_client_state();
            if let Err(err) = state
                .common
                .display_handle
                .insert_client(client_stream, Arc::new(client_state))
            {
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

fn refresh(state: &mut State) {
    if matches!(state.last_refresh, LastRefresh::Scheduled(_)) {
        return;
    }

    if matches!(state.last_refresh, LastRefresh::At(instant) if Instant::now().duration_since(instant) < Duration::from_millis(150))
    {
        if let Ok(token) = state.common.event_loop_handle.insert_source(
            Timer::from_duration(Duration::from_millis(150)),
            |_, _, state| {
                state.last_refresh = LastRefresh::None;
                TimeoutAction::Drop
            },
        ) {
            state.last_refresh = LastRefresh::Scheduled(token);
            return;
        } else {
            warn!("Failed to schedule refresh");
        }
    }

    state.common.refresh();
    state::Common::refresh_focus(state);
    OverlapNotifyState::refresh(state);
    state.common.update_x11_stacking_order();
    state.last_refresh = LastRefresh::At(Instant::now());
}
