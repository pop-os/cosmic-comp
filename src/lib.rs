// SPDX-License-Identifier: GPL-3.0-only

#![allow(
    clippy::too_many_arguments,
    clippy::type_complexity,
    clippy::len_without_is_empty,
    clippy::collapsible_match,
    clippy::large_enum_variant,
    clippy::wrong_self_convention,
    clippy::mutable_key_type,
    clippy::doc_lazy_continuation,
    clippy::single_match,
    clippy::unnecessary_get_then_check,
    clippy::needless_range_loop,
    clippy::missing_safety_doc,
    clippy::match_like_matches_macro,
    clippy::field_reassign_with_default
)]

use calloop::timer::{TimeoutAction, Timer};
use smithay::{
    reexports::{
        calloop::{EventLoop, Interest, Mode, PostAction, generic::Generic},
        wayland_server::{Display, DisplayHandle},
    },
    wayland::socket::ListeningSocketSource,
};

use anyhow::{Context, Result};
use state::{BackendData, LastRefresh, State};
use std::{
    env,
    ffi::OsString,
    os::unix::process::CommandExt,
    process,
    sync::Arc,
    time::{Duration, Instant},
};
use tracing::{debug, error, info, warn};
use wayland::protocols::{
    keyboard_layout::KeyboardLayoutState, layer_usable_area::UsableAreaState,
    overlap_notify::OverlapNotifyState,
};

use crate::wayland::handlers::compositor::client_compositor_state;

use clap_lex::RawArgs;

#[cfg(feature = "heap-profile")]
#[global_allocator]
static ALLOC: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

use std::error::Error;

pub mod backend;
pub mod clipboard;
pub mod comp_theme;
pub mod config;
pub mod dbus;
#[cfg(feature = "debug")]
pub mod debug;
pub mod hooks;
pub mod input;
mod logger;
pub mod perf;
pub mod session;
pub mod shell;
pub mod state;
#[cfg(feature = "systemd")]
pub mod systemd;
pub mod theme;
pub mod toolkit_config;
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
                if let Err(err) = dbus::ready(&self.common) {
                    error!(?err, "Failed to update the D-Bus activation environment");
                }
            }

            // potentially tell the session we are setup now
            if let Err(err) =
                session::run_socket(self.common.event_loop_handle.clone(), &self.common)
            {
                warn!(?err, "Failed to setup cosmic-session communication");
            }

            self.common.kiosk_child = if let Some(mut command) = self.kiosk_command.take() {
                // Run command in kiosk mode
                command.envs(
                    session::get_env(&self.common).expect("WAYLAND_DISPLAY should be valid UTF-8"),
                );
                unsafe {
                    command.pre_exec(|| {
                        utils::rlimit::restore_nofile_limit();
                        Ok(())
                    })
                };

                info!("Running {:?}", command.get_program());
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

pub fn run(hooks: crate::hooks::Hooks) -> Result<(), Box<dyn Error>> {
    utils::timing::init();
    let raw_args = RawArgs::from_args();
    let mut cursor = raw_args.cursor();
    raw_args.next_os(&mut cursor);
    let git_hash = option_env!("GIT_HASH").unwrap_or("unknown");

    let mut kiosk_command = None;
    let mut with_xwayland = true;
    // Parse the arguments
    while let Some(arg) = raw_args.next_os(&mut cursor) {
        match arg.to_str() {
            Some("--help") | Some("-h") => {
                print_help(env!("CARGO_PKG_VERSION"), git_hash);
                return Ok(());
            }
            Some("--no-xwayland") => {
                tracing::info!("Running without Xwayland");
                with_xwayland = false;
            }
            Some("--version") | Some("-V") => {
                println!(
                    "cosmic-comp {} (git commit {})",
                    env!("CARGO_PKG_VERSION"),
                    git_hash
                );
                return Ok(());
            }
            _ => {
                let mut cmd = process::Command::new(arg);
                cmd.args(raw_args.remaining(&mut cursor));
                kiosk_command = Some(cmd);
            }
        }
    }

    // setup logger
    logger::init_logger()?;
    info!("Cosmic starting up!");
    utils::timing::mark("process-start");

    // This thread runs the event loop. Recorded before any iced element exists,
    // so ProgramLoop can tell inline-safe calls from ones that must be deferred.
    utils::iced::mark_main_thread();

    // Off by default; reports parking_lot lock cycles (e.g. a nested shell
    // read() racing a writer) that would otherwise hang the compositor silently.
    utils::deadlock::spawn_watchdog();

    // Periodic RSS/swap/heap self-report; on its own thread so a stalled
    // event loop still reports. See utils::memlog for the env knobs.
    utils::memlog::spawn();

    profiling::register_thread!("Main Thread");
    #[cfg(feature = "profile-with-tracy")]
    tracy_client::Client::start();

    utils::rlimit::increase_nofile_limit();
    // This needs to be done before any potential program launches
    // (e.g. Xwayland) as it handles passed file descriptors.
    if let Err(err) = session::setup_socket() {
        warn!("Session error: {:?}", err);
    };

    // init hook globals
    hooks::HOOKS.set(hooks)
        .expect("Hooks global has already been initialized. Running multiple instances of COSMIC in one process is not supported.");

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
        with_xwayland,
        kiosk_command,
    );
    // init backend
    backend::init_backend_auto(&display, &mut event_loop, &mut state)?;
    utils::timing::mark("backend-ready");

    if let Err(err) = theme::watch_theme(event_loop.handle()) {
        warn!(?err, "Failed to watch theme");
    }

    if let Err(err) = install_termination_handler(event_loop.handle()) {
        warn!(?err, "Failed to install termination handler");
    }

    // run the event loop
    event_loop.run(None, &mut state, |state| {
        // Only track main-loop health during an active capture window (a single
        // relaxed atomic load otherwise — no steady-state cost).
        let loop_work_start = crate::perf::is_capturing().then(|| state.common.loop_health.begin());
        // shall we shut down?
        if state.common.should_stop {
            info!("Shutting down");
            utils::timing::mark("shutdown-begin");
            state.common.event_loop_signal.stop();
            state.common.event_loop_signal.wakeup();
            return;
        }

        // trigger routines
        // Check for pending will_stop timeout
        {
            use crate::wayland::protocols::voice_mode::VoiceModeHandler;
            state.check_pending_stop_timeout();
            state.check_frozen_timeout();
        }

        // Poll audio levels from shared memory
        let (clients, transition_completed, edge_resize_active) = {
            let mut shell = state.common.shell.write();
            shell.voice_orb_state.poll_shmem_audio_levels();
            let transition_completed = shell.voice_orb_state.transition_just_completed;
            // Captured before update_animations: if a side-panel resize spring or its
            // post-resize settle is in flight, the panel edge may come to rest under a
            // stationary pointer, so re-evaluate the hover sash/cursor below.
            let edge_resize_active =
                shell.active_layer_resize_anim.is_some() || shell.layer_resize_settle.is_some();
            (
                shell.update_animations(),
                transition_completed,
                edge_resize_active,
            )
        };

        // Re-evaluate the side-panel edge hover after a resize tick: the edge can
        // settle under a stationary pointer with no motion event, so the sash + EW
        // cursor would otherwise not reappear until the next pointer move.
        if edge_resize_active {
            let seat = state.common.shell.read().seats.last_active().clone();
            state.update_edge_resize_hover(&seat);
        }

        // Sync protocol state when attach_and_transition animation completes
        if transition_completed {
            use crate::wayland::protocols::voice_mode::OrbState;
            state
                .common
                .voice_mode_state
                .set_orb_state(OrbState::Hidden);
            state
                .common
                .shell
                .write()
                .voice_orb_state
                .close_shmem_reader();
        }

        {
            let dh = state.common.display_handle.clone();
            for client in clients.values() {
                client_compositor_state(client).blocker_cleared(state, &dh);
            }
        }

        refresh(state);

        let loop_active = {
            let shell = state.common.shell.read();
            // Only outputs hosting an animation need continuous redraws; a
            // layer slide on one output must not keep other outputs rendering.
            let mut active = false;
            for output in shell.animating_outputs() {
                active = true;
                state.backend.schedule_render(&output);
            }
            active
        };

        // send out events
        let _ = state.common.display_handle.flush_clients();

        // check if kiosk child is running
        if let Some(child) = state.common.kiosk_child.as_mut() {
            match child.try_wait() {
                // Kiosk child exited with status
                Ok(Some(exit_status)) => {
                    info!("Command exited with status {:?}", exit_status);
                    crate::utils::timing::mark("kiosk-child-exited");
                    // Stop cleanly so surface threads are joined before exit() (signal -> 1).
                    state.common.kiosk_exit_code = Some(exit_status.code().unwrap_or(1));
                    state.common.should_stop = true;
                }
                // Command still running
                Ok(None) => {}
                // Kiosk child disappeared, exiting with error
                Err(err) => {
                    warn!(?err, "Failed to wait for command");
                    state.common.kiosk_exit_code = Some(1);
                    state.common.should_stop = true;
                }
            }
        }

        if let Some(work_start) = loop_work_start {
            state.common.loop_health.end(work_start, loop_active);
        }
    })?;

    // kill kiosk child if loop exited
    if let Some(mut child) = state.common.kiosk_child.take() {
        let _ = child.kill();
    }

    let kiosk_exit_code = state.common.kiosk_exit_code;

    // Join surface threads before exit() so no thread is mid-eglCreateSync when
    // Mesa's atexit handlers run and corrupt the heap (issue #2375). Safe here
    // because the event loop has stopped; an unconditional join in Surface::Drop
    // would instead deadlock against apply_config_for_outputs.
    if let BackendData::Kms(kms) = &mut state.backend {
        // Release master first so the surface drop path skips its blocking commit.
        for device in kms.drm_devices.values_mut() {
            device.drm.pause();
        }

        // Keep the last frame scanning out after we exit, so the next compositor can adopt
        // it. Ordering is load-bearing: AFTER pause() (buffer can no longer change) and
        // BEFORE the drain (which drops each DrmOutput, removing it from the manager map).
        if freeze_on_exit_enabled() {
            for device in kms.drm_devices.values_mut() {
                let mut drm = device.drm.lock();
                let compositors = drm.compositors();
                debug!(
                    count = compositors.len(),
                    "freeze-on-exit: freezing scanout"
                );
                for compositor in compositors.values() {
                    compositor.lock().unwrap().freeze_scanout();
                }
            }
            utils::timing::mark("freeze-complete");
        }

        for device in kms.drm_devices.values_mut() {
            for (_, surface) in device.inner.surfaces.drain() {
                surface.drop_and_join();
            }
        }
        utils::timing::mark("surfaces-joined");
    }

    // drop eventloop & state before logger
    std::mem::drop(event_loop);
    std::mem::drop(state);
    utils::timing::mark("process-exit");

    if let Some(code) = kiosk_exit_code {
        process::exit(code);
    }

    Ok(())
}

fn print_help(version: &str, git_rev: &str) {
    println!(
        r#"cosmic-comp {version} (git commit {git_rev})
System76 <info@system76.com>

Designed for the COSMIC™ desktop environment, cosmic-comp is a Wayland Compositor.

Project home page: https://github.com/pop-os/cosmic-comp

Options:
  -h, --help          Show this message
  --no-xwayland       Run without Xwayland
  -v, --version       Show the version of cosmic-comp"#
    );
}

/// Write end of the signal self-pipe; -1 until installed.
static SIGNAL_PIPE_WRITE: std::sync::atomic::AtomicI32 = std::sync::atomic::AtomicI32::new(-1);

extern "C" fn handle_termination_signal(sig: libc::c_int) {
    let fd = SIGNAL_PIPE_WRITE.load(std::sync::atomic::Ordering::Relaxed);
    if fd >= 0 {
        // write(2) is async-signal-safe; a full pipe just means a wakeup is pending. The
        // byte carries the signal number.
        unsafe { libc::write(fd, [sig as u8].as_ptr() as *const _, 1) };
    }
}

/// Turn SIGTERM/SIGINT/SIGHUP into a graceful shutdown instead of instant death.
///
/// As a child of cosmic-session we are ended by signal, not by the kiosk child exiting;
/// the default disposition skips shutdown entirely (threads never join, no scanout freeze).
/// Routed through a self-pipe — the only thing a handler may safely touch.
fn install_termination_handler(handle: calloop::LoopHandle<'static, state::State>) -> Result<()> {
    use std::os::unix::io::{AsRawFd, FromRawFd, OwnedFd};

    let mut fds = [0 as libc::c_int; 2];
    // CLOEXEC so the pipe doesn't leak into the kiosk child or Xwayland.
    if unsafe { libc::pipe2(fds.as_mut_ptr(), libc::O_CLOEXEC | libc::O_NONBLOCK) } != 0 {
        return Err(std::io::Error::last_os_error()).context("Failed to create signal pipe");
    }
    let (read_fd, write_fd) =
        unsafe { (OwnedFd::from_raw_fd(fds[0]), OwnedFd::from_raw_fd(fds[1])) };

    SIGNAL_PIPE_WRITE.store(write_fd.as_raw_fd(), std::sync::atomic::Ordering::Relaxed);
    // Leaked deliberately: the handler may fire until process exit.
    std::mem::forget(write_fd);

    for sig in [libc::SIGTERM, libc::SIGINT, libc::SIGHUP] {
        let handler = handle_termination_signal as *const () as libc::sighandler_t;
        if unsafe { libc::signal(sig, handler) } == libc::SIG_ERR {
            return Err(std::io::Error::last_os_error())
                .context("Failed to install signal handler");
        }
    }

    handle
        .insert_source(
            Generic::new(read_fd, Interest::READ, Mode::Level),
            |_, fd, state: &mut state::State| {
                // Drain so a level-triggered source doesn't spin.
                let mut buf = [0u8; 16];
                let mut last = 0u8;
                loop {
                    let n = unsafe {
                        libc::read(fd.as_raw_fd(), buf.as_mut_ptr() as *mut _, buf.len())
                    };
                    if n <= 0 {
                        break;
                    }
                    last = buf[n as usize - 1];
                }
                if !state.common.should_stop {
                    let name = match libc::c_int::from(last) {
                        libc::SIGTERM => "SIGTERM",
                        libc::SIGINT => "SIGINT",
                        libc::SIGHUP => "SIGHUP",
                        _ => "signal",
                    };
                    info!("Received {name}, shutting down");
                    state.common.should_stop = true;
                }
                Ok(PostAction::Continue)
            },
        )
        .map_err(|err| anyhow::anyhow!("Failed to insert signal source: {err}"))?;

    Ok(())
}

/// Session-handoff freeze (exit CLOSEFB + the frame hold). On by default; set
/// `COSMIC_FREEZE_SCANOUT_ON_EXIT=0` to fall back to the plain blank-on-exit.
pub(crate) fn freeze_on_exit_enabled() -> bool {
    !matches!(
        std::env::var("COSMIC_FREEZE_SCANOUT_ON_EXIT").as_deref(),
        Ok("0") | Ok("false") | Ok("no") | Ok("off")
    )
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
            match state
                .common
                .display_handle
                .insert_client(client_stream, Arc::new(client_state))
            {
                Ok(client) => state.coldstart_notify_client_connected(&client),
                Err(err) => warn!(?err, "Error adding wayland client"),
            }
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
    // Run the loop-bound work iced elements deferred to this thread: idle
    // callbacks parked by programs running on a render thread, and the teardown
    // of elements dropped there (see drain_deferred_loop_work). This is a
    // post-dispatch point, so touching the loop is safe here; do it
    // unconditionally, before the refresh throttle below can early-return.
    crate::utils::iced::drain_deferred_loop_work(&state.common.event_loop_handle);

    // MERGE: dropped `crate::backend::render::blur::reap_dead_blur_textures()` —
    // our blur backdrop cache is replaced by upstream's framebuffer-capture blur
    // (src/backend/render/wayland/blur_effect.rs), which owns no per-window textures.

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
    state.refresh_game_mode_state();
    OverlapNotifyState::refresh(state);
    UsableAreaState::refresh(state);
    state.common.update_x11_stacking_order();
    KeyboardLayoutState::refresh(state);
    state.last_refresh = LastRefresh::At(Instant::now());
}
