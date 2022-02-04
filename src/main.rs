// SPDX-License-Identifier: GPL-3.0-only

use smithay::reexports::{
    calloop::{generic::Generic, EventLoop, Interest, Mode, PostAction},
    wayland_server::Display,
};

use anyhow::{Context, Result};
use std::sync::atomic::Ordering;

pub mod backend;
pub mod input;
pub mod shell;
pub mod state;
mod logger;
pub mod utils;

#[cfg(feature = "debug")]
pub mod debug;

fn main() -> Result<()> {
    // setup logger
    let log = logger::init_logger()?;
    slog_scope::info!("Cosmic starting up!");

    // init event loop
    let mut event_loop = EventLoop::try_new().with_context(|| "Failed to initialize event loop")?;
    // init wayland
    let display = init_wayland_display(&mut event_loop)?;
    // init state
    let mut state = state::State::new(display, event_loop.handle(), log);
    // init backend
    backend::init_backend_auto(&mut event_loop, &mut state)?;

    // run the event loop
    let signal = event_loop.get_signal();
    event_loop.run(None, &mut state, |state| {
        // shall we shut down?
        if state.common.spaces.outputs().next().is_none() || state.common.should_stop {
            slog_scope::info!("Shutting down");
            signal.stop();
            signal.wakeup();
            return;
        }

        // do we need to trigger another render
        if state.common.dirty_flag.swap(false, Ordering::SeqCst) {
            for output in state.common.spaces.outputs() {
                state.backend.schedule_render(output)
            }
        }

        // send out events
        let display = state.common.display.clone();
        display.borrow_mut().flush_clients(state);
        // trigger routines
        state.common.spaces.refresh();
    })?;

    Ok(())
}

fn init_wayland_display(event_loop: &mut EventLoop<state::State>) -> Result<Display> {
    let mut display = Display::new();
    let socket_name = display.add_socket_auto()?;

    slog_scope::info!("Listening on {:?}", socket_name);
    event_loop
        .handle()
        .insert_source(
            Generic::from_fd(display.get_poll_fd(), Interest::READ, Mode::Level),
            move |_, _, state: &mut state::State| {
                let display = state.common.display.clone();
                let mut display = display.borrow_mut();
                match display.dispatch(std::time::Duration::from_millis(0), state) {
                    Ok(_) => Ok(PostAction::Continue),
                    Err(e) => {
                        slog_scope::error!("I/O error on the Wayland display: {}", e);
                        state.common.should_stop = true;
                        Err(e)
                    }
                }
            },
        )
        .with_context(|| "Failed to init the wayland event source.")?;

    Ok(display)
}
