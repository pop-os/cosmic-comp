// SPDX-License-Identifier: GPL-3.0-only

use smithay::reexports::{
    calloop::{generic::Generic, EventLoop, Interest, Mode, PostAction},
    wayland_server::Display,
};

use anyhow::{Context, Result};
use slog::Drain;

pub mod state;

fn main() -> Result<()> {
    // setup logger
    let decorator = slog_term::TermDecorator::new().stderr().build();
    // usually we would not want to use a Mutex here, but this is usefull for a prototype,
    // to make sure we do not miss any in-flight messages, when we crash.
    let logger = slog::Logger::root(
        std::sync::Mutex::new(
            slog_term::CompactFormat::new(decorator)
                .build()
                .ignore_res(),
        )
        .fuse(),
        slog::o!(),
    );
    let _guard = slog_scope::set_global_logger(logger);
    slog_stdlog::init().unwrap();

    slog_scope::info!("Version: {}", std::env!("CARGO_PKG_VERSION"));
    if cfg!(debug_assertions) {
        slog_scope::debug!(
            "Debug build ({})",
            std::option_env!("GIT_HASH").unwrap_or("Unknown")
        );
    }
    slog_scope::info!("Cosmic starting up!");

    // init event loop
    let mut event_loop = EventLoop::try_new().with_context(|| "Failed to initialize event loop")?;

    // add wayland socket
    let mut display = Display::new();
    let socket_name = display.add_socket_auto()?;
    event_loop
        .handle()
        .insert_source(
            Generic::from_fd(display.get_poll_fd(), Interest::READ, Mode::Level),
            move |_, _, state: &mut state::State| {
                let display = state.display.clone();
                let mut display = display.borrow_mut();
                match display.dispatch(std::time::Duration::from_millis(0), state) {
                    Ok(_) => Ok(PostAction::Continue),
                    Err(e) => {
                        slog_scope::error!("I/O error on the Wayland display: {}", e);
                        state.should_stop = true;
                        Err(e)
                    }
                }
            },
        )
        .expect("Failed to init the wayland event source.");
    slog_scope::info!("Listening on {:?}", socket_name);

    // init state
    let mut state = state::State::new(display);

    // run the event loop
    let signal = event_loop.get_signal();
    event_loop.run(None, &mut state, |state| {
        // shall we shut down?
        if state.should_stop {
            signal.stop();
            return;
        }

        // send out events
        let display = state.display.clone();
        display.borrow_mut().flush_clients(state);
    })?;

    Ok(())
}
