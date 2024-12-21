// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use anyhow::{Context, Result};
use cosmic_comp_config::NumlockState;
use smithay::backend::input::{self as smithay_input};
use smithay::reexports::{calloop::EventLoop, wayland_server::DisplayHandle};
use smithay::utils::SERIAL_COUNTER;
use tracing::{info, warn};

pub mod render;

pub mod kms;
pub mod winit;
pub mod x11;
// TODO
// pub mod wayland; // tbd in smithay

pub fn init_backend_auto(
    dh: &DisplayHandle,
    event_loop: &mut EventLoop<'static, State>,
    state: &mut State,
) -> Result<()> {
    let res = match std::env::var("COSMIC_BACKEND") {
        Ok(x) if x == "x11" => x11::init_backend(dh, event_loop, state),
        Ok(x) if x == "winit" => winit::init_backend(dh, event_loop, state),
        Ok(x) if x == "kms" => kms::init_backend(dh, event_loop, state),
        Ok(_) => unimplemented!("There is no backend with this identifier"),
        Err(_) => {
            if std::env::var_os("DISPLAY").is_some()
                || std::env::var_os("WAYLAND_DISPLAY").is_some()
            {
                match x11::init_backend(dh, event_loop, state) {
                    Ok(_) => Ok(()),
                    Err(err) => {
                        warn!(?err, "Initializing X11 Backend failed.");
                        info!("Falling back to winit backend.");
                        winit::init_backend(dh, event_loop, state)
                    }
                }
            } else {
                kms::init_backend(dh, event_loop, state)
            }
        }
    };

    if res.is_ok() {
        let output = state
            .common
            .shell
            .read()
            .unwrap()
            .outputs()
            .next()
            .with_context(|| "Backend initialized without output")
            .cloned()?;
        let (initial_seat, keyboard) = crate::shell::create_seat(
            dh,
            &mut state.common.seat_state,
            &output,
            &state.common.config,
            "seat-0".into(),
        );

        state
            .common
            .shell
            .write()
            .unwrap()
            .seats
            .add_seat(initial_seat);

        let desired_numlock = state
            .common
            .config
            .cosmic_conf
            .keyboard_config
            .numlock_state;
        // Restore numlock state based on config.
        let toggle_numlock = match desired_numlock {
            NumlockState::BootOff => keyboard.modifier_state().num_lock,
            NumlockState::BootOn => !keyboard.modifier_state().num_lock,
            NumlockState::LastBoot => {
                keyboard.modifier_state().num_lock
                    != state.common.config.dynamic_conf.numlock().last_state
            }
        };

        // If we're enabling numlock...
        if toggle_numlock {
            let mut input = |key_state| {
                let time = state.common.clock.now().as_millis();
                let _ = keyboard.input(
                    state,
                    smithay_input::Keycode::new(77),
                    key_state,
                    SERIAL_COUNTER.next_serial(),
                    time,
                    |_, _, _| smithay::input::keyboard::FilterResult::<()>::Forward,
                );
            };
            // Press and release the numlock key to get modifiers updated.
            input(smithay_input::KeyState::Pressed);
            input(smithay_input::KeyState::Released);
        }
        {
            {
                state
                    .common
                    .startup_done
                    .store(true, std::sync::atomic::Ordering::SeqCst);
                for output in state.common.shell.read().unwrap().outputs() {
                    state.backend.schedule_render(&output);
                }
            }
        }
    }
    res
}
