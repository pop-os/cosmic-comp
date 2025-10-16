// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use crate::wayland::protocols::a11y::A11yHandler;
use anyhow::{Context, Result, anyhow};
use cosmic_comp_config::NumlockState;
use cosmic_config::CosmicConfigEntry;
use cosmic_settings_daemon_config::greeter;
use smithay::reexports::{calloop::EventLoop, wayland_server::DisplayHandle};
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
            .outputs()
            .next()
            .with_context(|| "Backend initialized without output")
            .cloned()?;
        let initial_seat = crate::shell::create_seat(
            dh,
            &mut state.common.seat_state,
            &output,
            &state.common.config,
            "seat-0".into(),
        );

        let keyboard = initial_seat
            .get_keyboard()
            .ok_or_else(|| anyhow!("`shell::create_seat` did not setup keyboard"))?;

        state
            .common
            .shell
            .write()
            .seats
            .add_seat(initial_seat.clone());

        let greeter_state = match greeter::GreeterAccessibilityState::config() {
            Ok(helper) => match greeter::GreeterAccessibilityState::get_entry(&helper) {
                Ok(s) => s,
                Err((errs, s)) => {
                    for err in errs {
                        tracing::error!("Error loading greeter state: {err:?}");
                    }
                    s
                }
            },
            Err(_) => {
                tracing::info!("`cosmic-greeter` state not found.");
                greeter::GreeterAccessibilityState::default()
            }
        };

        if let Some(magnifier) = greeter_state.magnifier {
            let mut zoom = state.common.config.cosmic_conf.accessibility_zoom;

            zoom.start_on_login = magnifier;
            if let Err(err) = state
                .common
                .config
                .cosmic_conf
                .set_accessibility_zoom(&state.common.config.cosmic_helper, zoom)
            {
                tracing::error!("Failed to set screen filter: {err:?}");
            }
        }

        if let Some(inverted) = greeter_state.invert_colors {
            if inverted != state.a11y_state().screen_inverted() {
                state.request_screen_invert(inverted);
            }
        }

        if state
            .common
            .config
            .cosmic_conf
            .accessibility_zoom
            .start_on_login
        {
            state.common.shell.write().trigger_zoom(
                &initial_seat,
                None,
                1.0 + (state.common.config.cosmic_conf.accessibility_zoom.increment as f64 / 100.),
                &state.common.config.cosmic_conf.accessibility_zoom,
                true,
                &state.common.event_loop_handle,
            );
        }

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
            /// Linux scancode for numlock key.
            const NUMLOCK_SCANCODE: u32 = 69;
            crate::config::change_modifier_state(&keyboard, NUMLOCK_SCANCODE, state);
        }
        {
            {
                state
                    .common
                    .startup_done
                    .store(true, std::sync::atomic::Ordering::SeqCst);
                for output in state.common.shell.read().outputs() {
                    state.backend.schedule_render(output);
                }
            }
        }
    }
    res
}
