// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use anyhow::Result;
use smithay::reexports::calloop::EventLoop;

pub mod render;

pub mod kms;
pub mod winit;
pub mod x11;
// TODO
// pub mod wayland; // tbd in smithay

pub fn init_backend_auto(event_loop: &mut EventLoop<'static, State>, state: &mut State) -> Result<()> {
    match std::env::var("COSMIC_BACKEND") {
        Ok(x) if x == "x11" => x11::init_backend(event_loop, state),
        Ok(x) if x == "winit" => winit::init_backend(event_loop, state),
        Ok(x) if x == "kms" => kms::init_backend(event_loop, state),
        Ok(_) => unimplemented!("There is no backend with this identifier"),
        Err(_) => {
            if std::env::var_os("DISPLAY").is_some()
                || std::env::var_os("WAYLAND_DISPLAY").is_some()
            {
                match x11::init_backend(event_loop, state) {
                    Ok(_) => Ok(()),
                    Err(err) => {
                        slog_scope::warn!("X11 Backend failed with error: {}", err);
                        slog_scope::info!("Falling back to winit backend.");
                        winit::init_backend(event_loop, state)
                    }
                }
            } else {
                kms::init_backend(event_loop, state)
            }
        }
    }
}
