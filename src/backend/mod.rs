// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use crate::wayland::protocols::a11y::A11yHandler;
use anyhow::{Context, Result, anyhow};
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
        if let Some(output) = state.common.shell.read().outputs().next().cloned() {
            if let Err(err) = state.finish_startup(output) {
                tracing::error!("Failed to finish startup: {err:?}");
            }
        } else {
            tracing::warn!("Backend initialized without output, deferring startup");
        }
    }
    res
}
