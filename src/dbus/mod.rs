use crate::{
    state::{BackendData, Common, State},
    utils::prelude::OutputExt,
};
use anyhow::{Context, Result};
use calloop::{InsertError, LoopHandle, RegistrationToken};
use cosmic_comp_config::output::comp::OutputState;
use futures_executor::{ThreadPool, block_on};
use futures_util::stream::StreamExt;
use std::collections::HashMap;
use tracing::{error, warn};
use zbus::blocking::{Connection, fdo::DBusProxy};

pub mod a11y_keyboard_monitor;
#[cfg(feature = "systemd")]
pub mod logind;
mod name_owners;
mod power;
#[cfg(feature = "systemd")]
mod sleep;

pub fn init(
    evlh: &LoopHandle<'static, State>,
    executor: &ThreadPool,
) -> Result<Vec<RegistrationToken>> {
    let mut tokens = Vec::new();

    match block_on(power::init()) {
        Ok(power_daemon) => {
            let (tx, rx) = calloop::channel::channel();

            let token = evlh
                .insert_source(rx, |event, _, state| match event {
                    calloop::channel::Event::Msg(_) => {
                        let nodes = match &mut state.backend {
                            BackendData::Kms(kms) => {
                                kms.drm_devices.keys().cloned().collect::<Vec<_>>()
                            }
                            _ => Vec::new(),
                        };
                        let mut added = Vec::new();
                        for node in nodes {
                            match state.device_changed(node.dev_id()) {
                                Ok(outputs) => added.extend(outputs),
                                Err(err) => {
                                    tracing::error!(?err, "Failed to update drm device {}.", node)
                                }
                            }
                        }
                        if let Err(err) = state.refresh_output_config() {
                            warn!("Unable to load output config: {}", err);
                            if !added.is_empty() {
                                for output in added {
                                    output.config_mut().enabled = OutputState::Disabled;
                                }
                                if let Err(err) = state.refresh_output_config() {
                                    error!("Unrecoverable config error: {}", err);
                                }
                            }
                        }
                    }
                    calloop::channel::Event::Closed => (),
                })
                .map_err(|InsertError { error, .. }| error)
                .with_context(|| "Failed to add channel to event_loop")?;

            // start helper thread
            executor.spawn_ok(async move {
                if let Ok(mut msg_iter) = power_daemon.receive_hot_plug_detect().await {
                    while let Some(msg) = msg_iter.next().await {
                        if tx.send(msg).is_err() {
                            break;
                        }
                    }
                }
            });

            tokens.push(token);
        }
        Err(err) => {
            tracing::info!(?err, "Failed to connect to com.system76.PowerDaemon");
        }
    };

    // Listen for system sleep events (PrepareForSleep)
    #[cfg(feature = "systemd")]
    match block_on(sleep::init()) {
        Ok(logind_manager) => {
            let (tx, rx) = calloop::channel::channel::<bool>();

            let token = evlh
                .insert_source(rx, |event, _, state| match event {
                    calloop::channel::Event::Msg(preparing) => {
                        if preparing {
                            state.pause_session();
                            // Release the inhibitor lock so logind proceeds with suspend.
                            state.common.inhibit_sleep_fd.take();
                        } else {
                            // Session resume is handled by SessionEvent::ActivateSession
                            // (from libseat), which always fires on wake. Calling
                            // resume_session here as well would race with the render
                            // threads' first frame submission (use_mode resets the
                            // swapchain), leaving NVIDIA primary-plane outputs blank.
                            // Re-acquire the sleep inhibitor lock for the next cycle.
                            state.common.event_loop_handle.insert_idle(|state| {
                                match logind::inhibit_sleep() {
                                    Ok(fd) => {
                                        state.common.inhibit_sleep_fd = Some(fd);
                                    }
                                    Err(err) => {
                                        warn!(?err, "Failed to re-acquire sleep inhibitor lock");
                                    }
                                }
                            });
                        }
                    }
                    calloop::channel::Event::Closed => (),
                })
                .map_err(|InsertError { error, .. }| error)
                .with_context(|| "Failed to add sleep channel to event_loop")?;

            executor.spawn_ok(async move {
                if let Ok(mut stream) = logind_manager.receive_prepare_for_sleep().await {
                    while let Some(signal) = stream.next().await {
                        if let Ok(args) = signal.args() {
                            if tx.send(args.start).is_err() {
                                break;
                            }
                        }
                    }
                }
            });

            tokens.push(token);
        }
        Err(err) => {
            tracing::info!(?err, "Failed to connect to logind for sleep signals");
        }
    };

    Ok(tokens)
}

/// Updated the D-Bus activation environment with `WAYLAND_DISPLAY` and
/// `DISPLAY` variables.
pub fn ready(common: &Common) -> Result<()> {
    let conn = Connection::session()?;
    let proxy = DBusProxy::new(&conn)?;

    proxy.update_activation_environment(HashMap::from([
        ("WAYLAND_DISPLAY", common.socket.to_str().unwrap()),
        (
            "DISPLAY",
            &common
                .xwayland_state
                .as_ref()
                .map(|s| format!(":{}", s.display))
                .unwrap_or_default(),
        ),
    ]))?;

    Ok(())
}
