use crate::{
    state::{BackendData, Common, State},
    utils::prelude::OutputExt,
};
use anyhow::{Context, Result};
use calloop::{InsertError, LoopHandle, stream::StreamSource};
use cosmic_comp_config::output::comp::OutputState;
use futures_executor::block_on;
use std::collections::HashMap;
use tracing::{error, warn};
use zbus::blocking::{Connection, fdo::DBusProxy};

pub mod a11y_keyboard_monitor;
#[cfg(feature = "systemd")]
pub mod logind;
mod name_owners;
mod power;

pub fn init(evlh: &LoopHandle<'static, State>) -> Result<()> {
    match block_on(power::init()) {
        Ok(power_daemon) => {
            if let Ok(stream) = block_on(power_daemon.receive_hot_plug_detect()) {
                let source = StreamSource::new(stream).unwrap();
                evlh.insert_source(source, |_, _, state| {
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
                })
                .map_err(|InsertError { error, .. }| error)
                .with_context(|| "Failed to add channel to event_loop")?;
            }
        }
        Err(err) => {
            tracing::info!(?err, "Failed to connect to com.system76.PowerDaemon");
        }
    };
    Ok(())
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
