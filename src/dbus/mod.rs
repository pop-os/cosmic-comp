use crate::{
    state::{BackendData, Common, State},
    utils::prelude::OutputExt,
};
use anyhow::{Context, Result};
use calloop::{InsertError, LoopHandle, RegistrationToken};
use cosmic_comp_config::output::comp::OutputState;
use futures_executor::{block_on, ThreadPool};
use futures_util::stream::StreamExt;
use std::collections::HashMap;
use tracing::{error, warn};
use zbus::blocking::{fdo::DBusProxy, Connection};

pub mod a11y_keyboard_monitor;
#[cfg(feature = "systemd")]
pub mod logind;
mod name_owners;
mod power;

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

                        ()
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
                .unwrap_or(String::new()),
        ),
    ]))?;

    Ok(())
}

/*
/// Serve interfaces on session socket
///
/// (Currently only the a11y keyboard monitor interface)
fn serve_interfaces(executor: &ThreadPool) {
    let executor_clone = executor.clone();
    executor.spawn_ok(async move {
        serve_interfaces_inner(&executor_clone);
    });
}

async fn serve_interfaces_inner(executor: &ThreadPool, a11y_clients: Arc<Mutex<a11y_keyboard_monitor::Clients>>) -> zbus::Result<()> {
    let conn = zbus::Connection::session().await?;
    name_owners::NameOwners::new(&conn, executor).await?;
    Ok(())
}
*/
