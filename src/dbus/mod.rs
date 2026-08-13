use crate::{
    state::{BackendData, Common, State},
    utils::prelude::OutputExt,
};
use anyhow::{Context, Result};
use calloop::{InsertError, LoopHandle, stream::StreamSource};
use cosmic_comp_config::output::comp::OutputState;
use std::{
    cell::{RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
};
use tracing::{error, warn};

pub mod a11y_keyboard_monitor;
pub mod game_mode;
use a11y_keyboard_monitor::A11yKeyboardMonitorState;
#[cfg(feature = "logind")]
pub mod logind;
mod name_owners;
mod power;

#[derive(Clone, Debug)]
pub struct DBusState(Rc<DBusStateInner>);

#[derive(Debug)]
struct DBusStateInner {
    evlh: LoopHandle<'static, State>,
    executor: calloop::futures::Scheduler<()>,
    session_conn: zbus::Result<zbus::Connection>,
    system_conn: zbus::Result<zbus::Connection>,
    a11y_keyboard_monitor: RefCell<Option<a11y_keyboard_monitor::A11yKeyboardMonitorState>>,
}

impl DBusState {
    pub fn init(evlh: &LoopHandle<'static, State>) -> Self {
        let (source, executor) = calloop::futures::executor().unwrap();
        let session_conn = futures_executor::block_on(zbus::Connection::session());
        let system_conn = futures_executor::block_on(zbus::Connection::system());
        let state = Self(Rc::new(DBusStateInner {
            evlh: evlh.clone(),
            executor,
            session_conn,
            system_conn,
            a11y_keyboard_monitor: RefCell::new(None),
        }));
        evlh.insert_source(source, |_, _, _| {}).unwrap();
        let state_clone = state.clone();
        state.spawn(async move {
            if let Err(err) = init_session(&state_clone).await {
                tracing::error!("Failed to initialize session DBus connection: {}", err);
            }
        });
        let state_clone = state.clone();
        state.spawn(async move {
            if let Err(err) = init_system(&state_clone).await {
                tracing::error!("Failed to initialize system DBus connection: {}", err);
            }
        });
        state
    }

    pub fn a11y_keyboard_monitor(
        &self,
    ) -> Option<RefMut<'_, a11y_keyboard_monitor::A11yKeyboardMonitorState>> {
        RefMut::filter_map(self.0.a11y_keyboard_monitor.borrow_mut(), |x| x.as_mut()).ok()
    }

    // TODO Lazy async init when we don't have anything blocking main thread
    async fn session_conn(&self) -> zbus::Result<&zbus::Connection> {
        self.0.session_conn.as_ref().map_err(|err| err.clone())
    }

    async fn system_conn(&self) -> zbus::Result<&zbus::Connection> {
        self.0.system_conn.as_ref().map_err(|err| err.clone())
    }

    fn spawn(&self, fut: impl Future<Output = ()> + 'static) {
        let _ = self.0.executor.schedule(fut);
    }
}

async fn init_session(state: &DBusState) -> zbus::Result<()> {
    let conn = state.session_conn().await?;
    let name_owners = name_owners::NameOwners::new(conn, &state.0.executor).await?;
    let a11y_keyboard_monitor_state =
        A11yKeyboardMonitorState::new(conn, &name_owners, &state.0.executor).await?;
    *state.0.a11y_keyboard_monitor.borrow_mut() = Some(a11y_keyboard_monitor_state);
    Ok(())
}

async fn init_system(state: &DBusState) -> zbus::Result<()> {
    let conn = state.system_conn().await?.clone();
    let evlh = state.0.evlh.clone();
    state.spawn(async move {
        if let Err(err) = power_hot_plug_task(conn, evlh).await {
            tracing::warn!(?err, "Failed to initialize dbus handlers");
        }
    });
    let conn = state.system_conn().await?.clone();
    let evlh = state.0.evlh.clone();
    state.spawn(async move {
        if let Err(err) = shutdown_task(conn, evlh).await {
            tracing::warn!(?err, "Failed to watch logind for shutdown");
        }
    });
    Ok(())
}

/// logind's shutdown announcement, with the `type` metadata that distinguishes a real
/// reboot/poweroff from a soft-reboot. `logind-zbus` only exposes the plain signal,
/// which fires for soft-reboot too — and a soft-reboot keeps the display alive for a
/// new compositor, so it must stay a normal handoff.
#[zbus::proxy(
    interface = "org.freedesktop.login1.Manager",
    default_service = "org.freedesktop.login1",
    default_path = "/org/freedesktop/login1"
)]
trait ShutdownManager {
    #[zbus(signal)]
    fn prepare_for_shutdown_with_metadata(
        &self,
        start: bool,
        metadata: HashMap<String, zbus::zvariant::OwnedValue>,
    ) -> zbus::Result<()>;
}

/// Watch for an imminent reboot/poweroff and start the fade to black.
///
/// Only the fade is driven from here; the exit path reads `Shell::shutdown_fade`. On a
/// systemd too old for the metadata signal nothing arrives and the behaviour is
/// unchanged — the desktop is frozen as before.
async fn shutdown_task(conn: zbus::Connection, evlh: LoopHandle<'static, State>) -> Result<()> {
    let proxy = ShutdownManagerProxy::new(&conn)
        .await
        .context("no logind manager")?;
    let stream = proxy
        .receive_prepare_for_shutdown_with_metadata()
        .await
        .context("failed to subscribe to PrepareForShutdownWithMetadata")?;

    let source = StreamSource::new(stream).unwrap();
    evlh.insert_source(source, |signal, _, state| {
        let Some(signal) = signal else {
            return;
        };
        let Ok(args) = signal.args() else {
            return;
        };
        if !args.start {
            // Shutdown cancelled — logind emits this if the job fails. Repaint, or the
            // screen stays on the black plate with nothing left to damage it.
            let was_fading = state.common.shell.write().shutdown_fade.take().is_some();
            if was_fading {
                tracing::info!("shutdown: cancelled, restoring the desktop");
                state.schedule_all_outputs();
            }
            return;
        }
        // A soft-reboot hands the live display to a fresh compositor, so it wants the
        // normal frozen-frame handoff, not a fade to black.
        let kind = args
            .metadata
            .get("type")
            .and_then(|v| <String as TryFrom<zbus::zvariant::OwnedValue>>::try_from(v.clone()).ok())
            .unwrap_or_default();
        if kind == "soft-reboot" {
            tracing::debug!("shutdown: soft-reboot, keeping the handoff freeze");
            return;
        }
        tracing::info!(%kind, "shutdown: fading to black before exit");
        state.begin_shutdown_fade();
    })
    .map_err(|InsertError { error, .. }| error)
    .with_context(|| "Failed to add shutdown signal to event_loop")?;
    Ok(())
}

async fn power_hot_plug_task(
    conn: zbus::Connection,
    evlh: LoopHandle<'static, State>,
) -> Result<()> {
    match power::init(&conn).await {
        Ok(power_daemon) => {
            if let Ok(stream) = power_daemon.receive_hot_plug_detect().await {
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
    futures_executor::block_on(async {
        let conn = common.dbus_state.session_conn().await?;
        let dbus = zbus::fdo::DBusProxy::new(conn).await?;

        dbus.update_activation_environment(HashMap::from([
            ("WAYLAND_DISPLAY", common.socket.to_str().unwrap()),
            (
                "DISPLAY",
                &common
                    .xwayland_state
                    .as_ref()
                    .map(|s| format!(":{}", s.display))
                    .unwrap_or_default(),
            ),
        ]))
        .await
    })?;
    Ok(())
}
