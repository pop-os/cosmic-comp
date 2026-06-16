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
    sync::{Arc, Mutex},
};
use tracing::{error, warn};

pub mod a11y_keyboard_monitor;
use a11y_keyboard_monitor::A11yKeyboardMonitorState;
pub mod ei;
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
    ei_sender: Arc<Mutex<Option<calloop::channel::Sender<std::os::unix::net::UnixStream>>>>,
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
            ei_sender: Arc::new(Mutex::new(None)),
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

    pub fn set_ei_sender(&self, sender: calloop::channel::Sender<std::os::unix::net::UnixStream>) {
        *self.0.ei_sender.lock().unwrap() = Some(sender);
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
    ei::init(conn, &name_owners, state.0.ei_sender.clone()).await?;
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
