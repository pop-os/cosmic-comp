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
use zbus::blocking::{Connection, fdo::DBusProxy};

pub mod a11y_keyboard_monitor;
use a11y_keyboard_monitor::A11yKeyboardMonitorState;
#[cfg(feature = "systemd")]
pub mod logind;
mod name_owners;
mod power;

#[derive(Clone, Debug)]
pub struct DBusState(Rc<DBusStateInner>);

#[derive(Debug)]
struct DBusStateInner {
    evlh: LoopHandle<'static, State>,
    executor: calloop::futures::Scheduler<()>,
    session_con: async_once_cell::OnceCell<zbus::Connection>,
    a11y_keyboard_monitor: RefCell<Option<a11y_keyboard_monitor::A11yKeyboardMonitorState>>,
}

impl DBusState {
    pub fn init(evlh: &LoopHandle<'static, State>) -> Self {
        let (source, executor) = calloop::futures::executor().unwrap();
        let state = Self(Rc::new(DBusStateInner {
            evlh: evlh.clone(),
            executor,
            session_con: async_once_cell::OnceCell::new(),
            a11y_keyboard_monitor: RefCell::new(None),
        }));
        evlh.insert_source(source, |_, _, _| {}).unwrap();
        state.spawn(init_task(state.clone()));
        state
    }

    pub fn a11y_keyboard_monitor(
        &self,
    ) -> Option<RefMut<'_, a11y_keyboard_monitor::A11yKeyboardMonitorState>> {
        RefMut::filter_map(self.0.a11y_keyboard_monitor.borrow_mut(), |x| x.as_mut()).ok()
    }

    async fn session_con(&self) -> zbus::Result<&zbus::Connection> {
        self.0
            .session_con
            .get_or_try_init(zbus::Connection::session())
            .await
    }

    fn spawn(&self, fut: impl Future<Output = ()> + 'static) {
        let _ = self.0.executor.schedule(fut);
    }
}

async fn init_task(state: DBusState) {
    match state.session_con().await {
        Ok(conn) => {
            match name_owners::NameOwners::new(&conn, &state.0.executor).await {
                Ok(name_owners) => {
                    match A11yKeyboardMonitorState::new(conn, &name_owners, &state.0.executor).await
                    {
                        Ok(a11y_keyboard_monitor_state) => {
                            *state.0.a11y_keyboard_monitor.borrow_mut() =
                                Some(a11y_keyboard_monitor_state);
                        }
                        Err(err) => {
                            // TODO
                        }
                    }
                }
                Err(err) => {
                    // TODO
                }
            }
        }
        Err(err) => {
            // TODO
        }
    }

    match zbus::Connection::system().await {
        Ok(conn) => {
            let evlh = state.0.evlh.clone();
            state.spawn(async move {
                if let Err(err) = power_hot_plug_task(conn, evlh).await {
                    tracing::warn!(?err, "Failed to initialize dbus handlers");
                }
            });
        }
        Err(err) => {
            // TODO
        }
    }
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
