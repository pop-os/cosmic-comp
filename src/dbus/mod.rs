use crate::{
    dbus::scheduler::Sys76Scheduler,
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
mod scheduler;

#[derive(Clone, Debug)]
pub struct DBusState(Rc<DBusStateInner>);

#[derive(Debug)]
struct DBusStateInner {
    evlh: LoopHandle<'static, State>,
    executor: calloop::futures::Scheduler<()>,
    session_conn: zbus::Result<zbus::Connection>,
    system_conn: zbus::Result<zbus::Connection>,
    a11y_keyboard_monitor: RefCell<Option<a11y_keyboard_monitor::A11yKeyboardMonitorState>>,
    ei_state: RefCell<Option<ei::EiState>>,
    ei_sender: Arc<Mutex<Option<calloop::channel::Sender<crate::libei::EiRequest>>>>,
    /// The DBus proxy to system76-scheduler.
    ///
    /// This is an [`Option`] because system76-scheduler may or may not be running.
    ///
    /// Currently, connection is only attempted once.
    system76_scheduler: Option<Sys76Scheduler>,
}

impl DBusState {
    pub fn init(evlh: &LoopHandle<'static, State>) -> Self {
        let (source, executor) = calloop::futures::executor().unwrap();
        let session_conn = zbus::block_on(zbus::Connection::session());
        let system_conn = zbus::block_on(zbus::Connection::system());
        let system76_scheduler = match &system_conn {
            // Attempt to connect once
            Ok(conn) => match zbus::block_on(Sys76Scheduler::new(conn)) {
                Ok(scheduler) => Some(scheduler),
                Err(err) => {
                    tracing::info!(?err, "failed to connect to com.system76.Scheduler");
                    None
                }
            },
            Err(err) => {
                tracing::error!(?err, "failed to initialize system DBus connection");
                None
            }
        };
        let state = Self(Rc::new(DBusStateInner {
            evlh: evlh.clone(),
            executor,
            session_conn,
            system_conn,
            a11y_keyboard_monitor: RefCell::new(None),
            ei_state: RefCell::new(None),
            ei_sender: Arc::new(Mutex::new(None)),
            system76_scheduler,
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

    pub fn ei_state(&self) -> Option<RefMut<'_, ei::EiState>> {
        RefMut::filter_map(self.0.ei_state.borrow_mut(), |x| x.as_mut()).ok()
    }

    pub fn set_ei_sender(&self, sender: calloop::channel::Sender<crate::libei::EiRequest>) {
        *self.0.ei_sender.lock().unwrap() = Some(sender);
    }

    /// Attempts to report the process ID of the foreground process to system76-scheduler.
    pub fn report_foreground_process(&self, pid: u32) {
        let state_clone = self.clone();
        self.spawn(async move {
            match &state_clone.0.system76_scheduler {
                Some(sched) => {
                    if let Err(err) = sched.set_foreground_process(pid).await {
                        tracing::warn!(
                            ?err,
                            "failed to report foreground process to system76-scheduler"
                        );
                    }
                }
                None => tracing::trace!("not reporting foreground process to system76-scheduler"),
            }
        });
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
    let ei_state = ei::EiState::new(
        conn,
        &name_owners,
        state.0.ei_sender.clone(),
        &state.0.executor,
    )
    .await?;
    *state.0.ei_state.borrow_mut() = Some(ei_state);
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
    zbus::block_on(async {
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
