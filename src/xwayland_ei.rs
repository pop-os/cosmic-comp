//! libei access for Xwayland.
//!
//! Instead of the RemoteDesktop portal (which can't tell X clients apart or remember a choice),
//! Xwayland gets a private `LIBEI_SOCKET` that only its own pid may use.
//! Each connection is then allowed by config, by an earlier answer, or after asking the user
//! through the cosmic portal's Access dialog.

use std::{
    collections::HashMap,
    io,
    net::Shutdown,
    os::unix::net::{UnixListener, UnixStream},
    path::PathBuf,
    sync::atomic::{AtomicU32, Ordering},
};

use cosmic_comp_config::{CosmicCompConfig, XwaylandEiDecision};
use cosmic_config::ConfigSet;
use smithay::{
    backend::libei::EiInputSeat,
    reexports::{
        calloop::{self, LoopHandle, generic::Generic},
        reis::eis,
    },
};
use tracing::{debug, error, info, warn};
use zbus::{ObjectPath, OwnedVariant, Variant};

use crate::{
    libei::{DEVICE_TYPE_KEYBOARD, DEVICE_TYPE_POINTER, DEVICE_TYPE_TOUCHSCREEN, EiRequest},
    state::State,
};

#[derive(Debug)]
struct Pending {
    conn: eis::Connection,
    seat: EiInputSeat,
    device_types: u32,
    socket: UnixStream,
}

#[derive(Debug, Default)]
pub struct XwaylandEiAccess {
    // By default answers are all kept until the compositor restarts
    // TODO: The issue is xWayland will keep asking for all queued events if you deny it, so we have
    // to save for the session to avoid a flood of requests. Also "allow once" is not possible since
    // xWayland is just gonna use the socket we give it. So, any decision is either per session, or
    // permanent (saved to disk).
    session: HashMap<String, XwaylandEiDecision>,
    // A key here means a dialog for that name is open
    pending: HashMap<String, Vec<Pending>>,
}

impl XwaylandEiAccess {
    /// `None` means ask the user.
    fn decide(&self, config: &CosmicCompConfig, name: &str) -> Option<XwaylandEiDecision> {
        // A saved answer wins over both the global switch and this session's answers
        // TODO: does this make sense? If a user toggles the "allow all X11 apps" and still their
        // app gets denied, is that OK?
        if let Some(saved) = config.xwayland_ei_apps.get(name) {
            Some(*saved)
        } else if config.xwayland_ei_no_prompt {
            Some(XwaylandEiDecision::Allow)
        } else {
            self.session.get(name).copied()
        }
    }

    /// Drop a connection that went away while its dialog was still open.
    pub fn forget(&mut self, conn: &eis::Connection) {
        for pending in self.pending.values_mut() {
            pending.retain(|p| &p.conn != conn);
        }
    }
}

/// Bind the socket Xwayland gets as `LIBEI_SOCKET`.
pub fn bind_socket() -> io::Result<(UnixListener, PathBuf)> {
    let dir = std::env::var_os("XDG_RUNTIME_DIR")
        .ok_or_else(|| io::Error::other("XDG_RUNTIME_DIR is not set"))?;
    let path = PathBuf::from(dir).join(format!("cosmic-xwayland-eis-{}", std::process::id()));
    let _ = std::fs::remove_file(&path);
    let listener = UnixListener::bind(&path)?;
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o600))?;
    }
    listener.set_nonblocking(true)?;
    Ok((listener, path))
}

fn is_xwayland_child(status: &str, parent: u32) -> bool {
    let field = |key: &str| {
        status
            .lines()
            .find_map(|line| line.strip_prefix(key)?.strip_prefix(':'))
            .map(str::trim)
    };
    field("Name") == Some("Xwayland") && field("PPid") == Some(parent.to_string().as_str())
}

/// Accept only EI connections from our own Xwayland.
pub fn listen(
    handle: &LoopHandle<'static, State>,
    listener: UnixListener,
    ei_sender: calloop::channel::Sender<EiRequest>,
) {
    let source = Generic::new(listener, calloop::Interest::READ, calloop::Mode::Level);
    let result = handle.insert_source(source, move |_, listener, _| {
        loop {
            let stream = match listener.accept() {
                Ok((stream, _)) => stream,
                Err(err) if err.kind() == io::ErrorKind::WouldBlock => break,
                Err(err) => {
                    warn!(?err, "Failed to accept Xwayland EI connection");
                    break;
                }
            };
            // HACK: we can't get the xwayland pid, but any of our children called xwayland is ok.
            let peer = rustix::net::sockopt::socket_peercred(&stream).map(|cred| cred.pid);
            let ok = peer.as_ref().is_ok_and(|pid| {
                std::fs::read_to_string(format!("/proc/{}/status", pid.as_raw_nonzero()))
                    .is_ok_and(|status| is_xwayland_child(&status, std::process::id()))
            });
            if !ok {
                warn!(
                    ?peer,
                    "Rejecting EI connection on the Xwayland socket from another process"
                );
                continue;
            }
            if let Err(err) = stream.set_nonblocking(true) {
                warn!(?err, "Failed to set Xwayland EI socket non-blocking");
                continue;
            }
            let _ = ei_sender.send((
                stream,
                DEVICE_TYPE_KEYBOARD | DEVICE_TYPE_POINTER | DEVICE_TYPE_TOUCHSCREEN,
                true,
            ));
        }
        Ok(calloop::PostAction::Continue)
    });
    if let Err(err) = result {
        error!(?err, "Failed to listen for Xwayland EI connections");
    }
}

impl State {
    pub(crate) fn xwayland_ei_connected(
        &mut self,
        conn: eis::Connection,
        seat: EiInputSeat,
        name: String,
        device_types: u32,
        socket: UnixStream,
    ) {
        let access = &mut self.common.xwayland_ei;
        match access.decide(&self.common.config.cosmic_conf, &name) {
            Some(XwaylandEiDecision::Allow) => self.add_ei_devices(&conn, &seat, device_types),
            Some(XwaylandEiDecision::Deny) => {
                // Xwayland reconnects for all events in the queue, so keep this to `debug!` level
                debug!(name, "Refusing input emulation from X11 app denied earlier");
                let _ = socket.shutdown(Shutdown::Both);
            }
            None => {
                let first = !access.pending.contains_key(&name);
                access
                    .pending
                    .entry(name.clone())
                    .or_default()
                    .push(Pending {
                        conn,
                        seat,
                        device_types,
                        socket,
                    });
                if first {
                    self.common.dbus_state.xwayland_ei_access_dialog(name);
                }
            }
        }
    }

    /// The dialog for `name` closed with `decision`, `remember` saves it to the config.
    pub(crate) fn xwayland_ei_answered(
        &mut self,
        name: String,
        decision: XwaylandEiDecision,
        remember: bool,
    ) {
        info!(
            name,
            ?decision,
            remember,
            "User answered input emulation for X11 app"
        );
        let access = &mut self.common.xwayland_ei;
        let pending = access.pending.remove(&name).unwrap_or_default();
        access.session.insert(name.clone(), decision);
        // Never save an unnamed app
        // TODO: make sure this doesn't cause issues, and make sure all apps do return a name
        // saving "" for the name would then match all unnamed apps which is not good
        if remember && !name.is_empty() {
            let apps = &mut self.common.config.cosmic_conf.xwayland_ei_apps;
            apps.insert(name, decision);
            let apps = apps.clone();
            let config = self.common.config.cosmic_helper.clone();
            std::thread::spawn(move || {
                if let Err(err) = config.set("xwayland_ei_apps", apps) {
                    error!(?err, "Failed to update xwayland_ei_apps key");
                }
            });
        }
        for p in pending {
            match decision {
                XwaylandEiDecision::Allow => self.add_ei_devices(&p.conn, &p.seat, p.device_types),
                XwaylandEiDecision::Deny => {
                    let _ = p.socket.shutdown(Shutdown::Both);
                }
            }
        }
    }
}

/// Ask the user through the cosmic portals Access dialog.
pub async fn access_dialog(
    conn: &zbus::Connection,
    name: &str,
) -> zbus::Result<(XwaylandEiDecision, bool)> {
    static REQUEST: AtomicU32 = AtomicU32::new(0);
    let handle = ObjectPath::try_from(format!(
        "/com/system76/CosmicComp/request/xwayland_ei_{}",
        REQUEST.fetch_add(1, Ordering::Relaxed)
    ))?;
    let app = if name.is_empty() {
        "An X11 app".to_owned()
    } else {
        format!("An X11 app claiming to be “{name}”")
    };
    // TODO: localization!
    let body = format!("{app} wants to control your keyboard and pointer.");

    let mut options: HashMap<&str, Variant> = HashMap::new();
    options.insert("grant_label", "Allow".into());
    options.insert("deny_label", "Deny".into());
    options.insert("icon", "input-keyboard-symbolic".into());
    if !name.is_empty() {
        // No options makes it a boolean choice, drawn as a checkbox
        let choices = vec![(
            "remember",
            "Remember my choice",
            Vec::<(&str, &str)>::new(),
            "false",
        )];
        options.insert("choices", choices.into());
    }

    let reply = conn
        .call_method(
            Some("org.freedesktop.impl.portal.desktop.cosmic"),
            "/org/freedesktop/portal/desktop",
            Some("org.freedesktop.impl.portal.Access"),
            "AccessDialog",
            &(handle, "", "", "Allow input emulation?", "", body, options),
        )
        .await?;
    let (response, results): (u32, HashMap<String, OwnedVariant>) = reply.body().deserialize()?;
    Ok(read_answer(response, &results))
}

fn read_answer(
    response: u32,
    results: &HashMap<String, OwnedVariant>,
) -> (XwaylandEiDecision, bool) {
    let decision = match response {
        0 => XwaylandEiDecision::Allow,
        _ => XwaylandEiDecision::Deny,
    };
    // The deny button reports choices too
    let remember = response <= 1
        && results
            .get("choices")
            .and_then(|choices| Vec::<(String, String)>::try_from(choices.try_clone().ok()?).ok())
            .is_some_and(|choices| {
                choices
                    .iter()
                    .any(|(id, v)| id == "remember" && v == "true")
            });
    (decision, remember)
}
