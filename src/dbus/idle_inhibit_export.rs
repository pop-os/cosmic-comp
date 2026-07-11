// SPDX-License-Identifier: GPL-3.0-only
//! Exports the set of clients currently holding a Wayland idle-inhibitor
//! (`zwp_idle_inhibitor_v1`) over the session bus, so a panel applet can show
//! which applications are preventing the system from going idle / sleeping.
//!
//! Only the compositor knows about Wayland idle-inhibitors, and there is no way
//! for another client to enumerate them, so we surface them here. This runs on
//! its own thread with a blocking zbus connection and communicates with the
//! compositor purely through an `Arc<Mutex<..>>`; any failure only logs and can
//! never affect the compositor's main loop.

use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

/// Maps a `wl_surface` protocol id -> (client pid, best-effort app id) for every
/// surface that currently holds an idle-inhibitor. Shared between the compositor
/// thread (writer) and the D-Bus server thread (reader).
pub type IdleInhibitors = Arc<Mutex<HashMap<u32, (u32, String)>>>;

/// Create an empty, shareable inhibitor table.
pub fn new() -> IdleInhibitors {
    Arc::new(Mutex::new(HashMap::new()))
}

struct Export {
    inhibitors: IdleInhibitors,
}

#[zbus::interface(name = "com.system76.CosmicComp.IdleInhibit")]
impl Export {
    /// `(pid, app_id)` for each surface currently holding a Wayland
    /// idle-inhibitor. The same pid may appear more than once (one entry per
    /// inhibiting surface); dedup on the client side if desired. `app_id` is
    /// best-effort and may be empty — resolve the pid via `/proc` if needed.
    fn list_inhibitors(&self) -> Vec<(u32, String)> {
        self.inhibitors
            .lock()
            .map(|map| map.values().cloned().collect())
            .unwrap_or_default()
    }
}

/// Spawn the D-Bus export server on a dedicated thread. Returns immediately;
/// errors (e.g. no session bus, name already taken) are logged and the thread
/// simply exits without disturbing the compositor.
pub fn spawn(inhibitors: IdleInhibitors) {
    let _ = std::thread::Builder::new()
        .name("idle-inhibit-dbus".into())
        .spawn(move || {
            if let Err(err) = serve(inhibitors) {
                tracing::warn!(?err, "idle-inhibitor D-Bus export unavailable");
            }
        });
}

fn serve(inhibitors: IdleInhibitors) -> zbus::Result<()> {
    let _conn = zbus::blocking::connection::Builder::session()?
        .name("com.system76.CosmicComp")?
        .serve_at("/com/system76/CosmicComp", Export { inhibitors })?
        .build()?;
    // Keep the connection (and thus the bus name + served object) alive for the
    // lifetime of the compositor. zbus services I/O on its own internal threads.
    loop {
        std::thread::park();
    }
}
