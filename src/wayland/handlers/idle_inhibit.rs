use smithay::{
    reexports::wayland_server::{Resource, protocol::wl_surface::WlSurface},
    wayland::idle_inhibit::IdleInhibitHandler,
};

use crate::state::State;

impl IdleInhibitHandler for State {
    fn inhibit(&mut self, surface: WlSurface) {
        // Record the inhibiting client's pid for the D-Bus export (best-effort;
        // never fails the inhibit itself).
        let key = surface.id().protocol_id();
        let pid = surface
            .client()
            .and_then(|client| client.get_credentials(&self.common.display_handle).ok())
            .map(|creds| creds.pid as u32)
            .unwrap_or(0);
        if let Ok(mut map) = self.common.idle_inhibitors_export.lock() {
            map.insert(key, (pid, String::new()));
        }

        self.common.idle_inhibiting_surfaces.insert(surface);
        self.common.idle_notifier_state.set_is_inhibited(true);
    }

    fn uninhibit(&mut self, surface: WlSurface) {
        let key = surface.id().protocol_id();
        if let Ok(mut map) = self.common.idle_inhibitors_export.lock() {
            map.remove(&key);
        }

        self.common.idle_inhibiting_surfaces.remove(&surface);
        self.common.refresh_idle_inhibit();
    }
}
