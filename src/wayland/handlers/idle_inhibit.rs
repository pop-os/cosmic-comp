use smithay::{
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    wayland::idle_inhibit::IdleInhibitHandler,
};

use crate::state::State;

impl IdleInhibitHandler for State {
    fn inhibit(&mut self, surface: WlSurface) {
        self.common.idle_inhibiting_surfaces.insert(surface);
        self.common.refresh_idle_inhibit();
    }

    fn uninhibit(&mut self, surface: WlSurface) {
        self.common.idle_inhibiting_surfaces.remove(&surface);
        self.common.refresh_idle_inhibit();
    }
}
