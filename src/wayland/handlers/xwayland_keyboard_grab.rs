// SPDX-License-Identifier: GPL-3.0-only

use crate::{shell::focus::target::KeyboardFocusTarget, state::State};
use smithay::{
    delegate_xwayland_keyboard_grab, reexports::wayland_server::protocol::wl_surface::WlSurface,
    wayland::xwayland_keyboard_grab::XWaylandKeyboardGrabHandler,
};

impl XWaylandKeyboardGrabHandler for State {
    fn keyboard_focus_for_xsurface(&self, surface: &WlSurface) -> Option<KeyboardFocusTarget> {
        let element = self
            .common
            .shell
            .read()
            .unwrap()
            .workspaces
            .spaces()
            .find_map(|x| x.element_for_surface(surface).cloned())?;
        Some(KeyboardFocusTarget::Element(element))
    }
}
delegate_xwayland_keyboard_grab!(State);
