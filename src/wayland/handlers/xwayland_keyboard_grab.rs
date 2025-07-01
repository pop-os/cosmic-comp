// SPDX-License-Identifier: GPL-3.0-only

use crate::{shell::focus::target::KeyboardFocusTarget, state::State};
use smithay::{
    delegate_xwayland_keyboard_grab,
    input::Seat,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    wayland::xwayland_keyboard_grab::{XWaylandKeyboardGrab, XWaylandKeyboardGrabHandler},
};
use std::sync::Mutex;

#[derive(Default)]
pub struct XWaylandGrabSeatData {
    pub grab: Mutex<Option<(WlSurface, XWaylandKeyboardGrab<State>)>>,
}

impl XWaylandKeyboardGrabHandler for State {
    fn grab(&mut self, surface: WlSurface, seat: Seat<Self>, grab: XWaylandKeyboardGrab<Self>) {
        let data = seat
            .user_data()
            .get_or_insert(XWaylandGrabSeatData::default);
        *data.grab.lock().unwrap() = Some((surface, grab));
    }

    fn keyboard_focus_for_xsurface(&self, surface: &WlSurface) -> Option<KeyboardFocusTarget> {
        let element = self
            .common
            .shell
            .read()
            .workspaces
            .spaces()
            .find_map(|x| x.element_for_surface(surface).cloned())?;
        Some(KeyboardFocusTarget::Element(element))
    }
}
delegate_xwayland_keyboard_grab!(State);
