// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    input::pointer::PointerHandle,
    reexports::wayland_server::protocol::{wl_pointer::WlPointer, wl_surface::WlSurface},
    utils::{Logical, Point, Serial},
    wayland::pointer_warp::PointerWarpHandler,
};

impl PointerWarpHandler for State {
    fn warp_pointer(
        &mut self,
        surface: WlSurface,
        pointer: WlPointer,
        pos: Point<f64, Logical>,
        serial: Serial,
    ) {
        let Some(resource_handle) = PointerHandle::<State>::from_resource(&pointer) else {
            return;
        };

        let shell = self.common.shell.read();

        let pointer_handle = shell.seats.iter().find_map(|seat| {
            if let Some(pointer_handle) = seat.get_pointer()
                && resource_handle == pointer_handle
                && pointer_handle.last_enter() == Some(serial)
                && let Some(keyboard) = seat.get_keyboard()
                && let Some(keyboard_focus) = keyboard.current_focus()
                && keyboard_focus.has_surface(&shell, &surface)
            {
                return Some(pointer_handle);
            }
            None
        });

        drop(shell);

        if let Some(pointer_handle) = pointer_handle {
            self.apply_cursor_hint(&surface, &pointer_handle, pos);
        }
    }
}
