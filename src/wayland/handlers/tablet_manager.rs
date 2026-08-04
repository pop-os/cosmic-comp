// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    backend::input::TabletToolDescriptor, input::pointer::CursorImageStatus,
    input::tablet::TabletSeatHandler, reexports::wayland_server::protocol::wl_surface,
};

impl TabletSeatHandler for State {
    type ToolFocus = wl_surface::WlSurface;

    fn tablet_tool_image(&mut self, _tool: &TabletToolDescriptor, _image: CursorImageStatus) {
        // TODO display cursor for each tablet tool
    }
}
