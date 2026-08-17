// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    backend::input::TabletToolDescriptor,
    input::{pointer::CursorImageStatus, tablet::TabletSeatHandler},
    reexports::wayland_server::protocol::wl_surface::WlSurface,
};

impl TabletSeatHandler for State {
    type ToolFocus = WlSurface;

    fn tablet_tool_image(&mut self, _tool: &TabletToolDescriptor, _image: CursorImageStatus) {
        // TODO display cursor for each tablet tool
    }
}
