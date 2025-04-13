// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    backend::input::TabletToolDescriptor, delegate_tablet_manager,
    input::pointer::CursorImageStatus, wayland::tablet_manager::TabletSeatHandler,
};

impl TabletSeatHandler for State {
    fn tablet_tool_image(&mut self, _tool: &TabletToolDescriptor, _image: CursorImageStatus) {
        // TODO display cursor for each tablet tool
    }
}

delegate_tablet_manager!(State);
