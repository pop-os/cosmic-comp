// SPDX-License-Identifier: GPL-3.0-only

use crate::{shell::focus::target::PointerFocusTarget, state::State};
use smithay::{
    backend::input::TabletToolDescriptor, input::pointer::CursorImageStatus,
    input::tablet::TabletSeatHandler,
};

impl TabletSeatHandler for State {
    type ToolFocus = PointerFocusTarget;

    fn tablet_tool_image(&mut self, _tool: &TabletToolDescriptor, _image: CursorImageStatus) {
        // TODO display cursor for each tablet tool
    }
}
