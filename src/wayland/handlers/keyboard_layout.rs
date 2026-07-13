// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use crate::wayland::protocols::keyboard_layout::{KeyboardLayoutHandler, KeyboardLayoutState};

impl KeyboardLayoutHandler for State {
    fn keyboard_layout_state(&mut self) -> &mut KeyboardLayoutState {
        &mut self.common.keyboard_layout_state
    }
}
