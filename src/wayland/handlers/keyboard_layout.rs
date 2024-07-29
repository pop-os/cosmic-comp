// SPDX-License-Identifier: GPL-3.0-only

use crate::wayland::protocols::keyboard_layout::{KeyboardLayoutHandler, KeyboardLayoutState};
use crate::{state::State, wayland::protocols::keyboard_layout::delegate_keyboard_layout};

impl KeyboardLayoutHandler for State {
    fn keyboard_layout_state(&mut self) -> &mut KeyboardLayoutState {
        &mut self.common.keyboard_layout_state
    }
}

delegate_keyboard_layout!(State);
