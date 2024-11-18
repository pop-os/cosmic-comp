// SPDX-License-Identifier: GPL-3.0-only

use crate::wayland::protocols::keymap::{KeymapHandler, KeymapState};
use crate::{state::State, wayland::protocols::keymap::delegate_keymap};

impl KeymapHandler for State {
    fn keymap_state(&mut self) -> &mut KeymapState {
        &mut self.common.keymap_state
    }
}

delegate_keymap!(State);
