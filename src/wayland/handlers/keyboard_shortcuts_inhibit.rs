// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    delegate_keyboard_shortcuts_inhibit,
    wayland::keyboard_shortcuts_inhibit::{
        KeyboardShortcutsInhibitHandler, KeyboardShortcutsInhibitState, KeyboardShortcutsInhibitor,
    },
};

impl KeyboardShortcutsInhibitHandler for State {
    fn keyboard_shortcuts_inhibit_state(&mut self) -> &mut KeyboardShortcutsInhibitState {
        &mut self.common.keyboard_shortcuts_inhibit_state
    }

    fn new_inhibitor(&mut self, inhibitor: KeyboardShortcutsInhibitor) {
        // TODO: Restrict what apps can inhibit shortcuts
        inhibitor.activate();
    }
}

delegate_keyboard_shortcuts_inhibit!(State);
