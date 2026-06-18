// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    backend::input::{KeyState, Keycode},
    input::{Seat, keyboard::IsolatedKeyboardState},
    wayland::virtual_keyboard::VirtualKeyboardHandler,
};

impl VirtualKeyboardHandler for State {
    fn virtual_keyboard_key(
        &mut self,
        seat: &Seat<State>,
        keyboard_state: &mut IsolatedKeyboardState,
        keycode: Keycode,
        key_state: KeyState,
        _time: u32,
    ) {
        // Route the key through the shortcut filter with the virtual keyboard's own isolated
        // state, so e.g. `Super` triggers compositor shortcuts instead of leaking to the
        // focused client. Mirrors how libei input is handled.
        self.inject_isolated_key(seat, keyboard_state, keycode, key_state);
    }
}
