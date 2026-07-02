// SPDX-License-Identifier: GPL-3.0-only

use crate::{input::InputBackendId, state::State};
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
        // Route the key through the per-source shortcut filter with the virtual keyboard's own
        // isolated state, so e.g. `Super` triggers compositor shortcuts instead of leaking to
        // the focused client. Mirrors how libei input is handled.
        self.inject_isolated_key(
            &InputBackendId::VirtualKeyboard,
            seat,
            keyboard_state,
            keycode,
            key_state,
            true,
        );
    }

    fn virtual_keyboard_destroyed(
        &mut self,
        seat: &Seat<State>,
        keyboard_state: &mut IsolatedKeyboardState,
    ) {
        // Release any keys the virtual keyboard still holds so they don't stick in the focused
        // client, then drop the source's suppressed-key/shortcut bookkeeping.
        let backend_id = InputBackendId::VirtualKeyboard;
        let held = keyboard_state.pressed_keys().collect::<Vec<_>>();
        for keycode in held.into_iter().rev() {
            self.inject_isolated_key(
                &backend_id,
                seat,
                keyboard_state,
                keycode,
                KeyState::Released,
                false,
            );
        }
        self.clear_input_source_state(&backend_id);
    }
}
