// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    backend::input::KeyState,
    delegate_virtual_keyboard_manager,
    input::keyboard::{FilterResult, KeyboardHandle, Keycode, xkb::ModMask},
    utils::SERIAL_COUNTER,
    wayland::virtual_keyboard::VirtualKeyboardHandler,
};

impl VirtualKeyboardHandler for State {
    fn on_keyboard_event(
        &mut self,
        keycode: Keycode,
        state: KeyState,
        time: u32,
        keyboard: KeyboardHandle<Self>,
    ) {
        let serial = SERIAL_COUNTER.next_serial();
        keyboard.input(self, keycode, state, serial, time, |_, _, _| {
            FilterResult::Forward::<bool>
        });
    }
    fn on_keyboard_modifiers(
        &mut self,
        _depressed_mods: ModMask,
        _latched_mods: ModMask,
        _locked_mods: ModMask,
        _keyboard: KeyboardHandle<Self>,
    ) {
    }
}

delegate_virtual_keyboard_manager!(State);
