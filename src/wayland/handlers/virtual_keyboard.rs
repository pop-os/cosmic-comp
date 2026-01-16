// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::backend::input::KeyState;
use smithay::delegate_virtual_keyboard_manager;
use smithay::input::keyboard::FilterResult;
use smithay::input::keyboard::KeyboardHandle;
use smithay::input::keyboard::Keycode;
use smithay::input::keyboard::xkb::ModMask;
use smithay::utils::SERIAL_COUNTER;
use smithay::wayland::virtual_keyboard::VirtualKeyboardHandler;

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
