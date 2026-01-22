// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    delegate_virtual_keyboard_manager, wayland::virtual_keyboard::VirtualKeyboardHandler,
};

impl VirtualKeyboardHandler for State {
    fn on_keyboard_event(
        &mut self,
        keycode: xkbcommon::xkb::Keycode,
        state: smithay::backend::input::KeyState,
        time: u32,
        keyboard: smithay::input::keyboard::KeyboardHandle<Self>,
    ) {
        todo!()
    }

    fn on_keyboard_modifiers(
        &mut self,
        depressed_mods: xkbcommon::xkb::ModMask,
        latched_mods: xkbcommon::xkb::ModMask,
        locked_mods: xkbcommon::xkb::ModMask,
        keyboard: smithay::input::keyboard::KeyboardHandle<Self>,
    ) {
        todo!()
    }
}

delegate_virtual_keyboard_manager!(State);
