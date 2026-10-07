// SPDX-License-Identifier: GPL-3.0-only

use crate::{config::xkb_config_to_wl, input::InputBackendId, state::State};
use smithay::{
    backend::input::{InputBackend, InputEvent},
    input::Seat,
    wayland::virtual_keyboard::{
        VirtualKeyboardBackend, VirtualKeyboardDevice, VirtualKeyboardHandler,
        VirtualKeyboardSpecialEvent,
    },
};
use std::{any::Any, cell::RefCell, sync::Arc};
use tracing::error;

#[derive(Default)]
struct SeatVirtualKeyboardData {
    active_virtual_keyboard_keymap: RefCell<Option<Arc<str>>>,
}

// Clear stored virtual keyboard keymap, before updating keyboard's keymap
pub fn clear_active_virtual_keyboard_keymap(seat: &Seat<State>) {
    let data = seat
        .user_data()
        .get_or_insert(SeatVirtualKeyboardData::default);
    *data.active_virtual_keyboard_keymap.borrow_mut() = None;
}

pub fn update_virtual_keyboard_keymap<B: InputBackend>(
    state: &mut State,
    seat: &Seat<State>,
    device: &B::Device,
) where
    <B as InputBackend>::Device: 'static,
{
    let Some(keyboard) = seat.get_keyboard() else {
        return;
    };

    let data = seat
        .user_data()
        .get_or_insert(SeatVirtualKeyboardData::default);

    let virtual_keyboard_keymap =
        <dyn Any>::downcast_ref::<VirtualKeyboardDevice>(device).and_then(|d| d.keymap());

    let mut active_virtual_keyboard_keymap = data.active_virtual_keyboard_keymap.borrow_mut();

    match (virtual_keyboard_keymap, &*active_virtual_keyboard_keymap) {
        // No virtual keyboard keymap is active or required
        (None, None) => {}
        // Same virtual keyboard keymap is already active
        (Some(keymap), Some(active_keymap)) if Arc::ptr_eq(&keymap, active_keymap) => {}
        // Activate new virtual keyboard keymap
        (Some(keymap), _) => {
            if let Err(err) = keyboard.set_keymap_from_string(state, keymap.to_string()) {
                error!(?err, "Failed to apply virtual keyboard keymap");
            }
            *active_virtual_keyboard_keymap = Some(keymap);
        }
        // Restore system keymap from config
        (None, _) => {
            let conf = state.common.config.xkb_config();
            if let Err(err) = keyboard.set_xkb_config(state, xkb_config_to_wl(&conf)) {
                error!(?err, "Failed to load provided xkb config");
            }
            *active_virtual_keyboard_keymap = None;
        }
    }
}

impl VirtualKeyboardHandler for State {
    fn process_virtual_keyboard_event(&mut self, event: InputEvent<VirtualKeyboardBackend>) {
        if let InputEvent::Special(special) = &event {
            match special {
                // Handled by `update_virtual_keyboard_keymap`
                VirtualKeyboardSpecialEvent::KeymapChanged { device: _ } => {}
                VirtualKeyboardSpecialEvent::Modifiers {
                    device: _,
                    mods_depressed,
                    mods_latched,
                    mods_locked,
                    group,
                } => {
                    let Some(keyboard) =
                        self.common.shell.read().seats.last_active().get_keyboard()
                    else {
                        return;
                    };
                    keyboard.with_xkb_state(self, |mut context| {
                        context.set_modifier_mask(
                            *mods_depressed,
                            *mods_latched,
                            *mods_locked,
                            *group,
                        )
                    });
                }
            }
        };
        self.process_input_event(event, InputBackendId::VirtualKeyboard);
    }
}
