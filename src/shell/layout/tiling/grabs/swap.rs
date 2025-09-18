use cosmic_settings_config::shortcuts;
use smithay::{
    backend::input::{KeyState, Keycode},
    input::{
        keyboard::{
            GrabStartData as KeyboardGrabStartData, KeyboardGrab, KeyboardInnerHandle,
            ModifiersState,
        },
        Seat, SeatHandler,
    },
    utils::Serial,
};

use crate::{
    config::key_bindings::cosmic_modifiers_from_smithay,
    shell::{layout::tiling::NodeDesc, Trigger},
    state::State,
};

pub struct SwapWindowGrab {
    seat: Seat<State>,
    desc: NodeDesc,
}

impl SwapWindowGrab {
    pub fn new(seat: Seat<State>, desc: NodeDesc) -> Self {
        SwapWindowGrab { seat, desc }
    }
}

impl KeyboardGrab<State> for SwapWindowGrab {
    fn input(
        &mut self,
        data: &mut State,
        handle: &mut KeyboardInnerHandle<'_, State>,
        keycode: Keycode,
        state: KeyState,
        modifiers: Option<ModifiersState>,
        serial: Serial,
        time: u32,
    ) {
        if !matches!(&data.common.shell.read().overview_mode.active_trigger(), Some(Trigger::KeyboardSwap(_, d)) if d == &self.desc)
        {
            handle.unset_grab(self, data, serial, false);
            return;
        }

        if state == KeyState::Released {
            return;
        }

        let syms = Vec::from(handle.keysym_handle(keycode).raw_syms());
        let focus_bindings = &data
            .common
            .config
            .shortcuts
            .iter()
            .filter(|(_, action)| matches!(action, shortcuts::Action::Focus(_)))
            .map(|(pattern, action)| {
                let shortcuts::Action::Focus(direction) = action else {
                    unreachable!()
                };
                (pattern.key, *direction)
            })
            .collect::<Vec<_>>();
        let Some(direction) = syms.iter().find_map(|sym| {
            focus_bindings.iter().find_map(|(key, direction)| {
                (key.is_some() && sym == key.as_ref().unwrap()).then_some(*direction)
            })
        }) else {
            return;
        };

        data.handle_shortcut_action(
            shortcuts::Action::Focus(direction),
            &self.seat,
            serial,
            time,
            shortcuts::Binding {
                modifiers: modifiers
                    .map(cosmic_modifiers_from_smithay)
                    .unwrap_or_default(),
                keycode: None,
                key: Some(handle.keysym_handle(keycode).modified_sym()),
                description: None,
            },
            None,
            true,
        );
    }

    fn set_focus(
        &mut self,
        data: &mut State,
        handle: &mut KeyboardInnerHandle<'_, State>,
        focus: Option<<State as SeatHandler>::KeyboardFocus>,
        serial: Serial,
    ) {
        handle.set_focus(data, focus, serial)
    }

    fn start_data(&self) -> &KeyboardGrabStartData<State> {
        &KeyboardGrabStartData { focus: None }
    }

    fn unset(&mut self, _state: &mut State) {}
}
