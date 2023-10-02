use smithay::{
    backend::input::KeyState,
    input::{
        keyboard::{
            GrabStartData as KeyboardGrabStartData, KeyboardGrab, KeyboardInnerHandle,
            ModifiersState,
        },
        Seat, SeatHandler,
    },
    utils::Serial,
};
use xkbcommon::xkb::Keysym;

use crate::{
    config::{Action, KeyPattern},
    shell::{layout::tiling::NodeDesc, OverviewMode, Trigger},
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
        keycode: u32,
        state: KeyState,
        modifiers: Option<ModifiersState>,
        serial: Serial,
        time: u32,
    ) {
        if self.desc.output.upgrade().is_none()
            || !matches!(&data.common.shell.overview_mode, OverviewMode::Started(Trigger::KeyboardSwap(_, d), _) if d == &self.desc)
        {
            handle.unset_grab(data, serial, false);
            return;
        }

        if state == KeyState::Released {
            return;
        }

        let syms = Vec::from(handle.keysym_handle(keycode).raw_syms());
        let focus_bindings = &data
            .common
            .config
            .static_conf
            .key_bindings
            .iter()
            .filter(|(_, action)| matches!(action, Action::Focus(_)))
            .map(|(pattern, action)| {
                let Action::Focus(direction) = action else { unreachable!() };
                (pattern.key, *direction)
            })
            .collect::<Vec<_>>();
        let Some(direction) = syms.iter().find_map(|sym| focus_bindings.iter().find_map(|(key, direction)| (key.is_some() && sym == key.as_ref().unwrap()).then_some(*direction))) else { return };

        data.handle_action(
            Action::Focus(direction),
            &self.seat,
            serial,
            time,
            KeyPattern {
                modifiers: modifiers.map(Into::into).unwrap_or_default(),
                key: Some(Keysym::new(keycode)),
            },
            None,
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
}
