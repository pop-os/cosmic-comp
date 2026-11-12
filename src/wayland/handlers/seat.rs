// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::Devices,
    shell::focus::target::{KeyboardFocusTarget, PointerFocusTarget},
    state::State,
    utils::prelude::{OutputExt, RectGlobalExt, SeatExt},
};
use smithay::{
    desktop::utils::output_update,
    input::{
        Seat, SeatHandler, SeatState,
        keyboard::LedState,
        pointer::{CursorImageAttributes, CursorImageStatus},
    },
    output::Output,
    utils::Rectangle,
    wayland::compositor::with_states,
};
use std::sync::Mutex;

impl SeatHandler for State {
    type KeyboardFocus = KeyboardFocusTarget;
    type PointerFocus = PointerFocusTarget;
    type TouchFocus = PointerFocusTarget;

    fn seat_state(&mut self) -> &mut SeatState<Self> {
        &mut self.common.seat_state
    }

    fn cursor_image(&mut self, seat: &smithay::input::Seat<Self>, image: CursorImageStatus) {
        seat.set_cursor_image_status(image);
        update_cursor_surface_outputs(seat, self.common.shell.read().outputs());
    }

    fn focus_changed(
        &mut self,
        _seat: &smithay::input::Seat<Self>,
        _focused: Option<&Self::KeyboardFocus>,
    ) {
    }

    fn led_state_changed(&mut self, seat: &smithay::input::Seat<Self>, led_state: LedState) {
        let userdata = seat.user_data();
        let devices = userdata.get::<Devices>().unwrap();
        devices.update_led_state(led_state);
    }
}

/// Send `wl_surface.enter`/`leave` to a client-drawn cursor surface, so clients that
/// pick their cursor buffer scale from the entered outputs (GTK3) render at the right scale.
pub fn update_cursor_surface_outputs<'a>(
    seat: &Seat<State>,
    outputs: impl IntoIterator<Item = &'a Output>,
) {
    let CursorImageStatus::Surface(surface) = seat.cursor_image_status() else {
        return;
    };
    let Some(pointer) = seat.get_pointer() else {
        return;
    };
    let hotspot = with_states(&surface, |states| {
        states
            .data_map
            .get::<Mutex<CursorImageAttributes>>()
            .map(|attrs| attrs.lock().unwrap().hotspot)
    })
    .unwrap_or_default();
    let origin = pointer.current_location().to_i32_round() - hotspot;
    for output in outputs {
        let geometry = output.geometry().as_logical();
        // `output_update` takes the overlap relative to the surface
        let overlap = Rectangle::new(geometry.loc - origin, geometry.size);
        output_update(output, Some(overlap), &surface);
    }
}
