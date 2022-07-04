// SPDX-License-Identifier: GPL-3.0-only

use crate::{input::ActiveOutput, state::State};
use smithay::{
    desktop::{Space, Window},
    wayland::{
        compositor::with_states, output::Output, seat::Seat,
        shell::xdg::XdgToplevelSurfaceRoleAttributes,
    },
};
use std::sync::Mutex;

pub mod floating;
pub mod tiling;

#[derive(Debug, serde::Deserialize, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Orientation {
    Horizontal,
    Vertical,
}

pub fn should_be_floating(window: &Window) -> bool {
    let surface = window.toplevel().wl_surface();
    with_states(surface, |states| {
        let attrs = states
            .data_map
            .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
            .unwrap()
            .lock()
            .unwrap();

        // simple heuristic taken from
        // sway/desktop/xdg_shell.c:188 @ 0ee54a52
        if attrs.parent.is_some()
            || (attrs.min_size.w != 0 && attrs.min_size.h != 0 && attrs.min_size == attrs.max_size)
        {
            return true;
        }

        // else take a look at our exceptions
        match (
            attrs.app_id.as_deref().unwrap_or(""),
            attrs.title.as_deref().unwrap_or(""),
        ) {
            ("gcr-prompter", _) => true,
            _ => false,
        }
    })
}

fn output_from_seat(seat: Option<&Seat<State>>, space: &Space) -> Option<Output> {
    seat.and_then(|seat| {
        seat.user_data()
            .get::<ActiveOutput>()
            .map(|active| active.0.borrow().clone())
            .or_else(|| {
                seat.get_pointer()
                    .map(|ptr| space.output_under(ptr.current_location()).next().unwrap())
                    .cloned()
            })
    })
    .or_else(|| space.outputs().next().cloned())
}
