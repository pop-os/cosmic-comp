// SPDX-License-Identifier: GPL-3.0-only
use crate::input::ActiveOutput;
use smithay::{
    desktop::{Space, Window},
    reexports::wayland_protocols::xdg_shell::server::xdg_toplevel::ResizeEdge,
    wayland::{
        compositor::with_states,
        output::Output,
        seat::{PointerGrabStartData, Seat},
        shell::xdg::XdgToplevelSurfaceRoleAttributes,
        Serial,
    },
};
use std::sync::Mutex;

pub mod combined;
pub mod floating;
pub mod tiling;

#[derive(Debug, serde::Deserialize, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Orientation {
    Horizontal,
    Vertical,
}

#[derive(Debug, serde::Deserialize, Clone, Copy, PartialEq, Eq)]
pub enum FocusDirection {
    Left,
    Right,
    Up,
    Down,
}

pub trait Layout {
    fn map_window<'a>(
        &mut self,
        space: &mut Space,
        window: &Window,
        seat: &Seat,
        focus_stack: Box<dyn Iterator<Item = &'a Window> + 'a>,
    ); //working around object safety..
    fn refresh(&mut self, space: &mut Space);
    fn unmap_window(&mut self, space: &mut Space, window: &Window);

    fn move_focus<'a>(
        &mut self,
        direction: FocusDirection,
        seat: &Seat,
        space: &mut Space,
        focus_stack: Box<dyn Iterator<Item = &'a Window> + 'a>,
    ) -> Option<Window> {
        let _ = (direction, seat, space, focus_stack);
        None
    }
    fn update_orientation<'a>(
        &mut self,
        orientation: Orientation,
        seat: &Seat,
        space: &mut Space,
        focus_stack: Box<dyn Iterator<Item = &'a Window> + 'a>,
    ) {
        let _ = (orientation, seat, space, focus_stack);
    }
    fn maximize_request(&mut self, space: &mut Space, window: &Window, output: &Output) {
        let _ = (space, window, output);
    }
    fn move_request(
        &mut self,
        space: &mut Space,
        window: &Window,
        seat: &Seat,
        serial: Serial,
        start_data: PointerGrabStartData,
    ) {
        let _ = (space, window, seat, serial, start_data);
    }
    fn resize_request(
        &mut self,
        space: &mut Space,
        window: &Window,
        seat: &Seat,
        serial: Serial,
        start_data: PointerGrabStartData,
        edges: ResizeEdge,
    ) {
        let _ = (space, window, seat, serial, start_data, edges);
    }
}

pub fn new_default_layout() -> Box<dyn Layout> {
    Box::new(combined::Combined::new(
        tiling::TilingLayout::new(),
        floating::FloatingLayout::new(),
        |window| {
            if let Some(surface) = window.toplevel().get_surface() {
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
                        || (attrs.min_size.w != 0
                            && attrs.min_size.h != 0
                            && attrs.min_size == attrs.max_size)
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
                .unwrap_or(false)
            } else {
                false
            }
        },
    ))
}

fn output_from_seat(seat: Option<&Seat>, space: &Space) -> Option<Output> {
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
