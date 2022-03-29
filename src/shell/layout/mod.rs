// SPDX-License-Identifier: GPL-3.0-only
use crate::input::ActiveOutput;
use smithay::{
    desktop::{Space, Window},
    reexports::wayland_protocols::xdg_shell::server::xdg_toplevel::ResizeEdge,
    wayland::{
        output::Output,
        seat::{PointerGrabStartData, Seat},
        Serial,
    },
};

pub mod combined;
pub mod floating;
pub mod tiling;

#[derive(Debug, serde::Deserialize, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Orientation {
    Horizontal,
    Vertical,
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
    //fn unmap_window(&mut self, space: &mut Space, window: &Window);

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

pub fn new_default_layout(idx: u8) -> Box<dyn Layout> {
    Box::new(combined::Combined::new(
        tiling::TilingLayout::new(idx),
        floating::FloatingLayout,
        |app_id, title| {
            slog_scope::debug!(
                "New filter input: ({}:{})",
                app_id.unwrap_or("<unknown>"),
                title.unwrap_or("<no title>")
            );
            match (app_id.unwrap_or(""), title.unwrap_or("")) {
                ("gcr-prompter", _) => true,
                _ => false,
            }
        },
    ))
}

fn output_from_seat(seat: &Seat, space: &Space) -> Output {
    seat.user_data()
        .get::<ActiveOutput>()
        .map(|active| active.0.borrow().clone())
        .or_else(|| {
            seat.get_pointer()
                .map(|ptr| space.output_under(ptr.current_location()).next().unwrap())
                .cloned()
        })
        .unwrap_or_else(|| space.outputs().next().cloned().unwrap())
}
