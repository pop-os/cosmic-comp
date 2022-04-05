// SPDX-License-Identifier: GPL-3.0-only

use crate::shell::layout::Layout;
use smithay::{
    desktop::{layer_map_for_output, Kind, Space, Window},
    reexports::wayland_protocols::xdg_shell::server::xdg_toplevel::{
        ResizeEdge, State as XdgState,
    },
    wayland::{
        output::Output,
        seat::{PointerGrabStartData, Seat},
        Serial,
    },
};

mod grabs;
pub use self::grabs::*;

#[derive(Debug, Default)]
pub struct FloatingLayout {
    pending_windows: Vec<Window>,
}

impl Layout for FloatingLayout {
    fn map_window<'a>(
        &mut self,
        space: &mut Space,
        window: &Window,
        seat: &Seat,
        _focus_stack: Box<dyn Iterator<Item = &'a Window> + 'a>,
    ) {
        if let Some(output) = super::output_from_seat(Some(seat), space) {
            Self::map_window(space, window, &output)
        } else {
            self.pending_windows.push(window.clone());
        }
    }

    fn refresh(&mut self, space: &mut Space) {
        self.pending_windows.retain(|w| w.toplevel().alive());
        if let Some(output) = super::output_from_seat(None, space) {
            for window in self.pending_windows.drain(..) {
                Self::map_window(space, &window, &output);
            }
        }
        // TODO make sure all windows are still visible on any output or move them
    }

    fn unmap_window(&mut self, space: &mut Space, window: &Window) {
        space.unmap_window(window);
        self.pending_windows.retain(|w| w != window);
    }

    fn maximize_request(&mut self, space: &mut Space, window: &Window, output: &Output) {
        let layers = layer_map_for_output(&output);
        let geometry = layers.non_exclusive_zone();

        space.map_window(&window, (-geometry.loc.x, -geometry.loc.y), true);
        #[allow(irrefutable_let_patterns)]
        if let Kind::Xdg(surface) = &window.toplevel() {
            let ret = surface.with_pending_state(|state| {
                state.states.set(XdgState::Maximized);
                state.size = Some(geometry.size);
            });

            if ret.is_ok() {
                window.configure();
            }
        }
    }

    fn move_request(
        &mut self,
        space: &mut Space,
        window: &Window,
        seat: &Seat,
        serial: Serial,
        start_data: PointerGrabStartData,
    ) {
        if let Some(pointer) = seat.get_pointer() {
            let mut initial_window_location = space.window_location(&window).unwrap();

            #[allow(irrefutable_let_patterns)]
            if let Kind::Xdg(surface) = &window.toplevel() {
                // If surface is maximized then unmaximize it
                if let Some(current_state) = surface.current_state() {
                    if current_state.states.contains(XdgState::Maximized) {
                        let fs_changed = surface.with_pending_state(|state| {
                            state.states.unset(XdgState::Maximized);
                            state.size = None;
                        });

                        if fs_changed.is_ok() {
                            surface.send_configure();

                            // NOTE: In real compositor mouse location should be mapped to a new window size
                            // For example, you could:
                            // 1) transform mouse pointer position from compositor space to window space (location relative)
                            // 2) divide the x coordinate by width of the window to get the percentage
                            //   - 0.0 would be on the far left of the window
                            //   - 0.5 would be in middle of the window
                            //   - 1.0 would be on the far right of the window
                            // 3) multiply the percentage by new window width
                            // 4) by doing that, drag will look a lot more natural
                            //
                            // but for anvil needs setting location to pointer location is fine
                            let pos = pointer.current_location();
                            initial_window_location = (pos.x as i32, pos.y as i32).into();
                        }
                    }
                }
            }

            let grab = MoveSurfaceGrab::new(start_data, window.clone(), initial_window_location);

            pointer.set_grab(grab, serial, 0);
        }
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
        if let Some(pointer) = seat.get_pointer() {
            let location = space.window_location(&window).unwrap();
            let size = window.geometry().size;

            let grab =
                grabs::ResizeSurfaceGrab::new(start_data, window.clone(), edges, location, size);

            pointer.set_grab(grab, serial, 0);
        }
    }
}

impl FloatingLayout {
    pub fn new() -> FloatingLayout {
        Default::default()
    }

    fn map_window(
        space: &mut Space,
        window: &Window,
        output: &Output,
    ) {
        let win_geo = window.bbox();
        let layers = layer_map_for_output(&output);
        let geometry = layers.non_exclusive_zone();

        let position = (
            -geometry.loc.x + (geometry.size.w / 2) - (win_geo.size.w / 2),
            -geometry.loc.y + (geometry.size.h / 2) - (win_geo.size.h / 2),
        );
        #[allow(irrefutable_let_patterns)]
        if let Kind::Xdg(xdg) = &window.toplevel() {
            let ret = xdg.with_pending_state(|state| {
                state.states.unset(XdgState::TiledLeft);
                state.states.unset(XdgState::TiledRight);
                state.states.unset(XdgState::TiledTop);
                state.states.unset(XdgState::TiledBottom);
            });
            if ret.is_ok() {
                xdg.send_configure();
            }
        }
        space.map_window(&window, position, false);
    }
}
