// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    desktop::{layer_map_for_output, space::RenderZindex, Kind, Space, Window},
    input::{
        pointer::{Focus, GrabStartData as PointerGrabStartData},
        Seat,
    },
    output::Output,
    reexports::wayland_protocols::xdg::shell::server::xdg_toplevel::{
        ResizeEdge, State as XdgState,
    },
    utils::{IsAlive, Logical, Point, Rectangle, Serial},
    wayland::{compositor::with_states, shell::xdg::XdgToplevelSurfaceRoleAttributes},
};
use std::{collections::HashSet, sync::Mutex};

use crate::state::State;

mod grabs;
pub use self::grabs::*;

pub const FLOATING_INDEX: u8 = RenderZindex::Shell as u8 + 1;

#[derive(Debug, Default)]
pub struct FloatingLayout {
    pending_windows: Vec<Window>,
    pub windows: HashSet<Window>,
}

#[derive(Default)]
pub struct WindowUserDataInner {
    last_geometry: Rectangle<i32, Logical>,
}
pub type WindowUserData = Mutex<WindowUserDataInner>;

impl FloatingLayout {
    pub fn new() -> FloatingLayout {
        Default::default()
    }

    pub fn map_window(
        &mut self,
        space: &mut Space,
        window: Window,
        seat: &Seat<State>,
        position: impl Into<Option<Point<i32, Logical>>>,
    ) {
        if let Some(output) = super::output_from_seat(Some(seat), space) {
            self.map_window_internal(space, window, &output, position.into());
        } else {
            self.pending_windows.push(window);
        }
    }

    pub fn refresh(&mut self, space: &mut Space) {
        self.pending_windows.retain(|w| w.toplevel().alive());
        if let Some(output) = super::output_from_seat(None, space) {
            for window in std::mem::take(&mut self.pending_windows).into_iter() {
                self.map_window_internal(space, window, &output, None);
            }
        }
        // TODO make sure all windows are still visible on any output or move them
    }

    fn map_window_internal(
        &mut self,
        space: &mut Space,
        window: Window,
        output: &Output,
        position: Option<Point<i32, Logical>>,
    ) {
        let last_geometry = window
            .user_data()
            .get::<WindowUserData>()
            .map(|u| u.lock().unwrap().last_geometry);
        let mut win_geo = window.geometry();

        let layers = layer_map_for_output(&output);
        let geometry = layers.non_exclusive_zone();

        let mut geo_updated = false;
        if let Some(size) = last_geometry.clone().map(|g| g.size) {
            geo_updated = win_geo.size == size;
            win_geo.size = size;
        }
        {
            let (min_size, max_size) = with_states(window.toplevel().wl_surface(), |states| {
                let attrs = states
                    .data_map
                    .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
                    .unwrap()
                    .lock()
                    .unwrap();
                (attrs.min_size, attrs.max_size)
            });
            if win_geo.size.w > geometry.size.w / 3 * 2 {
                // try a more reasonable size
                let mut width = geometry.size.w / 3 * 2;
                if max_size.w != 0 {
                    // don't go larger then the max_size ...
                    width = std::cmp::min(max_size.w, width);
                }
                if min_size.w != 0 {
                    // ... but also don't go smaller than the min_size
                    width = std::cmp::max(min_size.w, width);
                }
                // but no matter the supported sizes, don't be larger than our non-exclusive-zone
                win_geo.size.w = std::cmp::min(width, geometry.size.w);
                geo_updated = true;
            }
            if win_geo.size.h > geometry.size.h / 3 * 2 {
                // try a more reasonable size
                let mut height = geometry.size.h / 3 * 2;
                if max_size.h != 0 {
                    // don't go larger then the max_size ...
                    height = std::cmp::min(max_size.h, height);
                }
                if min_size.h != 0 {
                    // ... but also don't go smaller than the min_size
                    height = std::cmp::max(min_size.h, height);
                }
                // but no matter the supported sizes, don't be larger than our non-exclusive-zone
                win_geo.size.h = std::cmp::min(height, geometry.size.h);
                geo_updated = true;
            }
        }

        let position = position
            .or_else(|| last_geometry.map(|g| g.loc))
            .unwrap_or_else(|| {
                (
                    geometry.loc.x + (geometry.size.w / 2) - (win_geo.size.w / 2) + win_geo.loc.x,
                    geometry.loc.y + (geometry.size.h / 2) - (win_geo.size.h / 2) + win_geo.loc.y,
                )
                    .into()
            });

        #[allow(irrefutable_let_patterns)]
        if let Kind::Xdg(xdg) = &window.toplevel() {
            xdg.with_pending_state(|state| {
                state.states.unset(XdgState::TiledLeft);
                state.states.unset(XdgState::TiledRight);
                state.states.unset(XdgState::TiledTop);
                state.states.unset(XdgState::TiledBottom);
                if geo_updated {
                    state.size = Some(win_geo.size);
                }
            });
            xdg.send_configure();
        }

        space.map_window(&window, position, FLOATING_INDEX, false);
        self.windows.insert(window);
    }

    pub fn unmap_window(&mut self, space: &mut Space, window: &Window) {
        #[allow(irrefutable_let_patterns)]
        let is_maximized = match &window.toplevel() {
            Kind::Xdg(surface) => {
                surface.with_pending_state(|state| state.states.contains(XdgState::Maximized))
            }
        };

        if !is_maximized {
            if let Some(location) = space.window_location(window) {
                let user_data = window.user_data();
                user_data.insert_if_missing(|| WindowUserData::default());
                user_data
                    .get::<WindowUserData>()
                    .unwrap()
                    .lock()
                    .unwrap()
                    .last_geometry = Rectangle::from_loc_and_size(location, window.geometry().size);
            }
        }

        space.unmap_window(window);
        self.pending_windows.retain(|w| w != window);
        self.windows.remove(window);
    }

    pub fn maximize_request(&mut self, space: &mut Space, window: &Window, output: &Output) {
        let layers = layer_map_for_output(&output);
        let geometry = layers.non_exclusive_zone();

        if let Some(location) = space.window_location(window) {
            let user_data = window.user_data();
            user_data.insert_if_missing(|| WindowUserData::default());
            user_data
                .get::<WindowUserData>()
                .unwrap()
                .lock()
                .unwrap()
                .last_geometry = Rectangle::from_loc_and_size(location, window.geometry().size);
        }

        space.map_window(
            &window,
            (geometry.loc.x, geometry.loc.y),
            FLOATING_INDEX,
            true,
        );
        #[allow(irrefutable_let_patterns)]
        if let Kind::Xdg(surface) = &window.toplevel() {
            surface.with_pending_state(|state| {
                state.states.set(XdgState::Maximized);
                state.size = Some(geometry.size);
            });
            window.configure();
        }
    }

    pub fn unmaximize_request(&mut self, space: &mut Space, window: &Window) {
        let last_geometry = window
            .user_data()
            .get::<WindowUserData>()
            .map(|u| u.lock().unwrap().last_geometry);
        match window.toplevel() {
            Kind::Xdg(toplevel) => {
                toplevel.with_pending_state(|state| {
                    state.states.unset(XdgState::Maximized);
                    state.size = last_geometry.map(|g| g.size);
                });
                toplevel.send_configure();
            }
        }
        if let Some(last_location) = last_geometry.map(|g| g.loc) {
            space.map_window(&window, last_location, FLOATING_INDEX, true);
        }
    }

    pub fn resize_request(
        state: &mut State,
        window: &Window,
        seat: &Seat<State>,
        serial: Serial,
        start_data: PointerGrabStartData<State>,
        edges: ResizeEdge,
    ) {
        // it is so stupid, that we have to do this here. TODO: Refactor grabs
        let workspace = state
            .common
            .shell
            .space_for_window_mut(window.toplevel().wl_surface())
            .unwrap();
        let space = &mut workspace.space;

        if let Some(pointer) = seat.get_pointer() {
            let location = space.window_location(&window).unwrap();
            let size = window.geometry().size;

            let grab =
                grabs::ResizeSurfaceGrab::new(start_data, window.clone(), edges, location, size);

            pointer.set_grab(state, grab, serial, Focus::Clear);
        }
    }
}
