// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    desktop::{Kind, Window},
    reexports::{
        wayland_protocols::xdg_shell::server::xdg_toplevel,
        wayland_server::protocol::{wl_pointer::ButtonState, wl_surface},
    },
    utils::{Logical, Point, Rectangle, Size},
    wayland::{
        compositor::with_states,
        seat::{AxisFrame, PointerGrab, PointerGrabStartData, PointerInnerHandle},
        shell::xdg::{SurfaceCachedState, ToplevelConfigure, XdgToplevelSurfaceRoleAttributes},
        Serial,
    },
};
use std::{cell::RefCell, sync::Mutex};

#[derive(Debug, Default)]
struct MoveData {
    new_location: Point<i32, Logical>,
}

pub struct MoveSurfaceGrab {
    start_data: PointerGrabStartData,
    window: Window,
    initial_window_location: Point<i32, Logical>,
}

impl PointerGrab for MoveSurfaceGrab {
    fn motion(
        &mut self,
        _handle: &mut PointerInnerHandle<'_>,
        location: Point<f64, Logical>,
        _focus: Option<(wl_surface::WlSurface, Point<i32, Logical>)>,
        _serial: Serial,
        _time: u32,
    ) {
        let delta = location - self.start_data.location;
        let new_location = self.initial_window_location.to_f64() + delta;
        self.window
            .user_data()
            .insert_if_missing(|| RefCell::<Option<MoveData>>::new(None));
        let data = self
            .window
            .user_data()
            .get::<RefCell<Option<MoveData>>>()
            .unwrap();
        *data.borrow_mut() = Some(MoveData {
            new_location: new_location.to_i32_round(),
        });
    }

    fn button(
        &mut self,
        handle: &mut PointerInnerHandle<'_>,
        button: u32,
        state: ButtonState,
        serial: Serial,
        time: u32,
    ) {
        handle.button(button, state, serial, time);
        if handle.current_pressed().is_empty() {
            // No more buttons are pressed, release the grab.
            handle.unset_grab(serial, time);
        }
    }

    fn axis(&mut self, handle: &mut PointerInnerHandle<'_>, details: AxisFrame) {
        handle.axis(details)
    }

    fn start_data(&self) -> &PointerGrabStartData {
        &self.start_data
    }
}

impl MoveSurfaceGrab {
    pub fn new(
        start_data: PointerGrabStartData,
        window: Window,
        initial_window_location: Point<i32, Logical>,
    ) -> MoveSurfaceGrab {
        MoveSurfaceGrab {
            start_data,
            window,
            initial_window_location,
        }
    }

    pub fn apply_move_state(window: &Window) -> Option<Point<i32, Logical>> {
        window
            .user_data()
            .get::<RefCell<Option<MoveData>>>()
            .and_then(|opt| {
                opt.borrow_mut()
                    .take()
                    .map(|move_data| move_data.new_location)
            })
    }
}

bitflags::bitflags! {
    struct ResizeEdge: u32 {
        const NONE = 0;
        const TOP = 1;
        const BOTTOM = 2;
        const LEFT = 4;
        const TOP_LEFT = 5;
        const BOTTOM_LEFT = 6;
        const RIGHT = 8;
        const TOP_RIGHT = 9;
        const BOTTOM_RIGHT = 10;
    }
}

impl From<xdg_toplevel::ResizeEdge> for ResizeEdge {
    #[inline]
    fn from(x: xdg_toplevel::ResizeEdge) -> Self {
        Self::from_bits(x.to_raw()).unwrap()
    }
}

impl From<ResizeEdge> for xdg_toplevel::ResizeEdge {
    #[inline]
    fn from(x: ResizeEdge) -> Self {
        Self::from_raw(x.bits()).unwrap()
    }
}

/// Information about the resize operation.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct ResizeData {
    /// The edges the surface is being resized with.
    edges: ResizeEdge,
    /// The initial window location.
    initial_window_location: Point<i32, Logical>,
    /// The initial window size (geometry width and height).
    initial_window_size: Size<i32, Logical>,
}

/// State of the resize operation.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum ResizeState {
    /// The surface is not being resized.
    NotResizing,
    /// The surface is currently being resized.
    Resizing(ResizeData),
    /// The resize has finished, and the surface needs to ack the final configure.
    WaitingForFinalAck(ResizeData, Serial),
    /// The resize has finished, and the surface needs to commit its final state.
    WaitingForCommit(ResizeData),
}

impl Default for ResizeState {
    fn default() -> Self {
        ResizeState::NotResizing
    }
}

pub struct ResizeSurfaceGrab {
    start_data: PointerGrabStartData,
    window: Window,
    edges: ResizeEdge,
    initial_window_size: Size<i32, Logical>,
    last_window_size: Size<i32, Logical>,
}

impl PointerGrab for ResizeSurfaceGrab {
    fn motion(
        &mut self,
        handle: &mut PointerInnerHandle<'_>,
        location: Point<f64, Logical>,
        _focus: Option<(wl_surface::WlSurface, Point<i32, Logical>)>,
        serial: Serial,
        time: u32,
    ) {
        // It is impossible to get `min_size` and `max_size` of dead toplevel, so we return early.
        if !self.window.toplevel().alive() | self.window.toplevel().get_surface().is_none() {
            handle.unset_grab(serial, time);
            return;
        }

        let (mut dx, mut dy) = (location - self.start_data.location).into();

        let mut new_window_width = self.initial_window_size.w;
        let mut new_window_height = self.initial_window_size.h;

        let left_right = ResizeEdge::LEFT | ResizeEdge::RIGHT;
        let top_bottom = ResizeEdge::TOP | ResizeEdge::BOTTOM;

        if self.edges.intersects(left_right) {
            if self.edges.intersects(ResizeEdge::LEFT) {
                dx = -dx;
            }

            new_window_width = (self.initial_window_size.w as f64 + dx) as i32;
        }

        if self.edges.intersects(top_bottom) {
            if self.edges.intersects(ResizeEdge::TOP) {
                dy = -dy;
            }

            new_window_height = (self.initial_window_size.h as f64 + dy) as i32;
        }

        let (min_size, max_size) =
            with_states(self.window.toplevel().get_surface().unwrap(), |states| {
                let data = states.cached_state.current::<SurfaceCachedState>();
                (data.min_size, data.max_size)
            })
            .unwrap();

        let min_width = min_size.w.max(1);
        let min_height = min_size.h.max(1);
        let max_width = if max_size.w == 0 {
            i32::max_value()
        } else {
            max_size.w
        };
        let max_height = if max_size.h == 0 {
            i32::max_value()
        } else {
            max_size.h
        };

        new_window_width = new_window_width.max(min_width).min(max_width);
        new_window_height = new_window_height.max(min_height).min(max_height);

        self.last_window_size = (new_window_width, new_window_height).into();

        match &self.window.toplevel() {
            Kind::Xdg(xdg) => {
                let ret = xdg.with_pending_state(|state| {
                    state.states.set(xdg_toplevel::State::Resizing);
                    state.size = Some(self.last_window_size);
                });
                if ret.is_ok() {
                    xdg.send_configure();
                }
            }
        }
    }

    fn button(
        &mut self,
        handle: &mut PointerInnerHandle<'_>,
        button: u32,
        state: ButtonState,
        serial: Serial,
        time: u32,
    ) {
        handle.button(button, state, serial, time);
        if handle.current_pressed().is_empty() {
            // No more buttons are pressed, release the grab.
            handle.unset_grab(serial, time);

            // If toplevel is dead, we can't resize it, so we return early.
            if !self.window.toplevel().alive() | self.window.toplevel().get_surface().is_none() {
                return;
            }

            #[allow(irrefutable_let_patterns)]
            if let Kind::Xdg(xdg) = &self.window.toplevel() {
                let ret = xdg.with_pending_state(|state| {
                    state.states.unset(xdg_toplevel::State::Resizing);
                    state.size = Some(self.last_window_size);
                });
                if ret.is_ok() {
                    xdg.send_configure();
                }
            }

            let mut resize_state = self
                .window
                .user_data()
                .get::<RefCell<ResizeState>>()
                .unwrap()
                .borrow_mut();
            if let ResizeState::Resizing(resize_data) = *resize_state {
                *resize_state = ResizeState::WaitingForFinalAck(resize_data, serial);
            } else {
                panic!("invalid resize state: {:?}", resize_state);
            }
        }
    }

    fn axis(&mut self, handle: &mut PointerInnerHandle<'_>, details: AxisFrame) {
        handle.axis(details)
    }

    fn start_data(&self) -> &PointerGrabStartData {
        &self.start_data
    }
}

impl ResizeSurfaceGrab {
    pub fn new(
        start_data: PointerGrabStartData,
        window: Window,
        edges: xdg_toplevel::ResizeEdge,
        initial_window_geometry: Rectangle<i32, Logical>,
    ) -> ResizeSurfaceGrab {
        let (initial_window_location, initial_window_size) =
            (initial_window_geometry.loc, initial_window_geometry.size);
        let resize_state = ResizeState::Resizing(ResizeData {
            edges: edges.into(),
            initial_window_location,
            initial_window_size,
        });

        window
            .user_data()
            .insert_if_missing(|| RefCell::new(ResizeState::default()));
        *window
            .user_data()
            .get::<RefCell<ResizeState>>()
            .unwrap()
            .borrow_mut() = resize_state;

        ResizeSurfaceGrab {
            start_data,
            window,
            edges: edges.into(),
            initial_window_size,
            last_window_size: initial_window_size,
        }
    }

    pub fn ack_configure(window: &Window, configure: ToplevelConfigure) {
        let surface = if let Some(surface) = window.toplevel().get_surface() {
            surface
        } else {
            return;
        };

        let waiting_for_serial =
            if let Some(data) = window.user_data().get::<RefCell<ResizeState>>() {
                if let ResizeState::WaitingForFinalAck(_, serial) = *data.borrow() {
                    Some(serial)
                } else {
                    None
                }
            } else {
                None
            };

        if let Some(serial) = waiting_for_serial {
            // When the resize grab is released the surface
            // resize state will be set to WaitingForFinalAck
            // and the client will receive a configure request
            // without the resize state to inform the client
            // resizing has finished. Here we will wait for
            // the client to acknowledge the end of the
            // resizing. To check if the surface was resizing
            // before sending the configure we need to use
            // the current state as the received acknowledge
            // will no longer have the resize state set
            let is_resizing = with_states(&surface, |states| {
                states
                    .data_map
                    .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
                    .unwrap()
                    .lock()
                    .unwrap()
                    .current
                    .states
                    .contains(xdg_toplevel::State::Resizing)
            })
            .unwrap();

            if configure.serial >= serial && is_resizing {
                let mut resize_state = window
                    .user_data()
                    .get::<RefCell<ResizeState>>()
                    .unwrap()
                    .borrow_mut();
                if let ResizeState::WaitingForFinalAck(resize_data, _) = *resize_state {
                    *resize_state = ResizeState::WaitingForCommit(resize_data);
                } else {
                    unreachable!()
                }
            }
        }
    }

    pub fn apply_resize_state(
        window: &Window,
        geometry: Rectangle<i32, Logical>,
    ) -> Option<Point<i32, Logical>> {
        let mut new_location = None;

        if let Some(resize_state) = window.user_data().get::<RefCell<ResizeState>>() {
            let mut resize_state = resize_state.borrow_mut();

            // If the window is being resized by top or left, its location must be adjusted
            // accordingly.
            match *resize_state {
                ResizeState::Resizing(resize_data)
                | ResizeState::WaitingForFinalAck(resize_data, _)
                | ResizeState::WaitingForCommit(resize_data) => {
                    let ResizeData {
                        edges,
                        initial_window_location,
                        initial_window_size,
                    } = resize_data;

                    if edges.intersects(ResizeEdge::TOP_LEFT) {
                        let mut location = geometry.loc;

                        if edges.intersects(ResizeEdge::LEFT) {
                            location.x = initial_window_location.x
                                + (initial_window_size.w - geometry.size.w);
                        }
                        if edges.intersects(ResizeEdge::TOP) {
                            location.y = initial_window_location.y
                                + (initial_window_size.h - geometry.size.h);
                        }

                        new_location = Some(location);
                    }
                }
                ResizeState::NotResizing => (),
            }

            // Finish resizing.
            if let ResizeState::WaitingForCommit(_) = *resize_state {
                *resize_state = ResizeState::NotResizing;
            }
        }

        new_location
    }
}
