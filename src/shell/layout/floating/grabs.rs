// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    desktop::{Kind, Window},
    reexports::{
        wayland_protocols::xdg::shell::server::xdg_toplevel,
        wayland_server::DisplayHandle,
    },
    utils::{IsAlive, Logical, Point, Size},
    wayland::{
        compositor::with_states,
        seat::{AxisFrame, PointerGrab, PointerGrabStartData, PointerInnerHandle, MotionEvent, ButtonEvent},
        shell::xdg::{SurfaceCachedState, ToplevelConfigure, XdgToplevelSurfaceRoleAttributes},
        Serial,
    },
};
use crate::utils::prelude::*;
use std::{
    cell::RefCell,
    convert::TryFrom,
    sync::Mutex,
};

pub struct MoveSurfaceGrab {
    start_data: PointerGrabStartData,
    window: Window,
    initial_window_location: Point<i32, Logical>,
    delta: Point<f64, Logical>,
}

impl PointerGrab<State> for MoveSurfaceGrab {
    fn motion(
        &mut self,
        data: &mut State,
        _dh: &DisplayHandle, 
        handle: &mut PointerInnerHandle<'_, State>, 
        event: &MotionEvent
    ) {
        // While the grab is active, no client has pointer focus
        handle.motion(event.location, None, event.serial, event.time);
        self.delta = event.location - self.start_data.location;
        
        if let Some(workspace) = data.common.shell.space_for_surface_mut(self.window.toplevel().wl_surface()) {
            let new_location = (self.initial_window_location.to_f64() + self.delta).to_i32_round();
            workspace.space.map_window(&self.window, new_location, true);
        }
    }

    fn button(
        &mut self,
        _data: &mut State,
        _dh: &DisplayHandle,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &ButtonEvent,
    ) {
        handle.button(event.button, event.state, event.serial, event.time);
        if handle.current_pressed().is_empty() {
            // No more buttons are pressed, release the grab.
            handle.unset_grab(event.serial, event.time);
        }
    }

    fn axis(
        &mut self,
        _data: &mut State,
        _dh: &DisplayHandle,
        handle: &mut PointerInnerHandle<'_, State>,
        details: AxisFrame,
    ) {
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
            delta: (0.0, 0.0).into(),
        }
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
        Self::from_bits(x.into()).unwrap()
    }
}

impl From<ResizeEdge> for xdg_toplevel::ResizeEdge {
    #[inline]
    fn from(x: ResizeEdge) -> Self {
        Self::try_from(x.bits()).unwrap()
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

impl PointerGrab<State> for ResizeSurfaceGrab {
    fn motion(
        &mut self,
        _data: &mut State,
        _dh: &DisplayHandle,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &MotionEvent,
    ) {
        // While the grab is active, no client has pointer focus
        handle.motion(event.location, None, event.serial, event.time);

        // It is impossible to get `min_size` and `max_size` of dead toplevel, so we return early.
        if !self.window.alive() {
            handle.unset_grab(event.serial, event.time);
            return;
        }

        let (mut dx, mut dy) = (event.location - self.start_data.location).into();

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
            with_states(self.window.toplevel().wl_surface(), |states| {
                let data = states.cached_state.current::<SurfaceCachedState>();
                (data.min_size, data.max_size)
            });

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
                xdg.with_pending_state(|state| {
                    state.states.set(xdg_toplevel::State::Resizing);
                    state.size = Some(self.last_window_size);
                });
                xdg.send_configure();
            }
        }
    }

    fn button(
        &mut self,
        _data: &mut State,
        _dh: &DisplayHandle,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &ButtonEvent,
    ) {
        handle.button(event.button, event.state, event.serial, event.time);
        if handle.current_pressed().is_empty() {
            // No more buttons are pressed, release the grab.
            handle.unset_grab(event.serial, event.time);

            // If toplevel is dead, we can't resize it, so we return early.
            if !self.window.alive() {
                return;
            }

            #[allow(irrefutable_let_patterns)]
            if let Kind::Xdg(xdg) = &self.window.toplevel() {
                xdg.with_pending_state(|state| {
                    state.states.unset(xdg_toplevel::State::Resizing);
                    state.size = Some(self.last_window_size);
                });
                xdg.send_configure();
            }

            let mut resize_state = self
                .window
                .user_data()
                .get::<RefCell<ResizeState>>()
                .unwrap()
                .borrow_mut();
            if let ResizeState::Resizing(resize_data) = *resize_state {
                *resize_state = ResizeState::WaitingForFinalAck(resize_data, event.serial);
            } else {
                panic!("invalid resize state: {:?}", resize_state);
            }
        }
    }

    fn axis(
        &mut self,
        _data: &mut State,
        _dh: &DisplayHandle,
        handle: &mut PointerInnerHandle<'_, State>,
        details: AxisFrame,
    ) {
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
        initial_window_location: Point<i32, Logical>,
        initial_window_size: Size<i32, Logical>,
    ) -> ResizeSurfaceGrab {
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
        let surface = window.toplevel().wl_surface();

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
            });

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
        mut location: Point<i32, Logical>,
        size: Size<i32, Logical>,
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
                        if edges.intersects(ResizeEdge::LEFT) {
                            location.x =
                                initial_window_location.x + (initial_window_size.w - size.w);
                        }
                        if edges.intersects(ResizeEdge::TOP) {
                            location.y =
                                initial_window_location.y + (initial_window_size.h - size.h);
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
