// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::{
        element::CosmicMapped, focus::target::PointerFocusTarget, grabs::ResizeEdge, CosmicSurface,
    },
    utils::prelude::*,
};
use smithay::{
    desktop::space::SpaceElement,
    input::pointer::{
        AxisFrame, ButtonEvent, GrabStartData as PointerGrabStartData, MotionEvent, PointerGrab,
        PointerInnerHandle,
    },
    utils::{IsAlive, Logical, Point, Rectangle, Size},
};

/// Information about the resize operation.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct ResizeData {
    /// The edges the surface is being resized with.
    edges: ResizeEdge,
    /// The initial window location.
    initial_window_location: Point<i32, Logical>,
    /// The initial window size (geometry width and height).
    initial_window_size: Size<i32, Logical>,
}

/// State of the resize operation.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ResizeState {
    /// The surface is currently being resized.
    Resizing(ResizeData),
    /// The resize has finished, and the surface needs to commit its final state.
    WaitingForCommit(ResizeData),
}

pub struct ResizeSurfaceGrab {
    start_data: PointerGrabStartData<State>,
    window: CosmicMapped,
    edges: ResizeEdge,
    initial_window_size: Size<i32, Logical>,
    last_window_size: Size<i32, Logical>,
}

impl PointerGrab<State> for ResizeSurfaceGrab {
    fn motion(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<i32, Logical>)>,
        event: &MotionEvent,
    ) {
        // While the grab is active, no client has pointer focus
        handle.motion(data, None, event);

        // It is impossible to get `min_size` and `max_size` of dead toplevel, so we return early.
        if !self.window.alive() {
            handle.unset_grab(data, event.serial, event.time);
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

        let (min_size, max_size) = (self.window.min_size(), self.window.max_size());

        let min_width = min_size.map(|s| s.w).unwrap_or(1);
        let min_height = min_size.map(|s| s.h).unwrap_or(1);
        let max_width = max_size.map(|s| s.w).unwrap_or(i32::max_value());
        let max_height = max_size.map(|s| s.h).unwrap_or(i32::max_value());

        new_window_width = new_window_width.max(min_width).min(max_width);
        new_window_height = new_window_height.max(min_height).min(max_height);

        self.last_window_size = (new_window_width, new_window_height).into();

        self.window.set_resizing(true);
        self.window.set_geometry(Rectangle::from_loc_and_size(
            self.window.geometry().loc,
            self.last_window_size,
        ));
        self.window.configure();
    }

    fn button(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &ButtonEvent,
    ) {
        handle.button(data, event);
        if handle.current_pressed().is_empty() {
            // No more buttons are pressed, release the grab.
            handle.unset_grab(data, event.serial, event.time);

            // If toplevel is dead, we can't resize it, so we return early.
            if !self.window.alive() {
                return;
            }

            self.window.set_resizing(false);
            self.window.set_geometry(Rectangle::from_loc_and_size(
                self.window.geometry().loc,
                self.last_window_size,
            ));
            self.window.configure();

            let mut resize_state = self.window.resize_state.lock().unwrap();
            if let Some(ResizeState::Resizing(resize_data)) = *resize_state {
                *resize_state = Some(ResizeState::WaitingForCommit(resize_data));
            } else {
                panic!("invalid resize state: {:?}", resize_state);
            }
        }
    }

    fn axis(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        details: AxisFrame,
    ) {
        handle.axis(data, details)
    }

    fn start_data(&self) -> &PointerGrabStartData<State> {
        &self.start_data
    }
}

impl ResizeSurfaceGrab {
    pub fn new(
        start_data: PointerGrabStartData<State>,
        mapped: CosmicMapped,
        edges: ResizeEdge,
        initial_window_location: Point<i32, Logical>,
        initial_window_size: Size<i32, Logical>,
    ) -> ResizeSurfaceGrab {
        let resize_state = ResizeState::Resizing(ResizeData {
            edges,
            initial_window_location,
            initial_window_size,
        });

        *mapped.resize_state.lock().unwrap() = Some(resize_state);

        ResizeSurfaceGrab {
            start_data,
            window: mapped,
            edges,
            initial_window_size,
            last_window_size: initial_window_size,
        }
    }

    pub fn apply_resize_to_location(window: CosmicMapped, space: &mut Workspace) {
        if let Some(location) = space.floating_layer.space.element_location(&window) {
            let mut new_location = None;

            let mut resize_state = window.resize_state.lock().unwrap();
            // If the window is being resized by top or left, its location must be adjusted
            // accordingly.
            match *resize_state {
                Some(ResizeState::Resizing(resize_data))
                | Some(ResizeState::WaitingForCommit(resize_data)) => {
                    let ResizeData {
                        edges,
                        initial_window_location,
                        initial_window_size,
                    } = resize_data;

                    if edges.intersects(ResizeEdge::TOP_LEFT) {
                        let size = window.geometry().size;
                        let mut new = location.clone();
                        if edges.intersects(ResizeEdge::LEFT) {
                            new.x = initial_window_location.x + (initial_window_size.w - size.w);
                        }
                        if edges.intersects(ResizeEdge::TOP) {
                            new.y = initial_window_location.y + (initial_window_size.h - size.h);
                        }

                        new_location = Some(new);
                    }
                }
                _ => {}
            };

            // Finish resizing.
            if let Some(ResizeState::WaitingForCommit(_)) = *resize_state {
                if !window.is_resizing().unwrap_or(false) {
                    *resize_state = None;
                }
            }
            std::mem::drop(resize_state);

            if let Some(new_location) = new_location {
                for (window, offset) in window.windows() {
                    if let CosmicSurface::Wayland(window) = window {
                        update_reactive_popups(
                            &window,
                            new_location + offset,
                            space.floating_layer.space.outputs(),
                        );
                    }
                }
                let mut geometry = window.geometry();
                geometry.loc += location - new_location;
                let _ = window.set_geometry(geometry);
                space
                    .floating_layer
                    .space
                    .map_element(window, new_location, false);
            }
        }
    }
}
