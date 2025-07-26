// SPDX-License-Identifier: GPL-3.0-only

use std::sync::atomic::{AtomicBool, Ordering};

use crate::{
    backend::render::cursor::CursorState,
    shell::{
        element::CosmicMapped,
        focus::target::PointerFocusTarget,
        grabs::{GrabStartData, ReleaseMode, ResizeEdge},
    },
    utils::prelude::*,
};
use smithay::{
    backend::input::ButtonState,
    desktop::{space::SpaceElement, WindowSurface},
    input::{
        pointer::{
            AxisFrame, ButtonEvent, CursorIcon, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent,
            GrabStartData as PointerGrabStartData, MotionEvent, PointerGrab, PointerInnerHandle,
            RelativeMotionEvent,
        },
        touch::{
            DownEvent, GrabStartData as TouchGrabStartData, MotionEvent as TouchMotionEvent,
            OrientationEvent, ShapeEvent, TouchGrab, TouchInnerHandle, UpEvent,
        },
        Seat,
    },
    output::Output,
    utils::{IsAlive, Logical, Point, Rectangle, Serial, Size},
};
use tracing::debug;

/// Information about the resize operation.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct ResizeData {
    /// The edges the surface is being resized with.
    pub edges: ResizeEdge,
    /// The initial window location.
    pub initial_window_location: Point<i32, Local>,
    /// The initial window size (geometry width and height).
    pub initial_window_size: Size<i32, Logical>,
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
    start_data: GrabStartData,
    seat: Seat<State>,
    window: CosmicMapped,
    edges: ResizeEdge,
    output: Output,
    edge_snap_threshold: u32,
    initial_window_location: Point<i32, Local>,
    initial_window_size: Size<i32, Logical>,
    last_window_size: Size<i32, Logical>,
    release: ReleaseMode,
}

impl ResizeSurfaceGrab {
    // Returns `true` if grab should be unset
    fn update_location(&mut self, location: Point<f64, Global>) -> bool {
        // It is impossible to get `min_size` and `max_size` of dead toplevel, so we return early.
        if !self.window.alive() {
            self.seat
                .user_data()
                .get::<ResizeGrabMarker>()
                .unwrap()
                .0
                .store(false, Ordering::SeqCst);
            return true;
        }

        let (mut dx, mut dy) = (location - self.start_data.location().as_global()).into();

        let mut new_window_width = self.initial_window_size.w;
        let mut new_window_height = self.initial_window_size.h;

        let left_right = ResizeEdge::LEFT | ResizeEdge::RIGHT;
        let top_bottom = ResizeEdge::TOP | ResizeEdge::BOTTOM;

        let mut location_diff = Point::default();

        if self.edges.intersects(left_right) {
            if self.edges.intersects(ResizeEdge::LEFT) {
                location_diff.x += dx;
                dx = -dx;
            }

            new_window_width = (self.initial_window_size.w as f64 + dx) as i32;

            // If the resizing vertical edge is close to our output's edge in the same direction, snap to it.
            let output_geom = self.output.geometry().to_local(&self.output);
            if self.edges.intersects(ResizeEdge::LEFT) {
                if ((self.initial_window_location.x - dx as i32 - output_geom.loc.x).abs() as u32)
                    < self.edge_snap_threshold
                {
                    new_window_width = self.initial_window_size.w - output_geom.loc.x
                        + self.initial_window_location.x;
                }
            } else {
                if ((self.initial_window_location.x + self.initial_window_size.w + dx as i32
                    - output_geom.loc.x
                    - output_geom.size.w)
                    .abs() as u32)
                    < self.edge_snap_threshold
                {
                    new_window_width =
                        output_geom.loc.x - self.initial_window_location.x + output_geom.size.w;
                }
            }
        }

        if self.edges.intersects(top_bottom) {
            if self.edges.intersects(ResizeEdge::TOP) {
                location_diff.y += dy;
                dy = -dy;
            }

            new_window_height = (self.initial_window_size.h as f64 + dy) as i32;

            // If the resizing horizontal edge is close to our output's edge in the same direction, snap to it.
            let output_geom = self.output.geometry().to_local(&self.output);
            if self.edges.intersects(ResizeEdge::TOP) {
                if ((self.initial_window_location.y - dy as i32 - output_geom.loc.y).abs() as u32)
                    < self.edge_snap_threshold
                {
                    new_window_height = self.initial_window_size.h - output_geom.loc.y
                        + self.initial_window_location.y;
                }
            } else {
                if ((self.initial_window_location.y + self.initial_window_size.h + dy as i32
                    - output_geom.loc.y
                    - output_geom.size.h)
                    .abs() as u32)
                    < self.edge_snap_threshold
                {
                    new_window_height =
                        output_geom.loc.y - self.initial_window_location.y + output_geom.size.h;
                }
            }
        }

        let (min_size, max_size) = (self.window.min_size(), self.window.max_size());

        let min_width = min_size.map(|s| s.w).unwrap_or(360);
        let min_height = min_size.map(|s| s.h).unwrap_or(240);
        let max_width = max_size.map(|s| s.w).unwrap_or(i32::max_value());
        let max_height = max_size.map(|s| s.h).unwrap_or(i32::max_value());

        new_window_width = new_window_width.max(min_width).min(max_width);
        new_window_height = new_window_height.max(min_height).min(max_height);

        self.last_window_size = (new_window_width, new_window_height).into();

        let win_loc =
            (self.initial_window_location + location_diff.to_i32_round()).to_global(&self.output);
        self.window.set_resizing(true);
        self.window
            .set_geometry(Rectangle::new(win_loc, self.last_window_size.as_global()));
        if self.window.latest_size_committed() {
            self.window.configure();
        }

        false
    }

    pub fn is_touch_grab(&self) -> bool {
        match self.start_data {
            GrabStartData::Touch(_) => true,
            GrabStartData::Pointer(_) => false,
        }
    }
}

impl PointerGrab<State> for ResizeSurfaceGrab {
    fn motion(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &MotionEvent,
    ) {
        // While the grab is active, no client has pointer focus
        handle.motion(data, None, event);

        if self.update_location(event.location.as_global()) {
            handle.unset_grab(self, data, event.serial, event.time, true);
        }
    }

    fn relative_motion(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &RelativeMotionEvent,
    ) {
        // While the grab is active, no client has pointer focus
        handle.relative_motion(state, None, event);
    }

    fn button(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &ButtonEvent,
    ) {
        handle.button(data, event);
        match self.release {
            ReleaseMode::NoMouseButtons => {
                if handle.current_pressed().is_empty() {
                    handle.unset_grab(self, data, event.serial, event.time, true);
                }
            }
            ReleaseMode::Click => {
                if event.state == ButtonState::Pressed {
                    handle.unset_grab(self, data, event.serial, event.time, true);
                }
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

    fn frame(&mut self, data: &mut State, handle: &mut PointerInnerHandle<'_, State>) {
        handle.frame(data)
    }

    fn gesture_swipe_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeBeginEvent,
    ) {
        handle.gesture_swipe_begin(data, event)
    }

    fn gesture_swipe_update(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeUpdateEvent,
    ) {
        handle.gesture_swipe_update(data, event)
    }

    fn gesture_swipe_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeEndEvent,
    ) {
        handle.gesture_swipe_end(data, event)
    }

    fn gesture_pinch_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchBeginEvent,
    ) {
        handle.gesture_pinch_begin(data, event)
    }

    fn gesture_pinch_update(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchUpdateEvent,
    ) {
        handle.gesture_pinch_update(data, event)
    }

    fn gesture_pinch_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchEndEvent,
    ) {
        handle.gesture_pinch_end(data, event)
    }

    fn gesture_hold_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureHoldBeginEvent,
    ) {
        handle.gesture_hold_begin(data, event)
    }

    fn gesture_hold_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureHoldEndEvent,
    ) {
        handle.gesture_hold_end(data, event)
    }

    fn start_data(&self) -> &PointerGrabStartData<State> {
        match &self.start_data {
            GrabStartData::Pointer(start_data) => start_data,
            _ => unreachable!(),
        }
    }

    fn unset(&mut self, _data: &mut State) {
        self.ungrab();
    }
}

impl TouchGrab<State> for ResizeSurfaceGrab {
    fn down(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &DownEvent,
        seq: Serial,
    ) {
        handle.down(data, None, event, seq)
    }

    fn up(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &UpEvent,
        seq: Serial,
    ) {
        if event.slot == <Self as TouchGrab<State>>::start_data(self).slot {
            handle.unset_grab(self, data);
        }

        handle.up(data, event, seq);
    }

    fn motion(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &TouchMotionEvent,
        seq: Serial,
    ) {
        if event.slot == <Self as TouchGrab<State>>::start_data(self).slot {
            if self.update_location(event.location.as_global()) {
                handle.unset_grab(self, data);
            }
        }

        handle.motion(data, None, event, seq);
    }

    fn frame(&mut self, data: &mut State, handle: &mut TouchInnerHandle<'_, State>, seq: Serial) {
        handle.frame(data, seq)
    }

    fn cancel(&mut self, data: &mut State, handle: &mut TouchInnerHandle<'_, State>, _seq: Serial) {
        handle.unset_grab(self, data);
    }

    fn shape(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &ShapeEvent,
        seq: Serial,
    ) {
        handle.shape(data, event, seq)
    }

    fn orientation(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &OrientationEvent,
        seq: Serial,
    ) {
        handle.orientation(data, event, seq)
    }

    fn start_data(&self) -> &TouchGrabStartData<State> {
        match &self.start_data {
            GrabStartData::Touch(start_data) => start_data,
            _ => unreachable!(),
        }
    }

    fn unset(&mut self, _data: &mut State) {
        self.ungrab();
    }
}

pub struct ResizeGrabMarker(AtomicBool);

impl ResizeGrabMarker {
    pub fn get(&self) -> bool {
        self.0.load(Ordering::SeqCst)
    }
}

impl ResizeSurfaceGrab {
    pub fn new(
        start_data: GrabStartData,
        mapped: CosmicMapped,
        edges: ResizeEdge,
        output: Output,
        edge_snap_threshold: u32,
        initial_window_location: Point<i32, Local>,
        initial_window_size: Size<i32, Logical>,
        seat: &Seat<State>,
        release: ReleaseMode,
    ) -> ResizeSurfaceGrab {
        let resize_state = ResizeState::Resizing(ResizeData {
            edges,
            initial_window_location,
            initial_window_size,
        });

        *mapped.resize_state.lock().unwrap() = Some(resize_state);
        seat.user_data()
            .get_or_insert::<ResizeGrabMarker, _>(|| ResizeGrabMarker(AtomicBool::new(true)))
            .0
            .store(true, Ordering::SeqCst);

        let shape = match edges {
            ResizeEdge::TOP_LEFT => Some(CursorIcon::NwResize),
            ResizeEdge::TOP_RIGHT => Some(CursorIcon::NeResize),
            ResizeEdge::BOTTOM_LEFT => Some(CursorIcon::SwResize),
            ResizeEdge::BOTTOM_RIGHT => Some(CursorIcon::SeResize),
            ResizeEdge::TOP => Some(CursorIcon::NResize),
            ResizeEdge::RIGHT => Some(CursorIcon::EResize),
            ResizeEdge::BOTTOM => Some(CursorIcon::SResize),
            ResizeEdge::LEFT => Some(CursorIcon::WResize),
            _ => None,
        };
        if let Some(shape) = shape {
            let cursor_state = seat.user_data().get::<CursorState>().unwrap();
            cursor_state.lock().unwrap().set_shape(shape);
        }

        ResizeSurfaceGrab {
            start_data,
            seat: seat.clone(),
            window: mapped,
            edges,
            output,
            initial_window_location,
            initial_window_size,
            last_window_size: initial_window_size,
            release,
            edge_snap_threshold,
        }
    }

    pub fn apply_resize_to_location(window: CosmicMapped, shell: &mut Shell) {
        let mut resize_state = window.resize_state.lock().unwrap();

        if resize_state.is_none() {
            return;
        }

        let (output, floating_layer) = if let Some((output, set)) = shell
            .workspaces
            .sets
            .iter_mut()
            .find(|(_, set)| set.sticky_layer.mapped().any(|m| m == &window))
        {
            (output, &mut set.sticky_layer)
        } else if let Some(workspace) = shell.space_for_mut(&window) {
            (&workspace.output, &mut workspace.floating_layer)
        } else {
            return;
        };

        if let Some(location) = floating_layer
            .space
            .element_location(&window)
            .map(PointExt::as_local)
            .map(|p| p.to_global(output))
        {
            let mut new_location = None;

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
                    let initial_window_location = initial_window_location.to_global(output);

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
                if !window.is_resizing(false).unwrap_or(false) {
                    *resize_state = None;
                }
            }
            std::mem::drop(resize_state);

            if let Some(new_location) = new_location {
                for (window, offset) in window.windows() {
                    match window.0.underlying_surface() {
                        WindowSurface::Wayland(toplevel) => {
                            update_reactive_popups(
                                toplevel,
                                new_location + offset.as_global(),
                                floating_layer.space.outputs(),
                            );
                        }
                        WindowSurface::X11(surface) => {
                            let mut geometry = surface.geometry();
                            geometry.loc += (location - new_location).as_logical();
                            let _ = surface.configure(geometry);
                        }
                    }
                }
                floating_layer.space.map_element(
                    window,
                    new_location.to_local(&output).as_logical(),
                    false,
                );
            }
        }
    }

    fn ungrab(&mut self) {
        // No more buttons are pressed, release the grab.
        self.seat
            .user_data()
            .get::<ResizeGrabMarker>()
            .unwrap()
            .0
            .store(false, Ordering::SeqCst);

        // If toplevel is dead, we can't resize it, so we return early.
        if !self.window.alive() {
            return;
        }

        self.window.set_resizing(false);
        self.window.configure();

        let mut resize_state = self.window.resize_state.lock().unwrap();
        if let Some(ResizeState::Resizing(resize_data)) = *resize_state {
            *resize_state = Some(ResizeState::WaitingForCommit(resize_data));
        } else {
            debug!("unexpected resize state: {:?}", resize_state);
        }
    }
}

impl Drop for ResizeSurfaceGrab {
    fn drop(&mut self) {
        let cursor_state = self.seat.user_data().get::<CursorState>().unwrap();
        cursor_state.lock().unwrap().unset_shape();
    }
}
