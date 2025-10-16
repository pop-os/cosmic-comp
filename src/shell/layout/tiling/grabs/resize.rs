// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render::cursor::CursorState,
    shell::{
        focus::target::PointerFocusTarget,
        grabs::{GrabStartData, ReleaseMode},
        layout::Orientation,
    },
    utils::prelude::*,
};
use id_tree::{NodeId, Tree};
use smithay::{
    backend::input::ButtonState,
    input::{
        Seat,
        pointer::{
            AxisFrame, ButtonEvent, CursorIcon, Focus, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent,
            GrabStartData as PointerGrabStartData, MotionEvent, PointerGrab, PointerInnerHandle,
            PointerTarget, RelativeMotionEvent,
        },
        touch::{
            DownEvent, GrabStartData as TouchGrabStartData, MotionEvent as TouchMotionEvent,
            OrientationEvent, ShapeEvent, TouchGrab, TouchInnerHandle, TouchTarget, UpEvent,
        },
    },
    output::WeakOutput,
    utils::{IsAlive, Logical, Point, Serial},
};

use super::super::{Data, TilingLayout};

#[derive(Debug, Clone, PartialEq)]
pub struct ResizeForkTarget {
    pub node: NodeId,
    pub output: WeakOutput,
    pub left_up_idx: usize,
    pub orientation: Orientation,
}

impl IsAlive for ResizeForkTarget {
    fn alive(&self) -> bool {
        self.output.upgrade().is_some()
    }
}

impl PointerTarget<State> for ResizeForkTarget {
    fn enter(&self, seat: &Seat<State>, _data: &mut State, _event: &MotionEvent) {
        let user_data = seat.user_data();
        let cursor_state = user_data.get::<CursorState>().unwrap();
        cursor_state
            .lock()
            .unwrap()
            .set_shape(match self.orientation {
                Orientation::Horizontal => CursorIcon::RowResize,
                Orientation::Vertical => CursorIcon::ColResize,
            });
    }

    fn leave(
        &self,
        seat: &Seat<State>,
        _data: &mut State,
        _serial: smithay::utils::Serial,
        _time: u32,
    ) {
        let user_data = seat.user_data();
        let cursor_state = user_data.get::<CursorState>().unwrap();
        cursor_state.lock().unwrap().unset_shape();
    }

    fn button(&self, seat: &Seat<State>, data: &mut State, event: &ButtonEvent) {
        if event.button == 0x110 && event.state == ButtonState::Pressed {
            let seat = seat.clone();
            let node = self.node.clone();
            let output = self.output.clone();
            let left_up_idx = self.left_up_idx;
            let orientation = self.orientation;
            let serial = event.serial;
            let button = event.button;
            data.common.event_loop_handle.insert_idle(move |state| {
                let pointer = seat.get_pointer().unwrap();
                let location = pointer.current_location();
                pointer.set_grab(
                    state,
                    ResizeForkGrab::new(
                        GrabStartData::Pointer(PointerGrabStartData {
                            focus: None,
                            button,
                            location,
                        }),
                        location.as_global(),
                        node,
                        left_up_idx,
                        orientation,
                        output,
                        ReleaseMode::NoMouseButtons,
                    ),
                    serial,
                    Focus::Clear,
                )
            });
        }
    }

    fn motion(&self, _seat: &Seat<State>, _data: &mut State, _event: &MotionEvent) {}
    fn relative_motion(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &RelativeMotionEvent,
    ) {
    }
    fn axis(&self, _seat: &Seat<State>, _data: &mut State, _frame: AxisFrame) {}
    fn frame(&self, _seat: &Seat<State>, _data: &mut State) {}
    fn gesture_swipe_begin(&self, _: &Seat<State>, _: &mut State, _: &GestureSwipeBeginEvent) {}
    fn gesture_swipe_update(&self, _: &Seat<State>, _: &mut State, _: &GestureSwipeUpdateEvent) {}
    fn gesture_swipe_end(&self, _: &Seat<State>, _: &mut State, _: &GestureSwipeEndEvent) {}
    fn gesture_pinch_begin(&self, _: &Seat<State>, _: &mut State, _: &GesturePinchBeginEvent) {}
    fn gesture_pinch_update(&self, _: &Seat<State>, _: &mut State, _: &GesturePinchUpdateEvent) {}
    fn gesture_pinch_end(&self, _: &Seat<State>, _: &mut State, _: &GesturePinchEndEvent) {}
    fn gesture_hold_begin(&self, _: &Seat<State>, _: &mut State, _: &GestureHoldBeginEvent) {}
    fn gesture_hold_end(&self, _: &Seat<State>, _: &mut State, _: &GestureHoldEndEvent) {}
}

impl TouchTarget<State> for ResizeForkTarget {
    fn down(&self, seat: &Seat<State>, data: &mut State, event: &DownEvent, _seq: Serial) {
        let seat = seat.clone();
        let node = self.node.clone();
        let output = self.output.clone();
        let left_up_idx = self.left_up_idx;
        let orientation = self.orientation;
        let serial = event.serial;
        let slot = event.slot;
        let location = event.location;
        data.common.event_loop_handle.insert_idle(move |state| {
            let touch = seat.get_touch().unwrap();
            touch.set_grab(
                state,
                ResizeForkGrab::new(
                    GrabStartData::Touch(TouchGrabStartData {
                        focus: None,
                        slot,
                        location,
                    }),
                    location.as_global(),
                    node,
                    left_up_idx,
                    orientation,
                    output,
                    ReleaseMode::NoMouseButtons,
                ),
                serial,
            )
        });
    }

    fn up(&self, _seat: &Seat<State>, _data: &mut State, _event: &UpEvent, _seq: Serial) {}
    fn motion(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &TouchMotionEvent,
        _seq: Serial,
    ) {
    }
    fn frame(&self, _seat: &Seat<State>, _data: &mut State, _seq: Serial) {}
    fn cancel(&self, _seat: &Seat<State>, _data: &mut State, _seq: Serial) {}
    fn shape(&self, _seat: &Seat<State>, _data: &mut State, _event: &ShapeEvent, _seq: Serial) {}
    fn orientation(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &OrientationEvent,
        _seq: Serial,
    ) {
    }
}

pub struct ResizeForkGrab {
    start_data: GrabStartData,
    last_loc: Point<f64, Global>,
    old_tree: Option<Tree<Data>>,
    accumulated_delta: f64,
    node: NodeId,
    output: WeakOutput,
    left_up_idx: usize,
    orientation: Orientation,
    release: ReleaseMode,
}

impl ResizeForkGrab {
    pub fn new(
        start_data: GrabStartData,
        pointer_loc: Point<f64, Global>,
        node: NodeId,
        idx: usize,
        orientation: Orientation,
        output: WeakOutput,
        release: ReleaseMode,
    ) -> ResizeForkGrab {
        ResizeForkGrab {
            start_data,
            last_loc: pointer_loc,
            old_tree: None,
            accumulated_delta: 0.0,
            node,
            output,
            left_up_idx: idx,
            orientation,
            release,
        }
    }
}

impl ResizeForkGrab {
    // Returns `true` if grab should be unset
    fn update_location(
        &mut self,
        data: &mut State,
        location: Point<f64, Logical>,
        force: bool,
    ) -> bool {
        let delta = location - self.last_loc.as_logical();
        self.last_loc = location.as_global();

        if let Some(output) = self.output.upgrade() {
            let mut shell = data.common.shell.write();
            let Some(workspace) = shell.active_space_mut(&output) else {
                return false;
            };
            let tiling_layer = &mut workspace.tiling_layer;
            let gaps = tiling_layer.gaps();

            let mut tree = tiling_layer.queue.trees.back().unwrap().0.copy_clone();
            match &mut self.old_tree {
                Some(old_tree) => {
                    // it would be so nice to just `zip` here, but `zip` just returns `None` once either returns `None`.
                    let mut iter_a = old_tree
                        .root_node_id()
                        .into_iter()
                        .flat_map(|root_id| old_tree.traverse_pre_order_ids(root_id).unwrap());
                    let mut iter_b = tree
                        .root_node_id()
                        .into_iter()
                        .flat_map(|root_id| tree.traverse_pre_order_ids(root_id).unwrap());

                    // so lets do it manually
                    let mut equal = true;
                    let mut a = iter_a.next();
                    let mut b = iter_b.next();
                    while a.is_some() || b.is_some() {
                        equal = a == b;
                        if !equal {
                            break;
                        }
                        a = iter_a.next();
                        b = iter_b.next();
                    }

                    if !equal {
                        *old_tree = tree.copy_clone();
                        self.accumulated_delta = 0.0;
                    } else {
                        tree = old_tree.copy_clone();
                    }
                }
                x @ None => {
                    *x = Some(tree.copy_clone());
                }
            };
            if tree.get(&self.node).is_ok() {
                let delta = match self.orientation {
                    Orientation::Vertical => delta.x,
                    Orientation::Horizontal => delta.y,
                }
                .round();
                self.accumulated_delta += delta;

                // check that we are still alive
                let mut iter = tree
                    .children_ids(&self.node)
                    .unwrap()
                    .skip(self.left_up_idx);
                let first_elem = iter.next();
                let second_elem = iter.next();
                if first_elem.is_none() || second_elem.is_none() {
                    return true;
                };

                if self.accumulated_delta.round() as i32 == 0 {
                    return false;
                }

                match tree.get_mut(&self.node).unwrap().data_mut() {
                    Data::Group {
                        sizes, orientation, ..
                    } => {
                        if sizes[self.left_up_idx] + sizes[self.left_up_idx + 1]
                            < match orientation {
                                Orientation::Vertical => 720,
                                Orientation::Horizontal => 480,
                            }
                        {
                            return false;
                        };

                        let old_size = sizes[self.left_up_idx];
                        sizes[self.left_up_idx] = (old_size
                            + self.accumulated_delta.round() as i32)
                            .max(if self.orientation == Orientation::Vertical {
                                360
                            } else {
                                240
                            });
                        let diff = old_size - sizes[self.left_up_idx];
                        let next_size = sizes[self.left_up_idx + 1] + diff;
                        sizes[self.left_up_idx + 1] =
                            next_size.max(if self.orientation == Orientation::Vertical {
                                360
                            } else {
                                240
                            });
                        let next_diff = next_size - sizes[self.left_up_idx + 1];
                        sizes[self.left_up_idx] += next_diff;
                    }
                    _ => unreachable!(),
                }

                let should_configure = force
                    || tree
                        .traverse_pre_order(&self.node)
                        .unwrap()
                        .all(|node| match node.data() {
                            Data::Mapped { mapped, .. } => mapped.latest_size_committed(),
                            _ => true,
                        });
                if should_configure {
                    let blocker = TilingLayout::update_positions(&output, &mut tree, gaps);
                    tiling_layer.queue.push_tree(tree, None, blocker);
                }
            } else {
                return true;
            }
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

impl PointerGrab<State> for ResizeForkGrab {
    fn motion(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &MotionEvent,
    ) {
        // While the grab is active, no client has pointer focus
        handle.motion(data, None, event);

        if self.update_location(data, event.location, false) {
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

    fn unset(&mut self, data: &mut State) {
        self.update_location(data, self.last_loc.as_logical(), true);
    }
}

impl TouchGrab<State> for ResizeForkGrab {
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
        if event.slot == <Self as TouchGrab<State>>::start_data(self).slot
            && self.update_location(data, event.location, false)
        {
            handle.unset_grab(self, data);
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

    fn start_data(&self) -> &TouchGrabStartData<State> {
        match &self.start_data {
            GrabStartData::Touch(start_data) => start_data,
            _ => unreachable!(),
        }
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

    fn unset(&mut self, data: &mut State) {
        self.update_location(data, self.last_loc.as_logical(), true);
    }
}
