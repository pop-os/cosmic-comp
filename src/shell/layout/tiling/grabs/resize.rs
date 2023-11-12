// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render::cursor::{CursorShape, CursorState},
    shell::{focus::target::PointerFocusTarget, layout::Orientation},
    utils::prelude::*,
};
use id_tree::{NodeId, Tree};
use smithay::{
    backend::input::ButtonState,
    input::{
        pointer::{
            AxisFrame, ButtonEvent, Focus, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent,
            GrabStartData as PointerGrabStartData, MotionEvent, PointerGrab, PointerInnerHandle,
            PointerTarget, RelativeMotionEvent,
        },
        Seat,
    },
    output::WeakOutput,
    utils::{IsAlive, Logical, Point},
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
        cursor_state.set_shape(match self.orientation {
            Orientation::Horizontal => CursorShape::RowResize,
            Orientation::Vertical => CursorShape::ColResize,
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
        cursor_state.set_shape(CursorShape::Default)
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
                    ResizeForkGrab {
                        start_data: PointerGrabStartData {
                            focus: None,
                            button,
                            location,
                        },
                        old_tree: None,
                        accumulated_delta: 0.0,
                        last_loc: location,
                        node,
                        output,
                        left_up_idx,
                        orientation,
                    },
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

pub struct ResizeForkGrab {
    start_data: PointerGrabStartData<State>,
    last_loc: Point<f64, Logical>,
    old_tree: Option<Tree<Data>>,
    accumulated_delta: f64,
    node: NodeId,
    output: WeakOutput,
    left_up_idx: usize,
    orientation: Orientation,
}

impl PointerGrab<State> for ResizeForkGrab {
    fn motion(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<i32, Logical>)>,
        event: &MotionEvent,
    ) {
        // While the grab is active, no client has pointer focus
        handle.motion(data, None, event);

        let delta = event.location - self.last_loc;

        if let Some(output) = self.output.upgrade() {
            let tiling_layer = &mut data.common.shell.active_space_mut(&output).tiling_layer;
            let gaps = tiling_layer.gaps();

            let tree = &mut tiling_layer.queue.trees.back_mut().unwrap().0;
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
                        *tree = old_tree.copy_clone();
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
                    return handle.unset_grab(data, event.serial, event.time, true);
                };

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
                            return;
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

                self.last_loc = event.location;
                let blocker = TilingLayout::update_positions(&output, tree, gaps);
                tiling_layer.pending_blockers.extend(blocker);
            } else {
                handle.unset_grab(data, event.serial, event.time, true);
            }
        }
    }

    fn relative_motion(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<i32, Logical>)>,
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
        if handle.current_pressed().is_empty() {
            // No more buttons are pressed, release the grab.
            handle.unset_grab(data, event.serial, event.time, true);
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
        &self.start_data
    }
}
