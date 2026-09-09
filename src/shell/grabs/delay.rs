use smithay::{
    backend::input::{InputTime, TabletToolDescriptor},
    input::{
        Seat, SeatHandler,
        pointer::{
            AxisFrame as PointerAxisFrame, ButtonEvent as PointerButtonEvent, Focus,
            GestureHoldBeginEvent, GestureHoldEndEvent, GesturePinchBeginEvent,
            GesturePinchEndEvent, GesturePinchUpdateEvent, GestureSwipeBeginEvent,
            GestureSwipeEndEvent, GestureSwipeUpdateEvent, GrabStartData as PointerGrabStartData,
            MotionEvent as PointerMotionEvent, PointerGrab, PointerInnerHandle,
            RelativeMotionEvent,
        },
        tablet::{
            TabletSeatHandler, TabletSeatTrait,
            tool::{
                AxisFrame as TabletAxisFrame, ButtonEvent as TabletButtonEvent,
                DownEvent as TabletDownEvent, GrabStartData as TabletGrabStartData,
                MotionEvent as TabletMotionEvent, ProximityInEvent, ProximityOutEvent,
                TabletToolGrab, TabletToolInnerHandle, UpEvent as TabletUpEvent,
            },
        },
        touch::{
            DownEvent as TouchDownEvent, GrabStartData as TouchGrabStartData,
            MotionEvent as TouchMotionEvent, OrientationEvent, ShapeEvent, TouchGrab,
            TouchInnerHandle, UpEvent as TouchUpEvent,
        },
    },
    utils::{Logical, Point, SERIAL_COUNTER, Serial},
};

use crate::{shell::grabs::GrabType, state::State};

use super::GrabStartData;

pub struct DelayGrab<G> {
    grab_factory: Option<Box<dyn FnOnce(&mut State) -> Option<(G, Focus)>>>,
    seat: Seat<State>,
    serial: Option<Serial>,
    start_data: GrabStartData,
}

unsafe impl<G> Send for DelayGrab<G> {}

impl<G> DelayGrab<G> {
    pub fn new(
        factory: impl FnOnce(&mut State) -> Option<(G, Focus)> + 'static,
        seat: Seat<State>,
        serial: Option<Serial>,
        start_data: GrabStartData,
    ) -> Self {
        DelayGrab {
            grab_factory: Some(Box::new(factory)),
            seat,
            serial,
            start_data,
        }
    }

    pub fn grab_type(&self) -> GrabType {
        self.start_data.type_()
    }

    pub fn tool(&self) -> Option<&TabletToolDescriptor> {
        if let GrabStartData::TabletTool { tool, .. } = &self.start_data {
            Some(tool)
        } else {
            None
        }
    }
}

impl<G: PointerGrab<State>> PointerGrab<State> for DelayGrab<G> {
    fn motion(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        focus: Option<(<State as SeatHandler>::PointerFocus, Point<f64, Logical>)>,
        event: &PointerMotionEvent,
    ) {
        handle.motion(data, focus, event);

        let distance = self.start_data.distance(event.location);
        if distance >= 1.
            && let Some(factory) = self.grab_factory.take()
        {
            let serial = self.serial.unwrap_or(event.serial);
            let seat = self.seat.clone();
            data.common.event_loop_handle.insert_idle(move |data| {
                if let Some((grab, focus)) = factory(data) {
                    seat.get_pointer()
                        .unwrap()
                        .set_grab(data, grab, serial, focus);
                }
            });
        }
    }

    fn relative_motion(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        focus: Option<(<State as SeatHandler>::PointerFocus, Point<f64, Logical>)>,
        event: &RelativeMotionEvent,
    ) {
        handle.relative_motion(data, focus, event);
    }

    fn button(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &PointerButtonEvent,
    ) {
        handle.button(data, event);
        if handle.current_pressed().is_empty() {
            handle.unset_grab(self, data, event.serial, event.time, true);
        }
    }

    fn axis(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        details: PointerAxisFrame,
    ) {
        handle.axis(data, details);
    }

    fn frame(&mut self, data: &mut State, handle: &mut PointerInnerHandle<'_, State>) {
        handle.frame(data);
    }

    fn gesture_swipe_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeBeginEvent,
    ) {
        handle.gesture_swipe_begin(data, event);
    }

    fn gesture_swipe_update(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeUpdateEvent,
    ) {
        handle.gesture_swipe_update(data, event);
    }

    fn gesture_swipe_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeEndEvent,
    ) {
        handle.gesture_swipe_end(data, event);
    }

    fn gesture_pinch_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchBeginEvent,
    ) {
        handle.gesture_pinch_begin(data, event);
    }

    fn gesture_pinch_update(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchUpdateEvent,
    ) {
        handle.gesture_pinch_update(data, event);
    }

    fn gesture_pinch_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchEndEvent,
    ) {
        handle.gesture_pinch_end(data, event);
    }

    fn gesture_hold_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureHoldBeginEvent,
    ) {
        handle.gesture_hold_begin(data, event);
    }

    fn gesture_hold_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureHoldEndEvent,
    ) {
        handle.gesture_hold_end(data, event);
    }

    fn start_data(&self) -> &PointerGrabStartData<State> {
        match &self.start_data {
            GrabStartData::Pointer(start_data) => start_data,
            _ => unreachable!(),
        }
    }

    fn unset(&mut self, _data: &mut State) {}
}

impl<G: TouchGrab<State>> TouchGrab<State> for DelayGrab<G> {
    fn down(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        focus: Option<(<State as SeatHandler>::TouchFocus, Point<f64, Logical>)>,
        event: &TouchDownEvent,
    ) {
        handle.down(data, focus, event);
    }

    fn up(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &TouchUpEvent,
    ) {
        handle.up(data, event);

        if event.slot == TouchGrab::start_data(self).slot {
            handle.unset_grab(self, data);
        }
    }

    fn motion(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        focus: Option<(<State as SeatHandler>::TouchFocus, Point<f64, Logical>)>,
        event: &TouchMotionEvent,
    ) {
        handle.motion(data, focus, event);

        let distance = self.start_data.distance(event.location);
        if distance >= 1.
            && let Some(factory) = self.grab_factory.take()
        {
            let seat = self.seat.clone();
            let serial = self.serial.unwrap_or_else(|| SERIAL_COUNTER.next_serial());
            data.common.event_loop_handle.insert_idle(move |data| {
                if let Some((grab, _)) = factory(data) {
                    seat.get_touch().unwrap().set_grab(data, grab, serial);
                }
            });
        }
    }

    fn frame(&mut self, data: &mut State, handle: &mut TouchInnerHandle<'_, State>) {
        handle.frame(data)
    }

    fn cancel(&mut self, data: &mut State, handle: &mut TouchInnerHandle<'_, State>) {
        handle.cancel(data);
        handle.unset_grab(self, data);
    }

    fn shape(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &ShapeEvent,
    ) {
        handle.shape(data, event)
    }

    fn orientation(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &OrientationEvent,
    ) {
        handle.orientation(data, event);
    }

    fn start_data(&self) -> &TouchGrabStartData<State> {
        match &self.start_data {
            GrabStartData::Touch(start_data) => start_data,
            _ => unreachable!(),
        }
    }

    fn unset(&mut self, _data: &mut State) {}
}

impl<G: TabletToolGrab<State>> TabletToolGrab<State> for DelayGrab<G> {
    fn start_data(&self) -> &TabletGrabStartData<State> {
        match &self.start_data {
            GrabStartData::TabletTool { data, .. } => data,
            _ => unreachable!(),
        }
    }

    fn proximity_out(
        &mut self,
        data: &mut State,
        handle: &mut TabletToolInnerHandle<'_, State>,
        event: &ProximityOutEvent,
    ) {
        handle.proximity_out(data, event);
    }

    fn motion(
        &mut self,
        data: &mut State,
        handle: &mut TabletToolInnerHandle<'_, State>,
        focus: Option<(<State as TabletSeatHandler>::ToolFocus, Point<f64, Logical>)>,
        event: &TabletMotionEvent,
    ) {
        handle.motion(data, focus, event);

        let distance = self.start_data.distance(event.location);
        if distance >= 1.
            && let Some(factory) = self.grab_factory.take()
        {
            let seat = self.seat.clone();
            let serial = self.serial.unwrap_or_else(|| SERIAL_COUNTER.next_serial());
            let time = InputTime::now();
            let Some(tool) = self.tool().cloned() else {
                handle.unset_grab(self, data, serial, time, true);
                return;
            };
            data.common.event_loop_handle.insert_idle(move |data| {
                if let Some((grab, focus)) = factory(data) {
                    seat.tablet_seat()
                        .get_tool(&tool)
                        .unwrap()
                        .set_grab(data, grab, time, serial, focus);
                }
            });
        }
    }

    fn down(
        &mut self,
        data: &mut State,
        handle: &mut TabletToolInnerHandle<'_, State>,
        event: &TabletDownEvent,
    ) {
        handle.down(data, event)
    }

    fn up(
        &mut self,
        data: &mut State,
        handle: &mut TabletToolInnerHandle<'_, State>,
        event: &TabletUpEvent,
    ) {
        handle.up(data, event);
        handle.unset_grab(self, data, event.serial, event.time, false);
    }

    fn button(
        &mut self,
        data: &mut State,
        handle: &mut TabletToolInnerHandle<'_, State>,
        event: &TabletButtonEvent,
    ) {
        handle.button(data, event);
    }

    fn axis(
        &mut self,
        data: &mut State,
        handle: &mut TabletToolInnerHandle<'_, State>,
        frame: TabletAxisFrame,
    ) {
        handle.axis(data, frame)
    }

    fn frame(
        &mut self,
        data: &mut State,
        handle: &mut TabletToolInnerHandle<'_, State>,
        time: InputTime,
    ) {
        handle.frame(data, time)
    }

    fn unset(&mut self, _data: &mut State) {}

    fn proximity_in(
        &mut self,
        data: &mut State,
        handle: &mut TabletToolInnerHandle<'_, State>,
        focus: Option<(<State as TabletSeatHandler>::ToolFocus, Point<f64, Logical>)>,
        event: &ProximityInEvent,
    ) {
        handle.proximity_in(data, focus, event);
    }
}
