// SPDX-License-Identifier: GPL-3.0-only

//! Pointer Emulating TabletTool grab.

use crate::{shell::focus::target::PointerFocusTarget, state::State};
use smithay::{
    backend::input::{ButtonState, InputTime, MouseButton},
    input::{
        Seat, pointer,
        tablet::{
            self,
            tool::{
                AxisFrame, ButtonEvent, DownEvent, GrabStartData, ProximityInEvent,
                ProximityOutEvent, TabletToolGrab, TabletToolInnerHandle, UpEvent,
            },
        },
    },
    utils::{Logical, Point, SERIAL_COUNTER},
};

// from https://gitlab.freedesktop.org/libinput/libinput/-/blob/main/include/linux/linux/input-event-codes.h
const BTN_STYLUS: u32 = 0x14b;
const BTN_STYLUS_2: u32 = 0x14c;
const BTN_LEFT: u32 = 0x110;
const BTN_RIGHT: u32 = 0x111;
const BTN_MIDDLE: u32 = 0x112;
const BTN_FORWARD: u32 = 0x115;
const BTN_BACK: u32 = 0x116;

pub struct PointerEmulationGrab {
    start_data: GrabStartData<State>,
    seat: Seat<State>,

    above_source: bool,
    tip_down: bool,
    button_down: Vec<u32>,
}

impl PointerEmulationGrab {
    pub fn new(start_data: GrabStartData<State>, seat: Seat<State>) -> Self {
        Self {
            start_data,
            seat,

            above_source: true,
            tip_down: false,
            button_down: Vec::new(),
        }
    }

    fn button_to_mouse(&self, button: u32) -> Option<MouseButton> {
        // FIXME: This should be configurable.
        if button == BTN_STYLUS {
            Some(MouseButton::Right)
        } else if button == BTN_STYLUS_2 {
            Some(MouseButton::Middle)
        } else {
            None
        }
    }

    fn pointer_button(&self, data: &mut State, button: MouseButton, state: ButtonState) {
        if let Some(pointer) = self.seat.get_pointer() {
            let button = match button {
                MouseButton::Left => BTN_LEFT,
                MouseButton::Right => BTN_RIGHT,
                MouseButton::Middle => BTN_MIDDLE,
                MouseButton::Back => BTN_BACK,
                MouseButton::Forward => BTN_FORWARD,
                _ => return,
            };

            pointer.button(
                data,
                &pointer::ButtonEvent {
                    serial: SERIAL_COUNTER.next_serial(),
                    button,
                    state,
                    time: InputTime::now(),
                },
            );

            pointer.frame(data);
        }
    }
}

type Type = tablet::tool::MotionEvent;

impl TabletToolGrab<State> for PointerEmulationGrab {
    fn proximity_in(
        &mut self,
        data: &mut State,
        handle: &mut TabletToolInnerHandle<'_, State>,
        focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &ProximityInEvent,
    ) {
        handle.proximity_in(data, focus.clone(), event);

        if !self.above_source && !self.tip_down && self.button_down.is_empty() {
            handle.unset_grab(
                self,
                data,
                SERIAL_COUNTER.next_serial(),
                InputTime::now(),
                true,
            );
        } else {
            if let Some(pointer) = self.seat.get_pointer() {
                pointer.motion(
                    data,
                    focus,
                    &pointer::MotionEvent {
                        location: event.location,
                        serial: SERIAL_COUNTER.next_serial(),
                        time: InputTime::now(),
                    },
                );
                pointer.frame(data);
            }
        }
    }

    fn proximity_out(
        &mut self,
        data: &mut State,
        handle: &mut TabletToolInnerHandle<'_, State>,
        event: &ProximityOutEvent,
    ) {
        handle.proximity_out(data, event);

        handle.unset_grab(self, data, event.serial, event.time, true);
    }

    fn down(
        &mut self,
        data: &mut State,
        handle: &mut TabletToolInnerHandle<'_, State>,
        event: &DownEvent,
    ) {
        handle.down(data, event);
        self.tip_down = true;

        self.pointer_button(data, MouseButton::Left, ButtonState::Pressed);
    }

    fn up(
        &mut self,
        data: &mut State,
        handle: &mut TabletToolInnerHandle<'_, State>,
        event: &UpEvent,
    ) {
        handle.up(data, event);

        self.pointer_button(data, MouseButton::Left, ButtonState::Released);
        self.tip_down = false;

        if !self.above_source && !self.tip_down && self.button_down.is_empty() {
            handle.unset_grab(self, data, event.serial, event.time, true);
        }
    }

    fn motion(
        &mut self,
        data: &mut State,
        handle: &mut TabletToolInnerHandle<'_, State>,
        focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &Type,
    ) {
        self.above_source = focus.as_ref().map(|(target, _)| target)
            == self.start_data.focus.as_ref().map(|(target, _)| target);
        handle.motion(data, self.start_data.focus.clone(), event);

        if !self.above_source && !self.tip_down && self.button_down.is_empty() {
            handle.unset_grab(
                self,
                data,
                SERIAL_COUNTER.next_serial(),
                InputTime::now(),
                true,
            );
        } else {
            if let Some(pointer) = self.seat.get_pointer() {
                pointer.motion(
                    data,
                    focus,
                    &pointer::MotionEvent {
                        location: event.location,
                        serial: SERIAL_COUNTER.next_serial(),
                        time: InputTime::now(),
                    },
                );
                pointer.frame(data);
            }
        }
    }

    fn button(
        &mut self,
        data: &mut State,
        handle: &mut TabletToolInnerHandle<'_, State>,
        event: &ButtonEvent,
    ) {
        handle.button(data, event);

        self.button_down.retain(|b| b != &event.button);
        if matches!(event.state, ButtonState::Pressed) {
            self.button_down.push(event.button);
        }

        if let Some(button) = self.button_to_mouse(event.button) {
            self.pointer_button(data, button, event.state);
        }

        if !self.above_source && !self.tip_down && self.button_down.is_empty() {
            handle.unset_grab(
                self,
                data,
                SERIAL_COUNTER.next_serial(),
                InputTime::now(),
                true,
            );
        }
    }

    fn axis(
        &mut self,
        data: &mut State,
        handle: &mut TabletToolInnerHandle<'_, State>,
        frame: AxisFrame,
    ) {
        handle.axis(data, frame);
    }

    fn frame(
        &mut self,
        data: &mut State,
        handle: &mut TabletToolInnerHandle<'_, State>,
        time: InputTime,
    ) {
        handle.frame(data, time);
    }

    fn unset(&mut self, data: &mut State) {
        if self.tip_down {
            self.pointer_button(data, MouseButton::Left, ButtonState::Released);
        }

        let buttons = std::mem::take(&mut self.button_down);
        for button in buttons.into_iter() {
            if let Some(button) = self.button_to_mouse(button) {
                self.pointer_button(data, button, ButtonState::Released);
            }
        }
    }

    fn start_data(&self) -> &GrabStartData<State> {
        &self.start_data
    }
}
