// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render::{ElementFilter, cursor::notify_cursor_activity},
    config::{
        Action, Config, PrivateAction,
        key_bindings::{
            cosmic_keystate_from_smithay, cosmic_modifiers_eq_smithay,
            cosmic_modifiers_from_smithay,
        },
    },
    input::gestures::{GestureState, SwipeAction},
    shell::{
        SeatExt, Trigger,
        focus::{
            Stage, render_input_order,
            target::{KeyboardFocusTarget, PointerFocusTarget},
        },
        grabs::{ReleaseMode, ResizeEdge},
        layout::{
            floating::ResizeGrabMarker,
            tiling::{NodeDesc, SwapWindowGrab, TilingLayout},
        },
        zoom::ZoomState,
    },
    utils::{prelude::*, quirks::workspace_overview_is_open},
    wayland::handlers::{
        image_copy_capture::{SessionHolder, cursor_capture_constraints},
        xwayland_keyboard_grab::XWaylandGrabSeat,
    },
};
use calloop::{
    RegistrationToken,
    timer::{TimeoutAction, Timer},
};
use cosmic_comp_config::{NumlockState, workspace::WorkspaceLayout};
use cosmic_settings_config::shortcuts;
use cosmic_settings_config::shortcuts::action::{Direction, ResizeDirection};
#[cfg(feature = "logind")]
use smithay::backend::input::{Switch, SwitchState, SwitchToggleEvent};
use smithay::{
    backend::input::{
        AbsolutePositionEvent, Axis, AxisRelativeDirection, AxisSource, Device, DeviceCapability,
        GestureBeginEvent, GestureEndEvent, GesturePinchUpdateEvent as _,
        GestureSwipeUpdateEvent as _, InputBackend, InputEvent, KeyState, PointerAxisEvent,
        ProximityState, TabletToolButtonEvent, TabletToolEvent, TabletToolProximityEvent,
        TabletToolTipEvent, TabletToolTipState, TouchEvent,
    },
    desktop::{PopupKeyboardGrab, WindowSurfaceType, utils::under_from_surface_tree},
    input::{
        Seat,
        keyboard::{FilterResult, KeysymHandle, Layout, ModifiersState, SerializedMods},
        pointer::{
            AxisFrame, ButtonEvent, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent, MotionEvent,
            PointerGrab, PointerHandle, RelativeMotionEvent,
        },
        touch::{DownEvent, MotionEvent as TouchMotionEvent, UpEvent},
    },
    output::Output,
    reexports::{
        input::Device as InputDevice,
        wayland_server::{Resource as _, protocol::wl_surface::WlSurface},
    },
    utils::{Clock, Logical, Monotonic, Point, Rectangle, SERIAL_COUNTER, Serial},
    wayland::{
        compositor::CompositorHandler,
        image_copy_capture::CursorSessionRef,
        keyboard_shortcuts_inhibit::KeyboardShortcutsInhibitorSeat,
        pointer_constraints::{PointerConstraint, with_pointer_constraint},
        seat::WaylandFocus,
        tablet_manager::{TabletDescriptor, TabletSeatTrait},
    },
};
use tracing::{error, trace};
use xkbcommon::xkb::{Keycode, Keysym};

use std::{
    any::Any,
    borrow::Cow,
    cell::RefCell,
    collections::{HashMap, HashSet},
    ops::ControlFlow,
    time::{Duration, Instant},
};

pub mod actions;
pub mod gestures;

/// Identifies the input backend instance an event came from, used to disambiguate device ids
/// (which are only unique within a single backend instance, see
/// [`smithay::backend::input::Device::id`]).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InputBackendId {
    /// The session input backend (libinput / winit / x11) these are mutually exclusive
    Normal,
    /// A specific Ei client connection
    Ei(smithay::reexports::reis::eis::Connection),
    /// The `zwp_virtual_keyboard_v1` protocol (all virtual keyboards share this source)
    VirtualKeyboard,
}

/// Used for debouncing focus updates due to pointer motion, if after the focus change is
/// triggered the event will cancel if the pointer moves to the original target
#[derive(Debug)]
pub struct PointerFocusState {
    //the window under the cursor prior to it's movement
    originally_focused_window: Option<KeyboardFocusTarget>,
    //the window under the cursor after it's movement
    scheduled_focused_window: Option<KeyboardFocusTarget>,
    token: RegistrationToken,
}

#[derive(Default)]
pub struct SupressedKeys(
    RefCell<HashMap<InputBackendId, Vec<(Keycode, Option<RegistrationToken>)>>>,
);
#[derive(Default)]
pub struct SupressedButtons(RefCell<HashMap<InputBackendId, HashSet<u32>>>);
#[derive(Default, Debug)]
pub struct ModifiersShortcutQueue(RefCell<HashMap<InputBackendId, shortcuts::Binding>>);

impl SupressedKeys {
    fn add(
        &self,
        backend_id: &InputBackendId,
        keysym: &KeysymHandle,
        token: impl Into<Option<RegistrationToken>>,
    ) {
        self.0
            .borrow_mut()
            .entry(backend_id.clone())
            .or_default()
            .push((keysym.raw_code(), token.into()));
    }

    fn filter(
        &self,
        backend_id: &InputBackendId,
        keysym: &KeysymHandle,
    ) -> Option<Vec<RegistrationToken>> {
        let mut by_source = self.0.borrow_mut();
        let keys = by_source.get_mut(backend_id)?;
        let (removed, remaining) = keys
            .drain(..)
            .partition(|(key, _)| *key == keysym.raw_code());
        *keys = remaining;

        if removed.is_empty() {
            return None;
        }

        Some(
            removed
                .into_iter()
                .filter_map(|(_, token)| token)
                .collect::<Vec<_>>(),
        )
    }

    fn clear_source(&self, backend_id: &InputBackendId) {
        self.0.borrow_mut().remove(backend_id);
    }
}

impl SupressedButtons {
    fn add(&self, backend_id: &InputBackendId, button: u32) {
        self.0
            .borrow_mut()
            .entry(backend_id.clone())
            .or_default()
            .insert(button);
    }

    fn remove(&self, backend_id: &InputBackendId, button: u32) -> bool {
        self.0
            .borrow_mut()
            .get_mut(backend_id)
            .is_some_and(|buttons| buttons.remove(&button))
    }

    fn clear_source(&self, backend_id: &InputBackendId) {
        self.0.borrow_mut().remove(backend_id);
    }
}

impl ModifiersShortcutQueue {
    pub fn set(&self, backend_id: &InputBackendId, binding: shortcuts::Binding) {
        self.0.borrow_mut().insert(backend_id.clone(), binding);
    }

    pub fn take(&self, backend_id: &InputBackendId, binding: &shortcuts::Binding) -> bool {
        let mut set = self.0.borrow_mut();
        if set.get(backend_id).is_some_and(|queued| queued == binding) {
            set.remove(backend_id);
            true
        } else {
            false
        }
    }

    pub fn clear(&self, backend_id: &InputBackendId) {
        self.0.borrow_mut().remove(backend_id);
    }
}

impl State {
    #[profiling::function]
    pub fn process_input_event<B: InputBackend>(
        &mut self,
        event: InputEvent<B>,
        backend_id: InputBackendId,
    ) where
        <B as InputBackend>::Device: 'static,
    {
        crate::wayland::handlers::output_power::set_all_surfaces_dpms_on(self);

        use smithay::backend::input::Event;
        match event {
            InputEvent::DeviceAdded { device } => {
                let shell = self.common.shell.read();
                let seat = shell.seats.last_active();
                let led_state = seat.get_keyboard().unwrap().led_state();
                seat.devices().add_device(&device, led_state, &backend_id);
                if device.has_capability(DeviceCapability::TabletTool) {
                    seat.tablet_seat().add_tablet::<Self>(
                        &self.common.display_handle,
                        &TabletDescriptor::from(&device),
                    );
                }
            }
            InputEvent::DeviceRemoved { device } => {
                for seat in &mut self.common.shell.read().seats.iter() {
                    let devices = seat.devices();
                    if devices.has_device(&device, &backend_id) {
                        devices.remove_device(&device, &backend_id);
                        if device.has_capability(DeviceCapability::TabletTool) {
                            seat.tablet_seat()
                                .remove_tablet(&TabletDescriptor::from(&device));
                            if seat.tablet_seat().count_tablets() == 0 {
                                seat.tablet_seat().clear_tools();
                            }
                        }
                        break;
                    }
                }
            }

            InputEvent::Keyboard { event, .. } => {
                use smithay::backend::input::KeyboardKeyEvent;

                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);

                    let keycode = event.key_code();
                    let state = event.state();
                    trace!(?keycode, ?state, "key");

                    let serial = SERIAL_COUNTER.next_serial();
                    let time = Event::time_msec(&event);
                    let keyboard = seat.get_keyboard().unwrap();
                    let previous_modifiers = keyboard.modifier_state();
                    if let Some((action, pattern)) = keyboard
                        .input(
                            self,
                            keycode,
                            state,
                            serial,
                            time,
                            |data, modifiers, handle| {
                                data.process_keyboard_filter(
                                    &backend_id,
                                    &seat,
                                    modifiers,
                                    handle,
                                    serial,
                                    time,
                                    keycode,
                                    state,
                                    previous_modifiers,
                                )
                            },
                        )
                        .flatten()
                    {
                        if pattern.key.is_none() && state == KeyState::Released {
                            // we still want to send release-events and not have apps stuck on some modifiers.
                            keyboard.input(self, keycode, state, serial, time, |_, _, _| {
                                FilterResult::<()>::Forward
                            });
                        }
                        self.handle_action(action, &backend_id, &seat, serial, time, pattern, None)
                    }

                    // If we want to track numlock state so it can be reused on the next boot...
                    if let NumlockState::LastBoot =
                        self.common.config.cosmic_conf.keyboard_config.numlock_state
                    {
                        // .. and the state has been updated ...
                        if self.common.config.dynamic_conf.numlock().last_state
                            != keyboard.modifier_state().num_lock
                        {
                            // ... then record the updated state.
                            // The call to `numlock_mut` will generate a `PersistenceGuard`. The
                            // `PersistenceGuard` will write to a file when it's dropped here.
                            self.common.config.dynamic_conf.numlock_mut().last_state =
                                keyboard.modifier_state().num_lock;
                        }
                    }
                }
            }

            InputEvent::PointerMotion { event, .. } => {
                use smithay::backend::input::PointerMotionEvent;

                let shell = self.common.shell.write();
                if let Some(seat) = shell
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned()
                {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    notify_cursor_activity(self, &seat);
                    let current_output = seat.active_output();

                    let mut position = seat.get_pointer().unwrap().current_location().as_global();

                    let under = State::surface_under(position, &current_output, &shell)
                        .map(|(target, pos)| (target, pos.as_logical()));

                    let ptr = seat.get_pointer().unwrap();

                    let mut pointer_locked = false;
                    let mut pointer_confined = false;
                    let mut confine_region = None;
                    if let Some((surface, surface_loc)) = under
                        .as_ref()
                        .and_then(|(target, l)| Some((target.wl_surface()?, l)))
                    {
                        with_pointer_constraint(&surface, &ptr, |constraint| match constraint {
                            Some(constraint) if constraint.is_active() => {
                                // Constraint does not apply if not within region
                                if !constraint.region().is_none_or(|x| {
                                    x.contains(
                                        (ptr.current_location() - *surface_loc).to_i32_floor(),
                                    )
                                }) {
                                    return;
                                }
                                match &*constraint {
                                    PointerConstraint::Locked(_locked) => {
                                        pointer_locked = true;
                                    }
                                    PointerConstraint::Confined(confine) => {
                                        pointer_confined = true;
                                        confine_region = confine.region().cloned();
                                    }
                                }
                            }
                            _ => {}
                        });
                    }

                    std::mem::drop(shell);
                    ptr.relative_motion(
                        self,
                        under.clone(),
                        &RelativeMotionEvent {
                            delta: event.delta(),
                            delta_unaccel: event.delta_unaccel(),
                            utime: event.time(),
                        },
                    );

                    if pointer_locked {
                        ptr.frame(self);
                        return;
                    }

                    let original_position = position;
                    position += event.delta().as_global();
                    let shell = self.common.shell.read();
                    let output = shell
                        .outputs()
                        .find(|output| output.geometry().to_f64().contains(position))
                        .cloned()
                        .unwrap_or(current_output.clone());
                    drop(shell);
                    let output_geometry = output.geometry();

                    let scale = output.current_scale().fractional_scale();
                    let physical = output
                        .current_mode()
                        .map(|mode| output.current_transform().transform_size(mode.size))
                        .unwrap_or_default();
                    let logical = physical.to_f64().to_logical(scale);
                    let output_geometry_loc = output_geometry.loc.to_f64();
                    // output_geometry.size is a rounded value and may undershoot/overshoot the accurate logical size
                    // We constrain the position with:
                    // - output_geometry.size so that we don't send leave events to a fullscreen app
                    // - logical size so that the position doesn't end up outside the actual size of the output
                    // See https://github.com/pop-os/cosmic-comp/pull/2568
                    let max_x = output_geometry_loc.x
                        + logical.w.min(output_geometry.size.w as f64).next_down();
                    let max_y = output_geometry_loc.y
                        + logical.h.min(output_geometry.size.h as f64).next_down();
                    position.x = position.x.clamp(output_geometry_loc.x, max_x);
                    position.y = position.y.clamp(output_geometry_loc.y, max_y);

                    if ptr.is_grabbed() {
                        if seat
                            .user_data()
                            .get::<ResizeGrabMarker>()
                            .map(|marker| marker.get())
                            .unwrap_or(false)
                            && output != current_output
                        {
                            ptr.frame(self);
                            return;
                        }
                        //If the pointer isn't grabbed, we should check if the focused element should be updated
                    } else if self.common.config.cosmic_conf.focus_follows_cursor {
                        let shell = self.common.shell.read();
                        let old_keyboard_target =
                            State::element_under(original_position, &current_output, &shell, &seat);
                        let new_keyboard_target =
                            State::element_under(position, &output, &shell, &seat);

                        if old_keyboard_target != new_keyboard_target
                            && new_keyboard_target.is_some()
                        {
                            let create_source = if self.common.pointer_focus_state.is_none() {
                                true
                            } else {
                                let PointerFocusState {
                                    originally_focused_window,
                                    scheduled_focused_window,
                                    token,
                                } = self.common.pointer_focus_state.as_ref().unwrap();

                                if &new_keyboard_target == originally_focused_window {
                                    //if we moved to the original window, just cancel the event
                                    self.common.event_loop_handle.remove(*token);
                                    //clear the state
                                    self.common.pointer_focus_state = None;
                                    false
                                } else if &new_keyboard_target != scheduled_focused_window {
                                    //if we moved to a new window, update the scheduled focus
                                    self.common.event_loop_handle.remove(*token);
                                    true
                                } else {
                                    //the state doesn't need to be updated or cleared
                                    false
                                }
                            };

                            if create_source {
                                // prevent popups from being unfocusable if there is a gap between them and their parent
                                let delay = calloop::timer::Timer::from_duration(
                                    //default to 250ms
                                    std::time::Duration::from_millis(
                                        self.common.config.cosmic_conf.focus_follows_cursor_delay,
                                    ),
                                );
                                let seat = seat.clone();
                                let token = self
                                    .common
                                    .event_loop_handle
                                    .insert_source(delay, move |_, _, state| {
                                        let target = state
                                            .common
                                            .pointer_focus_state
                                            .as_ref()
                                            .unwrap()
                                            .scheduled_focused_window
                                            .clone();
                                        //clear it prior in case the user twitches in the microsecond it
                                        //takes this function to run
                                        state.common.pointer_focus_state = None;

                                        Shell::set_focus(
                                            state,
                                            target.as_ref(),
                                            &seat,
                                            Some(SERIAL_COUNTER.next_serial()),
                                            false,
                                        );

                                        TimeoutAction::Drop
                                    })
                                    .ok();
                                if token.is_some() {
                                    let originally_focused_window =
                                        if self.common.pointer_focus_state.is_none() {
                                            old_keyboard_target
                                        } else {
                                            // In this case, the pointer has moved to a new window (neither original, nor scheduled)
                                            // so we should preserve the original window for the focus state
                                            self.common
                                                .pointer_focus_state
                                                .as_ref()
                                                .unwrap()
                                                .originally_focused_window
                                                .clone()
                                        };

                                    self.common.pointer_focus_state = Some(PointerFocusState {
                                        originally_focused_window,
                                        scheduled_focused_window: new_keyboard_target,
                                        token: token.unwrap(),
                                    });
                                }
                            }
                        }
                    }

                    // If confined, help user to update valid coordinates can improve user experience
                    let shell = self.common.shell.read();
                    let new_under = if pointer_confined && let Some((surface, surface_loc)) = &under
                    {
                        let is_legal = |pos: Point<f64, Global>, shell: &Shell| {
                            let new_under = State::surface_under(pos, &output, shell)
                                .map(|(target, pos)| (target, pos.as_logical()));

                            // TODO: We might need a solution that allows constraints to bypass the surface without affecting the constraints themselves
                            if new_under.as_ref().and_then(|(under, _)| under.wl_surface())
                                != surface.wl_surface()
                            {
                                return (false, None);
                            }

                            match surface {
                                PointerFocusTarget::WlSurface { surface, .. } => {
                                    if under_from_surface_tree(
                                        surface,
                                        position.as_logical() - surface_loc.to_f64(),
                                        (0, 0),
                                        WindowSurfaceType::ALL,
                                    )
                                    .is_none()
                                    {
                                        return (false, None);
                                    }
                                }
                                PointerFocusTarget::X11Surface { surface, .. } => {
                                    if surface
                                        .surface_under(
                                            position.as_logical() - surface_loc.to_f64(),
                                            (0, 0),
                                            WindowSurfaceType::ALL,
                                        )
                                        .is_none()
                                    {
                                        return (false, None);
                                    }
                                }
                                _ => {}
                            }

                            if let Some(region) = &confine_region
                                && !region
                                    .contains((pos.as_logical() - *surface_loc).to_i32_floor())
                            {
                                return (false, None);
                            }
                            (true, new_under)
                        };

                        match is_legal(position, &shell) {
                            (true, under) => under,
                            _ => {
                                let y_only_pos = Point::new(original_position.x, position.y);
                                let x_only_pos = Point::new(position.x, original_position.y);
                                match is_legal(y_only_pos, &shell) {
                                    (true, under) => {
                                        position = y_only_pos;
                                        under
                                    }
                                    _ => match is_legal(x_only_pos, &shell) {
                                        (true, under) => {
                                            position = x_only_pos;
                                            under
                                        }
                                        _ => {
                                            position = original_position;
                                            None
                                        }
                                    },
                                }
                            }
                        }
                    } else {
                        State::surface_under(position, &output, &shell)
                            .map(|(target, pos)| (target, pos.as_logical()))
                    };

                    drop(shell);
                    if pointer_confined && new_under.is_none() {
                        ptr.frame(self);
                        return;
                    }

                    let serial = SERIAL_COUNTER.next_serial();
                    ptr.motion(
                        self,
                        under,
                        &MotionEvent {
                            location: position.as_logical(),
                            serial,
                            time: event.time_msec(),
                        },
                    );
                    ptr.frame(self);

                    // If pointer is now in a constraint region and window is in focused, activate it
                    if let Some((under, surface_location)) = new_under
                        .and_then(|(target, loc)| Some((target.wl_surface()?.into_owned(), loc)))
                    {
                        let shell = self.common.shell.read();
                        let is_focused = seat
                            .get_keyboard()
                            .and_then(|k| k.current_focus())
                            .is_some_and(|f| f.has_surface(&shell, &under));

                        if is_focused {
                            with_pointer_constraint(&under, &ptr, |constraint| match constraint {
                                Some(constraint) if !constraint.is_active() => {
                                    let region = match &*constraint {
                                        PointerConstraint::Locked(locked) => locked.region(),
                                        PointerConstraint::Confined(confined) => confined.region(),
                                    };
                                    let point =
                                        (ptr.current_location() - surface_location).to_i32_floor();
                                    if region.is_none_or(|region| region.contains(point)) {
                                        constraint.activate();
                                    }
                                }
                                _ => {}
                            });
                        }
                    }

                    let mut shell = self.common.shell.write();
                    shell.update_pointer_position(position.to_local(&output), &output);
                    shell.update_focal_point(
                        &seat,
                        original_position,
                        self.common.config.cosmic_conf.accessibility_zoom.view_moves,
                    );

                    if output != current_output {
                        for session in cursor_sessions_for_output(&shell, &current_output) {
                            session.set_cursor_pos(None);
                        }
                        seat.set_active_output(&output);
                    }

                    update_output_image_copy_cursor_position(
                        &shell,
                        &self.common.clock,
                        &output,
                        &seat,
                        position,
                    );
                }
            }
            InputEvent::PointerMotionAbsolute { event, .. } => {
                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    notify_cursor_activity(self, &seat);
                    let (output, output_geometry, position) =
                        if matches!(&backend_id, InputBackendId::Ei(_)) {
                            // EI absolute coordinates are in the compositor's *global*
                            // logical space: each advertised region carries its output's
                            // global offset, so the client sends a global position. Use
                            // the coordinate directly and find the output it lands in,
                            // rather than mapping relative to the focused output (which
                            // cannot address other monitors). This is the KWin/mutter
                            // model.
                            let position =
                            smithay::backend::input::AbsolutePositionEvent::position_transformed(
                                &event,
                                // smithay's EI impl ignores the size and returns the
                                // raw coordinate.
                                Size::from((0, 0)),
                            )
                            .as_global();
                            let output = self
                                .common
                                .shell
                                .read()
                                .outputs()
                                .find(|o| o.geometry().to_f64().contains(position))
                                .cloned()
                                .unwrap_or_else(|| seat.active_output());
                            let output_geometry = output.geometry();
                            (output, output_geometry, position)
                        } else {
                            let output = seat.active_output();
                            let output_geometry = output.geometry();
                            let position = output_geometry.loc.to_f64()
                            + smithay::backend::input::AbsolutePositionEvent::position_transformed(
                                &event,
                                output_geometry.size.as_logical(),
                            )
                            .as_global();
                            (output, output_geometry, position)
                        };
                    let serial = SERIAL_COUNTER.next_serial();
                    let under = State::surface_under(position, &output, &self.common.shell.write())
                        .map(|(target, pos)| (target, pos.as_logical()));

                    let ptr = seat.get_pointer().unwrap();
                    ptr.motion(
                        self,
                        under,
                        &MotionEvent {
                            location: position.as_logical(),
                            serial,
                            time: event.time_msec(),
                        },
                    );
                    ptr.frame(self);

                    // Keep the seat's active output following the pointer. Click-to-
                    // focus (PointerButton) resolves its target via
                    // `seat.active_output()`
                    let previous_output = seat.active_output();
                    if previous_output != output {
                        seat.set_active_output(&output);
                    }

                    let shell = self.common.shell.read();
                    update_output_image_copy_cursor_position(
                        &shell,
                        &self.common.clock,
                        &output,
                        &seat,
                        position,
                    );
                }
            }
            InputEvent::PointerButton { event, .. } => {
                use smithay::backend::input::{ButtonState, PointerButtonEvent};

                //
                let Some(seat) = self
                    .common
                    .shell
                    .read()
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned()
                else {
                    return;
                };
                self.common.idle_notifier_state.notify_activity(&seat);
                notify_cursor_activity(self, &seat);

                let current_focus = seat.get_keyboard().unwrap().current_focus();
                let shortcuts_inhibited = current_focus.as_ref().is_some_and(|f| {
                    f.wl_surface()
                        .map(|surface| {
                            seat.keyboard_shortcuts_inhibitor_for_surface(&surface)
                                .map(|inhibitor| inhibitor.is_active())
                                .unwrap_or(false)
                                || seat.has_active_xwayland_grab(&surface)
                        })
                        .unwrap_or(false)
                });

                let serial = SERIAL_COUNTER.next_serial();
                let button = event.button_code();

                let mut pass_event = !seat.supressed_buttons().remove(&backend_id, button);
                if event.state() == ButtonState::Pressed {
                    // change the keyboard focus unless the pointer is grabbed
                    // We test for any matching surface type here but always use the root
                    // (in case of a window the toplevel) surface for the focus.
                    // see: https://gitlab.freedesktop.org/wayland/wayland/-/issues/294
                    if !seat.get_pointer().unwrap().is_grabbed() {
                        let output = seat.active_output();

                        let global_position =
                            seat.get_pointer().unwrap().current_location().as_global();
                        let under = {
                            let shell = self.common.shell.read();
                            State::element_under(global_position, &output, &shell, &seat)
                        };
                        // Grabbing a tiling resize handle (the gap between tiles) must not change keyboard focus
                        let on_resize_fork = matches!(
                            seat.get_pointer().unwrap().current_focus(),
                            Some(PointerFocusTarget::ResizeFork(_))
                        );
                        if let Some(target) = under.filter(|_| !on_resize_fork) {
                            if let Some(surface) = target.toplevel().map(Cow::into_owned)
                                && self.source_modifiers(&backend_id, &seat).logo
                                && !shortcuts_inhibited
                            {
                                let seat_clone = seat.clone();
                                let mouse_button = PointerButtonEvent::button(&event);

                                let mut supress_button = || {
                                    // If the logo is held then the pointer event is
                                    // aimed at the compositor and shouldn't be passed
                                    // to the application.
                                    pass_event = false;
                                    seat.supressed_buttons().add(&backend_id, button);
                                };

                                fn dispatch_grab<G: PointerGrab<State> + 'static>(
                                    grab: Option<(G, smithay::input::pointer::Focus)>,
                                    seat: Seat<State>,
                                    backend_id: &InputBackendId,
                                    serial: Serial,
                                    state: &mut State,
                                ) {
                                    if let Some((target, focus)) = grab {
                                        seat.modifiers_shortcut_queue().clear(backend_id);

                                        seat.get_pointer()
                                            .unwrap()
                                            .set_grab(state, target, serial, focus);
                                    }
                                }

                                if let Some(mouse_button) = mouse_button {
                                    match mouse_button {
                                        smithay::backend::input::MouseButton::Left => {
                                            supress_button();
                                            let backend_id = backend_id.clone();
                                            self.common.event_loop_handle.insert_idle(
                                                move |state| {
                                                    let mut shell = state.common.shell.write();
                                                    let res = shell.move_request(
                                                        &surface,
                                                        &seat_clone,
                                                        serial,
                                                        ReleaseMode::NoMouseButtons,
                                                        false,
                                                        &state.common.config,
                                                        &state.common.event_loop_handle,
                                                        false,
                                                    );
                                                    drop(shell);
                                                    dispatch_grab(
                                                        res,
                                                        seat_clone,
                                                        &backend_id,
                                                        serial,
                                                        state,
                                                    );
                                                },
                                            );
                                        }
                                        smithay::backend::input::MouseButton::Right => {
                                            supress_button();
                                            let backend_id = backend_id.clone();
                                            self.common.event_loop_handle.insert_idle(
                                                move |state| {
                                                    let mut shell = state.common.shell.write();
                                                    let Some(target_elem) =
                                                        shell.element_for_surface(&surface)
                                                    else {
                                                        return;
                                                    };
                                                    let Some(geom) = shell
                                                        .space_for(target_elem)
                                                        .and_then(|f| {
                                                            f.element_geometry(target_elem)
                                                        })
                                                        .or_else(|| {
                                                            shell
                                                                .workspaces
                                                                .sets
                                                                .get(&output)
                                                                .and_then(|set| {
                                                                    set.sticky_layer
                                                                        .element_geometry(
                                                                            target_elem,
                                                                        )
                                                                })
                                                        })
                                                    else {
                                                        return;
                                                    };
                                                    let geom = geom.to_f64();
                                                    let center =
                                                        geom.loc + geom.size.downscale(2.0);
                                                    let offset =
                                                        center.to_global(&output) - global_position;
                                                    let edge =
                                                        match (offset.x > 0.0, offset.y > 0.0) {
                                                            (true, true) => ResizeEdge::TOP_LEFT,
                                                            (false, true) => ResizeEdge::TOP_RIGHT,
                                                            (true, false) => {
                                                                ResizeEdge::BOTTOM_LEFT
                                                            }
                                                            (false, false) => {
                                                                ResizeEdge::BOTTOM_RIGHT
                                                            }
                                                        };
                                                    let res = shell.resize_request(
                                                        &surface,
                                                        &seat_clone,
                                                        serial,
                                                        edge,
                                                        state
                                                            .common
                                                            .config
                                                            .cosmic_conf
                                                            .edge_snap_threshold,
                                                        false,
                                                    );
                                                    drop(shell);
                                                    dispatch_grab(
                                                        res,
                                                        seat_clone,
                                                        &backend_id,
                                                        serial,
                                                        state,
                                                    );
                                                },
                                            );
                                        }
                                        _ => {}
                                    }
                                }
                            }

                            Shell::set_focus(self, Some(&target), &seat, Some(serial), false);
                        }
                    }
                } else {
                    let mut shell = self.common.shell.write();
                    if let Some(Trigger::Pointer(action_button)) =
                        shell.overview_mode().0.active_trigger()
                        && *action_button == button
                    {
                        shell.set_overview_mode(None, self.common.event_loop_handle.clone());
                    }
                    std::mem::drop(shell);
                };

                if pass_event
                    && !matches!(current_focus, Some(KeyboardFocusTarget::LockSurface(_)))
                    && !shortcuts_inhibited
                {
                    self.common.xwayland_notify_pointer_button_event(
                        button,
                        event.state(),
                        serial,
                        event.time_msec(),
                    );
                }

                let ptr = seat.get_pointer().unwrap();
                if pass_event {
                    ptr.button(
                        self,
                        &ButtonEvent {
                            button,
                            state: event.state(),
                            serial,
                            time: event.time_msec(),
                        },
                    );
                    ptr.frame(self);
                } else if event.state() == ButtonState::Released {
                    ptr.unset_grab(self, serial, event.time_msec())
                }
            }
            InputEvent::PointerAxis { event, .. } => {
                let scroll_factor =
                    if let Some(device) = <dyn Any>::downcast_ref::<InputDevice>(&event.device()) {
                        self.common.config.scroll_factor(device)
                    } else {
                        1.0
                    };

                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    notify_cursor_activity(self, &seat);

                    if self.source_modifiers(&backend_id, &seat).logo
                        && self
                            .common
                            .config
                            .cosmic_conf
                            .accessibility_zoom
                            .enable_mouse_zoom_shortcuts
                    {
                        seat.modifiers_shortcut_queue().clear(&backend_id);
                        if let Some(mut percentage) = event
                            .amount_v120(Axis::Vertical)
                            .map(|val| val / 120.)
                            .or_else(|| event.amount(Axis::Vertical))
                            .map(|val| val * scroll_factor)
                        {
                            if event.relative_direction(Axis::Vertical)
                                == AxisRelativeDirection::Inverted
                            {
                                percentage *= -1.;
                            }

                            if event.source() == AxisSource::Wheel {
                                percentage *= 5.;
                            }

                            let change = -(percentage / 100.);
                            self.update_zoom(&seat, change, event.source() == AxisSource::Wheel);
                        }
                    } else {
                        let mut frame = AxisFrame::new(event.time_msec()).source(event.source());
                        let horizontal_amount = event
                            .amount(Axis::Horizontal)
                            .or_else(|| Some(event.amount_v120(Axis::Horizontal)? * 15.0 / 120.));
                        if let Some(horizontal_amount) = horizontal_amount {
                            if horizontal_amount != 0.0 {
                                frame = frame
                                    .relative_direction(
                                        Axis::Horizontal,
                                        event.relative_direction(Axis::Horizontal),
                                    )
                                    .value(Axis::Horizontal, scroll_factor * horizontal_amount);
                                if let Some(discrete) = event.amount_v120(Axis::Horizontal) {
                                    frame = frame.v120(
                                        Axis::Horizontal,
                                        (discrete * scroll_factor).round() as i32,
                                    );
                                }
                            } else if event.source() == AxisSource::Finger {
                                frame = frame.stop(Axis::Horizontal);
                            }
                        }
                        let vertical_amount = event
                            .amount(Axis::Vertical)
                            .or_else(|| Some(event.amount_v120(Axis::Vertical)? * 15.0 / 120.));
                        if let Some(vertical_amount) = vertical_amount {
                            if vertical_amount != 0.0 {
                                frame = frame
                                    .relative_direction(
                                        Axis::Vertical,
                                        event.relative_direction(Axis::Vertical),
                                    )
                                    .value(Axis::Vertical, scroll_factor * vertical_amount);
                                if let Some(discrete) = event.amount_v120(Axis::Vertical) {
                                    frame = frame.v120(
                                        Axis::Vertical,
                                        (discrete * scroll_factor).round() as i32,
                                    );
                                }
                            } else if event.source() == AxisSource::Finger {
                                frame = frame.stop(Axis::Vertical);
                            }
                        }
                        let ptr = seat.get_pointer().unwrap();
                        ptr.axis(self, frame);
                        ptr.frame(self);
                    }
                }
            }

            InputEvent::GestureSwipeBegin { event, .. } => {
                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    if event.fingers() >= 3 && !workspace_overview_is_open(&seat.active_output()) {
                        self.common.gesture_state = Some(GestureState::new(event.fingers()));
                    } else {
                        let serial = SERIAL_COUNTER.next_serial();
                        let pointer = seat.get_pointer().unwrap();
                        pointer.gesture_swipe_begin(
                            self,
                            &GestureSwipeBeginEvent {
                                serial,
                                time: event.time_msec(),
                                fingers: event.fingers(),
                            },
                        );
                    }
                }
            }
            InputEvent::GestureSwipeUpdate { event, .. } => {
                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    let mut activate_action: Option<SwipeAction> = None;
                    if let Some(ref mut gesture_state) = self.common.gesture_state {
                        let first_update = gesture_state.update(
                            event.delta(),
                            Duration::from_millis(event.time_msec() as u64),
                        );
                        // Decide on action if first update
                        if first_update {
                            let mut natural_scroll = false;
                            if let Some(scroll_config) =
                                &self.common.config.cosmic_conf.input_touchpad.scroll_config
                                && let Some(natural) = scroll_config.natural_scroll
                            {
                                natural_scroll = natural;
                            }
                            activate_action = match gesture_state.fingers {
                                3 => None, // TODO: 3 finger gestures
                                4 => {
                                    if self.common.config.cosmic_conf.workspaces.workspace_layout
                                        == WorkspaceLayout::Horizontal
                                    {
                                        match gesture_state.direction {
                                            Some(Direction::Left) => {
                                                if natural_scroll {
                                                    Some(SwipeAction::NextWorkspace)
                                                } else {
                                                    Some(SwipeAction::PrevWorkspace)
                                                }
                                            }
                                            Some(Direction::Right) => {
                                                if natural_scroll {
                                                    Some(SwipeAction::PrevWorkspace)
                                                } else {
                                                    Some(SwipeAction::NextWorkspace)
                                                }
                                            }
                                            _ => None, // TODO: Other actions
                                        }
                                    } else {
                                        match gesture_state.direction {
                                            Some(Direction::Up) => {
                                                if natural_scroll {
                                                    Some(SwipeAction::NextWorkspace)
                                                } else {
                                                    Some(SwipeAction::PrevWorkspace)
                                                }
                                            }
                                            Some(Direction::Down) => {
                                                if natural_scroll {
                                                    Some(SwipeAction::PrevWorkspace)
                                                } else {
                                                    Some(SwipeAction::NextWorkspace)
                                                }
                                            }
                                            _ => None, // TODO: Other actions
                                        }
                                    }
                                }
                                _ => None,
                            };

                            gesture_state.action = activate_action;
                        }

                        match gesture_state.action {
                            Some(x @ SwipeAction::NextWorkspace)
                            | Some(x @ SwipeAction::PrevWorkspace) => {
                                self.common.shell.write().update_workspace_delta(
                                    &seat.active_output(),
                                    gesture_state.delta,
                                    matches!(x, SwipeAction::NextWorkspace),
                                )
                            }
                            _ => {}
                        }
                    } else {
                        let pointer = seat.get_pointer().unwrap();
                        pointer.gesture_swipe_update(
                            self,
                            &GestureSwipeUpdateEvent {
                                time: event.time_msec(),
                                delta: event.delta(),
                            },
                        );
                    }

                    if let Some(action) = activate_action {
                        self.handle_swipe_action(action, &seat);
                    }
                }
            }
            InputEvent::GestureSwipeEnd { event, .. } => {
                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    if let Some(ref gesture_state) = self.common.gesture_state {
                        match gesture_state.action {
                            Some(SwipeAction::NextWorkspace) | Some(SwipeAction::PrevWorkspace) => {
                                let velocity = gesture_state.velocity();
                                let norm_velocity =
                                    if self.common.config.cosmic_conf.workspaces.workspace_layout
                                        == WorkspaceLayout::Horizontal
                                    {
                                        velocity / seat.active_output().geometry().size.w as f64
                                    } else {
                                        velocity / seat.active_output().geometry().size.h as f64
                                    };
                                let _ = self.common.shell.write().end_workspace_swipe(
                                    &seat.active_output(),
                                    norm_velocity,
                                    &mut self.common.workspace_state.update(),
                                );
                            }
                            _ => {}
                        }
                        self.common.gesture_state = None;
                    } else {
                        let serial = SERIAL_COUNTER.next_serial();
                        let pointer = seat.get_pointer().unwrap();
                        pointer.gesture_swipe_end(
                            self,
                            &GestureSwipeEndEvent {
                                serial,
                                time: event.time_msec(),
                                cancelled: event.cancelled(),
                            },
                        );
                    }
                }
            }
            InputEvent::GesturePinchBegin { event, .. } => {
                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    let serial = SERIAL_COUNTER.next_serial();
                    let pointer = seat.get_pointer().unwrap();
                    pointer.gesture_pinch_begin(
                        self,
                        &GesturePinchBeginEvent {
                            serial,
                            time: event.time_msec(),
                            fingers: event.fingers(),
                        },
                    );
                }
            }
            InputEvent::GesturePinchUpdate { event, .. } => {
                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    let pointer = seat.get_pointer().unwrap();
                    pointer.gesture_pinch_update(
                        self,
                        &GesturePinchUpdateEvent {
                            time: event.time_msec(),
                            delta: event.delta(),
                            scale: event.scale(),
                            rotation: event.rotation(),
                        },
                    );
                }
            }
            InputEvent::GesturePinchEnd { event, .. } => {
                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    let serial = SERIAL_COUNTER.next_serial();
                    let pointer = seat.get_pointer().unwrap();
                    pointer.gesture_pinch_end(
                        self,
                        &GesturePinchEndEvent {
                            serial,
                            time: event.time_msec(),
                            cancelled: event.cancelled(),
                        },
                    );
                }
            }
            InputEvent::GestureHoldBegin { event, .. } => {
                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    let serial = SERIAL_COUNTER.next_serial();
                    let pointer = seat.get_pointer().unwrap();
                    pointer.gesture_hold_begin(
                        self,
                        &GestureHoldBeginEvent {
                            serial,
                            time: event.time_msec(),
                            fingers: event.fingers(),
                        },
                    );
                }
            }
            InputEvent::GestureHoldEnd { event, .. } => {
                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    let serial = SERIAL_COUNTER.next_serial();
                    let pointer = seat.get_pointer().unwrap();
                    pointer.gesture_hold_end(
                        self,
                        &GestureHoldEndEvent {
                            serial,
                            time: event.time_msec(),
                            cancelled: event.cancelled(),
                        },
                    );
                }
            }

            InputEvent::TouchDown { event, .. } => {
                let shell = self.common.shell.write();
                if let Some(seat) = shell
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned()
                {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    let Some(output) =
                        mapped_output_for_device(&self.common.config, &shell, &event.device())
                            .cloned()
                    else {
                        return;
                    };

                    let position =
                        transform_output_mapped_position(&output, &event, shell.zoom_state());
                    let under = State::surface_under(position, &output, &shell)
                        .map(|(target, pos)| (target, pos.as_logical()));

                    std::mem::drop(shell);

                    let serial = SERIAL_COUNTER.next_serial();
                    let touch = seat.get_touch().unwrap();
                    touch.down(
                        self,
                        under,
                        &DownEvent {
                            slot: event.slot(),
                            location: position.as_logical(),
                            serial,
                            time: event.time_msec(),
                        },
                    );
                }
            }
            InputEvent::TouchMotion { event, .. } => {
                let shell = self.common.shell.write();
                if let Some(seat) = shell
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned()
                {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    let Some(output) =
                        mapped_output_for_device(&self.common.config, &shell, &event.device())
                            .cloned()
                    else {
                        return;
                    };

                    let position =
                        transform_output_mapped_position(&output, &event, shell.zoom_state());
                    let under = State::surface_under(position, &output, &shell)
                        .map(|(target, pos)| (target, pos.as_logical()));

                    std::mem::drop(shell);

                    let touch = seat.get_touch().unwrap();
                    touch.motion(
                        self,
                        under,
                        &TouchMotionEvent {
                            slot: event.slot(),
                            location: position.as_logical(),
                            time: event.time_msec(),
                        },
                    );
                }
            }
            InputEvent::TouchUp { event, .. } => {
                let mut shell = self.common.shell.write();
                if let Some(Trigger::Touch(slot)) = shell.overview_mode().0.active_trigger()
                    && *slot == event.slot()
                {
                    shell.set_overview_mode(None, self.common.event_loop_handle.clone());
                }

                let maybe_seat = shell
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    std::mem::drop(shell);
                    let serial = SERIAL_COUNTER.next_serial();
                    let touch = seat.get_touch().unwrap();
                    touch.up(
                        self,
                        &UpEvent {
                            slot: event.slot(),
                            time: event.time_msec(),
                            serial,
                        },
                    );
                }
            }
            InputEvent::TouchCancel { event, .. } => {
                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    let touch = seat.get_touch().unwrap();
                    touch.cancel(self);
                }
            }
            InputEvent::TouchFrame { event, .. } => {
                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    let touch = seat.get_touch().unwrap();
                    touch.frame(self);
                }
            }

            InputEvent::TabletToolAxis { event, .. } => {
                let shell = self.common.shell.write();
                if let Some(seat) = shell
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned()
                {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    notify_cursor_activity(self, &seat);
                    let Some(output) =
                        mapped_output_for_device(&self.common.config, &shell, &event.device())
                            .cloned()
                    else {
                        return;
                    };

                    let position =
                        transform_output_mapped_position(&output, &event, shell.zoom_state());
                    let under = State::surface_under(position, &output, &shell)
                        .map(|(target, pos)| (target, pos.as_logical()));

                    std::mem::drop(shell);

                    let pointer = seat.get_pointer().unwrap();
                    pointer.motion(
                        self,
                        under.clone(),
                        &MotionEvent {
                            location: position.as_logical(),
                            serial: SERIAL_COUNTER.next_serial(),
                            time: self.common.clock.now().as_millis(),
                        },
                    );

                    let tablet_seat = seat.tablet_seat();

                    let tablet = tablet_seat.get_tablet(&TabletDescriptor::from(&event.device()));
                    let tool = tablet_seat.get_tool(&event.tool());

                    if let (Some(tablet), Some(tool)) = (tablet, tool) {
                        if event.pressure_has_changed() {
                            tool.pressure(event.pressure());
                        }
                        if event.distance_has_changed() {
                            tool.distance(event.distance());
                        }
                        if event.tilt_has_changed() {
                            tool.tilt(event.tilt());
                        }
                        if event.slider_has_changed() {
                            tool.slider_position(event.slider_position());
                        }
                        if event.rotation_has_changed() {
                            tool.rotation(event.rotation());
                        }
                        if event.wheel_has_changed() {
                            tool.wheel(event.wheel_delta(), event.wheel_delta_discrete());
                        }

                        tool.motion(
                            position.as_logical(),
                            under
                                .and_then(|(f, loc)| f.wl_surface().map(|s| (s.into_owned(), loc))),
                            &tablet,
                            SERIAL_COUNTER.next_serial(),
                            event.time_msec(),
                        );
                    }
                }
            }
            InputEvent::TabletToolProximity { event, .. } => {
                let shell = self.common.shell.write();
                if let Some(seat) = shell
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned()
                {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    notify_cursor_activity(self, &seat);
                    let Some(output) =
                        mapped_output_for_device(&self.common.config, &shell, &event.device())
                            .cloned()
                    else {
                        return;
                    };

                    let position =
                        transform_output_mapped_position(&output, &event, shell.zoom_state());
                    let under = State::surface_under(position, &output, &shell)
                        .map(|(target, pos)| (target, pos.as_logical()));

                    std::mem::drop(shell);

                    let pointer = seat.get_pointer().unwrap();
                    pointer.motion(
                        self,
                        under.clone(),
                        &MotionEvent {
                            location: position.as_logical(),
                            serial: SERIAL_COUNTER.next_serial(),
                            time: self.common.clock.now().as_millis(),
                        },
                    );

                    let tablet_seat = seat.tablet_seat();

                    let tablet = tablet_seat.get_tablet(&TabletDescriptor::from(&event.device()));
                    let dh = self.common.display_handle.clone();
                    let tool = tablet_seat.add_tool::<Self>(self, &dh, &event.tool());

                    if let Some(tablet) = tablet {
                        match event.state() {
                            ProximityState::In => {
                                if let Some(under) = under.and_then(|(f, loc)| {
                                    f.wl_surface().map(|s| (s.into_owned(), loc))
                                }) {
                                    tool.proximity_in(
                                        position.as_logical(),
                                        under,
                                        &tablet,
                                        SERIAL_COUNTER.next_serial(),
                                        event.time_msec(),
                                    )
                                }
                            }
                            ProximityState::Out => tool.proximity_out(event.time_msec()),
                        }
                    }
                }
            }
            InputEvent::TabletToolTip { event, .. } => {
                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    notify_cursor_activity(self, &seat);
                    if let Some(tool) = seat.tablet_seat().get_tool(&event.tool()) {
                        match event.tip_state() {
                            TabletToolTipState::Down => {
                                tool.tip_down(SERIAL_COUNTER.next_serial(), event.time_msec());
                            }
                            TabletToolTipState::Up => {
                                tool.tip_up(event.time_msec());
                            }
                        }
                    }
                }
            }
            InputEvent::TabletToolButton { event, .. } => {
                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .seats
                    .for_device(&event.device(), &backend_id)
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    notify_cursor_activity(self, &seat);
                    if let Some(tool) = seat.tablet_seat().get_tool(&event.tool()) {
                        tool.button(
                            event.button(),
                            event.button_state(),
                            SERIAL_COUNTER.next_serial(),
                            event.time_msec(),
                        );
                    }
                }
            }
            InputEvent::Special(_) => {}
            #[allow(unused_variables)]
            InputEvent::SwitchToggle { event } => {
                #[cfg(feature = "logind")]
                if event.switch() == Some(Switch::Lid) && self.common.inhibit_lid_fd.is_some() {
                    let backend = self.backend.lock();
                    let output = backend
                        .all_outputs()
                        .iter()
                        .find(|o| o.is_internal())
                        .cloned();
                    let closed = event.state() == SwitchState::On;

                    if closed {
                        backend
                            .disable_internal_output(&mut self.common.output_configuration_state);
                    } else {
                        backend.enable_internal_output(&mut self.common.output_configuration_state);
                    }
                    std::mem::drop(backend);

                    if let Err(err) = self.refresh_output_config() {
                        if !closed {
                            tracing::warn!(?err, "Failed to re-enable internal connector");
                            if let Some(output) = output {
                                use cosmic_comp_config::output::comp::OutputState;

                                output.config_mut().enabled = OutputState::Disabled;
                                if let Err(err) = self.refresh_output_config() {
                                    error!("Unrecoverable output configuration error: {}", err);
                                }
                            }
                        } else {
                            // Disabling an output should never fail.
                            error!("Unrecoverable output configuration error: {}", err);
                        }
                    }
                }
            }
        }
    }

    /// The modifier state held by the source that produced an event.
    ///
    /// For a libei connection this is its own isolated keyboard state; for everything else
    /// it is the seat's (physical) keyboard.
    pub(crate) fn source_modifiers(
        &self,
        backend_id: &InputBackendId,
        seat: &Seat<State>,
    ) -> ModifiersState {
        match backend_id {
            InputBackendId::Ei(conn) => self
                .common
                .ei_isolated_kbd
                .get(conn)
                .map(|iso| iso.modifier_state())
                .unwrap_or_default(),
            _ => seat
                .get_keyboard()
                .map(|k| k.modifier_state())
                .unwrap_or_default(),
        }
    }

    pub(crate) fn clear_input_source_state(&mut self, backend_id: &InputBackendId) {
        let seats = self
            .common
            .shell
            .read()
            .seats
            .iter()
            .cloned()
            .collect::<Vec<_>>();
        for seat in seats {
            seat.supressed_keys().clear_source(backend_id);
            seat.supressed_buttons().clear_source(backend_id);
            seat.modifiers_shortcut_queue().clear(backend_id);
            seat.clear_last_modifier_change(backend_id);
        }
    }

    pub(crate) fn release_ei_keyboard(&mut self, conn: &smithay::reexports::reis::eis::Connection) {
        let pressed_keys = self
            .common
            .ei_isolated_kbd
            .get(conn)
            .map(|iso| iso.pressed_keys().collect::<Vec<_>>())
            .unwrap_or_default();

        for keycode in pressed_keys.into_iter().rev() {
            self.inject_ei_key_internal(conn, keycode, KeyState::Released, false);
        }
    }

    pub(crate) fn process_keyboard_filter(
        &mut self,
        backend_id: &InputBackendId,
        seat: &Seat<State>,
        modifiers: &ModifiersState,
        handle: KeysymHandle<'_>,
        serial: Serial,
        time: u32,
        keycode: Keycode,
        key_state: KeyState,
        previous_modifiers: ModifiersState,
    ) -> FilterResult<Option<(Action, shortcuts::Binding)>> {
        if previous_modifiers != *modifiers {
            seat.set_last_modifier_change(backend_id, serial);
        }

        let current_focus = seat.get_keyboard().unwrap().current_focus();
        let shortcuts_inhibited = current_focus.as_ref().is_some_and(|f| {
            f.wl_surface()
                .map(|surface| {
                    seat.keyboard_shortcuts_inhibitor_for_surface(&surface)
                        .map(|inhibitor| inhibitor.is_active())
                        .unwrap_or(false)
                        || seat.has_active_xwayland_grab(&surface)
                })
                .unwrap_or(false)
        });
        let sym = handle.modified_sym();

        let result = self.filter_keyboard_input(
            backend_id, seat, modifiers, handle, serial, keycode, key_state, time,
        );

        if (matches!(result, FilterResult::Forward)
            && !seat.get_keyboard().unwrap().is_grabbed()
            && !shortcuts_inhibited
            && !matches!(current_focus, Some(KeyboardFocusTarget::LockSurface(_))))
        // we don't want to accidentally leave any keys pressed
            || key_state == KeyState::Released
        {
            self.common
                .xwayland_notify_key_event(sym, keycode, key_state, *modifiers, serial, time);
        }

        result
    }

    /// Inject a key through an isolated keyboard state: run it through the per-source shortcut
    /// filter and deliver it to the focused client, keeping this source's modifier/key state
    /// fully isolated from the physical keyboard. Shared by libei connections and virtual keyboards.
    ///
    /// `handle_shortcuts` is `false` when synthesizing releases during teardown, so still-held
    /// keys are just forwarded without re-triggering bindings.
    pub(crate) fn inject_isolated_key(
        &mut self,
        backend_id: &InputBackendId,
        seat: &Seat<State>,
        iso: &mut smithay::input::keyboard::IsolatedKeyboardState,
        keycode: Keycode,
        key_state: KeyState,
        handle_shortcuts: bool,
    ) {
        let Some(keyboard) = seat.get_keyboard() else {
            return;
        };
        let serial = SERIAL_COUNTER.next_serial();
        let time = self.common.clock.now().as_millis();
        let previous_modifiers = iso.modifier_state();
        let result = keyboard
            .input_isolated(
                self,
                iso,
                keycode,
                key_state,
                serial,
                time,
                |data, modifiers, handle| {
                    if handle_shortcuts {
                        data.process_keyboard_filter(
                            backend_id,
                            seat,
                            modifiers,
                            handle,
                            serial,
                            time,
                            keycode,
                            key_state,
                            previous_modifiers,
                        )
                    } else {
                        FilterResult::Forward
                    }
                },
            )
            .flatten();

        if let Some((action, pattern)) = result {
            self.handle_action(action, backend_id, seat, serial, time, pattern, None);
        }
    }

    /// The seat's currently active keyboard layout group.
    fn seat_active_layout(&mut self, seat: &Seat<State>) -> Option<Layout> {
        let keyboard = seat.get_keyboard()?;
        Some(keyboard.with_xkb_state(self, |context| {
            context.xkb().lock().unwrap().active_layout()
        }))
    }

    /// Inject a key from a libei connection through its isolated keyboard state.
    pub(crate) fn inject_ei_key(
        &mut self,
        conn: &smithay::reexports::reis::eis::Connection,
        keycode: Keycode,
        key_state: KeyState,
    ) {
        self.inject_ei_key_internal(conn, keycode, key_state, true);
    }

    fn inject_ei_key_internal(
        &mut self,
        conn: &smithay::reexports::reis::eis::Connection,
        keycode: Keycode,
        key_state: KeyState,
        handle_shortcuts: bool,
    ) {
        let seat = self.common.shell.read().seats.last_active().clone();
        let backend_id = InputBackendId::Ei(conn.clone());
        // Temporarily take the isolated state out so we can borrow `self` mutably for delivery.
        let Some(mut iso) = self.common.ei_isolated_kbd.remove(conn) else {
            return;
        };
        // Mirror the seat's active layout group so injection matches the user's layout.
        if let Some(layout) = self.seat_active_layout(&seat) {
            iso.set_layout(layout);
        }
        self.inject_isolated_key(
            &backend_id,
            &seat,
            &mut iso,
            keycode,
            key_state,
            handle_shortcuts,
        );
        self.common.ei_isolated_kbd.insert(conn.clone(), iso);
    }

    /// Resolve a keysym from a libei `ei_text` device to a keycode
    pub(crate) fn inject_ei_text_keysym(
        &mut self,
        conn: &smithay::reexports::reis::eis::Connection,
        keysym: u32,
        key_state: KeyState,
        handle_shortcuts: bool,
    ) {
        use smithay::wayland::input_method::InputMethodSeat;
        use smithay::wayland::text_input::TextInputSeat;

        let keysym = Keysym::new(keysym);

        // Mirror the seat's active layout group onto this source, so the keysym resolves in
        let active_layout = {
            let seat = self.common.shell.read().seats.last_active().clone();
            self.seat_active_layout(&seat)
        };
        if let Some(layout) = active_layout
            && let Some(iso) = self.common.ei_isolated_kbd.get_mut(conn)
        {
            iso.set_layout(layout);
        }

        // If this keysym was injected via the keycode path on press, its release must go the
        // same way regardless of the current modifier state.
        // e.g. Rustdesk may release the modifier *before* the key
        let forced_keycode_release = key_state == KeyState::Released
            && self
                .common
                .ei_text_keycode_held
                .get(conn)
                .is_some_and(|held| held.contains(&keysym));

        // if a printable, non-control keysym with no modifier held + text-input protocol when a text-input client is focused then commit the character directly
        // (which also handles out-of-layout / Unicode that has no keycode)
        // otherwise go through to keycode injection, which reaches every app.
        let no_mods = self.common.ei_isolated_kbd.get(conn).is_some_and(|iso| {
            let m = iso.modifier_state();
            !(m.ctrl || m.alt || m.shift || m.logo)
        });
        if !forced_keycode_release
            && no_mods
            && let Some(c) = keysym.key_char()
            && !c.is_control()
        {
            let seat = self.common.shell.read().seats.last_active().clone();
            // Only commit through text-input when we're the active input method (no real IME).
            if !seat.input_method().has_instance() {
                let text_input = seat.text_input();
                let mut handled = false;
                text_input.with_active_text_input(|ti, _surface| {
                    // A text-input client is focused: commit on press, no-op on release.
                    if key_state == KeyState::Pressed {
                        ti.commit_string(Some(c.to_string()));
                    }
                    handled = true;
                });
                if handled {
                    if key_state == KeyState::Pressed {
                        text_input.done(false);
                    }
                    return;
                }
                // No active text-input: fall through to keycode injection (press and release).
            }
        }

        // If the keysym exists in the active layout, inject its real keycode with the
        // shift-level modifiers applied (no keymap change). Out-of-layout / Unicode keysyms
        // have no keycode and can only be delivered via the text-input fast path above.
        // TODO: update this doc when we have the final solution
        let resolved = self
            .common
            .ei_isolated_kbd
            .get(conn)
            .and_then(|iso| iso.keycode_for_keysym(keysym));

        match resolved {
            Some((keycode, mask)) => {
                // Remember (press) / forget (release) that this keysym is held via keycode
                match key_state {
                    KeyState::Pressed => {
                        self.common
                            .ei_text_keycode_held
                            .entry(conn.clone())
                            .or_default()
                            .insert(keysym);
                    }
                    KeyState::Released => {
                        if let Some(held) = self.common.ei_text_keycode_held.get_mut(conn) {
                            held.remove(&keysym);
                        }
                    }
                }
                self.inject_ei_keysym_in_layout(conn, keycode, mask, key_state, handle_shortcuts)
            }
            // Out-of-layout / Unicode: bind a spare keycode to the keysym and inject it, on the
            // press event only (KWin's fallback).
            None if key_state == KeyState::Pressed => {
                self.inject_ei_keysym_remapped(conn, keysym, handle_shortcuts)
            }
            None => {}
        }
    }

    /// Inject an in-layout keysym by its real keycode, bracketing it with the shift-level
    /// modifiers it needs (KWin's primary keysym-injection path).
    fn inject_ei_keysym_in_layout(
        &mut self,
        conn: &smithay::reexports::reis::eis::Connection,
        keycode: Keycode,
        mask: u32,
        key_state: KeyState,
        handle_shortcuts: bool,
    ) {
        let seat = self.common.shell.read().seats.last_active().clone();
        let Some(keyboard) = seat.get_keyboard() else {
            return;
        };
        let backend_id = InputBackendId::Ei(conn.clone());
        let Some(mut iso) = self.common.ei_isolated_kbd.remove(conn) else {
            return;
        };

        if mask == 0 {
            self.inject_isolated_key(
                &backend_id,
                &seat,
                &mut iso,
                keycode,
                key_state,
                handle_shortcuts,
            );
        } else {
            // Momentarily OR in the level's modifiers, deliver them, send the key, then restore
            // the real modifier state (so e.g. injecting `!` doesn't leave Shift stuck).
            let mods = iso.serialized_mods();
            iso.update_modifiers(SerializedMods {
                depressed: mods.depressed | mask,
                ..mods
            });
            keyboard.input_isolated_modifiers(self, &iso);
            self.inject_isolated_key(
                &backend_id,
                &seat,
                &mut iso,
                keycode,
                key_state,
                handle_shortcuts,
            );
            iso.update_modifiers(mods);
            keyboard.input_isolated_modifiers(self, &iso);
        }

        self.common.ei_isolated_kbd.insert(conn.clone(), iso);
    }

    /// Inject an out-of-layout keysym via a temporary spare-keycode keymap (KWin's fallback),
    /// as an atomic press+release so the keymap only changes while no key is held.
    fn inject_ei_keysym_remapped(
        &mut self,
        conn: &smithay::reexports::reis::eis::Connection,
        keysym: Keysym,
        handle_shortcuts: bool,
    ) {
        let seat = self.common.shell.read().seats.last_active().clone();
        let backend_id = InputBackendId::Ei(conn.clone());
        let Some(mut iso) = self.common.ei_isolated_kbd.remove(conn) else {
            return;
        };
        if let Some(keycode) = iso.remap_keysym_to_spare_keycode(keysym) {
            self.inject_isolated_key(
                &backend_id,
                &seat,
                &mut iso,
                keycode,
                KeyState::Pressed,
                handle_shortcuts,
            );
            self.inject_isolated_key(
                &backend_id,
                &seat,
                &mut iso,
                keycode,
                KeyState::Released,
                handle_shortcuts,
            );
            iso.restore_keymap();
        } else {
            tracing::warn!("EI text keysym {:?} could not be mapped; ignoring", keysym);
        }
        self.common.ei_isolated_kbd.insert(conn.clone(), iso);
    }

    /// Determine is key event should be intercepted as a key binding, or forwarded to surface
    #[profiling::function]
    pub fn filter_keyboard_input(
        &mut self,
        backend_id: &InputBackendId,
        seat: &Seat<Self>,
        modifiers: &ModifiersState,
        handle: KeysymHandle<'_>,
        serial: Serial,
        keycode: Keycode,
        key_state: KeyState,
        time: u32,
    ) -> FilterResult<Option<(Action, shortcuts::Binding)>> {
        // Pre-compute for layout-agnostic shortcut matching
        let raw_syms = handle.raw_syms();
        let latin_sym = handle.raw_latin_sym_or_raw_current_sym();
        let key_matches = |binding_key: Keysym| -> bool {
            raw_syms.contains(&binding_key) || latin_sym.is_some_and(|sym| sym == binding_key)
        };

        let mut shell = self.common.shell.write();

        let keyboard = seat.get_keyboard().unwrap();
        let pointer = seat.get_pointer().unwrap();
        // We're only interested in filtering keyboard grabs if we initiated them.
        // The easiest way to check that is to check the type of the grab.
        let keyboard_grabbed = keyboard.with_grab(|_serial, grab| {
            grab.is::<SwapWindowGrab>() || grab.is::<PopupKeyboardGrab<State>>()
        }) == Some(true);
        let is_grabbed = keyboard_grabbed || pointer.is_grabbed();

        let current_focus = keyboard.current_focus();
        //this should fall back to active output since there may not be a focused output
        let focused_output = seat.focused_or_active_output();

        let shortcuts_inhibited = current_focus.as_ref().is_some_and(|f| {
            f.wl_surface()
                .map(|surface| {
                    seat.keyboard_shortcuts_inhibitor_for_surface(&surface)
                        .map(|inhibitor| inhibitor.is_active())
                        .unwrap_or(false)
                        || seat.has_active_xwayland_grab(&surface)
                })
                .unwrap_or(false)
        });

        if let Some(a11y_keyboard_monitor) = self.common.dbus_state.a11y_keyboard_monitor() {
            a11y_keyboard_monitor.key_event(modifiers, &handle, key_state);
        }

        // Leave move overview mode, if any modifier was released
        if let Some(Trigger::KeyboardMove(action_modifiers)) =
            shell.overview_mode().0.active_trigger()
            && ((action_modifiers.ctrl && !modifiers.ctrl)
                || (action_modifiers.alt && !modifiers.alt)
                || (action_modifiers.logo && !modifiers.logo)
                || (action_modifiers.shift && !modifiers.shift))
        {
            shell.set_overview_mode(None, self.common.event_loop_handle.clone());
        }
        // Leave swap overview mode, if any key was released
        if let Some(Trigger::KeyboardSwap(action_pattern, old_descriptor)) =
            shell.overview_mode().0.active_trigger()
            && ((action_pattern.modifiers.ctrl && !modifiers.ctrl)
                || (action_pattern.modifiers.alt && !modifiers.alt)
                || (action_pattern.modifiers.logo && !modifiers.logo)
                || (action_pattern.modifiers.shift && !modifiers.shift)
                || (action_pattern.key.is_some()
                    && key_matches(action_pattern.key.unwrap())
                    && key_state == KeyState::Released))
        {
            shell.set_overview_mode(None, self.common.event_loop_handle.clone());

            self.keyboard_swap(
                seat,
                &mut shell,
                old_descriptor,
                current_focus,
                &focused_output,
            );
        }

        // Leave or update resize mode, if modifiers changed or initial key was released
        if let Some(action_pattern) = shell.resize_mode().0.active_binding() {
            if action_pattern.key.is_some()
                && key_state == KeyState::Released
                && key_matches(action_pattern.key.unwrap())
            {
                shell.set_resize_mode(
                    None,
                    &self.common.config,
                    self.common.event_loop_handle.clone(),
                );
            } else if !cosmic_modifiers_eq_smithay(&action_pattern.modifiers, modifiers) {
                let mut new_pattern = action_pattern.clone();
                new_pattern.modifiers = cosmic_modifiers_from_smithay(*modifiers);
                let enabled =
                    self.common
                        .config
                        .shortcuts
                        .iter()
                        .find_map(move |(binding, action)| {
                            if binding == &new_pattern
                                && matches!(action, shortcuts::Action::Resizing(_))
                            {
                                let shortcuts::Action::Resizing(direction) = action else {
                                    unreachable!()
                                };
                                Some((new_pattern.clone(), *direction))
                            } else {
                                None
                            }
                        });
                shell.set_resize_mode(
                    enabled,
                    &self.common.config,
                    self.common.event_loop_handle.clone(),
                );
            }
        }

        // Special case resizing with regards to arrow keys
        if let Some(direction) = shell.resize_mode().0.active_direction() {
            let resize_edge = match handle.modified_sym() {
                Keysym::Left | Keysym::h | Keysym::H => Some(ResizeEdge::LEFT),
                Keysym::Down | Keysym::j | Keysym::J => Some(ResizeEdge::BOTTOM),
                Keysym::Up | Keysym::k | Keysym::K => Some(ResizeEdge::TOP),
                Keysym::Right | Keysym::l | Keysym::L => Some(ResizeEdge::RIGHT),
                _ => None,
            };

            if let Some(mut edge) = resize_edge {
                if direction == ResizeDirection::Inwards {
                    edge.flip_direction();
                }
                let action = Action::Private(PrivateAction::Resizing(
                    direction,
                    edge.into(),
                    cosmic_keystate_from_smithay(key_state),
                ));
                let key_pattern = shortcuts::Binding {
                    modifiers: cosmic_modifiers_from_smithay(*modifiers),
                    keycode: None,
                    key: Some(handle.modified_sym()),
                    description: None,
                };

                if key_state == KeyState::Released {
                    if let Some(tokens) = seat.supressed_keys().filter(backend_id, &handle) {
                        for token in tokens {
                            self.common.event_loop_handle.remove(token);
                        }
                    }
                } else {
                    let seat_clone = seat.clone();
                    let action_clone = action.clone();
                    let key_pattern_clone = key_pattern.clone();
                    let backend_id_clone = backend_id.clone();
                    let start = Instant::now();
                    let token = self
                        .common
                        .event_loop_handle
                        .insert_source(
                            Timer::from_duration(Duration::from_millis(200)),
                            move |current, _, state| {
                                let duration = current.duration_since(start).as_millis();
                                state.handle_action(
                                    action_clone.clone(),
                                    &backend_id_clone,
                                    &seat_clone,
                                    serial,
                                    time.overflowing_add(duration as u32).0,
                                    key_pattern_clone.clone(),
                                    None,
                                );
                                calloop::timer::TimeoutAction::ToDuration(Duration::from_millis(25))
                            },
                        )
                        .ok();

                    seat.supressed_keys().add(backend_id, &handle, token);
                }
                return FilterResult::Intercept(Some((action, key_pattern)));
            }
        }

        std::mem::drop(shell);

        // cancel grabs
        if is_grabbed
            && handle.modified_sym() == Keysym::Escape
            && key_state == KeyState::Pressed
            && !modifiers.alt
            && !modifiers.ctrl
            && !modifiers.logo
            && !modifiers.shift
        {
            seat.supressed_keys().add(backend_id, &handle, None);
            return FilterResult::Intercept(Some((
                Action::Private(PrivateAction::Escape),
                shortcuts::Binding {
                    modifiers: shortcuts::Modifiers::default(),
                    keycode: None,
                    key: Some(Keysym::Escape),
                    description: None,
                },
            )));
        }

        if let Some(mut a11y_keyboard_monitor) = self.common.dbus_state.a11y_keyboard_monitor() {
            if key_state == KeyState::Released {
                let removed =
                    a11y_keyboard_monitor.remove_active_virtual_mod(handle.modified_sym());
                // If `Caps_Lock` is a virtual modifier, and is in locked state, clear it
                if removed
                    && handle.modified_sym() == Keysym::Caps_Lock
                    && (modifiers.serialized.locked & 2) != 0
                {
                    let seat = seat.clone();
                    let key_code = keycode;
                    self.common.event_loop_handle.insert_idle(move |state| {
                        if let Some(keyboard) = seat.get_keyboard() {
                            let serial = SERIAL_COUNTER.next_serial();
                            let time = state.common.clock.now().as_millis();
                            keyboard.input(
                                state,
                                key_code,
                                KeyState::Pressed,
                                serial,
                                time,
                                |_, _, _| FilterResult::<()>::Forward,
                            );
                            let serial = SERIAL_COUNTER.next_serial();
                            keyboard.input(
                                state,
                                key_code,
                                KeyState::Released,
                                serial,
                                time,
                                |_, _, _| FilterResult::<()>::Forward,
                            );
                        }
                    });
                }
            } else if key_state == KeyState::Pressed
                && a11y_keyboard_monitor.has_virtual_mod(handle.modified_sym())
            {
                a11y_keyboard_monitor.add_active_virtual_mod(handle.modified_sym());

                tracing::debug!(
                    "active virtual mods: {:?}",
                    a11y_keyboard_monitor.active_virtual_mods()
                );
                seat.supressed_keys().add(backend_id, &handle, None);

                return FilterResult::Intercept(None);
            }
        }

        // Skip released events for initially surpressed keys
        if key_state == KeyState::Released
            && let Some(tokens) = seat.supressed_keys().filter(backend_id, &handle)
        {
            for token in tokens {
                self.common.event_loop_handle.remove(token);
            }
            return FilterResult::Intercept(None);
        }

        // Handle VT switches
        if key_state == KeyState::Pressed
            && (Keysym::XF86_Switch_VT_1.raw()..=Keysym::XF86_Switch_VT_12.raw())
                .contains(&handle.modified_sym().raw())
        {
            if let Err(err) = self.backend.kms().switch_vt(
                (handle.modified_sym().raw() - Keysym::XF86_Switch_VT_1.raw() + 1) as i32,
            ) {
                error!(?err, "Failed switching virtual terminal.");
            }
            seat.supressed_keys().add(backend_id, &handle, None);
            return FilterResult::Intercept(None);
        }

        if let Some(a11y_keyboard_monitor) = self.common.dbus_state.a11y_keyboard_monitor()
            && key_state == KeyState::Pressed
            && (a11y_keyboard_monitor.has_keyboard_grab()
                || a11y_keyboard_monitor.has_key_grab(modifiers, handle.modified_sym()))
        {
            let modifiers_queue = seat.modifiers_shortcut_queue();
            modifiers_queue.clear(backend_id);
            seat.supressed_keys().add(backend_id, &handle, None);
            return FilterResult::Intercept(None);
        }

        // handle the rest of the global shortcuts
        let mut clear_queue = true;
        if !shortcuts_inhibited {
            let modifiers_queue = seat.modifiers_shortcut_queue();

            for (binding, action) in self.common.config.shortcuts.iter() {
                if *action == shortcuts::Action::Disable {
                    continue;
                }

                // is this a released (triggered) modifier-only binding?
                if binding.key.is_none()
                    && key_state == KeyState::Released
                    && !cosmic_modifiers_eq_smithay(&binding.modifiers, modifiers)
                    && modifiers_queue.take(backend_id, binding)
                {
                    modifiers_queue.clear(backend_id);
                    return FilterResult::Intercept(Some((
                        Action::Shortcut(action.clone()),
                        binding.clone(),
                    )));
                }

                // could this potentially become a modifier-only binding?
                if binding.key.is_none()
                    && key_state == KeyState::Pressed
                    && cosmic_modifiers_eq_smithay(&binding.modifiers, modifiers)
                {
                    modifiers_queue.set(backend_id, binding.clone());
                    clear_queue = false;
                }

                // is this a normal binding?
                if binding.key.is_some()
                    && key_state == KeyState::Pressed
                    && key_matches(binding.key.unwrap())
                    && cosmic_modifiers_eq_smithay(&binding.modifiers, modifiers)
                {
                    modifiers_queue.clear(backend_id);
                    seat.supressed_keys().add(backend_id, &handle, None);
                    return FilterResult::Intercept(Some((
                        Action::Shortcut(action.clone()),
                        binding.clone(),
                    )));
                }
            }
        }

        // no binding
        if clear_queue {
            seat.modifiers_shortcut_queue().clear(backend_id);
        }
        // keys are passed through to apps
        FilterResult::Forward
    }

    fn keyboard_swap(
        &self,
        seat: &Seat<Self>,
        shell: &mut Shell,
        old_descriptor: &NodeDesc,
        current_focus: Option<KeyboardFocusTarget>,
        focused_output: &Output,
    ) {
        if let Some(focus) = current_focus {
            if let Some(new_descriptor) = shell
                .workspaces
                .active(focused_output)
                .unwrap()
                .1
                .node_desc(focus)
            {
                let mut spaces = shell.workspaces.spaces_mut();
                if old_descriptor.handle != new_descriptor.handle {
                    let (mut old_w, mut other_w) =
                        spaces.partition::<Vec<_>, _>(|w| w.handle == old_descriptor.handle);
                    if let Some(old_workspace) = old_w.get_mut(0)
                        && let Some(new_workspace) = other_w
                            .iter_mut()
                            .find(|w| w.handle == new_descriptor.handle)
                    {
                        {
                            let mut stack = new_workspace.focus_stack.get_mut(seat);
                            for elem in old_descriptor.focus_stack.iter().flat_map(|node_id| {
                                old_workspace.tiling_layer.element_for_node(node_id)
                            }) {
                                stack.append(elem.clone());
                            }
                        }
                        {
                            let mut stack = old_workspace.focus_stack.get_mut(seat);
                            for elem in new_descriptor.focus_stack.iter().flat_map(|node_id| {
                                new_workspace.tiling_layer.element_for_node(node_id)
                            }) {
                                stack.append(elem.clone());
                            }
                        }
                        if let Some(focus) = TilingLayout::swap_trees(
                            &mut old_workspace.tiling_layer,
                            Some(&mut new_workspace.tiling_layer),
                            old_descriptor,
                            &new_descriptor,
                        ) {
                            let seat = seat.clone();
                            self.common.event_loop_handle.insert_idle(move |state| {
                                Shell::set_focus(state, Some(&focus), &seat, None, true);
                            });
                        }
                        old_workspace.refresh_focus_stack();
                        new_workspace.refresh_focus_stack();
                    }
                } else if let Some(workspace) = spaces.find(|w| w.handle == new_descriptor.handle) {
                    if let Some(focus) = TilingLayout::swap_trees(
                        &mut workspace.tiling_layer,
                        None,
                        old_descriptor,
                        &new_descriptor,
                    ) {
                        std::mem::drop(spaces);
                        let seat = seat.clone();
                        self.common.event_loop_handle.insert_idle(move |state| {
                            Shell::set_focus(state, Some(&focus), &seat, None, true);
                        });
                    }
                    workspace.refresh_focus_stack();
                }
            }
        } else {
            let new_workspace = shell.workspaces.active(focused_output).unwrap().1.handle;
            if new_workspace != old_descriptor.handle {
                let spaces = shell.workspaces.spaces_mut();
                let (mut old_w, mut other_w) =
                    spaces.partition::<Vec<_>, _>(|w| w.handle == old_descriptor.handle);
                if let Some(old_workspace) = old_w.get_mut(0)
                    && let Some(new_workspace) =
                        other_w.iter_mut().find(|w| w.handle == new_workspace)
                    && new_workspace.tiling_layer.windows().next().is_none()
                {
                    {
                        let mut stack = new_workspace.focus_stack.get_mut(seat);
                        for elem in old_descriptor.focus_stack.iter().flat_map(|node_id| {
                            old_workspace.tiling_layer.element_for_node(node_id)
                        }) {
                            stack.append(elem.clone());
                        }
                    }
                    if let Some(focus) = TilingLayout::move_tree(
                        &mut old_workspace.tiling_layer,
                        &mut new_workspace.tiling_layer,
                        &new_workspace.handle,
                        seat,
                        new_workspace.focus_stack.get(seat).iter(),
                        old_descriptor.clone(),
                        None,
                    ) {
                        let seat = seat.clone();
                        self.common.event_loop_handle.insert_idle(move |state| {
                            Shell::set_focus(state, Some(&focus), &seat, None, true);
                        });
                    }
                    old_workspace.refresh_focus_stack();
                }
            }
        }
    }

    #[profiling::function]
    pub fn element_under(
        global_pos: Point<f64, Global>,
        output: &Output,
        shell: &Shell,
        seat: &Seat<State>,
    ) -> Option<KeyboardFocusTarget> {
        let (previous_workspace, workspace) = shell.workspaces.active(output)?;
        let (previous_idx, idx) = shell.workspaces.active_num(output);
        let previous_workspace = previous_workspace
            .zip(previous_idx)
            .map(|((w, start), idx)| (w.handle, idx, start));
        let workspace = (workspace.handle, idx);
        let element_filter = if workspace_overview_is_open(output) {
            ElementFilter::LayerShellOnly
        } else {
            ElementFilter::All
        };

        render_input_order(
            shell,
            output,
            previous_workspace,
            workspace,
            element_filter,
            |stage| {
                match stage {
                    Stage::ZoomUI => {}
                    Stage::SessionLock(lock_surface) => {
                        return ControlFlow::Break(Ok(lock_surface
                            .cloned()
                            .map(KeyboardFocusTarget::LockSurface)));
                    }
                    Stage::LayerPopup {
                        layer,
                        popup,
                        location,
                        ..
                    } => {
                        if layer.can_receive_keyboard_focus() {
                            let surface = popup.wl_surface();
                            if under_from_surface_tree(
                                surface,
                                global_pos.as_logical(),
                                location.as_logical(),
                                WindowSurfaceType::POPUP | WindowSurfaceType::SUBSURFACE,
                            )
                            .is_some()
                            {
                                return ControlFlow::Break(Ok(Some(
                                    KeyboardFocusTarget::LayerSurface(layer),
                                )));
                            }
                        }
                    }
                    Stage::LayerSurface {
                        layer, location, ..
                    } => {
                        if under_from_surface_tree(
                            layer.wl_surface(),
                            global_pos.as_logical(),
                            location.as_logical(),
                            WindowSurfaceType::TOPLEVEL | WindowSurfaceType::SUBSURFACE,
                        )
                        .is_some()
                        {
                            return ControlFlow::Break(Ok(if layer.can_receive_keyboard_focus() {
                                Some(KeyboardFocusTarget::LayerSurface(layer))
                            } else {
                                // Don't change keyboard focus if in input region of layer shell
                                // surface, but surface doesn't have keyboard interactivity.
                                None
                            }));
                        }
                    }
                    Stage::OverrideRedirect { .. } => {
                        // Override redirect windows take a grab on their own via
                        // the Xwayland keyboard grab protocol. Don't focus them via click.
                    }
                    Stage::StickyPopups(layout) => {
                        if let Some(element) =
                            layout.popup_element_under(global_pos.to_local(output), seat)
                        {
                            return ControlFlow::Break(Ok(Some(element)));
                        }
                    }
                    Stage::Sticky(layout) => {
                        if let Some(element) =
                            layout.toplevel_element_under(global_pos.to_local(output), seat)
                        {
                            return ControlFlow::Break(Ok(Some(element)));
                        }
                    }
                    Stage::WorkspacePopups { workspace, offset } => {
                        let location = global_pos + offset.as_global().to_f64();
                        let output = workspace.output();
                        let output_geo = output.geometry().to_local(output);
                        if Rectangle::new(offset.as_local(), output_geo.size)
                            .intersection(output_geo)
                            .is_some_and(|geometry| {
                                geometry.contains(global_pos.to_local(output).to_i32_floor())
                            })
                            && let Some(element) = workspace.popup_element_under(location, seat)
                        {
                            return ControlFlow::Break(Ok(Some(element)));
                        }
                    }
                    Stage::Workspace { workspace, offset } => {
                        let location = global_pos + offset.as_global().to_f64();
                        let output = workspace.output();
                        let output_geo = output.geometry().to_local(output);
                        if Rectangle::new(offset.as_local(), output_geo.size)
                            .intersection(output_geo)
                            .is_some_and(|geometry| {
                                geometry.contains(global_pos.to_local(output).to_i32_floor())
                            })
                            && let Some(element) = workspace.toplevel_element_under(location, seat)
                        {
                            return ControlFlow::Break(Ok(Some(element)));
                        }
                    }
                }
                ControlFlow::Continue(())
            },
        )
        .ok()
        .flatten()
    }

    #[profiling::function]
    pub fn surface_under(
        global_pos: Point<f64, Global>,
        output: &Output,
        shell: &Shell,
    ) -> Option<(PointerFocusTarget, Point<f64, Global>)> {
        let (previous_workspace, workspace) = shell.workspaces.active(output)?;
        let (previous_idx, idx) = shell.workspaces.active_num(output);
        let previous_workspace = previous_workspace
            .zip(previous_idx)
            .map(|((w, start), idx)| (w.handle, idx, start));
        let workspace = (workspace.handle, idx);

        let element_filter = if workspace_overview_is_open(output) {
            ElementFilter::LayerShellOnly
        } else {
            ElementFilter::All
        };

        let relative_pos = global_pos.to_local(output);
        let output_geo = output.geometry();
        let overview = shell.overview_mode().0;
        let seat = shell.seats.last_active();

        render_input_order(
            shell,
            output,
            previous_workspace,
            workspace,
            element_filter,
            |stage| {
                match stage {
                    Stage::ZoomUI => {
                        if let Some(zoom_state) = shell.zoom_state()
                            && let Some((target, loc)) =
                                zoom_state.surface_under(output, global_pos)
                        {
                            return ControlFlow::Break(Ok(Some((target, loc))));
                        }
                    }
                    Stage::SessionLock(lock_surface) => {
                        return ControlFlow::Break(Ok(lock_surface.and_then(|surface| {
                            let location = output_geo.loc;
                            if let Some((surface, surface_loc)) = under_from_surface_tree(
                                surface.wl_surface(),
                                global_pos.as_logical(),
                                location.as_logical(),
                                WindowSurfaceType::ALL,
                            ) {
                                Some((
                                    PointerFocusTarget::WlSurface {
                                        surface,
                                        toplevel: None,
                                    },
                                    surface_loc.as_global().to_f64(),
                                ))
                            } else {
                                None
                            }
                        })));
                    }
                    Stage::LayerPopup {
                        popup, location, ..
                    } => {
                        let surface = popup.wl_surface();
                        if let Some((surface, surface_loc)) = under_from_surface_tree(
                            surface,
                            global_pos.as_logical(),
                            location.as_logical(),
                            WindowSurfaceType::ALL,
                        ) {
                            return ControlFlow::Break(Ok(Some((
                                PointerFocusTarget::WlSurface {
                                    surface,
                                    toplevel: None,
                                },
                                surface_loc.as_global().to_f64(),
                            ))));
                        }
                    }
                    Stage::LayerSurface {
                        layer, location, ..
                    } => {
                        let surface = layer.wl_surface();
                        if let Some((surface, surface_loc)) = under_from_surface_tree(
                            surface,
                            global_pos.as_logical(),
                            location.as_logical(),
                            WindowSurfaceType::ALL,
                        ) {
                            return ControlFlow::Break(Ok(Some((
                                PointerFocusTarget::WlSurface {
                                    surface,
                                    toplevel: None,
                                },
                                surface_loc.as_global().to_f64(),
                            ))));
                        }
                    }
                    Stage::OverrideRedirect { surface, location } => {
                        if let Some((_, surface_loc)) = surface.surface_under(
                            global_pos.as_logical(),
                            location.as_logical(),
                            WindowSurfaceType::ALL,
                        ) {
                            return ControlFlow::Break(Ok(Some((
                                PointerFocusTarget::X11Surface {
                                    surface: surface.clone(),
                                    toplevel: None,
                                },
                                surface_loc.as_global().to_f64(),
                            ))));
                        }
                    }
                    Stage::StickyPopups(floating_layer) => {
                        if let Some(under) = floating_layer
                            .popup_surface_under(relative_pos, seat)
                            .map(|(target, point)| (target, point.to_global(output)))
                        {
                            return ControlFlow::Break(Ok(Some(under)));
                        }
                    }
                    Stage::Sticky(floating_layer) => {
                        if let Some(under) = floating_layer
                            .toplevel_surface_under(relative_pos, seat)
                            .map(|(target, point)| (target, point.to_global(output)))
                        {
                            return ControlFlow::Break(Ok(Some(under)));
                        }
                    }
                    Stage::WorkspacePopups { workspace, offset } => {
                        let global_pos = global_pos + offset.to_f64().as_global();
                        if let Some(under) =
                            workspace.popup_surface_under(global_pos, overview.clone(), seat)
                        {
                            return ControlFlow::Break(Ok(Some(under)));
                        }
                    }
                    Stage::Workspace { workspace, offset } => {
                        let global_pos = global_pos + offset.to_f64().as_global();
                        if let Some(under) =
                            workspace.toplevel_surface_under(global_pos, overview.clone(), seat)
                        {
                            return ControlFlow::Break(Ok(Some(under)));
                        }
                    }
                }

                ControlFlow::Continue(())
            },
        )
        .ok()
        .flatten()
    }

    pub fn apply_cursor_hint(
        &mut self,
        surface: &WlSurface,
        pointer: &PointerHandle<Self>,
        mut location: Point<f64, Logical>,
    ) {
        let Some(client) = surface.client() else {
            return;
        };
        location = location.downscale(self.client_compositor_state(&client).client_scale());

        let point_and_output = {
            let shell = self.common.shell.read();
            let found = shell.workspaces.sets.iter().find_map(|(out, set)| {
                set.surface_geometry_offset_from_toplevel(surface)
                    .map(|(geometry, surface_offset)| (out, geometry, surface_offset))
            });

            if let Some((output, geometry, surface_offset)) = found {
                let pos_in_element = location + surface_offset.to_f64();
                let window_size = geometry.size.to_f64();

                let is_legal = |p: Point<f64, Logical>| {
                    let in_window =
                        p.x >= 0.0 && p.y >= 0.0 && p.x < window_size.w && p.y < window_size.h;
                    if !in_window {
                        return false;
                    }

                    with_pointer_constraint(surface, pointer, |constraint| {
                        if let Some(constraint) = constraint
                            && let Some(region) = constraint.region()
                        {
                            let point_in_surface = (p - surface_offset.to_f64()).to_i32_floor();
                            return region.contains(point_in_surface);
                        }
                        true
                    })
                };

                let workspace_origin = output.geometry().loc.to_f64();
                let origin = geometry.loc.to_f64();

                if is_legal(pos_in_element) {
                    let x = workspace_origin.x + origin.x + pos_in_element.x;
                    let y = workspace_origin.y + origin.y + pos_in_element.y;
                    Some((Point::<_, Global>::new(x, y), output.clone()))
                } else {
                    None
                }
            } else {
                None
            }
        };

        if let Some((point, output)) = point_and_output {
            // TODO: Replace with `wl_pointer.warp`
            let original_position = pointer.current_location();
            let serial = SERIAL_COUNTER.next_serial();
            let under = State::surface_under(point, &output, &self.common.shell.write())
                .map(|(target, pos)| (target, pos.as_logical()));
            let time = self.common.clock.now();
            pointer.relative_motion(
                self,
                under.clone(),
                &RelativeMotionEvent {
                    delta: (0., 0.).into(),
                    delta_unaccel: (0., 0.).into(),
                    utime: time.as_micros(),
                },
            );
            pointer.motion(
                self,
                under,
                &MotionEvent {
                    location: point.as_logical(),
                    serial,
                    time: time.as_millis(),
                },
            );
            pointer.frame(self);

            let mut shell = self.common.shell.write();
            shell.update_pointer_position(point.to_local(&output), &output);

            let seat = shell
                .seats
                .iter()
                .find(|s| s.get_pointer().as_ref() == Some(pointer))
                .cloned();

            if let Some(seat) = seat {
                shell.update_focal_point(
                    &seat,
                    original_position.as_global(),
                    self.common.config.cosmic_conf.accessibility_zoom.view_moves,
                );

                update_output_image_copy_cursor_position(
                    &shell,
                    &self.common.clock,
                    &output,
                    &seat,
                    point,
                );
            }
        }
    }
}

// Output and workspace sessions for the given output
fn cursor_sessions_for_output<'a>(
    shell: &'a Shell,
    output: &'a Output,
) -> impl Iterator<Item = CursorSessionRef> + 'a {
    shell
        .active_space(output)
        .into_iter()
        .flat_map(|workspace| {
            workspace
                .cursor_sessions()
                .into_iter()
                .chain(output.cursor_sessions())
        })
}

fn transform_output_mapped_position<B, E>(
    output: &Output,
    event: &E,
    zoom_state: Option<&ZoomState>,
) -> Point<f64, Global>
where
    B: InputBackend,
    E: AbsolutePositionEvent<B>,
    B::Device: 'static,
{
    let geometry = zoom_state
        .and_then(|_| output.zoomed_geometry())
        .unwrap_or_else(|| output.geometry());
    let transform = output.current_transform();
    let size = transform
        .invert()
        .transform_size(geometry.size.as_logical());
    geometry.loc.to_f64()
        + transform
            .transform_point_in(event.position_transformed(size), &size.to_f64())
            .as_global()
}

// TODO Is it possible to determine mapping for external touchscreen?
// Support map_to_region like sway?
fn mapped_output_for_device<'a, D: Device + 'static>(
    config: &Config,
    shell: &'a Shell,
    device: &D,
) -> Option<&'a Output> {
    let map_to_output = if let Some(device) = <dyn Any>::downcast_ref::<InputDevice>(device) {
        config
            .map_to_output(device)
            .and_then(|name| shell.outputs().find(|output| output.name() == name))
    } else {
        None
    };
    map_to_output.or_else(|| shell.builtin_output())
}

pub fn update_output_image_copy_cursor_position(
    shell: &Shell,
    clock: &Clock<Monotonic>,
    output: &Output,
    seat: &Seat<State>,
    position: Point<f64, Global>,
) {
    let output_geometry = output.geometry();
    for session in cursor_sessions_for_output(&shell, &output) {
        if let Some(cursor_geometry) = seat.cursor_geometry(
            (position - output_geometry.loc.to_f64())
                .as_logical()
                .to_buffer(
                    output.current_scale().fractional_scale(),
                    output.current_transform(),
                    &output_geometry.size.to_f64().as_logical(),
                ),
            clock.now(),
        ) {
            let constraints = cursor_capture_constraints(Some(cursor_geometry));
            if session
                .current_constraints()
                .map(|current_constraints| current_constraints.size != constraints.size)
                .unwrap_or(true)
            {
                session.update_constraints(constraints);
            }
            session.set_cursor_hotspot(cursor_geometry.hotspot);
            session.set_cursor_pos(Some(cursor_geometry.geometry.loc));
        }
    }
}
