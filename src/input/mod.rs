// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render::{
        ElementFilter,
        cursor::{hide_cursor, notify_cursor_activity},
    },
    config::{
        Action, Config, PrivateAction,
        key_bindings::{
            cosmic_keystate_from_smithay, cosmic_modifiers_eq_smithay,
            cosmic_modifiers_from_smithay,
        },
    },
    input::gestures::{GestureState, SwipeAction},
    shell::{
        LastModifierChange, SeatExt, Trigger,
        element::window::{begin_pointer_hover_check, finalize_pointer_hover},
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
    state::BackendData,
    utils::{prelude::*, process::workspaces_enabled, quirks::workspace_overview_is_open},
    wayland::handlers::{
        image_copy_capture::SessionHolder, xwayland_keyboard_grab::XWaylandGrabSeat,
    },
};
use calloop::{
    RegistrationToken,
    timer::{TimeoutAction, Timer},
};
use cosmic_comp_config::{NumlockState, workspace::WorkspaceLayout};
use cosmic_settings_config::shortcuts;
use cosmic_settings_config::shortcuts::action::{Direction, ResizeDirection};
use smithay::{
    backend::input::{
        AbsolutePositionEvent, Axis, AxisRelativeDirection, AxisSource, Device, DeviceCapability,
        GestureBeginEvent, GestureEndEvent, GesturePinchUpdateEvent as _,
        GestureSwipeUpdateEvent as _, InputBackend, InputEvent, KeyState, KeyboardKeyEvent,
        PointerAxisEvent, ProximityState, Switch, SwitchState, SwitchToggleEvent,
        TabletToolButtonEvent, TabletToolEvent, TabletToolProximityEvent, TabletToolTipEvent,
        TabletToolTipState, TouchEvent,
    },
    desktop::{PopupKeyboardGrab, WindowSurfaceType, utils::under_from_surface_tree},
    input::{
        Seat,
        keyboard::{FilterResult, KeysymHandle, ModifiersState},
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
        wayland_server::{
            Resource as _,
            protocol::{wl_shm::Format as ShmFormat, wl_surface::WlSurface},
        },
    },
    utils::{Logical, Point, Rectangle, SERIAL_COUNTER, Serial, Size},
    wayland::{
        compositor::CompositorHandler,
        image_copy_capture::{BufferConstraints, CursorSessionRef},
        keyboard_shortcuts_inhibit::KeyboardShortcutsInhibitorSeat,
        pointer_constraints::{PointerConstraint, with_pointer_constraint},
        seat::WaylandFocus,
        tablet_manager::{TabletDescriptor, TabletSeatTrait},
    },
};
use tracing::{error, trace, warn};
use xkbcommon::xkb::{Keycode, Keysym};

use std::{
    any::Any,
    borrow::Cow,
    cell::RefCell,
    collections::HashSet,
    ops::ControlFlow,
    time::{Duration, Instant},
};

pub mod actions;
pub mod gestures;

/// Width of the invisible edge grab zone for the `agentos-chat-panel` side panel
/// (logical px): a left press within this distance of the panel's inner edge starts a
/// resize, and two such presses within the double-click window toggle maximize/restore.
const EDGE_GRAB_PX: f64 = 8.0;

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
pub struct SupressedKeys(RefCell<Vec<(Keycode, Option<RegistrationToken>)>>);
#[derive(Default)]
pub struct SupressedButtons(RefCell<HashSet<u32>>);
#[derive(Default, Debug)]
pub struct ModifiersShortcutQueue(RefCell<Option<shortcuts::Binding>>);

impl SupressedKeys {
    fn add(&self, keysym: &KeysymHandle, token: impl Into<Option<RegistrationToken>>) {
        self.0.borrow_mut().push((keysym.raw_code(), token.into()));
    }

    fn filter(&self, keysym: &KeysymHandle) -> Option<Vec<RegistrationToken>> {
        let mut keys = self.0.borrow_mut();
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
}

impl SupressedButtons {
    fn add(&self, button: u32) {
        self.0.borrow_mut().insert(button);
    }

    fn remove(&self, button: u32) -> bool {
        self.0.borrow_mut().remove(&button)
    }
}

impl ModifiersShortcutQueue {
    pub fn set(&self, binding: shortcuts::Binding) {
        let mut set = self.0.borrow_mut();
        *set = Some(binding);
    }

    pub fn take(&self, binding: &shortcuts::Binding) -> bool {
        let mut set = self.0.borrow_mut();
        if set.is_some() && set.as_ref().unwrap() == binding {
            *set = None;
            true
        } else {
            false
        }
    }

    pub fn clear(&self) {
        let mut set = self.0.borrow_mut();
        *set = None;
    }
}

impl State {
    /// Update the side-panel edge hover state (sash indicator + EW-resize cursor)
    /// on a non-grabbed pointer move. A no-op while grabbed or when the hovered
    /// surface is unchanged; on a transition it flips the cursor and schedules a
    /// redraw so the hover sash appears/disappears. Also re-run from the animation
    /// loop after a resize so the sash reappears when the edge settles under a
    /// stationary pointer (no motion event to trigger it otherwise).
    pub(crate) fn update_edge_resize_hover(&mut self, seat: &Seat<State>) {
        use crate::backend::render::cursor::CursorState;
        use smithay::input::pointer::CursorIcon;

        let Some(pointer) = seat.get_pointer() else {
            return;
        };
        if pointer.is_grabbed() {
            return;
        }
        let global_position = pointer.current_location().as_global();
        let (new_hover, changed) = {
            let shell = self.common.shell.read();
            let new_hover = shell
                .layer_resize_target(global_position, EDGE_GRAB_PX)
                .map(|t| crate::shell::EdgeHover {
                    surface_id: t.surface_id,
                    anchor_right: t.anchor_right,
                });
            let changed = match (&shell.edge_hover, &new_hover) {
                (None, None) => false,
                (Some(a), Some(b)) => a.surface_id != b.surface_id,
                _ => true,
            };
            (new_hover, changed)
        };
        if !changed {
            return;
        }
        if let Some(cursor_state) = seat.user_data().get::<CursorState>() {
            if new_hover.is_some() {
                cursor_state.lock().unwrap().set_shape(CursorIcon::EwResize);
            } else {
                cursor_state.lock().unwrap().unset_shape();
            }
        }
        let output = seat.active_output();
        self.common.shell.write().edge_hover = new_hover;
        self.backend.schedule_render(&output);
    }

    #[profiling::function]
    pub fn process_input_event<B: InputBackend>(&mut self, event: InputEvent<B>)
    where
        <B as InputBackend>::Device: 'static,
    {
        crate::wayland::handlers::output_power::set_all_surfaces_dpms_on(self);

        use smithay::backend::input::Event;
        match event {
            InputEvent::DeviceAdded { device } => {
                let shell = self.common.shell.read();
                let seat = shell.seats.last_active();
                let led_state = seat.get_keyboard().unwrap().led_state();
                seat.devices().add_device(&device, led_state);
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
                    if devices.has_device(&device) {
                        devices.remove_device(&device);
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
                    .for_device(&event.device())
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
                                if previous_modifiers != *modifiers {
                                    *seat
                                        .user_data()
                                        .get::<LastModifierChange>()
                                        .unwrap()
                                        .0
                                        .lock()
                                        .unwrap() = Some(serial);
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

                                let result = Self::filter_keyboard_input(
                                    data, &event, &seat, modifiers, handle, serial,
                                );

                                if (matches!(result, FilterResult::Forward)
                                    && !seat.get_keyboard().unwrap().is_grabbed()
                                    && !shortcuts_inhibited
                                    && !matches!(
                                        current_focus,
                                        Some(KeyboardFocusTarget::LockSurface(_))
                                    ))
                                // we don't want to accidentally leave any keys pressed
                                // and do more filtering in `xwayland_notify_key_event`
                                // for released keys
                                    || state == KeyState::Released
                                {
                                    data.common.xwayland_notify_key_event(
                                        sym, keycode, state, serial, time,
                                    );
                                }

                                result
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
                        self.handle_action(action, &seat, serial, time, pattern, None)
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
                if let Some(seat) = shell.seats.for_device(&event.device()).cloned() {
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
                                        (ptr.current_location() - *surface_loc).to_i32_round(),
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
                    let original_position = position;
                    position += event.delta().as_global();

                    let output = shell
                        .outputs()
                        .find(|output| output.geometry().to_f64().contains(position))
                        .cloned()
                        .unwrap_or(current_output.clone());

                    let output_geometry = output.geometry();
                    position.x = position.x.clamp(
                        output_geometry.loc.x as f64,
                        (output_geometry.loc.x + output_geometry.size.w - 1) as f64,
                    );
                    position.y = position.y.clamp(
                        output_geometry.loc.y as f64,
                        (output_geometry.loc.y + output_geometry.size.h - 1) as f64,
                    );

                    // Begin hover check cycle — focus_under will mark if a window is hit.
                    // Skip hover tracking when pointer is grabbed (e.g. dragging a window),
                    // otherwise moving a dragged window over another triggers its pill.
                    let pointer_grabbed = ptr.is_grabbed();
                    if !pointer_grabbed {
                        begin_pointer_hover_check();
                    }

                    let mut new_under = State::surface_under(position, &output, &shell)
                        .map(|(target, pos)| (target, pos.as_logical()));

                    // If no window was hit during surface_under, clear the hovered state.
                    if !pointer_grabbed {
                        finalize_pointer_hover();
                    }

                    std::mem::drop(shell);

                    // Update compositor-driven auto-hide cursor tracking.
                    {
                        let cursor_surface = new_under
                            .as_ref()
                            .and_then(|(target, _)| target.wl_surface());
                        let mut shell = self.common.shell.write();
                        shell.update_auto_hide_cursor(cursor_surface.as_deref(), position);
                    }

                    // Update compositor-driven tooltip positions.
                    {
                        let cursor_surface = new_under
                            .as_ref()
                            .and_then(|(target, _)| target.wl_surface());
                        let cursor_in_parent = new_under.as_ref().map(|(_, loc)| {
                            smithay::utils::Point::<i32, smithay::utils::Logical>::from((
                                (position.x - loc.x).round() as i32,
                                (position.y - loc.y).round() as i32,
                            ))
                        });
                        self.common.tooltip_state.update_positions(
                            cursor_surface.as_deref(),
                            position,
                            cursor_in_parent,
                        );
                    }

                    ptr.relative_motion(
                        self,
                        under.clone(),
                        &RelativeMotionEvent {
                            delta: event.delta(),
                            delta_unaccel: event.delta_unaccel(),
                            utime: event.time(),
                        },
                    );

                    // Record for input-latency measurement (pointer motion is the
                    // cleanly attributable signal: it always forces a cursor redraw).
                    self.record_pointer_latency(event.time());

                    if pointer_locked {
                        ptr.frame(self);
                        return;
                    }

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
                    new_under = if pointer_confined && let Some((surface, surface_loc)) = &under {
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
                                    .contains((pos.as_logical() - *surface_loc).to_i32_round())
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
                        new_under.clone(),
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
                                        (ptr.current_location() - surface_location).to_i32_round();
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

                    for session in cursor_sessions_for_output(&shell, &output) {
                        if let Some((geometry, offset)) = seat.cursor_geometry(
                            (position - output_geometry.loc.to_f64())
                                .as_logical()
                                .to_buffer(
                                    output.current_scale().fractional_scale(),
                                    output.current_transform(),
                                    &output_geometry.size.to_f64().as_logical(),
                                ),
                            self.common.clock.now(),
                        ) {
                            if session
                                .current_constraints()
                                .map(|constraint| constraint.size != geometry.size)
                                .unwrap_or(true)
                            {
                                let mut cursor_size = geometry.size;
                                // Client shouldn't try to allocate 0x0 buffer
                                if cursor_size == Size::new(0, 0) {
                                    cursor_size = Size::new(1, 1);
                                }
                                session.update_constraints(BufferConstraints {
                                    size: cursor_size,
                                    shm: vec![ShmFormat::Argb8888],
                                    dma: None,
                                });
                            }
                            session.set_cursor_hotspot(offset);
                            session.set_cursor_pos(Some(geometry.loc));
                        }
                    }

                    drop(shell);
                    self.update_edge_resize_hover(&seat);
                }
            }
            InputEvent::PointerMotionAbsolute { event, .. } => {
                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .seats
                    .for_device(&event.device())
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    notify_cursor_activity(self, &seat);
                    let output = seat.active_output();
                    let output_geometry = output.geometry();
                    let position = output_geometry.loc.to_f64()
                        + smithay::backend::input::AbsolutePositionEvent::position_transformed(
                            &event,
                            output_geometry.size.as_logical(),
                        )
                        .as_global();
                    let serial = SERIAL_COUNTER.next_serial();
                    let shell_guard = self.common.shell.write();

                    let ptr = seat.get_pointer().unwrap();
                    let pointer_grabbed = ptr.is_grabbed();

                    // Begin hover check cycle — focus_under will mark if a window is hit.
                    // Skip when grabbed (drag) to avoid activating windows under the dragged one.
                    if !pointer_grabbed {
                        begin_pointer_hover_check();
                    }

                    let under = State::surface_under(position, &output, &shell_guard)
                        .map(|(target, pos)| (target, pos.as_logical()));

                    // If no window was hit during surface_under, clear the hovered state.
                    if !pointer_grabbed {
                        finalize_pointer_hover();
                    }

                    drop(shell_guard);

                    // Update compositor-driven auto-hide cursor tracking.
                    {
                        let cursor_surface =
                            under.as_ref().and_then(|(target, _)| target.wl_surface());
                        let mut shell = self.common.shell.write();
                        shell.update_auto_hide_cursor(cursor_surface.as_deref(), position);
                    }

                    // Update compositor-driven tooltip positions.
                    {
                        let cursor_surface =
                            under.as_ref().and_then(|(target, _)| target.wl_surface());
                        let cursor_in_parent = under.as_ref().map(|(_, loc)| {
                            smithay::utils::Point::<i32, smithay::utils::Logical>::from((
                                (position.x - loc.x).round() as i32,
                                (position.y - loc.y).round() as i32,
                            ))
                        });
                        self.common.tooltip_state.update_positions(
                            cursor_surface.as_deref(),
                            position,
                            cursor_in_parent,
                        );
                    }

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

                    // Record for input-latency measurement (see PointerMotion arm).
                    self.record_pointer_latency(smithay::backend::input::Event::time(&event));

                    let shell = self.common.shell.read();
                    for session in cursor_sessions_for_output(&shell, &output) {
                        if let Some((geometry, offset)) = seat.cursor_geometry(
                            (position - output_geometry.loc.to_f64())
                                .as_logical()
                                .to_buffer(
                                    output.current_scale().fractional_scale(),
                                    output.current_transform(),
                                    &output_geometry.size.to_f64().as_logical(),
                                ),
                            self.common.clock.now(),
                        ) {
                            if session
                                .current_constraints()
                                .map(|constraint| constraint.size != geometry.size)
                                .unwrap_or(true)
                            {
                                let mut cursor_size = geometry.size;
                                // Client shouldn't try to allocate 0x0 buffer
                                if cursor_size == Size::new(0, 0) {
                                    cursor_size = Size::new(1, 1);
                                }
                                session.update_constraints(BufferConstraints {
                                    size: cursor_size,
                                    shm: vec![ShmFormat::Argb8888],
                                    dma: None,
                                });
                            }
                            session.set_cursor_hotspot(offset);
                            session.set_cursor_pos(Some(geometry.loc));
                        }
                    }

                    drop(shell);
                    self.update_edge_resize_hover(&seat);
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
                    .for_device(&event.device())
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

                let mut pass_event = !seat.supressed_buttons().remove(button);
                if event.state() == ButtonState::Pressed {
                    // Check for layer surface dismiss notifications
                    // Get the surface that was clicked and check if any armed dismiss controllers
                    // should be notified
                    {
                        let ptr = seat.get_pointer().unwrap();
                        let pointer_location = ptr.current_location().as_global();
                        let output = seat.active_output();

                        // Get the surface under the pointer, plus whether it is a
                        // layer-shell surface (panel / dock / popover chrome).
                        let (clicked_surface_id, clicked_is_layer) = {
                            let shell = self.common.shell.read();
                            match State::surface_under(pointer_location, &output, &shell) {
                                Some((target, _)) => {
                                    let surface = WaylandFocus::wl_surface(&target);
                                    let id = surface.as_ref().map(|s| s.id());
                                    let is_layer = surface.as_ref().is_some_and(|s| {
                                        smithay::desktop::layer_map_for_output(&output)
                                            .layer_for_surface(
                                                s,
                                                smithay::desktop::WindowSurfaceType::ALL,
                                            )
                                            .is_some()
                                    });
                                    (id, is_layer)
                                }
                                None => (None, false),
                            }
                        };

                        // Check dismiss even when the click landed on empty space
                        // (`clicked_surface_id == None`): bare desktop is outside
                        // every dismiss group, so it must still close armed popovers.
                        // `clicked_is_layer` only matters to controllers that opted
                        // into `set_ignore_layer_clicks`; all others dismiss as before.
                        {
                            let controllers = self.common.dismiss_controller_registry.get_all();
                            let to_dismiss = crate::wayland::protocols::layer_surface_dismiss::check_dismiss_on_click(
                                clicked_surface_id.clone(),
                                clicked_is_layer,
                                &controllers,
                            );
                            if !to_dismiss.is_empty() {
                                tracing::debug!(
                                    ?clicked_surface_id,
                                    dismiss_count = to_dismiss.len(),
                                    "Firing dismiss for armed controllers"
                                );
                                crate::wayland::protocols::layer_surface_dismiss::fire_dismiss(
                                    to_dismiss,
                                );
                            }
                        }
                    }

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

                        // Compositor-driven edge resize for the agentos-chat-panel
                        // side panel: a left press within a few px of its inner
                        // (dragged) edge starts a resize grab instead of forwarding
                        // the click. The compositor owns the resize from here.
                        if !shortcuts_inhibited
                            && matches!(
                                PointerButtonEvent::button(&event),
                                Some(smithay::backend::input::MouseButton::Left)
                            )
                        {
                            let resize_target = {
                                let shell = self.common.shell.read();
                                shell.layer_resize_target(global_position, EDGE_GRAB_PX)
                            };
                            if let Some(start) = resize_target {
                                pass_event = false;
                                seat.supressed_buttons().add(button);

                                // A double-click on the edge toggles maximize/restore
                                // instead of starting a drag. The press is never forwarded
                                // to the client, so the double-click is timed here,
                                // compositor-side, against the previous edge press.
                                const DOUBLE_CLICK: std::time::Duration =
                                    std::time::Duration::from_millis(400);
                                // The two clicks must land within the same edge grab zone.
                                const POS_TOL: f64 = EDGE_GRAB_PX;
                                let now = std::time::Instant::now();
                                let is_double = {
                                    let shell = self.common.shell.read();
                                    shell.layer_maximize.as_ref().is_some_and(|m| {
                                        m.surface_id == start.surface_id
                                            && m.last_click.is_some_and(|(t, x)| {
                                                now.duration_since(t) <= DOUBLE_CLICK
                                                    && (global_position.x - x).abs() <= POS_TOL
                                            })
                                    })
                                };

                                if is_double {
                                    // Second click of a pair: apply the toggle now and
                                    // consume the click — no grab is started.
                                    let surface_id = start.surface_id.clone();
                                    let out = start.output.clone();
                                    let anchor_right = start.anchor_right;
                                    let current_width = start.width;
                                    {
                                        let mut shell = self.common.shell.write();
                                        shell.toggle_layer_resize_maximize(
                                            &surface_id,
                                            &out,
                                            anchor_right,
                                            current_width,
                                        );
                                        // Reset the tracker so a following click starts a
                                        // fresh single-click (drag), not another toggle.
                                        if let Some(m) = shell.layer_maximize.as_mut() {
                                            m.last_click = None;
                                        }
                                    }
                                    self.backend.schedule_render(&out);
                                } else {
                                    // First click of a potential double-click: remember it (so
                                    // the next click within the window toggles maximize/restore),
                                    // then start a ghost-only edge-drag grab. The panel does NOT
                                    // resize while dragging — only the ghost indicator follows the
                                    // pointer; on release it springs to the ghost width (see
                                    // `EdgeResizeGrab`).
                                    {
                                        let mut shell = self.common.shell.write();
                                        match shell.layer_maximize.as_mut() {
                                            Some(m) if m.surface_id == start.surface_id => {
                                                m.last_click = Some((now, global_position.x));
                                            }
                                            _ => {
                                                shell.layer_maximize =
                                                    Some(crate::shell::LayerMaximizeState {
                                                        surface_id: start.surface_id.clone(),
                                                        restore_width: start.width,
                                                        last_click: Some((now, global_position.x)),
                                                    });
                                            }
                                        }
                                    }
                                    let seat_clone = seat.clone();
                                    self.common.event_loop_handle.insert_idle(move |state| {
                                        let pointer = seat_clone.get_pointer().unwrap();
                                        let start_data = smithay::input::pointer::GrabStartData {
                                            focus: None,
                                            button: 0x110,
                                            location: pointer.current_location(),
                                        };
                                        {
                                            let mut shell = state.common.shell.write();
                                            shell.layer_resize_settle = None;
                                            // Cancel any in-flight release spring so a re-grab
                                            // mid-animation starts from the panel's current
                                            // rested width instead of resizing toward the old
                                            // target during the new drag.
                                            shell.active_layer_resize_anim = None;
                                            shell.active_layer_resize = None;
                                            // Clear hover so that after the grab ends (its Drop
                                            // unsets the cursor), the next motion sees a
                                            // None->Some transition and re-establishes the hover
                                            // sash + EW-resize cursor.
                                            shell.edge_hover = None;
                                            shell.edge_drag_ghost =
                                                Some(crate::shell::EdgeDragGhost {
                                                    surface_id: start.surface_id.clone(),
                                                    output: start.output.clone(),
                                                    anchor_right: start.anchor_right,
                                                    width: start.width,
                                                });
                                        }
                                        let grab = crate::shell::grabs::EdgeResizeGrab::new(
                                            start_data,
                                            &seat_clone,
                                            start,
                                        );
                                        pointer.set_grab(
                                            state,
                                            grab,
                                            serial,
                                            smithay::input::pointer::Focus::Clear,
                                        );
                                    });
                                }
                            }
                        }

                        if let Some(target) = under {
                            if let Some(surface) = target.toplevel().map(Cow::into_owned)
                                && seat.get_keyboard().unwrap().modifier_state().logo
                                && !shortcuts_inhibited
                            {
                                let seat_clone = seat.clone();
                                let mouse_button = PointerButtonEvent::button(&event);

                                let mut supress_button = || {
                                    // If the logo is held then the pointer event is
                                    // aimed at the compositor and shouldn't be passed
                                    // to the application.
                                    pass_event = false;
                                    seat.supressed_buttons().add(button);
                                };

                                fn dispatch_grab<G: PointerGrab<State> + 'static>(
                                    grab: Option<(G, smithay::input::pointer::Focus)>,
                                    seat: Seat<State>,
                                    serial: Serial,
                                    state: &mut State,
                                ) {
                                    if let Some((target, focus)) = grab {
                                        seat.modifiers_shortcut_queue().clear();

                                        seat.get_pointer()
                                            .unwrap()
                                            .set_grab(state, target, serial, focus);
                                    }
                                }

                                if let Some(mouse_button) = mouse_button {
                                    match mouse_button {
                                        smithay::backend::input::MouseButton::Left => {
                                            supress_button();
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
                                                    dispatch_grab(res, seat_clone, serial, state);
                                                },
                                            );
                                        }
                                        smithay::backend::input::MouseButton::Right => {
                                            supress_button();
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
                                                    dispatch_grab(res, seat_clone, serial, state);
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
                    .for_device(&event.device())
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    notify_cursor_activity(self, &seat);

                    if seat.get_keyboard().unwrap().modifier_state().logo
                        && self
                            .common
                            .config
                            .cosmic_conf
                            .accessibility_zoom
                            .enable_mouse_zoom_shortcuts
                    {
                        seat.modifiers_shortcut_queue().clear();
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
                        if let Some(horizontal_amount) = event.amount(Axis::Horizontal) {
                            if horizontal_amount != 0.0 {
                                frame = frame
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
                        if let Some(vertical_amount) = event.amount(Axis::Vertical) {
                            if vertical_amount != 0.0 {
                                frame =
                                    frame.value(Axis::Vertical, scroll_factor * vertical_amount);
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
                    .for_device(&event.device())
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    // With workspaces disabled the multi-finger swipe has nothing to switch
                    // to, so don't claim the gesture - hand it to the client instead. This
                    // also disables the update/end arms, which both key off `gesture_state`.
                    if event.fingers() >= 3
                        && workspaces_enabled()
                        && !workspace_overview_is_open(&seat.active_output())
                    {
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
                    .for_device(&event.device())
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
                    .for_device(&event.device())
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
                    .for_device(&event.device())
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
                    .for_device(&event.device())
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
                    .for_device(&event.device())
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
                    .for_device(&event.device())
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
                    .for_device(&event.device())
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
                if let Some(seat) = shell.seats.for_device(&event.device()).cloned() {
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

                    // Determine which window/surface should receive keyboard focus.
                    // `surface_under`/`touch.down` only route touch events to the
                    // surface under the finger; they never change the keyboard focus
                    // or raise/activate the tapped window. Without this, tapping a
                    // window behind the currently focused one does nothing (HUM-165).
                    // Mirror the pointer-button path, which calls `Shell::set_focus`.
                    let keyboard_focus_target =
                        State::element_under(position, &output, &shell, &seat);

                    // Check for layer surface dismiss on touch down (with whether the
                    // touch landed on layer-shell chrome, for opted-in controllers).
                    let clicked_surface_id = under
                        .as_ref()
                        .and_then(|(target, _)| WaylandFocus::wl_surface(target).map(|s| s.id()));
                    let clicked_is_layer = under.as_ref().is_some_and(|(target, _)| {
                        WaylandFocus::wl_surface(target).is_some_and(|s| {
                            smithay::desktop::layer_map_for_output(&output)
                                .layer_for_surface(&s, smithay::desktop::WindowSurfaceType::ALL)
                                .is_some()
                        })
                    });

                    std::mem::drop(shell);

                    // Fire dismiss if touch is outside armed surfaces — including a
                    // touch on empty space (`None`), which is outside every group.
                    {
                        let controllers = self.common.dismiss_controller_registry.get_all();
                        let to_dismiss = crate::wayland::protocols::layer_surface_dismiss::check_dismiss_on_click(
                            clicked_surface_id.clone(),
                            clicked_is_layer,
                            &controllers,
                        );
                        if !to_dismiss.is_empty() {
                            tracing::debug!(
                                ?clicked_surface_id,
                                dismiss_count = to_dismiss.len(),
                                "Firing dismiss for armed controllers (touch)"
                            );
                            crate::wayland::protocols::layer_surface_dismiss::fire_dismiss(
                                to_dismiss,
                            );
                        }
                    }

                    let serial = SERIAL_COUNTER.next_serial();

                    // Hide the mouse cursor while interacting via touch (as Windows
                    // does). It stays hidden until the next real pointer/mouse activity,
                    // which reveals it again via `notify_cursor_activity`.
                    hide_cursor(self, &seat);

                    let touch = seat.get_touch().unwrap();

                    // Change keyboard focus to the tapped window, unless touch is
                    // already grabbed (e.g. an in-progress client touch gesture),
                    // in which case focus must stay with the grab owner.
                    if !touch.is_grabbed()
                        && let Some(target) = keyboard_focus_target
                    {
                        Shell::set_focus(self, Some(&target), &seat, Some(serial), false);
                    }

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
                if let Some(seat) = shell.seats.for_device(&event.device()).cloned() {
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

                let maybe_seat = shell.seats.for_device(&event.device()).cloned();
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
                    .for_device(&event.device())
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
                    .for_device(&event.device())
                    .cloned();
                if let Some(seat) = maybe_seat {
                    self.common.idle_notifier_state.notify_activity(&seat);
                    let touch = seat.get_touch().unwrap();
                    touch.frame(self);
                }
            }

            InputEvent::TabletToolAxis { event, .. } => {
                let shell = self.common.shell.write();
                if let Some(seat) = shell.seats.for_device(&event.device()).cloned() {
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
                if let Some(seat) = shell.seats.for_device(&event.device()).cloned() {
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
                    .for_device(&event.device())
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
                    .for_device(&event.device())
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
                            warn!(?err, "Failed to re-enable internal connector");
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

    /// Determine is key event should be intercepted as a key binding, or forwarded to surface
    #[profiling::function]
    pub fn filter_keyboard_input<B: InputBackend, E: KeyboardKeyEvent<B>>(
        &mut self,
        event: &E,
        seat: &Seat<Self>,
        modifiers: &ModifiersState,
        handle: KeysymHandle<'_>,
        serial: Serial,
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
            a11y_keyboard_monitor.key_event(modifiers, &handle, event.state());
        }

        // Handle voice mode key bindings from configuration
        // This enables:
        // - Tap (short press < 300ms): Focus chat input field
        // - Hold (press and hold): Push-to-talk voice mode
        let voice_config = &self.common.config.voice_config;
        let keysym = handle.modified_sym();
        tracing::debug!(
            keysym_raw = keysym.raw(),
            keysym_name = ?keysym,
            key_code = u32::from(event.key_code()),
            key_state = ?event.state(),
            voice_enabled = voice_config.enabled,
            "Voice key check - incoming key event"
        );
        // In game mode, on the game's own output, the Super key is the LAUNCHER
        // key: forward its raw press AND release to the launcher over the
        // one.playtron.GameMode D-Bus interface (both edges, so the launcher can
        // distinguish tap vs hold or change behavior later) and consume it, so it
        // neither opens the start menu nor reaches the game. The controller GUIDE
        // button reaches Grid directly via InputPlumber; only Super comes here.
        if shell.game_mode.active
            && shell.game_mode.output.as_ref() == Some(&focused_output)
            && matches!(keysym, Keysym::Super_L | Keysym::Super_R)
        {
            let pressed = event.state() == KeyState::Pressed;
            drop(shell);
            self.common.game_mode_bridge.notify_launcher_key(pressed);
            return FilterResult::Intercept(None);
        }

        let matches = voice_config.matches_binding(keysym, modifiers);

        // Hard-coded F18 check: always treat F18 as voice key regardless of config
        let is_f18 = keysym == Keysym::F18;

        // For key release, also check if the key matches without modifiers (since modifiers
        // may have been released before the main key). Check if voice mode is active.
        let voice_mode_active = shell.is_voice_mode_active();
        let key_matches_without_mods = voice_config.matches_key_only(keysym) || is_f18;
        let is_voice_key_release = event.state() == KeyState::Released && key_matches_without_mods;

        tracing::trace!(
            matches,
            is_f18,
            key_matches_without_mods,
            is_voice_key_release,
            voice_mode_active,
            "Voice key check - binding match results"
        );

        // Don't let the voice-mode Super gesture fire while the perf-capture
        // chord modifiers (Ctrl+Alt+Shift, alongside Super) are held — pressing
        // Ctrl+Alt+Super+Shift+F1x must not also trigger voice. (The activation
        // hold-timer below has the same guard, to cover any key-press order.)
        let perf_chord_mods = modifiers.ctrl && modifiers.alt && modifiers.shift;
        // Skip voice/chat handling while a keyboard-shortcuts inhibitor is active,
        // so the Super press is forwarded to the focused client instead. Mirrors the
        // `!shortcuts_inhibited` gate on global compositor shortcuts below.
        if voice_config.enabled
            && !perf_chord_mods
            && !shortcuts_inhibited
            && (matches || is_f18 || is_voice_key_release)
        {
            // Block voice key handling entirely during session lock (idle/login screen)
            if shell.session_lock.is_some() {
                tracing::debug!("Voice key ignored - session is locked");
                return FilterResult::Intercept(None);
            }

            tracing::debug!("Voice key binding matched! Processing...");
            // When the orb is frozen (thinking) or transitioning, ignore the guide key entirely.
            // Only Escape can dismiss the orb during these states.
            if self
                .common
                .voice_mode_state
                .orb_state()
                .is_non_interruptible()
            {
                tracing::debug!(
                    "Voice key ignored - orb is in non-interruptible state (use Escape to dismiss)"
                );
                return FilterResult::Intercept(None);
            }

            // Check if voice mode was activated on demand (button click).
            // When on-demand is active, any voice key press/release simply deactivates
            // voice mode without triggering focus_input or tap detection.
            let is_on_demand = self.common.voice_mode_state.is_activated_on_demand();

            if is_on_demand && voice_mode_active {
                std::mem::drop(shell); // Release shell lock before protocol calls

                if event.state() == KeyState::Pressed {
                    // Suppress the key press so we also get the release
                    tracing::debug!(
                        "Voice key pressed while on-demand voice active - suppressing for deactivation on release"
                    );
                    seat.supressed_keys().add(&handle, None);
                } else if event.state() == KeyState::Released {
                    // Deactivate voice mode - no focus_input, no tap detection
                    tracing::debug!(
                        "Voice key released while on-demand voice active - deactivating voice mode"
                    );
                    use crate::wayland::protocols::voice_mode::VoiceModeHandler;
                    self.common.voice_mode_state.clear_key_press();
                    self.deactivate_voice_mode();
                }
                return FilterResult::Intercept(None);
            }

            std::mem::drop(shell); // Release shell lock before protocol calls

            if event.state() == KeyState::Pressed {
                // Record key press time for tap vs hold detection
                // Do NOT activate voice mode immediately - wait to see if it's a tap or hold
                self.common.voice_mode_state.record_key_press();
                tracing::debug!("Voice key pressed - recording time for tap detection");

                // Start a timer to activate voice mode after TAP_THRESHOLD_MS
                // If the key is released before the timer fires, it's a tap
                use crate::wayland::protocols::voice_mode::TAP_THRESHOLD_MS;
                let delay =
                    Timer::from_duration(std::time::Duration::from_millis(TAP_THRESHOLD_MS as u64));
                let seat_name = seat.name().to_string();

                let _ = self
                    .common
                    .event_loop_handle
                    .insert_source(delay, move |_, _, state| {
                        // If the perf-capture chord modifiers (Ctrl+Alt+Shift) are
                        // held when the hold-timer fires, this is the F1x chord,
                        // not a voice gesture — don't activate.
                        let chord_held = state
                            .common
                            .shell
                            .read()
                            .seats
                            .iter()
                            .find(|s| s.name() == seat_name)
                            .and_then(|s| s.get_keyboard())
                            .map(|kb| kb.modifier_state())
                            .map(|m| m.ctrl && m.alt && m.shift)
                            .unwrap_or(false);
                        if chord_held {
                            return TimeoutAction::Drop;
                        }
                        // Check if key is still being held (press time still recorded)
                        if state.common.voice_mode_state.should_activate_voice() {
                            tracing::debug!(
                                "Voice key held past threshold - activating voice mode"
                            );

                            // Re-fetch the current focused surface at activation time
                            let focused_surface: Option<WlSurface> = state
                                .common
                                .shell
                                .read()
                                .seats
                                .iter()
                                .find(|s| s.name() == seat_name)
                                .and_then(|seat| seat.get_keyboard())
                                .and_then(|kb| kb.current_focus())
                                .and_then(|f| f.wl_surface().map(|cow| cow.into_owned()));

                            use crate::wayland::protocols::voice_mode::VoiceModeHandler;
                            let orb_state = state.activate_voice_mode(focused_surface.as_ref());
                            if orb_state != crate::wayland::protocols::voice_mode::OrbState::Hidden
                            {
                                state.common.voice_mode_state.mark_voice_activated();
                                tracing::debug!(
                                    ?orb_state,
                                    "Voice mode activated via timer (hold)"
                                );
                            }
                        }
                        TimeoutAction::Drop
                    });

                // Suppress the key so release is also handled
                seat.supressed_keys().add(&handle, None);
                return FilterResult::Intercept(None);
            } else if event.state() == KeyState::Released {
                // Check if this was a tap (short press < 300ms) vs hold
                let was_tap = self.common.voice_mode_state.check_tap_without_clearing();

                if was_tap {
                    // Tap detected: send focus_input event to focused surface or default receiver
                    // Don't activate voice mode at all
                    tracing::debug!(
                        "Voice key tap detected - sending focus_input (not activating voice)"
                    );
                    self.common.voice_mode_state.clear_key_press();

                    // Get the currently focused surface to send focus_input to it
                    // If it has a receiver, it gets the event; otherwise fall back to last focused or default
                    let focused_surface: Option<WlSurface> = current_focus
                        .as_ref()
                        .and_then(|f| f.wl_surface().map(|cow| cow.into_owned()));

                    // send_focus_input_to_surface_or_default returns the surface that received the event
                    if let Some(surface_to_focus) = self
                        .common
                        .voice_mode_state
                        .send_focus_input_to_surface_or_default(focused_surface.as_ref())
                    {
                        // Check if this is a layer surface
                        // Layer surfaces with OnDemand interactivity need explicit keyboard focus
                        let shell = self.common.shell.read();
                        if let Some(layer_surface) =
                            shell.find_layer_surface_by_wl_surface(&surface_to_focus)
                        {
                            drop(shell);
                            // Decide how to handle the tap:
                            // - If HOME_ENABLED: enter full home mode (minimizes windows + triggers HideOnHome animations)
                            // - If no voice windows: just minimize windows
                            // - Otherwise: just set focus without minimizing
                            let has_voice_windows =
                                self.common.voice_mode_state.has_non_default_receivers();
                            if crate::shell::home_enabled() {
                                tracing::info!(
                                    "Entering home mode and setting keyboard focus to default receiver layer surface"
                                );
                                self.common.shell.write().enter_home();
                            } else if !has_voice_windows {
                                tracing::info!(
                                    "Minimizing windows (no voice windows open) and setting keyboard focus to layer surface"
                                );
                                self.common.shell.write().minimize_all_windows_only();
                            } else {
                                tracing::info!(
                                    "Setting keyboard focus to layer surface (home mode disabled, voice windows exist)"
                                );
                            }
                            let focus_target = KeyboardFocusTarget::from(layer_surface);
                            Shell::set_focus(self, Some(&focus_target), seat, None, false);
                        } else {
                            drop(shell);
                            // Regular window - activate it (unminimize, switch workspace, raise, focus)
                            use crate::wayland::handlers::xdg_activation::ActivationContext;
                            let output = seat.active_output();
                            let workspace_handle = {
                                let shell = self.common.shell.read();
                                shell.active_space(&output).unwrap().handle
                            };
                            self.activate_surface(
                                &surface_to_focus,
                                Some((
                                    crate::shell::ActivationKey::Wayland(surface_to_focus.clone()),
                                    ActivationContext::Workspace(workspace_handle),
                                )),
                            );
                            tracing::debug!("Activated window for voice focus_input");
                        }
                    }
                } else if voice_mode_active {
                    // Voice mode is active (was held long enough) - now deactivate
                    use crate::wayland::protocols::voice_mode::VoiceModeHandler;
                    self.common.voice_mode_state.clear_key_press();
                    self.deactivate_voice_mode();
                    tracing::debug!("Voice mode deactivated via hotkey (hold release)");
                } else {
                    // Hold detected but voice mode wasn't activated yet
                    // This can happen if timer hasn't fired yet - just clear state
                    tracing::debug!("Voice key released before timer fired");
                    self.common.voice_mode_state.clear_key_press();
                }

                // Key was suppressed on press, intercept release too
                return FilterResult::Intercept(None);
            }
            // Re-acquire shell lock for subsequent code
            shell = self.common.shell.write();
        }

        // Forward Right Super key (KEY_RIGHTMETA, keycode 134) to specific apps
        // Apps can be configured via COSMIC_VOICE_INPUT_APP_IDS env var (comma-separated)
        // This allows apps like chat-ui to use it as a voice input trigger
        const KEY_RIGHTMETA: u32 = 134;
        if event.key_code() == KEY_RIGHTMETA.into() {
            let allowed_app_ids: Vec<&str> = std::env::var("COSMIC_VOICE_INPUT_APP_IDS")
                .map(|s| s.leak() as &str)
                .unwrap_or("chat-ui")
                .split(',')
                .map(|s| s.trim())
                .collect();

            let current_app_id = match &current_focus {
                Some(KeyboardFocusTarget::Element(elem)) => Some(elem.active_window().app_id()),
                Some(KeyboardFocusTarget::Fullscreen(surface)) => Some(surface.app_id()),
                _ => None,
            };

            if let Some(app_id) = current_app_id
                && allowed_app_ids.iter().any(|allowed| *allowed == app_id)
            {
                return FilterResult::Forward;
            }
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
                    && event.state() == KeyState::Released))
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
                && event.state() == KeyState::Released
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
                    cosmic_keystate_from_smithay(event.state()),
                ));
                let key_pattern = shortcuts::Binding {
                    modifiers: cosmic_modifiers_from_smithay(*modifiers),
                    keycode: None,
                    key: Some(handle.modified_sym()),
                    description: None,
                };

                if event.state() == KeyState::Released {
                    if let Some(tokens) = seat.supressed_keys().filter(&handle) {
                        for token in tokens {
                            self.common.event_loop_handle.remove(token);
                        }
                    }
                } else {
                    let seat_clone = seat.clone();
                    let action_clone = action.clone();
                    let key_pattern_clone = key_pattern.clone();
                    let start = Instant::now();
                    let time = event.time_msec();
                    let token = self
                        .common
                        .event_loop_handle
                        .insert_source(
                            Timer::from_duration(Duration::from_millis(200)),
                            move |current, _, state| {
                                let duration = current.duration_since(start).as_millis();
                                state.handle_action(
                                    action_clone.clone(),
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

                    seat.supressed_keys().add(&handle, token);
                }
                return FilterResult::Intercept(Some((action, key_pattern)));
            }
        }

        std::mem::drop(shell);

        // Escape dismisses the orb when frozen (thinking mode)
        if handle.modified_sym() == Keysym::Escape
            && event.state() == KeyState::Pressed
            && self
                .common
                .voice_mode_state
                .orb_state()
                .is_non_interruptible()
        {
            tracing::debug!(
                "Escape pressed during frozen/transitioning orb - cancelling voice mode"
            );
            use crate::wayland::protocols::voice_mode::VoiceModeHandler;
            self.cancel_voice_mode();
            seat.supressed_keys().add(&handle, None);
            return FilterResult::Intercept(None);
        }

        // cancel grabs
        if is_grabbed
            && handle.modified_sym() == Keysym::Escape
            && event.state() == KeyState::Pressed
            && !modifiers.alt
            && !modifiers.ctrl
            && !modifiers.logo
            && !modifiers.shift
        {
            seat.supressed_keys().add(&handle, None);
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
            if event.state() == KeyState::Released {
                let removed =
                    a11y_keyboard_monitor.remove_active_virtual_mod(handle.modified_sym());
                // If `Caps_Lock` is a virtual modifier, and is in locked state, clear it
                if removed
                    && handle.modified_sym() == Keysym::Caps_Lock
                    && (modifiers.serialized.locked & 2) != 0
                {
                    let seat = seat.clone();
                    let key_code = event.key_code();
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
            } else if event.state() == KeyState::Pressed
                && a11y_keyboard_monitor.has_virtual_mod(handle.modified_sym())
            {
                a11y_keyboard_monitor.add_active_virtual_mod(handle.modified_sym());

                tracing::debug!(
                    "active virtual mods: {:?}",
                    a11y_keyboard_monitor.active_virtual_mods()
                );
                seat.supressed_keys().add(&handle, None);

                return FilterResult::Intercept(None);
            }
        }

        // Skip released events for initially surpressed keys
        if event.state() == KeyState::Released
            && let Some(tokens) = seat.supressed_keys().filter(&handle)
        {
            for token in tokens {
                self.common.event_loop_handle.remove(token);
            }
            return FilterResult::Intercept(None);
        }

        // Performance-measurement hotkeys. Checked BEFORE the VT-switch handler
        // below, because Ctrl+Alt+F-keys also produce XF86_Switch_VT_* keysyms —
        // so the full chord additionally requires Shift and is consumed here so a
        // TTY switch can't fire:
        //   Ctrl+Alt+Super+Shift+F12 — live UI performance report
        //   Ctrl+Alt+Super+Shift+F11 — cold-start (app-launch) benchmark
        if event.state() == KeyState::Pressed
            && modifiers.ctrl
            && modifiers.alt
            && modifiers.logo
            && modifiers.shift
        {
            if handle.raw_syms().contains(&Keysym::F12) {
                seat.supressed_keys().add(&handle, None);
                return FilterResult::Intercept(Some((
                    Action::Private(PrivateAction::PerfReport),
                    shortcuts::Binding::default(),
                )));
            }
            if handle.raw_syms().contains(&Keysym::F11) {
                seat.supressed_keys().add(&handle, None);
                return FilterResult::Intercept(Some((
                    Action::Private(PrivateAction::ColdStartBench),
                    shortcuts::Binding::default(),
                )));
            }
        }

        // Handle VT switches (KMS backend only)
        if event.state() == KeyState::Pressed
            && (Keysym::XF86_Switch_VT_1.raw()..=Keysym::XF86_Switch_VT_12.raw())
                .contains(&handle.modified_sym().raw())
            && let BackendData::Kms(kms) = &mut self.backend
        {
            if let Err(err) = kms.switch_vt(
                (handle.modified_sym().raw() - Keysym::XF86_Switch_VT_1.raw() + 1) as i32,
            ) {
                error!(?err, "Failed switching virtual terminal.");
            }
            seat.supressed_keys().add(&handle, None);
            return FilterResult::Intercept(None);
        }

        // Intercept window-control keys (F11, Alt+F4) for embedded windows and forward to parent
        if event.state() == KeyState::Pressed {
            let is_f11 = handle.modified_sym() == Keysym::F11;
            let is_alt_f4 = handle.modified_sym() == Keysym::F4 && modifiers.alt;

            if is_f11 || is_alt_f4 {
                // Get current focus from keyboard again (since current_focus may have been moved)
                if let Some(focus) = keyboard.current_focus() {
                    // Check if focused window is embedded
                    if let Some(surface) = focus.wl_surface() {
                        let surface_id = surface.id().to_string();
                        // Get parent info from embed map
                        let parent_info = crate::wayland::handlers::surface_embed::EMBEDDED_APP_IDS
                            .read()
                            .ok()
                            .and_then(|map| {
                                map.get(&surface_id)
                                    .map(|info| info.parent_surface_id.clone())
                            });

                        if let Some(parent_surface_id) = parent_info {
                            // Find parent element (with shell lock released before set_focus)
                            let parent_target = {
                                let shell = self.common.shell.read();
                                shell.element_for_surface_id(&parent_surface_id).cloned()
                            };

                            if let Some(parent) = parent_target {
                                tracing::debug!(
                                    key = ?handle.modified_sym(),
                                    embedded_surface = %surface_id,
                                    parent_surface = %parent_surface_id,
                                    "Forwarding window-control key from embedded to parent"
                                );
                                // Send key to parent surface
                                keyboard.set_focus(
                                    self,
                                    Some(KeyboardFocusTarget::from(parent)),
                                    serial,
                                );
                                // Don't intercept - let it flow through to the now-focused parent
                                return FilterResult::Forward;
                            }
                        }
                    }
                }
            }
        }

        if event.state() == KeyState::Pressed
            && self
                .common
                .dbus_state
                .a11y_keyboard_monitor()
                .is_some_and(|monitor| {
                    monitor.has_keyboard_grab()
                        || monitor.has_key_grab(modifiers, handle.modified_sym())
                })
        {
            let modifiers_queue = seat.modifiers_shortcut_queue();
            modifiers_queue.clear();
            seat.supressed_keys().add(&handle, None);
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
                    && event.state() == KeyState::Released
                    && !cosmic_modifiers_eq_smithay(&binding.modifiers, modifiers)
                    && modifiers_queue.take(binding)
                {
                    modifiers_queue.clear();
                    return FilterResult::Intercept(Some((
                        Action::Shortcut(action.clone()),
                        binding.clone(),
                    )));
                }

                // could this potentially become a modifier-only binding?
                if binding.key.is_none()
                    && event.state() == KeyState::Pressed
                    && cosmic_modifiers_eq_smithay(&binding.modifiers, modifiers)
                {
                    modifiers_queue.set(binding.clone());
                    clear_queue = false;
                }

                // is this a normal binding?
                if binding.key.is_some()
                    && event.state() == KeyState::Pressed
                    && key_matches(binding.key.unwrap())
                    && cosmic_modifiers_eq_smithay(&binding.modifiers, modifiers)
                {
                    modifiers_queue.clear();
                    seat.supressed_keys().add(&handle, None);
                    return FilterResult::Intercept(Some((
                        Action::Shortcut(action.clone()),
                        binding.clone(),
                    )));
                }
            }
        }

        // no binding
        if clear_queue {
            seat.modifiers_shortcut_queue().clear();
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
            &element_filter,
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
                        // Apply auto-hide offset so hidden surfaces
                        // don't intercept keyboard focus.
                        let layer_geo = layer.bbox();
                        let surface_id = layer.wl_surface().id();
                        let (ox, oy) =
                            shell.get_auto_hide_offset(layer.wl_surface(), layer_geo.size.h);
                        let (sx, sy) = shell.get_layer_slide_offset(&surface_id);
                        // Popover open-animation translate (slide-up), so hit-testing
                        // tracks the surface as it rises into place.
                        let (opx, opy) = shell.get_layer_open_offset(&surface_id);
                        let total_ox = ox + sx + opx;
                        let total_oy = oy + sy + opy;
                        let input_location = if total_ox != 0 || total_oy != 0 {
                            location + smithay::utils::Point::from((total_ox, total_oy))
                        } else {
                            location
                        };
                        if under_from_surface_tree(
                            layer.wl_surface(),
                            global_pos.as_logical(),
                            input_location.as_logical(),
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
                    Stage::OverlaySurface { .. } => {
                        // Keyboard input to a blocking game-mode overlay is routed
                        // by the input grab (SetOverlay blocking); pointer falls
                        // through to the game.
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
                                geometry.contains(global_pos.to_local(output).to_i32_round())
                            })
                            && let Some(element) = workspace.popup_element_under(location, seat)
                        {
                            return ControlFlow::Break(Ok(Some(element)));
                        }
                    }
                    Stage::Workspace {
                        workspace,
                        offset,
                        alpha: _,
                        game_mode_only,
                    } => {
                        let location = global_pos + offset.as_global().to_f64();
                        let output = workspace.output();
                        let output_geo = output.geometry().to_local(output);
                        if Rectangle::new(offset.as_local(), output_geo.size)
                            .intersection(output_geo)
                            .is_some_and(|geometry| {
                                geometry.contains(global_pos.to_local(output).to_i32_round())
                            })
                        {
                            // Input must match what game mode actually RENDERS: only
                            // the controlled surface is drawn, so only it may be hit.
                            let element = match game_mode_only {
                                Some(controlled) => {
                                    workspace.controlled_element_under(location, controlled)
                                }
                                None => workspace.toplevel_element_under(location, seat),
                            };
                            if let Some(element) = element {
                                return ControlFlow::Break(Ok(Some(element)));
                            }
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
            &element_filter,
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
                        // Apply auto-hide offset so hidden/animating
                        // surfaces don't intercept pointer input at
                        // their original position.
                        let layer_geo = layer.bbox();
                        let surface_id = layer.wl_surface().id();
                        let (ox, oy) =
                            shell.get_auto_hide_offset(layer.wl_surface(), layer_geo.size.h);
                        let (sx, sy) = shell.get_layer_slide_offset(&surface_id);
                        // Popover open-animation translate (slide-up), so pointer
                        // hit-testing tracks the surface as it rises into place.
                        let (opx, opy) = shell.get_layer_open_offset(&surface_id);
                        let total_ox = ox + sx + opx;
                        let total_oy = oy + sy + opy;
                        let input_location = if total_ox != 0 || total_oy != 0 {
                            location + smithay::utils::Point::from((total_ox, total_oy))
                        } else {
                            location
                        };

                        let surface = layer.wl_surface();
                        if let Some((surface, surface_loc)) = under_from_surface_tree(
                            surface,
                            global_pos.as_logical(),
                            input_location.as_logical(),
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
                    Stage::OverlaySurface { .. } => {
                        // Keyboard input to a blocking game-mode overlay is routed
                        // by the input grab; pointer falls through to the game.
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
                    Stage::Workspace {
                        workspace,
                        offset,
                        alpha: _,
                        game_mode_only,
                    } => {
                        let global_pos = global_pos + offset.to_f64().as_global();
                        // Pointer must match what game mode actually RENDERS (see the
                        // element_under arm above).
                        let under = match game_mode_only {
                            Some(controlled) => {
                                workspace.controlled_surface_under(global_pos, controlled)
                            }
                            None => {
                                workspace.toplevel_surface_under(global_pos, overview.clone(), seat)
                            }
                        };
                        if let Some(under) = under {
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
                            let point_in_surface = (p - surface_offset.to_f64()).to_i32_round();
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
                    Some((Point::new(x, y), output.clone()))
                } else {
                    None
                }
            } else {
                None
            }
        };

        if let Some((point, output)) = point_and_output {
            let original_position = pointer.current_location();
            pointer.set_location(point);

            let mut shell = self.common.shell.write();
            shell.update_pointer_position(point.as_global().to_local(&output), &output);

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

                let output_geometry = output.geometry();
                for session in cursor_sessions_for_output(&shell, &output) {
                    if let Some((geometry, offset)) = seat.cursor_geometry(
                        point.to_buffer(
                            output.current_scale().fractional_scale(),
                            output.current_transform(),
                            &output_geometry.size.to_f64().as_logical(),
                        ),
                        self.common.clock.now(),
                    ) {
                        if session
                            .current_constraints()
                            .map(|constraint| constraint.size != geometry.size)
                            .unwrap_or(true)
                        {
                            session.update_constraints(BufferConstraints {
                                size: geometry.size,
                                shm: vec![ShmFormat::Argb8888],
                                dma: None,
                            });
                        }
                        session.set_cursor_hotspot(offset);
                        session.set_cursor_pos(Some(geometry.loc));
                    }
                }
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
