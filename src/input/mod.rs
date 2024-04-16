// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    config::{Action, Config, KeyModifiers, KeyPattern},
    input::gestures::{GestureState, SwipeAction},
    shell::{
        focus::{
            target::{KeyboardFocusTarget, PointerFocusTarget},
            FocusDirection,
        },
        grabs::{ReleaseMode, ResizeEdge},
        layout::{
            floating::ResizeGrabMarker,
            tiling::{SwapWindowGrab, TilingLayout},
        },
        Direction, FocusResult, InvalidWorkspaceIndex, MoveResult, OverviewMode, ResizeDirection,
        ResizeMode, SeatExt, Trigger, WorkspaceDelta,
    },
    utils::prelude::*,
    wayland::{
        handlers::{screencopy::SessionHolder, xdg_activation::ActivationContext},
        protocols::{
            screencopy::{BufferConstraints, CursorSession},
            workspace::WorkspaceUpdateGuard,
        },
    },
};
use calloop::{timer::Timer, RegistrationToken};
use cosmic_comp_config::{workspace::WorkspaceLayout, TileBehavior};
use cosmic_config::ConfigSet;
use smithay::{
    backend::input::{
        AbsolutePositionEvent, Axis, AxisSource, Device, DeviceCapability, GestureBeginEvent,
        GestureEndEvent, GesturePinchUpdateEvent as _, GestureSwipeUpdateEvent as _, InputBackend,
        InputEvent, KeyState, PointerAxisEvent, ProximityState, TabletToolButtonEvent,
        TabletToolEvent, TabletToolProximityEvent, TabletToolTipEvent, TabletToolTipState,
        TouchEvent,
    },
    desktop::{
        layer_map_for_output, space::SpaceElement, utils::under_from_surface_tree,
        WindowSurfaceType,
    },
    input::{
        keyboard::{FilterResult, KeysymHandle},
        pointer::{
            AxisFrame, ButtonEvent, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent, MotionEvent,
            RelativeMotionEvent,
        },
        touch::{DownEvent, MotionEvent as TouchMotionEvent, UpEvent},
        Seat,
    },
    output::Output,
    reexports::{
        input::Device as InputDevice, wayland_server::protocol::wl_shm::Format as ShmFormat,
    },
    utils::{Point, Serial, SERIAL_COUNTER},
    wayland::{
        keyboard_shortcuts_inhibit::KeyboardShortcutsInhibitorSeat,
        pointer_constraints::{with_pointer_constraint, PointerConstraint},
        seat::WaylandFocus,
        shell::wlr_layer::Layer as WlrLayer,
        tablet_manager::{TabletDescriptor, TabletSeatTrait},
    },
    xwayland::X11Surface,
};
#[cfg(not(feature = "debug"))]
use tracing::info;
use tracing::{error, trace};
use xkbcommon::xkb::{Keycode, Keysym};

use std::{
    any::Any,
    cell::RefCell,
    os::unix::process::CommandExt,
    thread,
    time::{Duration, Instant},
};

pub mod gestures;

#[derive(Default)]
pub struct SupressedKeys(RefCell<Vec<(Keycode, Option<RegistrationToken>)>>);
#[derive(Default, Debug)]
pub struct ModifiersShortcutQueue(RefCell<Option<KeyPattern>>);

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

        let removed = removed
            .into_iter()
            .map(|(_, token)| token)
            .flatten()
            .collect::<Vec<_>>();
        if removed.is_empty() {
            None
        } else {
            Some(removed)
        }
    }
}

impl ModifiersShortcutQueue {
    pub fn set(&self, binding: KeyPattern) {
        let mut set = self.0.borrow_mut();
        *set = Some(binding);
    }

    pub fn take(&self, binding: &KeyPattern) -> bool {
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
    pub fn process_input_event<B: InputBackend>(
        &mut self,
        event: InputEvent<B>,
        needs_key_repetition: bool,
    ) where
        <B as InputBackend>::Device: 'static,
    {
        use smithay::backend::input::Event;
        match event {
            InputEvent::DeviceAdded { device } => {
                let shell = self.common.shell.read().unwrap();
                let seat = shell.seats.last_active();
                for cap in seat.devices().add_device(&device) {
                    match cap {
                        DeviceCapability::TabletTool => {
                            seat.tablet_seat().add_tablet::<Self>(
                                &self.common.display_handle,
                                &TabletDescriptor::from(&device),
                            );
                        }
                        // TODO: Handle touch
                        _ => {}
                    }
                }
                #[cfg(feature = "debug")]
                {
                    self.common.egui.state.handle_device_added(&device);
                }
            }
            InputEvent::DeviceRemoved { device } => {
                for seat in &mut self.common.shell.read().unwrap().seats.iter() {
                    let devices = seat.devices();
                    if devices.has_device(&device) {
                        for cap in devices.remove_device(&device) {
                            match cap {
                                DeviceCapability::TabletTool => {
                                    seat.tablet_seat()
                                        .remove_tablet(&TabletDescriptor::from(&device));
                                }
                                // TODO: Handle touch
                                _ => {}
                            }
                        }
                        break;
                    }
                }
                #[cfg(feature = "debug")]
                {
                    self.common.egui.state.handle_device_removed(&device);
                }
            }
            InputEvent::Keyboard { event, .. } => {
                use smithay::backend::input::KeyboardKeyEvent;

                let loop_handle = self.common.event_loop_handle.clone();

                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .unwrap()
                    .seats
                    .for_device(&event.device())
                    .cloned();
                if let Some(seat) = maybe_seat {
                    let current_output = seat.active_output();
                    let shortcuts_inhibited = self
                        .common
                        .shell
                        .read()
                        .unwrap()
                        .active_space(&current_output)
                        .focus_stack
                        .get(&seat)
                        .last()
                        .and_then(|window| {
                            window.wl_surface().and_then(|surface| {
                                seat.keyboard_shortcuts_inhibitor_for_surface(&surface)
                            })
                        })
                        .map(|inhibitor| inhibitor.is_active())
                        .unwrap_or(false);

                    let keycode = event.key_code();
                    let state = event.state();
                    trace!(?keycode, ?state, "key");

                    let serial = SERIAL_COUNTER.next_serial();
                    let time = Event::time_msec(&event);
                    let keyboard = seat.get_keyboard().unwrap();
                    let pointer = seat.get_pointer().unwrap();
                    let is_grabbed = keyboard.is_grabbed() || pointer.is_grabbed();
                    let current_focus = keyboard.current_focus();
                    if let Some((action, pattern)) = keyboard
                            .input(
                                self,
                                keycode,
                                state,
                                serial,
                                time,
                                |data, modifiers, handle| {
                                    // Leave move overview mode, if any modifier was released
                                    let mut shell = data.common.shell.write().unwrap();
                                    if let OverviewMode::Started(Trigger::KeyboardMove(action_modifiers), _) =
                                        shell.overview_mode().0
                                    {
                                        if (action_modifiers.ctrl && !modifiers.ctrl)
                                            || (action_modifiers.alt && !modifiers.alt)
                                            || (action_modifiers.logo && !modifiers.logo)
                                            || (action_modifiers.shift && !modifiers.shift)
                                        {
                                            shell.set_overview_mode(None, data.common.event_loop_handle.clone());
                                        }
                                    }
                                    // Leave swap overview mode, if any key was released
                                    if let OverviewMode::Started(Trigger::KeyboardSwap(action_pattern, old_descriptor), _) =
                                        shell.overview_mode().0
                                    {
                                        if (action_pattern.modifiers.ctrl && !modifiers.ctrl)
                                            || (action_pattern.modifiers.alt && !modifiers.alt)
                                            || (action_pattern.modifiers.logo && !modifiers.logo)
                                            || (action_pattern.modifiers.shift && !modifiers.shift)
                                            || (action_pattern.key.is_some() && handle.raw_syms().contains(&action_pattern.key.unwrap()) && state == KeyState::Released)
                                        {
                                            shell.set_overview_mode(None, data.common.event_loop_handle.clone());

                                            if let Some(focus) = current_focus {
                                                if let Some(new_descriptor) = shell.workspaces.active(&current_output).1.node_desc(focus) {
                                                    let mut spaces = shell.workspaces.spaces_mut();
                                                    if old_descriptor.handle != new_descriptor.handle {
                                                        let (mut old_w, mut other_w) = spaces.partition::<Vec<_>, _>(|w| w.handle == old_descriptor.handle);
                                                        if let Some(old_workspace) = old_w.get_mut(0) {
                                                            if let Some(new_workspace) = other_w.iter_mut().find(|w| w.handle == new_descriptor.handle) {
                                                                if let Some(focus) = TilingLayout::swap_trees(&mut old_workspace.tiling_layer, Some(&mut new_workspace.tiling_layer), &old_descriptor, &new_descriptor) {
                                                                    let seat = seat.clone();
                                                                    data.common.event_loop_handle.insert_idle(move |state| {
                                                                        Shell::set_focus(state, Some(&focus), &seat, None);
                                                                    });
                                                                }
                                                                old_workspace.refresh_focus_stack();
                                                                new_workspace.refresh_focus_stack();
                                                            }
                                                        }
                                                    } else {
                                                        if let Some(workspace) = spaces.find(|w| w.handle == new_descriptor.handle) {
                                                            if let Some(focus) = TilingLayout::swap_trees(&mut workspace.tiling_layer, None, &old_descriptor, &new_descriptor) {
                                                                std::mem::drop(spaces);
                                                                let seat = seat.clone();
                                                                data.common.event_loop_handle.insert_idle(move |state| {
                                                                    Shell::set_focus(state, Some(&focus), &seat, None);
                                                                });
                                                            }
                                                            workspace.refresh_focus_stack();
                                                        }
                                                    }
                                                }
                                            } else {
                                                let new_workspace = shell.workspaces.active(&current_output).1.handle;
                                                if new_workspace != old_descriptor.handle {
                                                    let spaces = shell.workspaces.spaces_mut();
                                                    let (mut old_w, mut other_w) = spaces.partition::<Vec<_>, _>(|w| w.handle == old_descriptor.handle);
                                                    if let Some(old_workspace) = old_w.get_mut(0) {
                                                        if let Some(new_workspace) = other_w.iter_mut().find(|w| w.handle == new_workspace) {
                                                            if new_workspace.tiling_layer.windows().next().is_none() {
                                                                if let Some(focus) = TilingLayout::move_tree(&mut old_workspace.tiling_layer, &mut new_workspace.tiling_layer, &new_workspace.handle, &seat, new_workspace.focus_stack.get(&seat).iter(), old_descriptor) {
                                                                    let seat = seat.clone();
                                                                    data.common.event_loop_handle.insert_idle(move |state| {
                                                                        Shell::set_focus(state, Some(&focus), &seat, None);
                                                                    });
                                                                }
                                                                old_workspace.refresh_focus_stack();
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }

                                    // Leave or update resize mode, if modifiers changed or initial key was released
                                    if let (ResizeMode::Started(action_pattern, _, _), _) =
                                        shell.resize_mode()
                                    {
                                        if action_pattern.key.is_some() && state == KeyState::Released
                                            && handle.raw_syms().contains(&action_pattern.key.unwrap())
                                        {
                                            shell.set_resize_mode(None, &data.common.config, data.common.event_loop_handle.clone());
                                        } else if action_pattern.modifiers != *modifiers {
                                            let mut new_pattern = action_pattern.clone();
                                            new_pattern.modifiers = modifiers.clone().into();
                                            let enabled = data
                                                .common
                                                .config
                                                .static_conf
                                                .key_bindings
                                                .iter()
                                                .find_map(move |(binding, action)| {
                                                    if binding == &new_pattern
                                                        && matches!(action, Action::Resizing(_))
                                                    {
                                                        let Action::Resizing(direction) = action else { unreachable!() };
                                                        Some((new_pattern.clone(), *direction))
                                                    } else {
                                                        None
                                                    }
                                                });
                                            shell.set_resize_mode(enabled, &data.common.config, data.common.event_loop_handle.clone());
                                        }
                                    }

                                    // Special case resizing with regards to arrow keys
                                    if let (ResizeMode::Started(_, _, direction), _) =
                                        shell.resize_mode()
                                    {
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
                                            let action = Action::_ResizingInternal(direction, edge, state);
                                            let key_pattern = KeyPattern {
                                                modifiers: modifiers.clone().into(),
                                                key: Some(Keysym::new(handle.raw_code().raw())),
                                            };

                                            if state == KeyState::Released {
                                                if let Some(tokens) = seat.supressed_keys().filter(&handle) {
                                                    for token in tokens {
                                                        loop_handle.remove(token);
                                                    }
                                                }
                                            } else {
                                                let token = if needs_key_repetition {
                                                    let seat_clone = seat.clone();
                                                    let action_clone = action.clone();
                                                    let key_pattern_clone = key_pattern.clone();
                                                    let start = Instant::now();
                                                    loop_handle.insert_source(Timer::from_duration(Duration::from_millis(200)), move |current, _, state| {
                                                        let duration = current.duration_since(start).as_millis();
                                                        state.handle_action(action_clone.clone(), &seat_clone, serial, time.overflowing_add(duration as u32).0, key_pattern_clone.clone(), None, true);
                                                        calloop::timer::TimeoutAction::ToDuration(Duration::from_millis(25))
                                                    }).ok()
                                                } else { None };

                                                seat.supressed_keys()
                                                        .add(&handle, token);
                                            }
                                            return FilterResult::Intercept(Some((
                                                action,
                                                key_pattern
                                            )));
                                        }
                                    }

                                    // cancel grabs
                                    if is_grabbed
                                        && handle.modified_sym() == Keysym::Escape
                                        && state == KeyState::Pressed
                                        && !modifiers.alt
                                        && !modifiers.ctrl
                                        && !modifiers.logo
                                        && !modifiers.shift
                                    {
                                        seat.supressed_keys()
                                                .add(&handle, None);
                                        return FilterResult::Intercept(Some((
                                            Action::Escape,
                                            KeyPattern {
                                                modifiers: KeyModifiers::default(),
                                                key: Some(Keysym::Escape),
                                            }
                                        )));
                                    }

                                    // Skip released events for initially surpressed keys
                                    if state == KeyState::Released {
                                        if let Some(tokens) = seat.supressed_keys().filter(&handle) {
                                            for token in tokens {
                                                loop_handle.remove(token);
                                            }
                                            return FilterResult::Intercept(None);
                                        }
                                    }

                                    // Pass keys to debug interface, if it has focus
                                    #[cfg(feature = "debug")]
                                    {
                                        if shell.seats.iter().position(|x| x == &seat).unwrap() == 0
                                            && data.common.egui.active
                                        {
                                            if data.common.egui.state.wants_keyboard() {
                                                data.common.egui.state.handle_keyboard(
                                                    &handle,
                                                    state == KeyState::Pressed,
                                                    modifiers.clone(),
                                                );
                                                seat.supressed_keys()
                                                    .add(&handle, None);
                                                return FilterResult::Intercept(None);
                                            }
                                        }
                                    }

                                    // Handle VT switches
                                    if state == KeyState::Pressed
                                        && (Keysym::XF86_Switch_VT_1.raw() ..= Keysym::XF86_Switch_VT_12.raw())
                                            .contains(&handle.modified_sym().raw())
                                    {
                                        if let Err(err) = data.backend.kms().switch_vt(
                                            (handle.modified_sym().raw() - Keysym::XF86_Switch_VT_1.raw()
                                                + 1)
                                                as i32,
                                        ) {
                                            error!(?err, "Failed switching virtual terminal.");
                                        }
                                        seat.supressed_keys().add(&handle, None);
                                        return FilterResult::Intercept(None);
                                    }

                                    // handle the rest of the global shortcuts
                                    let mut can_clear_modifiers_shortcut = true;
                                    if !shortcuts_inhibited {
                                        let modifiers_queue = seat.modifiers_shortcut_queue();
                                        for (binding, action) in
                                            data.common.config.static_conf.key_bindings.iter()
                                        {
                                            let modifiers_bypass = binding.key.is_none()
                                                && state == KeyState::Released
                                                && binding.modifiers != *modifiers
                                                && modifiers_queue.take(binding);

                                            if !modifiers_bypass && binding.key.is_none() && state == KeyState::Pressed && binding.modifiers == *modifiers {
                                                modifiers_queue.set(binding.clone());
                                                can_clear_modifiers_shortcut = false;
                                            }

                                            if (
                                                    binding.key.is_some()
                                                    && state == KeyState::Pressed
                                                    && handle.raw_syms().contains(&binding.key.unwrap())
                                                    && binding.modifiers == *modifiers
                                                ) || modifiers_bypass
                                            {
                                                modifiers_queue.clear();
                                                seat.supressed_keys().add(&handle, None);
                                                return FilterResult::Intercept(Some((
                                                    action.clone(),
                                                    binding.clone(),
                                                )));
                                            }
                                        }
                                    }

                                    if can_clear_modifiers_shortcut {
                                        seat.modifiers_shortcut_queue().clear();
                                    }

                                    // keys are passed through to apps
                                    FilterResult::Forward
                                },
                            )
                            .flatten()
                        {
                            self.handle_action(action, &seat, serial, time, pattern, None, true)
                        }
                }
            }
            InputEvent::PointerMotion { event, .. } => {
                use smithay::backend::input::PointerMotionEvent;

                let mut shell = self.common.shell.write().unwrap();
                if let Some(seat) = shell.seats.for_device(&event.device()).cloned() {
                    let current_output = seat.active_output();

                    let mut position = seat.get_pointer().unwrap().current_location().as_global();

                    let under = State::surface_under(position, &current_output, &mut *shell)
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
                                if !constraint.region().map_or(true, |x| {
                                    x.contains(ptr.current_location().to_i32_round() - *surface_loc)
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

                    position += event.delta().as_global();

                    let output = shell
                        .outputs()
                        .find(|output| output.geometry().to_f64().contains(position))
                        .cloned()
                        .unwrap_or(current_output.clone());

                    let new_under = State::surface_under(position, &output, &mut *shell)
                        .map(|(target, pos)| (target, pos.as_logical()));

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

                    if ptr.is_grabbed()
                        && seat
                            .user_data()
                            .get::<ResizeGrabMarker>()
                            .map(|marker| marker.get())
                            .unwrap_or(false)
                    {
                        if output != current_output {
                            ptr.frame(self);
                            return;
                        }
                    }

                    let output_geometry = output.geometry();

                    position.x = position.x.clamp(
                        output_geometry.loc.x as f64,
                        ((output_geometry.loc.x + output_geometry.size.w) as f64).next_lower(), // FIXME: Replace with f64::next_down when stable
                    );
                    position.y = position.y.clamp(
                        output_geometry.loc.y as f64,
                        ((output_geometry.loc.y + output_geometry.size.h) as f64).next_lower(), // FIXME: Replace with f64::next_down when stable
                    );

                    // If confined, don't move pointer if it would go outside surface or region
                    if pointer_confined {
                        if let Some((surface, surface_loc)) = &under {
                            if new_under.as_ref().and_then(|(under, _)| under.wl_surface())
                                != surface.wl_surface()
                            {
                                ptr.frame(self);
                                return;
                            }
                            if let PointerFocusTarget::WlSurface { surface, .. } = surface {
                                if under_from_surface_tree(
                                    surface,
                                    position.as_logical() - surface_loc.to_f64(),
                                    (0, 0),
                                    WindowSurfaceType::ALL,
                                )
                                .is_some()
                                {
                                    ptr.frame(self);
                                    return;
                                }
                            }
                            if let Some(region) = confine_region {
                                if !region
                                    .contains(position.as_logical().to_i32_round() - *surface_loc)
                                {
                                    ptr.frame(self);
                                    return;
                                }
                            }
                        }
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

                    // If pointer is now in a constraint region, activate it
                    if let Some((under, surface_location)) =
                        new_under.and_then(|(target, loc)| Some((target.wl_surface()?, loc)))
                    {
                        with_pointer_constraint(&under, &ptr, |constraint| match constraint {
                            Some(constraint) if !constraint.is_active() => {
                                let region = match &*constraint {
                                    PointerConstraint::Locked(locked) => locked.region(),
                                    PointerConstraint::Confined(confined) => confined.region(),
                                };
                                let point =
                                    ptr.current_location().to_i32_round() - surface_location;
                                if region.map_or(true, |region| region.contains(point)) {
                                    constraint.activate();
                                }
                            }
                            _ => {}
                        });
                    }

                    let shell = self.common.shell.read().unwrap();

                    if output != current_output {
                        for session in cursor_sessions_for_output(&*shell, &current_output) {
                            session.set_cursor_pos(None);
                        }
                        seat.set_active_output(&output);
                    }

                    for session in cursor_sessions_for_output(&shell, &output) {
                        if let Some((geometry, offset)) = seat.cursor_geometry(
                            position.as_logical().to_buffer(
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
                    #[cfg(feature = "debug")]
                    if shell.seats().position(|x| x == &seat).unwrap() == 0 {
                        if let Some(output) = shell.outputs().next() {
                            let location = position.to_local(&output).to_i32_round().as_logical();
                            self.common.egui.state.handle_pointer_motion(location);
                        }
                    }
                }
            }
            InputEvent::PointerMotionAbsolute { event, .. } => {
                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .unwrap()
                    .seats
                    .for_device(&event.device())
                    .cloned();
                if let Some(seat) = maybe_seat {
                    let output = seat.active_output();
                    let geometry = output.geometry();
                    let position = geometry.loc.to_f64()
                        + smithay::backend::input::AbsolutePositionEvent::position_transformed(
                            &event,
                            geometry.size.as_logical(),
                        )
                        .as_global();
                    let serial = SERIAL_COUNTER.next_serial();
                    let under = State::surface_under(
                        position,
                        &output,
                        &mut *self.common.shell.write().unwrap(),
                    )
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

                    let shell = self.common.shell.read().unwrap();
                    for session in cursor_sessions_for_output(&*shell, &output) {
                        if let Some((geometry, offset)) = seat.cursor_geometry(
                            position.as_logical().to_buffer(
                                output.current_scale().fractional_scale(),
                                output.current_transform(),
                                &geometry.size.to_f64().as_logical(),
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
                    #[cfg(feature = "debug")]
                    if shell.seats.iter().position(|x| x == &seat).unwrap() == 0 {
                        if let Some(output) = shell.outputs().next() {
                            let location = position.to_local(&output).to_i32_round().as_logical();
                            self.common.egui.state.handle_pointer_motion(location);
                        }
                    }
                }
            }
            InputEvent::PointerButton { event, .. } => {
                use smithay::backend::input::{ButtonState, PointerButtonEvent};

                let mut shell = self.common.shell.write().unwrap();
                if let Some(seat) = shell.seats.for_device(&event.device()).cloned() {
                    #[cfg(feature = "debug")]
                    if shell.seats.iter().position(|x| x == &seat).unwrap() == 0
                        && self.common.egui.active
                    {
                        if self.common.egui.state.wants_pointer() {
                            if let Some(button) = event.button() {
                                self.common.egui.state.handle_pointer_button(
                                    button,
                                    event.state() == ButtonState::Pressed,
                                );
                            }
                            return;
                        }
                    }

                    let serial = SERIAL_COUNTER.next_serial();
                    let button = event.button_code();
                    if event.state() == ButtonState::Pressed {
                        // change the keyboard focus unless the pointer is grabbed
                        // We test for any matching surface type here but always use the root
                        // (in case of a window the toplevel) surface for the focus.
                        // see: https://gitlab.freedesktop.org/wayland/wayland/-/issues/294
                        if !seat.get_pointer().unwrap().is_grabbed() {
                            let output = seat.active_output();

                            let pos = seat.get_pointer().unwrap().current_location().as_global();
                            let relative_pos = pos.to_local(&output);
                            let mut under: Option<KeyboardFocusTarget> = None;

                            if let Some(session_lock) = shell.session_lock.as_ref() {
                                under = session_lock
                                    .surfaces
                                    .get(&output)
                                    .map(|lock| lock.clone().into());
                            } else if let Some(window) =
                                shell.active_space(&output).get_fullscreen()
                            {
                                let layers = layer_map_for_output(&output);
                                if let Some(layer) =
                                    layers.layer_under(WlrLayer::Overlay, relative_pos.as_logical())
                                {
                                    let layer_loc = layers.layer_geometry(layer).unwrap().loc;
                                    if layer.can_receive_keyboard_focus()
                                        && layer
                                            .surface_under(
                                                relative_pos.as_logical() - layer_loc.to_f64(),
                                                WindowSurfaceType::ALL,
                                            )
                                            .is_some()
                                    {
                                        under = Some(layer.clone().into());
                                    }
                                } else {
                                    under = Some(window.clone().into());
                                }
                            } else {
                                let done = {
                                    let layers = layer_map_for_output(&output);
                                    if let Some(layer) = layers
                                        .layer_under(WlrLayer::Overlay, relative_pos.as_logical())
                                        .or_else(|| {
                                            layers.layer_under(
                                                WlrLayer::Top,
                                                relative_pos.as_logical(),
                                            )
                                        })
                                    {
                                        let layer_loc = layers.layer_geometry(layer).unwrap().loc;
                                        if layer.can_receive_keyboard_focus()
                                            && layer
                                                .surface_under(
                                                    relative_pos.as_logical() - layer_loc.to_f64(),
                                                    WindowSurfaceType::ALL,
                                                )
                                                .is_some()
                                        {
                                            under = Some(layer.clone().into());
                                            true
                                        } else {
                                            false
                                        }
                                    } else {
                                        false
                                    }
                                };
                                if !done {
                                    // Don't check override redirect windows, because we don't set keyboard focus to them explicitly.
                                    // These cases are handled by the XwaylandKeyboardGrab.
                                    if let Some(target) = shell.element_under(pos, &output) {
                                        if seat.get_keyboard().unwrap().modifier_state().logo {
                                            if let Some(surface) = target.toplevel() {
                                                let seat_clone = seat.clone();
                                                self.common.event_loop_handle.insert_idle(
                                                    move |state| {
                                                        let mut shell =
                                                            state.common.shell.write().unwrap();
                                                        let res = shell.move_request(
                                                            &surface,
                                                            &seat_clone,
                                                            serial,
                                                            ReleaseMode::NoMouseButtons,
                                                            false,
                                                            &state.common.config,
                                                            &state.common.event_loop_handle,
                                                            &state.common.xdg_activation_state,
                                                        );
                                                        if let Some((target, focus)) = res {
                                                            std::mem::drop(shell);
                                                            if target.is_touch_grab() {
                                                                seat_clone
                                                                    .get_touch()
                                                                    .unwrap()
                                                                    .set_grab(
                                                                        state, target, serial,
                                                                    );
                                                            } else {
                                                                seat_clone
                                                                    .get_pointer()
                                                                    .unwrap()
                                                                    .set_grab(
                                                                        state, target, serial,
                                                                        focus,
                                                                    );
                                                            }
                                                        }
                                                    },
                                                );
                                            }
                                        } else {
                                            under = Some(target);
                                        }
                                    } else {
                                        let layers = layer_map_for_output(&output);
                                        if let Some(layer) = layers
                                            .layer_under(
                                                WlrLayer::Bottom,
                                                relative_pos.as_logical(),
                                            )
                                            .or_else(|| {
                                                layers.layer_under(
                                                    WlrLayer::Background,
                                                    relative_pos.as_logical(),
                                                )
                                            })
                                        {
                                            let layer_loc =
                                                layers.layer_geometry(layer).unwrap().loc;
                                            if layer.can_receive_keyboard_focus()
                                                && layer
                                                    .surface_under(
                                                        relative_pos.as_logical()
                                                            - layer_loc.to_f64(),
                                                        WindowSurfaceType::ALL,
                                                    )
                                                    .is_some()
                                            {
                                                under = Some(layer.clone().into());
                                            }
                                        };
                                    }
                                }
                            }
                            std::mem::drop(shell);
                            Shell::set_focus(self, under.as_ref(), &seat, Some(serial));
                        } else {
                            std::mem::drop(shell);
                        }
                    } else {
                        if let OverviewMode::Started(Trigger::Pointer(action_button), _) =
                            shell.overview_mode().0
                        {
                            if action_button == button {
                                shell
                                    .set_overview_mode(None, self.common.event_loop_handle.clone());
                            }
                        }
                        std::mem::drop(shell);
                    };

                    let ptr = seat.get_pointer().unwrap();
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
                    .unwrap()
                    .seats
                    .for_device(&event.device())
                    .cloned();
                if let Some(seat) = maybe_seat {
                    #[cfg(feature = "debug")]
                    if self
                        .common
                        .shell
                        .read()
                        .unwrap()
                        .seats
                        .iter()
                        .position(|x| x == &seat)
                        .unwrap()
                        == 0
                        && self.common.egui.active
                    {
                        if self.common.egui.state.wants_pointer() {
                            self.common.egui.state.handle_pointer_axis(
                                event
                                    .amount_v120(Axis::Horizontal)
                                    .or_else(|| {
                                        event.amount(Axis::Horizontal).map(|x| x * 3.0 * 120.0)
                                    })
                                    .unwrap_or(0.0)
                                    / 120.0,
                                event
                                    .amount_v120(Axis::Vertical)
                                    .or_else(|| {
                                        event.amount(Axis::Vertical).map(|x| x * 3.0 * 120.0)
                                    })
                                    .unwrap_or(0.0)
                                    / 120.0,
                            );
                            return;
                        }
                    }

                    let mut frame = AxisFrame::new(event.time_msec()).source(event.source());
                    if let Some(horizontal_amount) = event.amount(Axis::Horizontal) {
                        if horizontal_amount != 0.0 {
                            frame =
                                frame.value(Axis::Horizontal, scroll_factor * horizontal_amount);
                            if let Some(discrete) = event.amount_v120(Axis::Horizontal) {
                                frame = frame.v120(Axis::Horizontal, discrete as i32);
                            }
                        } else if event.source() == AxisSource::Finger {
                            frame = frame.stop(Axis::Horizontal);
                        }
                    }
                    if let Some(vertical_amount) = event.amount(Axis::Vertical) {
                        if vertical_amount != 0.0 {
                            frame = frame.value(Axis::Vertical, scroll_factor * vertical_amount);
                            if let Some(discrete) = event.amount_v120(Axis::Vertical) {
                                frame = frame.v120(Axis::Vertical, discrete as i32);
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
            InputEvent::GestureSwipeBegin { event, .. } => {
                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .unwrap()
                    .seats
                    .for_device(&event.device())
                    .cloned();
                if let Some(seat) = maybe_seat {
                    if event.fingers() >= 3 {
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
                    .unwrap()
                    .seats
                    .for_device(&event.device())
                    .cloned();
                if let Some(seat) = maybe_seat {
                    let mut activate_action: Option<SwipeAction> = None;
                    if let Some(ref mut gesture_state) = self.common.gesture_state {
                        let first_update = gesture_state.update(
                            event.delta(),
                            Duration::from_millis(event.time_msec() as u64),
                        );
                        // Decide on action if first update
                        if first_update {
                            activate_action = match gesture_state.fingers {
                                3 => None, // TODO: 3 finger gestures
                                4 => {
                                    if self.common.config.cosmic_conf.workspaces.workspace_layout
                                        == WorkspaceLayout::Horizontal
                                    {
                                        match gesture_state.direction {
                                            Some(Direction::Left) => {
                                                Some(SwipeAction::NextWorkspace)
                                            }
                                            Some(Direction::Right) => {
                                                Some(SwipeAction::PrevWorkspace)
                                            }
                                            _ => None, // TODO: Other actions
                                        }
                                    } else {
                                        match gesture_state.direction {
                                            Some(Direction::Up) => Some(SwipeAction::NextWorkspace),
                                            Some(Direction::Down) => {
                                                Some(SwipeAction::PrevWorkspace)
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
                            Some(SwipeAction::NextWorkspace) | Some(SwipeAction::PrevWorkspace) => {
                                self.common.shell.write().unwrap().update_workspace_delta(
                                    &seat.active_output(),
                                    gesture_state.delta,
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
                    match activate_action {
                        Some(SwipeAction::NextWorkspace) => {
                            let _ = to_next_workspace(
                                &mut *self.common.shell.write().unwrap(),
                                &seat,
                                true,
                                &mut self.common.workspace_state.update(),
                            );
                        }
                        Some(SwipeAction::PrevWorkspace) => {
                            let _ = to_previous_workspace(
                                &mut *self.common.shell.write().unwrap(),
                                &seat,
                                true,
                                &mut self.common.workspace_state.update(),
                            );
                        }
                        _ => {}
                    }
                }
            }
            InputEvent::GestureSwipeEnd { event, .. } => {
                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .unwrap()
                    .seats
                    .for_device(&event.device())
                    .cloned();
                if let Some(seat) = maybe_seat {
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
                                let _ = self.common.shell.write().unwrap().end_workspace_swipe(
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
                    .unwrap()
                    .seats
                    .for_device(&event.device())
                    .cloned();
                if let Some(seat) = maybe_seat {
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
                    .unwrap()
                    .seats
                    .for_device(&event.device())
                    .cloned();
                if let Some(seat) = maybe_seat {
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
                    .unwrap()
                    .seats
                    .for_device(&event.device())
                    .cloned();
                if let Some(seat) = maybe_seat {
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
                    .unwrap()
                    .seats
                    .for_device(&event.device())
                    .cloned();
                if let Some(seat) = maybe_seat {
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
                    .unwrap()
                    .seats
                    .for_device(&event.device())
                    .cloned();
                if let Some(seat) = maybe_seat {
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
                let mut shell = self.common.shell.write().unwrap();
                if let Some(seat) = shell.seats.for_device(&event.device()).cloned() {
                    let Some(output) =
                        mapped_output_for_device(&self.common.config, &*shell, &event.device())
                            .cloned()
                    else {
                        return;
                    };

                    let geometry = output.geometry();

                    let position = geometry.loc.to_f64()
                        + event
                            .position_transformed(geometry.size.as_logical())
                            .as_global();

                    let under = State::surface_under(position, &output, &mut *shell)
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
                let mut shell = self.common.shell.write().unwrap();
                if let Some(seat) = shell.seats.for_device(&event.device()).cloned() {
                    let Some(output) =
                        mapped_output_for_device(&self.common.config, &*shell, &event.device())
                            .cloned()
                    else {
                        return;
                    };

                    let geometry = output.geometry();

                    let position = geometry.loc.to_f64()
                        + event
                            .position_transformed(geometry.size.as_logical())
                            .as_global();

                    let under = State::surface_under(position, &output, &mut *shell)
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
                let mut shell = self.common.shell.write().unwrap();
                if let OverviewMode::Started(Trigger::Touch(slot), _) = shell.overview_mode().0 {
                    if slot == event.slot() {
                        shell.set_overview_mode(None, self.common.event_loop_handle.clone());
                    }
                }

                let maybe_seat = shell.seats.for_device(&event.device()).cloned();
                if let Some(seat) = maybe_seat {
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
                    .unwrap()
                    .seats
                    .for_device(&event.device())
                    .cloned();
                if let Some(seat) = maybe_seat {
                    let touch = seat.get_touch().unwrap();
                    touch.cancel(self);
                }
            }
            InputEvent::TouchFrame { event, .. } => {
                let maybe_seat = self
                    .common
                    .shell
                    .read()
                    .unwrap()
                    .seats
                    .for_device(&event.device())
                    .cloned();
                if let Some(seat) = maybe_seat {
                    let touch = seat.get_touch().unwrap();
                    touch.frame(self);
                }
            }
            InputEvent::TabletToolAxis { event, .. } => {
                let mut shell = self.common.shell.write().unwrap();
                if let Some(seat) = shell.seats.for_device(&event.device()).cloned() {
                    let Some(output) =
                        mapped_output_for_device(&self.common.config, &shell, &event.device())
                            .cloned()
                    else {
                        return;
                    };
                    let geometry = output.geometry();

                    let position = event
                        .position_transformed(geometry.size.as_logical())
                        .as_global()
                        + geometry.loc.to_f64();

                    let under = State::surface_under(position, &output, &mut *shell)
                        .map(|(target, pos)| (target, pos.as_logical()));

                    std::mem::drop(shell);

                    let pointer = seat.get_pointer().unwrap();
                    pointer.motion(
                        self,
                        under.clone(),
                        &MotionEvent {
                            location: position.as_logical(),
                            serial: SERIAL_COUNTER.next_serial(),
                            time: 0,
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
                            under.and_then(|(f, loc)| f.wl_surface().map(|s| (s, loc))),
                            &tablet,
                            SERIAL_COUNTER.next_serial(),
                            event.time_msec(),
                        );
                    }
                }
            }
            InputEvent::TabletToolProximity { event, .. } => {
                let mut shell = self.common.shell.write().unwrap();
                if let Some(seat) = shell.seats.for_device(&event.device()).cloned() {
                    let Some(output) =
                        mapped_output_for_device(&self.common.config, &shell, &event.device())
                            .cloned()
                    else {
                        return;
                    };
                    let geometry = output.geometry();

                    let position = event
                        .position_transformed(geometry.size.as_logical())
                        .as_global()
                        + geometry.loc.to_f64();

                    let under = State::surface_under(position, &output, &mut *shell)
                        .map(|(target, pos)| (target, pos.as_logical()));

                    std::mem::drop(shell);

                    let pointer = seat.get_pointer().unwrap();
                    pointer.motion(
                        self,
                        under.clone(),
                        &MotionEvent {
                            location: position.as_logical(),
                            serial: SERIAL_COUNTER.next_serial(),
                            time: 0,
                        },
                    );

                    let tablet_seat = seat.tablet_seat();

                    let tablet = tablet_seat.get_tablet(&TabletDescriptor::from(&event.device()));
                    let tool = tablet_seat.get_tool(&event.tool());

                    if let (Some(tablet), Some(tool)) = (tablet, tool) {
                        match event.state() {
                            ProximityState::In => {
                                if let Some(under) =
                                    under.and_then(|(f, loc)| f.wl_surface().map(|s| (s, loc)))
                                {
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
                    .unwrap()
                    .seats
                    .for_device(&event.device())
                    .cloned();
                if let Some(seat) = maybe_seat {
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
                    .unwrap()
                    .seats
                    .for_device(&event.device())
                    .cloned();
                if let Some(seat) = maybe_seat {
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
            InputEvent::SwitchToggle { event: _ } => {}
        }
    }

    pub fn handle_action(
        &mut self,
        action: Action,
        seat: &Seat<State>,
        serial: Serial,
        time: u32,
        pattern: KeyPattern,
        direction: Option<Direction>,
        propagate: bool,
    ) {
        // TODO: Detect if started from login manager or tty, and only allow
        // `Terminate` if it will return to login manager.
        if self.common.shell.read().unwrap().session_lock.is_some()
            && !matches!(action, Action::Terminate | Action::Debug)
        {
            return;
        }

        match action {
            Action::Terminate => {
                self.common.should_stop = true;
            }
            #[cfg(feature = "debug")]
            Action::Debug => {
                self.common.egui.active = !self.common.egui.active;
                for mapped in self
                    .common
                    .shell
                    .read()
                    .unwrap()
                    .workspaces
                    .spaces()
                    .flat_map(|w| w.mapped())
                {
                    mapped.set_debug(self.common.egui.active);
                }
            }
            #[cfg(not(feature = "debug"))]
            Action::Debug => {
                info!("Debug overlay not included in this build.")
            }
            Action::Close => {
                let current_output = seat.active_output();
                let shell = self.common.shell.read().unwrap();
                let workspace = shell.active_space(&current_output);
                if let Some(window) = workspace.focus_stack.get(seat).last() {
                    window.send_close();
                }
            }
            Action::Escape => {
                {
                    let mut shell = self.common.shell.write().unwrap();
                    shell.set_overview_mode(None, self.common.event_loop_handle.clone());
                    shell.set_resize_mode(
                        None,
                        &self.common.config,
                        self.common.event_loop_handle.clone(),
                    );
                }
                let pointer = seat.get_pointer().unwrap();
                let keyboard = seat.get_keyboard().unwrap();
                if pointer.is_grabbed() {
                    pointer.unset_grab(self, serial, time);
                }
                if keyboard.is_grabbed() {
                    keyboard.unset_grab(self);
                }
            }
            Action::Workspace(key_num) => {
                let current_output = seat.active_output();
                let workspace = match key_num {
                    0 => 9,
                    x => x - 1,
                };
                let _ = self.common.shell.write().unwrap().activate(
                    &current_output,
                    workspace as usize,
                    WorkspaceDelta::new_shortcut(),
                    &mut self.common.workspace_state.update(),
                );
            }
            Action::LastWorkspace => {
                let current_output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();
                let workspace = shell.workspaces.len(&current_output).saturating_sub(1);
                let _ = shell.activate(
                    &current_output,
                    workspace,
                    WorkspaceDelta::new_shortcut(),
                    &mut self.common.workspace_state.update(),
                );
            }
            Action::NextWorkspace => {
                let next = to_next_workspace(
                    &mut *self.common.shell.write().unwrap(),
                    seat,
                    false,
                    &mut self.common.workspace_state.update(),
                );
                if next.is_err() && propagate {
                    if let Some(inferred) = pattern.inferred_direction() {
                        self.handle_action(
                            Action::SwitchOutput(inferred),
                            seat,
                            serial,
                            time,
                            pattern,
                            direction,
                            false,
                        )
                    };
                }
            }
            Action::PreviousWorkspace => {
                let previous = to_previous_workspace(
                    &mut *self.common.shell.write().unwrap(),
                    seat,
                    false,
                    &mut self.common.workspace_state.update(),
                );
                if previous.is_err() && propagate {
                    if let Some(inferred) = pattern.inferred_direction() {
                        self.handle_action(
                            Action::SwitchOutput(inferred),
                            seat,
                            serial,
                            time,
                            pattern,
                            direction,
                            false,
                        )
                    };
                }
            }
            x @ Action::MoveToWorkspace(_) | x @ Action::SendToWorkspace(_) => {
                let current_output = seat.active_output();
                let follow = matches!(x, Action::MoveToWorkspace(_));
                let workspace = match x {
                    Action::MoveToWorkspace(0) | Action::SendToWorkspace(0) => 9,
                    Action::MoveToWorkspace(x) | Action::SendToWorkspace(x) => x - 1,
                    _ => unreachable!(),
                };
                let res = self.common.shell.write().unwrap().move_current_window(
                    seat,
                    &current_output,
                    (&current_output, Some(workspace as usize)),
                    follow,
                    None,
                    &mut self.common.workspace_state.update(),
                );
                if let Ok(Some((target, _point))) = res {
                    Shell::set_focus(self, Some(&target), seat, None);
                }
            }
            x @ Action::MoveToLastWorkspace | x @ Action::SendToLastWorkspace => {
                let current_output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();
                let workspace = shell.workspaces.len(&current_output).saturating_sub(1);
                let res = shell.move_current_window(
                    seat,
                    &current_output,
                    (&current_output, Some(workspace as usize)),
                    matches!(x, Action::MoveToLastWorkspace),
                    None,
                    &mut self.common.workspace_state.update(),
                );
                if let Ok(Some((target, _point))) = res {
                    std::mem::drop(shell);
                    Shell::set_focus(self, Some(&target), seat, None);
                }
            }
            x @ Action::MoveToNextWorkspace | x @ Action::SendToNextWorkspace => {
                let current_output = seat.active_output();
                let res = {
                    let mut shell = self.common.shell.write().unwrap();
                    let workspace = shell
                        .workspaces
                        .active_num(&current_output)
                        .1
                        .saturating_add(1);
                    shell.move_current_window(
                        seat,
                        &current_output,
                        (&current_output, Some(workspace as usize)),
                        matches!(x, Action::MoveToNextWorkspace),
                        direction,
                        &mut self.common.workspace_state.update(),
                    )
                };

                match res {
                    Ok(Some((target, _point))) => {
                        Shell::set_focus(self, Some(&target), seat, None);
                    }
                    Err(_) if propagate => {
                        if let Some(inferred) = pattern.inferred_direction() {
                            self.handle_action(
                                if matches!(x, Action::MoveToNextWorkspace) {
                                    Action::MoveToOutput(inferred)
                                } else {
                                    Action::SendToOutput(inferred)
                                },
                                seat,
                                serial,
                                time,
                                pattern,
                                direction,
                                false,
                            )
                        }
                    }
                    _ => {}
                }
            }
            x @ Action::MoveToPreviousWorkspace | x @ Action::SendToPreviousWorkspace => {
                let current_output = seat.active_output();
                let res = {
                    let mut shell = self.common.shell.write().unwrap();
                    let workspace = shell
                        .workspaces
                        .active_num(&current_output)
                        .1
                        .saturating_sub(1);
                    // TODO: Possibly move to prev output, if idx < 0
                    shell.move_current_window(
                        seat,
                        &current_output,
                        (&current_output, Some(workspace as usize)),
                        matches!(x, Action::MoveToPreviousWorkspace),
                        direction,
                        &mut self.common.workspace_state.update(),
                    )
                };

                match res {
                    Ok(Some((target, _point))) => {
                        Shell::set_focus(self, Some(&target), seat, None);
                    }
                    Err(_) if propagate => {
                        if let Some(inferred) = pattern.inferred_direction() {
                            self.handle_action(
                                if matches!(x, Action::MoveToPreviousWorkspace) {
                                    Action::MoveToOutput(inferred)
                                } else {
                                    Action::SendToOutput(inferred)
                                },
                                seat,
                                serial,
                                time,
                                pattern,
                                direction,
                                false,
                            )
                        }
                    }
                    _ => {}
                }
            }
            Action::SwitchOutput(direction) => {
                let current_output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();

                let next_output = shell.next_output(&current_output, direction).cloned();

                if let Some(next_output) = next_output {
                    let idx = shell.workspaces.active_num(&next_output).1;
                    let res = shell.activate(
                        &next_output,
                        idx,
                        WorkspaceDelta::new_shortcut(),
                        &mut self.common.workspace_state.update(),
                    );
                    match res {
                        Ok(Some(new_pos)) => {
                            std::mem::drop(shell);
                            seat.set_active_output(&next_output);
                            if let Some(ptr) = seat.get_pointer() {
                                ptr.motion(
                                    self,
                                    None,
                                    &MotionEvent {
                                        location: new_pos.to_f64().as_logical(),
                                        serial,
                                        time,
                                    },
                                );
                                ptr.frame(self);
                            }
                        }
                        Ok(None) => {
                            seat.set_active_output(&next_output);
                        }
                        _ => {}
                    }
                } else if propagate {
                    std::mem::drop(shell);
                    match (
                        direction,
                        self.common.config.cosmic_conf.workspaces.workspace_layout,
                    ) {
                        (Direction::Left, WorkspaceLayout::Horizontal)
                        | (Direction::Up, WorkspaceLayout::Vertical) => self.handle_action(
                            Action::PreviousWorkspace,
                            seat,
                            serial,
                            time,
                            pattern,
                            Some(direction),
                            false,
                        ),
                        (Direction::Right, WorkspaceLayout::Horizontal)
                        | (Direction::Down, WorkspaceLayout::Vertical) => self.handle_action(
                            Action::NextWorkspace,
                            seat,
                            serial,
                            time,
                            pattern,
                            Some(direction),
                            false,
                        ),

                        _ => {}
                    }
                }
            }
            Action::NextOutput => {
                let current_output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();

                let next_output = shell
                    .outputs()
                    .skip_while(|o| *o != &current_output)
                    .skip(1)
                    .next()
                    .cloned();
                if let Some(next_output) = next_output {
                    let idx = shell.workspaces.active_num(&next_output).1;
                    let res = shell.activate(
                        &next_output,
                        idx,
                        WorkspaceDelta::new_shortcut(),
                        &mut self.common.workspace_state.update(),
                    );
                    match res {
                        Ok(Some(new_pos)) => {
                            std::mem::drop(shell);
                            seat.set_active_output(&next_output);
                            if let Some(ptr) = seat.get_pointer() {
                                ptr.motion(
                                    self,
                                    None,
                                    &MotionEvent {
                                        location: new_pos.to_f64().as_logical(),
                                        serial,
                                        time,
                                    },
                                );
                                ptr.frame(self);
                            }
                        }
                        Ok(None) => {
                            seat.set_active_output(&next_output);
                        }
                        _ => {}
                    }
                }
            }
            Action::PreviousOutput => {
                let current_output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();

                let prev_output = shell
                    .outputs()
                    .rev()
                    .skip_while(|o| *o != &current_output)
                    .skip(1)
                    .next()
                    .cloned();
                if let Some(prev_output) = prev_output {
                    let idx = shell.workspaces.active_num(&prev_output).1;
                    let res = shell.activate(
                        &prev_output,
                        idx,
                        WorkspaceDelta::new_shortcut(),
                        &mut self.common.workspace_state.update(),
                    );
                    match res {
                        Ok(Some(new_pos)) => {
                            std::mem::drop(shell);
                            seat.set_active_output(&prev_output);
                            if let Some(ptr) = seat.get_pointer() {
                                ptr.motion(
                                    self,
                                    None,
                                    &MotionEvent {
                                        location: new_pos.to_f64().as_logical(),
                                        serial,
                                        time,
                                    },
                                );
                                ptr.frame(self);
                            }
                        }
                        Ok(None) => {
                            seat.set_active_output(&prev_output);
                        }
                        _ => {}
                    }
                }
            }
            action @ Action::MoveToOutput(_) | action @ Action::SendToOutput(_) => {
                let is_move_action = matches!(action, Action::MoveToOutput(_));
                let direction = match action {
                    Action::MoveToOutput(dir) => dir,
                    Action::SendToOutput(dir) => dir,
                    _ => unreachable!(),
                };

                let current_output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();
                let next_output = shell.next_output(&current_output, direction).cloned();

                if let Some(next_output) = next_output {
                    let res = shell.move_current_window(
                        seat,
                        &current_output,
                        (&next_output, None),
                        is_move_action,
                        Some(direction),
                        &mut self.common.workspace_state.update(),
                    );
                    if let Ok(Some((target, new_pos))) = res {
                        std::mem::drop(shell);
                        Shell::set_focus(self, Some(&target), seat, None);
                        if let Some(ptr) = seat.get_pointer() {
                            ptr.motion(
                                self,
                                None,
                                &MotionEvent {
                                    location: new_pos.to_f64().as_logical(),
                                    serial,
                                    time,
                                },
                            );
                            ptr.frame(self);
                        }
                    }
                } else if propagate {
                    std::mem::drop(shell);
                    match (
                        direction,
                        self.common.config.cosmic_conf.workspaces.workspace_layout,
                    ) {
                        (Direction::Left, WorkspaceLayout::Horizontal)
                        | (Direction::Up, WorkspaceLayout::Vertical) => self.handle_action(
                            Action::MoveToPreviousWorkspace,
                            seat,
                            serial,
                            time,
                            pattern,
                            Some(direction),
                            false,
                        ),
                        (Direction::Right, WorkspaceLayout::Horizontal)
                        | (Direction::Down, WorkspaceLayout::Vertical) => self.handle_action(
                            Action::MoveToNextWorkspace,
                            seat,
                            serial,
                            time,
                            pattern,
                            Some(direction),
                            false,
                        ),

                        _ => {}
                    }
                }
            }
            x @ Action::MoveToNextOutput | x @ Action::SendToNextOutput => {
                let current_output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();

                let next_output = shell
                    .outputs()
                    .skip_while(|o| *o != &current_output)
                    .skip(1)
                    .next()
                    .cloned();
                if let Some(next_output) = next_output {
                    let res = shell.move_current_window(
                        seat,
                        &current_output,
                        (&next_output, None),
                        matches!(x, Action::MoveToNextOutput),
                        direction,
                        &mut self.common.workspace_state.update(),
                    );
                    if let Ok(Some((target, new_pos))) = res {
                        std::mem::drop(shell);
                        Shell::set_focus(self, Some(&target), seat, None);
                        if let Some(ptr) = seat.get_pointer() {
                            ptr.motion(
                                self,
                                None,
                                &MotionEvent {
                                    location: new_pos.to_f64().as_logical(),
                                    serial,
                                    time,
                                },
                            );
                            ptr.frame(self);
                        }
                    }
                }
            }
            x @ Action::MoveToPreviousOutput | x @ Action::SendToPreviousOutput => {
                let current_output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();

                let prev_output = shell
                    .outputs()
                    .rev()
                    .skip_while(|o| *o != &current_output)
                    .skip(1)
                    .next()
                    .cloned();
                if let Some(prev_output) = prev_output {
                    let res = shell.move_current_window(
                        seat,
                        &current_output,
                        (&prev_output, None),
                        matches!(x, Action::MoveToPreviousOutput),
                        direction,
                        &mut self.common.workspace_state.update(),
                    );
                    if let Ok(Some((target, new_pos))) = res {
                        std::mem::drop(shell);
                        Shell::set_focus(self, Some(&target), seat, None);
                        if let Some(ptr) = seat.get_pointer() {
                            ptr.motion(
                                self,
                                None,
                                &MotionEvent {
                                    location: new_pos.to_f64().as_logical(),
                                    serial,
                                    time,
                                },
                            );
                            ptr.frame(self);
                        }
                    }
                }
            }
            Action::MigrateWorkspaceToNextOutput => {
                let current_output = seat.active_output();
                let (active, next_output) = {
                    let shell = self.common.shell.read().unwrap();
                    let output = shell
                        .outputs()
                        .skip_while(|o| *o != &current_output)
                        .skip(1)
                        .next()
                        .cloned();

                    (shell.active_space(&current_output).handle, output)
                };
                if let Some(next_output) = next_output {
                    self.common
                        .migrate_workspace(&current_output, &next_output, &active);
                }
            }
            Action::MigrateWorkspaceToPreviousOutput => {
                let current_output = seat.active_output();
                let (active, prev_output) = {
                    let shell = self.common.shell.read().unwrap();
                    let output = shell
                        .outputs()
                        .rev()
                        .skip_while(|o| *o != &current_output)
                        .skip(1)
                        .next()
                        .cloned();

                    (shell.active_space(&current_output).handle, output)
                };
                if let Some(prev_output) = prev_output {
                    self.common
                        .migrate_workspace(&current_output, &prev_output, &active);
                }
            }
            Action::MigrateWorkspaceToOutput(direction) => {
                let current_output = seat.active_output();
                let (active, next_output) = {
                    let shell = self.common.shell.read().unwrap();

                    (
                        shell.active_space(&current_output).handle,
                        shell.next_output(&current_output, direction).cloned(),
                    )
                };

                if let Some(next_output) = next_output {
                    self.common
                        .migrate_workspace(&current_output, &next_output, &active);
                }
            }
            Action::Focus(focus) => {
                let result = self.common.shell.read().unwrap().next_focus(focus, seat);

                match result {
                    FocusResult::None => {
                        let dir = match focus {
                            FocusDirection::Down => Some(Direction::Down),
                            FocusDirection::Up => Some(Direction::Up),
                            FocusDirection::Left => Some(Direction::Left),
                            FocusDirection::Right => Some(Direction::Right),
                            _ => None,
                        };

                        if let Some(direction) = dir {
                            self.handle_action(
                                Action::SwitchOutput(direction),
                                seat,
                                serial,
                                time,
                                pattern,
                                Some(direction),
                                true,
                            )
                        }
                    }
                    FocusResult::Handled => {}
                    FocusResult::Some(target) => {
                        Shell::set_focus(self, Some(&target), seat, None);
                    }
                }
            }
            Action::Move(direction) => {
                let res = self
                    .common
                    .shell
                    .write()
                    .unwrap()
                    .move_current_element(direction, seat);
                match res {
                    MoveResult::MoveFurther(_move_further) => self.handle_action(
                        Action::MoveToOutput(direction),
                        seat,
                        serial,
                        time,
                        pattern,
                        Some(direction),
                        true,
                    ),
                    MoveResult::ShiftFocus(shift) => {
                        Shell::set_focus(self, Some(&shift), seat, None);
                    }
                    _ => {
                        let current_output = seat.active_output();
                        let mut shell = self.common.shell.write().unwrap();
                        let workspace = shell.active_space(&current_output);
                        if let Some(focused_window) = workspace.focus_stack.get(seat).last() {
                            if workspace.is_tiled(focused_window) {
                                shell.set_overview_mode(
                                    Some(Trigger::KeyboardMove(pattern.modifiers)),
                                    self.common.event_loop_handle.clone(),
                                );
                            }
                        }
                    }
                }
            }
            Action::SwapWindow => {
                let current_output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();

                let workspace = shell.active_space_mut(&current_output);
                if workspace.get_fullscreen().is_some() {
                    return; // TODO, is this what we want? Maybe disengage fullscreen instead?
                }

                let keyboard_handle = seat.get_keyboard().unwrap();
                if let Some(focus) = keyboard_handle.current_focus() {
                    if let Some(descriptor) = workspace.node_desc(focus) {
                        let grab = SwapWindowGrab::new(seat.clone(), descriptor.clone());
                        drop(shell);
                        keyboard_handle.set_grab(self, grab, serial);
                        let mut shell = self.common.shell.write().unwrap();
                        shell.set_overview_mode(
                            Some(Trigger::KeyboardSwap(pattern, descriptor)),
                            self.common.event_loop_handle.clone(),
                        );
                    }
                }
            }
            Action::Minimize => {
                let current_output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();
                let workspace = shell.active_space_mut(&current_output);
                let focus_stack = workspace.focus_stack.get(seat);
                let focused_window = focus_stack.last().cloned();
                if let Some(window) = focused_window {
                    shell.minimize_request(&window);
                }
            }
            Action::Maximize => {
                let current_output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();
                let workspace = shell.active_space(&current_output);
                let focus_stack = workspace.focus_stack.get(seat);
                let focused_window = focus_stack.last().cloned();
                if let Some(window) = focused_window {
                    shell.maximize_toggle(&window, seat);
                }
            }
            Action::Resizing(direction) => self.common.shell.write().unwrap().set_resize_mode(
                Some((pattern, direction)),
                &self.common.config,
                self.common.event_loop_handle.clone(),
            ),
            Action::_ResizingInternal(direction, edge, state) => {
                if state == KeyState::Pressed {
                    self.common
                        .shell
                        .write()
                        .unwrap()
                        .resize(seat, direction, edge);
                } else {
                    self.common
                        .shell
                        .write()
                        .unwrap()
                        .finish_resize(direction, edge);
                }
            }
            Action::ToggleOrientation => {
                let output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();
                let workspace = shell.active_space_mut(&output);
                workspace.tiling_layer.update_orientation(None, &seat);
            }
            Action::Orientation(orientation) => {
                let output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();
                let workspace = shell.active_space_mut(&output);
                workspace
                    .tiling_layer
                    .update_orientation(Some(orientation), &seat);
            }
            Action::ToggleStacking => {
                let res = self
                    .common
                    .shell
                    .write()
                    .unwrap()
                    .toggle_stacking_focused(seat);
                if let Some(new_focus) = res {
                    Shell::set_focus(self, Some(&new_focus), seat, Some(serial));
                }
            }
            Action::ToggleTiling => {
                if matches!(
                    self.common.config.cosmic_conf.autotile_behavior,
                    TileBehavior::Global
                ) {
                    let autotile = !self.common.config.cosmic_conf.autotile;
                    self.common.config.cosmic_conf.autotile = autotile;

                    {
                        let mut shell = self.common.shell.write().unwrap();
                        let shell_ref = &mut *shell;
                        shell_ref.workspaces.update_autotile(
                            self.common.config.cosmic_conf.autotile,
                            &mut self.common.workspace_state.update(),
                            shell_ref.seats.iter(),
                        );
                    }
                    let config = self.common.config.cosmic_helper.clone();
                    thread::spawn(move || {
                        if let Err(err) = config.set("autotile", autotile) {
                            error!(?err, "Failed to update autotile key");
                        }
                    });
                } else {
                    let output = seat.active_output();
                    let mut shell = self.common.shell.write().unwrap();
                    let workspace = shell.workspaces.active_mut(&output);
                    let mut guard = self.common.workspace_state.update();
                    workspace.toggle_tiling(seat, &mut guard);
                }
            }
            Action::ToggleWindowFloating => {
                let output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();
                let workspace = shell.active_space_mut(&output);
                workspace.toggle_floating_window_focused(seat);
            }
            Action::ToggleSticky => {
                self.common
                    .shell
                    .write()
                    .unwrap()
                    .toggle_sticky_current(seat);
            }
            Action::Spawn(command) => {
                let mut shell = self.common.shell.write().unwrap();

                let (token, data) = self.common.xdg_activation_state.create_external_token(None);
                let (token, data) = (token.clone(), data.clone());

                let output = shell.seats.last_active().active_output();
                let workspace = shell.active_space_mut(&output);
                workspace.pending_tokens.insert(token.clone());
                let handle = workspace.handle;
                std::mem::drop(shell);
                data.user_data
                    .insert_if_missing(move || ActivationContext::Workspace(handle));

                let wayland_display = self.common.socket.clone();
                let display = self
                    .common
                    .xwayland_state
                    .as_ref()
                    .map(|s| format!(":{}", s.display))
                    .unwrap_or_default();

                let mut cmd = std::process::Command::new("/bin/sh");

                cmd.arg("-c")
                    .arg(command.clone())
                    .env("WAYLAND_DISPLAY", &wayland_display)
                    .env("DISPLAY", &display)
                    .env("XDG_ACTIVATION_TOKEN", &*token)
                    .env("DESKTOP_STARTUP_ID", &*token)
                    .env_remove("COSMIC_SESSION_SOCK");
                unsafe { cmd.pre_exec(|| Ok(crate::utils::rlimit::restore_nofile_limit())) };

                std::thread::spawn(move || match cmd.spawn() {
                    Ok(mut child) => {
                        let _res = child.wait();
                    }
                    Err(err) => {
                        tracing::warn!(?err, "Failed to spawn \"{}\"", command);
                    }
                });
            }
        }
    }

    // TODO: Try to get rid of the *mutable* Shell references (needed for hovered_stack in floating_layout)
    pub fn surface_under(
        global_pos: Point<f64, Global>,
        output: &Output,
        shell: &mut Shell,
    ) -> Option<(PointerFocusTarget, Point<i32, Global>)> {
        let session_lock = shell.session_lock.as_ref();
        let relative_pos = global_pos.to_local(output);
        let output_geo = output.geometry();

        if let Some(session_lock) = session_lock {
            return session_lock.surfaces.get(output).map(|surface| {
                (
                    PointerFocusTarget::WlSurface {
                        surface: surface.wl_surface().clone(),
                        toplevel: None,
                    },
                    output_geo.loc,
                )
            });
        }

        if let Some(window) = shell.workspaces.active(output).1.get_fullscreen() {
            let layers = layer_map_for_output(output);
            if let Some(layer) = layers.layer_under(WlrLayer::Overlay, relative_pos.as_logical()) {
                let layer_loc = layers.layer_geometry(layer).unwrap().loc;
                if let Some((wl_surface, surface_loc)) = layer.surface_under(
                    relative_pos.as_logical() - layer_loc.to_f64(),
                    WindowSurfaceType::ALL,
                ) {
                    return Some((
                        PointerFocusTarget::WlSurface {
                            surface: wl_surface,
                            toplevel: None,
                        },
                        output_geo.loc + layer_loc.as_global() + surface_loc.as_global(),
                    ));
                }
            }
            if let Some((surface, geo)) = shell
                .override_redirect_windows
                .iter()
                .find(|or| {
                    or.is_in_input_region(
                        &(global_pos.as_logical() - X11Surface::geometry(*or).loc.to_f64()),
                    )
                })
                .and_then(|or| {
                    or.wl_surface()
                        .map(|surface| (surface, X11Surface::geometry(or).loc.as_global()))
                })
            {
                return Some((
                    PointerFocusTarget::WlSurface {
                        surface,
                        toplevel: None,
                    },
                    geo,
                ));
            }
            PointerFocusTarget::under_surface(window, relative_pos.as_logical())
                .map(|(target, surface_loc)| (target, output_geo.loc + surface_loc.as_global()))
        } else {
            {
                let layers = layer_map_for_output(output);
                if let Some(layer) = layers
                    .layer_under(WlrLayer::Overlay, relative_pos.as_logical())
                    .or_else(|| layers.layer_under(WlrLayer::Top, relative_pos.as_logical()))
                {
                    let layer_loc = layers.layer_geometry(layer).unwrap().loc;
                    if let Some((wl_surface, surface_loc)) = layer.surface_under(
                        relative_pos.as_logical() - layer_loc.to_f64(),
                        WindowSurfaceType::ALL,
                    ) {
                        return Some((
                            PointerFocusTarget::WlSurface {
                                surface: wl_surface,
                                toplevel: None,
                            },
                            output_geo.loc + layer_loc.as_global() + surface_loc.as_global(),
                        ));
                    }
                }
            }
            if let Some((surface, geo)) = shell
                .override_redirect_windows
                .iter()
                .find(|or| {
                    or.is_in_input_region(
                        &(global_pos.as_logical() - X11Surface::geometry(*or).loc.to_f64()),
                    )
                })
                .and_then(|or| {
                    or.wl_surface()
                        .map(|surface| (surface, X11Surface::geometry(or).loc.as_global()))
                })
            {
                return Some((
                    PointerFocusTarget::WlSurface {
                        surface,
                        toplevel: None,
                    },
                    geo,
                ));
            }
            if let Some((target, loc)) = shell.surface_under(global_pos, output) {
                return Some((target, loc));
            }
            {
                let layers = layer_map_for_output(output);
                if let Some(layer) = layers
                    .layer_under(WlrLayer::Bottom, relative_pos.as_logical())
                    .or_else(|| layers.layer_under(WlrLayer::Background, relative_pos.as_logical()))
                {
                    let layer_loc = layers.layer_geometry(layer).unwrap().loc;
                    if let Some((wl_surface, surface_loc)) = layer.surface_under(
                        relative_pos.as_logical() - layer_loc.to_f64(),
                        WindowSurfaceType::ALL,
                    ) {
                        return Some((
                            PointerFocusTarget::WlSurface {
                                surface: wl_surface,
                                toplevel: None,
                            },
                            output_geo.loc + layer_loc.as_global() + surface_loc.as_global(),
                        ));
                    }
                }
            }
            None
        }
    }
}

fn to_next_workspace(
    shell: &mut Shell,
    seat: &Seat<State>,
    gesture: bool,
    workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
) -> Result<Option<Point<i32, Global>>, InvalidWorkspaceIndex> {
    let current_output = seat.active_output();
    let workspace = shell
        .workspaces
        .active_num(&current_output)
        .1
        .saturating_add(1);

    shell.activate(
        &current_output,
        workspace,
        if gesture {
            WorkspaceDelta::new_gesture()
        } else {
            WorkspaceDelta::new_shortcut()
        },
        workspace_state,
    )
}

fn to_previous_workspace(
    shell: &mut Shell,
    seat: &Seat<State>,
    gesture: bool,
    workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
) -> Result<Option<Point<i32, Global>>, InvalidWorkspaceIndex> {
    let current_output = seat.active_output();
    let workspace = shell
        .workspaces
        .active_num(&current_output)
        .1
        .saturating_sub(1);

    shell.activate(
        &current_output,
        workspace,
        if gesture {
            WorkspaceDelta::new_gesture()
        } else {
            WorkspaceDelta::new_shortcut()
        },
        workspace_state,
    )
}

fn cursor_sessions_for_output(
    shell: &Shell,
    output: &Output,
) -> impl Iterator<Item = CursorSession> {
    let workspace = shell.active_space(&output);
    let maybe_fullscreen = workspace.get_fullscreen();
    workspace
        .cursor_sessions()
        .into_iter()
        .chain(
            maybe_fullscreen
                .map(|w| w.cursor_sessions())
                .into_iter()
                .flatten(),
        )
        .chain(output.cursor_sessions().into_iter())
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

// FIXME: When f64::next_down reaches stable rust, use that instead
trait NextDown {
    fn next_lower(self) -> Self;
}

impl NextDown for f64 {
    fn next_lower(self) -> Self {
        // We must use strictly integer arithmetic to prevent denormals from
        // flushing to zero after an arithmetic operation on some platforms.
        const NEG_TINY_BITS: u64 = 0x8000_0000_0000_0001; // Smallest (in magnitude) negative f64.
        const CLEAR_SIGN_MASK: u64 = 0x7fff_ffff_ffff_ffff;

        let bits = self.to_bits();
        if self.is_nan() || bits == Self::NEG_INFINITY.to_bits() {
            return self;
        }

        let abs = bits & CLEAR_SIGN_MASK;
        let next_bits = if abs == 0 {
            NEG_TINY_BITS
        } else if bits == abs {
            bits - 1
        } else {
            bits + 1
        };
        Self::from_bits(next_bits)
    }
}
