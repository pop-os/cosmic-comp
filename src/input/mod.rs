// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    config::{Action, Config},
    shell::{
        focus::{target::PointerFocusTarget, FocusDirection},
        layout::{
            floating::SeatMoveGrabState,
            tiling::{Direction, FocusResult},
        },
        Ordering, OverrideRedirectWindow, Workspace,
    }, // shell::grabs::SeatMoveGrabState
    state::Common,
    utils::prelude::*,
    wayland::{handlers::screencopy::ScreencopySessions, protocols::screencopy::Session},
};
use cosmic_protocols::screencopy::v1::server::zcosmic_screencopy_session_v1::InputType;
use smithay::{
    backend::{
        input::{
            Axis, AxisSource, Device, DeviceCapability, InputBackend, InputEvent, KeyState,
            PointerAxisEvent,
        },
        renderer::element::Id,
    },
    desktop::{layer_map_for_output, space::SpaceElement, WindowSurfaceType},
    input::{
        keyboard::{keysyms, FilterResult, KeysymHandle, XkbConfig},
        pointer::{AxisFrame, ButtonEvent, CursorImageStatus, MotionEvent},
        Seat, SeatState,
    },
    output::Output,
    reexports::wayland_server::DisplayHandle,
    utils::{Logical, Point, Rectangle, Serial, SERIAL_COUNTER},
    wayland::{
        keyboard_shortcuts_inhibit::KeyboardShortcutsInhibitorSeat, seat::WaylandFocus,
        shell::wlr_layer::Layer as WlrLayer,
    },
};

use std::{cell::RefCell, collections::HashMap};
use xkbcommon::xkb::KEY_XF86Switch_VT_12;

crate::utils::id_gen!(next_seat_id, SEAT_ID, SEAT_IDS);

#[repr(transparent)]
pub struct SeatId(pub usize);
pub struct ActiveOutput(pub RefCell<Output>);
#[derive(Default)]
pub struct SupressedKeys(RefCell<Vec<u32>>);
#[derive(Default)]
pub struct Devices(RefCell<HashMap<String, Vec<DeviceCapability>>>);

impl Default for SeatId {
    fn default() -> SeatId {
        SeatId(next_seat_id())
    }
}

impl Drop for SeatId {
    fn drop(&mut self) {
        SEAT_IDS.lock().unwrap().remove(&self.0);
    }
}

impl SupressedKeys {
    fn add(&self, keysym: &KeysymHandle) {
        self.0.borrow_mut().push(keysym.raw_code());
    }

    fn filter(&self, keysym: &KeysymHandle) -> bool {
        let mut keys = self.0.borrow_mut();
        if let Some(i) = keys.iter().position(|x| *x == keysym.raw_code()) {
            keys.remove(i);
            true
        } else {
            false
        }
    }
}

impl Devices {
    fn add_device<D: Device>(&self, device: &D) -> Vec<DeviceCapability> {
        let id = device.id();
        let mut map = self.0.borrow_mut();
        let caps = [DeviceCapability::Keyboard, DeviceCapability::Pointer]
            .iter()
            .cloned()
            .filter(|c| device.has_capability(*c))
            .collect::<Vec<_>>();
        let new_caps = caps
            .iter()
            .cloned()
            .filter(|c| map.values().flatten().all(|has| *c != *has))
            .collect::<Vec<_>>();
        map.insert(id, caps);
        new_caps
    }

    pub fn has_device<D: Device>(&self, device: &D) -> bool {
        self.0.borrow().contains_key(&device.id())
    }

    fn remove_device<D: Device>(&self, device: &D) -> Vec<DeviceCapability> {
        let id = device.id();
        let mut map = self.0.borrow_mut();
        map.remove(&id)
            .unwrap_or(Vec::new())
            .into_iter()
            .filter(|c| map.values().flatten().all(|has| *c != *has))
            .collect()
    }
}

pub fn add_seat(
    dh: &DisplayHandle,
    seat_state: &mut SeatState<State>,
    output: &Output,
    config: &Config,
    name: String,
) -> Seat<State> {
    let mut seat = seat_state.new_wl_seat(dh, name, None);
    let userdata = seat.user_data();
    userdata.insert_if_missing(SeatId::default);
    userdata.insert_if_missing(Devices::default);
    userdata.insert_if_missing(SupressedKeys::default);
    userdata.insert_if_missing(SeatMoveGrabState::default);
    userdata.insert_if_missing(|| ActiveOutput(RefCell::new(output.clone())));
    userdata.insert_if_missing(|| RefCell::new(CursorImageStatus::Default));

    // A lot of clients bind keyboard and pointer unconditionally once on launch..
    // Initial clients might race the compositor on adding periheral and
    // end up in a state, where they are not able to receive input.
    // Additionally a lot of clients don't handle keyboards/pointer objects being
    // removed very well either and we don't want to crash applications, because the
    // user is replugging their keyboard or mouse.
    //
    // So instead of doing the right thing (and initialize these capabilities as matching
    // devices appear), we have to surrender to reality and just always expose a keyboard and pointer.
    let conf = config.xkb_config();
    if let Err(err) = seat.add_keyboard((&conf).into(), 200, 25) {
        slog_scope::warn!(
            "Failed to load provided xkb config: {}. Trying default...",
            err
        );
        seat.add_keyboard(XkbConfig::default(), 200, 25)
            .expect("Failed to load xkb configuration files");
    }
    seat.add_pointer();

    seat
}

impl State {
    pub fn process_input_event<B: InputBackend>(&mut self, event: InputEvent<B>) {
        use smithay::backend::input::Event;

        match event {
            InputEvent::DeviceAdded { device } => {
                let seat = &mut self.common.last_active_seat();
                let userdata = seat.user_data();
                let devices = userdata.get::<Devices>().unwrap();
                for cap in devices.add_device(&device) {
                    match cap {
                        // TODO: Handle touch, tablet
                        _ => {}
                    }
                }
            }
            InputEvent::DeviceRemoved { device } => {
                for seat in &mut self.common.seats() {
                    let userdata = seat.user_data();
                    let devices = userdata.get::<Devices>().unwrap();
                    if devices.has_device(&device) {
                        for cap in devices.remove_device(&device) {
                            match cap {
                                // TODO: Handle touch, tablet
                                _ => {}
                            }
                        }
                        break;
                    }
                }
            }
            InputEvent::Keyboard { event, .. } => {
                use smithay::backend::input::KeyboardKeyEvent;

                let device = event.device();
                for seat in self.common.seats().cloned().collect::<Vec<_>>().iter() {
                    let current_output = seat.active_output();
                    let workspace = self.common.shell.active_space_mut(&current_output);
                    let shortcuts_inhibited = workspace
                        .focus_stack
                        .get(seat)
                        .last()
                        .and_then(|window| {
                            window.wl_surface().and_then(|surface| {
                                seat.keyboard_shortcuts_inhibitor_for_surface(&surface)
                            })
                        })
                        .map(|inhibitor| inhibitor.is_active())
                        .unwrap_or(false);

                    let userdata = seat.user_data();
                    let devices = userdata.get::<Devices>().unwrap();
                    if devices.has_device(&device) {
                        let keycode = event.key_code();
                        let state = event.state();
                        slog_scope::trace!("key"; "keycode" => keycode, "state" => format!("{:?}", state));

                        let serial = SERIAL_COUNTER.next_serial();
                        let time = Event::time(&event);
                        if let Some(action) = seat
                            .get_keyboard()
                            .unwrap()
                            .input(
                                self,
                                keycode,
                                state,
                                serial,
                                time,
                                |data, modifiers, handle| {
                                    if state == KeyState::Released
                                        && userdata.get::<SupressedKeys>().unwrap().filter(&handle)
                                    {
                                        return FilterResult::Intercept(None);
                                    }

                                    if state == KeyState::Pressed
                                        && (keysyms::KEY_XF86Switch_VT_1..=KEY_XF86Switch_VT_12)
                                            .contains(&handle.modified_sym())
                                    {
                                        if let Err(err) = data.backend.kms().switch_vt(
                                            (handle.modified_sym() - keysyms::KEY_XF86Switch_VT_1
                                                + 1)
                                                as i32,
                                        ) {
                                            slog_scope::error!(
                                                "Failed switching virtual terminal: {}",
                                                err
                                            );
                                        }
                                        userdata.get::<SupressedKeys>().unwrap().add(&handle);
                                        return FilterResult::Intercept(None);
                                    }

                                    // here we can handle global shortcuts and the like
                                    if !shortcuts_inhibited {
                                        for (binding, action) in
                                            data.common.config.static_conf.key_bindings.iter()
                                        {
                                            if state == KeyState::Pressed
                                                && binding.modifiers == *modifiers
                                                && handle.raw_syms().contains(&binding.key)
                                            {
                                                userdata
                                                    .get::<SupressedKeys>()
                                                    .unwrap()
                                                    .add(&handle);
                                                return FilterResult::Intercept(Some(
                                                    action.clone(),
                                                ));
                                            }
                                        }
                                    }

                                    FilterResult::Forward
                                },
                            )
                            .flatten()
                        {
                            self.handle_action(action, seat, serial, time)
                        }
                        break;
                    }
                }
            }
            InputEvent::PointerMotion { event, .. } => {
                use smithay::backend::input::PointerMotionEvent;

                let device = event.device();
                for seat in self.common.seats().cloned().collect::<Vec<_>>().iter() {
                    let userdata = seat.user_data();
                    let devices = userdata.get::<Devices>().unwrap();
                    if devices.has_device(&device) {
                        let current_output = seat.active_output();

                        let mut position = seat.get_pointer().unwrap().current_location();
                        position += event.delta();

                        let output = self
                            .common
                            .shell
                            .outputs()
                            .find(|output| output.geometry().to_f64().contains(position))
                            .cloned()
                            .unwrap_or(current_output.clone());
                        if output != current_output {
                            for session in sessions_for_output(&self.common, &current_output) {
                                session.cursor_leave(seat, InputType::Pointer);
                            }

                            for session in sessions_for_output(&self.common, &output) {
                                session.cursor_enter(seat, InputType::Pointer);
                            }

                            seat.set_active_output(&output);
                        }
                        let output_geometry = output.geometry();

                        position.x = 0.0f64
                            .max(position.x)
                            .min((output_geometry.loc.x + output_geometry.size.w) as f64);
                        position.y = 0.0f64
                            .max(position.y)
                            .min((output_geometry.loc.y + output_geometry.size.h) as f64);

                        let serial = SERIAL_COUNTER.next_serial();
                        let relative_pos = self.common.shell.map_global_to_space(position, &output);
                        let workspace = self.common.shell.active_space(&output);
                        let under = State::surface_under(
                            position,
                            relative_pos,
                            &output,
                            output_geometry,
                            &self.common.shell.override_redirect_windows,
                            &workspace,
                        );

                        for session in sessions_for_output(&self.common, &output) {
                            if let Some((geometry, offset)) = seat.cursor_geometry(
                                position.to_buffer(
                                    output.current_scale().fractional_scale(),
                                    output.current_transform(),
                                    &output.geometry().size.to_f64(),
                                ),
                                self.common.clock.now(),
                            ) {
                                session.cursor_info(seat, InputType::Pointer, geometry, offset);
                            }
                        }
                        seat.get_pointer().unwrap().motion(
                            self,
                            under,
                            &MotionEvent {
                                location: position,
                                serial,
                                time: event.time(),
                            },
                        );
                        break;
                    }
                }
            }
            InputEvent::PointerMotionAbsolute { event, .. } => {
                let device = event.device();
                for seat in self.common.seats().cloned().collect::<Vec<_>>().iter() {
                    let userdata = seat.user_data();
                    let devices = userdata.get::<Devices>().unwrap();
                    if devices.has_device(&device) {
                        let output = seat.active_output();
                        let geometry = output.geometry();
                        let position = geometry.loc.to_f64()
                            + smithay::backend::input::AbsolutePositionEvent::position_transformed(
                                &event,
                                geometry.size,
                            );
                        let relative_pos = self.common.shell.map_global_to_space(position, &output);
                        let workspace = self.common.shell.active_space(&output);
                        let serial = SERIAL_COUNTER.next_serial();
                        let under = State::surface_under(
                            position,
                            relative_pos,
                            &output,
                            geometry,
                            &self.common.shell.override_redirect_windows,
                            &workspace,
                        );

                        for session in sessions_for_output(&self.common, &output) {
                            if let Some((geometry, offset)) = seat.cursor_geometry(
                                position.to_buffer(
                                    output.current_scale().fractional_scale(),
                                    output.current_transform(),
                                    &output.geometry().size.to_f64(),
                                ),
                                self.common.clock.now(),
                            ) {
                                session.cursor_info(seat, InputType::Pointer, geometry, offset);
                            }
                        }
                        seat.get_pointer().unwrap().motion(
                            self,
                            under,
                            &MotionEvent {
                                location: position,
                                serial,
                                time: event.time(),
                            },
                        );

                        break;
                    }
                }
            }
            InputEvent::PointerButton { event, .. } => {
                use smithay::backend::input::{ButtonState, PointerButtonEvent};

                let device = event.device();
                for seat in self.common.seats().cloned().collect::<Vec<_>>().iter() {
                    let userdata = seat.user_data();
                    let devices = userdata.get::<Devices>().unwrap();
                    if devices.has_device(&device) {
                        let serial = SERIAL_COUNTER.next_serial();
                        let button = event.button_code();
                        if event.state() == ButtonState::Pressed {
                            // change the keyboard focus unless the pointer or keyboard is grabbed
                            // We test for any matching surface type here but always use the root
                            // (in case of a window the toplevel) surface for the focus.
                            // see: https://gitlab.freedesktop.org/wayland/wayland/-/issues/294
                            if !seat.get_pointer().unwrap().is_grabbed()
                                && !seat.get_keyboard().map(|k| k.is_grabbed()).unwrap_or(false)
                            {
                                let output = seat.active_output();
                                let pos = seat.get_pointer().unwrap().current_location();
                                let relative_pos =
                                    self.common.shell.map_global_to_space(pos, &output);
                                let workspace = self.common.shell.active_space_mut(&output);
                                let layers = layer_map_for_output(&output);
                                let mut under = None;

                                if let Some(window) = workspace.get_fullscreen(&output) {
                                    if let Some(layer) =
                                        layers.layer_under(WlrLayer::Overlay, relative_pos)
                                    {
                                        let layer_loc = layers.layer_geometry(layer).unwrap().loc;
                                        if layer.can_receive_keyboard_focus()
                                            && layer
                                                .surface_under(
                                                    relative_pos - layer_loc.to_f64(),
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
                                    if let Some(layer) = layers
                                        .layer_under(WlrLayer::Overlay, relative_pos)
                                        .or_else(|| layers.layer_under(WlrLayer::Top, relative_pos))
                                    {
                                        let layer_loc = layers.layer_geometry(layer).unwrap().loc;
                                        if layer.can_receive_keyboard_focus()
                                            && layer
                                                .surface_under(
                                                    relative_pos - layer_loc.to_f64(),
                                                    WindowSurfaceType::ALL,
                                                )
                                                .is_some()
                                        {
                                            under = Some(layer.clone().into());
                                        }
                                    } else if let Some((window, _)) =
                                        workspace.element_under(relative_pos)
                                    {
                                        under = Some(window.clone().into());
                                    } else if let Some(layer) = layers
                                        .layer_under(WlrLayer::Bottom, pos)
                                        .or_else(|| layers.layer_under(WlrLayer::Background, pos))
                                    {
                                        let layer_loc = layers.layer_geometry(layer).unwrap().loc;
                                        if layer.can_receive_keyboard_focus()
                                            && layer
                                                .surface_under(
                                                    relative_pos - layer_loc.to_f64(),
                                                    WindowSurfaceType::ALL,
                                                )
                                                .is_some()
                                        {
                                            under = Some(layer.clone().into());
                                        }
                                    };
                                }

                                Common::set_focus(self, under.as_ref(), seat, Some(serial));
                            }
                        };
                        seat.get_pointer().unwrap().button(
                            self,
                            &ButtonEvent {
                                button,
                                state: event.state(),
                                serial,
                                time: event.time(),
                            },
                        );
                        break;
                    }
                }
            }
            InputEvent::PointerAxis { event, .. } => {
                let device = event.device();
                for seat in self.common.seats().cloned().collect::<Vec<_>>().iter() {
                    let userdata = seat.user_data();
                    let devices = userdata.get::<Devices>().unwrap();
                    if devices.has_device(&device) {
                        let horizontal_amount =
                            event.amount(Axis::Horizontal).unwrap_or_else(|| {
                                event.amount_discrete(Axis::Horizontal).unwrap_or(0.0) * 3.0
                            });
                        let vertical_amount = event.amount(Axis::Vertical).unwrap_or_else(|| {
                            event.amount_discrete(Axis::Vertical).unwrap_or(0.0) * 3.0
                        });
                        let horizontal_amount_discrete = event.amount_discrete(Axis::Horizontal);
                        let vertical_amount_discrete = event.amount_discrete(Axis::Vertical);

                        {
                            let mut frame = AxisFrame::new(event.time()).source(event.source());
                            if horizontal_amount != 0.0 {
                                frame = frame.value(Axis::Horizontal, horizontal_amount);
                                if let Some(discrete) = horizontal_amount_discrete {
                                    frame = frame.discrete(Axis::Horizontal, discrete as i32);
                                }
                            } else if event.source() == AxisSource::Finger {
                                frame = frame.stop(Axis::Horizontal);
                            }
                            if vertical_amount != 0.0 {
                                frame = frame.value(Axis::Vertical, vertical_amount);
                                if let Some(discrete) = vertical_amount_discrete {
                                    frame = frame.discrete(Axis::Vertical, discrete as i32);
                                }
                            } else if event.source() == AxisSource::Finger {
                                frame = frame.stop(Axis::Vertical);
                            }
                            seat.get_pointer().unwrap().axis(self, frame);
                        }
                        break;
                    }
                }
            }
            _ => { /* TODO e.g. tablet or touch events */ }
        }
    }

    fn handle_action(&mut self, action: Action, seat: &Seat<State>, serial: Serial, time: u32) {
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
                    .workspaces
                    .spaces()
                    .flat_map(|w| w.mapped())
                {
                    mapped.set_debug(self.common.egui.active);
                }
            }
            #[cfg(not(feature = "debug"))]
            Action::Debug => {
                slog_scope::info!("Debug overlay not included in this version")
            }
            Action::Close => {
                let current_output = seat.active_output();
                let workspace = self.common.shell.active_space_mut(&current_output);
                if let Some(window) = workspace.focus_stack.get(seat).last() {
                    window.send_close();
                }
            }
            Action::Workspace(key_num) => {
                let current_output = seat.active_output();
                let workspace = match key_num {
                    0 => 9,
                    x => x - 1,
                };
                let _ = self
                    .common
                    .shell
                    .activate(&current_output, workspace as usize);
            }
            Action::NextWorkspace => {
                let current_output = seat.active_output();
                let workspace = self
                    .common
                    .shell
                    .workspaces
                    .active_num(&current_output)
                    .saturating_add(1);
                // TODO: Possibly move to next output, if idx to large
                let _ = self.common.shell.activate(&current_output, workspace);
            }
            Action::PreviousWorkspace => {
                let current_output = seat.active_output();
                let workspace = self
                    .common
                    .shell
                    .workspaces
                    .active_num(&current_output)
                    .saturating_sub(1);
                // TODO: Possibly move to prev output, if idx < 0
                let _ = self.common.shell.activate(&current_output, workspace);
            }
            Action::LastWorkspace => {
                let current_output = seat.active_output();
                let workspace = self
                    .common
                    .shell
                    .workspaces
                    .len(&current_output)
                    .saturating_sub(1);
                let _ = self.common.shell.activate(&current_output, workspace);
            }
            Action::MoveToWorkspace(key_num) => {
                let current_output = seat.active_output();
                let workspace = match key_num {
                    0 => 9,
                    x => x - 1,
                };
                Shell::move_current_window(
                    self,
                    seat,
                    &current_output,
                    (&current_output, Some(workspace as usize)),
                );
            }
            Action::MoveToNextWorkspace => {
                let current_output = seat.active_output();
                let workspace = self
                    .common
                    .shell
                    .workspaces
                    .active_num(&current_output)
                    .saturating_add(1);
                // TODO: Possibly move to next output, if idx too large
                Shell::move_current_window(
                    self,
                    seat,
                    &current_output,
                    (&current_output, Some(workspace as usize)),
                );
            }
            Action::MoveToPreviousWorkspace => {
                let current_output = seat.active_output();
                let workspace = self
                    .common
                    .shell
                    .workspaces
                    .active_num(&current_output)
                    .saturating_sub(1);
                // TODO: Possibly move to prev output, if idx < 0
                Shell::move_current_window(
                    self,
                    seat,
                    &current_output,
                    (&current_output, Some(workspace as usize)),
                );
            }
            Action::MoveToLastWorkspace => {
                let current_output = seat.active_output();
                let workspace = self
                    .common
                    .shell
                    .workspaces
                    .len(&current_output)
                    .saturating_sub(1);
                Shell::move_current_window(
                    self,
                    seat,
                    &current_output,
                    (&current_output, Some(workspace as usize)),
                );
            }
            Action::NextOutput => {
                let current_output = seat.active_output();
                if let Some(next_output) = self
                    .common
                    .shell
                    .outputs
                    .iter()
                    .skip_while(|o| *o != &current_output)
                    .skip(1)
                    .next()
                    .cloned()
                {
                    let idx = self.common.shell.workspaces.active_num(&next_output);
                    if let Some(new_pos) = self.common.shell.activate(&next_output, idx) {
                        seat.set_active_output(&next_output);
                        if let Some(ptr) = seat.get_pointer() {
                            ptr.motion(
                                self,
                                None,
                                &MotionEvent {
                                    location: new_pos.to_f64(),
                                    serial,
                                    time,
                                },
                            );
                        }
                    }
                }
            }
            Action::PreviousOutput => {
                let current_output = seat.active_output();
                if let Some(prev_output) = self
                    .common
                    .shell
                    .outputs
                    .iter()
                    .rev()
                    .skip_while(|o| *o != &current_output)
                    .skip(1)
                    .next()
                    .cloned()
                {
                    let idx = self.common.shell.workspaces.active_num(&prev_output);
                    if let Some(new_pos) = self.common.shell.activate(&prev_output, idx) {
                        seat.set_active_output(&prev_output);
                        if let Some(ptr) = seat.get_pointer() {
                            ptr.motion(
                                self,
                                None,
                                &MotionEvent {
                                    location: new_pos.to_f64(),
                                    serial,
                                    time,
                                },
                            );
                        }
                    }
                }
            }
            Action::MoveToNextOutput => {
                let current_output = seat.active_output();
                if let Some(next_output) = self
                    .common
                    .shell
                    .outputs
                    .iter()
                    .skip_while(|o| *o != &current_output)
                    .skip(1)
                    .next()
                    .cloned()
                {
                    if let Some(new_pos) = Shell::move_current_window(
                        self,
                        seat,
                        &current_output,
                        (&next_output, None),
                    ) {
                        if let Some(ptr) = seat.get_pointer() {
                            ptr.motion(
                                self,
                                None,
                                &MotionEvent {
                                    location: new_pos.to_f64(),
                                    serial,
                                    time,
                                },
                            );
                        }
                    }
                }
            }
            Action::MoveToPreviousOutput => {
                let current_output = seat.active_output();
                if let Some(prev_output) = self
                    .common
                    .shell
                    .outputs
                    .iter()
                    .rev()
                    .skip_while(|o| *o != &current_output)
                    .skip(1)
                    .next()
                    .cloned()
                {
                    if let Some(new_pos) = Shell::move_current_window(
                        self,
                        seat,
                        &current_output,
                        (&prev_output, None),
                    ) {
                        if let Some(ptr) = seat.get_pointer() {
                            ptr.motion(
                                self,
                                None,
                                &MotionEvent {
                                    location: new_pos.to_f64(),
                                    serial,
                                    time,
                                },
                            );
                        }
                    }
                }
            }
            Action::Focus(focus) => {
                let current_output = seat.active_output();
                let workspace = self.common.shell.active_space_mut(&current_output);
                let focus_stack = workspace.focus_stack.get(seat);
                match workspace
                    .tiling_layer
                    .next_focus(focus, seat, focus_stack.iter())
                {
                    FocusResult::None => {
                        // TODO: Handle Workspace orientation
                        match focus {
                            FocusDirection::Left => {
                                self.handle_action(Action::PreviousWorkspace, seat, serial, time)
                            }
                            FocusDirection::Right => {
                                self.handle_action(Action::NextWorkspace, seat, serial, time)
                            }
                            FocusDirection::Up => {
                                self.handle_action(Action::PreviousOutput, seat, serial, time)
                            }
                            FocusDirection::Down => {
                                self.handle_action(Action::NextOutput, seat, serial, time)
                            }
                            _ => {}
                        }
                    }
                    FocusResult::Handled => {}
                    FocusResult::Some(target) => {
                        std::mem::drop(focus_stack);
                        Common::set_focus(self, Some(&target), seat, None);
                    }
                }
            }
            Action::Move(direction) => {
                let current_output = seat.active_output();
                let workspace = self.common.shell.active_space_mut(&current_output);
                if let Some(_move_further) =
                    workspace.tiling_layer.move_current_window(direction, seat)
                {
                    // TODO: Handle Workspace orientation
                    // TODO: Being able to move Groups (move_further should be KeyboardFocusTarget instead)
                    match direction {
                        Direction::Left => {
                            self.handle_action(Action::MoveToPreviousWorkspace, seat, serial, time)
                        }
                        Direction::Right => {
                            self.handle_action(Action::MoveToNextWorkspace, seat, serial, time)
                        }
                        Direction::Up => {
                            self.handle_action(Action::MoveToPreviousOutput, seat, serial, time)
                        }
                        Direction::Down => {
                            self.handle_action(Action::MoveToNextOutput, seat, serial, time)
                        }
                    }
                }
            }
            Action::Maximize => {
                let current_output = seat.active_output();
                let workspace = self.common.shell.active_space_mut(&current_output);
                let focus_stack = workspace.focus_stack.get(seat);
                let focused_window = focus_stack.last();
                if let Some(window) = focused_window.map(|f| f.active_window()) {
                    workspace.maximize_toggle(&window, &current_output);
                }
            }
            Action::ToggleOrientation => {
                let output = seat.active_output();
                let workspace = self.common.shell.active_space_mut(&output);
                let focus_stack = workspace.focus_stack.get(seat);
                workspace
                    .tiling_layer
                    .update_orientation(None, &seat, focus_stack.iter());
            }
            Action::Orientation(orientation) => {
                let output = seat.active_output();
                let workspace = self.common.shell.active_space_mut(&output);
                let focus_stack = workspace.focus_stack.get(seat);
                workspace.tiling_layer.update_orientation(
                    Some(orientation),
                    &seat,
                    focus_stack.iter(),
                );
            }
            Action::ToggleTiling => {
                let output = seat.active_output();
                let workspace = self.common.shell.active_space_mut(&output);
                workspace.toggle_tiling(seat);
            }
            Action::ToggleWindowFloating => {
                let output = seat.active_output();
                let workspace = self.common.shell.active_space_mut(&output);
                workspace.toggle_floating_window(seat);
            }
            Action::Spawn(command) => {
                if let Err(err) = std::process::Command::new("/bin/sh")
                    .arg("-c")
                    .arg(command)
                    .env("WAYLAND_DISPLAY", &self.common.socket)
                    .env_remove("COSMIC_SESSION_SOCK")
                    .spawn()
                {
                    slog_scope::warn!("Failed to spawn: {}", err);
                }
            }
        }
    }

    pub fn surface_under(
        global_pos: Point<f64, Logical>,
        relative_pos: Point<f64, Logical>,
        output: &Output,
        output_geo: Rectangle<i32, Logical>,
        override_redirect_windows: &[OverrideRedirectWindow],
        workspace: &Workspace,
    ) -> Option<(PointerFocusTarget, Point<i32, Logical>)> {
        let layers = layer_map_for_output(output);
        if let Some(window) = workspace.get_fullscreen(output) {
            if let Some(layer) = layers.layer_under(WlrLayer::Overlay, relative_pos) {
                let layer_loc = layers.layer_geometry(layer).unwrap().loc;
                if layer
                    .surface_under(relative_pos - layer_loc.to_f64(), WindowSurfaceType::ALL)
                    .is_some()
                {
                    return Some((layer.clone().into(), output_geo.loc + layer_loc));
                }
            }
            Some((window.clone().into(), output_geo.loc))
        } else {
            if let Some(layer) = layers
                .layer_under(WlrLayer::Overlay, relative_pos)
                .or_else(|| layers.layer_under(WlrLayer::Top, relative_pos))
            {
                let layer_loc = layers.layer_geometry(layer).unwrap().loc;
                if layer
                    .surface_under(relative_pos - layer_loc.to_f64(), WindowSurfaceType::ALL)
                    .is_some()
                {
                    return Some((layer.clone().into(), output_geo.loc + layer_loc));
                }
            }
            if let Some(or) = override_redirect_windows.iter().find(|or| {
                or.above == Ordering::Above
                    && or
                        .surface
                        .is_in_input_region(&(global_pos - or.surface.geometry().loc.to_f64()))
            }) {
                return Some((or.surface.clone().into(), or.surface.geometry().loc));
            }
            if let Some((mapped, loc)) = workspace.element_under(relative_pos) {
                let filter = workspace
                    .mapped()
                    .skip_while(|m| *m != mapped)
                    .collect::<Vec<_>>();
                if let Some(or) = override_redirect_windows
                    .iter()
                    .filter(|or| {
                        if let Ordering::AboveWindow(w) = &or.above {
                            !filter.iter().any(|f| {
                                f.wl_surface()
                                    .map(|s| Id::from_wayland_resource(&s))
                                    .as_ref()
                                    == Some(&w)
                            })
                        } else {
                            false
                        }
                    })
                    .find(|or| {
                        or.surface
                            .is_in_input_region(&(global_pos - or.surface.geometry().loc.to_f64()))
                    })
                {
                    return Some((or.surface.clone().into(), or.surface.geometry().loc));
                }

                return Some((
                    mapped.clone().into(),
                    loc + (global_pos - relative_pos).to_i32_round(),
                ));
            }
            if let Some(or) = override_redirect_windows.iter().find(|or| {
                or.above == Ordering::Below
                    && or
                        .surface
                        .is_in_input_region(&(global_pos - or.surface.geometry().loc.to_f64()))
            }) {
                return Some((or.surface.clone().into(), or.surface.geometry().loc));
            }
            if let Some(layer) = layers
                .layer_under(WlrLayer::Bottom, relative_pos)
                .or_else(|| layers.layer_under(WlrLayer::Background, relative_pos))
            {
                let layer_loc = layers.layer_geometry(layer).unwrap().loc;
                if layer
                    .surface_under(relative_pos - layer_loc.to_f64(), WindowSurfaceType::ALL)
                    .is_some()
                {
                    return Some((layer.clone().into(), output_geo.loc + layer_loc));
                }
            }
            None
        }
    }
}

fn sessions_for_output(state: &Common, output: &Output) -> impl Iterator<Item = Session> {
    let workspace = state.shell.active_space(&output);
    let maybe_fullscreen = workspace.get_fullscreen(&output);
    workspace
        .screencopy_sessions
        .iter()
        .map(|s| (&**s).clone())
        .chain(
            maybe_fullscreen
                .and_then(|w| w.user_data().get::<ScreencopySessions>())
                .map(|sessions| {
                    sessions
                        .0
                        .borrow()
                        .iter()
                        .map(|s| (&**s).clone())
                        .collect::<Vec<_>>()
                })
                .into_iter()
                .flatten(),
        )
        .chain(
            output
                .user_data()
                .get::<ScreencopySessions>()
                .map(|sessions| {
                    sessions
                        .0
                        .borrow()
                        .iter()
                        .map(|s| (&**s).clone())
                        .collect::<Vec<_>>()
                })
                .into_iter()
                .into_iter()
                .flatten(),
        )
        .collect::<Vec<_>>()
        .into_iter()
}
