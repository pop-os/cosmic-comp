// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    config::{Action, Config, KeyModifiers, WorkspaceLayout},
    shell::{
        focus::{target::PointerFocusTarget, FocusDirection},
        layout::{
            floating::SeatMoveGrabState,
            tiling::{Direction, FocusResult},
        },
        OverviewMode, Workspace,
    }, // shell::grabs::SeatMoveGrabState
    state::Common,
    utils::prelude::*,
    wayland::{handlers::screencopy::ScreencopySessions, protocols::screencopy::Session},
};
use cosmic_protocols::screencopy::v1::server::zcosmic_screencopy_session_v1::InputType;
use smithay::{
    backend::input::{
        Axis, AxisSource, Device, DeviceCapability, InputBackend, InputEvent, KeyState,
        PointerAxisEvent,
    },
    desktop::{layer_map_for_output, space::SpaceElement, WindowSurfaceType},
    input::{
        keyboard::{keysyms, FilterResult, KeysymHandle, XkbConfig},
        pointer::{AxisFrame, ButtonEvent, CursorImageStatus, MotionEvent, RelativeMotionEvent},
        Seat, SeatState,
    },
    output::Output,
    reexports::wayland_server::DisplayHandle,
    utils::{Logical, Point, Rectangle, Serial, SERIAL_COUNTER},
    wayland::{
        keyboard_shortcuts_inhibit::KeyboardShortcutsInhibitorSeat, seat::WaylandFocus,
        shell::wlr_layer::Layer as WlrLayer,
    },
    xwayland::X11Surface,
};
#[cfg(not(feature = "debug"))]
use tracing::info;
use tracing::{error, trace, warn};

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
    let mut seat = seat_state.new_wl_seat(dh, name);
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
        warn!(
            ?err,
            "Failed to load provided xkb config. Trying default...",
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
                #[cfg(feature = "debug")]
                {
                    self.common.egui.state.handle_device_added(&device);
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
                #[cfg(feature = "debug")]
                {
                    self.common.egui.state.handle_device_removed(&device);
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
                        trace!(?keycode, ?state, "key");

                        let serial = SERIAL_COUNTER.next_serial();
                        let time = Event::time_msec(&event);
                        if let Some((action, mods)) = seat
                            .get_keyboard()
                            .unwrap()
                            .input(
                                self,
                                keycode,
                                state,
                                serial,
                                time,
                                |data, modifiers, handle| {
                                    if let OverviewMode::Started(action_modifiers, _) =
                                        data.common.shell.overview_mode()
                                    {
                                        if (action_modifiers.ctrl && !modifiers.ctrl)
                                            || (action_modifiers.alt && !modifiers.alt)
                                            || (action_modifiers.logo && !modifiers.logo)
                                            || (action_modifiers.shift && !modifiers.shift)
                                        {
                                            data.common.shell.set_overview_mode(None);
                                        }
                                    }

                                    if state == KeyState::Released
                                        && userdata.get::<SupressedKeys>().unwrap().filter(&handle)
                                    {
                                        return FilterResult::Intercept(None);
                                    }

                                    #[cfg(feature = "debug")]
                                    {
                                        if data.common.seats().position(|x| x == seat).unwrap() == 0
                                            && data.common.egui.active
                                        {
                                            if data.common.egui.state.wants_keyboard() {
                                                data.common.egui.state.handle_keyboard(
                                                    &handle,
                                                    state == KeyState::Pressed,
                                                    modifiers.clone(),
                                                );
                                                userdata
                                                    .get::<SupressedKeys>()
                                                    .unwrap()
                                                    .add(&handle);
                                                return FilterResult::Intercept(None);
                                            }
                                        }
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
                                            error!(?err, "Failed switching virtual terminal.");
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
                                                return FilterResult::Intercept(Some((
                                                    action.clone(),
                                                    binding.modifiers.clone(),
                                                )));
                                            }
                                        }
                                    }

                                    FilterResult::Forward
                                },
                            )
                            .flatten()
                        {
                            self.handle_action(action, seat, serial, time, mods, None)
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

                        position.x = (output_geometry.loc.x as f64)
                            .max(position.x)
                            .min((output_geometry.loc.x + output_geometry.size.w) as f64);
                        position.y = (output_geometry.loc.y as f64)
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
                        let ptr = seat.get_pointer().unwrap();
                        ptr.motion(
                            self,
                            under.clone(),
                            &MotionEvent {
                                location: position,
                                serial,
                                time: event.time_msec(),
                            },
                        );
                        ptr.relative_motion(
                            self,
                            under,
                            &RelativeMotionEvent {
                                delta: event.delta(),
                                delta_unaccel: event.delta_unaccel(),
                                utime: event.time(),
                            },
                        );
                        #[cfg(feature = "debug")]
                        if self.common.seats().position(|x| x == seat).unwrap() == 0 {
                            let location = if let Some(output) = self.common.shell.outputs.first() {
                                self.common
                                    .shell
                                    .map_global_to_space(position, output)
                                    .to_i32_round()
                            } else {
                                position.to_i32_round()
                            };
                            self.common.egui.state.handle_pointer_motion(location);
                        }
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
                                time: event.time_msec(),
                            },
                        );
                        #[cfg(feature = "debug")]
                        if self.common.seats().position(|x| x == seat).unwrap() == 0 {
                            let location = if let Some(output) = self.common.shell.outputs.first() {
                                self.common
                                    .shell
                                    .map_global_to_space(position, output)
                                    .to_i32_round()
                            } else {
                                position.to_i32_round()
                            };
                            self.common.egui.state.handle_pointer_motion(location);
                        }

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
                        #[cfg(feature = "debug")]
                        if self.common.seats().position(|x| x == seat).unwrap() == 0
                            && self.common.egui.active
                        {
                            if self.common.egui.state.wants_pointer() {
                                if let Some(button) = event.button() {
                                    self.common.egui.state.handle_pointer_button(
                                        button,
                                        event.state() == ButtonState::Pressed,
                                    );
                                }
                                break;
                            }
                        }

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
                                time: event.time_msec(),
                            },
                        );
                        break;
                    }
                }
            }
            InputEvent::PointerAxis { event, .. } => {
                let device = event.device();
                for seat in self.common.seats().cloned().collect::<Vec<_>>().iter() {
                    #[cfg(feature = "debug")]
                    if self.common.seats().position(|x| x == seat).unwrap() == 0
                        && self.common.egui.active
                    {
                        if self.common.egui.state.wants_pointer() {
                            self.common.egui.state.handle_pointer_axis(
                                event
                                    .amount_discrete(Axis::Horizontal)
                                    .or_else(|| event.amount(Axis::Horizontal).map(|x| x * 3.0))
                                    .unwrap_or(0.0),
                                event
                                    .amount_discrete(Axis::Vertical)
                                    .or_else(|| event.amount(Axis::Vertical).map(|x| x * 3.0))
                                    .unwrap_or(0.0),
                            );
                            break;
                        }
                    }

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
                            let mut frame =
                                AxisFrame::new(event.time_msec()).source(event.source());
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

    fn handle_action(
        &mut self,
        action: Action,
        seat: &Seat<State>,
        serial: Serial,
        time: u32,
        mods: KeyModifiers,
        direction: Option<Direction>,
    ) {
        match action {
            Action::Terminate => {
                self.common.should_stop = true;
            }
            #[cfg(feature = "debug")]
            Action::Debug => {
                self.common.egui.active = !self.common.egui.active;
                puffin::set_scopes_on(self.common.egui.active);
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
                info!("Debug overlay not included in this build.")
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
                    .1
                    .saturating_add(1);
                if self
                    .common
                    .shell
                    .activate(&current_output, workspace)
                    .is_err()
                {
                    self.handle_action(Action::NextOutput, seat, serial, time, mods, direction);
                }
            }
            Action::PreviousWorkspace => {
                let current_output = seat.active_output();
                let workspace = self
                    .common
                    .shell
                    .workspaces
                    .active_num(&current_output)
                    .1
                    .saturating_sub(1);
                if self
                    .common
                    .shell
                    .activate(&current_output, workspace)
                    .is_err()
                {
                    self.handle_action(Action::PreviousOutput, seat, serial, time, mods, direction);
                }
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
            x @ Action::MoveToWorkspace(_) | x @ Action::SendToWorkspace(_) => {
                let current_output = seat.active_output();
                let follow = matches!(x, Action::MoveToWorkspace(_));
                let workspace = match x {
                    Action::MoveToWorkspace(0) | Action::SendToWorkspace(0) => 9,
                    Action::MoveToWorkspace(x) | Action::SendToWorkspace(x) => x - 1,
                    _ => unreachable!(),
                };
                let _ = Shell::move_current_window(
                    self,
                    seat,
                    &current_output,
                    (&current_output, Some(workspace as usize)),
                    follow,
                    None,
                );
            }
            x @ Action::MoveToNextWorkspace | x @ Action::SendToNextWorkspace => {
                let current_output = seat.active_output();
                let workspace = self
                    .common
                    .shell
                    .workspaces
                    .active_num(&current_output)
                    .1
                    .saturating_add(1);
                if Shell::move_current_window(
                    self,
                    seat,
                    &current_output,
                    (&current_output, Some(workspace as usize)),
                    matches!(x, Action::MoveToNextWorkspace),
                    direction,
                )
                .is_err()
                {
                    self.handle_action(
                        if matches!(x, Action::MoveToNextWorkspace) {
                            Action::MoveToNextOutput
                        } else {
                            Action::SendToNextOutput
                        },
                        seat,
                        serial,
                        time,
                        mods,
                        direction,
                    )
                }
            }
            x @ Action::MoveToPreviousWorkspace | x @ Action::SendToPreviousWorkspace => {
                let current_output = seat.active_output();
                let workspace = self
                    .common
                    .shell
                    .workspaces
                    .active_num(&current_output)
                    .1
                    .saturating_sub(1);
                // TODO: Possibly move to prev output, if idx < 0
                if Shell::move_current_window(
                    self,
                    seat,
                    &current_output,
                    (&current_output, Some(workspace as usize)),
                    matches!(x, Action::MoveToPreviousWorkspace),
                    direction,
                )
                .is_err()
                {
                    self.handle_action(
                        if matches!(x, Action::MoveToNextWorkspace) {
                            Action::MoveToPreviousOutput
                        } else {
                            Action::SendToPreviousOutput
                        },
                        seat,
                        serial,
                        time,
                        mods,
                        direction,
                    )
                }
            }
            x @ Action::MoveToLastWorkspace | x @ Action::SendToLastWorkspace => {
                let current_output = seat.active_output();
                let workspace = self
                    .common
                    .shell
                    .workspaces
                    .len(&current_output)
                    .saturating_sub(1);
                let _ = Shell::move_current_window(
                    self,
                    seat,
                    &current_output,
                    (&current_output, Some(workspace as usize)),
                    matches!(x, Action::MoveToLastWorkspace),
                    None,
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
                    let idx = self.common.shell.workspaces.active_num(&next_output).1;
                    if let Ok(Some(new_pos)) = self.common.shell.activate(&next_output, idx) {
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
                    let idx = self.common.shell.workspaces.active_num(&prev_output).1;
                    if let Ok(Some(new_pos)) = self.common.shell.activate(&prev_output, idx) {
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
            x @ Action::MoveToNextOutput | x @ Action::SendToNextOutput => {
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
                    if let Ok(Some(new_pos)) = Shell::move_current_window(
                        self,
                        seat,
                        &current_output,
                        (&next_output, None),
                        matches!(x, Action::MoveToNextOutput),
                        direction,
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
            x @ Action::MoveToPreviousOutput | x @ Action::SendToPreviousOutput => {
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
                    if let Ok(Some(new_pos)) = Shell::move_current_window(
                        self,
                        seat,
                        &current_output,
                        (&prev_output, None),
                        matches!(x, Action::MoveToPreviousOutput),
                        direction,
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
                let mut result = workspace
                    .tiling_layer
                    .next_focus(focus, seat, focus_stack.iter());
                if workspace.get_fullscreen(&current_output).is_some() {
                    result = FocusResult::None;
                }

                match result {
                    FocusResult::None => {
                        match (focus, self.common.config.static_conf.workspace_layout) {
                            (FocusDirection::Left, WorkspaceLayout::Horizontal)
                            | (FocusDirection::Up, WorkspaceLayout::Vertical) => self
                                .handle_action(
                                    Action::PreviousWorkspace,
                                    seat,
                                    serial,
                                    time,
                                    mods,
                                    direction,
                                ),
                            (FocusDirection::Right, WorkspaceLayout::Horizontal)
                            | (FocusDirection::Down, WorkspaceLayout::Vertical) => self
                                .handle_action(
                                    Action::NextWorkspace,
                                    seat,
                                    serial,
                                    time,
                                    mods,
                                    direction,
                                ),
                            (FocusDirection::Left, WorkspaceLayout::Vertical)
                            | (FocusDirection::Up, WorkspaceLayout::Horizontal) => self
                                .handle_action(
                                    Action::PreviousOutput,
                                    seat,
                                    serial,
                                    time,
                                    mods,
                                    direction,
                                ),
                            (FocusDirection::Right, WorkspaceLayout::Vertical)
                            | (FocusDirection::Down, WorkspaceLayout::Horizontal) => self
                                .handle_action(
                                    Action::NextOutput,
                                    seat,
                                    serial,
                                    time,
                                    mods,
                                    direction,
                                ),
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
                if workspace.get_fullscreen(&current_output).is_some() {
                    return; // TODO, is this what we want? How do we indicate the switch?
                }

                if let Some(_move_further) =
                    workspace.tiling_layer.move_current_node(direction, seat)
                {
                    // TODO: Being able to move Groups (move_further should be KeyboardFocusTarget instead)
                    match (direction, self.common.config.static_conf.workspace_layout) {
                        (Direction::Left, WorkspaceLayout::Horizontal)
                        | (Direction::Up, WorkspaceLayout::Vertical) => self.handle_action(
                            Action::MoveToPreviousWorkspace,
                            seat,
                            serial,
                            time,
                            mods,
                            Some(direction),
                        ),
                        (Direction::Right, WorkspaceLayout::Horizontal)
                        | (Direction::Down, WorkspaceLayout::Vertical) => self.handle_action(
                            Action::MoveToNextWorkspace,
                            seat,
                            serial,
                            time,
                            mods,
                            Some(direction),
                        ),
                        (Direction::Left, WorkspaceLayout::Vertical)
                        | (Direction::Up, WorkspaceLayout::Horizontal) => self.handle_action(
                            Action::MoveToPreviousOutput,
                            seat,
                            serial,
                            time,
                            mods,
                            Some(direction),
                        ),
                        (Direction::Right, WorkspaceLayout::Vertical)
                        | (Direction::Down, WorkspaceLayout::Horizontal) => self.handle_action(
                            Action::MoveToNextOutput,
                            seat,
                            serial,
                            time,
                            mods,
                            Some(direction),
                        ),
                    }
                } else {
                    let focus_stack = workspace.focus_stack.get(seat);
                    if let Some(focused_window) = focus_stack.last() {
                        if workspace.is_tiled(focused_window) {
                            self.common.shell.set_overview_mode(Some(mods));
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
                workspace.tiling_layer.update_orientation(None, &seat);
            }
            Action::Orientation(orientation) => {
                let output = seat.active_output();
                let workspace = self.common.shell.active_space_mut(&output);
                workspace
                    .tiling_layer
                    .update_orientation(Some(orientation), &seat);
            }
            Action::ToggleStacking => {
                let output = seat.active_output();
                let workspace = self.common.shell.active_space_mut(&output);
                let focus_stack = workspace.focus_stack.get_mut(seat);
                workspace.tiling_layer.toggle_stacking(seat, focus_stack);
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
                let wayland_display = self.common.socket.clone();

                let display = self
                    .common
                    .xwayland_state
                    .as_ref()
                    .map(|s| format!(":{}", s.display))
                    .unwrap_or_default();

                std::thread::spawn(move || {
                    let mut cmd = std::process::Command::new("/bin/sh");

                    cmd.arg("-c")
                        .arg(command.clone())
                        .env("WAYLAND_DISPLAY", &wayland_display)
                        .env("DISPLAY", &display)
                        .env_remove("COSMIC_SESSION_SOCK");

                    match cmd.spawn() {
                        Ok(mut child) => {
                            let _res = child.wait();
                        }
                        Err(err) => {
                            tracing::warn!(?err, "Failed to spawn \"{}\"", command);
                        }
                    }
                });
            }
        }
    }

    pub fn surface_under(
        global_pos: Point<f64, Logical>,
        relative_pos: Point<f64, Logical>,
        output: &Output,
        output_geo: Rectangle<i32, Logical>,
        override_redirect_windows: &[X11Surface],
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
            if let Some(or) = override_redirect_windows
                .iter()
                .find(|or| or.is_in_input_region(&(global_pos - or.geometry().loc.to_f64())))
            {
                return Some((or.clone().into(), or.geometry().loc));
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
            if let Some(or) = override_redirect_windows
                .iter()
                .find(|or| or.is_in_input_region(&(global_pos - or.geometry().loc.to_f64())))
            {
                return Some((or.clone().into(), or.geometry().loc));
            }
            if let Some((mapped, loc)) = workspace.element_under(relative_pos) {
                return Some((
                    mapped.clone().into(),
                    loc + (global_pos - relative_pos).to_i32_round(),
                ));
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
