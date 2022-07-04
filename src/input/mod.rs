// SPDX-License-Identifier: GPL-3.0-only

use crate::{config::Action, shell::Workspace, utils::prelude::*};
use smithay::{
    backend::input::{Device, DeviceCapability, InputBackend, InputEvent, KeyState},
    desktop::{layer_map_for_output, Kind, WindowSurfaceType},
    reexports::wayland_server::{protocol::wl_surface::WlSurface, DisplayHandle, Resource},
    utils::{Logical, Point, Rectangle},
    wayland::{
        data_device::set_data_device_focus,
        output::Output,
        seat::{
            keysyms, ButtonEvent, CursorImageStatus, FilterResult, KeysymHandle, MotionEvent, Seat,
            XkbConfig,
        },
        shell::wlr_layer::Layer as WlrLayer,
        SERIAL_COUNTER,
    },
};
use std::{cell::RefCell, collections::HashMap};
use xkbcommon::xkb::KEY_XF86Switch_VT_12;

pub struct ActiveOutput(pub RefCell<Output>);
pub struct SupressedKeys(RefCell<Vec<u32>>);
pub struct Devices(RefCell<HashMap<String, Vec<DeviceCapability>>>);

impl SupressedKeys {
    fn new() -> SupressedKeys {
        SupressedKeys(RefCell::new(Vec::new()))
    }

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
    fn new() -> Devices {
        Devices(RefCell::new(HashMap::new()))
    }

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

pub fn add_seat(dh: &DisplayHandle, name: String) -> Seat<State> {
    let seat = Seat::<State>::new(dh, name, None);
    let userdata = seat.user_data();
    userdata.insert_if_missing(|| Devices::new());
    userdata.insert_if_missing(|| SupressedKeys::new());
    userdata.insert_if_missing(|| RefCell::new(CursorImageStatus::Default));
    seat
}

impl State {
    pub fn process_input_event<B: InputBackend>(
        &mut self,
        dh: &DisplayHandle,
        event: InputEvent<B>,
    ) {
        use smithay::backend::input::Event;

        match event {
            InputEvent::DeviceAdded { device } => {
                let seat = &mut self.common.last_active_seat;
                let userdata = seat.user_data();
                let devices = userdata.get::<Devices>().unwrap();
                for cap in devices.add_device(&device) {
                    match cap {
                        DeviceCapability::Keyboard => {
                            let dh_clone = dh.clone();
                            let _ = seat.add_keyboard(
                                XkbConfig::default(),
                                200,
                                25,
                                move |seat, focus| {
                                    if let Some(client) =
                                        focus.and_then(|s| dh_clone.get_client(s.id()).ok())
                                    {
                                        set_data_device_focus(&dh_clone, seat, Some(client))
                                    }
                                },
                            );
                        }
                        DeviceCapability::Pointer => {
                            let output = self
                                .common
                                .shell
                                .outputs()
                                .next()
                                .expect("Backend initialized without output")
                                .clone();
                            seat.user_data()
                                .insert_if_missing(|| ActiveOutput(RefCell::new(output)));
                            let owned_seat = seat.clone();
                            seat.add_pointer(move |status| {
                                *owned_seat
                                    .user_data()
                                    .get::<RefCell<CursorImageStatus>>()
                                    .unwrap()
                                    .borrow_mut() = status;
                            });
                        }
                        _ => {}
                    }
                }
                #[cfg(feature = "debug")]
                {
                    self.common.egui.debug_state.handle_device_added(&device);
                    self.common.egui.log_state.handle_device_added(&device);
                }
            }
            InputEvent::DeviceRemoved { device } => {
                for seat in &mut self.common.seats {
                    let userdata = seat.user_data();
                    let devices = userdata.get::<Devices>().unwrap();
                    if devices.has_device(&device) {
                        for cap in devices.remove_device(&device) {
                            match cap {
                                DeviceCapability::Keyboard => {
                                    seat.remove_keyboard();
                                }
                                DeviceCapability::Pointer => {
                                    seat.remove_pointer();
                                }
                                _ => {}
                            }
                        }
                        break;
                    }
                }
                #[cfg(feature = "debug")]
                {
                    self.common.egui.debug_state.handle_device_added(&device);
                    self.common.egui.log_state.handle_device_added(&device);
                }
            }
            InputEvent::Keyboard { event, .. } => {
                use smithay::backend::input::KeyboardKeyEvent;

                let device = event.device();
                for seat in self.common.seats.clone().iter() {
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
                            .input(dh, keycode, state, serial, time, |modifiers, handle| {
                                if state == KeyState::Released
                                    && userdata.get::<SupressedKeys>().unwrap().filter(&handle)
                                {
                                    return FilterResult::Intercept(None);
                                }

                                #[cfg(feature = "debug")]
                                {
                                    if self.common.seats.iter().position(|x| x == seat).unwrap()
                                        == 0
                                        && self.common.egui.active
                                    {
                                        if self.common.egui.debug_state.wants_keyboard() {
                                            self.common.egui.debug_state.handle_keyboard(
                                                &handle,
                                                state == KeyState::Pressed,
                                                modifiers.clone(),
                                            );
                                            userdata.get::<SupressedKeys>().unwrap().add(&handle);
                                            return FilterResult::Intercept(None);
                                        }
                                        if self.common.egui.log_state.wants_keyboard() {
                                            self.common.egui.log_state.handle_keyboard(
                                                &handle,
                                                state == KeyState::Pressed,
                                                modifiers.clone(),
                                            );
                                            userdata.get::<SupressedKeys>().unwrap().add(&handle);
                                            return FilterResult::Intercept(None);
                                        }
                                    }
                                }

                                if state == KeyState::Pressed
                                    && (keysyms::KEY_XF86Switch_VT_1..=KEY_XF86Switch_VT_12)
                                        .contains(&handle.modified_sym())
                                {
                                    if let Err(err) = self.backend.kms().switch_vt(
                                        (handle.modified_sym() - keysyms::KEY_XF86Switch_VT_1 + 1)
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
                                for (binding, action) in
                                    self.common.config.static_conf.key_bindings.iter()
                                {
                                    if state == KeyState::Pressed
                                        && binding.modifiers == *modifiers
                                        && handle.raw_syms().contains(&binding.key)
                                    {
                                        userdata.get::<SupressedKeys>().unwrap().add(&handle);
                                        return FilterResult::Intercept(Some(action));
                                    }
                                }

                                FilterResult::Forward
                            })
                            .flatten()
                        {
                            match action {
                                Action::Terminate => {
                                    self.common.should_stop = true;
                                }
                                #[cfg(feature = "debug")]
                                Action::Debug => {
                                    self.common.egui.active = !self.common.egui.active;
                                }
                                #[cfg(not(feature = "debug"))]
                                Action::Debug => {
                                    slog_scope::info!("Debug overlay not included in this version")
                                }
                                Action::Close => {
                                    let current_output = active_output(seat, &self.common);
                                    let workspace =
                                        self.common.shell.active_space_mut(&current_output);
                                    if let Some(window) = workspace.focus_stack(seat).last() {
                                        #[allow(irrefutable_let_patterns)]
                                        if let Kind::Xdg(xdg) = &window.toplevel() {
                                            xdg.send_close();
                                        }
                                    }
                                }
                                Action::Workspace(key_num) => {
                                    let current_output = active_output(seat, &self.common);
                                    let workspace = match key_num {
                                        0 => 9,
                                        x => x - 1,
                                    };
                                    self.common.shell.activate(
                                        seat,
                                        &current_output,
                                        workspace as usize,
                                    );
                                }
                                Action::MoveToWorkspace(key_num) => {
                                    let current_output = active_output(seat, &self.common);
                                    let workspace = match key_num {
                                        0 => 9,
                                        x => x - 1,
                                    };
                                    self.common.shell.move_current_window(
                                        seat,
                                        &current_output,
                                        workspace as usize,
                                    );
                                }
                                Action::Focus(focus) => {
                                    let current_output = active_output(seat, &self.common);
                                    let workspace =
                                        self.common.shell.active_space_mut(&current_output);
                                    let focus_stack = workspace.focus_stack(seat);
                                    if let Some(window) = workspace.tiling_layer.move_focus(
                                        *focus,
                                        seat,
                                        &mut workspace.space,
                                        focus_stack.iter(),
                                    ) {
                                        self.common.set_focus(
                                            dh,
                                            Some(window.toplevel().wl_surface()),
                                            seat,
                                            None,
                                        );
                                    }
                                }
                                Action::Fullscreen => {
                                    let current_output = active_output(seat, &self.common);
                                    let workspace =
                                        self.common.shell.active_space_mut(&current_output);
                                    let focused_window = workspace.focus_stack(seat).last();
                                    if let Some(window) = focused_window {
                                        workspace.fullscreen_toggle(&window, &current_output);
                                    }
                                }
                                Action::Orientation(orientation) => {
                                    let output = active_output(seat, &self.common);
                                    let workspace = self.common.shell.active_space_mut(&output);
                                    let focus_stack = workspace.focus_stack(seat);
                                    workspace.tiling_layer.update_orientation(
                                        *orientation,
                                        &seat,
                                        &mut workspace.space,
                                        focus_stack.iter(),
                                    );
                                }
                                Action::Spawn(command) => {
                                    if let Err(err) = std::process::Command::new("/bin/sh")
                                        .arg("-c")
                                        .arg(command)
                                        .env("WAYLAND_DISPLAY", &self.common.socket)
                                        .spawn()
                                    {
                                        slog_scope::warn!("Failed to spawn: {}", err);
                                    }
                                }
                            }
                        }
                        break;
                    }
                }
            }
            InputEvent::PointerMotion { event, .. } => {
                use smithay::backend::input::PointerMotionEvent;

                let device = event.device();
                for seat in self.common.seats.clone().iter() {
                    let userdata = seat.user_data();
                    let devices = userdata.get::<Devices>().unwrap();
                    if devices.has_device(&device) {
                        let current_output = active_output(seat, &self.common);

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
                            set_active_output(seat, &output);
                        }
                        let output_geometry = output.geometry();

                        position.x = 0.0f64
                            .max(position.x)
                            .min((output_geometry.loc.x + output_geometry.size.w) as f64);
                        position.y = 0.0f64
                            .max(position.y)
                            .min((output_geometry.loc.y + output_geometry.size.h) as f64);

                        let serial = SERIAL_COUNTER.next_serial();
                        let relative_pos = self
                            .common
                            .shell
                            .space_relative_output_geometry(position, &output);
                        let workspace = self.common.shell.active_space_mut(&output);
                        let under = State::surface_under(
                            position,
                            relative_pos,
                            &output,
                            output_geometry,
                            &workspace,
                        );
                        seat.get_pointer().unwrap().motion(
                            self,
                            dh,
                            &MotionEvent {
                                location: position,
                                focus: under,
                                serial,
                                time: event.time(),
                            },
                        );

                        #[cfg(feature = "debug")]
                        if self.common.seats.iter().position(|x| x == seat).unwrap() == 0 {
                            self.common
                                .egui
                                .debug_state
                                .handle_pointer_motion(position.to_i32_round());
                            self.common
                                .egui
                                .log_state
                                .handle_pointer_motion(position.to_i32_round());
                        }
                        break;
                    }
                }
            }
            InputEvent::PointerMotionAbsolute { event, .. } => {
                use smithay::backend::input::PointerMotionAbsoluteEvent;

                let device = event.device();
                for seat in self.common.seats.clone().iter() {
                    let userdata = seat.user_data();
                    let devices = userdata.get::<Devices>().unwrap();
                    if devices.has_device(&device) {
                        let output = active_output(seat, &self.common);
                        let geometry = output.geometry();
                        let position =
                            geometry.loc.to_f64() + event.position_transformed(geometry.size);
                        let relative_pos = self
                            .common
                            .shell
                            .space_relative_output_geometry(position, &output);
                        let workspace = self.common.shell.active_space_mut(&output);
                        let serial = SERIAL_COUNTER.next_serial();
                        let under = State::surface_under(
                            position,
                            relative_pos,
                            &output,
                            geometry,
                            &workspace,
                        );
                        seat.get_pointer().unwrap().motion(
                            self,
                            dh,
                            &MotionEvent {
                                location: position,
                                focus: under,
                                serial,
                                time: event.time(),
                            },
                        );

                        #[cfg(feature = "debug")]
                        if self.common.seats.iter().position(|x| x == seat).unwrap() == 0 {
                            self.common
                                .egui
                                .debug_state
                                .handle_pointer_motion(position.to_i32_round());
                            self.common
                                .egui
                                .log_state
                                .handle_pointer_motion(position.to_i32_round());
                        }
                        break;
                    }
                }
            }
            InputEvent::PointerButton { event, .. } => {
                use smithay::{
                    backend::input::{ButtonState, PointerButtonEvent},
                    reexports::wayland_server::protocol::wl_pointer,
                };

                let device = event.device();
                for seat in self.common.seats.clone().iter() {
                    let userdata = seat.user_data();
                    let devices = userdata.get::<Devices>().unwrap();
                    if devices.has_device(&device) {
                        #[cfg(feature = "debug")]
                        if self.common.seats.iter().position(|x| x == seat).unwrap() == 0
                            && self.common.egui.active
                        {
                            if self.common.egui.debug_state.wants_pointer() {
                                if let Some(button) = event.button() {
                                    self.common.egui.debug_state.handle_pointer_button(
                                        button,
                                        event.state() == ButtonState::Pressed,
                                        self.common.egui.modifiers.clone(),
                                    );
                                }
                                break;
                            }
                            if self.common.egui.log_state.wants_pointer() {
                                if let Some(button) = event.button() {
                                    self.common.egui.log_state.handle_pointer_button(
                                        button,
                                        event.state() == ButtonState::Pressed,
                                        self.common.egui.modifiers.clone(),
                                    );
                                }
                                break;
                            }
                        }

                        let serial = SERIAL_COUNTER.next_serial();
                        let button = event.button_code();
                        let state = match event.state() {
                            ButtonState::Pressed => {
                                // change the keyboard focus unless the pointer or keyboard is grabbed
                                // We test for any matching surface type here but always use the root
                                // (in case of a window the toplevel) surface for the focus.
                                // see: https://gitlab.freedesktop.org/wayland/wayland/-/issues/294
                                if !seat.get_pointer().unwrap().is_grabbed()
                                    && !seat.get_keyboard().map(|k| k.is_grabbed()).unwrap_or(false)
                                {
                                    let output = active_output(seat, &self.common);
                                    let pos = seat.get_pointer().unwrap().current_location();
                                    let output_geo = output.geometry();
                                    let relative_pos = self
                                        .common
                                        .shell
                                        .space_relative_output_geometry(pos, &output);
                                    let workspace = self.common.shell.active_space_mut(&output);
                                    let layers = layer_map_for_output(&output);
                                    let mut under = None;

                                    if let Some(window) = workspace.get_fullscreen(&output) {
                                        if let Some(layer) =
                                            layers.layer_under(WlrLayer::Overlay, relative_pos)
                                        {
                                            if layer.can_receive_keyboard_focus() {
                                                let layer_loc =
                                                    layers.layer_geometry(layer).unwrap().loc;
                                                under = layer
                                                    .surface_under(
                                                        pos - output_geo.loc.to_f64()
                                                            - layer_loc.to_f64(),
                                                        WindowSurfaceType::ALL,
                                                    )
                                                    .map(|(_, _)| layer.wl_surface().clone());
                                            }
                                        } else {
                                            under = window
                                                .surface_under(
                                                    pos - output_geo.loc.to_f64(),
                                                    WindowSurfaceType::ALL,
                                                )
                                                .map(|(_, _)| {
                                                    window.toplevel().wl_surface().clone()
                                                });
                                        }
                                    } else {
                                        if let Some(layer) = layers
                                            .layer_under(WlrLayer::Overlay, relative_pos)
                                            .or_else(|| {
                                                layers.layer_under(WlrLayer::Top, relative_pos)
                                            })
                                        {
                                            if layer.can_receive_keyboard_focus() {
                                                let layer_loc =
                                                    layers.layer_geometry(layer).unwrap().loc;
                                                under = layer
                                                    .surface_under(
                                                        pos - output_geo.loc.to_f64()
                                                            - layer_loc.to_f64(),
                                                        WindowSurfaceType::ALL,
                                                    )
                                                    .map(|(_, _)| layer.wl_surface().clone());
                                            }
                                        } else if let Some((window, _, _)) = workspace
                                            .space
                                            .surface_under(relative_pos, WindowSurfaceType::ALL)
                                        {
                                            under = Some(window.toplevel().wl_surface().clone());
                                        } else if let Some(layer) =
                                            layers.layer_under(WlrLayer::Bottom, pos).or_else(
                                                || layers.layer_under(WlrLayer::Background, pos),
                                            )
                                        {
                                            if layer.can_receive_keyboard_focus() {
                                                let layer_loc =
                                                    layers.layer_geometry(layer).unwrap().loc;
                                                under = layer
                                                    .surface_under(
                                                        pos - output_geo.loc.to_f64()
                                                            - layer_loc.to_f64(),
                                                        WindowSurfaceType::ALL,
                                                    )
                                                    .map(|(_, _)| layer.wl_surface().clone());
                                            }
                                        };
                                    }

                                    self.common
                                        .set_focus(dh, under.as_ref(), seat, Some(serial));
                                }
                                wl_pointer::ButtonState::Pressed
                            }
                            ButtonState::Released => wl_pointer::ButtonState::Released,
                        };
                        seat.get_pointer().unwrap().button(
                            self,
                            dh,
                            &ButtonEvent {
                                button,
                                state,
                                serial,
                                time: event.time(),
                            },
                        );
                        break;
                    }
                }
            }
            InputEvent::PointerAxis { event, .. } => {
                use smithay::{
                    backend::input::{Axis, AxisSource, PointerAxisEvent},
                    reexports::wayland_server::protocol::wl_pointer,
                    wayland::seat::AxisFrame,
                };

                let device = event.device();
                for seat in self.common.seats.clone().iter() {
                    #[cfg(feature = "debug")]
                    if self.common.seats.iter().position(|x| x == seat).unwrap() == 0
                        && self.common.egui.active
                    {
                        if self.common.egui.debug_state.wants_pointer() {
                            self.common.egui.debug_state.handle_pointer_axis(
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
                        if self.common.egui.log_state.wants_pointer() {
                            self.common.egui.log_state.handle_pointer_axis(
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
                        let source = match event.source() {
                            AxisSource::Continuous => wl_pointer::AxisSource::Continuous,
                            AxisSource::Finger => wl_pointer::AxisSource::Finger,
                            AxisSource::Wheel | AxisSource::WheelTilt => {
                                wl_pointer::AxisSource::Wheel
                            }
                        };
                        let horizontal_amount =
                            event.amount(Axis::Horizontal).unwrap_or_else(|| {
                                event.amount_discrete(Axis::Horizontal).unwrap() * 3.0
                            });
                        let vertical_amount = event.amount(Axis::Vertical).unwrap_or_else(|| {
                            event.amount_discrete(Axis::Vertical).unwrap() * 3.0
                        });
                        let horizontal_amount_discrete = event.amount_discrete(Axis::Horizontal);
                        let vertical_amount_discrete = event.amount_discrete(Axis::Vertical);

                        {
                            let mut frame = AxisFrame::new(event.time()).source(source);
                            if horizontal_amount != 0.0 {
                                frame = frame
                                    .value(wl_pointer::Axis::HorizontalScroll, horizontal_amount);
                                if let Some(discrete) = horizontal_amount_discrete {
                                    frame = frame.discrete(
                                        wl_pointer::Axis::HorizontalScroll,
                                        discrete as i32,
                                    );
                                }
                            } else if source == wl_pointer::AxisSource::Finger {
                                frame = frame.stop(wl_pointer::Axis::HorizontalScroll);
                            }
                            if vertical_amount != 0.0 {
                                frame =
                                    frame.value(wl_pointer::Axis::VerticalScroll, vertical_amount);
                                if let Some(discrete) = vertical_amount_discrete {
                                    frame = frame.discrete(
                                        wl_pointer::Axis::VerticalScroll,
                                        discrete as i32,
                                    );
                                }
                            } else if source == wl_pointer::AxisSource::Finger {
                                frame = frame.stop(wl_pointer::Axis::VerticalScroll);
                            }
                            seat.get_pointer().unwrap().axis(self, dh, frame);
                        }
                        break;
                    }
                }
            }
            _ => { /* TODO e.g. tablet or touch events */ }
        }
    }

    pub fn surface_under(
        global_pos: Point<f64, Logical>,
        relative_pos: Point<f64, Logical>,
        output: &Output,
        output_geo: Rectangle<i32, Logical>,
        workspace: &Workspace,
    ) -> Option<(WlSurface, Point<i32, Logical>)> {
        let layers = layer_map_for_output(output);
        if let Some(window) = workspace.get_fullscreen(output) {
            if let Some(layer) = layers
                .layer_under(WlrLayer::Overlay, relative_pos)
                .or_else(|| layers.layer_under(WlrLayer::Top, relative_pos))
            {
                let layer_loc = layers.layer_geometry(layer).unwrap().loc;
                layer
                    .surface_under(
                        global_pos - output_geo.loc.to_f64() - layer_loc.to_f64(),
                        WindowSurfaceType::ALL,
                    )
                    .map(|(s, loc)| (s, loc + layer_loc + output_geo.loc))
            } else {
                window
                    .surface_under(global_pos - output_geo.loc.to_f64(), WindowSurfaceType::ALL)
                    .map(|(s, loc)| (s, loc + output_geo.loc))
            }
        } else {
            if let Some(layer) = layers
                .layer_under(WlrLayer::Overlay, relative_pos)
                .or_else(|| layers.layer_under(WlrLayer::Top, relative_pos))
            {
                let layer_loc = layers.layer_geometry(layer).unwrap().loc;
                layer
                    .surface_under(
                        global_pos - output_geo.loc.to_f64() - layer_loc.to_f64(),
                        WindowSurfaceType::ALL,
                    )
                    .map(|(s, loc)| (s, loc + layer_loc + output_geo.loc))
            } else if let Some((_, surface, loc)) = workspace
                .space
                .surface_under(relative_pos, WindowSurfaceType::ALL)
            {
                Some((surface, loc + (global_pos - relative_pos).to_i32_round()))
            } else if let Some(layer) = layers
                .layer_under(WlrLayer::Bottom, relative_pos)
                .or_else(|| layers.layer_under(WlrLayer::Background, relative_pos))
            {
                let layer_loc = layers.layer_geometry(layer).unwrap().loc;
                layer
                    .surface_under(
                        global_pos - output_geo.loc.to_f64() - layer_loc.to_f64(),
                        WindowSurfaceType::ALL,
                    )
                    .map(|(s, loc)| (s, loc + layer_loc + output_geo.loc))
            } else {
                None
            }
        }
    }
}
