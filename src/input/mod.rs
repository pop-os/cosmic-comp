// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    config::{Action, Config},
    shell::{focus::target::PointerFocusTarget, layout::floating::SeatMoveGrabState, Workspace}, // shell::grabs::SeatMoveGrabState
    state::Common,
    utils::prelude::*,
};
use smithay::{
    backend::input::{
        AbsolutePositionEvent, Axis, AxisSource, Device, DeviceCapability, InputBackend,
        InputEvent, KeyState, PointerAxisEvent,
    },
    desktop::layer_map_for_output,
    input::{
        keyboard::{keysyms, FilterResult, KeysymHandle, XkbConfig},
        pointer::{AxisFrame, ButtonEvent, CursorImageStatus, MotionEvent},
        Seat, SeatState,
    },
    output::Output,
    reexports::wayland_server::DisplayHandle,
    utils::{Logical, Point, Rectangle, SERIAL_COUNTER},
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
                #[cfg(feature = "debug")]
                {
                    self.common.egui.debug_state.handle_device_added(&device);
                    self.common.egui.log_state.handle_device_added(&device);
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
                    self.common.egui.debug_state.handle_device_added(&device);
                    self.common.egui.log_state.handle_device_added(&device);
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
                                    #[cfg(feature = "debug")]
                                    {
                                        if data.common.seats.iter().position(|x| x == seat).unwrap()
                                            == 0
                                            && data.common.egui.active
                                        {
                                            if data.common.egui.debug_state.wants_keyboard() {
                                                data.common.egui.debug_state.handle_keyboard(
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
                                            if data.common.egui.log_state.wants_keyboard() {
                                                data.common.egui.log_state.handle_keyboard(
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
                                    let current_output = seat.active_output();
                                    let workspace =
                                        self.common.shell.active_space_mut(&current_output);
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
                                    if let Some(motion_event) = self
                                        .common
                                        .shell
                                        .activate(&current_output, workspace as usize)
                                    {
                                        if let Some(ptr) = seat.get_pointer() {
                                            ptr.motion(self, None, &motion_event);
                                        }
                                    }
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
                                        workspace as usize,
                                    );
                                }
                                Action::Focus(focus) => {
                                    let current_output = seat.active_output();
                                    let workspace =
                                        self.common.shell.active_space_mut(&current_output);
                                    let focus_stack = workspace.focus_stack.get(seat);
                                    if let Some(target) = workspace.tiling_layer.next_focus(
                                        focus,
                                        seat,
                                        focus_stack.iter(),
                                    ) {
                                        std::mem::drop(focus_stack);
                                        Common::set_focus(self, Some(&target), seat, None);
                                    }
                                }
                                Action::Fullscreen => {
                                    let current_output = seat.active_output();
                                    let workspace =
                                        self.common.shell.active_space_mut(&current_output);
                                    let focus_stack = workspace.focus_stack.get(seat);
                                    let focused_window = focus_stack.last();
                                    if let Some(window) = focused_window.map(|f| f.active_window())
                                    {
                                        workspace.fullscreen_toggle(&window, &current_output);
                                    }
                                }
                                Action::Orientation(orientation) => {
                                    let output = seat.active_output();
                                    let workspace = self.common.shell.active_space_mut(&output);
                                    let focus_stack = workspace.focus_stack.get(seat);
                                    workspace.tiling_layer.update_orientation(
                                        orientation,
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
                                } /*
                                  Action::Screenshot => {
                                      let home = match std::env::var("HOME") {
                                          Ok(home) => home,
                                          Err(err) => {
                                              slog_scope::error!(
                                                  "$HOME is not set, can't save screenshots: {}",
                                                  err
                                              );
                                              break;
                                          }
                                      };
                                      let timestamp = match std::time::SystemTime::UNIX_EPOCH
                                          .elapsed()
                                      {
                                          Ok(duration) => duration.as_secs(),
                                          Err(err) => {
                                              slog_scope::error!("Unable to get timestamp, can't save screenshots: {}", err);
                                              break;
                                          }
                                      };
                                      for output in self.common.shell.outputs.clone().into_iter() {
                                          match self
                                              .backend
                                              .offscreen_for_output(&output, &mut self.common)
                                          {
                                              Ok((buffer, size)) => {
                                                  let mut path = std::path::PathBuf::new();
                                                  path.push(&home);
                                                  path.push(format!(
                                                      "{}_{}.png",
                                                      output.name(),
                                                      timestamp
                                                  ));

                                                  fn write_png(
                                                      path: impl AsRef<std::path::Path>,
                                                      data: Vec<u8>,
                                                      size: Size<i32, Buffer>,
                                                  ) -> anyhow::Result<()>
                                                  {
                                                      use std::{fs, io};

                                                      let file = io::BufWriter::new(
                                                          fs::File::create(&path)?,
                                                      );
                                                      let mut encoder = png::Encoder::new(
                                                          file,
                                                          size.w as u32,
                                                          size.h as u32,
                                                      );
                                                      encoder.set_color(png::ColorType::Rgba);
                                                      encoder.set_depth(png::BitDepth::Eight);
                                                      let mut writer = encoder.write_header()?;
                                                      writer.write_image_data(&data)?;
                                                      Ok(())
                                                  }

                                                  if let Err(err) = write_png(&path, buffer, size) {
                                                      slog_scope::error!(
                                                          "Unable to save screenshot at {}: {}",
                                                          path.display(),
                                                          err
                                                      );
                                                  }
                                              }
                                              Err(err) => slog_scope::error!(
                                                  "Could not save screenshot for output {}: {}",
                                                  output.name(),
                                                  err
                                              ),
                                          }
                                      }
                                  }
                                  */
                            }
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
                            under,
                            &MotionEvent {
                                location: position,
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
                let device = event.device();
                for seat in self.common.seats().cloned().collect::<Vec<_>>().iter() {
                    let userdata = seat.user_data();
                    let devices = userdata.get::<Devices>().unwrap();
                    if devices.has_device(&device) {
                        let output = seat.active_output();
                        let geometry = output.geometry();
                        let position =
                            geometry.loc.to_f64() + event.position_transformed(geometry.size);
                        let relative_pos = self.common.shell.map_global_to_space(position, &output);
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
                            under,
                            &MotionEvent {
                                location: position,
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
                use smithay::backend::input::{ButtonState, PointerButtonEvent};

                let device = event.device();
                for seat in self.common.seats().cloned().collect::<Vec<_>>().iter() {
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
                                        if layer.can_receive_keyboard_focus() {
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
                                        if layer.can_receive_keyboard_focus() {
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
                                        if layer.can_receive_keyboard_focus() {
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

    pub fn surface_under(
        global_pos: Point<f64, Logical>,
        relative_pos: Point<f64, Logical>,
        output: &Output,
        output_geo: Rectangle<i32, Logical>,
        workspace: &Workspace,
    ) -> Option<(PointerFocusTarget, Point<i32, Logical>)> {
        let layers = layer_map_for_output(output);
        if let Some(window) = workspace.get_fullscreen(output) {
            if let Some(layer) = layers
                .layer_under(WlrLayer::Overlay, relative_pos)
                .or_else(|| layers.layer_under(WlrLayer::Top, relative_pos))
            {
                let layer_loc = layers.layer_geometry(layer).unwrap().loc;
                Some((layer.clone().into(), output_geo.loc + layer_loc))
            } else {
                Some((window.clone().into(), output_geo.loc))
            }
        } else {
            if let Some(layer) = layers
                .layer_under(WlrLayer::Overlay, relative_pos)
                .or_else(|| layers.layer_under(WlrLayer::Top, relative_pos))
            {
                let layer_loc = layers.layer_geometry(layer).unwrap().loc;
                Some((layer.clone().into(), output_geo.loc + layer_loc))
            } else if let Some((mapped, loc)) = workspace.element_under(relative_pos) {
                Some((
                    mapped.clone().into(),
                    loc + (global_pos - relative_pos).to_i32_round(),
                ))
            } else if let Some(layer) = layers
                .layer_under(WlrLayer::Bottom, relative_pos)
                .or_else(|| layers.layer_under(WlrLayer::Background, relative_pos))
            {
                let layer_loc = layers.layer_geometry(layer).unwrap().loc;
                Some((layer.clone().into(), output_geo.loc + layer_loc))
            } else {
                None
            }
        }
    }
}
