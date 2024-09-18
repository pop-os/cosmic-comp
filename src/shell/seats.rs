// SPDX-License-Identifier: GPL-3.0-only

use std::{any::Any, cell::RefCell, collections::HashMap, sync::Mutex};

use crate::{
    backend::render::cursor::CursorState,
    config::{xkb_config_to_wl, Config},
    input::{ModifiersShortcutQueue, SupressedButtons, SupressedKeys},
    state::State,
};
use smithay::{
    backend::input::{Device, DeviceCapability},
    desktop::utils::bbox_from_surface_tree,
    input::{
        keyboard::{LedState, XkbConfig},
        pointer::{CursorImageAttributes, CursorImageStatus},
        Seat, SeatState,
    },
    output::Output,
    reexports::{input::Device as InputDevice, wayland_server::DisplayHandle},
    utils::{Buffer, IsAlive, Monotonic, Point, Rectangle, Time, Transform},
    wayland::compositor::with_states,
};
use tracing::warn;

use super::grabs::{SeatMenuGrabState, SeatMoveGrabState};

crate::utils::id_gen!(next_seat_id, SEAT_ID, SEAT_IDS);

// for more information on seats, see:
// <https://wayland-book.com/print.html#seats-handling-input>
/// Seats are an abstraction over a set of input devices grouped together, such as a keyboard, pointer and touch device.
/// i.e. Those used by a user to operate the computer.
#[derive(Debug)]
pub struct Seats {
    seats: Vec<Seat<State>>,
    last_active: Option<Seat<State>>,
}

impl Seats {
    pub fn new() -> Seats {
        Seats {
            seats: Vec::new(),
            last_active: None,
        }
    }

    pub fn add_seat(&mut self, seat: Seat<State>) {
        if self.seats.is_empty() {
            self.last_active = Some(seat.clone());
        }
        self.seats.push(seat);
    }

    pub fn remove_seat(&mut self, seat: &Seat<State>) {
        self.seats.retain(|s| s != seat);
        if self.seats.is_empty() {
            self.last_active = None;
        } else if self.last_active.as_ref().is_some_and(|s| s == seat) {
            self.last_active = Some(self.seats[0].clone());
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Seat<State>> {
        self.seats.iter()
    }

    pub fn last_active(&self) -> &Seat<State> {
        self.last_active.as_ref().expect("No seat?")
    }

    pub fn update_last_active(&mut self, seat: &Seat<State>) {
        self.last_active = Some(seat.clone());
    }

    pub fn for_device<D: Device>(&self, device: &D) -> Option<&Seat<State>> {
        self.iter().find(|seat| {
            // XXX
            /*
            let userdata = seat.user_data();
            let devices = userdata.get::<Devices>().unwrap();
            devices.has_device(device)
            */
            true
        })
    }
}

impl Devices {
    pub fn add_device<D: Device + 'static>(&self, device: &D) -> Vec<DeviceCapability> {
        let id = device.id();
        let mut map = self.capabilities.borrow_mut();
        let caps = [
            DeviceCapability::Keyboard,
            DeviceCapability::Pointer,
            DeviceCapability::TabletTool,
        ]
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

        if device.has_capability(DeviceCapability::Keyboard) {
            if let Some(device) = <dyn Any>::downcast_ref::<InputDevice>(device) {
                self.keyboards.borrow_mut().push(device.clone());
            }
        }

        new_caps
    }

    pub fn has_device<D: Device>(&self, device: &D) -> bool {
        self.capabilities.borrow().contains_key(&device.id())
    }

    pub fn remove_device<D: Device>(&self, device: &D) -> Vec<DeviceCapability> {
        let id = device.id();

        let mut keyboards = self.keyboards.borrow_mut();
        if let Some(idx) = keyboards.iter().position(|x| x.id() == id) {
            keyboards.remove(idx);
        }

        let mut map = self.capabilities.borrow_mut();
        map.remove(&id)
            .unwrap_or(Vec::new())
            .into_iter()
            .filter(|c| map.values().flatten().all(|has| *c != *has))
            .collect()
    }

    pub fn update_led_state(&self, led_state: LedState) {
        for keyboard in self.keyboards.borrow_mut().iter_mut() {
            keyboard.led_update(led_state.into());
        }
    }
}

#[derive(Default)]
pub struct Devices {
    capabilities: RefCell<HashMap<String, Vec<DeviceCapability>>>,
    // Used for updating keyboard leds on kms backend
    keyboards: RefCell<Vec<InputDevice>>,
}

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

#[repr(transparent)]
struct SeatId(pub usize);

/// The output which contains the cursor associated with a seat.
struct ActiveOutput(pub Mutex<Output>);

/// The output which currently has keyboard focus
struct FocusedOutput(pub Mutex<Option<Output>>);

pub fn create_seat(
    dh: &DisplayHandle,
    seat_state: &mut SeatState<State>,
    output: &Output,
    config: &Config,
    name: String,
) -> Seat<State> {
    let mut seat = seat_state.new_wl_seat(dh, name);
    let userdata = seat.user_data();
    userdata.insert_if_missing_threadsafe(SeatId::default);
    userdata.insert_if_missing(Devices::default);
    userdata.insert_if_missing(SupressedKeys::default);
    userdata.insert_if_missing(SupressedButtons::default);
    userdata.insert_if_missing(ModifiersShortcutQueue::default);
    userdata.insert_if_missing_threadsafe(SeatMoveGrabState::default);
    userdata.insert_if_missing_threadsafe(SeatMenuGrabState::default);
    userdata.insert_if_missing_threadsafe(CursorState::default);
    userdata.insert_if_missing_threadsafe(|| ActiveOutput(Mutex::new(output.clone())));
    userdata.insert_if_missing_threadsafe(|| FocusedOutput(Mutex::new(None)));
    userdata.insert_if_missing_threadsafe(|| Mutex::new(CursorImageStatus::default_named()));

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
    if let Err(err) = seat.add_keyboard(
        xkb_config_to_wl(&conf),
        (conf.repeat_delay as i32).abs(),
        (conf.repeat_rate as i32).abs(),
    ) {
        warn!(
            ?err,
            "Failed to load provided xkb config. Trying default...",
        );
        seat.add_keyboard(
            XkbConfig::default(),
            (conf.repeat_delay as i32).abs(),
            (conf.repeat_rate as i32).abs(),
        )
        .expect("Failed to load xkb configuration files");
    }
    seat.add_pointer();
    seat.add_touch();

    seat
}

pub trait SeatExt {
    fn id(&self) -> usize;

    fn active_output(&self) -> Output;
    fn focused_output(&self) -> Option<Output>;
    fn focused_or_active_output(&self) -> Output {
        self.focused_output()
            .unwrap_or_else(|| self.active_output())
    }
    fn set_active_output(&self, output: &Output);
    fn set_focused_output(&self, output: Option<&Output>);
    fn devices(&self) -> &Devices;
    fn supressed_keys(&self) -> &SupressedKeys;
    fn supressed_buttons(&self) -> &SupressedButtons;
    fn modifiers_shortcut_queue(&self) -> &ModifiersShortcutQueue;

    fn cursor_geometry(
        &self,
        loc: impl Into<Point<f64, Buffer>>,
        time: Time<Monotonic>,
    ) -> Option<(Rectangle<i32, Buffer>, Point<i32, Buffer>)>;
}

impl SeatExt for Seat<State> {
    fn id(&self) -> usize {
        self.user_data().get::<SeatId>().unwrap().0
    }

    /// Returns the output that contains the cursor associated with a seat. Note that the window which has keyboard focus
    /// may be on a different output. Currently, to get the focused output, first get the keyboard focus target and pass
    /// it to get_focused_output in the shell.
    fn active_output(&self) -> Output {
        self.user_data()
            .get::<ActiveOutput>()
            .map(|x| x.0.lock().unwrap().clone())
            .unwrap()
    }

    /// Returns the output which currently has keyboard focus. If no window has keyboard focus (e.g. when there are no windows)
    /// the focused output will be the same as the active output.
    fn focused_output(&self) -> Option<Output> {
        if self
            .get_keyboard()
            .is_some_and(|k| k.current_focus().is_some())
        {
            self.user_data()
                .get::<FocusedOutput>()
                .map(|x| x.0.lock().unwrap().clone())?
        } else {
            None
        }
    }

    fn set_active_output(&self, output: &Output) {
        *self
            .user_data()
            .get::<ActiveOutput>()
            .unwrap()
            .0
            .lock()
            .unwrap() = output.clone();
    }

    fn set_focused_output(&self, output: Option<&Output>) {
        *self
            .user_data()
            .get::<FocusedOutput>()
            .unwrap()
            .0
            .lock()
            .unwrap() = output.cloned();
    }

    fn devices(&self) -> &Devices {
        self.user_data().get::<Devices>().unwrap()
    }

    fn supressed_keys(&self) -> &SupressedKeys {
        self.user_data().get::<SupressedKeys>().unwrap()
    }

    fn supressed_buttons(&self) -> &SupressedButtons {
        self.user_data().get::<SupressedButtons>().unwrap()
    }

    fn modifiers_shortcut_queue(&self) -> &ModifiersShortcutQueue {
        self.user_data().get::<ModifiersShortcutQueue>().unwrap()
    }

    fn cursor_geometry(
        &self,
        loc: impl Into<Point<f64, Buffer>>,
        time: Time<Monotonic>,
    ) -> Option<(Rectangle<i32, Buffer>, Point<i32, Buffer>)> {
        let location = loc.into().to_i32_round();

        let cursor_status = self
            .user_data()
            .get::<Mutex<CursorImageStatus>>()
            .map(|lock| {
                let mut cursor_status = lock.lock().unwrap();
                if let CursorImageStatus::Surface(ref surface) = *cursor_status {
                    if !surface.alive() {
                        *cursor_status = CursorImageStatus::default_named();
                    }
                }
                cursor_status.clone()
            })
            .unwrap_or(CursorImageStatus::default_named());

        match cursor_status {
            CursorImageStatus::Surface(surface) => {
                let hotspot = with_states(&surface, |states| {
                    states
                        .data_map
                        .get::<Mutex<CursorImageAttributes>>()
                        .unwrap()
                        .lock()
                        .unwrap()
                        .hotspot
                });
                let geo = bbox_from_surface_tree(&surface, (location.x, location.y));
                let buffer_geo = Rectangle::from_loc_and_size(
                    (geo.loc.x, geo.loc.y),
                    geo.size.to_buffer(1, Transform::Normal),
                );
                Some((buffer_geo, (hotspot.x, hotspot.y).into()))
            }
            CursorImageStatus::Named(cursor_icon) => {
                let seat_userdata = self.user_data();
                seat_userdata.insert_if_missing_threadsafe(CursorState::default);
                let state = seat_userdata.get::<CursorState>().unwrap();
                let frame = state
                    .lock()
                    .unwrap()
                    .get_named_cursor(cursor_icon)
                    .get_image(1, time.as_millis());

                Some((
                    Rectangle::from_loc_and_size(
                        location,
                        (frame.width as i32, frame.height as i32),
                    ),
                    (frame.xhot as i32, frame.yhot as i32).into(),
                ))
            }
            CursorImageStatus::Hidden => None,
        }
    }
}
