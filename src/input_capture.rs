//! Input capture for the XDG portal.
//!
//! The existing libei integration in this compositor is an EI *sender* (it
//! accepts injected events). InputCapture needs the other direction: the
//! compositor is an EIS *receiver* and sends physical input to a client after
//! a pointer barrier has been crossed. `reis` already provides the protocol
//! machinery for that receiver context; this module owns the small amount of
//! compositor policy around it.

use std::collections::HashSet;
use std::os::unix::net::UnixStream;

use calloop::{PostAction, channel};
use futures_channel::oneshot;
use reis::{
    calloop::EisRequestSourceEvent,
    eis::{self, device::DeviceType},
    request::{Connection, Device, DeviceCapability, EisRequest, Seat},
};
use smithay::{
    backend::{
        input::{AxisSource, ButtonState, InputTime, KeyState},
        libei::EiRegion,
    },
    input::keyboard::{KeymapFile, SerializedMods},
};

use crate::{dbus::DBusState, state::State};

/// InputCapture device capability values, as defined by the portal protocol.
pub(crate) const DEVICE_KEYBOARD: u32 = 1;
pub(crate) const DEVICE_POINTER: u32 = 2;
pub(crate) const SUPPORTED_DEVICE_TYPES: u32 = DEVICE_KEYBOARD | DEVICE_POINTER;
pub(crate) const MAX_BARRIERS: usize = 256;
pub(crate) type Zone = (u32, u32, i32, i32);

/// Logical output rectangles used by both GetZones and barrier validation.
/// The calloop thread owns the snapshot and its zone_set generation.
fn zones_from_regions(regions: Vec<EiRegion>) -> Vec<Zone> {
    regions
        .into_iter()
        .filter_map(|region| {
            let rect = region.rect;
            (rect.size.w > 0 && rect.size.h > 0).then_some((
                rect.size.w as u32,
                rect.size.h as u32,
                rect.loc.x,
                rect.loc.y,
            ))
        })
        .collect()
}

/// EI regions use unsigned origins. Translate the whole layout instead of
/// clamping each negative origin to zero, which would make outputs overlap.
fn eis_region_offset(regions: &[EiRegion]) -> (i64, i64) {
    let min_x = regions
        .iter()
        .map(|r| i64::from(r.rect.loc.x))
        .min()
        .unwrap_or(0);
    let min_y = regions
        .iter()
        .map(|r| i64::from(r.rect.loc.y))
        .min()
        .unwrap_or(0);
    (
        0_i64.saturating_sub(min_x).max(0),
        0_i64.saturating_sub(min_y).max(0),
    )
}

fn next_zone_set(current: u32) -> u32 {
    current.wrapping_add(1).max(1)
}

/// Release's cursor location is an untrusted *suggestion* from the client.
/// Accept a point on, or just beyond, a logical edge and clamp it back onto
/// the desktop; discard arbitrary off-screen or non-finite coordinates.
fn safe_cursor_position(position: (f64, f64), zones: &[Zone]) -> Option<(f64, f64)> {
    let (x, y) = position;
    if !x.is_finite() || !y.is_finite() {
        return None;
    }
    // A point exactly on a neighboring zone beats a fuzzy match against the
    // preceding output's edge.
    if zones.iter().any(|(width, height, left, top)| {
        let (left, top) = (f64::from(*left), f64::from(*top));
        x >= left && x < left + f64::from(*width) && y >= top && y < top + f64::from(*height)
    }) {
        return Some(position);
    }
    zones.iter().find_map(|(width, height, left, top)| {
        let (left, top) = (f64::from(*left), f64::from(*top));
        let (right, bottom) = (left + f64::from(*width), top + f64::from(*height));
        (x >= left - 1.0 && x <= right + 1.0 && y >= top - 1.0 && y <= bottom + 1.0)
            .then(|| (x.clamp(left, right - 1.0), y.clamp(top, bottom - 1.0)))
    })
}

/// A pointer barrier in compositor-global logical coordinates.
#[derive(Clone, Debug)]
pub(crate) struct Barrier {
    pub(crate) id: u32,
    pub(crate) position: (i32, i32, i32, i32),
    direction: Option<BarrierDirection>,
}

#[derive(Clone, Copy, Debug)]
enum BarrierDirection {
    Horizontal(f64),
    Vertical(f64),
}

impl Barrier {
    pub(crate) fn new(id: u32, position: (i32, i32, i32, i32)) -> Self {
        Self {
            id,
            position,
            direction: None,
        }
    }
}

/// Messages sent from the compositor D-Bus interface to the compositor's
/// event loop. D-Bus methods must not touch input state from the D-Bus task,
/// because input state is owned by the calloop thread.
#[derive(Debug)]
pub(crate) enum Request {
    Connect {
        session_handle: String,
        portal_owner: String,
        device_types: u32,
        stream: UnixStream,
        reply: oneshot::Sender<Result<(), String>>,
    },
    GetZones {
        reply: oneshot::Sender<Result<(u32, Vec<Zone>), String>>,
    },
    SetPointerBarriers {
        session_handle: String,
        zone_set: u32,
        barriers: Vec<Barrier>,
        reply: oneshot::Sender<Result<Vec<u32>, String>>,
    },
    Enable {
        session_handle: String,
        reply: oneshot::Sender<Result<(), String>>,
    },
    Disable {
        session_handle: String,
        reply: oneshot::Sender<Result<(), String>>,
    },
    Release {
        session_handle: String,
        activation_id: Option<u32>,
        cursor_position: Option<(f64, f64)>,
        reply: oneshot::Sender<Result<(), String>>,
    },
    Close {
        session_handle: String,
    },
}

/// Signals emitted by the compositor and forwarded by the COSMIC portal to
/// the public `org.freedesktop.portal.InputCapture` interface.
#[derive(Clone, Debug)]
pub(crate) enum Signal {
    Activated {
        session_handle: String,
        activation_id: u32,
        barrier_id: u32,
        cursor_position: (f64, f64),
    },
    Deactivated {
        session_handle: String,
        activation_id: u32,
        cursor_position: (f64, f64),
    },
    Disabled {
        session_handle: String,
    },
    ZonesChanged {
        session_handle: String,
        zone_set: u32,
    },
}

/// Install the control channel used by the private compositor D-Bus API.
pub(crate) fn setup(handle: &calloop::LoopHandle<'static, State>) -> channel::Sender<Request> {
    let (sender, source) = channel::channel();
    handle
        .insert_source(source, |event, _, state| {
            if let channel::Event::Msg(request) = event {
                state.handle_input_capture_request(request);
            }
        })
        .expect("Failed to insert input capture channel into the event loop");
    sender
}

/// State for one InputCapture session. There is intentionally one active
/// session: a compositor seat can only have one owner for physical input at a
/// time, and Synergy/Deskflow uses one session per desktop.
#[derive(Debug)]
pub(crate) struct InputCaptureState {
    session_handle: Option<String>,
    portal_owner: Option<String>,
    device_types: u32,
    enabled: bool,
    locked: bool,
    active: bool,
    activation_id: u32,
    zone_set: u32,
    barriers: Vec<Barrier>,
    connection: Option<Connection>,
    seat: Option<Seat>,
    keyboard: Option<Device>,
    pointer: Option<Device>,
    pointer_absolute: Option<Device>,
    absolute_pointer_bound: bool,
    cursor_position: (f64, f64),
}

impl Default for InputCaptureState {
    fn default() -> Self {
        Self {
            session_handle: None,
            portal_owner: None,
            device_types: 0,
            enabled: false,
            locked: false,
            active: false,
            activation_id: 0,
            zone_set: 1,
            barriers: Vec::new(),
            connection: None,
            seat: None,
            keyboard: None,
            pointer: None,
            pointer_absolute: None,
            absolute_pointer_bound: false,
            cursor_position: (0.0, 0.0),
        }
    }
}

impl InputCaptureState {
    fn is_session(&self, session_handle: &str) -> bool {
        self.session_handle.as_deref() == Some(session_handle)
    }

    pub(crate) fn captures_keyboard(&self) -> bool {
        !self.locked
            && self.active
            && self.device_types & DEVICE_KEYBOARD != 0
            && self.keyboard.is_some()
    }

    /// True while captured input is diverted off this seat. The hardware cursor
    /// should stay hidden for the whole activation, not only idle-timeout.
    pub(crate) fn hides_cursor(&self) -> bool {
        self.active && !self.locked
    }

    pub(crate) fn suspend_for_lock(&mut self, dbus: &DBusState) {
        self.locked = true;
        if self.active {
            self.deactivate(dbus, None);
        }
        // Keep the authorized session armed across the lock. All capture paths
        // check `locked`, so neither pointer nor keyboard events can leave the
        // compositor until unlock. Emitting Disabled here would require the
        // client to call Enable again; clients such as Synergy do not reliably
        // recover from that signal during a screen lock.
        tracing::info!(
            enabled = self.enabled,
            eis_connected = self.connection.is_some(),
            "InputCapture suspended for session lock"
        );
    }

    pub(crate) fn resume_after_unlock(&mut self) {
        self.locked = false;
        tracing::info!(
            enabled = self.enabled,
            eis_connected = self.connection.is_some(),
            "InputCapture resumed after session unlock"
        );
    }

    pub(crate) fn revoke_if_portal_gone(&mut self, dbus: &DBusState) {
        let (Some(session), Some(owner)) =
            (self.session_handle.clone(), self.portal_owner.as_deref())
        else {
            return;
        };
        if !dbus.portal_owns_input_capture(owner) {
            tracing::warn!("Revoking InputCapture after portal owner disappeared");
            self.close(dbus, &session);
        }
    }

    fn connect(
        &mut self,
        session_handle: String,
        portal_owner: String,
        device_types: u32,
        stream: UnixStream,
    ) -> Result<eis::Context, String> {
        if self.locked {
            return Err("input capture unavailable while session is locked".to_string());
        }
        if device_types == 0 || device_types & !SUPPORTED_DEVICE_TYPES != 0 {
            return Err("invalid input capture capabilities".to_string());
        }
        if self.session_handle.is_some() {
            return Err("an input capture session is already connected".to_string());
        }
        let _ = stream.set_nonblocking(true);
        let context = eis::Context::new(stream)
            .map_err(|err| format!("failed to create EIS context: {err}"))?;
        self.session_handle = Some(session_handle);
        self.portal_owner = Some(portal_owner);
        self.device_types = device_types & SUPPORTED_DEVICE_TYPES;
        self.enabled = false;
        self.active = false;
        self.connection = None;
        self.seat = None;
        self.keyboard = None;
        self.pointer = None;
        self.pointer_absolute = None;
        self.absolute_pointer_bound = false;
        Ok(context)
    }

    fn connected(&mut self, dbus: &DBusState, connection: &Connection) -> PostAction {
        if connection.context_type() != eis::handshake::ContextType::Receiver {
            connection.disconnected(
                eis::connection::DisconnectReason::Value,
                Some("InputCapture requires an EIS receiver context"),
            );
            let _ = connection.flush();
            self.disconnected(dbus);
            return PostAction::Remove;
        }

        let capabilities = match self.device_types {
            DEVICE_KEYBOARD => DeviceCapability::Keyboard.into(),
            DEVICE_POINTER => {
                DeviceCapability::Pointer
                    | DeviceCapability::PointerAbsolute
                    | DeviceCapability::Scroll
                    | DeviceCapability::Button
            }
            _ => {
                DeviceCapability::Keyboard
                    | DeviceCapability::Pointer
                    | DeviceCapability::PointerAbsolute
                    | DeviceCapability::Scroll
                    | DeviceCapability::Button
            }
        };
        let seat = connection.add_seat(Some("default"), capabilities);
        self.connection = Some(connection.clone());
        // The seat is kept by the device handles once the client binds. Keep
        // the advertised seat alive by retaining the connection; reis owns the
        // seat internally as well.
        let _ = seat;
        PostAction::Continue
    }

    fn bind(
        &mut self,
        request: reis::request::Bind,
        keymap: Option<KeymapFile>,
        regions: Vec<EiRegion>,
    ) {
        let Some(connection) = self.connection.clone() else {
            return;
        };
        let capabilities = request.capabilities;
        self.seat = Some(request.seat.clone());
        self.absolute_pointer_bound = capabilities.contains(DeviceCapability::PointerAbsolute);

        if self.keyboard.is_none()
            && self.device_types & DEVICE_KEYBOARD != 0
            && capabilities.contains(DeviceCapability::Keyboard)
        {
            let device = request.seat.add_device(
                Some("physical keyboard"),
                DeviceType::Virtual,
                DeviceCapability::Keyboard.into(),
                move |device| {
                    if let Some(keymap) = keymap
                        && let Some(keyboard) = device.interface::<eis::Keyboard>()
                    {
                        let _ = keymap.with_fd(true, |fd, len| {
                            keyboard.keymap(eis::keyboard::KeymapType::Xkb, len as u32, fd);
                        });
                    }
                },
            );
            device.resumed();
            self.keyboard = Some(device);
        }

        if self.pointer.is_none()
            && self.device_types & DEVICE_POINTER != 0
            && capabilities.contains(DeviceCapability::Pointer)
        {
            let device = request.seat.add_device(
                Some("physical pointer"),
                DeviceType::Virtual,
                DeviceCapability::Pointer | DeviceCapability::Button | DeviceCapability::Scroll,
                |_| {},
            );
            device.resumed();
            self.pointer = Some(device);
        }

        self.add_absolute_pointer(&regions);
        let _ = connection.flush();
    }

    /// Synergy derives the logical desktop shape from regions advertised on
    /// an absolute-pointer EIS device. Relative pointer input is still sent on
    /// the separate pointer device, but without this region-reporting device
    /// Synergy keeps its initial 1x1 fallback screen and cannot switch away.
    fn add_absolute_pointer(&mut self, regions: &[EiRegion]) {
        if self.pointer_absolute.is_some()
            || !self.absolute_pointer_bound
            || self.device_types & DEVICE_POINTER == 0
            || regions.is_empty()
        {
            return;
        }
        let Some(seat) = self.seat.as_ref() else {
            return;
        };
        let (offset_x, offset_y) = eis_region_offset(regions);
        let regions = regions.to_vec();
        let device = seat.add_device(
            Some("physical absolute pointer"),
            DeviceType::Virtual,
            DeviceCapability::PointerAbsolute | DeviceCapability::Button | DeviceCapability::Scroll,
            move |device| {
                for region in &regions {
                    if let Some(mapping_id) = &region.mapping_id {
                        device.device().region_mapping_id(mapping_id);
                    }
                    device.device().region(
                        (i64::from(region.rect.loc.x) + offset_x).clamp(0, u32::MAX as i64) as u32,
                        (i64::from(region.rect.loc.y) + offset_y).clamp(0, u32::MAX as i64) as u32,
                        region.rect.size.w.max(0) as u32,
                        region.rect.size.h.max(0) as u32,
                        region.scale,
                    );
                }
            },
        );
        device.resumed();
        if self.active {
            device.start_emulating(self.activation_id);
        }
        self.pointer_absolute = Some(device);
        self.flush();
    }

    fn refresh_absolute_pointer(&mut self, regions: &[EiRegion]) {
        if let Some(device) = self.pointer_absolute.take() {
            if self.active {
                device.stop_emulating();
            }
            device.remove();
            self.flush();
        }
        self.add_absolute_pointer(regions);
    }

    fn eis_request(
        &mut self,
        dbus: &DBusState,
        request: EisRequest,
        keymap: Option<KeymapFile>,
        regions: Vec<EiRegion>,
    ) -> PostAction {
        match request {
            EisRequest::Disconnect => {
                self.disconnected(dbus);
                PostAction::Remove
            }
            EisRequest::Bind(request) => {
                self.bind(request, keymap, regions);
                PostAction::Continue
            }
            // Receiver clients should not send input requests. These are
            // harmless protocol bookkeeping requests in reis's high-level
            // converter and do not affect compositor state here.
            _ => PostAction::Continue,
        }
    }

    fn disconnected(&mut self, dbus: &DBusState) {
        let session_handle = self.session_handle.clone();
        if self.active {
            self.deactivate(dbus, None);
        }
        self.connection = None;
        self.seat = None;
        self.keyboard = None;
        self.pointer = None;
        self.pointer_absolute = None;
        self.absolute_pointer_bound = false;
        self.enabled = false;
        self.session_handle = None;
        self.portal_owner = None;
        self.device_types = 0;
        self.barriers.clear();
        if let Some(session_handle) = session_handle {
            self.signal(dbus, Signal::Disabled { session_handle });
        }
    }

    pub(crate) fn zones(&self) -> u32 {
        self.zone_set
    }

    pub(crate) fn set_barriers(
        &mut self,
        dbus: &DBusState,
        session_handle: &str,
        zone_set: u32,
        barriers: Vec<Barrier>,
        zones: &[(u32, u32, i32, i32)],
    ) -> Result<Vec<u32>, String> {
        if !self.is_session(session_handle) {
            return Err("unknown input capture session".to_string());
        }
        if barriers.len() > MAX_BARRIERS {
            return Err("too many input capture barriers".to_string());
        }
        if zone_set != self.zone_set {
            if self.active {
                self.deactivate(dbus, None);
            }
            self.enabled = false;
            self.barriers.clear();
            return Ok(barriers.into_iter().map(|barrier| barrier.id).collect());
        }

        let mut failed = Vec::new();
        let mut accepted = Vec::new();
        let mut seen = HashSet::new();
        for barrier in barriers {
            if barrier.id == 0 || !seen.insert(barrier.id) {
                failed.push(barrier.id);
                continue;
            }
            if let Some(direction) = barrier_direction(&barrier, zones) {
                let mut barrier = barrier;
                barrier.direction = Some(direction);
                accepted.push(barrier);
            } else {
                failed.push(barrier.id);
            }
        }
        if self.active {
            self.deactivate(dbus, None);
        }
        self.enabled = false;
        self.barriers = accepted;
        Ok(failed)
    }

    pub(crate) fn enable(&mut self, session_handle: &str) -> Result<(), String> {
        if !self.is_session(session_handle) {
            return Err("unknown input capture session".to_string());
        }
        if self.connection.is_none() {
            return Err("EIS is not connected".to_string());
        }
        if self.locked {
            return Err("input capture unavailable while session is locked".to_string());
        }
        self.enabled = true;
        Ok(())
    }

    pub(crate) fn disable(&mut self, session_handle: &str) -> Result<(), String> {
        if !self.is_session(session_handle) {
            return Err("unknown input capture session".to_string());
        }
        self.stop_emulating();
        self.enabled = false;
        Ok(())
    }

    pub(crate) fn release_applies(&self, session_handle: &str, activation_id: Option<u32>) -> bool {
        self.is_session(session_handle)
            && self.active
            && activation_id.is_none_or(|id| id == self.activation_id)
    }

    pub(crate) fn release(
        &mut self,
        session_handle: &str,
        activation_id: Option<u32>,
        _cursor_position: Option<(f64, f64)>,
    ) -> Result<(), String> {
        if !self.is_session(session_handle) {
            return Err("unknown input capture session".to_string());
        }
        if let Some(activation_id) = activation_id
            && (!self.active || activation_id != self.activation_id)
        {
            return Ok(());
        }
        self.stop_emulating();
        Ok(())
    }

    pub(crate) fn close(&mut self, dbus: &DBusState, session_handle: &str) {
        if !self.is_session(session_handle) {
            return;
        }
        if self.active {
            self.deactivate(dbus, None);
        }
        if let Some(connection) = self.connection.take() {
            connection.disconnected(eis::connection::DisconnectReason::Disconnected, None);
        }
        self.keyboard = None;
        self.pointer = None;
        self.pointer_absolute = None;
        self.seat = None;
        self.absolute_pointer_bound = false;
        self.enabled = false;
        self.session_handle = None;
        self.portal_owner = None;
        self.device_types = 0;
        self.barriers.clear();
    }

    pub(crate) fn output_changed(&mut self, dbus: &DBusState, regions: &[EiRegion]) {
        self.zone_set = next_zone_set(self.zone_set);
        self.barriers.clear();
        self.refresh_absolute_pointer(regions);
        if let Some(session_handle) = self.session_handle.clone() {
            if self.active {
                self.deactivate(dbus, None);
            }
            self.signal(
                dbus,
                Signal::ZonesChanged {
                    session_handle,
                    zone_set: self.zone_set,
                },
            );
        }
    }

    /// Route a normal physical pointer motion. Returns true when the event was
    /// consumed by capture (including the motion that activates capture).
    pub(crate) fn motion(
        &mut self,
        dbus: &DBusState,
        position: (f64, f64),
        delta: (f64, f64),
        time: InputTime,
        modifiers: Option<SerializedMods>,
    ) -> bool {
        if self.locked {
            return false;
        }
        if self.active {
            if self.device_types & DEVICE_POINTER == 0 || self.pointer.is_none() {
                return false;
            }
            self.cursor_position.0 += delta.0;
            self.cursor_position.1 += delta.1;
            self.send_pointer_motion(delta, time);
            return true;
        }
        if !self.enabled
            || self.connection.is_none()
            || self.device_types & DEVICE_POINTER == 0
            || self.pointer.is_none()
        {
            return false;
        }
        let Some(barrier_id) = self.triggered_barrier(position, delta) else {
            return false;
        };
        let cursor_position = (position.0 + delta.0, position.1 + delta.1);
        self.activate(dbus, barrier_id, cursor_position, modifiers);
        true
    }

    pub(crate) fn keyboard(&mut self, time: InputTime, key: u32, state: KeyState) -> bool {
        if self.locked
            || !self.active
            || self.device_types & DEVICE_KEYBOARD == 0
            || self.keyboard.is_none()
        {
            return false;
        }
        let Some(device) = self.keyboard.as_ref() else {
            return false;
        };
        let Some(keyboard) = device.interface::<eis::Keyboard>() else {
            return false;
        };
        let state = match state {
            KeyState::Pressed => eis::keyboard::KeyState::Press,
            KeyState::Released => eis::keyboard::KeyState::Released,
        };
        keyboard.key(key.saturating_sub(8), state);
        device.frame(time.micros());
        self.flush();
        true
    }

    pub(crate) fn button(&mut self, time: InputTime, button: u32, state: ButtonState) -> bool {
        if self.locked
            || !self.active
            || self.device_types & DEVICE_POINTER == 0
            || self.pointer.is_none()
        {
            return false;
        }
        let Some(device) = self.pointer.as_ref() else {
            return false;
        };
        let Some(button_interface) = device.interface::<eis::Button>() else {
            return false;
        };
        let state = match state {
            ButtonState::Pressed => eis::button::ButtonState::Press,
            ButtonState::Released => eis::button::ButtonState::Released,
        };
        button_interface.button(button, state);
        device.frame(time.micros());
        self.flush();
        true
    }

    pub(crate) fn axis(
        &mut self,
        time: InputTime,
        source: AxisSource,
        amount: (Option<f64>, Option<f64>),
        discrete: (Option<f64>, Option<f64>),
    ) -> bool {
        if self.locked
            || !self.active
            || self.device_types & DEVICE_POINTER == 0
            || self.pointer.is_none()
        {
            return false;
        }
        let Some(device) = self.pointer.as_ref() else {
            return false;
        };
        let Some(scroll) = device.interface::<eis::Scroll>() else {
            return false;
        };
        let (continuous_x, continuous_y) = amount;
        let (discrete_x, discrete_y) = discrete;
        if continuous_x.is_some() || continuous_y.is_some() {
            scroll.scroll(
                continuous_x.unwrap_or_default() as f32,
                continuous_y.unwrap_or_default() as f32,
            );
        } else if discrete_x.is_some() || discrete_y.is_some() {
            scroll.scroll_discrete(
                discrete_x.unwrap_or_default().round() as i32,
                discrete_y.unwrap_or_default().round() as i32,
            );
        } else if matches!(source, AxisSource::Finger) {
            scroll.scroll_stop(1, 1, 0);
        } else {
            return true;
        }
        device.frame(time.micros());
        self.flush();
        true
    }

    fn send_pointer_motion(&mut self, delta: (f64, f64), time: InputTime) {
        let Some(device) = self.pointer.as_ref() else {
            return;
        };
        let Some(pointer) = device.interface::<eis::Pointer>() else {
            return;
        };
        pointer.motion_relative(delta.0 as f32, delta.1 as f32);
        device.frame(time.micros());
        self.flush();
    }

    fn activate(
        &mut self,
        dbus: &DBusState,
        barrier_id: u32,
        cursor_position: (f64, f64),
        modifiers: Option<SerializedMods>,
    ) {
        if self.active {
            return;
        }
        let Some(connection) = self.connection.as_ref() else {
            return;
        };
        if self.keyboard.is_none() && self.pointer.is_none() {
            return;
        }

        self.activation_id = self.activation_id.wrapping_add(1).max(1);
        self.cursor_position = cursor_position;
        self.active = true;
        if let Some(device) = self.keyboard.as_ref() {
            device.start_emulating(self.activation_id);
        }
        if let Some(device) = self.pointer.as_ref() {
            device.start_emulating(self.activation_id);
        }
        if let Some(device) = self.pointer_absolute.as_ref() {
            device.start_emulating(self.activation_id);
        }
        if let (Some(modifiers), Some(device)) = (modifiers, self.keyboard.as_ref())
            && let Some(keyboard) = device.interface::<eis::Keyboard>()
        {
            connection.with_next_serial(|serial| {
                keyboard.modifiers(
                    serial,
                    modifiers.depressed,
                    modifiers.locked,
                    modifiers.latched,
                    modifiers.layout_effective,
                );
            });
        }
        self.flush();
        let Some(session_handle) = self.session_handle.clone() else {
            return;
        };
        self.signal(
            dbus,
            Signal::Activated {
                session_handle,
                activation_id: self.activation_id,
                barrier_id,
                cursor_position,
            },
        );
    }

    fn deactivate(&mut self, dbus: &DBusState, cursor_position: Option<(f64, f64)>) {
        if !self.stop_emulating() {
            return;
        }
        let Some(session_handle) = self.session_handle.clone() else {
            return;
        };
        let cursor_position = cursor_position.unwrap_or(self.cursor_position);
        self.signal(
            dbus,
            Signal::Deactivated {
                session_handle,
                activation_id: self.activation_id,
                cursor_position,
            },
        );
    }

    fn stop_emulating(&mut self) -> bool {
        if !self.active {
            return false;
        }
        if let Some(device) = self.keyboard.as_ref() {
            device.stop_emulating();
        }
        if let Some(device) = self.pointer.as_ref() {
            device.stop_emulating();
        }
        if let Some(device) = self.pointer_absolute.as_ref() {
            device.stop_emulating();
        }
        self.flush();
        self.active = false;
        true
    }

    fn triggered_barrier(&self, position: (f64, f64), delta: (f64, f64)) -> Option<u32> {
        let new_position = (position.0 + delta.0, position.1 + delta.1);
        self.barriers.iter().find_map(|barrier| {
            let (x1, y1, x2, y2) = barrier.position;
            let direction = barrier.direction?;
            if x1 == x2 {
                let BarrierDirection::Vertical(outward) = direction else {
                    return None;
                };
                if delta.0 == 0.0
                    || !crosses_outward(position.0, new_position.0, x1 as f64, outward)
                {
                    return None;
                }
                let ratio = (x1 as f64 - position.0) / delta.0;
                let y = position.1 + delta.1 * ratio;
                if between(y, y1 as f64, y2 as f64) {
                    return Some(barrier.id);
                }
            } else if y1 == y2 {
                let BarrierDirection::Horizontal(outward) = direction else {
                    return None;
                };
                if delta.1 == 0.0
                    || !crosses_outward(position.1, new_position.1, y1 as f64, outward)
                {
                    return None;
                }
                let ratio = (y1 as f64 - position.1) / delta.1;
                let x = position.0 + delta.0 * ratio;
                if between(x, x1 as f64, x2 as f64) {
                    return Some(barrier.id);
                }
            }
            None
        })
    }

    fn flush(&self) {
        if let Some(connection) = self.connection.as_ref() {
            let _ = connection.flush();
        }
    }

    fn signal(&self, dbus: &DBusState, signal: Signal) {
        dbus.emit_input_capture_signal(signal);
    }
}

fn between(value: f64, first: f64, second: f64) -> bool {
    value >= first.min(second) - 0.5 && value <= first.max(second) + 0.5
}

fn crosses_outward(from: f64, to: f64, line: f64, outward: f64) -> bool {
    if outward < 0.0 {
        (from > line && to <= line) || (from == line && to < line)
    } else {
        (from < line && to >= line) || (from == line && to > line)
    }
}

fn barrier_direction(
    barrier: &Barrier,
    zones: &[(u32, u32, i32, i32)],
) -> Option<BarrierDirection> {
    let (x1, y1, x2, y2) = barrier.position;
    if (x1 == x2 && y1 == y2) || (x1 != x2 && y1 != y2) {
        return None;
    }
    let x1 = x1 as i64;
    let y1 = y1 as i64;
    let x2 = x2 as i64;
    let y2 = y2 as i64;
    zones.iter().find_map(|(width, height, left, top)| {
        let left = *left as i64;
        let top = *top as i64;
        let right = left + i64::from(*width);
        let bottom = top + i64::from(*height);
        if x1 == x2 {
            let segment_start = y1.min(y2);
            let segment_end = y1.max(y2);
            let on_edge =
                (x1 == left || x1 == right) && segment_start >= top && segment_end < bottom;
            let outside_x = if x1 == left { left - 1 } else { right };
            if on_edge
                && !zones
                    .iter()
                    .any(|(other_width, other_height, other_left, other_top)| {
                        let other_left = *other_left as i64;
                        let other_top = *other_top as i64;
                        let other_right = other_left + i64::from(*other_width);
                        let other_bottom = other_top + i64::from(*other_height);
                        outside_x >= other_left
                            && outside_x < other_right
                            && ranges_overlap(
                                segment_start,
                                segment_end,
                                other_top,
                                other_bottom - 1,
                            )
                    })
            {
                Some(BarrierDirection::Vertical(if x1 == left {
                    -1.0
                } else {
                    1.0
                }))
            } else {
                None
            }
        } else {
            let segment_start = x1.min(x2);
            let segment_end = x1.max(x2);
            let on_edge =
                (y1 == top || y1 == bottom) && segment_start >= left && segment_end < right;
            let outside_y = if y1 == top { top - 1 } else { bottom };
            if on_edge
                && !zones
                    .iter()
                    .any(|(other_width, other_height, other_left, other_top)| {
                        let other_left = *other_left as i64;
                        let other_top = *other_top as i64;
                        let other_right = other_left + i64::from(*other_width);
                        let other_bottom = other_top + i64::from(*other_height);
                        outside_y >= other_top
                            && outside_y < other_bottom
                            && ranges_overlap(
                                segment_start,
                                segment_end,
                                other_left,
                                other_right - 1,
                            )
                    })
            {
                Some(BarrierDirection::Horizontal(if y1 == top {
                    -1.0
                } else {
                    1.0
                }))
            } else {
                None
            }
        }
    })
}

fn ranges_overlap(first_start: i64, first_end: i64, second_start: i64, second_end: i64) -> bool {
    first_start <= second_end && second_start <= first_end
}

fn make_keymap(config: &crate::config::Config) -> Option<KeymapFile> {
    let config = config.xkb_config();
    let context = xkbcommon::xkb::Context::new(xkbcommon::xkb::CONTEXT_NO_FLAGS);
    let keymap = xkbcommon::xkb::Keymap::new_from_names(
        &context,
        &config.rules,
        &config.model,
        &config.layout,
        &config.variant,
        config.options.clone(),
        xkbcommon::xkb::KEYMAP_COMPILE_NO_FLAGS,
    )?;
    Some(KeymapFile::new(&keymap))
}

impl State {
    pub(crate) fn handle_input_capture_request(&mut self, request: Request) {
        match request {
            Request::Connect {
                session_handle,
                portal_owner,
                device_types,
                stream,
                reply,
            } => {
                let dbus = self.common.dbus_state.clone();
                self.common.input_capture.revoke_if_portal_gone(&dbus);
                self.sync_input_capture_cursor_visibility();
                let context = match self.common.input_capture.connect(
                    session_handle.clone(),
                    portal_owner,
                    device_types,
                    stream,
                ) {
                    Ok(context) => context,
                    Err(err) => {
                        tracing::warn!(?err, "refusing input capture EIS connection");
                        let _ = reply.send(Err(err));
                        return;
                    }
                };
                let session_for_source = session_handle.clone();
                let handle = self.common.event_loop_handle.clone();
                if let Err(err) = handle.insert_source(
                    reis::calloop::EisRequestSource::new(context, 1),
                    move |event, connection, state| {
                        let action = match event {
                            Ok(EisRequestSourceEvent::Connected) => {
                                state.common.input_capture.connected(&dbus, connection)
                            }
                            Ok(EisRequestSourceEvent::Request(request)) => {
                                let keymap = matches!(&request, EisRequest::Bind(_))
                                    .then(|| make_keymap(&state.common.config))
                                    .flatten();
                                let regions = matches!(&request, EisRequest::Bind(_))
                                    .then(|| crate::libei::absolute_regions(state))
                                    .unwrap_or_default();
                                let action = state
                                    .common
                                    .input_capture
                                    .eis_request(&dbus, request, keymap, regions);
                                state.sync_input_capture_cursor_visibility();
                                action
                            }
                            Err(err) => {
                                tracing::warn!(?err, "input capture EIS connection failed");
                                state.common.input_capture.disconnected(&dbus);
                                state.sync_input_capture_cursor_visibility();
                                PostAction::Remove
                            }
                        };
                        let _ = connection.flush();
                        Ok(action)
                    },
                ) {
                    let err = format!("failed to insert input capture EIS source: {err}");
                    tracing::error!(?err, "failed to insert input capture EIS source");
                    self.common
                        .input_capture
                        .close(&self.common.dbus_state.clone(), &session_for_source);
                    self.sync_input_capture_cursor_visibility();
                    let _ = reply.send(Err(err));
                    return;
                }
                let _ = reply.send(Ok(()));
            }
            Request::GetZones { reply } => {
                // The legacy libportal CreateSession() helper calls
                // GetZones() before ConnectToEIS().  The private method is
                // already restricted to the portal/OSK callers, so the
                // current zone set can be returned before a compositor-side
                // EIS session exists.
                let zones = zones_from_regions(crate::libei::absolute_regions(self));
                let _ = reply.send(Ok((self.common.input_capture.zones(), zones)));
            }
            Request::SetPointerBarriers {
                session_handle,
                zone_set,
                barriers,
                reply,
            } => {
                let zones = zones_from_regions(crate::libei::absolute_regions(self));
                let dbus = self.common.dbus_state.clone();
                let result = self.common.input_capture.set_barriers(
                    &dbus,
                    &session_handle,
                    zone_set,
                    barriers,
                    &zones,
                );
                self.sync_input_capture_cursor_visibility();
                let _ = reply.send(result);
            }
            Request::Enable {
                session_handle,
                reply,
            } => {
                let result = self.common.input_capture.enable(&session_handle);
                let _ = reply.send(result);
            }
            Request::Disable {
                session_handle,
                reply,
            } => {
                let result = self.common.input_capture.disable(&session_handle);
                self.sync_input_capture_cursor_visibility();
                let _ = reply.send(result);
            }
            Request::Release {
                session_handle,
                activation_id,
                cursor_position,
                reply,
            } => {
                let apply_cursor_position = self
                    .common
                    .input_capture
                    .release_applies(&session_handle, activation_id);
                let result = self.common.input_capture.release(
                    &session_handle,
                    activation_id,
                    cursor_position,
                );
                if result.is_ok()
                    && apply_cursor_position
                    && let Some(position) = cursor_position
                    && self.common.shell.read().session_lock.is_none()
                    && let Some((x, y)) = safe_cursor_position(
                        position,
                        &zones_from_regions(crate::libei::absolute_regions(self)),
                    )
                {
                    let seat = self.common.shell.read().seats.last_active().clone();
                    if let Some(pointer) = seat.get_pointer() {
                        pointer.set_location(smithay::utils::Point::from((x, y)));
                    }
                }
                self.sync_input_capture_cursor_visibility();
                let _ = reply.send(result);
            }
            Request::Close { session_handle } => {
                let dbus = self.common.dbus_state.clone();
                self.common.input_capture.close(&dbus, &session_handle);
                self.sync_input_capture_cursor_visibility();
            }
        }
    }

    pub(crate) fn input_capture_output_changed(&mut self) {
        let regions = crate::libei::absolute_regions(self);
        let dbus = self.common.dbus_state.clone();
        self.common.input_capture.output_changed(&dbus, &regions);
        self.sync_input_capture_cursor_visibility();
    }

    pub(crate) fn sync_input_capture_cursor_visibility(&mut self) {
        crate::backend::render::cursor::set_hidden_for_input_capture(
            self,
            self.common.input_capture.hides_cursor(),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vertical_barrier_crossing() {
        let mut capture = InputCaptureState {
            barriers: vec![Barrier::new(7, (100, 0, 100, 200))],
            ..Default::default()
        };
        capture.barriers[0].direction = Some(BarrierDirection::Vertical(1.0));
        assert_eq!(capture.triggered_barrier((99.0, 50.0), (2.0, 0.0)), Some(7));
        assert_eq!(capture.triggered_barrier((99.0, 250.0), (2.0, 0.0)), None);
    }

    #[test]
    fn horizontal_barrier_crossing() {
        let mut capture = InputCaptureState {
            barriers: vec![Barrier::new(8, (0, 100, 200, 100))],
            ..Default::default()
        };
        capture.barriers[0].direction = Some(BarrierDirection::Horizontal(1.0));
        assert_eq!(capture.triggered_barrier((50.0, 99.0), (0.0, 2.0)), Some(8));
        assert_eq!(capture.triggered_barrier((250.0, 99.0), (0.0, 2.0)), None);
    }

    #[test]
    fn edge_barriers_trigger_only_outward() {
        let mut capture = InputCaptureState {
            barriers: vec![Barrier::new(9, (0, 0, 0, 200))],
            ..Default::default()
        };
        capture.barriers[0].direction = Some(BarrierDirection::Vertical(-1.0));
        assert_eq!(capture.triggered_barrier((0.0, 50.0), (-1.0, 0.0)), Some(9));
        assert_eq!(capture.triggered_barrier((0.0, 50.0), (1.0, 0.0)), None);
    }

    #[test]
    fn barriers_must_be_on_the_outer_edge_of_a_zone() {
        let zones = [(1920, 1080, 0, 0), (1920, 1080, 1920, 0)];

        assert!(barrier_direction(&Barrier::new(1, (1920, 0, 1920, 1079)), &zones,).is_none());
        assert!(barrier_direction(&Barrier::new(2, (3840, 0, 3840, 1079)), &zones,).is_some());
        assert!(barrier_direction(&Barrier::new(3, (0, 0, 1919, 0)), &zones,).is_some());
        assert!(barrier_direction(&Barrier::new(4, (0, 0, 100, 1)), &zones,).is_none());
    }

    #[test]
    fn single_monitor_and_invalid_barriers() {
        let zones = [(2560, 1440, 0, 0)];
        assert!(barrier_direction(&Barrier::new(1, (2560, 0, 2560, 1439)), &zones).is_some());
        assert!(barrier_direction(&Barrier::new(2, (0, 0, 0, 1439)), &zones).is_some());
        assert!(barrier_direction(&Barrier::new(3, (100, 0, 100, 1439)), &zones).is_none());
        assert!(barrier_direction(&Barrier::new(4, (0, 0, 100, 100)), &zones).is_none());
    }

    #[test]
    fn negative_monitor_coordinates_keep_distinct_eis_regions() {
        let regions = vec![
            EiRegion {
                rect: smithay::utils::Rectangle::new((-1920, -100).into(), (1920, 1080).into()),
                scale: 1.25,
                mapping_id: None,
            },
            EiRegion {
                rect: smithay::utils::Rectangle::new((0, 0).into(), (2560, 1440).into()),
                scale: 1.0,
                mapping_id: None,
            },
        ];
        let zones = zones_from_regions(regions.clone());
        assert_eq!(zones, vec![(1920, 1080, -1920, -100), (2560, 1440, 0, 0)]);
        assert_eq!(eis_region_offset(&regions), (1920, 100));
        assert!(barrier_direction(&Barrier::new(1, (-1920, -100, -1920, 979)), &zones).is_some());
        assert!(barrier_direction(&Barrier::new(2, (2560, 0, 2560, 1439)), &zones).is_some());
    }

    #[test]
    fn logical_zones_are_independent_of_output_scale_and_rotation() {
        let regions = vec![EiRegion {
            // A rotated output's *logical* rectangle is the source of truth.
            rect: smithay::utils::Rectangle::new((10, 20).into(), (900, 1600).into()),
            scale: 1.5,
            mapping_id: None,
        }];
        assert_eq!(zones_from_regions(regions), vec![(900, 1600, 10, 20)]);
    }

    #[test]
    fn second_eis_session_is_rejected() {
        let mut capture = InputCaptureState::default();
        let (first, _peer) = UnixStream::pair().unwrap();
        assert!(
            capture
                .connect(
                    "first".into(),
                    ":1.1".into(),
                    DEVICE_KEYBOARD | DEVICE_POINTER,
                    first
                )
                .is_ok()
        );
        let (second, _peer) = UnixStream::pair().unwrap();
        assert!(
            capture
                .connect("second".into(), ":1.1".into(), DEVICE_POINTER, second)
                .is_err()
        );
        assert!(capture.is_session("first"));
    }

    #[test]
    fn hotplug_changes_zone_generation_including_wraparound() {
        assert_eq!(next_zone_set(1), 2);
        assert_eq!(next_zone_set(u32::MAX), 1);
    }

    #[test]
    fn release_cursor_suggestion_is_confined_to_a_zone() {
        let zones = [(1920, 1080, -1920, 0), (2560, 1440, 0, 0)];
        assert_eq!(
            safe_cursor_position((-1.0, 50.0), &zones),
            Some((-1.0, 50.0))
        );
        assert_eq!(safe_cursor_position((0.0, 50.0), &zones), Some((0.0, 50.0)));
        assert_eq!(
            safe_cursor_position((2560.0, 50.0), &zones),
            Some((2559.0, 50.0))
        );
        assert_eq!(safe_cursor_position((99999.0, 50.0), &zones), None);
        assert_eq!(safe_cursor_position((f64::NAN, 50.0), &zones), None);
    }

    #[test]
    fn lock_blocks_new_eis_sessions_without_disarming_existing_capture() {
        let mut capture = InputCaptureState {
            enabled: true,
            locked: true,
            ..Default::default()
        };
        let (stream, _peer) = UnixStream::pair().unwrap();
        assert!(
            capture
                .connect("blocked".into(), ":1.1".into(), DEVICE_POINTER, stream)
                .is_err()
        );
        capture.resume_after_unlock();
        assert!(capture.enabled);
    }

    #[test]
    fn cursor_is_hidden_only_while_capture_is_active_and_unlocked() {
        let mut capture = InputCaptureState {
            active: true,
            ..Default::default()
        };
        assert!(capture.hides_cursor());

        capture.locked = true;
        assert!(!capture.hides_cursor());

        capture.locked = false;
        capture.active = false;
        assert!(!capture.hides_cursor());
    }
}
