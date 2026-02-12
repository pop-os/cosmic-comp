// SPDX-License-Identifier: GPL-3.0-only

//! EIS (Emulated Input Server) receiver for remote desktop input injection.
//!
//! Accepts input events from EIS clients (e.g., xdg-desktop-portal-cosmic
//! RemoteDesktop sessions) and injects them into the compositor's input stack
//! using the same Smithay APIs as the virtual keyboard handler.

use calloop::channel;
use reis::{
    PendingRequestResult, eis,
    event::DeviceCapability,
    handshake::{self, EisHandshaker},
    request::{EisRequest, EisRequestConverter},
};
use smithay::{
    backend::input::{KeyState, TouchSlot},
    input::{
        keyboard::{FilterResult, Keycode},
        touch::{DownEvent, MotionEvent as TouchMotionEvent, UpEvent},
    },
    utils::SERIAL_COUNTER,
};
use std::os::unix::net::UnixStream;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use tracing::{debug, error, info, warn};

use crate::state::State;
use crate::utils::prelude::{OutputExt, PointGlobalExt};

/// Events sent from the EIS processing thread to the compositor's calloop.
#[derive(Debug)]
pub enum EisInputEvent {
    /// Keyboard key press/release (evdev keycode)
    KeyboardKey { keycode: u32, pressed: bool },
    /// Relative pointer motion
    PointerMotion { dx: f64, dy: f64 },
    /// Absolute pointer motion (for absolute positioning devices)
    PointerMotionAbsolute { x: f64, y: f64 },
    /// Mouse button press/release (linux button code, e.g., BTN_LEFT=0x110)
    Button { button: u32, pressed: bool },
    /// Scroll delta (smooth scrolling)
    Scroll { dx: f64, dy: f64 },
    /// Touch down (finger placed on surface)
    TouchDown { touch_id: u32, x: f64, y: f64 },
    /// Touch motion (finger moved on surface)
    TouchMotion { touch_id: u32, x: f64, y: f64 },
    /// Touch up (finger lifted from surface)
    TouchUp { touch_id: u32 },
    /// Touch cancel (touch sequence cancelled)
    TouchCancel { touch_id: u32 },
    /// Client disconnected
    Disconnected,
}

/// Maximum number of concurrent EIS connections allowed.
const MAX_EIS_CONNECTIONS: usize = 8;

/// Poll timeout for EIS socket operations (30 seconds).
const POLL_TIMEOUT: rustix::event::Timespec = rustix::event::Timespec {
    tv_sec: 30,
    tv_nsec: 0,
};

/// Manages EIS connections and routes input events to the compositor.
#[derive(Debug)]
pub struct EisState {
    /// Channel sender to inject events into calloop
    pub(crate) tx: channel::Sender<EisInputEvent>,
    /// Number of active EIS connections (shared with background threads)
    active_connections: Arc<AtomicUsize>,
}

impl EisState {
    /// Create a new EIS state and register the event channel with calloop.
    ///
    /// Returns the `EisState` which can accept new socket connections.
    pub fn new(evlh: &calloop::LoopHandle<'static, State>) -> anyhow::Result<Self> {
        let (tx, rx) = channel::channel::<EisInputEvent>();

        evlh.insert_source(rx, |event, _, state| {
            if let channel::Event::Msg(eis_event) = event {
                process_eis_event(state, eis_event);
            }
        })
        .map_err(|e| anyhow::anyhow!("Failed to insert EIS channel source: {}", e.error))?;

        info!("EIS input receiver initialized");
        Ok(Self {
            tx,
            active_connections: Arc::new(AtomicUsize::new(0)),
        })
    }

    /// Accept a new EIS client connection from a UNIX socket fd.
    ///
    /// Spawns a background thread that reads EIS protocol events from the
    /// socket and forwards them as `EisInputEvent` through the calloop channel.
    /// Rejects the connection if the maximum number of concurrent connections
    /// has been reached.
    pub fn add_connection(&self, socket: UnixStream) {
        // Atomically check-and-increment to avoid TOCTOU race.
        loop {
            let current = self.active_connections.load(Ordering::Acquire);
            if current >= MAX_EIS_CONNECTIONS {
                warn!(
                    current,
                    max = MAX_EIS_CONNECTIONS,
                    "Rejecting EIS connection: limit reached"
                );
                return;
            }
            if self
                .active_connections
                .compare_exchange(current, current + 1, Ordering::AcqRel, Ordering::Acquire)
                .is_ok()
            {
                break;
            }
        }
        let tx = self.tx.clone();
        let counter = Arc::clone(&self.active_connections);
        let active = self.active_connections.load(Ordering::Acquire);
        info!(active, "Accepting new EIS client connection");

        let thread_name = format!("eis-client-{active}");
        std::thread::Builder::new()
            .name(thread_name)
            .spawn(move || {
                let result = run_eis_server(socket, tx);
                let remaining = counter.fetch_sub(1, Ordering::AcqRel) - 1;
                match result {
                    Ok(()) => info!(active = remaining, "EIS connection closed"),
                    Err(err) => error!(active = remaining, "EIS server error: {}", err),
                }
            })
            .expect("failed to spawn EIS thread");
    }
}

/// Perform the EIS server-side handshake (blocking, with timeout).
fn eis_handshake(context: &eis::Context) -> Result<handshake::EisHandshakeResp, reis::Error> {
    let mut handshaker = EisHandshaker::new(context, 0);
    // Flush the initial handshake version message
    context.flush().map_err(|e| reis::Error::Io(e.into()))?;

    loop {
        // Block until data is available (with timeout)
        let n = rustix::event::poll(
            &mut [rustix::event::PollFd::new(
                context,
                rustix::event::PollFlags::IN,
            )],
            Some(&POLL_TIMEOUT),
        )
        .map_err(|e| reis::Error::Io(e.into()))?;

        if n == 0 {
            return Err(reis::Error::Io(std::io::Error::new(
                std::io::ErrorKind::TimedOut,
                "EIS handshake timed out",
            )));
        }

        context.read().map_err(reis::Error::Io)?;

        while let Some(result) = context.pending_request() {
            let request = match result {
                PendingRequestResult::Request(r) => r,
                PendingRequestResult::ParseError(e) => return Err(reis::Error::Parse(e)),
                PendingRequestResult::InvalidObject(id) => {
                    return Err(handshake::HandshakeError::InvalidObject(id).into());
                }
            };

            if let Some(resp) = handshaker
                .handle_request(request)
                .map_err(reis::Error::Handshake)?
            {
                return Ok(resp);
            }
        }
    }
}

/// Run the EIS server protocol on a socket, forwarding events to the compositor.
fn run_eis_server(socket: UnixStream, tx: channel::Sender<EisInputEvent>) -> anyhow::Result<()> {
    /// Send an event to the compositor channel, returning from the enclosing
    /// function if the channel is closed.
    macro_rules! send_or_return {
        ($tx:expr, $event:expr) => {
            if $tx.send($event).is_err() {
                debug!("Compositor channel closed");
                return Ok(());
            }
        };
    }
    let context = eis::Context::new(socket)
        .map_err(|e| anyhow::anyhow!("failed to create EIS context from socket: {e}"))?;

    // Perform server-side handshake
    let handshake_resp = eis_handshake(&context)?;
    // Truncate client name to prevent log flooding from malicious clients.
    let client_name: String = handshake_resp
        .name
        .as_deref()
        .unwrap_or("<unknown>")
        .chars()
        .take(128)
        .collect();
    debug!(
        client = %client_name,
        context_type = ?handshake_resp.context_type,
        "EIS handshake complete"
    );

    // Create the request converter which manages seats, devices, and events
    let mut converter = EisRequestConverter::new(&context, handshake_resp, 0);

    // Add a seat with keyboard + pointer capabilities.
    // The seat is stored in the converter's connection state; _seat keeps it alive.
    let _seat = converter.handle().add_seat(
        Some("seat0"),
        &[
            DeviceCapability::Keyboard,
            DeviceCapability::Pointer,
            DeviceCapability::PointerAbsolute,
            DeviceCapability::Button,
            DeviceCapability::Scroll,
            DeviceCapability::Touch,
        ],
    );
    converter.handle().flush()?;

    // Main event loop
    loop {
        // Block until data is available (with timeout)
        let n = rustix::event::poll(
            &mut [rustix::event::PollFd::new(
                &context,
                rustix::event::PollFlags::IN,
            )],
            Some(&POLL_TIMEOUT),
        )
        .map_err(std::io::Error::from)?;

        if n == 0 {
            // Timeout with no data - check if channel is still open
            if tx.send(EisInputEvent::Disconnected).is_err() {
                debug!("Compositor channel closed during poll timeout");
                return Ok(());
            }
            // Channel alive but client idle - send disconnect and exit
            info!("EIS client idle timeout, disconnecting");
            return Ok(());
        }

        let bytes_read = context.read()?;
        if bytes_read == 0 {
            info!("EIS connection closed (EOF)");
            let _ = tx.send(EisInputEvent::Disconnected);
            break;
        }

        // Process raw protocol requests through the converter
        while let Some(result) = context.pending_request() {
            let raw_request = match result {
                PendingRequestResult::Request(r) => r,
                PendingRequestResult::ParseError(e) => {
                    warn!("EIS parse error: {}", e);
                    continue;
                }
                PendingRequestResult::InvalidObject(id) => {
                    debug!("EIS invalid object: {}", id);
                    continue;
                }
            };

            if let Err(err) = converter.handle_request(raw_request) {
                warn!("EIS request handling error: {}", err);
                continue;
            }
        }

        // Drain high-level events from the converter
        while let Some(eis_request) = converter.next_request() {
            match eis_request {
                EisRequest::KeyboardKey(key_evt) => {
                    let pressed = key_evt.state == eis::keyboard::KeyState::Press;
                    send_or_return!(
                        tx,
                        EisInputEvent::KeyboardKey {
                            keycode: key_evt.key,
                            pressed,
                        }
                    );
                }
                EisRequest::PointerMotion(motion) => {
                    send_or_return!(
                        tx,
                        EisInputEvent::PointerMotion {
                            dx: f64::from(motion.dx),
                            dy: f64::from(motion.dy),
                        }
                    );
                }
                EisRequest::PointerMotionAbsolute(motion) => {
                    send_or_return!(
                        tx,
                        EisInputEvent::PointerMotionAbsolute {
                            x: f64::from(motion.dx_absolute),
                            y: f64::from(motion.dy_absolute),
                        }
                    );
                }
                EisRequest::Button(btn) => {
                    let pressed = btn.state == eis::button::ButtonState::Press;
                    send_or_return!(
                        tx,
                        EisInputEvent::Button {
                            button: btn.button,
                            pressed,
                        }
                    );
                }
                EisRequest::ScrollDelta(scroll) => {
                    send_or_return!(
                        tx,
                        EisInputEvent::Scroll {
                            dx: f64::from(scroll.dx),
                            dy: f64::from(scroll.dy),
                        }
                    );
                }
                EisRequest::Disconnect => {
                    info!("EIS client disconnected");
                    let _ = tx.send(EisInputEvent::Disconnected);
                    return Ok(());
                }
                EisRequest::Bind(bind) => {
                    // Client bound to seat capabilities - add device and resume
                    let capabilities = capabilities_from_mask(bind.capabilities);
                    debug!("EIS client bound with capabilities: {:?}", capabilities);
                    let device = bind.seat.add_device(
                        Some("remote-input"),
                        eis::device::DeviceType::Virtual,
                        &capabilities,
                        |_| {},
                    );
                    device.resumed();
                    if let Err(e) = converter.handle().flush() {
                        warn!("Failed to flush EIS device announcement: {e}");
                    }
                }
                EisRequest::DeviceStartEmulating(_) | EisRequest::DeviceStopEmulating(_) => {
                    // Acknowledged implicitly
                }
                EisRequest::TouchDown(touch) => {
                    send_or_return!(
                        tx,
                        EisInputEvent::TouchDown {
                            touch_id: touch.touch_id,
                            x: f64::from(touch.x),
                            y: f64::from(touch.y),
                        }
                    );
                }
                EisRequest::TouchMotion(touch) => {
                    send_or_return!(
                        tx,
                        EisInputEvent::TouchMotion {
                            touch_id: touch.touch_id,
                            x: f64::from(touch.x),
                            y: f64::from(touch.y),
                        }
                    );
                }
                EisRequest::TouchUp(touch) => {
                    send_or_return!(
                        tx,
                        EisInputEvent::TouchUp {
                            touch_id: touch.touch_id,
                        }
                    );
                }
                EisRequest::TouchCancel(touch) => {
                    send_or_return!(
                        tx,
                        EisInputEvent::TouchCancel {
                            touch_id: touch.touch_id,
                        }
                    );
                }
                EisRequest::Frame(_) => {
                    // Frame boundaries - we process events individually
                }
                _ => {
                    debug!("Unhandled EIS request: {:?}", eis_request);
                }
            }
        }
    }

    Ok(())
}

/// Convert a capability bitmask to a list of DeviceCapability values.
fn capabilities_from_mask(mask: u64) -> Vec<DeviceCapability> {
    let mut caps = Vec::new();
    for cap in [
        DeviceCapability::Pointer,
        DeviceCapability::PointerAbsolute,
        DeviceCapability::Keyboard,
        DeviceCapability::Touch,
        DeviceCapability::Scroll,
        DeviceCapability::Button,
    ] {
        if mask & (2 << cap as u64) != 0 {
            caps.push(cap);
        }
    }
    caps
}

/// Resolve the surface under a given position, acquiring and releasing the
/// shell read lock. Used by touch events that need both the seat and the
/// surface target but must release the lock before calling into Smithay
/// (which needs `&mut State`).
#[allow(clippy::type_complexity)]
fn resolve_touch_target(
    state: &State,
    x: f64,
    y: f64,
) -> (
    smithay::input::Seat<State>,
    Option<(
        <State as smithay::input::SeatHandler>::PointerFocus,
        smithay::utils::Point<f64, smithay::utils::Logical>,
    )>,
) {
    let shell = state.common.shell.read();
    let seat = shell.seats.last_active().clone();
    let position = (x, y).into();
    let under = shell
        .outputs()
        .find(|output| output.geometry().to_f64().contains(position))
        .and_then(|output| {
            State::surface_under(position, output, &shell)
                .map(|(target, pos)| (target, pos.as_logical()))
        });
    (seat, under)
}

/// Returns `true` if the value is finite (not NaN or infinity).
fn is_finite_f64(v: f64) -> bool {
    v.is_finite()
}

/// Maximum valid evdev keycode (KEY_MAX from linux/input-event-codes.h).
const MAX_EVDEV_KEYCODE: u32 = 0x2FF;

/// Maximum touch slot ID (generous upper bound; real devices rarely exceed 20).
const MAX_TOUCH_ID: u32 = 256;

/// Process a single EIS input event by injecting it into the compositor's
/// Smithay input stack.
fn process_eis_event(state: &mut State, event: EisInputEvent) {
    let time = state.common.clock.now().as_millis();

    match event {
        EisInputEvent::KeyboardKey { keycode, pressed } => {
            if keycode > MAX_EVDEV_KEYCODE {
                warn!(keycode, "Rejecting keyboard event: keycode out of range");
                return;
            }
            let seat = state.common.shell.read().seats.last_active().clone();
            if let Some(keyboard) = seat.get_keyboard() {
                let serial = SERIAL_COUNTER.next_serial();
                let key_state = if pressed {
                    KeyState::Pressed
                } else {
                    KeyState::Released
                };
                keyboard.input(
                    state,
                    Keycode::new(keycode),
                    key_state,
                    serial,
                    time,
                    |_, _, _| FilterResult::Forward::<bool>,
                );
            }
        }
        EisInputEvent::PointerMotion { dx, dy } => {
            if !is_finite_f64(dx) || !is_finite_f64(dy) {
                warn!("Rejecting pointer motion: non-finite delta");
                return;
            }
            let seat = state.common.shell.read().seats.last_active().clone();
            if let Some(pointer) = seat.get_pointer() {
                let current = pointer.current_location();
                let new_location = (current.x + dx, current.y + dy).into();
                let serial = SERIAL_COUNTER.next_serial();
                pointer.motion(
                    state,
                    None,
                    &smithay::input::pointer::MotionEvent {
                        location: new_location,
                        serial,
                        time,
                    },
                );
                pointer.frame(state);
            }
        }
        EisInputEvent::PointerMotionAbsolute { x, y } => {
            if !is_finite_f64(x) || !is_finite_f64(y) {
                warn!("Rejecting absolute pointer motion: non-finite coordinates");
                return;
            }
            let seat = state.common.shell.read().seats.last_active().clone();
            if let Some(pointer) = seat.get_pointer() {
                let serial = SERIAL_COUNTER.next_serial();
                pointer.motion(
                    state,
                    None,
                    &smithay::input::pointer::MotionEvent {
                        location: (x, y).into(),
                        serial,
                        time,
                    },
                );
                pointer.frame(state);
            }
        }
        EisInputEvent::Button { button, pressed } => {
            if button > MAX_EVDEV_KEYCODE {
                warn!(button, "Rejecting button event: code out of range");
                return;
            }
            let seat = state.common.shell.read().seats.last_active().clone();
            if let Some(pointer) = seat.get_pointer() {
                let serial = SERIAL_COUNTER.next_serial();
                let state_val = if pressed {
                    smithay::backend::input::ButtonState::Pressed
                } else {
                    smithay::backend::input::ButtonState::Released
                };
                pointer.button(
                    state,
                    &smithay::input::pointer::ButtonEvent {
                        button,
                        state: state_val,
                        serial,
                        time,
                    },
                );
                pointer.frame(state);
            }
        }
        EisInputEvent::Scroll { dx, dy } => {
            if !is_finite_f64(dx) || !is_finite_f64(dy) {
                warn!("Rejecting scroll event: non-finite delta");
                return;
            }
            let seat = state.common.shell.read().seats.last_active().clone();
            if let Some(pointer) = seat.get_pointer() {
                use smithay::backend::input::Axis;
                let mut frame = smithay::input::pointer::AxisFrame::new(time);
                if dy.abs() > 0.0 {
                    frame = frame.value(Axis::Vertical, dy);
                }
                if dx.abs() > 0.0 {
                    frame = frame.value(Axis::Horizontal, dx);
                }
                pointer.axis(state, frame);
                pointer.frame(state);
            }
        }
        EisInputEvent::TouchDown { touch_id, x, y } => {
            if touch_id > MAX_TOUCH_ID {
                warn!(touch_id, "Rejecting touch down: ID out of range");
                return;
            }
            if !is_finite_f64(x) || !is_finite_f64(y) {
                warn!("Rejecting touch down: non-finite coordinates");
                return;
            }
            let (seat, under) = resolve_touch_target(state, x, y);
            if let Some(touch) = seat.get_touch() {
                let serial = SERIAL_COUNTER.next_serial();
                touch.down(
                    state,
                    under,
                    &DownEvent {
                        slot: TouchSlot::from(Some(touch_id)),
                        location: (x, y).into(),
                        serial,
                        time,
                    },
                );
                touch.frame(state);
            }
        }
        EisInputEvent::TouchMotion { touch_id, x, y } => {
            if touch_id > MAX_TOUCH_ID {
                warn!(touch_id, "Rejecting touch motion: ID out of range");
                return;
            }
            if !is_finite_f64(x) || !is_finite_f64(y) {
                warn!("Rejecting touch motion: non-finite coordinates");
                return;
            }
            let (seat, under) = resolve_touch_target(state, x, y);
            if let Some(touch) = seat.get_touch() {
                touch.motion(
                    state,
                    under,
                    &TouchMotionEvent {
                        slot: TouchSlot::from(Some(touch_id)),
                        location: (x, y).into(),
                        time,
                    },
                );
                touch.frame(state);
            }
        }
        EisInputEvent::TouchUp { touch_id } => {
            let seat = state.common.shell.read().seats.last_active().clone();
            if let Some(touch) = seat.get_touch() {
                let serial = SERIAL_COUNTER.next_serial();
                touch.up(
                    state,
                    &UpEvent {
                        slot: TouchSlot::from(Some(touch_id)),
                        time,
                        serial,
                    },
                );
                touch.frame(state);
            }
        }
        EisInputEvent::TouchCancel { touch_id: _ } => {
            let seat = state.common.shell.read().seats.last_active().clone();
            if let Some(touch) = seat.get_touch() {
                touch.cancel(state);
                touch.frame(state);
            }
        }
        EisInputEvent::Disconnected => {
            info!("EIS client disconnected, cleaning up");
        }
    }
}
