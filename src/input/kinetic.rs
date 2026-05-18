// SPDX-License-Identifier: GPL-3.0-only
//
// Compositor-side kinetic (inertial) scrolling for two-finger touchpad scroll.
//
// Buffered finger-source axis deltas feed a closed-form exponential fling on
// lift; synthetic `wl_pointer.axis` events with `AxisSource::Continuous` are
// emitted until velocity decays below `STOP_VELOCITY`. When kinetic is active
// the same `Continuous` source is also used for live finger scroll (see the
// PointerAxis arm in `mod.rs`), so the fling and the gesture before it look
// like one uninterrupted stream to the client — no source transitions, no
// `axis_stop` frames either way, since `Continuous` does not require them.

use crate::state::State;
use calloop::timer::{TimeoutAction, Timer};
use smithay::{
    backend::input::{Axis, AxisSource},
    input::{Seat, pointer::AxisFrame},
    utils::{Logical, Point},
};
use std::{cell::RefCell, collections::VecDeque, time::Duration};

/// Samples within this window of the most recent push contribute to the
/// fling velocity at lift; older samples are dropped by [`trim`]. Biases
/// the velocity estimate toward what the finger was doing *just* before
/// release rather than averaging across the whole gesture.
const VELOCITY_WINDOW: Duration = Duration::from_millis(80);
/// Minimum raw lift velocity (logical units/sec) required to start a fling.
const MIN_FLING_VELOCITY: f64 = 50.0;
/// Velocity (logical units/sec) at which exponential decay hands over to the
/// smooth-stop tail.
const STOP_VELOCITY: f64 = 15.0;
/// Per-millisecond friction during the exponential decay phase.
const FRICTION_PER_MS: f64 = 0.995;
/// Cap on initial fling velocity so an extreme flick doesn't whip-scroll the
/// entire page in one frame.
const MAX_FLING_VELOCITY: f64 = 4000.0;
/// Duration of the smooth-stop tail (cubic ease-out to zero) that follows
/// exponential decay. Without it, the fling cuts off abruptly at
/// `STOP_VELOCITY` which can feel like a snap on low-precision scroll
/// consumers (terminals, XWayland clients).
const TAIL_DURATION_MS: f64 = 80.0;
/// Tick interval for the fling timer (~125 Hz).
const TICK: Duration = Duration::from_millis(8);

#[derive(Clone, Copy, Debug)]
struct Sample {
    delta: Point<f64, Logical>,
    time: Duration,
}

#[derive(Default)]
pub struct KineticScroller(RefCell<Inner>);

#[derive(Default)]
struct Inner {
    samples: VecDeque<Sample>,
    velocity: Point<f64, Logical>,
    active: bool,
    last_tick: Option<Duration>,
    /// True once exponential decay has dropped below [`STOP_VELOCITY`] and
    /// we've switched to the cubic-ease-out tail.
    in_tail: bool,
    tail_start: Option<Duration>,
    tail_initial_velocity: Point<f64, Logical>,
}

impl Inner {
    fn clear_fling(&mut self) {
        self.active = false;
        self.velocity = Point::default();
        self.last_tick = None;
        self.in_tail = false;
        self.tail_start = None;
        self.tail_initial_velocity = Point::default();
    }
}

impl KineticScroller {
    /// Buffer a finger delta. If a fling was running, cancel it.
    fn push(&self, delta: Point<f64, Logical>, t: Duration) {
        let mut inner = self.0.borrow_mut();
        if inner.active {
            inner.clear_fling();
        }
        inner.samples.push_back(Sample { delta, time: t });
        Self::trim(&mut inner.samples, t);
    }

    /// Compute velocity from buffered samples and, if above
    /// `MIN_FLING_VELOCITY`, start the fling. Returns the initial velocity
    /// when a fling actually starts. The buffer is already trimmed to
    /// `VELOCITY_WINDOW` by [`push`], so `first`/`last` are the velocity
    /// window directly — no further filtering needed.
    fn start_fling(&self) -> Option<Point<f64, Logical>> {
        let mut inner = self.0.borrow_mut();
        let (first, last) = inner.samples.front().zip(inner.samples.back())?;
        let span = last.time.saturating_sub(first.time).as_secs_f64();
        let total = inner
            .samples
            .iter()
            .fold(Point::<f64, Logical>::default(), |acc, s| acc + s.delta);
        inner.samples.clear();

        if span <= 0.0 {
            return None;
        }
        let raw = Point::<f64, Logical>::from((total.x / span, total.y / span));
        if raw.x.abs() < MIN_FLING_VELOCITY && raw.y.abs() < MIN_FLING_VELOCITY {
            return None;
        }

        let velocity = Point::<f64, Logical>::from((
            raw.x.clamp(-MAX_FLING_VELOCITY, MAX_FLING_VELOCITY),
            raw.y.clamp(-MAX_FLING_VELOCITY, MAX_FLING_VELOCITY),
        ));
        inner.clear_fling();
        inner.velocity = velocity;
        inner.active = true;
        Some(velocity)
    }

    /// Advance the fling. Returns the delta to emit, or `None` once the
    /// exponential-decay-then-tail sequence completes (also clears `active`).
    fn tick(&self, now: Duration) -> Option<Point<f64, Logical>> {
        let mut inner = self.0.borrow_mut();
        if !inner.active {
            return None;
        }

        let last = inner.last_tick.unwrap_or(now);
        let dt_ms = now.saturating_sub(last).as_secs_f64() * 1000.0;
        inner.last_tick = Some(now);

        if inner.in_tail {
            let start = inner
                .tail_start
                .expect("tail_start set when in_tail is true");
            let elapsed_ms = now.saturating_sub(start).as_secs_f64() * 1000.0;
            if elapsed_ms >= TAIL_DURATION_MS {
                inner.clear_fling();
                return None;
            }
            // Cubic ease-out position curve: velocity(t) = v0 * (1 - t/T)^2.
            let t_norm = elapsed_ms / TAIL_DURATION_MS;
            let v_factor = (1.0 - t_norm).powi(2);
            let v = inner.tail_initial_velocity;
            let cur_v = (v.x * v_factor, v.y * v_factor);
            let dt_s = dt_ms / 1000.0;
            inner.velocity = Point::from(cur_v);
            return Some(Point::from((cur_v.0 * dt_s, cur_v.1 * dt_s)));
        }

        let delta = if dt_ms > 0.0 {
            let factor = FRICTION_PER_MS.powf(dt_ms);
            let scale = (factor - 1.0) / FRICTION_PER_MS.ln() / 1000.0;
            let v = inner.velocity;
            let out = Point::from((v.x * scale, v.y * scale));
            inner.velocity = Point::from((v.x * factor, v.y * factor));
            out
        } else {
            Point::default()
        };

        if inner.velocity.x.abs() < STOP_VELOCITY && inner.velocity.y.abs() < STOP_VELOCITY {
            // Hand off from exponential decay to the smooth-stop tail.
            inner.in_tail = true;
            inner.tail_start = Some(now);
            inner.tail_initial_velocity = inner.velocity;
        }
        Some(delta)
    }

    fn cancel_inner(&self) {
        let mut inner = self.0.borrow_mut();
        inner.clear_fling();
        inner.samples.clear();
    }

    fn trim(samples: &mut VecDeque<Sample>, latest: Duration) {
        while let Some(front) = samples.front() {
            if latest <= front.time + VELOCITY_WINDOW {
                break;
            }
            samples.pop_front();
        }
    }
}

// ---------------------------------------------------------------------------
// Plumbing API used by `src/input/mod.rs` and `src/shell/focus/mod.rs`. Kept
// as free functions so callers don't have to thread `KineticScroller`
// references manually — they just have a seat.

/// Cancel any running fling and discard buffered samples for this seat. No
/// `axis_stop` frame is emitted; the kinetic stream uses `AxisSource::Continuous`
/// which does not require one.
pub fn cancel(seat: &Seat<State>) {
    if let Some(scroller) = seat.user_data().get::<KineticScroller>() {
        scroller.cancel_inner();
    }
}

/// Buffer a finger delta. Cancels any running fling so a renewed gesture
/// starts cleanly.
pub fn push(seat: &Seat<State>, delta: Point<f64, Logical>, t: Duration) {
    if let Some(scroller) = seat.user_data().get::<KineticScroller>() {
        scroller.push(delta, t);
    }
}

/// Try to start a fling from buffered samples. Returns true if one started —
/// caller should suppress emitting any axis frame for this lift; the fling
/// will emit decay frames itself.
pub fn try_start_fling(state: &mut State, seat: &Seat<State>) -> bool {
    let started = seat
        .user_data()
        .get::<KineticScroller>()
        .and_then(|s| s.start_fling())
        .is_some();
    if started {
        schedule_tick(state, seat);
    }
    started
}

fn schedule_tick(state: &mut State, seat: &Seat<State>) {
    let seat = seat.clone();
    let _ = state.common.event_loop_handle.insert_source(
        Timer::from_duration(TICK),
        move |_, _, state| {
            tick_handler(state, &seat);
            TimeoutAction::Drop
        },
    );
}

fn tick_handler(state: &mut State, seat: &Seat<State>) {
    let now_time = state.common.clock.now();
    let time_msec = now_time.as_millis();
    let now: Duration = now_time.into();

    let result = seat
        .user_data()
        .get::<KineticScroller>()
        .and_then(|s| s.tick(now));

    let Some(delta) = result else {
        // Fling ended (decayed below STOP_VELOCITY) or was cancelled out from
        // under the timer. No `axis_stop` emission — Continuous source does
        // not require it.
        return;
    };

    let mut frame = AxisFrame::new(time_msec).source(AxisSource::Continuous);
    if delta.x != 0.0 {
        frame = frame.value(Axis::Horizontal, delta.x);
    }
    if delta.y != 0.0 {
        frame = frame.value(Axis::Vertical, delta.y);
    }
    let ptr = seat.get_pointer().unwrap();
    ptr.axis(state, frame);
    ptr.frame(state);
    schedule_tick(state, seat);
}
