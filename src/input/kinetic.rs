// SPDX-License-Identifier: GPL-3.0-only
//
// Compositor-side kinetic (inertial) scrolling for two-finger touchpad scroll.
//
// Buffered finger-source axis deltas feed a closed-form exponential fling on
// lift; synthetic `wl_pointer.axis` events with `AxisSource::Continuous` are
// emitted until velocity decays below `STOP_VELOCITY`. The fling deliberately
// suppresses the `wl_pointer.axis_stop` that libinput would normally emit at
// finger-lift — toolkits (GTK/Qt/Firefox/Chromium) treat that stop event as
// "user input ended, apply your own kinetic." Sending it AND our synthetic
// inertia would compound the two and overshoot.

use crate::state::State;
use calloop::timer::{TimeoutAction, Timer};
use smithay::{
    backend::input::{Axis, AxisSource},
    input::{Seat, pointer::AxisFrame},
    utils::{Logical, Point},
};
use std::{cell::RefCell, collections::VecDeque, time::Duration};
use tracing::trace;

/// Maximum age of samples retained for velocity calculation. Kept slightly
/// larger than [`VELOCITY_WINDOW`] so a momentary pause near lift doesn't
/// starve the velocity estimator.
const SAMPLE_WINDOW: Duration = Duration::from_millis(150);
/// Only samples from the most recent VELOCITY_WINDOW contribute to fling
/// velocity. Biases the estimate toward what the finger was doing *just*
/// before release rather than averaging across the whole gesture.
const VELOCITY_WINDOW: Duration = Duration::from_millis(80);
/// Minimum raw lift velocity (logical units/sec) required to start a fling.
const MIN_FLING_VELOCITY: f64 = 50.0;
/// Velocity (logical units/sec) at which the fling terminates.
const STOP_VELOCITY: f64 = 15.0;
/// Per-millisecond friction; matches `DECELERATION_TOUCHPAD` in
/// `input/gestures/mod.rs`.
const FRICTION_PER_MS: f64 = 0.997;
/// Cap on initial fling velocity so an extreme flick doesn't whip-scroll the
/// entire page in one frame.
const MAX_FLING_VELOCITY: f64 = 4000.0;
/// 120 v120 units == one wheel detent == ~15 logical pixels (Wayland
/// convention). Synthesising v120 alongside Continuous-source frames helps
/// clients that gate kinetic-style handling on the presence of high-res
/// wheel steps (Chromium in particular).
const V120_PER_LOGICAL: f64 = 8.0;
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
    /// Accumulated fractional v120 not yet emitted. Carries the rounding
    /// remainder across ticks so the integer stream stays smooth even as
    /// the fling decays toward zero.
    v120_residual: (f64, f64),
}

impl Inner {
    fn clear_fling(&mut self) {
        self.active = false;
        self.velocity = Point::default();
        self.last_tick = None;
        self.v120_residual = (0.0, 0.0);
    }
}

impl KineticScroller {
    /// Buffer a finger delta. If a fling was running, cancel it and return
    /// `true` (caller emits a Continuous `axis_stop` for the synthetic stream).
    pub fn push(&self, delta: Point<f64, Logical>, t: Duration) -> bool {
        let mut inner = self.0.borrow_mut();
        let was_active = inner.active;
        if was_active {
            inner.clear_fling();
        }

        if let Some(last) = inner.samples.back()
            && t < last.time
        {
            trace!(
                "ignoring kinetic sample with timestamp {t:?} earlier than last {:?}",
                last.time
            );
            return was_active;
        }

        inner.samples.push_back(Sample { delta, time: t });
        Self::trim(&mut inner.samples, t);
        was_active
    }

    /// Compute velocity from buffered samples and, if above
    /// `MIN_FLING_VELOCITY`, start the fling. Returns the initial velocity
    /// when a fling actually starts.
    pub fn start_fling(&self) -> Option<Point<f64, Logical>> {
        let mut inner = self.0.borrow_mut();
        let Some(last) = inner.samples.back().copied() else {
            inner.samples.clear();
            return None;
        };

        let cutoff = last.time.saturating_sub(VELOCITY_WINDOW);
        let (count, total, first_time) = inner.samples.iter().filter(|s| s.time >= cutoff).fold(
            (0usize, Point::<f64, Logical>::default(), None::<Duration>),
            |(n, acc, first), s| (n + 1, acc + s.delta, Some(first.unwrap_or(s.time))),
        );
        inner.samples.clear();

        if count < 2 {
            return None;
        }
        let first_time = first_time.unwrap();
        let span = last.time.saturating_sub(first_time).as_secs_f64();
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

    /// Advance the fling. Returns the delta to emit, or `None` when the fling
    /// has decayed below `STOP_VELOCITY` (also clears `active`).
    pub fn tick(&self, now: Duration) -> Option<Point<f64, Logical>> {
        let mut inner = self.0.borrow_mut();
        if !inner.active {
            return None;
        }

        let last = inner.last_tick.unwrap_or(now);
        let dt_ms = now.saturating_sub(last).as_secs_f64() * 1000.0;
        inner.last_tick = Some(now);

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
            inner.clear_fling();
            return None;
        }
        Some(delta)
    }

    /// Compute integer v120 step counts for the given pixel delta, carrying
    /// the rounding remainder forward so consecutive frames don't oscillate
    /// between 0 and 1 at low velocity.
    fn next_v120(&self, delta: Point<f64, Logical>) -> (i32, i32) {
        let mut inner = self.0.borrow_mut();
        let tx = delta.x * V120_PER_LOGICAL + inner.v120_residual.0;
        let ty = delta.y * V120_PER_LOGICAL + inner.v120_residual.1;
        let ex = tx.round() as i32;
        let ey = ty.round() as i32;
        inner.v120_residual = (tx - ex as f64, ty - ey as f64);
        (ex, ey)
    }

    pub fn is_active(&self) -> bool {
        self.0.borrow().active
    }

    pub fn cancel(&self) -> bool {
        let mut inner = self.0.borrow_mut();
        let was_active = inner.active;
        inner.clear_fling();
        inner.samples.clear();
        was_active
    }

    fn trim(samples: &mut VecDeque<Sample>, latest: Duration) {
        while let Some(front) = samples.front() {
            if latest <= front.time + SAMPLE_WINDOW {
                break;
            }
            samples.pop_front();
        }
    }
}

// ---------------------------------------------------------------------------
// Plumbing: protocol I/O glued to a seat's `KineticScroller`. Called from
// `src/input/mod.rs` and `src/shell/focus/mod.rs`.

/// Emit a `Continuous` source axis_stop frame to the seat's pointer.
fn emit_stop(state: &mut State, seat: &Seat<State>, time_msec: u32) {
    let frame = AxisFrame::new(time_msec)
        .source(AxisSource::Continuous)
        .stop(Axis::Horizontal)
        .stop(Axis::Vertical);
    let ptr = seat.get_pointer().unwrap();
    ptr.axis(state, frame);
    ptr.frame(state);
}

/// Cancel any running fling on this seat. Emits a Continuous `axis_stop`
/// if there was an active fling so kinetic-aware clients know the synthetic
/// stream ended.
pub fn cancel_with_stop(state: &mut State, seat: &Seat<State>, time_msec: u32) {
    if let Some(scroller) = seat.user_data().get::<KineticScroller>()
        && scroller.cancel()
    {
        emit_stop(state, seat, time_msec);
    }
}

/// Buffer a finger delta. If a fling was previously running, this cancels it
/// and emits a Continuous `axis_stop` before the caller's Finger frame so
/// clients see a clean source transition.
pub fn push_delta(
    state: &mut State,
    seat: &Seat<State>,
    delta: Point<f64, Logical>,
    t: Duration,
    time_msec: u32,
) {
    if let Some(scroller) = seat.user_data().get::<KineticScroller>()
        && scroller.push(delta, t)
    {
        emit_stop(state, seat, time_msec);
    }
}

/// Try to start a fling from the buffered samples. Returns true if one was
/// started — caller should skip its own `axis_stop` emission, the fling will
/// deliver it when it decays.
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

    // Capture active state *before* `tick` so we can distinguish a natural
    // decay end (active was true, tick returned None → emit stop) from a
    // stale timer firing after an external cancellation (active already
    // false → cancel already emitted the stop, sending another here would
    // land after the next gesture's Finger frames and confuse clients like
    // Firefox that treat it as a flush signal).
    let was_active = seat
        .user_data()
        .get::<KineticScroller>()
        .is_some_and(|s| s.is_active());

    let result = seat
        .user_data()
        .get::<KineticScroller>()
        .and_then(|s| s.tick(now));

    match result {
        Some(delta) => {
            let scroller = seat.user_data().get::<KineticScroller>().unwrap();
            let (v120_x, v120_y) = scroller.next_v120(delta);

            let mut frame = AxisFrame::new(time_msec).source(AxisSource::Continuous);
            if delta.x != 0.0 {
                frame = frame.value(Axis::Horizontal, delta.x);
                if v120_x != 0 {
                    frame = frame.v120(Axis::Horizontal, v120_x);
                }
            }
            if delta.y != 0.0 {
                frame = frame.value(Axis::Vertical, delta.y);
                if v120_y != 0 {
                    frame = frame.v120(Axis::Vertical, v120_y);
                }
            }
            let ptr = seat.get_pointer().unwrap();
            ptr.axis(state, frame);
            ptr.frame(state);
            schedule_tick(state, seat);
        }
        None => {
            if was_active {
                emit_stop(state, seat, time_msec);
            }
        }
    }
}
