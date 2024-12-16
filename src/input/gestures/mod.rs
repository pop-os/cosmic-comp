use cosmic_settings_config::shortcuts::action::Direction;
use smithay::utils::{Logical, Point};
use std::{collections::VecDeque, time::Duration};
use tracing::trace;

const HISTORY_LIMIT: Duration = Duration::from_millis(150);
const DECELERATION_TOUCHPAD: f64 = 0.997;

#[derive(Debug, Clone, Copy)]
pub struct SwipeEvent {
    delta: f64,
    timestamp: Duration,
}

#[derive(Debug, Clone, Copy)]
pub enum SwipeAction {
    NextWorkspace,
    PrevWorkspace,
}

#[derive(Debug, Clone)]
pub struct GestureState {
    pub fingers: u32,
    pub direction: Option<Direction>,
    pub action: Option<SwipeAction>,
    pub delta: f64,
    // Delta tracking inspired by Niri (GPL-3.0) https://github.com/YaLTeR/niri/tree/v0.1.3
    pub history: VecDeque<SwipeEvent>,
}

impl GestureState {
    pub fn new(fingers: u32) -> Self {
        GestureState {
            fingers,
            direction: None,
            action: None,
            delta: 0.0,
            history: VecDeque::new(),
        }
    }

    pub fn update(&mut self, movement: Point<f64, Logical>, timestamp: Duration) -> bool {
        let first_update = self.direction.is_none();

        if first_update {
            // Find proper direction
            self.direction = if movement.x.abs() > movement.y.abs() {
                if movement.x > 0.0 {
                    Some(Direction::Right)
                } else {
                    Some(Direction::Left)
                }
            } else if movement.y > 0.0 {
                Some(Direction::Down)
            } else {
                Some(Direction::Up)
            }
        }

        let delta = match self.direction {
            Some(Direction::Left) => -movement.x,
            Some(Direction::Right) => movement.x,
            Some(Direction::Up) => -movement.y,
            Some(Direction::Down) => movement.y,
            None => 0.0,
        };

        self.push(delta, timestamp);
        first_update
    }

    /// Pushes a new reading into the tracker.
    fn push(&mut self, delta: f64, timestamp: Duration) {
        // For the events that we care about, timestamps should always increase
        // monotonically.
        if let Some(last) = self.history.back() {
            if timestamp < last.timestamp {
                trace!(
                    "ignoring event with timestamp {timestamp:?} earlier than last {:?}",
                    last.timestamp
                );
                return;
            }
        }

        self.history.push_back(SwipeEvent { delta, timestamp });
        self.delta += delta;

        self.trim_history();
    }

    /// Computes the current gesture velocity.
    pub fn velocity(&self) -> f64 {
        let (Some(first), Some(last)) = (self.history.front(), self.history.back()) else {
            return 0.;
        };

        let total_time = (last.timestamp - first.timestamp).as_secs_f64();
        if total_time == 0. {
            return 0.;
        }

        let total_delta = self.history.iter().map(|event| event.delta).sum::<f64>();
        total_delta / total_time
    }

    /// Computes the gesture end position after decelerating to a halt.
    pub fn projected_end_pos(&self) -> f64 {
        let vel = self.velocity();
        self.delta - vel / (1000. * DECELERATION_TOUCHPAD.ln())
    }

    fn trim_history(&mut self) {
        let Some(&SwipeEvent { timestamp, .. }) = self.history.back() else {
            return;
        };

        while let Some(first) = self.history.front() {
            if timestamp <= first.timestamp + HISTORY_LIMIT {
                break;
            }

            let _ = self.history.pop_front();
        }
    }
}

impl Default for GestureState {
    fn default() -> Self {
        GestureState::new(0)
    }
}

// rubber_band.rs from Niri (GPL-3.0) https://github.com/YaLTeR/niri/blob/db49deb7fd2fbe805ceec060aa4dec65009ad7a7/src/rubber_band.rs
#[derive(Debug, Clone, Copy)]
pub struct RubberBand {
    pub stiffness: f64,
    pub limit: f64,
}

impl RubberBand {
    pub fn band(&self, x: f64) -> f64 {
        let c = self.stiffness;
        let d = self.limit;

        (1. - (1. / (x * c / d + 1.))) * d
    }

    pub fn derivative(&self, x: f64) -> f64 {
        let c = self.stiffness;
        let d = self.limit;

        c * d * d / (c * x + d).powi(2)
    }

    pub fn clamp(&self, min: f64, max: f64, x: f64) -> f64 {
        let clamped = x.clamp(min, max);
        let sign = if x < clamped { -1. } else { 1. };
        let diff = (x - clamped).abs();

        clamped + sign * self.band(diff)
    }

    pub fn clamp_derivative(&self, min: f64, max: f64, x: f64) -> f64 {
        if min <= x && x <= max {
            return 1.;
        }

        let clamped = x.clamp(min, max);
        let diff = (x - clamped).abs();
        self.derivative(diff)
    }
}
