// SPDX-License-Identifier: GPL-3.0-only

//! Configurable animation durations.

use std::{
    sync::RwLock,
    time::{Duration, Instant},
};

use cosmic_comp_config::AnimationDurations;

const MIN_MS: f32 = 1.0;
const MAX_MS: f32 = 60_000.0;

static DURATIONS: RwLock<AnimationDurations> = RwLock::new(AnimationDurations::DEFAULT);

pub fn set_durations(durations: AnimationDurations) {
    *DURATIONS.write().unwrap() = durations;
}

fn to_duration(ms: f32) -> Duration {
    if ms.is_nan() || ms < MIN_MS {
        return Duration::ZERO;
    }
    Duration::from_secs_f32(ms.min(MAX_MS) / 1000.0)
}

macro_rules! durations {
    ($($name:ident),* $(,)?) => {
        $(
            pub fn $name() -> Duration {
                to_duration(DURATIONS.read().unwrap().$name)
            }
        )*
    };
}

durations!(
    workspace,
    overview,
    resize_mode,
    zoom,
    tiling,
    floating,
    minimize,
    fullscreen,
    shake,
    tab,
    tab_scroll,
    move_rescale,
    cursor_magnify,
);

/// A zero-length animation is always complete (`1.0`), so it renders its final frame right away.
/// All animation progress must be computed through this to  avoid division by zero..
pub fn progress(start: Instant, duration: Duration) -> f32 {
    if duration.is_zero() {
        return 1.0;
    }
    Instant::now().duration_since(start).as_secs_f32() / duration.as_secs_f32()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn short_durations_disable_the_animation() {
        assert_eq!(to_duration(0.0), Duration::ZERO);
        assert_eq!(to_duration(0.99), Duration::ZERO);
        assert_eq!(to_duration(-5.0), Duration::ZERO);
        assert_eq!(to_duration(f32::NAN), Duration::ZERO);
        assert_eq!(to_duration(1.0), Duration::from_millis(1));
    }

    #[test]
    fn huge_durations_are_capped() {
        assert_eq!(to_duration(f32::INFINITY), Duration::from_secs(60));
        assert_eq!(to_duration(1e30), Duration::from_secs(60));
    }

    #[test]
    fn zero_duration_is_complete() {
        assert_eq!(progress(Instant::now(), Duration::ZERO), 1.0);
        let p = progress(Instant::now(), Duration::from_secs(10));
        assert!((0.0..0.1).contains(&p));
    }
}
