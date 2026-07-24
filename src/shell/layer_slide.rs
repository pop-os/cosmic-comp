// SPDX-License-Identifier: GPL-3.0-only

//! Slide animation for layer surfaces toggled via the visibility protocol.
//!
//! When a layer surface anchored to a single edge (Left or Right) is shown or
//! hidden via `layer_surface_visibility`, the compositor slides it in/out from
//! that edge instead of just fading opacity. This provides a smooth, polished
//! animation for side panels like the chat panel.
//!
//! The state machine:
//! - **Visible**: Surface is fully shown (offset = 0).
//! - **SlidingOut**: Animate surface off the edge.
//! - **Hidden**: Surface is fully off-screen.
//! - **SlidingIn**: Animate surface back on-screen.

use crate::backend::render::animations::motion;
use std::time::{Duration, Instant};
use wayland_backend::server::ObjectId;

/// Which edge the surface slides toward.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlideEdge {
    Left,
    Right,
}

/// Visibility state machine for the slide animation.
///
/// The `duration` of an active transition is scaled by the distance it has to
/// cover, so reversing a half-finished slide takes half the time instead of
/// crawling through the full duration for the remaining distance.
#[derive(Debug, Clone)]
pub enum SlideVisibility {
    /// Fully visible (factor = 0.0, no offset).
    Visible,
    /// Actively sliding off the edge.
    SlidingOut {
        start: Instant,
        from_factor: f32,
        duration: Duration,
        /// Motion tokens captured from the theme when the slide began.
        motion: motion::Motion,
    },
    /// Fully hidden (factor = 1.0, fully off-screen).
    Hidden,
    /// Actively sliding back on-screen.
    SlidingIn {
        start: Instant,
        from_factor: f32,
        duration: Duration,
        /// Motion tokens captured from the theme when the slide began.
        motion: motion::Motion,
    },
}

impl SlideVisibility {
    /// Returns the current slide factor: 0.0 = fully visible, 1.0 = fully hidden.
    pub fn factor(&self) -> f32 {
        match self {
            Self::Visible => 0.0,
            Self::SlidingOut {
                start,
                from_factor,
                duration,
                motion,
            } => {
                let t = progress_clamped(*start, *duration);
                let eased = motion.ease_spring(t);
                from_factor + (1.0 - from_factor) * eased
            }
            Self::Hidden => 1.0,
            Self::SlidingIn {
                start,
                from_factor,
                duration,
                motion,
            } => {
                let t = progress_clamped(*start, *duration);
                let eased = motion.ease_spring(t);
                from_factor * (1.0 - eased)
            }
        }
    }

    /// True while any animation is active.
    pub fn is_animating(&self) -> bool {
        matches!(self, Self::SlidingOut { .. } | Self::SlidingIn { .. })
    }

    /// Fraction of the slide still to run: ~1.0 right after a transition
    /// starts, 0.0 once settled. Eased, and consistent across reversals —
    /// used to drive content crossfades locked to the motion.
    pub fn remaining_fraction(&self) -> f32 {
        match self {
            Self::Visible | Self::Hidden => 0.0,
            // factor: current → 0
            Self::SlidingIn { .. } => self.factor().clamp(0.0, 1.0),
            // factor: current → 1
            Self::SlidingOut { .. } => (1.0 - self.factor()).clamp(0.0, 1.0),
        }
    }

    /// Advance the state machine. Returns `Some(true)` when fully visible,
    /// `Some(false)` when fully hidden (terminal transitions).
    pub fn update(&mut self) -> Option<bool> {
        match self {
            Self::SlidingOut {
                start, duration, ..
            } => {
                if start.elapsed() >= *duration {
                    *self = Self::Hidden;
                    Some(false)
                } else {
                    None
                }
            }
            Self::SlidingIn {
                start, duration, ..
            } => {
                if start.elapsed() >= *duration {
                    *self = Self::Visible;
                    Some(true)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Begin sliding out (hiding). `motion` is the theme snapshot captured by the
    /// caller (which has the theme handle); the slide uses `motion.panel_slide`
    /// and `motion.ease_spring`.
    pub fn start_hide(&mut self, motion: motion::Motion) {
        match self {
            Self::Visible => {
                *self = Self::SlidingOut {
                    start: Instant::now(),
                    from_factor: 0.0,
                    duration: motion.panel_slide,
                    motion,
                };
            }
            Self::SlidingIn { .. } => {
                // Reverse from current position; remaining distance is 1 - current.
                let current = self.factor();
                let duration = motion.panel_slide.mul_f32((1.0 - current).clamp(0.0, 1.0));
                *self = Self::SlidingOut {
                    start: Instant::now(),
                    from_factor: current,
                    duration,
                    motion,
                };
            }
            _ => {} // Already hiding or hidden
        }
    }

    /// Begin sliding in (showing). See [`Self::start_hide`] for `motion`.
    pub fn start_show(&mut self, motion: motion::Motion) {
        match self {
            Self::Hidden => {
                *self = Self::SlidingIn {
                    start: Instant::now(),
                    from_factor: 1.0,
                    duration: motion.panel_slide,
                    motion,
                };
            }
            Self::SlidingOut { .. } => {
                // Reverse from current position; remaining distance is the current factor.
                let current = self.factor();
                let duration = motion.panel_slide.mul_f32(current.clamp(0.0, 1.0));
                *self = Self::SlidingIn {
                    start: Instant::now(),
                    from_factor: current,
                    duration,
                    motion,
                };
            }
            _ => {} // Already showing or visible
        }
    }
}

/// Per-surface slide animation tracking.
#[derive(Debug)]
pub struct LayerSlide {
    /// The surface ObjectId this slide is for.
    pub surface_id: ObjectId,
    /// Which edge the surface slides toward.
    pub edge: SlideEdge,
    /// Current animation state.
    pub visibility: SlideVisibility,
    /// Width of the surface (for computing offset distance).
    pub surface_width: i32,
    /// The surface's exclusive zone value (for animating workspace layout).
    pub exclusive_zone: i32,
    /// The exclusive zone value last written into cached state and arranged.
    /// Lets the animation tick skip re-arranging/relayouting when the integer
    /// zone hasn't moved a whole pixel since the last application.
    pub last_applied_ez: Option<i32>,
    /// The eased factor sampled on the tick that last moved `last_applied_ez`.
    /// `render_offset` and `effective_exclusive_zone` both read THIS (not a
    /// fresh `visibility.factor()` at render time), so the panel's sliding edge
    /// and the workspace zone are computed from one identical factor and step in
    /// lockstep. Without this the panel offset sampled a slightly-fresher factor
    /// at render than the zone did at the tick, so windows lagged the panel.
    pub cached_factor: f32,
}

impl LayerSlide {
    pub fn new(
        surface_id: ObjectId,
        edge: SlideEdge,
        surface_width: i32,
        exclusive_zone: i32,
    ) -> Self {
        Self {
            surface_id,
            edge,
            visibility: SlideVisibility::Visible,
            surface_width,
            exclusive_zone,
            last_applied_ez: None,
            cached_factor: 0.0, // Visible
        }
    }

    /// Create a slide starting from fully hidden (for slide-in animations).
    pub fn new_hidden(
        surface_id: ObjectId,
        edge: SlideEdge,
        surface_width: i32,
        exclusive_zone: i32,
    ) -> Self {
        Self {
            surface_id,
            edge,
            visibility: SlideVisibility::Hidden,
            surface_width,
            exclusive_zone,
            last_applied_ez: None,
            cached_factor: 1.0, // Hidden
        }
    }

    /// Compute the render offset for the current animation factor.
    /// Returns (x_offset, y_offset) to shift the surface off its edge.
    ///
    /// Uses `cached_factor` (the tick-pinned sample) — NOT a fresh
    /// `visibility.factor()` — so the panel edge is computed from the exact same
    /// factor as `effective_exclusive_zone`/`last_applied_ez` and the two never
    /// drift a sub-pixel apart between ticks (the viewport-lag fix).
    pub fn render_offset(&self) -> (i32, i32) {
        let factor = self.cached_factor;
        if factor == 0.0 {
            return (0, 0);
        }
        let max_offset = self.surface_width;
        let offset = (max_offset as f32 * factor).round() as i32;
        match self.edge {
            SlideEdge::Left => (-offset, 0),
            SlideEdge::Right => (offset, 0),
        }
    }

    /// Compute the current effective exclusive zone (animated).
    /// When fully visible (factor=0), returns full exclusive_zone.
    /// When fully hidden (factor=1), returns 0.
    /// During animation, interpolates between these values.
    ///
    /// Computed as the exact complement of `render_offset`'s rounding so that
    /// (when `exclusive_zone == surface_width`, the common case) the panel's
    /// sliding edge and the workspace zone edge always sum to the full width —
    /// rounding them independently produced transient 1px seams/overlaps.
    pub fn effective_exclusive_zone(&self) -> i32 {
        self.ez_for_factor(self.cached_factor)
    }

    /// The effective exclusive zone for an arbitrary factor. The tick uses this
    /// with the live `visibility.factor()` to decide whether the integer zone
    /// moved; on a move it commits both `last_applied_ez` and `cached_factor`
    /// from that same factor so panel + zone stay coupled.
    pub fn ez_for_factor(&self, factor: f32) -> i32 {
        self.exclusive_zone - ((self.exclusive_zone as f32) * factor).round() as i32
    }
}

/// Clamped progress ratio for an animation started at `start` with `duration`.
/// A zero duration yields 1.0 (animation instantly complete).
fn progress_clamped(start: Instant, duration: Duration) -> f32 {
    (start.elapsed().as_secs_f32() / duration.as_secs_f32()).min(1.0)
}
