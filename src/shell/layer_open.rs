// SPDX-License-Identifier: GPL-3.0-only

//! Compositor-side open/close animation for layer-shell surfaces — the DEFAULT
//! show/hide transition for every surface that isn't edge-sliding (see
//! [`super::layer_slide`] and [`super::Shell::set_surface_hidden`]).
//!
//! It first shipped for agentos-panel's popover surfaces and now applies to all
//! fade+rise surfaces (panels, popovers, modals, notifications, the launcher…),
//! which animate IN when shown rather than appearing instantly.
//!
//! The animation matches the design prototype, with values resolved from the
//! theme's motion tokens (captured into [`motion::Motion`] at creation):
//! - duration: `motion.layer_open`
//! - easing: `motion.ease_in_out` (design `--ease-in-out`)
//! - translateY: +6px (below the resting anchored position) → 0 (slides UP)
//! - scale: 0.97 → 1.0
//! - opacity: 0 → 1
//! - transform-origin: CENTER of the surface
//!
//! ALL THREE channels (alpha, translateY, scale) are driven from a single
//! eased factor `t ∈ [0,1]` so they stay perfectly in sync.

use crate::backend::render::animations::motion;
use std::time::{Duration, Instant};
use wayland_backend::server::ObjectId;

/// Distance the surface rises during the animation (design `translateY: 6px → 0`).
pub const OPEN_RISE_PX: f32 = 6.0;
/// Starting scale of the surface (design `scale: 0.97 → 1.0`).
pub const START_SCALE: f32 = 0.97;

/// Per-surface open-animation tracking.
#[derive(Debug, Clone)]
pub struct LayerOpen {
    /// The surface ObjectId this open animation is for.
    pub surface_id: ObjectId,
    /// When the animation started (first buffer commit).
    pub start: Instant,
    /// Motion tokens captured from the theme when the animation began.
    motion: motion::Motion,
}

impl LayerOpen {
    pub fn new(surface_id: ObjectId, motion: motion::Motion) -> Self {
        Self {
            surface_id,
            start: Instant::now(),
            motion,
        }
    }

    /// Create an open whose clock is back-dated by `back_ms`, so it begins at a
    /// non-zero progress. Used to hand off from an in-flight CLOSE seamlessly:
    /// starting the open at linear progress `1 - p` (i.e.
    /// `back_ms = (1 - p) * layer_open`) makes its first frame match the
    /// close's current alpha/scale/offset exactly — because the easing is
    /// point-symmetric about (0.5, 0.5) — so a surface re-shown mid-dismissal
    /// rises the rest of the way instead of snapping to fully hidden first.
    pub fn new_backdated(surface_id: ObjectId, back_ms: u64, motion: motion::Motion) -> Self {
        let now = Instant::now();
        let start = now
            .checked_sub(Duration::from_millis(back_ms))
            .unwrap_or(now);
        Self {
            surface_id,
            start,
            motion,
        }
    }

    /// The single eased factor `t ∈ [0,1]` that drives all three channels.
    /// `0.0` at animation start, `1.0` at rest. ease-in-out over `motion.layer_open`.
    pub fn factor(&self) -> f32 {
        let progress = (self.start.elapsed().as_secs_f32() / self.motion.layer_open.as_secs_f32())
            .clamp(0.0, 1.0);
        self.motion.ease_in_out(progress)
    }

    /// Opacity for the surface: `0.0 → 1.0`, equal to the eased factor.
    pub fn alpha(&self) -> f32 {
        self.factor()
    }

    /// Translation offset `(x, y)` in logical pixels.
    /// Starts at `(0, +OPEN_RISE_PX)` (below the resting position) and settles to
    /// `(0, 0)` — i.e. it slides UP.
    pub fn translate_offset(&self) -> (i32, i32) {
        let t = self.factor();
        (0, ((1.0 - t) * OPEN_RISE_PX).round() as i32)
    }

    /// Scale for the surface: `START_SCALE → 1.0`, scaled around its CENTER.
    pub fn scale(&self) -> f32 {
        let t = self.factor();
        START_SCALE + t * (1.0 - START_SCALE)
    }

    /// True while the animation is still running.
    pub fn is_animating(&self) -> bool {
        self.start.elapsed() < self.motion.layer_open
    }
}

/// Per-surface close-animation tracking: the EXACT REVERSE of [`LayerOpen`].
///
/// Plays when a fade+rise surface is hidden via the `layer_surface_visibility`
/// protocol (the client sends `HideWindow`, then typically destroys the surface
/// once this completes). The surface stays alive and rendered (from its last
/// committed buffer) for the duration so it can animate OUT — the reverse of the
/// entrance:
/// - translateY: 0 → +6px (slides DOWN, below the resting position)
/// - scale: 1.0 → 0.97 (scales DOWN about CENTER)
/// - opacity: 1 → 0 (fades OUT)
///
/// All three channels are driven from the SAME eased factor so they stay
/// in sync, identical easing to the open.
#[derive(Debug, Clone)]
pub struct LayerClose {
    /// The surface ObjectId this close animation is for.
    pub surface_id: ObjectId,
    /// When the animation started (the `set_surface_hidden(true)` request).
    pub start: Instant,
    /// Motion tokens captured from the theme when the animation began.
    motion: motion::Motion,
}

impl LayerClose {
    pub fn new(surface_id: ObjectId, motion: motion::Motion) -> Self {
        Self {
            surface_id,
            start: Instant::now(),
            motion,
        }
    }

    /// Create a close whose clock is back-dated by `back_ms`, so it begins at a
    /// non-zero progress. Used to hand off from an in-flight OPEN seamlessly:
    /// because the easing is point-symmetric about (0.5, 0.5), starting the
    /// close at linear progress `1 - p` (i.e. `back_ms = (1 - p) * layer_open`)
    /// makes its first frame match the open's current alpha/scale/offset exactly
    /// — no jump when a popover is dismissed mid-entrance. A surface that was
    /// never actually shown (`back_ms == layer_open`) starts already hidden.
    pub fn new_backdated(surface_id: ObjectId, back_ms: u64, motion: motion::Motion) -> Self {
        let now = Instant::now();
        let start = now
            .checked_sub(Duration::from_millis(back_ms))
            .unwrap_or(now);
        Self {
            surface_id,
            start,
            motion,
        }
    }

    /// The single eased factor `t ∈ [0,1]` driving all three channels.
    /// `0.0` at the start of the close, `1.0` when fully hidden.
    pub fn factor(&self) -> f32 {
        let progress = (self.start.elapsed().as_secs_f32() / self.motion.layer_open.as_secs_f32())
            .clamp(0.0, 1.0);
        self.motion.ease_in_out(progress)
    }

    /// Opacity for the surface: `1.0 → 0.0` (the reverse of the open).
    pub fn alpha(&self) -> f32 {
        1.0 - self.factor()
    }

    /// Translation offset `(x, y)` in logical pixels.
    /// Starts at `(0, 0)` (resting) and settles to `(0, +OPEN_RISE_PX)` — i.e.
    /// it slides DOWN, the reverse of the open's slide-up.
    pub fn translate_offset(&self) -> (i32, i32) {
        let t = self.factor();
        (0, (t * OPEN_RISE_PX).round() as i32)
    }

    /// Scale for the surface: `1.0 → START_SCALE`, about its CENTER.
    pub fn scale(&self) -> f32 {
        let t = self.factor();
        1.0 - t * (1.0 - START_SCALE)
    }

    /// True while the animation is still running.
    pub fn is_animating(&self) -> bool {
        self.start.elapsed() < self.motion.layer_open
    }
}

// The eased factor is `Motion::ease_in_out` (the theme's `--ease-in-out`),
// shared with every other curve consumer via the captured `motion::Motion`.
