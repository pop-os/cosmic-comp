// SPDX-License-Identifier: GPL-3.0-only

//! Compositor-side spring resize animation for the side panel (`agentos-chat-panel`).
//!
//! Replaces the old live drag-resize: a programmatic width change (the
//! maximize/restore double-click today, width presets later) animates the panel
//! from its current width to a target width using the shared `--ease-spring`
//! curve and `PANEL_SLIDE` timing (`crate::backend::render::animations::motion`).
//! Each tick the eased width is forced
//! onto the surface via [`super::Shell::override_active_layer_resize`] — the same
//! path the live drag used — so windows reflow in lockstep and the client adopts
//! each `configure` exactly as it did during a drag.

use std::time::Instant;

use smithay::output::Output;
use wayland_backend::server::ObjectId;

use crate::backend::render::animations::motion;

/// Per-surface width resize animation.
#[derive(Debug, Clone)]
pub struct LayerResizeAnim {
    /// The surface being resized.
    pub surface_id: ObjectId,
    /// The output the surface lives on (the resize re-arranges this output).
    pub output: Output,
    /// Whether the panel is anchored to the right edge (its dragged edge is on
    /// the left). Forwarded into the transient `LayerResize` each tick.
    pub anchor_right: bool,
    /// When the animation started.
    pub start: Instant,
    /// Width at the start of the animation (logical px).
    pub from_width: i32,
    /// Target width to settle at (logical px).
    pub to_width: i32,
    /// Motion tokens captured from the theme when the resize began.
    motion: motion::Motion,
}

impl LayerResizeAnim {
    pub fn new(
        surface_id: ObjectId,
        output: Output,
        anchor_right: bool,
        from_width: i32,
        to_width: i32,
        motion: motion::Motion,
    ) -> Self {
        Self {
            surface_id,
            output,
            anchor_right,
            start: Instant::now(),
            from_width,
            to_width,
            motion,
        }
    }

    /// Eased progress `t ∈ [0,1]` over `motion.panel_slide`.
    pub fn factor(&self) -> f32 {
        let progress = (self.start.elapsed().as_secs_f32() / self.motion.panel_slide.as_secs_f32())
            .clamp(0.0, 1.0);
        self.motion.ease_spring(progress)
    }

    /// The current interpolated width in logical px.
    pub fn current_width(&self) -> i32 {
        let t = self.factor();
        (self.from_width as f32 + (self.to_width - self.from_width) as f32 * t).round() as i32
    }

    /// True while the animation is still running.
    pub fn is_animating(&self) -> bool {
        self.start.elapsed() < self.motion.panel_slide
    }
}
