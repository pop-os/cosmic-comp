// SPDX-License-Identifier: GPL-3.0-only

//! Voice Orb rendering element
//!
//! This module provides the voice orb shader and render element for the voice mode overlay.
//! The orb is a soft, luminous energy sphere with pastel spectral gradient that appears
//! when voice input mode is active.

use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::HashMap,
    sync::atomic::{AtomicU64, Ordering},
    time::Instant,
};

use keyframe::{ease, functions::EaseOutCubic};
use memmap2::Mmap;
use smithay::{
    backend::renderer::{
        element::Kind,
        gles::{
            GlesError, GlesPixelProgram, GlesRenderer, Uniform, UniformName, UniformType,
            element::PixelShaderElement,
        },
    },
    utils::{Logical, Point, Rectangle, Size},
};
use tracing::{debug, info};

use super::element::AsGlowRenderer;
use crate::wayland::protocols::voice_mode::{OrbState, VoiceState};

/// Include the voice orb shader source
pub static VOICE_ORB_SHADER: &str = include_str!("./shaders/voice_orb.frag");

/// Shared-memory reader for real-time audio level data written by chatserve.
///
/// The file at `/run/user/$UID/agentos-voice-levels` contains a single `AtomicU64`
/// packing two `u16` band levels (low in bits 0-15, high in bits 16-31).
pub struct AudioLevelReader {
    mmap: Mmap,
}

impl AudioLevelReader {
    /// Try to open and mmap the shared audio level file.
    /// Returns `None` if the file does not exist yet.
    pub fn open() -> Option<Self> {
        let uid = unsafe { libc::getuid() };
        let path = format!("/run/user/{}/agentos-voice-levels", uid);
        let file = std::fs::File::open(&path).ok()?;
        let mmap = unsafe { Mmap::map(&file).ok()? };
        if mmap.len() < 8 {
            return None;
        }
        debug!(path, "Audio level shmem reader opened");
        Some(Self { mmap })
    }

    /// Read the current low and high band levels (each 0-1000).
    pub fn read(&self) -> (u16, u16) {
        let ptr = self.mmap.as_ptr() as *const AtomicU64;
        // SAFETY: mmap is page-aligned (>= 8-byte aligned) and at least 8 bytes.
        let packed = unsafe { &*ptr }.load(Ordering::Relaxed);
        let low = (packed & 0xFFFF) as u16;
        let high = ((packed >> 16) & 0xFFFF) as u16;
        (low, high)
    }
}

/// Calculated metrics for orb positioning and scaling relative to a window
#[derive(Debug, Clone, Copy)]
pub struct OrbWindowMetrics {
    /// Normalized position of window center (0.0-1.0)
    pub center: Point<f32, Logical>,
    /// Scale to fit orb inside window
    pub contained_scale: f32,
    /// Scale to cover entire window (from center to corners)
    pub burst_scale: f32,
    /// Base orb size in pixels (20% of output width)
    pub floating_orb_size: f32,
}

impl OrbWindowMetrics {
    /// Calculate all orb metrics for a given window and output size
    pub fn calculate(window_geo: Rectangle<i32, Logical>, output_size: Size<i32, Logical>) -> Self {
        let floating_orb_size = output_size.w as f32 * 0.20;

        // Normalized window center position
        let center = Point::from((
            (window_geo.loc.x as f32 + window_geo.size.w as f32 / 2.0) / output_size.w as f32,
            (window_geo.loc.y as f32 + window_geo.size.h as f32 / 2.0) / output_size.h as f32,
        ));

        // Scale to fit orb inside window (with margin)
        let window_min_dim = (window_geo.size.w.min(window_geo.size.h) as f32) * 0.6;
        let contained_scale = (window_min_dim / floating_orb_size).min(0.5);

        // Scale to cover from center to corners (with margin)
        let burst_scale = Self::burst_scale_from_size(window_geo.size, floating_orb_size);

        Self {
            center,
            contained_scale,
            burst_scale,
            floating_orb_size,
        }
    }

    /// Calculate burst scale only (for when we just need to update scale on resize)
    pub fn burst_scale_for_window(
        window_geo: Rectangle<i32, Logical>,
        floating_orb_size: f32,
    ) -> f32 {
        Self::burst_scale_from_size(window_geo.size, floating_orb_size)
    }

    /// Calculate burst scale from window dimensions
    fn burst_scale_from_size(size: Size<i32, Logical>, floating_orb_size: f32) -> f32 {
        let w = size.w as f32;
        let h = size.h as f32;
        // Distance from center to corner, times 2 for diameter, plus 5% margin
        let corner_dist = ((w / 2.0).powi(2) + (h / 2.0).powi(2)).sqrt();
        let cover_diameter = corner_dist * 1.85;
        cover_diameter / floating_orb_size
    }
}

/// Voice orb shader wrapper
pub struct VoiceOrbShader(pub GlesPixelProgram);

/// Identifies which orb a cached render element belongs to.
///
/// There is at most one floating orb (`target_output` gates it to a single
/// output) and at most one window-level orb per attached surface, so this is
/// enough to keep the two from sharing an element.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum VoiceOrbKey {
    Floating,
    Attached(String),
}

/// Cache of the orb's render element, keyed by [`VoiceOrbKey`].
///
/// This exists purely to keep the element's smithay `Id` stable across frames.
/// `PixelShaderElement::new` mints a fresh `Id::new()`, so rebuilding the orb
/// every frame made the damage tracker see the previous frame's orb as a *gone*
/// element, which sets `force_effect_redraw` — and that re-captures and re-blurs
/// every framebuffer-effect element on the output, and punches their rects out
/// of every opaque region, for as long as the orb is visible.
///
/// Every other `PixelShaderElement` in this tree (`IndicatorShader`,
/// `BackdropShader`, `ShadowShader`) already memoizes this way; the orb was the
/// only one that did not. Cloning preserves the `Id` — it shares an `Arc`.
type VoiceOrbCache = RefCell<HashMap<VoiceOrbKey, PixelShaderElement>>;

/// Animation state for the voice orb
#[derive(Debug, Clone)]
pub enum VoiceOrbAnimation {
    /// Simple animation (grow, shrink, move)
    Simple {
        start_time: Instant,
        duration: f32,
        start_scale: f32,
        target_scale: f32,
        start_position: Point<f32, Logical>,
        target_position: Point<f32, Logical>,
    },
    /// Two-phase animation for attaching to window:
    /// Phase 1: Dart toward window while shrinking to fit inside
    /// Phase 2: Burst to fill window background (clipped)
    DartThenBurst {
        start_time: Instant,
        /// Duration of dart phase
        dart_duration: f32,
        /// Duration of burst phase
        burst_duration: f32,
        /// Starting position (screen center typically)
        start_position: Point<f32, Logical>,
        /// Window center position (normalized)
        window_center: Point<f32, Logical>,
        /// Starting scale
        start_scale: f32,
        /// Scale when contained in window (before burst)
        contained_scale: f32,
        /// Final burst scale (to cover window)
        burst_scale: f32,
    },
    /// Detach from window: shrink then dart back to center
    ShrinkThenDart {
        start_time: Instant,
        shrink_duration: f32,
        dart_duration: f32,
        window_center: Point<f32, Logical>,
        target_position: Point<f32, Logical>,
        start_scale: f32,
        contained_scale: f32,
    },
    /// Burst to fill window then fade out (for attach_and_transition)
    /// Orb expands to burst scale while fading out opacity
    BurstThenFadeOut {
        start_time: Instant,
        /// Duration of burst phase
        burst_duration: f32,
        /// Duration of fade phase (overlaps with burst end)
        fade_duration: f32,
        /// Starting position
        start_position: Point<f32, Logical>,
        /// Window center position (normalized)
        window_center: Point<f32, Logical>,
        /// Starting scale
        start_scale: f32,
        /// Final burst scale (to cover window)
        burst_scale: f32,
    },
}

impl VoiceOrbAnimation {
    /// Create a new grow-in animation with spring bounce
    /// Starts from current scale/position for smooth transitions when interrupted
    pub fn grow_in(current_scale: f32, current_position: Point<f32, Logical>) -> Self {
        Self::Simple {
            start_time: Instant::now(),
            duration: 0.4, // 400ms - longer grow phase with bounces at end
            start_scale: current_scale,
            target_scale: 1.0,
            start_position: current_position,
            target_position: Point::from((0.5, 0.5)),
        }
    }

    /// Create a shrink-out animation
    /// When stay_in_place is true, shrinks from current position (for attached mode)
    /// When false, shrinks while moving to center (for floating mode)
    pub fn shrink_out(
        current_scale: f32,
        current_position: Point<f32, Logical>,
        stay_in_place: bool,
    ) -> Self {
        Self::Simple {
            start_time: Instant::now(),
            duration: 0.3,
            start_scale: current_scale,
            target_scale: 0.0,
            start_position: current_position,
            target_position: if stay_in_place {
                current_position
            } else {
                Point::from((0.5, 0.5))
            },
        }
    }

    /// Create a burst-only animation (for when orb appears directly attached to window)
    /// Starts from current scale/position for smooth transitions when interrupted
    pub fn burst_only(
        current_scale: f32,
        current_position: Point<f32, Logical>,
        burst_scale: f32,
        window_center: Point<f32, Logical>,
    ) -> Self {
        Self::Simple {
            start_time: Instant::now(),
            duration: 0.35, // Slightly longer for direct burst
            start_scale: current_scale,
            target_scale: burst_scale,
            start_position: current_position,
            target_position: window_center,
        }
    }

    /// Create an animation to attach to a window (dart then burst)
    /// - contained_scale: scale at which orb fits inside window (relative to floating size)
    /// - burst_scale: scale to fill window with cover effect
    pub fn dart_then_burst(
        current_position: Point<f32, Logical>,
        window_center: Point<f32, Logical>,
        current_scale: f32,
        contained_scale: f32,
        burst_scale: f32,
    ) -> Self {
        Self::DartThenBurst {
            start_time: Instant::now(),
            dart_duration: 0.35,  // Fast dart
            burst_duration: 0.25, // Quick burst
            start_position: current_position,
            window_center,
            start_scale: current_scale,
            contained_scale,
            burst_scale,
        }
    }

    /// Create an animation to detach from window
    pub fn shrink_then_dart(
        window_center: Point<f32, Logical>,
        burst_scale: f32,
        contained_scale: f32,
    ) -> Self {
        Self::ShrinkThenDart {
            start_time: Instant::now(),
            shrink_duration: 0.2,
            dart_duration: 0.35,
            window_center,
            target_position: Point::from((0.5, 0.5)),
            start_scale: burst_scale,
            contained_scale,
        }
    }

    /// Create an animation to burst and fade out (for attach_and_transition)
    /// The orb expands to fill the window while fading out
    pub fn burst_then_fade_out(
        current_position: Point<f32, Logical>,
        window_center: Point<f32, Logical>,
        current_scale: f32,
        burst_scale: f32,
    ) -> Self {
        Self::BurstThenFadeOut {
            start_time: Instant::now(),
            burst_duration: 0.4, // Burst to fill window
            fade_duration: 0.3,  // Fade out (starts during burst)
            start_position: current_position,
            window_center,
            start_scale: current_scale,
            burst_scale,
        }
    }

    /// Get the animation progress (0.0 to 1.0)
    pub fn progress(&self) -> f32 {
        match self {
            Self::Simple {
                start_time,
                duration,
                ..
            } => {
                let elapsed = start_time.elapsed().as_secs_f32();
                (elapsed / duration).min(1.0)
            }
            Self::DartThenBurst {
                start_time,
                dart_duration,
                burst_duration,
                ..
            } => {
                let elapsed = start_time.elapsed().as_secs_f32();
                let total = dart_duration + burst_duration;
                (elapsed / total).min(1.0)
            }
            Self::ShrinkThenDart {
                start_time,
                shrink_duration,
                dart_duration,
                ..
            } => {
                let elapsed = start_time.elapsed().as_secs_f32();
                let total = shrink_duration + dart_duration;
                (elapsed / total).min(1.0)
            }
            Self::BurstThenFadeOut {
                start_time,
                burst_duration,
                fade_duration,
                ..
            } => {
                let elapsed = start_time.elapsed().as_secs_f32();
                // Total time is burst + overlap with fade
                let total = *burst_duration + (*fade_duration * 0.5); // Fade starts at 50% of burst
                (elapsed / total).min(1.0)
            }
        }
    }

    /// Check if the animation is complete
    pub fn is_complete(&self) -> bool {
        self.progress() >= 1.0
    }

    /// Check if we're in the "burst" phase (attached and filling window)
    pub fn is_in_burst_phase(&self) -> bool {
        match self {
            Self::DartThenBurst {
                start_time,
                dart_duration,
                ..
            } => start_time.elapsed().as_secs_f32() >= *dart_duration,
            Self::ShrinkThenDart {
                start_time,
                shrink_duration,
                ..
            } => {
                // In shrink phase (not yet darting) - still attached
                start_time.elapsed().as_secs_f32() < *shrink_duration
            }
            Self::BurstThenFadeOut { .. } => {
                // Always in burst phase for this animation
                true
            }
            _ => false,
        }
    }

    /// Get the current opacity (for BurstThenFadeOut animation)
    /// Returns 1.0 for animations that don't affect opacity
    pub fn current_opacity(&self) -> f32 {
        match self {
            Self::BurstThenFadeOut {
                start_time,
                burst_duration,
                fade_duration,
                ..
            } => {
                let elapsed = start_time.elapsed().as_secs_f32();
                // Fade starts at 50% of burst duration
                let fade_start = burst_duration * 0.5;
                if elapsed < fade_start {
                    1.0
                } else {
                    let fade_progress = (elapsed - fade_start) / fade_duration;
                    (1.0 - fade_progress.min(1.0)).max(0.0)
                }
            }
            _ => 1.0,
        }
    }

    /// Get the current scale with easing
    pub fn current_scale(&self) -> f32 {
        match self {
            Self::Simple {
                start_scale,
                target_scale,
                ..
            } => {
                let t = self.progress();
                let eased_t = if *start_scale < *target_scale {
                    ease_spring_bounce(t)
                } else {
                    ease(EaseOutCubic, 0.0, 1.0, t)
                };
                start_scale + (target_scale - start_scale) * eased_t
            }
            Self::DartThenBurst {
                start_time,
                dart_duration,
                burst_duration,
                start_scale,
                contained_scale,
                burst_scale,
                ..
            } => {
                let elapsed = start_time.elapsed().as_secs_f32();
                if elapsed < *dart_duration {
                    // Dart phase: shrink from start to contained
                    let t = elapsed / dart_duration;
                    let eased_t = ease(EaseOutCubic, 0.0, 1.0, t);
                    start_scale + (contained_scale - start_scale) * eased_t
                } else {
                    // Burst phase: expand from contained to burst (with spring)
                    let t = (elapsed - dart_duration) / burst_duration;
                    let eased_t = ease_spring_bounce(t.min(1.0));
                    contained_scale + (burst_scale - contained_scale) * eased_t
                }
            }
            Self::ShrinkThenDart {
                start_time,
                shrink_duration,
                dart_duration,
                start_scale,
                contained_scale,
                ..
            } => {
                let elapsed = start_time.elapsed().as_secs_f32();
                if elapsed < *shrink_duration {
                    // Shrink phase: from burst to contained
                    let t = elapsed / shrink_duration;
                    let eased_t = ease(EaseOutCubic, 0.0, 1.0, t);
                    start_scale + (contained_scale - start_scale) * eased_t
                } else {
                    // Dart phase: stay at contained scale, moving back to center
                    // Then grow back to 1.0 as we reach center
                    let t = (elapsed - shrink_duration) / dart_duration;
                    let eased_t = ease(EaseOutCubic, 0.0, 1.0, t.min(1.0));
                    contained_scale + (1.0 - contained_scale) * eased_t
                }
            }
            Self::BurstThenFadeOut {
                start_time,
                burst_duration,
                start_scale,
                burst_scale,
                ..
            } => {
                let elapsed = start_time.elapsed().as_secs_f32();
                let t = (elapsed / burst_duration).min(1.0);
                // Use simple ease out for burst+fade transition (no spring bounce)
                // Spring looks weird when combined with fade out
                let eased_t = ease(EaseOutCubic, 0.0, 1.0, t);
                start_scale + (burst_scale - start_scale) * eased_t
            }
        }
    }

    /// Get the current position with easing
    pub fn current_position(&self) -> Point<f32, Logical> {
        match self {
            Self::Simple {
                start_position,
                target_position,
                ..
            } => {
                let t = ease(EaseOutCubic, 0.0, 1.0, self.progress());
                Point::from((
                    start_position.x + (target_position.x - start_position.x) * t,
                    start_position.y + (target_position.y - start_position.y) * t,
                ))
            }
            Self::DartThenBurst {
                start_time,
                dart_duration,
                start_position,
                window_center,
                ..
            } => {
                let elapsed = start_time.elapsed().as_secs_f32();
                if elapsed < *dart_duration {
                    // Dart phase: move toward window
                    let t = elapsed / dart_duration;
                    let eased_t = ease(EaseOutCubic, 0.0, 1.0, t);
                    Point::from((
                        start_position.x + (window_center.x - start_position.x) * eased_t,
                        start_position.y + (window_center.y - start_position.y) * eased_t,
                    ))
                } else {
                    // Burst phase: stay at window center
                    *window_center
                }
            }
            Self::ShrinkThenDart {
                start_time,
                shrink_duration,
                dart_duration,
                window_center,
                target_position,
                ..
            } => {
                let elapsed = start_time.elapsed().as_secs_f32();
                if elapsed < *shrink_duration {
                    // Shrink phase: stay at window center
                    *window_center
                } else {
                    // Dart phase: move back to center
                    let t = (elapsed - shrink_duration) / dart_duration;
                    let eased_t = ease(EaseOutCubic, 0.0, 1.0, t.min(1.0));
                    Point::from((
                        window_center.x + (target_position.x - window_center.x) * eased_t,
                        window_center.y + (target_position.y - window_center.y) * eased_t,
                    ))
                }
            }
            Self::BurstThenFadeOut {
                start_time,
                burst_duration,
                start_position,
                window_center,
                ..
            } => {
                let elapsed = start_time.elapsed().as_secs_f32();
                if elapsed < *burst_duration * 0.3 {
                    // First 30%: move to window center
                    let t = elapsed / (burst_duration * 0.3);
                    let eased_t = ease(EaseOutCubic, 0.0, 1.0, t);
                    Point::from((
                        start_position.x + (window_center.x - start_position.x) * eased_t,
                        start_position.y + (window_center.y - start_position.y) * eased_t,
                    ))
                } else {
                    // Rest: stay at window center
                    *window_center
                }
            }
        }
    }
}

/// Spring easing with 3 bounces for pop-in effect
/// Grows from 0 to 1, overshoots, bounces back, settles
fn ease_spring_bounce(t: f32) -> f32 {
    if t <= 0.0 {
        return 0.0;
    }
    if t >= 1.0 {
        return 1.0;
    }

    // Keyframe approach for clear grow + bounce:
    // 0.0  -> 0.0  (start)
    // 0.65 -> 1.08 (slight overshoot)
    // 0.78 -> 0.96 (slight undershoot)
    // 0.90 -> 1.02 (tiny overshoot)
    // 1.0  -> 1.0  (settle)

    if t < 0.65 {
        // Grow phase: 0 to 1.08 with ease-in-out for smoother start
        let local_t = t / 0.65;
        // Ease-in-out cubic: starts slow, speeds up, then slows down
        let eased = if local_t < 0.5 {
            4.0 * local_t * local_t * local_t
        } else {
            1.0 - (-2.0 * local_t + 2.0).powi(3) / 2.0
        };
        eased * 1.08
    } else if t < 0.78 {
        // First bounce back: 1.08 to 0.96
        let local_t = (t - 0.65) / 0.13;
        1.08 - 0.12 * local_t
    } else if t < 0.90 {
        // Second bounce up: 0.96 to 1.02
        let local_t = (t - 0.78) / 0.12;
        0.96 + 0.06 * local_t
    } else {
        // Settle: 1.02 to 1.0
        let local_t = (t - 0.90) / 0.10;
        1.02 - 0.02 * local_t
    }
}

/// State for the voice orb rendering
pub struct VoiceOrbState {
    /// Current orb display state
    pub orb_state: OrbState,
    /// Current voice input state
    pub voice_state: VoiceState,
    /// Current scale (0.0 to 1.0 for floating, can be higher for burst)
    pub scale: f32,
    /// Current position (normalized to output)
    pub position: Point<f32, Logical>,
    /// Current opacity (0.0 = transparent, 1.0 = fully visible)
    pub opacity: f32,
    /// Smoothed low-frequency intensity (bass-like, fast response)
    pub low_intensity: f32,
    /// Smoothed high-frequency intensity (treble-like, slower response)
    pub high_intensity: f32,
    /// Thinking mode progress (0.0 = normal, 1.0 = fully thinking)
    /// Smoothly interpolated based on orb state (Frozen = thinking)
    pub thinking_progress: f32,
    /// Target intensity from audio level input
    target_intensity: f32,
    /// Target low-band intensity (from FFT shmem or single-channel fallback)
    target_low: f32,
    /// Target high-band intensity (from FFT shmem or single-channel fallback)
    target_high: f32,
    /// Current animation (if any)
    pub animation: Option<VoiceOrbAnimation>,
    /// Attached window geometry (if attached)
    pub attached_window: Option<Rectangle<i32, Logical>>,
    /// Attached window surface ID (for reliable matching during render)
    pub attached_surface_id: Option<String>,
    /// Animation start time for shader time uniform
    pub shader_time_start: Instant,
    /// Pending orb show - waiting for window fade to complete
    pending_show: bool,
    /// Pending orb hide - orb should start hiding
    pending_hide: bool,
    /// Cached burst scale for current window (orb scale to cover window)
    burst_scale: f32,
    /// Cached contained scale for current window (orb scale to fit inside)
    contained_scale: f32,
    /// True when shrinking from attached state (keep rendering at window level)
    pub shrinking_from_attached: bool,
    /// True when attach_and_transition animation just completed (for state sync)
    pub transition_just_completed: bool,
    /// Output name where floating orb should be rendered (set when voice mode starts)
    pub target_output: Option<String>,
    /// Shared-memory reader for real-time FFT audio level data from chatserve
    shmem_reader: Option<AudioLevelReader>,
    /// When true, poll_shmem_audio_levels() will not lazily reopen the reader.
    /// Set on hide(), cleared on show_floating()/show_attached().
    shmem_disabled: bool,
    /// Last time update_pulse() was called (for real delta time)
    last_pulse_update: Instant,
    /// Timestamp when the orb entered Frozen state (for timeout)
    frozen_since: Option<Instant>,
    /// Whether the orb was in Attached mode before entering Frozen state.
    /// Used to skip window fade-in on dismiss (windows are already visible when attached).
    pub frozen_was_attached: bool,
}

impl std::fmt::Debug for VoiceOrbState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VoiceOrbState")
            .field("orb_state", &self.orb_state)
            .field("voice_state", &self.voice_state)
            .field("scale", &self.scale)
            .field("low_intensity", &self.low_intensity)
            .field("high_intensity", &self.high_intensity)
            .field("thinking_progress", &self.thinking_progress)
            .field("frozen_was_attached", &self.frozen_was_attached)
            .field("shmem_reader", &self.shmem_reader.is_some())
            .finish_non_exhaustive()
    }
}

impl Clone for VoiceOrbState {
    fn clone(&self) -> Self {
        Self {
            orb_state: self.orb_state,
            voice_state: self.voice_state,
            scale: self.scale,
            position: self.position,
            opacity: self.opacity,
            low_intensity: self.low_intensity,
            high_intensity: self.high_intensity,
            thinking_progress: self.thinking_progress,
            target_intensity: self.target_intensity,
            target_low: self.target_low,
            target_high: self.target_high,
            animation: self.animation.clone(),
            attached_window: self.attached_window,
            attached_surface_id: self.attached_surface_id.clone(),
            shader_time_start: self.shader_time_start,
            pending_show: self.pending_show,
            pending_hide: self.pending_hide,
            burst_scale: self.burst_scale,
            contained_scale: self.contained_scale,
            shrinking_from_attached: self.shrinking_from_attached,
            transition_just_completed: self.transition_just_completed,
            target_output: self.target_output.clone(),
            // shmem_reader is not cloned — only the original state reads shmem
            shmem_reader: None,
            shmem_disabled: self.shmem_disabled,
            last_pulse_update: self.last_pulse_update,
            frozen_since: self.frozen_since,
            frozen_was_attached: self.frozen_was_attached,
        }
    }
}

impl Default for VoiceOrbState {
    fn default() -> Self {
        Self {
            orb_state: OrbState::Hidden,
            voice_state: VoiceState::Idle,
            scale: 0.0,
            position: Point::from((0.5, 0.5)),
            opacity: 1.0,
            low_intensity: 0.0,
            high_intensity: 0.0,
            thinking_progress: 0.0,
            target_intensity: 0.0,
            target_low: 0.0,
            target_high: 0.0,
            animation: None,
            attached_window: None,
            attached_surface_id: None,
            shader_time_start: Instant::now(),
            pending_show: false,
            pending_hide: false,
            burst_scale: 1.0,
            contained_scale: 0.3,
            shrinking_from_attached: false,
            transition_just_completed: false,
            target_output: None,
            shmem_reader: None,
            shmem_disabled: false,
            last_pulse_update: Instant::now(),
            frozen_since: None,
            frozen_was_attached: false,
        }
    }
}

impl VoiceOrbState {
    /// Request showing the orb (will start after window fade completes)
    /// `output_name` specifies which output the floating orb should render on
    pub fn request_show_floating(&mut self, output_name: String) {
        self.pending_show = true;
        self.pending_hide = false;
        // Clear any shrink state from previous hide
        self.shrinking_from_attached = false;
        // Set the target output for floating orb rendering
        self.target_output = Some(output_name);
    }

    /// Request showing the orb attached to a window (will start after window fade completes)
    pub fn request_show_attached(
        &mut self,
        window_geo: Rectangle<i32, Logical>,
        output_size: Size<i32, Logical>,
        surface_id: String,
    ) {
        let metrics = OrbWindowMetrics::calculate(window_geo, output_size);

        self.position = metrics.center;
        self.contained_scale = metrics.contained_scale;
        self.burst_scale = metrics.burst_scale;
        self.attached_window = Some(window_geo);
        self.attached_surface_id = Some(surface_id);
        self.pending_show = true;
        self.pending_hide = false;
        // Clear any shrink state from previous hide
        self.shrinking_from_attached = false;
        self.orb_state = OrbState::Attached;
    }

    /// Start showing the orb (floating in center)
    pub fn show_floating(&mut self) {
        self.orb_state = OrbState::Floating;
        self.shmem_disabled = false;
        debug!("Voice orb show_floating: shmem re-enabled");
        // Start from current position/scale for smooth transitions when interrupted
        self.animation = Some(VoiceOrbAnimation::grow_in(self.scale, self.position));
        self.pending_show = false;
    }

    /// Start showing the orb attached to a window (burst only, no dart)
    pub fn show_attached(&mut self) {
        self.shmem_disabled = false;
        debug!("Voice orb show_attached: shmem re-enabled");
        // Position and window_geo should already be set by request_show_attached
        // Start from current scale/position for smooth transitions when interrupted
        self.animation = Some(VoiceOrbAnimation::burst_only(
            self.scale,
            self.position,
            self.burst_scale,
            self.position,
        ));
        self.pending_show = false;
    }

    /// Request hiding the orb (will trigger window fade after orb shrinks)
    pub fn request_hide(&mut self) {
        self.pending_hide = true;
        self.pending_show = false;
        self.frozen_since = None;
    }

    /// Hide the orb
    pub fn hide(&mut self) {
        // Disable shmem reads and close the reader so it gets reopened fresh
        // next time. The writer deletes the file on drop and creates a new
        // one on the next recording, invalidating the old mmap.
        // shmem_disabled prevents poll_shmem_audio_levels() from lazily
        // reopening the reader during the shrink animation.
        self.shmem_disabled = true;
        self.frozen_since = None;
        self.close_shmem_reader();
        debug!("Voice orb hide: shmem disabled and reader closed");

        if self.orb_state != OrbState::Hidden {
            // When attached (or frozen from attached), shrink in place; when floating, shrink toward center
            let stay_in_place = self.is_effectively_attached();
            self.frozen_was_attached = false;

            // If scale is already near 0, skip the animation entirely
            // This prevents a "meaningless" shrink animation that would keep
            // shrinking_from_attached=true while scale=0
            if self.scale < 0.01 {
                debug!(
                    "Voice orb hiding: scale already ~0, skipping animation. stay_in_place={}, position={:?}, scale={}",
                    stay_in_place, self.position, self.scale
                );
                self.scale = 0.0;
                self.animation = None;
                self.shrinking_from_attached = false;
                self.orb_state = OrbState::Hidden;
                // Reaches Hidden without an animation, so `update()` never runs
                // the completion branch that would otherwise restart the clock.
                self.reset_shader_time();
            } else {
                // Track if we're shrinking from attached state (for render layer)
                self.shrinking_from_attached = stay_in_place;
                debug!(
                    "Voice orb hiding: stay_in_place={}, shrinking_from_attached={}, position={:?}, scale={}",
                    stay_in_place, self.shrinking_from_attached, self.position, self.scale
                );
                self.animation = Some(VoiceOrbAnimation::shrink_out(
                    self.scale,
                    self.position,
                    stay_in_place,
                ));
                self.orb_state = OrbState::Hidden;
            }
        }
        self.pending_show = false;
    }

    /// Check if the orb has a pending show request
    pub fn has_pending_show(&self) -> bool {
        self.pending_show
    }

    /// Check if the orb has a pending hide request
    pub fn has_pending_hide(&self) -> bool {
        self.pending_hide
    }

    /// Clear the pending hide flag (called when hide animation starts)
    pub fn clear_pending_hide(&mut self) {
        self.pending_hide = false;
    }

    /// Attach to a window with dart-then-burst animation
    pub fn attach_to(
        &mut self,
        window_geo: Rectangle<i32, Logical>,
        output_size: Size<i32, Logical>,
    ) {
        let metrics = OrbWindowMetrics::calculate(window_geo, output_size);

        self.contained_scale = metrics.contained_scale;
        self.burst_scale = metrics.burst_scale;

        self.animation = Some(VoiceOrbAnimation::dart_then_burst(
            self.position,
            metrics.center,
            self.scale,
            self.contained_scale,
            self.burst_scale,
        ));
        self.attached_window = Some(window_geo);
        self.orb_state = OrbState::Attached;
    }

    /// Detach from window and return to floating mode
    pub fn detach(&mut self) {
        if let Some(_window) = self.attached_window.take() {
            // Create shrink-then-dart animation
            self.animation = Some(VoiceOrbAnimation::shrink_then_dart(
                self.position,
                self.burst_scale,
                self.contained_scale,
            ));
        }
        self.orb_state = OrbState::Floating;
    }

    /// Transition from floating to attached mode when chat window gains focus
    pub fn transition_to_attached(
        &mut self,
        window_geo: Rectangle<i32, Logical>,
        output_size: Size<i32, Logical>,
    ) {
        if self.orb_state == OrbState::Floating {
            self.attach_to(window_geo, output_size);
        }
    }

    /// Transition from attached to floating mode when chat window loses focus
    pub fn transition_to_floating(&mut self) {
        if self.orb_state == OrbState::Attached {
            self.detach();
        }
    }

    /// Freeze the orb in place (for transcription processing)
    /// Orb stops pulsing and stays visible until attach_and_transition or cancel
    pub fn freeze(&mut self) {
        // Freeze if currently floating or attached
        if self.orb_state == OrbState::Floating || self.orb_state == OrbState::Attached {
            self.frozen_was_attached = self.orb_state == OrbState::Attached;
            debug!(
                "Freezing orb - entering thinking mode (was_attached={}, position={:?}, scale={}, burst_scale={}, attached_window={:?}, in_burst_phase={}, orb_state -> Frozen)",
                self.frozen_was_attached,
                self.position,
                self.scale,
                self.burst_scale,
                self.attached_window,
                self.is_in_burst_phase()
            );
            self.orb_state = OrbState::Frozen;
            self.frozen_since = Some(Instant::now());
            // Stop pulsing — zero the target and let smoothing decay naturally
            self.target_intensity = 0.0;
            self.voice_state = VoiceState::Idle;
        } else {
            debug!(
                "freeze() called but orb_state={:?}, ignoring",
                self.orb_state
            );
        }
    }

    /// Whether the orb is frozen and was previously attached to a window.
    /// Used to keep the orb rendering at window level during thinking mode.
    pub fn is_frozen_attached(&self) -> bool {
        self.orb_state == OrbState::Frozen && self.frozen_was_attached
    }

    /// Whether the orb is effectively in attached mode:
    /// either currently Attached, or Frozen from a previously-attached state.
    pub fn is_effectively_attached(&self) -> bool {
        self.orb_state == OrbState::Attached || self.is_frozen_attached()
    }

    /// Check if the frozen orb has exceeded the timeout
    pub fn has_frozen_timeout(&self) -> bool {
        self.frozen_since
            .map(|t| t.elapsed().as_secs_f32() >= 30.0)
            .unwrap_or(false)
    }

    /// Start the attach and transition animation (burst + fade out)
    /// This is called when a new chat window opens after transcription
    pub fn start_attach_and_transition(
        &mut self,
        window_geo: Rectangle<i32, Logical>,
        output_size: Size<i32, Logical>,
        surface_id: String,
    ) {
        // Only valid from Frozen state
        if self.orb_state != OrbState::Frozen {
            debug!("attach_and_transition called but orb not frozen, ignoring");
            return;
        }

        let metrics = OrbWindowMetrics::calculate(window_geo, output_size);

        debug!(
            "Starting attach_and_transition: window={:?}, burst_scale={}",
            window_geo, metrics.burst_scale
        );

        self.burst_scale = metrics.burst_scale;
        self.contained_scale = metrics.contained_scale;
        self.attached_window = Some(window_geo);
        self.attached_surface_id = Some(surface_id);
        self.orb_state = OrbState::Transitioning;
        self.frozen_since = None;
        self.frozen_was_attached = false;
        self.opacity = 1.0;

        // Start burst + fade animation
        self.animation = Some(VoiceOrbAnimation::burst_then_fade_out(
            self.position,
            metrics.center,
            self.scale,
            metrics.burst_scale,
        ));
    }

    /// Check if voice mode is currently active (not hidden)
    pub fn is_active(&self) -> bool {
        self.orb_state != OrbState::Hidden
    }

    /// Update animation state
    pub fn update(&mut self) {
        // Update pulse intensity smoothing every frame
        self.update_pulse();

        // Clear the transition completion flag at the start of each update
        // (it should be consumed by the caller after previous update)
        self.transition_just_completed = false;

        if let Some(ref animation) = self.animation {
            self.scale = animation.current_scale();
            self.position = animation.current_position();

            // Update opacity for fade-out animations
            let new_opacity = animation.current_opacity();
            if self.orb_state == OrbState::Transitioning
                && (self.opacity - new_opacity).abs() > 0.01
            {
                debug!(
                    "Voice orb transitioning: opacity changing from {} to {}",
                    self.opacity, new_opacity
                );
            }
            self.opacity = new_opacity;

            if animation.is_complete() {
                // Animation done, clear it
                self.animation = None;

                // If we animated to hidden, ensure scale is 0 and clear attached state
                if self.orb_state == OrbState::Hidden {
                    self.scale = 0.0;
                    // Reset position to center so next show starts from center
                    self.position = Point::from((0.5, 0.5));
                    self.shrinking_from_attached = false;
                    // Clear attached window info when fully hidden
                    self.attached_window = None;
                    self.attached_surface_id = None;
                    self.opacity = 1.0;
                    // Clear target output so next show picks current cursor output
                    self.target_output = None;
                    // Safe here: scale is 0, so the phase jump is not on screen.
                    self.reset_shader_time();
                }
                // If transitioning (attach_and_transition), complete -> hidden
                if self.orb_state == OrbState::Transitioning {
                    debug!("attach_and_transition animation complete, transitioning to hidden");
                    self.orb_state = OrbState::Hidden;
                    self.scale = 0.0;
                    self.position = Point::from((0.5, 0.5));
                    self.attached_window = None;
                    self.attached_surface_id = None;
                    self.opacity = 1.0;
                    // Clear target output so next show picks current cursor output
                    self.target_output = None;
                    // Set flag for external state sync
                    self.transition_just_completed = true;
                    // Safe here: scale is 0, so the phase jump is not on screen.
                    self.reset_shader_time();
                }
                // If attached, ensure we're at burst scale
                if self.orb_state == OrbState::Attached {
                    self.scale = self.burst_scale;
                }
                // If floating (after detach), ensure scale is 1.0
                if self.orb_state == OrbState::Floating && self.scale > 0.5 {
                    self.scale = 1.0;
                }
            }
        }
    }

    /// Check if currently in burst phase (rendering clipped to window)
    pub fn is_in_burst_phase(&self) -> bool {
        // If shrinking from attached, we're still in burst phase (need clipping)
        if self.shrinking_from_attached {
            return true;
        }
        // Transitioning state is always in burst phase (orb expands over window)
        if self.orb_state == OrbState::Transitioning {
            return true;
        }
        // Frozen from attached mode: keep rendering clipped to window
        if self.is_frozen_attached() {
            return true;
        }
        // If not attached, never in burst phase
        if self.orb_state != OrbState::Attached {
            return false;
        }
        // If attached and no animation, we're fully burst (or if simple animation while attached)
        if self.animation.is_none() {
            return true;
        }
        // Check if animation is in burst phase
        match &self.animation {
            Some(VoiceOrbAnimation::DartThenBurst {
                start_time,
                dart_duration,
                ..
            }) => {
                // In burst phase after dart completes
                start_time.elapsed().as_secs_f32() >= *dart_duration
            }
            Some(VoiceOrbAnimation::ShrinkThenDart {
                start_time,
                shrink_duration,
                ..
            }) => {
                // In shrink phase (not yet darting) - still attached/burst
                start_time.elapsed().as_secs_f32() < *shrink_duration
            }
            Some(VoiceOrbAnimation::Simple { target_scale, .. }) => {
                // Simple animation while attached - we're bursting if scale > 1
                // (burst_only animation has target_scale > 1, shrink has target 0)
                *target_scale > 1.0
            }
            Some(VoiceOrbAnimation::BurstThenFadeOut { .. }) => {
                // Always in burst phase during this animation
                true
            }
            None => true,
        }
    }

    /// Check if currently running the BurstThenFadeOut animation
    /// Used to delay layer shell fade-in until this animation completes
    pub fn is_in_burst_then_fade_out(&self) -> bool {
        matches!(
            &self.animation,
            Some(VoiceOrbAnimation::BurstThenFadeOut { .. })
        )
    }

    /// Update audio level with separate low and high frequency band values (0-1000 each).
    /// Used when reading from shared memory FFT data.
    pub fn set_audio_level_dual(&mut self, low: u16, high: u16) {
        let target_low = (low as f32 / 1000.0 * 2.5).clamp(0.0, 1.0);
        let target_high = (high as f32 / 1000.0 * 2.5).clamp(0.0, 1.0);
        self.target_low = target_low;
        self.target_high = target_high;
        // Keep target_intensity as max for compatibility
        self.target_intensity = target_low.max(target_high);
        if low > 0 || high > 0 {
            self.voice_state = VoiceState::Recording;
        }
    }

    /// Try to read audio levels from shared memory (written by chatserve).
    /// Opens the shmem file lazily on first call. Returns true if a non-zero
    /// level was read, false otherwise.
    pub fn poll_shmem_audio_levels(&mut self) -> bool {
        // Don't reopen the reader after hide() — the old file is invalidated
        if self.shmem_disabled {
            return false;
        }

        // Lazy-open the shmem reader
        if self.shmem_reader.is_none() {
            self.shmem_reader = AudioLevelReader::open();
            if self.shmem_reader.is_some() {
                debug!("poll_shmem_audio_levels: lazily opened shmem reader");
            }
        }

        if let Some(ref reader) = self.shmem_reader {
            let (low, high) = reader.read();
            if low > 0 || high > 0 {
                // Log first non-zero read for latency diagnosis
                if self.low_intensity == 0.0 && self.high_intensity == 0.0 {
                    info!(
                        low,
                        high, "[TIMING] First non-zero shmem read in compositor"
                    );
                }
                self.set_audio_level_dual(low, high);
                return true;
            }
        }
        false
    }

    /// Close the shmem reader (call when voice mode ends).
    pub fn close_shmem_reader(&mut self) {
        self.shmem_reader = None;
    }

    /// Update intensity values with smooth interpolation.
    /// Uses real elapsed time for frame-rate-independent smoothing.
    /// Base 0.01 means after 1 second the residual distance is 1%,
    /// giving a smooth ~0.5s perceived response time.
    pub fn update_pulse(&mut self) {
        let now = Instant::now();
        let dt = now.duration_since(self.last_pulse_update).as_secs_f32();
        self.last_pulse_update = now;

        // Clamp dt to avoid jumps after long pauses (e.g. first frame)
        let dt = dt.clamp(0.001, 0.1);

        // Frame-rate-independent exponential lerp:
        //   lerpFactor = 1.0 - pow(base, dt)
        // Base 0.01: at 60fps → lerp ≈ 0.074/frame; at 144fps → lerp ≈ 0.031/frame
        // Produces identical smoothing regardless of refresh rate.
        let lerp_factor = 1.0 - (0.01_f32).powf(dt);

        self.low_intensity += (self.target_low - self.low_intensity) * lerp_factor;
        self.high_intensity += (self.target_high - self.high_intensity) * lerp_factor;

        self.low_intensity = self.low_intensity.clamp(0.0, 1.0);
        self.high_intensity = self.high_intensity.clamp(0.0, 1.0);

        // Smoothly interpolate thinking_progress toward target
        // Target is 1.0 when Frozen (waiting for transcript), 0.0 otherwise
        // Use a slower lerp (base 0.05) for a gentler ~0.8s ramp into thinking mode
        let thinking_lerp = 1.0 - (0.05_f32).powf(dt);
        let thinking_target = if self.orb_state == OrbState::Frozen {
            1.0
        } else {
            0.0
        };
        self.thinking_progress += (thinking_target - self.thinking_progress) * thinking_lerp;
        self.thinking_progress = self.thinking_progress.clamp(0.0, 1.0);
    }

    /// Reset audio level (called when voice mode ends)
    pub fn reset_audio_level(&mut self) {
        // Don't zero out the smoothed values — let them decay naturally via
        // update_pulse() toward target 0. Just zero the target.
        self.target_intensity = 0.0;
        self.target_low = 0.0;
        self.target_high = 0.0;
        self.voice_state = VoiceState::Idle;
    }

    /// Check if the orb should be rendered
    pub fn should_render(&self) -> bool {
        self.scale > 0.001 || self.animation.is_some()
    }

    /// Check if the orb needs continuous rendering (for shader animation time updates)
    /// Returns true when the orb is visible and needs frame updates for animation
    pub fn needs_continuous_render(&self) -> bool {
        // Need continuous render if orb is visible (shader time needs updating)
        // or if there's an active scale/position animation
        self.should_render()
    }

    /// Get the shader time in seconds
    pub fn shader_time(&self) -> f32 {
        self.shader_time_start.elapsed().as_secs_f32()
    }

    /// Restart the shader clock.
    ///
    /// `shader_time_start` is set once at construction, so without this the
    /// `time` uniform is compositor *uptime*. That degrades two ways: an f32
    /// holding a day's worth of seconds quantizes to ~8ms, which is half a frame
    /// at 60Hz and shows up as the animation stepping rather than flowing; and
    /// `sin`/`cos` range reduction on the GPU loses precision badly once the
    /// argument reaches the tens of thousands of radians (`animTime` is
    /// `time * 1.5`).
    ///
    /// Only ever called while the orb is fully hidden. The shader's noise term
    /// walks the z axis of a 3D simplex field and is not periodic, so there is
    /// no phase this can be reset to without a visible jump — it is only safe
    /// when nothing is on screen. In particular this must NOT move to
    /// `show_floating`/`show_attached`, which are reachable mid-animation when a
    /// show interrupts a hide.
    fn reset_shader_time(&mut self) {
        self.shader_time_start = Instant::now();
    }

    /// Check if the orb is attached to a window and in burst phase,
    /// or if we're shrinking from an attached state
    /// When true, the orb should be rendered at the window level (between blur and content)
    /// rather than at the global level (on top of everything)
    pub fn should_render_at_window_level(&self) -> bool {
        // Render at window level if:
        // 1. Attached and in burst phase, OR
        // 2. Currently shrinking from an attached state (hide animation), OR
        // 3. Frozen from attached mode (thinking animation stays at window level)
        (self.orb_state == OrbState::Attached && self.is_in_burst_phase())
            || self.shrinking_from_attached
            || self.is_frozen_attached()
    }

    /// Get the attached window geometry if in window-level render mode
    pub fn attached_window_for_render(&self) -> Option<Rectangle<i32, Logical>> {
        if self.should_render_at_window_level() {
            self.attached_window
        } else {
            None
        }
    }

    /// Get the attached surface ID if in window-level render mode
    pub fn attached_surface_id_for_render(&self) -> Option<&str> {
        if self.should_render_at_window_level() {
            self.attached_surface_id.as_deref()
        } else {
            None
        }
    }

    /// Update the attached window geometry (call during render to keep orb in sync with window)
    /// Also updates the position and burst_scale to track the window center and size
    pub fn update_attached_window_geometry(
        &mut self,
        window_geo: Rectangle<i32, Logical>,
        output_size: Size<i32, Logical>,
    ) {
        if self.is_effectively_attached() || self.shrinking_from_attached {
            // Check if window size changed (need to recalculate burst_scale)
            let size_changed = self
                .attached_window
                .map(|old| old.size != window_geo.size)
                .unwrap_or(true);

            self.attached_window = Some(window_geo);

            // Update position to track window center
            let metrics = OrbWindowMetrics::calculate(window_geo, output_size);
            self.position = metrics.center;

            // Recalculate burst_scale if window size changed (only when attached, not shrinking)
            if size_changed && self.orb_state == OrbState::Attached {
                // Update scale if we're at burst scale (fully expanded)
                if self.animation.is_none() && (self.scale - self.burst_scale).abs() < 0.01 {
                    self.scale = metrics.burst_scale;
                }
                self.burst_scale = metrics.burst_scale;
            }
        }
    }
}

impl VoiceOrbShader {
    /// Initialize the voice orb shader
    pub fn init(renderer: &mut GlesRenderer) -> Result<(), GlesError> {
        let program = renderer.compile_custom_pixel_shader(
            VOICE_ORB_SHADER,
            &[
                UniformName::new("time", UniformType::_1f),
                UniformName::new("scale", UniformType::_1f),
                UniformName::new("iLowIntensity", UniformType::_1f),
                UniformName::new("iHighIntensity", UniformType::_1f),
                UniformName::new("attached", UniformType::_1f),
                UniformName::new("target_center", UniformType::_2f),
                UniformName::new("morph_progress", UniformType::_1f),
                UniformName::new("cover_scale", UniformType::_1f),
                UniformName::new("window_aspect", UniformType::_1f),
                UniformName::new("border_radius", UniformType::_1f),
                UniformName::new("viewport_scale", UniformType::_1f),
                UniformName::new("thinking", UniformType::_1f),
                UniformName::new("opacity", UniformType::_1f),
            ],
        )?;

        renderer
            .egl_context()
            .user_data()
            .insert_if_missing(|| VoiceOrbShader(program));

        Ok(())
    }

    /// Get the voice orb shader program
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> Option<GlesPixelProgram> {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<VoiceOrbShader>()
            .map(|s| s.0.clone())
    }

    /// Create a voice orb render element
    pub fn element<R: AsGlowRenderer>(
        renderer: &R,
        orb_state: &VoiceOrbState,
        output_geo: Rectangle<i32, Logical>,
    ) -> Option<PixelShaderElement> {
        Self::element_with_window_override(renderer, orb_state, output_geo, None, None)
    }

    /// Create a voice orb render element with an optional window geometry override
    /// Use this when the window has moved and we need to render the orb at the new position
    ///
    /// # Arguments
    /// * `window_geo_override` - Optional window geometry to use instead of stored attached geometry
    /// * `border_radius` - Optional window border radius (from theme.radius_s() + 4 for values >= 4)
    pub fn element_with_window_override<R: AsGlowRenderer>(
        renderer: &R,
        orb_state: &VoiceOrbState,
        output_geo: Rectangle<i32, Logical>,
        window_geo_override: Option<Rectangle<i32, Logical>>,
        border_radius: Option<f32>,
    ) -> Option<PixelShaderElement> {
        if !orb_state.should_render() {
            return None;
        }

        let shader = Self::get(renderer)?;

        // Calculate the base orb size (20% of display width)
        let base_orb_size = output_geo.size.w as f32 * 0.20;

        // Determine if we should clip to window (burst phase)
        let in_burst_phase = orb_state.is_in_burst_phase();

        // Use override geometry if provided, otherwise fall back to stored geometry
        let effective_window_geo = window_geo_override.or(orb_state.attached_window);

        // Calculate the orb geometry based on state
        let (geo, orb_scale_in_shader, position_override, effective_scale, viewport_scale) =
            if in_burst_phase {
                // In burst phase: render clipped to window bounds
                // The shader will draw an orb that extends beyond the window
                // but the element geometry clips it
                if let Some(window_geo) = effective_window_geo {
                    // Calculate correct burst_scale for current window geometry
                    // This handles dynamic window resizing
                    let correct_burst_scale =
                        OrbWindowMetrics::burst_scale_for_window(window_geo, base_orb_size);

                    // If we're at or near burst scale (fully expanded), use the corrected burst scale
                    // Otherwise, we're animating and should use the stored scale proportionally
                    let effective_scale = if orb_state.animation.is_none()
                        && (orb_state.scale - orb_state.burst_scale).abs() < 0.01
                    {
                        // Fully burst - use the correct burst scale for current window
                        correct_burst_scale
                    } else {
                        // Animating - interpolate between 0 and correct burst scale
                        // based on how far we are in the animation (0 to burst_scale)
                        let progress = if orb_state.burst_scale > 0.0 {
                            orb_state.scale / orb_state.burst_scale
                        } else {
                            0.0
                        };
                        correct_burst_scale * progress
                    };

                    debug!(
                        "Voice orb burst phase: window_geo={:?}, scale={}, position={:?}",
                        window_geo, effective_scale, orb_state.position
                    );

                    // Use window geometry as render bounds (clipping)
                    // Calculate how large the orb appears relative to window
                    let orb_diameter = base_orb_size * effective_scale;
                    // Shader uses max(width, height) for normalization, so scale relative to that
                    let window_max = window_geo.size.w.max(window_geo.size.h) as f32;

                    // Scale factor: how much bigger is the orb than the window's larger dimension?
                    // This tells the shader to draw the orb larger than the render area
                    let scale_in_shader = orb_diameter / window_max;

                    // Calculate normalized position from actual window geometry
                    let pos = Point::from((
                        (window_geo.loc.x as f32 + window_geo.size.w as f32 / 2.0)
                            / output_geo.size.w as f32,
                        (window_geo.loc.y as f32 + window_geo.size.h as f32 / 2.0)
                            / output_geo.size.h as f32,
                    ));

                    (
                        window_geo,
                        scale_in_shader,
                        Some(pos),
                        effective_scale,
                        1.0f32,
                    )
                } else {
                    return None;
                }
            } else {
                // Floating or darting: render as normal orb
                // Note: Use output-local coordinates (0,0 origin) for element positioning
                // The output_geo.loc is the global position but elements render in local space
                let orb_size = (base_orb_size * orb_state.scale) as i32;
                let render_size = (orb_size as f32 * 2.0) as i32; // Extra space for glow/halo at max audio
                let center_x = (orb_state.position.x * output_geo.size.w as f32) as i32;
                let center_y = (orb_state.position.y * output_geo.size.h as f32) as i32;

                let geo = Rectangle::new(
                    Point::from((center_x - render_size / 2, center_y - render_size / 2)),
                    Size::from((render_size, render_size)),
                );
                // viewport_scale compensates for the larger render area so the orb
                // stays the same visual size as with 1.5x (the original design ratio).
                let viewport_scale = render_size as f32 / (orb_size as f32 * 1.5);
                // In floating mode, orb scale in shader is 1.0 (normal size)
                (geo, 1.0f32, None, orb_state.scale, viewport_scale)
            };

        // Use position override (from actual window geometry) if available
        let position = position_override.unwrap_or(orb_state.position);

        // Time for animation
        let time = orb_state.shader_time();

        // Intensities are already smoothed by update_pulse() — use them directly.
        // No hard gate on voice_state; the smoothing handles fade-in/out naturally.
        let low_intensity = orb_state.low_intensity;
        let high_intensity = orb_state.high_intensity;

        // Window aspect ratio and border radius for proper orb rendering when clipped
        let (window_aspect, is_attached, effective_border_radius) = if in_burst_phase {
            if let Some(window_geo) = effective_window_geo {
                // Use provided border radius or fall back to 0 (no rounding)
                (
                    window_geo.size.w as f32 / window_geo.size.h as f32,
                    1.0f32,
                    border_radius.unwrap_or(0.0),
                )
            } else {
                (1.0, 0.0, 0.0)
            }
        } else {
            (1.0, 0.0, 0.0)
        };

        let uniforms = vec![
            Uniform::new("time", time),
            Uniform::new("scale", effective_scale),
            Uniform::new("iLowIntensity", low_intensity),
            Uniform::new("iHighIntensity", high_intensity),
            Uniform::new("attached", is_attached),
            Uniform::new("target_center", [position.x, position.y]),
            Uniform::new("morph_progress", 0.0f32), // Deprecated, kept for compatibility
            Uniform::new("cover_scale", orb_scale_in_shader),
            Uniform::new("window_aspect", window_aspect),
            Uniform::new("border_radius", effective_border_radius),
            Uniform::new("viewport_scale", viewport_scale),
            Uniform::new("thinking", orb_state.thinking_progress),
            // Carried as a uniform, not as the element's alpha: the element has
            // no alpha setter, so folding the fade in here is what lets the
            // element below be reused instead of rebuilt.
            Uniform::new("opacity", orb_state.opacity),
        ];

        // Reuse the element across frames so its `Id` stays stable — see
        // `VoiceOrbCache`. Only the geometry and uniforms change per frame, and
        // both `resize` and `update_uniforms` bump the commit counter, so the
        // orb still damages its own quad exactly as before.
        let key = match orb_state.attached_surface_id.as_deref() {
            Some(id) if in_burst_phase => VoiceOrbKey::Attached(id.to_string()),
            _ => VoiceOrbKey::Floating,
        };

        let user_data = Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data();
        user_data.insert_if_missing(|| VoiceOrbCache::new(HashMap::new()));
        let mut cache = user_data.get::<VoiceOrbCache>().unwrap().borrow_mut();
        // Only one orb exists at a time, so anything under another key belongs
        // to a previous floating/attached role and will never be reused.
        cache.retain(|k, _| k == &key);

        let elem = cache.entry(key).or_insert_with(|| {
            PixelShaderElement::new(
                shader,
                geo,
                None,
                // Always 1.0; the fade rides on the `opacity` uniform instead.
                1.0,
                // Uniforms are set by `update_uniforms` below, before any draw.
                Vec::new(),
                Kind::Unspecified,
            )
        });
        elem.resize(geo, None);
        elem.update_uniforms(uniforms);

        Some(elem.clone())
    }
}
