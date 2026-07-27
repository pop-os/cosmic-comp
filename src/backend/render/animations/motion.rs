// SPDX-License-Identifier: GPL-3.0-only

//! The compositor's single motion-token vocabulary — durations, easing curves,
//! and named spring presets — so every animation in the shell pulls from one
//! source instead of ad-hoc `Duration`s and per-feature copies of the same
//! bezier solver.
//!
//! This mirrors the app-side design tokens exposed by `icetron-themes`
//! (`duration_*`, `ease_*`, `spring_*`) so the compositor and the first-party
//! icetron apps share one motion language. The compositor cannot link
//! `icetron-themes` (separate crate graph), so these values are hand-synced from
//! the same design source — keep them in step when the design tokens change.
//!
//! Springs are evaluated by the analytic engine in [`super::spring`]
//! (Niri/libadwaita), parameterized as `SpringParams::new(damping_ratio,
//! stiffness, epsilon)`. That is a DIFFERENT parameterization from icetron's
//! `[stiffness, damping, mass]` arrays, so the presets below are aligned to
//! their icetron counterparts by *intent*, not copied by value.

use std::time::Duration;

use icetron_themes::ThemeInterface;

pub use super::spring::{Spring, SpringParams};

/// Convert `f32` milliseconds (icetron `duration_*` tokens) to a [`Duration`].
pub fn ms(millis: f32) -> Duration {
    Duration::from_millis(millis.max(0.0).round() as u64)
}

/// Convert an icetron spring preset `[stiffness, damping, mass]` into the
/// analytic engine's [`SpringParams`]. icetron gives *raw* damping (not a ratio),
/// which maps directly onto the struct fields.
pub fn spring_params(preset: [f32; 3]) -> SpringParams {
    SpringParams {
        stiffness: preset[0] as f64,
        damping: preset[1] as f64,
        mass: preset[2] as f64,
        epsilon: SPRING_EPSILON,
    }
}

/// Evaluate an icetron easing token (cubic-bezier control points
/// `[p1x, p1y, p2x, p2y]`) at progress `t ∈ [0,1]`.
pub fn cubic_bezier_cp(t: f32, cp: [f32; 4]) -> f32 {
    cubic_bezier(t, cp[0] as f64, cp[1] as f64, cp[2] as f64, cp[3] as f64)
}

/// A snapshot of the motion tokens the compositor needs, resolved **once** from
/// the active theme (icetron-themes, via `CompTheme`) so the animation code
/// reads real design values instead of hardcoded constants. Small and `Copy`;
/// rebuilt whenever the theme changes.
///
/// Durations map onto icetron's `duration_*` scale (`fast 100 / normal 200 /
/// slow 300 / slower 500`); a few compositor motions sit between steps and take
/// the nearest one (see [`Motion::from_theme`]).
#[derive(Debug, Clone, Copy)]
pub struct Motion {
    /// General window/layout transition (was `ANIMATION_DURATION`). `normal`.
    pub animation: Duration,
    /// Layer surface fade-in. `normal`.
    pub layer_fade_in: Duration,
    /// Layer surface fade-out. `fast`.
    pub layer_fade_out: Duration,
    /// Fullscreen enter/leave. `normal`.
    pub fullscreen: Duration,
    /// Tab-strip scroll. `normal`.
    pub scroll: Duration,
    /// Slide-content crossfade (was 220ms). `normal`.
    pub slide_crossfade: Duration,
    /// Layer open/close fade+rise (was 160ms). `normal`.
    pub layer_open: Duration,
    /// Minimize / unminimize scale-to-dock (was 320ms). `slow`.
    pub minimize: Duration,
    /// Panel show/hide slide + width resize (was 420ms). `slower`.
    pub panel_slide: Duration,
    /// Panel slide / width-resize ease-out control points (design
    /// `--ease-out-expo`). A smooth ease-out with no overshoot — NOT the theme's
    /// `ease_spring` (which is a bounce curve; springy motion uses the physics
    /// presets like `window_spring`).
    pub panel_ease_cp: [f32; 4],
    /// `--ease-in-out` control points (layer open/close fade+rise).
    pub ease_in_out_cp: [f32; 4],
    /// Window/workspace spring (`spring_window`), macOS-smooth.
    pub window_spring: SpringParams,
}

impl Motion {
    /// Resolve the snapshot from the active theme.
    pub fn from_theme(theme: &dyn ThemeInterface) -> Self {
        Self {
            animation: ms(theme.duration_normal()),
            layer_fade_in: ms(theme.duration_normal()),
            layer_fade_out: ms(theme.duration_fast()),
            fullscreen: ms(theme.duration_normal()),
            scroll: ms(theme.duration_normal()),
            slide_crossfade: ms(theme.duration_normal()),
            layer_open: ms(theme.duration_normal()),
            minimize: ms(theme.duration_slow()),
            panel_slide: ms(theme.duration_slower()),
            panel_ease_cp: theme.ease_out_expo(),
            ease_in_out_cp: theme.ease_in_out(),
            window_spring: spring_params(theme.spring_window()),
        }
    }

    /// Eased panel slide/resize factor at progress `t` (design `--ease-out-expo`).
    pub fn panel_ease(&self, t: f32) -> f32 {
        cubic_bezier_cp(t, self.panel_ease_cp)
    }

    /// Eased `--ease-in-out` factor at progress `t`.
    pub fn ease_in_out(&self, t: f32) -> f32 {
        cubic_bezier_cp(t, self.ease_in_out_cp)
    }
}

// === Easing solver ===========================================================
//
// The compositor evaluates easing curves whose control points come from the
// theme (`Motion::ease_spring`/`ease_in_out`, via [`cubic_bezier_cp`]). The
// solver below is the only easing math the compositor owns; the *values* are
// design tokens resolved into [`Motion`].

/// Evaluate a parametric cubic-bezier easing `y(t)` with control points
/// `P0=(0,0)`, `P1=(p1x,p1y)`, `P2=(p2x,p2y)`, `P3=(1,1)`. Solves `x(u)=t` for
/// the bezier parameter `u` with 8 Newton-Raphson iterations, then samples
/// `y(u)`. Callers pass progress already clamped to `[0,1]`.
pub fn cubic_bezier(t: f32, p1x: f64, p1y: f64, p2x: f64, p2y: f64) -> f32 {
    let t = t as f64;
    let mut u = t; // initial guess
    for _ in 0..8 {
        let x = bezier_component(u, p1x, p2x) - t;
        let dx = bezier_component_derivative(u, p1x, p2x);
        if dx.abs() < 1e-12 {
            break;
        }
        u -= x / dx;
        u = u.clamp(0.0, 1.0);
    }
    bezier_component(u, p1y, p2y) as f32
}

/// One component of a cubic bezier at parameter `u`:
/// `B(u) = 3(1-u)²·u·p1 + 3(1-u)·u²·p2 + u³`.
fn bezier_component(u: f64, p1: f64, p2: f64) -> f64 {
    let u2 = u * u;
    let u3 = u2 * u;
    let inv = 1.0 - u;
    let inv2 = inv * inv;
    3.0 * inv2 * u * p1 + 3.0 * inv * u2 * p2 + u3
}

/// Derivative of [`bezier_component`] with respect to `u`.
fn bezier_component_derivative(u: f64, p1: f64, p2: f64) -> f64 {
    let inv = 1.0 - u;
    3.0 * inv * inv * p1 + 6.0 * inv * u * (p2 - p1) + 3.0 * u * u * (1.0 - p2)
}

/// Settle threshold for springs built from theme presets ([`spring_params`]).
const SPRING_EPSILON: f64 = 0.0001;

#[cfg(test)]
mod tests {
    use super::*;

    fn approx(a: f32, b: f32) {
        assert!((a - b).abs() < 1e-4, "{a} != {b}");
    }

    #[test]
    fn cubic_bezier_cp_pins_endpoints() {
        // CSS `ease-in-out` control points (the theme's `ease_in_out`).
        let cp = [0.42, 0.0, 0.58, 1.0];
        approx(cubic_bezier_cp(0.0, cp), 0.0);
        approx(cubic_bezier_cp(1.0, cp), 1.0);
        // ease-in-out is point-symmetric about (0.5, 0.5).
        approx(cubic_bezier_cp(0.5, cp), 0.5);
    }

    #[test]
    fn ms_converts_and_clamps() {
        assert_eq!(ms(200.0), Duration::from_millis(200));
        assert_eq!(ms(0.0), Duration::ZERO);
        // Negative inputs clamp to zero rather than wrapping.
        assert_eq!(ms(-5.0), Duration::ZERO);
    }

    #[test]
    fn spring_params_maps_icetron_array() {
        // icetron `[stiffness, damping, mass]` maps straight onto SpringParams.
        let p = spring_params([300.0, 33.0, 1.0]);
        approx(p.stiffness as f32, 300.0);
        approx(p.damping as f32, 33.0);
        approx(p.mass as f32, 1.0);
        approx(p.epsilon as f32, SPRING_EPSILON as f32);
    }

    #[test]
    fn spring_from_preset_settles() {
        // A spring built from the tuned `spring_window` preset settles in finite time.
        let s = Spring {
            from: 0.0,
            to: 1.0,
            initial_velocity: 0.0,
            params: spring_params([300.0, 33.0, 1.0]),
        };
        approx(s.value_at(Duration::ZERO) as f32, 0.0);
        assert!(s.duration() < Duration::from_secs(3));
    }
}
