// SPDX-License-Identifier: GPL-3.0-only

//! Tearing mode policy.
//!
//! Decides, per output, whether the KMS surface thread should submit frames
//! with async (tearing) page flips instead of the usual vsync'd flips. This
//! module is policy-only: it makes no KMS/rendering decisions itself, it
//! just answers "should tearing be active right now" given the inputs the
//! caller (the surface render loop, Task 6) already has on hand.
//!
//! Kept deliberately free of deep type dependencies (no `Output`, no
//! `Shell`, no `SurfaceThreadState`) so the policy is unit-testable without
//! constructing any of that machinery.

use std::sync::OnceLock;

use crate::wayland::handlers::tearing_control::PresentationHint;

/// Caches the result of reading the `COSMIC_ALLOW_TEARING` environment
/// variable so we only touch the environment once per process lifetime.
static TEARING_ENV_ENABLED: OnceLock<bool> = OnceLock::new();

/// Returns whether tearing is allowed by the environment.
///
/// Reads `COSMIC_ALLOW_TEARING` once (via [`OnceLock`]) and caches the
/// result; the env var is considered "on" iff it is exactly `"1"`.
// consumed by Task 6
#[allow(dead_code)]
pub fn tearing_env_enabled() -> bool {
    *TEARING_ENV_ENABLED.get_or_init(|| {
        std::env::var("COSMIC_ALLOW_TEARING")
            .map(|v| v == "1")
            .unwrap_or(false)
    })
}

/// Decides whether tearing (async page flips) should be active for an
/// output right now.
///
/// All of the following must hold:
/// 1. [`tearing_env_enabled`] is `true`.
/// 2. `mirroring` is `false` (the output is not mirroring another output).
/// 3. `fullscreen_hint` is `Some(PresentationHint::Async)` — i.e. there is a
///    fullscreen surface on the output and its tearing-control hint is
///    `Async`.
///
/// The caller is expected to resolve `fullscreen_hint` itself, e.g. via
/// `workspace.get_fullscreen(seat).map(|fs| presentation_hint(fs.surface.wl_surface()))`
/// (see `redraw()` in `src/backend/kms/surface/mod.rs` for the existing
/// accessor chain used for direct-scanout candidacy).
// consumed by Task 6
#[allow(dead_code)]
pub fn tearing_active(mirroring: bool, fullscreen_hint: Option<PresentationHint>) -> bool {
    tearing_active_inner(tearing_env_enabled(), mirroring, fullscreen_hint)
}

/// Test-injectable core of [`tearing_active`]; takes the env-enabled bit as
/// a plain argument instead of reading the process environment, so unit
/// tests never need to mutate global state.
fn tearing_active_inner(
    env_enabled: bool,
    mirroring: bool,
    fullscreen_hint: Option<PresentationHint>,
) -> bool {
    env_enabled && !mirroring && fullscreen_hint == Some(PresentationHint::Async)
}

/// Frame submission/rejection counters for tearing mode.
///
/// Intended to be accumulated while tearing mode is active on an output and
/// flushed via [`TearingCounters::log_and_reset`] when the output leaves
/// tearing mode (or periodically), so operators can see how often tearing
/// submissions were rejected (e.g. falling back to vsync mid-flight).
#[derive(Debug, Default)]
// consumed by Task 6
#[allow(dead_code)]
pub struct TearingCounters {
    pub submitted: u64,
    pub rejected: u64,
}

impl TearingCounters {
    /// Logs the current counters via `tracing::info!` and resets both to
    /// zero.
    // consumed by Task 6
    #[allow(dead_code)]
    pub fn log_and_reset(&mut self) {
        tracing::warn!(
            submitted = self.submitted,
            rejected = self.rejected,
            "tearing mode counters"
        );
        self.submitted = 0;
        self.rejected = 0;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn env_disabled_is_always_false() {
        // Even with mirroring off and a matching fullscreen hint, tearing
        // must stay off if the env var isn't enabled.
        assert!(!tearing_active_inner(
            false,
            false,
            Some(PresentationHint::Async)
        ));
    }

    #[test]
    fn mirroring_forces_false() {
        assert!(!tearing_active_inner(
            true,
            true,
            Some(PresentationHint::Async)
        ));
    }

    #[test]
    fn no_fullscreen_surface_is_false() {
        assert!(!tearing_active_inner(true, false, None));
    }

    #[test]
    fn vsync_hint_is_false() {
        assert!(!tearing_active_inner(
            true,
            false,
            Some(PresentationHint::Vsync)
        ));
    }

    #[test]
    fn all_conditions_met_is_true() {
        assert!(tearing_active_inner(
            true,
            false,
            Some(PresentationHint::Async)
        ));
    }

    #[test]
    fn log_and_reset_zeroes_counters() {
        let mut counters = TearingCounters {
            submitted: 42,
            rejected: 7,
        };
        counters.log_and_reset();
        assert_eq!(counters.submitted, 0);
        assert_eq!(counters.rejected, 0);
    }
}
