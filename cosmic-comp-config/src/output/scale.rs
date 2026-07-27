// SPDX-License-Identifier: GPL-3.0-only

//! Heuristics for choosing a sensible default ("recommended") display scale.
//!
//! This lives in the config crate — rather than the compositor binary — so that
//! other tools (cosmic-settings, a first-run / FTUE app) can compute and label
//! the exact same "Recommended" value the compositor seeds onto a freshly-seen
//! output, e.g. "125% (Recommended)".

/// Pixel-density bounds (DPI) outside which a non-zero EDID physical size is
/// treated as bogus. Real panels — from ~110" 1080p displays (~20 DPI) up to
/// dense 4K laptops and small tablets (~500 DPI) — sit comfortably inside.
/// Values outside imply a junk size, such as centimetres reported in the
/// millimetre field (≈10× too small → ≈10× too high DPI), or a KVM /
/// passthrough feeding a fixed dummy size. The active resolution is always
/// trustworthy, so an implausible DPI means the *size* is wrong.
const MIN_PLAUSIBLE_DPI: f64 = 20.0;
const MAX_PLAUSIBLE_DPI: f64 = 600.0;

/// Target pixel density (DPI) that maps to 100% scale on a built-in panel.
/// Laptop screens are viewed from ~50-60cm, so they tolerate a denser target
/// than a desktop monitor before the UI needs scaling up.
const LAPTOP_DPI_TARGET: u32 = 144;

/// Target pixel density (DPI) that maps to 100% scale on an external monitor.
///
/// Set to 100 rather than the upstream 144-derived 120 because AgentOS displays
/// are routinely viewed from further than a desk-bound office monitor, and a
/// 120 target left the whole ~105-120 DPI class (27" 1440p, 34" 3440x1440,
/// 49" 5120x1440) at 100%, which reads too small at that distance.
///
/// The value is chosen for margin, not to hit one panel: the snap-to-50 in
/// [`scale_from_dpi`] means every target in ~93..=108 produces an identical
/// table, and 100 sits centrally in that window. Below ~93 it would start
/// pulling in the genuinely low-density ~92 DPI class (32" 1440p,
/// 57" 5120x1440); above ~108 it stops catching 27" 1440p at all.
const DESKTOP_DPI_TARGET: u32 = 100;

/// Compute a recommended fractional scale for an output from its physical size
/// and active resolution.
///
/// `embedded` is `true` for built-in panels (i.e. the connector interface is
/// embedded DisplayPort / eDP); it biases the heuristic towards the laptop DPI
/// target. The caller maps its connector type onto this flag — in the
/// compositor that is `interface == connector::Interface::EmbeddedDisplayPort`.
///
/// `monitor_size_mm` is the physical size reported by EDID; `(0, 0)` (or other
/// non-sensical values) trigger a resolution-only fallback.
pub fn calculate_scale(embedded: bool, monitor_size_mm: (u32, u32), resolution: (u16, u16)) -> f64 {
    let (w_mm, h_mm) = monitor_size_mm;
    let (w_px, h_px) = resolution;
    let shorter_res = w_px.min(h_px);
    let fallback_scale = match shorter_res {
        px if px <= 1600 => 1.0,
        _ => 2.0,
    };
    if w_mm == 0 || h_mm == 0 {
        // possibly projector, but could just be some no-brand display
        return fallback_scale;
    }

    let (w_in, h_in) = (w_mm as f64 / 25.4, h_mm as f64 / 25.4);
    // due to edid's setting non-sensicle values,
    // lets be really careful we don't devide by 0 (or non-normal values) when deriving values from size.
    // (also the size should be positive, but the u16 arguments already force that.)
    if !w_in.is_normal() || !h_in.is_normal() {
        return fallback_scale;
    }

    let dpi = (w_px as f64 / w_in + h_px as f64 / h_in) / 2.0;

    // Reject bogus-but-non-zero EDID sizes: the resolution is trustworthy, so a
    // DPI outside the plausible range means the physical size is junk (e.g.
    // centimetres in the millimetre field, or KVM passthrough garbage). Trusting
    // it would mis-snap the scale or misclassify the panel as a laptop/TV.
    if !(MIN_PLAUSIBLE_DPI..=MAX_PLAUSIBLE_DPI).contains(&dpi) {
        return fallback_scale;
    }

    let diag = diagonal_inches(w_in, h_in);

    match diag {
        _diag if diag < 20. || embedded => {
            // likely laptop
            scale_from_dpi(dpi, LAPTOP_DPI_TARGET, shorter_res)
        }
        // 16:10 has a height of 19.6, anything lower is most likely an ultrawide
        _diag if diag >= 40. && h_in >= 19. => {
            // likely TV
            match shorter_res {
                px if px <= 1200 => 1.0,
                _ => 2.0,
            }
        }
        _ => {
            // likely desktop
            scale_from_dpi(dpi, DESKTOP_DPI_TARGET, shorter_res)
        }
    }
}

pub fn scale_from_dpi(dpi: f64, step_size: u32, shorter_px: u16) -> f64 {
    let scale = (dpi / step_size as f64) * 100.0;

    // snap to 50%
    let scale = ((scale.round() as u32).next_multiple_of(50) as f64) / 100.0;

    // max values
    let max = match shorter_px {
        px if px <= 1200 => 1.0, // 125% is usually a worse experience than 100% atm
        // for 1440p and weird variants we let the algorithm take over, but limit the max, this is inspired by what windows does
        px if px <= 1600 => 1.5,
        _ => 2.0, // never go higher than 200% by default though (Note for the future: Tablets? Phones?)
    };

    let min = match shorter_px {
        px if px >= 2000 => 2.0, // 4k should default to 200%, as we prefer integer scales
        _ => 1.0,                // never go lower than 100%
    };

    scale.min(max).max(min)
}

/// Physical diagonal of a panel in inches, from its width and height in inches.
///
/// This is the Pythagorean diagonal `sqrt(w² + h²)`. It was historically
/// written as `sqrt(w² · h²)` — which is just the panel *area* (`w · h`), not a
/// diagonal — so the diagonal-keyed laptop (`< 20"`) and TV (`>= 40"`) gates in
/// [`calculate_scale`] never matched on real hardware (every monitor's area is
/// far above 40), leaving classification to the `embedded`/`h_in` fallbacks.
fn diagonal_inches(w_in: f64, h_in: f64) -> f64 {
    (w_in.powi(2) + h_in.powi(2)).sqrt()
}

#[cfg(test)]
mod test {
    use super::{calculate_scale, diagonal_inches};

    /// Build a panel of `diag_in` true-diagonal inches at the given pixel
    /// resolution and return its recommended scale as a percentage. `embedded`
    /// mirrors the compositor's `interface == EmbeddedDisplayPort` check.
    fn scale(embedded: bool, diag_in: f64, w_px: u16, h_px: u16) -> i32 {
        let aspect = (w_px as f64) / (h_px as f64);
        let diag_mm = diag_in * 25.4;
        let h_mm = diag_mm / (aspect.powf(2.0) + 1.0).sqrt();
        let w_mm = h_mm * aspect;
        // Round (don't truncate) to mimic how a real panel reports integer mm:
        // truncating both dims undershoots the recovered diagonal and would trip
        // the sharp `>= 40"` TV boundary for an exactly-40" display.
        let size_mm = (w_mm.round() as u32, h_mm.round() as u32);
        (calculate_scale(embedded, size_mm, (w_px, h_px)) * 100.0) as i32
    }

    #[test]
    fn test_scale() {
        // These laptop panels are all under 20" diagonal, so they take the
        // laptop branch whether or not the connector is embedded (eDP vs LVDS).
        // Laptops (eDP -> embedded; LVDS -> non-embedded)
        for &embedded in &[true, false] {
            // 14 inch 3:2
            assert_eq!(scale(embedded, 14.0, 3000, 2000), 200);

            // Various sizes 16:9
            for &diag_in in &[14.0, 15.6, 17.3] {
                assert_eq!(scale(embedded, diag_in, 1920, 1080), 100);
                assert_eq!(scale(embedded, diag_in, 2560, 1440), 150);
                assert_eq!(scale(embedded, diag_in, 3840, 2160), 200);
            }

            // Various sizes 16:10
            for &diag_in in &[14.0, 16.0] {
                assert_eq!(scale(embedded, diag_in, 1920, 1200), 100);
                assert_eq!(scale(embedded, diag_in, 2560, 1600), 150);
                assert_eq!(scale(embedded, diag_in, 3840, 2400), 200);
            }
        }

        // Desktops (non-embedded)
        {
            // 24 inch 16:9
            assert_eq!(scale(false, 24.0, 1920, 1080), 100);
            assert_eq!(scale(false, 24.0, 2560, 1440), 150);
            assert_eq!(scale(false, 24.0, 3840, 2160), 200);

            // Larger sizes 16:9
            for &diag_in in &[27.0, 32.0, 38.0] {
                assert_eq!(scale(false, diag_in, 1920, 1080), 100);
                assert_eq!(scale(false, diag_in, 3840, 2160), 200);
            }
            // At 1440p the 27" is ~109 DPI and scales to 150%, while the 32"
            // (~92 DPI) and 38" (~77 DPI) are genuinely low-density and stay at
            // 100%. See `DESKTOP_DPI_TARGET`.
            assert_eq!(scale(false, 27.0, 2560, 1440), 150);
            assert_eq!(scale(false, 32.0, 2560, 1440), 100);
            assert_eq!(scale(false, 38.0, 2560, 1440), 100);

            // Smaller sizes 21:9
            for &diag_in in &[26.0, 29.0] {
                assert_eq!(scale(false, diag_in, 2560, 1080), 100);
                assert_eq!(scale(false, diag_in, 3440, 1440), 150);
                assert_eq!(scale(false, diag_in, 5120, 2160), 200);
            }

            // Larger sizes 21:9
            for &diag_in in &[34.0, 45.0] {
                assert_eq!(scale(false, diag_in, 2560, 1080), 100);
                assert_eq!(scale(false, diag_in, 5120, 2160), 200);
            }
            // 34" 3440x1440 is ~110 DPI — the same density class as a 27" 1440p
            // — so it scales to 150%. The 45" (~83 DPI) stays at 100%.
            assert_eq!(scale(false, 34.0, 3440, 1440), 150);
            assert_eq!(scale(false, 45.0, 3440, 1440), 100);

            // Various sizes 32:9
            for &diag_in in &[45.0, 49.0, 57.0] {
                assert_eq!(scale(false, diag_in, 3840, 1080), 100);
                assert_eq!(scale(false, diag_in, 7680, 2160), 200);
            }
            // At 5120x1440 the 45" (~118 DPI) and 49" (~109 DPI) land in the
            // same density class as a 27" 1440p and scale to 150%; the 57"
            // (~93 DPI) stays at 100%.
            assert_eq!(scale(false, 45.0, 5120, 1440), 150);
            assert_eq!(scale(false, 49.0, 5120, 1440), 150);
            assert_eq!(scale(false, 57.0, 5120, 1440), 100);
        }

        // TVs (non-embedded)
        {
            // Various sizes 16:9
            for &diag_in in &[40.0, 42.0, 48.0, 55.0, 65.0, 77.0, 83.0] {
                assert_eq!(scale(false, diag_in, 1920, 1080), 100);
                assert_eq!(scale(false, diag_in, 2560, 1440), 200);
                assert_eq!(scale(false, diag_in, 3840, 2160), 200);
            }
        }

        // Zero sized displays (projectors, invalid EDID)
        {
            assert_eq!(scale(false, 0.0, 1920, 1080), 100);
            assert_eq!(scale(false, 0.0, 2560, 1440), 100);
            assert_eq!(scale(false, 0.0, 3840, 2160), 200);
        }
    }

    #[test]
    fn diagonal_is_pythagorean() {
        // `diagonal_inches` must be sqrt(w² + h²), not the area-root (w·h) the
        // old code computed. A 16:9 panel cut from a 27" diagonal must read
        // back ≈27" — whereas its area is ≈313 (the value the bug produced).
        let aspect = 16.0 / 9.0_f64;
        let h = 27.0 / (aspect.powi(2) + 1.0).sqrt();
        let w = h * aspect;
        assert!((diagonal_inches(w, h) - 27.0).abs() < 0.01);
        assert!(w * h > 300.0); // the old `sqrt(w²·h²)` would have landed here
    }

    #[test]
    fn large_monitor_below_40in_is_not_a_tv() {
        // A 39" 16:9 1440p panel sits just under the 40" TV threshold by true
        // diagonal, so it is a (low-DPI) desktop scaled at 100% — not a TV
        // forced to 200%. The old area-based `diag` (≈650) tripped the TV
        // branch and over-scaled it. (The neighbouring 40" panel *is* a TV at
        // 1440p -> 200%, asserted in `test_scale`.)
        assert_eq!(scale(false, 39.0, 2560, 1440), 100);
    }

    #[test]
    fn small_external_monitor_uses_laptop_density() {
        // A 19.3" 3200x1800 external (non-eDP) panel is below the 20" laptop
        // threshold by true diagonal, so it gets the denser laptop target
        // (150%). The old area-based `diag` (≈159) skipped the laptop branch,
        // landing on the desktop target and over-scaling to 200%.
        assert_eq!(scale(false, 19.3, 3200, 1800), 150);
    }

    #[test]
    fn garbage_small_edid_size_falls_back() {
        // A 27" 1440p monitor whose EDID reports its size in centimetres
        // (60×34) instead of millimetres reads as ~1080 DPI. The size is junk,
        // so we fall back to the resolution-only guess (100%) instead of
        // trusting it — which would otherwise snap to 150%.
        assert_eq!(calculate_scale(false, (60, 34), (2560, 1440)), 1.0);
    }

    #[test]
    fn garbage_large_edid_size_falls_back() {
        // A 27" 1440p monitor reporting a ~10× too-large size (6000×3360 mm)
        // reads as ~11 DPI and would be misclassified as a giant TV (200%).
        // Fall back to the resolution-only guess (100%) instead.
        assert_eq!(calculate_scale(false, (6000, 3360), (2560, 1440)), 1.0);
    }

    /// Regression test pinned to real hardware rather than a synthesised
    /// diagonal: the EDID values a 27" LG UltraGear (1440p native) and a
    /// Samsung eDP laptop panel actually report, as read back from
    /// `cosmic-randr list`. The monitor must recommend 150%, and lowering
    /// `DESKTOP_DPI_TARGET` must not disturb the embedded panel.
    #[test]
    fn real_hardware_recommendations() {
        // LG ULTRAGEAR 27" 1440p on DisplayPort — ~109 DPI.
        assert_eq!(calculate_scale(false, (600, 340), (2560, 1440)), 1.5);
        // Same panel driven at its (30Hz-only) 4K mode is ~163 DPI -> 200%.
        assert_eq!(calculate_scale(false, (600, 340), (3840, 2160)), 2.0);
        // Samsung ATNA40HQ08-0 built-in panel — ~242 DPI, unchanged at 200%.
        assert_eq!(calculate_scale(true, (300, 190), (2880, 1800)), 2.0);
    }

    #[test]
    fn dense_4k_laptop_is_not_treated_as_garbage() {
        // A 14" 4K laptop is ~315 DPI — dense but real — and must stay inside
        // the plausible-DPI window (scaled at 200%), not be rejected as bogus.
        assert_eq!(scale(true, 14.0, 3840, 2160), 200);
    }
}
