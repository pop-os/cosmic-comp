// SPDX-License-Identifier: GPL-3.0-only

//! Night shift applied by the CRTC gamma LUT.
//!
//! The `offscreen.frag` postprocess can only tint frames the surface thread renders,
//! so anything committed elsewhere - modesets, plane-allowance changes, smithay's
//! bandwidth fallbacks - lands untinted and the tint visibly blinks. A gamma ramp
//! lives on the CRTC instead, so it colours whatever is scanned out no matter who
//! committed it, and it leaves direct scanout intact rather than forcing a fullscreen
//! offscreen composite every frame.
//!
//! Not every driver exposes a gamma LUT, so [`apply`] reports whether it took effect
//! and the caller keeps the shader as a fallback.

use anyhow::{Context, Result};
use smithay::reexports::drm::control::{Device as ControlDevice, crtc};

/// At or above this the ramp is the identity; `offscreen.frag` uses the same cutoff.
pub const NEUTRAL_KELVIN: u16 = 6500;

/// Per-channel multipliers for a colour temperature in Kelvin.
///
/// Tanner Helland's Planckian-locus approximation, normalized so 6500 K maps to
/// `(1, 1, 1)`. Kept identical to the `night_shift` block in `offscreen.frag` so the
/// hardware ramp and the shader fallback look the same.
pub fn multipliers(kelvin: u16) -> [f32; 3] {
    if kelvin == 0 || kelvin >= NEUTRAL_KELVIN {
        return [1.0; 3];
    }

    // f64 throughout: the coefficients carry more digits than an f32 literal can hold.
    let temp = f64::from(kelvin) / 100.0;

    let red = if temp <= 66.0 {
        1.0
    } else {
        (1.29293618606 * (temp - 60.0).powf(-0.1332047592)).clamp(0.0, 1.0)
    };
    let green = if temp <= 66.0 {
        (0.39008157876 * temp.ln() - 0.63184144378).clamp(0.0, 1.0)
    } else {
        (1.12989086089 * (temp - 60.0).powf(-0.0755148492)).clamp(0.0, 1.0)
    };
    let blue = if temp >= 66.0 {
        1.0
    } else if temp <= 19.0 {
        0.0
    } else {
        (0.54320678911 * (temp - 10.0).ln() - 1.19625408914).clamp(0.0, 1.0)
    };

    // Normalize against the 6500 K values so daylight is a no-op.
    let norm_green = 0.39008157876 * 65.0_f64.ln() - 0.63184144378;
    let norm_blue = 0.54320678911 * 55.0_f64.ln() - 1.19625408914;

    [
        red as f32,
        (green / norm_green) as f32,
        (blue / norm_blue) as f32,
    ]
}

/// Number of entries in the crtc's gamma LUT, `None` when the driver has none.
fn lut_size(device: &impl ControlDevice, crtc: crtc::Handle) -> Option<usize> {
    let len = device.get_crtc(crtc).ok()?.gamma_length() as usize;
    // A single-entry LUT can't express a ramp.
    (len > 1).then_some(len)
}

/// A linear ramp scaled by `mult`, in the 16-bit units the LUT takes.
fn ramp(mult: f32, size: usize) -> Vec<u16> {
    (0..size)
        .map(|i| {
            let value = (i as f32 / (size - 1) as f32) * mult;
            (value.clamp(0.0, 1.0) * f32::from(u16::MAX)).round() as u16
        })
        .collect()
}

/// Program `kelvin` into the crtc's gamma LUT; 0 (or anything at/above
/// [`NEUTRAL_KELVIN`]) restores the identity ramp.
///
/// Returns `false` when the driver exposes no usable gamma LUT, in which case the
/// caller has to keep tinting in the shader.
pub fn apply(device: &impl ControlDevice, crtc: crtc::Handle, kelvin: u16) -> Result<bool> {
    let Some(size) = lut_size(device, crtc) else {
        return Ok(false);
    };

    let [red, green, blue] = multipliers(kelvin);
    device
        .set_gamma(
            crtc,
            &ramp(red, size),
            &ramp(green, size),
            &ramp(blue, size),
        )
        .context("Failed to set crtc gamma ramp")?;

    Ok(true)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn daylight_and_disabled_are_neutral() {
        assert_eq!(multipliers(0), [1.0; 3]);
        assert_eq!(multipliers(NEUTRAL_KELVIN), [1.0; 3]);
        assert_eq!(multipliers(9000), [1.0; 3]);
    }

    #[test]
    fn warm_temperatures_only_attenuate_green_and_blue() {
        let [red, green, blue] = multipliers(3500);
        assert_eq!(red, 1.0);
        assert!((0.0..1.0).contains(&green), "green was {green}");
        assert!((0.0..1.0).contains(&blue), "blue was {blue}");
        // Blue is pulled down harder than green - that is what makes it look warm.
        assert!(blue < green);
    }

    #[test]
    fn warmer_is_dimmer_on_blue() {
        assert!(multipliers(2000)[2] < multipliers(4000)[2]);
    }

    #[test]
    fn identity_ramp_spans_the_full_range() {
        let lut = ramp(1.0, 256);
        assert_eq!(lut[0], 0);
        assert_eq!(lut[255], u16::MAX);
        assert!(lut.windows(2).all(|w| w[0] <= w[1]));
    }

    #[test]
    fn scaled_ramp_is_clamped_and_monotonic() {
        let lut = ramp(0.5, 1024);
        assert_eq!(lut[0], 0);
        assert_eq!(lut[1023], u16::MAX / 2 + 1);
        assert!(lut.windows(2).all(|w| w[0] <= w[1]));
    }
}
