// SPDX-License-Identifier: GPL-3.0-only

//! Night shift applied by the display controller.
//!
//! The `offscreen.frag` postprocess can only tint frames the surface thread renders,
//! so anything committed elsewhere - modesets, plane-allowance changes, smithay's
//! bandwidth fallbacks - lands untinted and the tint visibly blinks. Colour hardware
//! on the CRTC colours whatever is scanned out no matter who committed it, and it
//! leaves direct scanout intact instead of forcing a fullscreen offscreen composite
//! every frame.
//!
//! Two mechanisms, tried in order, because coverage differs sharply by driver:
//!
//! * `GAMMA_LUT` - a per-channel ramp after blending, reached through the legacy
//!   gamma ioctl (the kernel routes it onto the atomic property for us).
//! * `CTM` - a colour matrix; a warm shift is just a diagonal scale. Qualcomm's
//!   `msm` DPU exposes this and *no* gamma LUT, so the second tier is not academic.
//!
//! [`apply`] reports which one took, or `None` when the CRTC has neither - the caller
//! then keeps tinting in the shader, which is also what the nested winit/x11 backends
//! always do, having no CRTC at all.

use anyhow::{Context, Result};
use smithay::reexports::drm::control::{
    AtomicCommitFlags, Device as ControlDevice, atomic::AtomicModeReq, crtc, property,
};

/// At or above this the tint is the identity; `offscreen.frag` uses the same cutoff.
pub const NEUTRAL_KELVIN: u16 = 6500;

/// Tracing target for which tier a CRTC landed on. Enabled by default in
/// `logger::init_logger` - the base filter is `warn` in release, and "did this machine
/// get hardware or the shader?" is the first question when night shift misbehaves, on
/// exactly the machines where nobody set RUST_LOG. One line per output per change;
/// silence with `RUST_LOG=night-shift=off`.
pub const LOG_TARGET: &str = "night-shift";

/// The display-controller feature carrying the tint.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mechanism {
    /// Per-channel ramp after blending.
    GammaLut,
    /// Colour transformation matrix, used here as a diagonal per-channel scale.
    Ctm,
}

/// What a CRTC currently has programmed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Applied {
    pub kelvin: u16,
    pub mechanism: Mechanism,
}

/// Per-channel multipliers for a colour temperature in Kelvin.
///
/// Tanner Helland's Planckian-locus approximation, normalized so 6500 K maps to
/// `(1, 1, 1)`. Kept identical to the `night_shift` block in `offscreen.frag` so the
/// hardware path and the shader fallback look the same - change one, change both.
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

/// Whether these multipliers leave every channel untouched.
fn is_neutral(multipliers: [f32; 3]) -> bool {
    multipliers == [1.0; 3]
}

/// Program `kelvin` into whatever colour hardware the CRTC has; 0 (or anything at or
/// above [`NEUTRAL_KELVIN`]) restores the untinted state.
///
/// `None` means the CRTC offers neither mechanism and the caller has to keep tinting
/// in the shader.
pub fn apply(
    device: &impl ControlDevice,
    crtc: crtc::Handle,
    kelvin: u16,
) -> Result<Option<Mechanism>> {
    let multipliers = multipliers(kelvin);

    if apply_gamma_lut(device, crtc, multipliers)? {
        return Ok(Some(Mechanism::GammaLut));
    }
    if apply_ctm(device, crtc, multipliers)? {
        return Ok(Some(Mechanism::Ctm));
    }

    Ok(None)
}

// --- GAMMA_LUT ---------------------------------------------------------------

/// Number of entries in the crtc's gamma LUT, `None` when the driver has none.
fn gamma_lut_size(device: &impl ControlDevice, crtc: crtc::Handle) -> Option<usize> {
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

fn apply_gamma_lut(
    device: &impl ControlDevice,
    crtc: crtc::Handle,
    multipliers: [f32; 3],
) -> Result<bool> {
    let Some(size) = gamma_lut_size(device, crtc) else {
        return Ok(false);
    };

    let [red, green, blue] = multipliers;
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

// --- CTM ---------------------------------------------------------------------

/// Encode a multiplier as DRM's S31.32 **sign-magnitude** fixed point (not two's
/// complement). Every multiplier here is in `[0, 1]`, so the sign bit stays clear.
fn s31_32(value: f32) -> u64 {
    (f64::from(value) * 4294967296.0) as u64
}

/// The handle of `crtc`'s property called `name`, if it has one.
fn find_property(
    device: &impl ControlDevice,
    crtc: crtc::Handle,
    name: &str,
) -> Result<Option<property::Handle>> {
    let properties = device
        .get_properties(crtc)
        .context("Failed to read crtc properties")?;

    for handle in properties.as_props_and_values().0 {
        let info = device
            .get_property(*handle)
            .context("Failed to read crtc property")?;
        if info.name().to_bytes() == name.as_bytes() {
            return Ok(Some(*handle));
        }
    }

    Ok(None)
}

fn apply_ctm(
    device: &impl ControlDevice,
    crtc: crtc::Handle,
    multipliers: [f32; 3],
) -> Result<bool> {
    let Some(property) = find_property(device, crtc, "CTM")? else {
        return Ok(false);
    };

    // Blob id 0 means "no matrix", which is cheaper and more honest than committing
    // an identity.
    let blob = if is_neutral(multipliers) {
        None
    } else {
        // Row-major 3x3; a per-channel scale only touches the diagonal.
        let mut matrix = [0u64; 9];
        matrix[0] = s31_32(multipliers[0]);
        matrix[4] = s31_32(multipliers[1]);
        matrix[8] = s31_32(multipliers[2]);

        match device
            .create_property_blob(&matrix)
            .context("Failed to create CTM blob")?
        {
            property::Value::Blob(id) => Some(id),
            other => anyhow::bail!("CTM blob came back as {other:?}"),
        }
    };

    let mut request = AtomicModeReq::new();
    request.add_raw_property(crtc.into(), property, blob.unwrap_or(0));

    // Blocking and without ALLOW_MODESET: the kernel stalls until an in-flight flip
    // from the surface thread lands, where a nonblocking commit would return EBUSY.
    // The request touches no plane state, so it cannot disturb that flip.
    let committed = device
        .atomic_commit(AtomicCommitFlags::empty(), request)
        .context("Failed to commit CTM");

    // The crtc state took its own reference, so ours is done either way.
    if let Some(id) = blob {
        let _ = device.destroy_property_blob(id);
    }

    committed?;
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
        assert!(is_neutral(multipliers(0)));
        assert!(!is_neutral(multipliers(3500)));
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

    #[test]
    fn ctm_fixed_point_puts_one_at_the_binary_point() {
        assert_eq!(s31_32(1.0), 1u64 << 32);
        assert_eq!(s31_32(0.5), 1u64 << 31);
        assert_eq!(s31_32(0.0), 0);
        // Sign-magnitude: the top bit stays clear for everything we produce.
        assert_eq!(s31_32(1.0) & (1u64 << 63), 0);
    }

    #[test]
    fn ctm_matches_the_gamma_ramp_at_the_same_temperature() {
        let [_, green, _] = multipliers(3500);
        let lut = ramp(green, 256);
        // Both encode the same scale: full-scale input maps to `green` of full output.
        let from_lut = f64::from(lut[255]) / f64::from(u16::MAX);
        let from_ctm = s31_32(green) as f64 / 4294967296.0;
        assert!(
            (from_lut - from_ctm).abs() < 1e-4,
            "lut {from_lut} vs ctm {from_ctm}"
        );
    }
}
