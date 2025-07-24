// SPDX-License-Identifier: GPL-3.0-only

use anyhow::{anyhow, Context, Result};
use smithay::{
    backend::drm::DrmDevice,
    reexports::drm::control::{crtc, property, Device as ControlDevice},
};
use tracing::debug;

/// Query gamma capabilities for a CRTC
fn get_lut_size(device: &impl ControlDevice, crtc: crtc::Handle) -> Result<usize> {
    let (value_type, raw_value) =
        crate::backend::kms::drm_helpers::get_property_val(device, crtc, "GAMMA_LUT_SIZE")?;

    let size = match value_type.convert_value(raw_value) {
        property::Value::UnsignedRange(size) => size as usize,
        _ => return Err(anyhow!("Invalid GAMMA_LUT_SIZE property type")),
    };

    if size == 0 {
        Err(anyhow!("GAMMA_LUT_SIZE is zero, no gamma support"))
    } else {
        Ok(size)
    }
}

/// Apply color temperature to a CRTC
pub fn apply_gamma_for_temperature(
    device: &DrmDevice,
    crtc: crtc::Handle,
    temperature: u32,
) -> Result<()> {
    let lut_size = get_lut_size(device, crtc).context("No gamma capability for CRTC")?;
    debug!(
        "Applying gamma for temperature {}K with LUT size {}",
        temperature, lut_size
    );

    let (r, g, b) = kelvin_to_rgb_multipliers(temperature);

    let gamma_length =
        get_lut_size(device, crtc).context("Failed to get gamma LUT size for CRTC")?;
    debug!("Gamma length for CRTC {:?}: {}", crtc, gamma_length);

    if gamma_length == 0 {
        return Err(anyhow!("CRTC {:?} does not support gamma correction", crtc));
    }

    // space for the gamma ramp
    let mut red_ramp = vec![0; gamma_length];
    let mut green_ramp = vec![0; gamma_length];
    let mut blue_ramp = vec![0; gamma_length];

    // Fill the gamma ramp with the RGB multipliers
    for i in 0..gamma_length {
        let normalized = i as f32 / (gamma_length - 1) as f32;
        red_ramp[i] = (normalized * r * 65535.0) as u16;
        green_ramp[i] = (normalized * g * 65535.0) as u16;
        blue_ramp[i] = (normalized * b * 65535.0) as u16;
    }

    // Apply the gamma ramp to the CRTC
    device
        .set_gamma(crtc, &red_ramp, &green_ramp, &blue_ramp)
        .context("Failed to set gamma for CRTC")?;

    Ok(())
}

macro_rules! normalise {
    ($x:expr) => {{
        if $x < 0.0 {
            $x = 0.;
        } else if $x > 255.0 {
            $x = 255.0;
        }
    }};
}

/// Convert color temperature to RGB multipliers using Tanner Helland algorithm
/// Ripped from https://github.com/spacekookie/colortemp/blob/master/src/lib.rs
/// to avoid adding a dependency on `colortemp` crate
fn kelvin_to_rgb_multipliers(temperature: u32) -> (f32, f32, f32) {
    let (mut red, mut green, mut blue);
    let temp = temperature / 100;

    /* Calculate red */
    if temp <= 66 {
        red = 255.;
    } else {
        red = (temp as f32) - 60.;
        red = 329.698727446 * red.powf(-329.698727446);
        normalise!(red);
    }

    /* Calculate green */
    if temp <= 66 {
        green = temp as f32;
        green = 99.4708025861 * green.ln() - 161.1195681661;
        normalise!(green);
    } else {
        green = temp as f32 - 60.;
        green = 288.1221695283 * green.powf(-0.0755148492);
        normalise!(green);
    }

    /* Feeling bluueeee */
    if temp >= 66 {
        blue = 255.;
    } else {
        if temp <= 19 {
            blue = 0.;
        } else {
            blue = temp as f32 - 10.;
            blue = 138.5177312231 * blue.ln() - 305.0447927307;
            normalise!(blue);
        }
    }

    return (red, green, blue);
}
