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
        let normalized = i as f32 / (gamma_length - 1) as f32 * 2.0;
        red_ramp[i] = (normalized * r * 65535.0).min(65535.0) as u16;
        green_ramp[i] = (normalized * g * 65535.0).min(65535.0) as u16;
        blue_ramp[i] = (normalized * b * 65535.0).min(65535.0) as u16;
    }

    // Apply the gamma ramp to the CRTC
    device
        .set_gamma(crtc, &red_ramp, &green_ramp, &blue_ramp)
        .context("Failed to set gamma for CRTC")?;

    Ok(())
}

/// Convert color temperature to RGB multipliers using Tanner Helland algorithm
fn kelvin_to_rgb_multipliers(temperature: u32) -> (f32, f32, f32) {
    let temp = (temperature as f32 / 100.0).clamp(10.0, 400.0);

    let red = if temp <= 66.0 {
        1.0
    } else {
        (329.698727446 * (temp - 60.0).powf(-0.1332047592) / 255.0).clamp(0.0, 1.0)
    };

    let green = if temp <= 66.0 {
        (99.4708025861 * temp.ln() - 161.1195681661) / 255.0
    } else {
        (288.1221695283 * (temp - 60.0).powf(-0.0755148492)) / 255.0
    }
    .clamp(0.0, 1.0);

    let blue = if temp >= 66.0 {
        1.0
    } else if temp <= 19.0 {
        0.0
    } else {
        (138.5177312231 * (temp - 10.0).ln() - 305.0447927307) / 255.0
    }
    .clamp(0.0, 1.0);

    (red, green, blue)
}
