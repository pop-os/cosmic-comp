// SPDX-License-Identifier: GPL-3.0-only

use anyhow::{anyhow, Context, Result};
use smithay::{
    backend::drm::DrmDevice,
    reexports::drm::{
        self,
        control::{
            atomic::AtomicModeReq, crtc, property, AtomicCommitFlags, Device as ControlDevice,
        },
    },
};
use tracing::info;

/// DRM color LUT entry (matches kernel struct drm_color_lut)
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct ColorLut {
    pub red: u16,
    pub green: u16,
    pub blue: u16,
    pub reserved: u16,
}

/// Query gamma capabilities for a CRTC
fn get_lut_size(device: &impl ControlDevice, crtc: crtc::Handle) -> Result<usize> {
    let lut_size =
        crate::backend::kms::drm_helpers::get_property_val(device, crtc, "GAMMA_LUT_SIZE")?;

    let size = match lut_size.0.convert_value(lut_size.1) {
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
    // let lut_size = get_lut_size(device, crtc).context("No gamma capability for CRTC")?;
    // info!("Applying gamma for temperature {}K with LUT size {}", temperature, lut_size);

    let multipliers = kelvin_to_rgb_multipliers(temperature);
    info!("RGB multipliers: {:?}", multipliers);
    // let lut = generate_gamma_lut(lut_size, multipliers);
    // info!("Generated gamma LUT: {:?}", lut);

    // // Create blob property for LUT
    // let blob_data = unsafe {
    //     std::slice::from_raw_parts(
    //         lut.as_ptr() as *const u8,
    //         lut.len() * std::mem::size_of::<ColorLut>(),
    //     )
    // };
    // let blob = device.get_gamma(crtc, red, green, blue);
    // info!("Created gamma LUT blob with size {}", blob_data.len());

    // // Apply gamma LUT
    // let gamma_lut_prop = crate::backend::kms::drm_helpers::get_prop(device, crtc, "GAMMA_LUT")?;
    // info!("Found GAMMA_LUT property: {:?}", gamma_lut_prop);

    // let mut req = AtomicModeReq::new();
    // req.add_property(crtc, gamma_lut_prop, blob);

    // device.atomic_commit(AtomicCommitFlags::ALLOW_MODESET, req)?;

    let gamma_length =
        get_lut_size(device, crtc).context("Failed to get gamma LUT size for CRTC")?;
    info!("Gamma length for CRTC {:?}: {}", crtc, gamma_length);

    if gamma_length == 0 {
        return Err(anyhow!("CRTC {:?} does not support gamma correction", crtc));
    }

    // space for the gamma ramp
    let mut red = vec![0; gamma_length];
    let mut green = vec![0; gamma_length];
    let mut blue = vec![0; gamma_length];

    // Fill the gamma ramp with the RGB multipliers
    for i in 0..gamma_length {
        let normalized = i as f32 / (gamma_length - 1) as f32;
        red[i] = (normalized * multipliers[0] * 65535.0) as u16;
        green[i] = (normalized * multipliers[1] * 65535.0) as u16;
        blue[i] = (normalized * multipliers[2] * 65535.0) as u16;
    }

    // Apply the gamma ramp to the CRTC
    device.set_gamma(crtc, &red, &green, &blue)?;

    Ok(())
}

/// Convert color temperature to RGB multipliers using Tanner Helland algorithm
fn kelvin_to_rgb_multipliers(temperature: u32) -> [f32; 3] {
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

    [red, green, blue]
}

/// Generate gamma LUT from RGB multipliers
fn generate_gamma_lut(size: usize, multipliers: [f32; 3]) -> Vec<ColorLut> {
    (0..size)
        .map(|i| {
            let normalized = i as f32 / (size - 1) as f32;
            ColorLut {
                red: (normalized * multipliers[0] * 65535.0) as u16,
                green: (normalized * multipliers[1] * 65535.0) as u16,
                blue: (normalized * multipliers[2] * 65535.0) as u16,
                reserved: 0,
            }
        })
        .collect()
}
