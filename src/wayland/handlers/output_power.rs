// SPDX-License-Identifier: GPL-3.0-only

use smithay::output::Output;

use crate::{
    backend::kms::Surface,
    state::{BackendData, State},
    utils::prelude::OutputExt,
    wayland::protocols::output_power::{
        OutputPowerHandler, OutputPowerState, delegate_output_power,
    },
};

pub fn set_all_surfaces_dpms_on(state: &mut State) {
    let mut changed = false;
    for surface in kms_surfaces(state) {
        if !surface.get_dpms() {
            surface.set_dpms(true);
            changed = true;
        }
    }

    if changed {
        OutputPowerState::refresh(state);
    }
}

fn kms_surfaces(state: &mut State) -> impl Iterator<Item = &mut Surface> {
    if let BackendData::Kms(kms_state) = &mut state.backend {
        Some(
            kms_state
                .drm_devices
                .values_mut()
                .flat_map(|device| device.inner.surfaces.values_mut()),
        )
    } else {
        None
    }
    .into_iter()
    .flatten()
}

// Get KMS `Surface` for output, and for all outputs mirroring it
fn kms_surfaces_for_output<'a>(
    state: &'a mut State,
    output: &'a Output,
) -> impl Iterator<Item = &'a mut Surface> + 'a {
    kms_surfaces(state).filter(move |surface| {
        surface.output == *output || surface.output.mirroring().as_ref() == Some(output)
    })
}

// Get KMS `Surface` for output
fn primary_kms_surface_for_output<'a>(
    state: &'a mut State,
    output: &Output,
) -> Option<&'a mut Surface> {
    kms_surfaces(state).find(|surface| surface.output == *output)
}

impl OutputPowerHandler for State {
    fn output_power_state(&mut self) -> &mut OutputPowerState {
        &mut self.common.output_power_state
    }

    fn get_dpms(&mut self, output: &Output) -> Option<bool> {
        let surface = primary_kms_surface_for_output(self, output)?;
        Some(surface.get_dpms())
    }

    fn set_dpms(&mut self, output: &Output, on: bool) {
        for surface in kms_surfaces_for_output(self, output) {
            surface.set_dpms(on);
        }
    }
}

delegate_output_power!(State);
