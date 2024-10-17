// SPDX-License-Identifier: GPL-3.0-only

use smithay::{output::Output, reexports::wayland_server::protocol::wl_output::WlOutput};

use crate::{
    backend::kms::Surface,
    state::{BackendData, State},
    wayland::protocols::output_power::{
        delegate_output_power, OutputPowerHandler, OutputPowerState,
    },
};

fn kms_surface_for_wl_output<'a>(
    state: &'a mut State,
    wl_output: &WlOutput,
) -> Option<&'a mut Surface> {
    let output = Output::from_resource(wl_output)?;
    if let BackendData::Kms(ref mut kms_state) = &mut state.backend {
        kms_state
            .drm_devices
            .values_mut()
            .flat_map(|device| device.surfaces.values_mut())
            .find(|surface| surface.output == output)
    } else {
        None
    }
}

impl OutputPowerHandler for State {
    fn output_power_state(&mut self) -> &mut OutputPowerState {
        &mut self.common.output_power_state
    }

    fn get_dpms(&mut self, wl_output: &WlOutput) -> Option<bool> {
        let surface = kms_surface_for_wl_output(self, wl_output)?;
        Some(surface.get_dpms())
    }

    fn set_dpms(&mut self, wl_output: &WlOutput, on: bool) {
        if let Some(surface) = kms_surface_for_wl_output(self, wl_output) {
            surface.set_dpms(on);
        }
    }
}

delegate_output_power!(State);
