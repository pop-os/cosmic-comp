// SPDX-License-Identifier: GPL-3.0-only

use smithay::{output::Output, reexports::wayland_server::protocol::wl_output::WlOutput};

use crate::{
    backend::kms::drm_helpers,
    state::{BackendData, State},
    wayland::protocols::output_power::{delegate_output_power, OutputPowerHandler},
};

impl OutputPowerHandler for State {
    fn set_dpms(&mut self, wl_output: &WlOutput, on: bool) {
        if let Some(output) = Output::from_resource(wl_output) {
            if let BackendData::Kms(ref mut kms_state) = &mut self.backend {
                for (_, device) in &mut kms_state.drm_devices {
                    for (_, surface) in &mut device.surfaces {
                        if surface.output == output {
                            drm_helpers::set_dpms(&device.drm, surface.connector, on);
                        }
                    }
                }
            }
        }
    }
}

delegate_output_power!(State);
