// SPDX-License-Identifier: GPL-3.0-only

use smithay::output::Output;

use crate::{
    state::{BackendData, State},
    wayland::protocols::gamma_control::{
        GammaControlHandler, GammaControlManagerState, delegate_gamma_control,
    },
};

impl GammaControlHandler for State {
    fn gamma_control_manager_state(&mut self) -> &mut GammaControlManagerState {
        &mut self.common.gamma_control_manager_state
    }

    fn get_gamma_size(&mut self, output: &Output) -> Option<u32> {
        let BackendData::Kms(kms_state) = &mut self.backend else {
            return None;
        };
        kms_state.get_gamma_size(output)
    }

    fn set_gamma(&mut self, output: &Output, ramp: Option<Vec<u16>>) -> Option<()> {
        let BackendData::Kms(kms_state) = &mut self.backend else {
            return None;
        };
        kms_state.set_gamma(output, ramp)
    }
}

delegate_gamma_control!(State);
