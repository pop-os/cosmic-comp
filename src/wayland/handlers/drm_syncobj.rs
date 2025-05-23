// SPDX-License-Identifier: GPL-3.0-only

use crate::state::{BackendData, State};
use smithay::{
    delegate_drm_syncobj,
    wayland::drm_syncobj::{DrmSyncobjHandler, DrmSyncobjState},
};

impl DrmSyncobjHandler for State {
    fn drm_syncobj_state(&mut self) -> Option<&mut DrmSyncobjState> {
        let kms = match &mut self.backend {
            BackendData::Kms(kms) => kms,
            _ => unreachable!(),
        };
        kms.syncobj_state.as_mut()
    }
}

delegate_drm_syncobj!(State);
