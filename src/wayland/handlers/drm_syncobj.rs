// SPDX-License-Identifier: GPL-3.0-only

use crate::state::{BackendData, State};
use smithay::{
    backend::drm::{DrmDeviceFd, NodeType}, delegate_drm_syncobj, wayland::drm_syncobj::{DrmSyncobjHandler, DrmSyncobjState},
};

impl DrmSyncobjHandler for State {
    fn drm_syncobj_state(&mut self) -> &mut DrmSyncobjState {
        let kms = match &mut self.backend {
            BackendData::Kms(kms) => kms,
            _ => unreachable!(),
        };
        kms.syncobj_state.as_mut().unwrap()
    }
}

delegate_drm_syncobj!(State);
