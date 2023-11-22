// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    state::State,
    wayland::protocols::drm::{DrmHandler, ImportError},
};
use smithay::{backend::allocator::dmabuf::Dmabuf, wayland::dmabuf::DmabufGlobal};

impl DrmHandler for State {
    fn dmabuf_imported(
        &mut self,
        global: &DmabufGlobal,
        dmabuf: Dmabuf,
    ) -> Result<(), ImportError> {
        self.backend
            .dmabuf_imported(global, dmabuf)
            .map_err(|_| ImportError::Failed)
    }
}
