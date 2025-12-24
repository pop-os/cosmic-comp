// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    state::State,
    wayland::protocols::drm::{DrmHandler, ImportError, delegate_wl_drm},
};
use smithay::{
    backend::{allocator::dmabuf::Dmabuf, drm::DrmNode},
    reexports::wayland_server::protocol::wl_buffer::WlBuffer,
    wayland::dmabuf::DmabufGlobal,
};

impl DrmHandler<Option<DrmNode>> for State {
    fn dmabuf_imported(
        &mut self,
        global: &DmabufGlobal,
        dmabuf: Dmabuf,
    ) -> Result<Option<DrmNode>, ImportError> {
        self.backend
            .dmabuf_imported(None, global, dmabuf)
            .map_err(|_| ImportError::Failed)
    }

    fn buffer_created(&mut self, _buffer: WlBuffer, _result: Option<DrmNode>) {}
}

delegate_wl_drm!(State; Option<DrmNode>);
