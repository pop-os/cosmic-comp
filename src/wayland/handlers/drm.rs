// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    state::{BackendData, State},
    wayland::protocols::drm::{delegate_wl_drm, DrmHandler, ImportError},
};
use smithay::{
    backend::{allocator::dmabuf::Dmabuf, drm::DrmNode},
    reexports::wayland_server::{protocol::wl_buffer::WlBuffer, Resource},
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

    fn buffer_created(&mut self, buffer: WlBuffer, result: Option<DrmNode>) {
        if let Some(node) = result {
            // kms backend
            if let BackendData::Kms(kms_state) = &mut self.backend {
                if let Some(device) = kms_state
                    .devices
                    .values_mut()
                    .find(|device| device.render_node == node)
                {
                    device.active_buffers.insert(buffer.downgrade());
                }
            }
        }
    }
}

delegate_wl_drm!(State; Option<DrmNode>);
