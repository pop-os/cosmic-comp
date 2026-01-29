// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    state::{BackendData, State},
    wayland::protocols::drm::{DrmHandler, ImportError, delegate_wl_drm},
};
use smithay::{
    backend::{allocator::dmabuf::Dmabuf, drm::DrmNode},
    reexports::wayland_server::{Resource, protocol::wl_buffer::WlBuffer},
    wayland::dmabuf::DmabufGlobal,
};
use tracing::warn;

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
                    .drm_devices
                    .values_mut()
                    .find(|device| device.inner.render_node == node)
                {
                    device.inner.active_buffers.insert(buffer.downgrade());
                }

                if let Err(err) = kms_state.refresh_used_devices() {
                    warn!(?err, "Failed to init devices.");
                };
            }
        }
    }
}

delegate_wl_drm!(State; Option<DrmNode>);
