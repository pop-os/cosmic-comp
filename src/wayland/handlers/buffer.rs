// SPDX-License-Identifier: GPL-3.0-only

use crate::state::{BackendData, State};
use smithay::{
    reexports::wayland_server::{protocol::wl_buffer::WlBuffer, Resource},
    wayland::buffer::BufferHandler,
};
use tracing::warn;

impl BufferHandler for State {
    fn buffer_destroyed(&mut self, buffer: &WlBuffer) {
        if let BackendData::Kms(kms_state) = &mut self.backend {
            for device in kms_state.drm_devices.values_mut() {
                if device.inner.active_buffers.remove(&buffer.downgrade()) {
                    if !device
                        .inner
                        .in_use(kms_state.primary_node.read().unwrap().as_ref())
                    {
                        if let Err(err) = kms_state.refresh_used_devices() {
                            warn!(?err, "Failed to init devices.");
                        };
                        break;
                    }
                }
            }
        }
    }
}
