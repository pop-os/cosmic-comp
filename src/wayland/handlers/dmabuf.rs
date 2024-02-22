// SPDX-License-Identifier: GPL-3.0-only

use crate::state::{BackendData, State};
use smithay::{
    backend::allocator::dmabuf::Dmabuf,
    delegate_dmabuf,
    reexports::wayland_server::Resource,
    wayland::dmabuf::{DmabufGlobal, DmabufHandler, DmabufState, ImportNotifier},
};

impl DmabufHandler for State {
    fn dmabuf_state(&mut self) -> &mut DmabufState {
        &mut self.common.dmabuf_state
    }

    fn dmabuf_imported(
        &mut self,
        global: &DmabufGlobal,
        dmabuf: Dmabuf,
        import_notifier: ImportNotifier,
    ) {
        match self
            .backend
            .dmabuf_imported(import_notifier.client(), global, dmabuf)
        {
            Err(err) => {
                tracing::debug!(?err, "dmabuf import failed");
                import_notifier.failed()
            }
            Ok(Some(node)) => {
                // kms backend
                let Ok(buffer) = import_notifier.successful::<State>() else {
                    return;
                };

                if let BackendData::Kms(kms_state) = &mut self.backend {
                    if let Some(device) = kms_state
                        .devices
                        .values_mut()
                        .find(|dev| dev.render_node == node)
                    {
                        device.active_buffers.insert(buffer.downgrade());
                    }
                }
            }
            Ok(None) => {
                let _ = import_notifier.successful::<State>();
            }
        }
    }
}

delegate_dmabuf!(State);
