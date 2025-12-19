// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    backend::allocator::dmabuf::Dmabuf,
    delegate_dmabuf,
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
        let client = import_notifier.client();
        match self.backend.dmabuf_imported(client.clone(), global, dmabuf) {
            Err(err) => {
                tracing::debug!(?err, "dmabuf import failed");
                import_notifier.failed()
            }
            Ok(_) => {
                let _ = import_notifier.successful::<State>();
            }
        }
    }
}

delegate_dmabuf!(State);
