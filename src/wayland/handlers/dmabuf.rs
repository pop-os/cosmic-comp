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
        if self.backend.dmabuf_imported(global, dmabuf).is_err() {
            import_notifier.failed();
        }
    }
}

delegate_dmabuf!(State);
