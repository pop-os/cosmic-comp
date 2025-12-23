// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    state::{BackendData, State},
    wayland::handlers::compositor::frame_time_filter_fn,
};
use smithay::{
    backend::{allocator::dmabuf::Dmabuf, renderer::element::Kind},
    delegate_dmabuf,
    desktop::WindowSurfaceType,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    wayland::{
        compositor::with_states,
        dmabuf::{DmabufFeedback, DmabufGlobal, DmabufHandler, DmabufState, ImportNotifier},
    },
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

    fn new_surface_feedback(
        &mut self,
        surface: &WlSurface,
        global: &DmabufGlobal,
    ) -> Option<DmabufFeedback> {
        let BackendData::Kms(kms) = &self.backend else {
            return None;
        };
        let shell = self.common.shell.read();

        let (handle, output) = shell.workspace_for_surface(surface)?;
        let is_fullscreen = shell
            .workspaces
            .space_for_handle(&handle)?
            .fullscreen
            .as_ref()
            .is_some_and(|f| f.surface.has_surface(surface, WindowSurfaceType::all()));

        let node = kms
            .drm_devices
            .values()
            .find(|device| {
                device
                    .socket
                    .as_ref()
                    .map(|s| &s.dmabuf_global == global)
                    .unwrap_or(false)
            })?
            .inner
            .render_node;
        let kms_surface = kms
            .drm_devices
            .values()
            .find_map(|device| device.inner.surfaces.values().find(|s| s.output == output))?;
        let feedback = kms_surface.feedback.get(&node)?.clone();

        Some(with_states(surface, |data| {
            if is_fullscreen {
                feedback
                    .primary_scanout_feedback
                    .unwrap_or(feedback.render_feedback)
            } else {
                if frame_time_filter_fn(data) == Kind::ScanoutCandidate {
                    feedback
                        .overlay_scanout_feedback
                        .unwrap_or(feedback.render_feedback)
                } else {
                    feedback.render_feedback
                }
            }
        }))
    }
}

delegate_dmabuf!(State);
