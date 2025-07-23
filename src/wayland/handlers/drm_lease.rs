// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    backend::drm::DrmNode,
    delegate_drm_lease,
    wayland::drm_lease::{
        DrmLease, DrmLeaseBuilder, DrmLeaseHandler, DrmLeaseRequest, DrmLeaseState, LeaseRejected,
    },
};

impl DrmLeaseHandler for State {
    fn drm_lease_state(&mut self, node: DrmNode) -> &mut DrmLeaseState {
        self.backend
            .kms()
            .drm_devices
            .get_mut(&node)
            .unwrap()
            .inner
            .leasing_global
            .as_mut()
            .unwrap()
    }

    fn lease_request(
        &mut self,
        node: DrmNode,
        request: DrmLeaseRequest,
    ) -> Result<DrmLeaseBuilder, LeaseRejected> {
        let kms = self.backend.kms();
        let mut backend = kms
            .drm_devices
            .get_mut(&node)
            .ok_or(LeaseRejected::default())?
            .lock();
        let mut renderer = match kms.api.single_renderer(&backend.inner.render_node) {
            Ok(renderer) => renderer,
            Err(err) => {
                tracing::warn!(
                    ?err,
                    "Failed to create renderer to disable direct scanout, denying lease"
                );
                return Err(LeaseRejected::default());
            }
        };
        if let Err(err) = backend.allow_overlay_scanout(
            false,
            &mut renderer,
            &self.common.clock,
            &self.common.shell,
        ) {
            tracing::warn!(?err, "Failed to disable direct scanout");
            return Err(LeaseRejected::default());
        }

        let mut builder = DrmLeaseBuilder::new(backend.drm.device());
        for conn in request.connectors {
            if let Some((_, crtc)) = backend
                .inner
                .leased_connectors
                .iter()
                .find(|(handle, _)| *handle == conn)
            {
                builder.add_connector(conn);
                builder.add_crtc(*crtc);
                let planes = backend
                    .drm
                    .device()
                    .planes(crtc)
                    .map_err(LeaseRejected::with_cause)?;
                let (primary_plane, primary_plane_claim) = planes
                    .primary
                    .iter()
                    .find_map(|plane| {
                        backend
                            .drm
                            .device_mut()
                            .claim_plane(plane.handle, *crtc)
                            .map(|claim| (plane, claim))
                    })
                    .ok_or_else(LeaseRejected::default)?;
                builder.add_plane(primary_plane.handle, primary_plane_claim);
                if let Some((cursor, claim)) = planes.cursor.into_iter().find_map(|plane| {
                    backend
                        .drm
                        .device_mut()
                        .claim_plane(plane.handle, *crtc)
                        .map(|claim| (plane, claim))
                }) {
                    builder.add_plane(cursor.handle, claim);
                }
            } else {
                tracing::warn!(
                    ?conn,
                    "Lease requested for desktop connector, denying request"
                );
                return Err(LeaseRejected::default());
            }
        }

        Ok(builder)
    }

    fn new_active_lease(&mut self, node: DrmNode, lease: DrmLease) {
        if let Some(backend) = self.backend.kms().drm_devices.get_mut(&node) {
            backend.inner.active_leases.push(lease);
        }
        // else the backend is gone, drop the lease
    }

    fn lease_destroyed(&mut self, node: DrmNode, lease: u32) {
        let kms = self.backend.kms();
        if let Some(backend) = kms.drm_devices.get_mut(&node) {
            let mut backend = backend.lock();
            backend.inner.active_leases.retain(|l| l.id() != lease);

            if backend.inner.active_leases.is_empty() {
                let mut renderer = match kms.api.single_renderer(&backend.inner.render_node) {
                    Ok(renderer) => renderer,
                    Err(err) => {
                        tracing::warn!(?err, "Failed to create renderer to enable direct scanout.");
                        return;
                    }
                };
                if let Err(err) = backend.allow_overlay_scanout(
                    true,
                    &mut renderer,
                    &self.common.clock,
                    &self.common.shell,
                ) {
                    tracing::warn!(?err, "Failed to enable direct scanout");
                }
            }
        }
    }
}

delegate_drm_lease!(State);
