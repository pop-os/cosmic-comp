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
            .leasing_global
            .as_mut()
            .unwrap()
    }

    fn lease_request(
        &mut self,
        node: DrmNode,
        request: DrmLeaseRequest,
    ) -> Result<DrmLeaseBuilder, LeaseRejected> {
        let backend = self
            .backend
            .kms()
            .drm_devices
            .get_mut(&node)
            .ok_or(LeaseRejected::default())?;

        let mut builder = DrmLeaseBuilder::new(&backend.drm);
        for conn in request.connectors {
            if let Some((_, crtc)) = backend
                .leased_connectors
                .iter()
                .find(|(handle, _)| *handle == conn)
            {
                builder.add_connector(conn);
                builder.add_crtc(*crtc);
                let planes = backend
                    .drm
                    .planes(crtc)
                    .map_err(LeaseRejected::with_cause)?;
                builder.add_plane(planes.primary.handle);
                if let Some(cursor) = planes.cursor {
                    builder.add_plane(cursor.handle);
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
            backend.active_leases.push(lease);
        }
        // else the backend is gone, drop the lease
    }

    fn lease_destroyed(&mut self, node: DrmNode, lease: u32) {
        if let Some(backend) = self.backend.kms().drm_devices.get_mut(&node) {
            backend.active_leases.retain(|l| l.id() != lease);
        }
    }
}

delegate_drm_lease!(State);
