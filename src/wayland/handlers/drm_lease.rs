// SPDX-License-Identifier: GPL-3.0-only

use std::{sync::mpsc::sync_channel, time::Duration};

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
                let (primary_plane, primary_plane_claim) = planes
                    .primary
                    .iter()
                    .find_map(|plane| {
                        backend
                            .drm
                            .claim_plane(plane.handle, *crtc)
                            .map(|claim| (plane, claim))
                    })
                    .ok_or_else(LeaseRejected::default)?;
                builder.add_plane(primary_plane.handle, primary_plane_claim);
                if let Some((cursor, claim)) = planes.cursor.into_iter().find_map(|plane| {
                    backend
                        .drm
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
            if backend.active_leases.is_empty() {
                let mut receivers = Vec::new();
                for surface in backend.surfaces.values_mut() {
                    let (tx, rx) = sync_channel(1);
                    surface.set_direct_scanout(false, tx);
                    receivers.push(rx);
                }
                for rx in receivers {
                    let _ = rx.recv_timeout(Duration::from_millis(100));
                }
            }
            backend.active_leases.push(lease);
        }
        // else the backend is gone, drop the lease
    }

    fn lease_destroyed(&mut self, node: DrmNode, lease: u32) {
        if let Some(backend) = self.backend.kms().drm_devices.get_mut(&node) {
            backend.active_leases.retain(|l| l.id() != lease);
            if backend.active_leases.is_empty() {
                for surface in backend.surfaces.values_mut() {
                    surface.set_direct_scanout(true, sync_channel(0).0);
                }
            }
        }
    }
}

delegate_drm_lease!(State);
