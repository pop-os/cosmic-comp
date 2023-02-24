// SPDX-License-Identifier: GPL-3.0-only

use anyhow::{anyhow, Context, Result};
use smithay::{
    backend::{
        allocator::Format,
        drm::{DrmNode, NodeType},
    },
    reexports::{
        calloop::RegistrationToken,
        wayland_server::{backend::GlobalId, Client, DisplayHandle},
    },
    wayland::{dmabuf::DmabufGlobal, socket::ListeningSocketSource},
    xwayland::XWaylandClientData,
};
use std::sync::Arc;
use tracing::{info, warn};

use crate::{
    state::{ClientState, Data},
    utils::prelude::*,
};

pub struct Socket {
    pub token: RegistrationToken,
    pub drm_global: GlobalId,
    pub dmabuf_global: DmabufGlobal,
}

impl State {
    pub(super) fn create_socket(
        &mut self,
        dh: &DisplayHandle,
        render_node: DrmNode,
        formats: impl Iterator<Item = Format>,
    ) -> Result<Socket> {
        let formats = formats.collect::<Vec<_>>();
        let socket_name = format!(
            "{}-{}",
            &self.common.socket.to_string_lossy(),
            render_node
                .dev_path()
                .unwrap()
                .file_name()
                .unwrap()
                .to_string_lossy()
        );

        // initialize globals
        let filter = move |client: &Client| {
            if let Some(normal_client) = client.get_data::<ClientState>() {
                let dev_id = normal_client.drm_node.unwrap();
                return dev_id == render_node;
            }
            if let Some(xwayland_client) = client.get_data::<XWaylandClientData>() {
                let dev_id = xwayland_client.user_data().get::<DrmNode>().unwrap();
                return *dev_id == render_node;
            }
            false
        };

        let dmabuf_global = self
            .common
            .dmabuf_state
            .create_global_with_filter::<State, _>(dh, formats.clone(), filter);

        let drm_global_id = self
            .common
            .wl_drm_state
            .create_global_with_filter::<State, _>(
                dh,
                render_node
                    .dev_path_with_type(NodeType::Render)
                    .or_else(|| render_node.dev_path())
                    .ok_or(anyhow!(
                        "Could not determine path for gpu node: {}",
                        render_node
                    ))?,
                formats,
                &dmabuf_global,
                filter,
            );

        // add a special socket for the gpu
        let listener = ListeningSocketSource::with_name(&socket_name)
            .with_context(|| format!("Failed to bind socket to {}", socket_name))?;
        let socket_name_clone = socket_name.clone();
        let token = self
            .common
            .event_loop_handle
            .insert_source(listener, move |client_stream, _, data: &mut Data| {
                if let Err(err) = data.display.handle().insert_client(
                    client_stream,
                    Arc::new(data.state.new_client_state_with_node(render_node)),
                ) {
                    warn!(
                        socket_name = socket_name_clone,
                        ?err,
                        "Error adding wayland client."
                    );
                }
            })
            .context("Failed to add gpu-wayland socket to the event loop")?;

        info!(socket_name, ?render_node, "Added gpu-specific socket.");

        Ok(Socket {
            token,
            drm_global: drm_global_id,
            dmabuf_global,
        })
    }
}
