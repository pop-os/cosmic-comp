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
};
use std::sync::Arc;

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
            let dev_id = client.get_data::<ClientState>().unwrap().drm_node.unwrap();
            dev_id == render_node
        };

        let dmabuf_global = self
            .common
            .dmabuf_state
            .create_global_with_filter::<State, _, _>(dh, formats.clone(), filter, None);

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
        let listener = ListeningSocketSource::with_name(&socket_name, None)
            .with_context(|| format!("Failed to bind socket to {}", socket_name))?;
        let token = self
            .common
            .event_loop_handle
            .insert_source(listener, move |client_stream, _, data: &mut Data| {
                if let Err(err) = data.display.handle().insert_client(
                    client_stream,
                    Arc::new(data.state.new_client_state_with_node(render_node)),
                ) {
                    slog_scope::warn!("Error adding wayland client ({}): {}", render_node, err);
                }
            })
            .context("Failed to add gpu-wayland socket to the event loop")?;

        slog_scope::info!("Added socket at {} for gpu {}", socket_name, render_node,);

        Ok(Socket {
            token,
            drm_global: drm_global_id,
            dmabuf_global,
        })
    }
}
