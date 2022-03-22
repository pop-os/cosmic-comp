// SPDX-License-Identifier: GPL-3.0-only

use anyhow::{Context, Result};
use smithay::{
    backend::{
        allocator::Format,
        drm::DrmNode,
        renderer::{gles2::Gles2Renderbuffer, ImportDma},
    },
    reexports::{
        calloop::{generic::Generic, Interest, Mode, PostAction, RegistrationToken},
        wayland_protocols::unstable::linux_dmabuf,
        wayland_server::Client,
    },
    wayland::dmabuf::init_dmabuf_global_with_filter,
};

use std::{
    env,
    os::unix::{
        io::{AsRawFd, IntoRawFd, RawFd},
        net::UnixListener,
    },
    path::PathBuf,
};

use crate::{state::State, utils::GlobalDrop, wayland::init_wl_drm_global};

pub struct Socket {
    pub token: RegistrationToken,
    pub drm_global: GlobalDrop<crate::wayland::wl_drm::WlDrm>,
    pub dmabuf_global: GlobalDrop<linux_dmabuf::v1::server::zwp_linux_dmabuf_v1::ZwpLinuxDmabufV1>,
}

impl State {
    pub(super) fn create_socket(
        &mut self,
        render_node: DrmNode,
        formats: impl Iterator<Item = Format>,
    ) -> Result<Socket> {
        let formats = formats.collect::<Vec<_>>();
        let is_primary = self.backend.kms().primary == render_node;
        let socket_path = PathBuf::from(env::var("XDG_RUNTIME_DIR").unwrap()).join(format!(
            "{}-{}",
            &self.common.socket.to_string_lossy(),
            render_node
                .dev_path()
                .unwrap()
                .file_name()
                .unwrap()
                .to_string_lossy()
        ));
        // HACK!
        let _ = std::fs::remove_file(&socket_path);

        let listener = UnixListener::bind(socket_path.clone())
            .with_context(|| format!("Failed to bind socket to {}", socket_path.display()))?;
        listener.set_nonblocking(true)?;
        let listener = WaylandListener(listener);
        let token = self
            .common
            .event_loop_handle
            .insert_source(
                Generic::new(listener, Interest::READ, Mode::Edge),
                move |_, listener, state: &mut State| {
                    loop {
                        match listener.0.accept() {
                            Ok((stream, _)) => {
                                let display = state.common.display.clone();
                                let client = unsafe {
                                    display
                                        .borrow_mut()
                                        .create_client(stream.into_raw_fd(), state)
                                };
                                client
                                    .data_map()
                                    .insert_if_missing_threadsafe(|| render_node);
                            }
                            Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                                // we have exhausted all the pending connections
                                break;
                            }
                            Err(e) => {
                                // this is a legitimate error
                                if let Ok(addr) = listener.0.local_addr() {
                                    if let Some(path) = addr.as_pathname() {
                                        slog_scope::error!(
                                    "Error accepting connection on listening socket {} : {}",
                                    path.display(),
                                    e
                                );
                                        return Err(e);
                                    }
                                }
                                slog_scope::error!(
                                    "Error accepting connection on listening socket <unnamed> : {}",
                                    e
                                );
                                return Err(e);
                            }
                        }
                    }

                    Ok(PostAction::Continue)
                },
            )
            .context("Failed to add gpu-wayland socket to the event loop")?;

        // initialize globals
        let filter = move |client: Client| {
            let dev_id = client.data_map().get::<DrmNode>();
            if dev_id.is_none() && is_primary {
                client
                    .data_map()
                    .insert_if_missing_threadsafe(|| render_node);
            }
            dev_id.map(|x| *x == render_node).unwrap_or(is_primary)
        };

        let drm_global = init_wl_drm_global(
            &mut *self.common.display.borrow_mut(),
            render_node.dev_path().unwrap(),
            formats.clone(),
            filter.clone(),
        );
        let dmabuf_global = init_dmabuf_global_with_filter(
            &mut *self.common.display.borrow_mut(),
            formats,
            move |buf, mut ddata| {
                let state = ddata.get::<State>().unwrap();
                state
                    .backend
                    .kms()
                    .api
                    .renderer::<Gles2Renderbuffer>(&render_node, &render_node)
                    .map(|mut renderer| renderer.import_dmabuf(buf, None).is_ok())
                    .unwrap_or(false)
            },
            filter,
            None,
        );

        slog_scope::info!(
            "Adding socket at {} for gpu {}",
            socket_path.display(),
            render_node
        );

        Ok(Socket {
            token,
            drm_global: GlobalDrop::from(drm_global),
            dmabuf_global: GlobalDrop::from(dmabuf_global),
        })
    }
}

struct WaylandListener(UnixListener);

impl AsRawFd for WaylandListener {
    fn as_raw_fd(&self) -> RawFd {
        self.0.as_raw_fd()
    }
}

impl Drop for WaylandListener {
    fn drop(&mut self) {
        if let Ok(socketaddr) = self.0.local_addr() {
            if let Some(path) = socketaddr.as_pathname() {
                let _ = ::std::fs::remove_file(path);
            }
        }
    }
}
