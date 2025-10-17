// SPDX-License-Identifier: GPL-3.0-only

// Re-export only the actual code, and then only use this re-export
// The `generated` module below is just some boilerplate to properly isolate stuff
// and avoid exposing internal details.
//
// You can use all the types from my_protocol as if they went from `wayland_client::protocol`.
pub use generated::wl_drm;

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/wayland-drm.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/wayland-drm.xml");
}

use smithay::{
    backend::allocator::{
        Fourcc, Modifier,
        dmabuf::{Dmabuf, DmabufFlags},
        format::FormatSet,
    },
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource,
        backend::GlobalId, protocol::wl_buffer::WlBuffer,
    },
    wayland::{
        buffer::BufferHandler,
        dmabuf::{DmabufGlobal, DmabufHandler},
    },
};
use tracing::trace;

use std::{convert::TryFrom, path::PathBuf, sync::Arc};

pub enum ImportError {
    Failed,
    InvalidFormat,
}

pub trait DrmHandler<R: 'static> {
    fn dmabuf_imported(&mut self, global: &DmabufGlobal, dmabuf: Dmabuf) -> Result<R, ImportError>;
    fn buffer_created(&mut self, buffer: WlBuffer, result: R) {
        let _ = (buffer, result);
    }
}

#[derive(Debug, Default)]
pub struct WlDrmState<R>(std::marker::PhantomData<R>);

/// Data associated with a drm global.
pub struct DrmGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
    formats: Arc<Vec<Fourcc>>,
    device_path: PathBuf,
    dmabuf_global: DmabufGlobal,
}

pub struct DrmInstanceData {
    formats: Arc<Vec<Fourcc>>,
    dmabuf_global: DmabufGlobal,
}

impl<D, R> GlobalDispatch<wl_drm::WlDrm, DrmGlobalData, D> for WlDrmState<R>
where
    D: GlobalDispatch<wl_drm::WlDrm, DrmGlobalData>
        + Dispatch<wl_drm::WlDrm, DrmInstanceData>
        + BufferHandler
        + DmabufHandler
        + 'static,
    R: 'static,
{
    fn bind(
        _state: &mut D,
        _dh: &DisplayHandle,
        _client: &Client,
        resource: New<wl_drm::WlDrm>,
        global_data: &DrmGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        let data = DrmInstanceData {
            formats: global_data.formats.clone(),
            dmabuf_global: global_data.dmabuf_global,
        };
        let drm_instance = data_init.init(resource, data);

        drm_instance.device(global_data.device_path.to_string_lossy().into_owned());
        if drm_instance.version() >= 2 {
            drm_instance.capabilities(wl_drm::Capability::Prime as u32);
        }
        for format in global_data.formats.iter() {
            if let Ok(converted) = wl_drm::Format::try_from(*format as u32) {
                drm_instance.format(converted as u32);
            }
        }
    }

    fn can_view(client: Client, global_data: &DrmGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D, R> Dispatch<wl_drm::WlDrm, DrmInstanceData, D> for WlDrmState<R>
where
    D: GlobalDispatch<wl_drm::WlDrm, DrmGlobalData>
        + Dispatch<wl_drm::WlDrm, DrmInstanceData>
        + Dispatch<WlBuffer, Dmabuf>
        + BufferHandler
        + DrmHandler<R>
        + 'static,
    R: 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        drm: &wl_drm::WlDrm,
        request: wl_drm::Request,
        data: &DrmInstanceData,
        _dh: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            wl_drm::Request::Authenticate { .. } => drm.authenticated(),
            wl_drm::Request::CreateBuffer { .. } => drm.post_error(
                wl_drm::Error::InvalidName,
                String::from("Flink handles are unsupported, use PRIME"),
            ),
            wl_drm::Request::CreatePlanarBuffer { .. } => drm.post_error(
                wl_drm::Error::InvalidName,
                String::from("Flink handles are unsupported, use PRIME"),
            ),
            wl_drm::Request::CreatePrimeBuffer {
                id,
                name,
                width,
                height,
                format,
                offset0,
                stride0,
                ..
            } => {
                let format = match Fourcc::try_from(format) {
                    Ok(format) => {
                        if !data.formats.contains(&format) {
                            drm.post_error(
                                wl_drm::Error::InvalidFormat,
                                String::from("Format not advertised by wl_drm"),
                            );
                            return;
                        }
                        format
                    }
                    Err(_) => {
                        drm.post_error(
                            wl_drm::Error::InvalidFormat,
                            String::from("Format unknown / not advertised by wl_drm"),
                        );
                        return;
                    }
                };

                if width < 1 || height < 1 {
                    drm.post_error(
                        wl_drm::Error::InvalidFormat,
                        String::from("width or height not positive"),
                    );
                    return;
                }

                let mut dma = Dmabuf::builder(
                    (width, height),
                    format,
                    Modifier::Invalid,
                    DmabufFlags::empty(),
                );
                dma.add_plane(name, 0, offset0 as u32, stride0 as u32);
                match dma.build() {
                    Some(dmabuf) => {
                        match state.dmabuf_imported(&data.dmabuf_global, dmabuf.clone()) {
                            Ok(result) => {
                                // import was successful
                                let buffer = data_init.init(id, dmabuf);
                                trace!("Created a new validated dma wl_buffer via wl_drm.");
                                state.buffer_created(buffer, result);
                            }

                            Err(ImportError::InvalidFormat) => {
                                drm.post_error(
                                    wl_drm::Error::InvalidFormat,
                                    "format and plane combination are not valid",
                                );
                            }

                            Err(ImportError::Failed) => {
                                // Buffer import failed. The protocol documentation heavily implies killing the
                                // client is the right thing to do here.
                                drm.post_error(wl_drm::Error::InvalidName, "buffer import failed");
                            }
                        }
                    }
                    None => {
                        // Buffer import failed. The protocol documentation heavily implies killing the
                        // client is the right thing to do here.
                        drm.post_error(
                            wl_drm::Error::InvalidName,
                            "dmabuf global was destroyed on server",
                        );
                    }
                }
            }
        }
    }
}

impl<R: 'static> WlDrmState<R> {
    pub fn create_global<D>(
        &mut self,
        display: &DisplayHandle,
        device_path: PathBuf,
        formats: FormatSet,
        dmabuf_global: &DmabufGlobal,
    ) -> GlobalId
    where
        D: GlobalDispatch<wl_drm::WlDrm, DrmGlobalData>
            + Dispatch<wl_drm::WlDrm, DrmInstanceData>
            + BufferHandler
            + DmabufHandler
            + 'static,
    {
        self.create_global_with_filter::<D, _>(display, device_path, formats, dmabuf_global, |_| {
            true
        })
    }

    pub fn create_global_with_filter<D, F>(
        &mut self,
        display: &DisplayHandle,
        device_path: PathBuf,
        formats: FormatSet,
        dmabuf_global: &DmabufGlobal,
        client_filter: F,
    ) -> GlobalId
    where
        D: GlobalDispatch<wl_drm::WlDrm, DrmGlobalData>
            + Dispatch<wl_drm::WlDrm, DrmInstanceData>
            + BufferHandler
            + DmabufHandler
            + 'static,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        let formats = Arc::new(
            formats
                .into_iter()
                .filter(|f| f.modifier == Modifier::Invalid)
                .map(|f| f.code)
                .collect(),
        );
        let data = DrmGlobalData {
            filter: Box::new(client_filter),
            formats,
            device_path,
            dmabuf_global: *dmabuf_global,
        };

        display.create_global::<D, wl_drm::WlDrm, _>(2, data)
    }
}

macro_rules! delegate_wl_drm {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty; $r: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::drm::wl_drm::WlDrm: $crate::wayland::protocols::drm::DrmGlobalData
        ] => $crate::wayland::protocols::drm::WlDrmState<$r>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::drm::wl_drm::WlDrm: $crate::wayland::protocols::drm::DrmInstanceData
        ] => $crate::wayland::protocols::drm::WlDrmState<$r>);
    };
}
pub(crate) use delegate_wl_drm;
