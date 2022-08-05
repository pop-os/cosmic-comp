// SPDX-License-Identifier: GPL-3.0-only

use std::{
    fs::File,
    io::{Seek, SeekFrom},
    os::unix::io::{FromRawFd, IntoRawFd},
    time::Instant,
};
use smithay::{
    backend::{
        allocator::{
            Buffer,
            dmabuf::Dmabuf,
        },
        drm::DrmNode,
    },
    desktop::Window,
    reexports::{
        wayland_server::{
            self,
            Client,
            Dispatch,
            GlobalDispatch,
            DisplayHandle,
            backend::GlobalId,
            protocol::wl_output::WlOutput,
        },
    },
};

use cosmic_protocols::{
    export_dmabuf::v1::server::{
        zcosmic_export_dmabuf_manager_v1::{self, ZcosmicExportDmabufManagerV1},
        zcosmic_export_dmabuf_frame_v1::{self, CancelReason, Flags, ZcosmicExportDmabufFrameV1},
    },
};

use crate::wayland::protocols::{
    toplevel_info::{ToplevelInfoHandler, window_from_handle},
    workspace::{WorkspaceHandle, WorkspaceHandler},
};


/// Export Dmabuf global state
#[derive(Debug)]
pub struct ExportDmabufState {
    global: GlobalId,
}

pub struct ExportDmabufGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

impl ExportDmabufState {
    /// Create a new dmabuf global
    pub fn new<D, F>(display: &DisplayHandle, client_filter: F) -> ExportDmabufState
    where
        D: GlobalDispatch<ZcosmicExportDmabufManagerV1, ExportDmabufGlobalData>
            + Dispatch<ZcosmicExportDmabufManagerV1, ()>
            + Dispatch<ZcosmicExportDmabufFrameV1, ()>
            + ExportDmabufHandler
            + 'static,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        ExportDmabufState {
            global: display.create_global::<D, ZcosmicExportDmabufManagerV1, _>(1, ExportDmabufGlobalData {
                filter: Box::new(client_filter),
            }),
        }
    }

    /// Returns the export dmabuf global.
    pub fn global(&self) -> GlobalId {
        self.global.clone()
    }
}

pub enum CaptureError {
    Temporary(Box<dyn std::error::Error>),
    Permanent(Box<dyn std::error::Error>),
    Resizing,
}

pub struct Capture {
    pub device: DrmNode,
    pub dmabuf: Dmabuf,
    pub presentation_time: Instant,
}

pub trait ExportDmabufHandler {
    fn capture_output(&mut self, dh: &DisplayHandle, output: WlOutput, overlay_cursor: bool) -> Result<Capture, CaptureError>;
    fn capture_workspace(&mut self, dh: &DisplayHandle, workspace: WorkspaceHandle, output: WlOutput, overlay_cursor: bool) -> Result<Capture, CaptureError>;
    fn capture_toplevel(&mut self, dh: &DisplayHandle, toplevel: Window, overlay_cursor: bool) -> Result<Capture, CaptureError>;
    fn start_time(&mut self) -> Instant;
}

impl<D> GlobalDispatch<ZcosmicExportDmabufManagerV1, ExportDmabufGlobalData, D> for ExportDmabufState
where
    D: GlobalDispatch<ZcosmicExportDmabufManagerV1, ExportDmabufGlobalData>
     + Dispatch<ZcosmicExportDmabufManagerV1, ()>
     + Dispatch<ZcosmicExportDmabufFrameV1, ()>
     + ExportDmabufHandler,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: wayland_server::New<ZcosmicExportDmabufManagerV1>,
        _global_data: &ExportDmabufGlobalData,
        data_init: &mut wayland_server::DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }

    fn can_view(client: Client, global_data: &ExportDmabufGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ZcosmicExportDmabufManagerV1, (), D> for ExportDmabufState
where
    D: GlobalDispatch<ZcosmicExportDmabufManagerV1, ExportDmabufGlobalData>
     + Dispatch<ZcosmicExportDmabufManagerV1, ()>
     + Dispatch<ZcosmicExportDmabufFrameV1, ()>
     + ExportDmabufHandler
     + WorkspaceHandler
     + ToplevelInfoHandler
{
    fn request(
        state: &mut D,
        _client: &wayland_server::Client,
        _resource: &ZcosmicExportDmabufManagerV1,
        request: <ZcosmicExportDmabufManagerV1 as wayland_server::Resource>::Request,
        _data: &(),
        dhandle: &DisplayHandle,
        data_init: &mut wayland_server::DataInit<'_, D>,
    ) {
        let start_time = state.start_time();
        match request {
            zcosmic_export_dmabuf_manager_v1::Request::CaptureOutput {
                frame,
                overlay_cursor,
                output,
            } => {
                let frame = data_init.init(frame, ());
                match state.capture_output(dhandle, output, overlay_cursor != 0) {
                    Ok(capture) => handle_capture(capture, frame, start_time),
                    Err(err) => frame.cancel(err.into()),
                }
            },
            zcosmic_export_dmabuf_manager_v1::Request::CaptureWorkspace {
                frame,
                overlay_cursor,
                workspace,
                output,
            } => {
                let frame = data_init.init(frame, ());
                match state.workspace_state().workspace_handle(&workspace) {
                    Some(workspace) => {
                        match state.capture_workspace(dhandle, workspace, output, overlay_cursor != 0) {
                            Ok(capture) => handle_capture(capture, frame, start_time),
                            Err(err) => frame.cancel(err.into()),
                        }
                    },
                    None => frame.cancel(CancelReason::Permanent),
                }
            },
            zcosmic_export_dmabuf_manager_v1::Request::CaptureToplevel {
                frame,
                overlay_cursor,
                toplevel,
            } => {
                let frame = data_init.init(frame, ());
                match window_from_handle(toplevel) {
                    Some(window) => {
                        match state.capture_toplevel(dhandle, window, overlay_cursor != 0) {
                            Ok(capture) => handle_capture(capture, frame, start_time),
                            Err(err) => frame.cancel(err.into()),
                        }
                    },
                    None => frame.cancel(CancelReason::Permanent),
                }
            },
            zcosmic_export_dmabuf_manager_v1::Request::Destroy => {},
            _ => {},
        }
    }
}

impl From<CaptureError> for CancelReason {
    fn from(err: CaptureError) -> Self {
       match err {
            CaptureError::Temporary(err) => {
                slog_scope::debug!("Temporary Capture Error: {}", err);
                CancelReason::Temporary
            },
            CaptureError::Permanent(err) => {
                slog_scope::warn!("Permanent Capture Error: {}", err);
                CancelReason::Permanent
            },
            CaptureError::Resizing => {
                CancelReason::Resizing
            }
        } 
    }
}

fn handle_capture(capture: Capture, frame: ZcosmicExportDmabufFrameV1, start_time: Instant) {
    let Capture { device, dmabuf, presentation_time } = capture;
    let format = dmabuf.format();
    let modifier: u64 = format.modifier.into();

    frame.device(Vec::from(device.dev_id().to_ne_bytes()));
    frame.frame(
        dmabuf.width(),
        dmabuf.height(),
        0,
        0,
        if dmabuf.y_inverted() { 1 } else { 0 },
        Flags::Transient,
        format.code as u32,
        (modifier >> 32) as u32,
        (modifier & 0xFFFFFFFF) as u32,
        dmabuf.num_planes() as u32,
    );

    for (i, (handle, (offset, stride))) in dmabuf.handles().zip(dmabuf.offsets().zip(dmabuf.strides())).enumerate() {
        let mut file = unsafe { File::from_raw_fd(handle) };
        let size = match file.seek(SeekFrom::End(0)) {
            Ok(size) => size,
            Err(err) => {
                slog_scope::debug!("Temporary Capture Error: {}", err);
                frame.cancel(zcosmic_export_dmabuf_frame_v1::CancelReason::Temporary);
                return;
            }
        };
        if let Err(err) = file.rewind() {
            slog_scope::debug!("Temporary Capture Error: {}", err);
            frame.cancel(zcosmic_export_dmabuf_frame_v1::CancelReason::Temporary);
            return;
        }
        let handle = file.into_raw_fd();
        frame.object(
            i as u32,
            handle,
            size as u32,
            offset,
            stride,
            i as u32,
        );
    }
   
    let duration = presentation_time.duration_since(start_time);
    let (tv_sec, tv_nsec) = (duration.as_secs(), duration.subsec_nanos());
    frame.ready(
        (tv_sec >> 32) as u32,
        (tv_sec & 0xFFFFFFFF) as u32,
        tv_nsec,
    );
}

impl<D> Dispatch<ZcosmicExportDmabufFrameV1, (), D> for ExportDmabufState
where
    D: GlobalDispatch<ZcosmicExportDmabufManagerV1, ExportDmabufGlobalData>
     + Dispatch<ZcosmicExportDmabufManagerV1, ()>
     + Dispatch<ZcosmicExportDmabufFrameV1, ()>
     + ExportDmabufHandler,
{
    fn request(
        _state: &mut D,
        _client: &wayland_server::Client,
        _resource: &ZcosmicExportDmabufFrameV1,
        request: <ZcosmicExportDmabufFrameV1 as wayland_server::Resource>::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        _data_init: &mut wayland_server::DataInit<'_, D>,
    ) {
        match request {
            zcosmic_export_dmabuf_frame_v1::Request::Destroy => {},
            _ => {},
        }
    }
}

macro_rules! delegate_export_dmabuf {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::export_dmabuf::v1::server::zcosmic_export_dmabuf_manager_v1::ZcosmicExportDmabufManagerV1: $crate::wayland::protocols::export_dmabuf::ExportDmabufGlobalData
        ] => $crate::wayland::protocols::export_dmabuf::ExportDmabufState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::export_dmabuf::v1::server::zcosmic_export_dmabuf_manager_v1::ZcosmicExportDmabufManagerV1: ()
        ] => $crate::wayland::protocols::export_dmabuf::ExportDmabufState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::export_dmabuf::v1::server::zcosmic_export_dmabuf_frame_v1::ZcosmicExportDmabufFrameV1: ()
        ] => $crate::wayland::protocols::export_dmabuf::ExportDmabufState);
    };
}
pub(crate) use delegate_export_dmabuf;
