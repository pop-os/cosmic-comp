// SPDX-License-Identifier: GPL-3.0-only

use anyhow::{anyhow, Context, Result};

use std::{
    cell::RefCell,
    time::Instant,
};

use smithay::{
    backend::{
        drm::{DrmNode, NodeType},
        egl::EGLDevice,
        renderer::{
            Bind,
            Offscreen,
            ExportDma,
            ImportAll,
            Renderer,
            gles2::{Gles2Renderer, Gles2Renderbuffer, Gles2Error},
            utils::with_renderer_surface_state,
        },
    },
    desktop::{space::RenderElement, Kind, Window, draw_window, draw_window_popups},
    wayland::{
        compositor::{get_children, with_states, SurfaceAttributes},
        dmabuf::get_dmabuf,
        output::Output,
        seat::CursorImageStatus,
    },
    reexports::{
        wayland_server::{DisplayHandle, Resource, protocol::wl_output::WlOutput},
    },
    utils::{IsAlive, Size, Transform},
};

use crate::{
    backend::render::{render_output, render_workspace, cursor::draw_cursor, AsGles2Renderer, CustomElem},
    state::{BackendData, ClientState, Common},
    utils::prelude::*,
    wayland::protocols::{
        export_dmabuf::{
            delegate_export_dmabuf, ExportDmabufHandler, Capture, CaptureError,
        },
        workspace::WorkspaceHandle,
    },
};

impl ExportDmabufHandler for State {
    fn capture_output(&mut self, _dh: &DisplayHandle, output: WlOutput, overlay_cursor: bool) -> Result<Capture, CaptureError> {
        let output = Output::from_resource(&output)
            .ok_or(CaptureError::Permanent(anyhow!("Output is gone").into()))?;
        
        let renderer = match self.backend {
            BackendData::Kms(ref mut kms) => {
                // the kms backend just keeps its dmabufs easily accessible for capture.
                return kms.capture_output(&output)
                    .map(|(device, dmabuf, presentation_time)| Capture {
                        device,
                        dmabuf,
                        presentation_time,
                    })
                    .ok_or(CaptureError::Temporary(anyhow!("Surface not initialized yet").into()));
            },
            BackendData::Winit(ref mut winit) => winit.backend.renderer(),
            BackendData::X11(ref mut x11) => &mut x11.renderer,
            _ => unreachable!(),
        };
        let device = device_from_renderer(renderer)
            .context("Failed to find DrmNode")
            .map_err(|err| CaptureError::Permanent(err.into()))?;

        let size = output.geometry().size.to_f64().to_buffer(
            output.current_scale().fractional_scale(),
            output.current_transform().into()
        ).to_i32_round();
        let buffer = Offscreen::<Gles2Renderbuffer>::create_buffer(renderer, size)
            .context("Failed to create render buffer for offscreen capture")
            .map_err(|err| CaptureError::Temporary(err.into()))?;
        renderer.bind(buffer)
            .context("Failed to bind render buffer for offscreen capture")
            .map_err(|err| CaptureError::Temporary(err.into()))?;
        render_output(
            None,
            renderer,
            0,
            &mut self.common,
            &output,
            !overlay_cursor,
            #[cfg(feature = "debug")]
            None,
        )
            .context("Failed to render desktop for offscreen capture")
            .map_err(|err| CaptureError::Temporary(err.into()))?;
        let dmabuf = renderer.export_framebuffer(size)
            .context("Failed to export buffer for offscreen capture")
            .map_err(|err| CaptureError::Temporary(err.into()))?;

        Ok(Capture {
            device,
            dmabuf,
            presentation_time: Instant::now(),
        })
    }

    fn capture_workspace(&mut self, _dh: &DisplayHandle, workspace: WorkspaceHandle, wl_output: WlOutput, overlay_cursor: bool) -> Result<Capture, CaptureError> {
        let output = Output::from_resource(&wl_output)
            .ok_or(CaptureError::Permanent(anyhow!("Output is gone").into()))?;
        let workspace = self.common.shell.spaces.iter().find(|w| w.handle == workspace)
            .ok_or(CaptureError::Permanent(anyhow!("Workspace is gone").into()))?
            .idx;
        if self.common.shell.active_space(&output).idx == workspace {
            self.capture_output(_dh, wl_output, overlay_cursor)
        } else {
            match self.backend {
                BackendData::Winit(ref mut winit) => {
                    let device = device_from_renderer(winit.backend.renderer())
                        .context("Failed to find DrmNode")
                        .map_err(|err| CaptureError::Permanent(err.into()))?;
                    capture_workspace(device, winit.backend.renderer(), &output, workspace, &mut self.common)
                },
                BackendData::X11(ref mut x11) => {
                    let device = device_from_renderer(&x11.renderer)
                        .context("Failed to find DrmNode")
                        .map_err(|err| CaptureError::Permanent(err.into()))?;
                    capture_workspace(device, &mut x11.renderer, &output, workspace, &mut self.common)
                },
                BackendData::Kms(ref mut kms) => {
                    let node = kms.target_node_for_output(&output)
                        .unwrap_or(kms.primary)
                        .node_with_type(NodeType::Render)
                        .with_context(|| "Unable to find node")
                        .map_err(|x| CaptureError::Permanent(x.into()))?
                        .map_err(|x| CaptureError::Permanent(x.into()))?;
                    let mut renderer = kms.api.renderer::<Gles2Renderbuffer>(&node, &node)
                        .with_context(|| format!("Failed to optain renderer for {:?}", node))
                        .map_err(|x| CaptureError::Permanent(x.into()))?;
                    capture_workspace(
                        node,
                        &mut renderer,
                        &output,
                        workspace,
                        &mut self.common,
                    )
                },
                BackendData::Unset => unreachable!(),
            }
        }
    }

    fn capture_toplevel(&mut self, dh: &DisplayHandle, window: Window, overlay_cursor: bool) -> Result<Capture, CaptureError> {
        let Kind::Xdg(xdg) = window.toplevel();
        let surface = xdg.wl_surface();
        let window_transform = with_states(surface, |states| states
            .cached_state
            .current::<SurfaceAttributes>()
            .buffer_transform
            .into()
        );

        let workspace = self.common.shell.space_for_window(surface);
        let pointers = if overlay_cursor && workspace.is_some() {
            self.common.seats
                .iter()
                .filter_map(|seat| {
                    let cursor_status = seat
                        .user_data()
                        .get::<RefCell<CursorImageStatus>>()
                        .map(|cell| {
                            let mut cursor_status = cell.borrow_mut();
                            if let CursorImageStatus::Image(ref surface) = *cursor_status {
                                if !surface.alive() {
                                    *cursor_status = CursorImageStatus::Default;
                                }
                            }
                            cursor_status.clone()
                        })
                        .unwrap_or(CursorImageStatus::Default);

                    if cursor_status != CursorImageStatus::Hidden {
                        let workspace = workspace.as_deref()?;
                        let loc = seat.get_pointer().map(|ptr| ptr.current_location())?;
                        let output = active_output(seat, &self.common);
                        
                        if self.common.shell.active_space(&output).idx == workspace.idx {
                            let relative = self.common.shell.space_relative_output_geometry(loc, &output);
                            // unwrap is safe, because we got this workspace from `space_for_window`. It has to contain the window.
                            let bbox = workspace.space.window_bbox(&window).unwrap();
                            bbox.contains(relative.to_i32_round()).then_some((seat, (relative - bbox.loc.to_f64()).to_i32_round()))
                        } else { None }
                    } else { None }
                })
                .collect::<Vec<_>>()
        } else {
            Vec::with_capacity(0)
        };

        let device = match self.backend {
            BackendData::Winit(ref mut winit) => device_from_renderer(winit.backend.renderer()),
            BackendData::X11(ref x11) => device_from_renderer(&x11.renderer),
            BackendData::Kms(ref kms) => Ok(dh.get_client(window.toplevel().wl_surface().id())
                .ok()
                .with_context(|| "Unable to find matching wayland client")
                .map_err(|x| CaptureError::Permanent(x.into()))?
                .get_data::<ClientState>()
                .unwrap()
                .drm_node
                .clone()
                .unwrap_or_else(|| kms.primary.clone())),
            _ => unreachable!(),
        }
            .context("Failed to find DrmNode")
            .map_err(|err| CaptureError::Permanent(err.into()))?;

        // first lets check, if we can just send a dmabuf from the client directly
        if pointers.is_empty() && window_transform == Transform::Normal && get_children(surface).is_empty() && self.common.shell.popups.find_popup(surface).is_none() {
            let dmabuf = with_renderer_surface_state(surface, |state| state.wl_buffer().and_then(|buf| get_dmabuf(buf).ok()));
            if let Some(dmabuf) = dmabuf {
                return Ok(Capture {
                    device,
                    dmabuf,
                    presentation_time: std::time::Instant::now(),
                });
            }
        }

        // we need to composite
        let mut _tmp_multirenderer = None;
        let renderer = match self.backend {
            BackendData::Winit(ref mut winit) => winit.backend.renderer(),
            BackendData::X11(ref mut x11) => &mut x11.renderer,
            BackendData::Kms(ref mut kms) => {
                _tmp_multirenderer = Some(kms.api.renderer::<Gles2Renderbuffer>(&device, &device)
                    .with_context(|| format!("Failed to optain renderer for {:?}", device))
                    .map_err(|x| CaptureError::Permanent(x.into()))?);
                _tmp_multirenderer.as_mut().unwrap().as_gles2()
            },
            BackendData::Unset => unreachable!(),
        };

        let bbox = window.bbox_with_popups();
        let size = bbox.size + Size::from((-bbox.loc.x, -bbox.loc.y));
        let buffer = Offscreen::<Gles2Renderbuffer>::create_buffer(renderer, size.to_buffer(1, window_transform))
            .context("Failed to create render buffer for offscreen capture")
            .map_err(|err| CaptureError::Temporary(err.into()))?;
        renderer.bind(buffer)
            .context("Failed to bind render buffer for offscreen capture")
            .map_err(|err| CaptureError::Temporary(err.into()))?;
        renderer.render(size.to_physical(1), Transform::Normal, |renderer, frame| {
            let log = slog_scope::logger();
            let damage = &[window.physical_bbox_with_popups((0.0, 0.0), 1.0)];
            draw_window(renderer, frame, &window, 1.0, (0.0, 0.0), damage, &log)?;
            draw_window_popups(renderer, frame, &window, 1.0, (0.0, 0.0), damage, &log)?;
            for (seat, loc) in pointers.into_iter() {
                if let Some(cursor_elem) = draw_cursor::<_, CustomElem>(renderer, seat, loc, &self.common.start_time, true) {
                    let damage = RenderElement::<Gles2Renderer>::accumulated_damage(&cursor_elem, 1.0, None);
                    cursor_elem.draw(renderer, frame, 1.0, loc.to_physical(1.0), &damage, &log)?;
                }
            }
            Result::<(), Gles2Error>::Ok(())
        })
            .context("Failed to render window for offscreen capture")
            .map_err(|err| CaptureError::Temporary(err.into()))?
            .context("Failed to render window for offscreen capture")
            .map_err(|err| CaptureError::Temporary(err.into()))?;
        
        let dmabuf = renderer.export_framebuffer(size.to_buffer(1, window_transform))
            .context("Failed to export buffer for offscreen capture")
            .map_err(|err| CaptureError::Temporary(err.into()))?;
        
        Ok(Capture { device, dmabuf, presentation_time: Instant::now() })
    }
    
    fn start_time(&mut self) -> Instant {
        self.common.start_time
    }
}

fn capture_workspace<E, T, R>(
    gpu: DrmNode,
    renderer: &mut R,
    output: &Output,
    idx: u8,
    state: &mut Common,
) -> Result<Capture, CaptureError>
where
    E: std::error::Error + Send + Sync + 'static,
    T: Clone + 'static,
    R: Renderer<Error=E, TextureId=T>
      + ImportAll
      + AsGles2Renderer
      + Offscreen<Gles2Renderbuffer>
      + Bind<Gles2Renderbuffer>
      + ExportDma,
    CustomElem: RenderElement<R>,
{
    let size = output.geometry().size.to_f64().to_buffer(
        output.current_scale().fractional_scale(),
        output.current_transform().into()
    ).to_i32_round();
    let buffer = Offscreen::<Gles2Renderbuffer>::create_buffer(renderer, size)
        .context("Failed to create render buffer for offscreen capture")
        .map_err(|err| CaptureError::Temporary(err.into()))?;
    renderer.bind(buffer)
        .context("Failed to bind render buffer for offscreen capture")
        .map_err(|err| CaptureError::Temporary(err.into()))?;
    render_workspace(
        Some(&gpu),
        renderer,
        0,
        state,
        idx,
        &output,
        true,
        #[cfg(feature = "debug")]
        None,
    )
        .map_err(|err| anyhow!("Failed to render desktop for offscreen capture: {:?}", err)) // meh..
        .map_err(|err| CaptureError::Temporary(err.into()))?;
    let dmabuf = renderer.export_framebuffer(size)
        .context("Failed to export buffer for offscreen capture")
        .map_err(|err| CaptureError::Temporary(err.into()))?;
    
    Ok(Capture { device: gpu, dmabuf, presentation_time: Instant::now() })
}

fn device_from_renderer(renderer: &Gles2Renderer) -> Result<DrmNode> {
    EGLDevice::device_for_display(renderer.egl_context().display())?
        .try_get_render_node()?
        .ok_or(anyhow!("No node associated with context (software context?)"))
}

delegate_export_dmabuf!(State);