use std::{
    cell::RefCell,
    collections::HashSet,
    ops::{Deref, DerefMut},
};

use anyhow::anyhow;
use cosmic_protocols::screencopy::v1::server::zcosmic_screencopy_session_v1::{
    FailureReason, InputType,
};
use smithay::{
    backend::{
        allocator::dmabuf::Dmabuf,
        drm::DrmNode,
        egl::EGLDevice,
        renderer::{
            buffer_dimensions, buffer_type,
            damage::{DamageTrackedRenderer, DamageTrackedRendererError},
            element::{
                surface::WaylandSurfaceRenderElement, AsRenderElements, RenderElement,
                RenderElementStates,
            },
            gles2::{Gles2Error, Gles2Renderbuffer},
            Bind, Blit, BufferType, ExportMem, ImportAll, ImportMem, Offscreen, Renderer,
        },
    },
    desktop::space::SpaceElement,
    output::Output,
    reexports::wayland_server::{
        protocol::{wl_buffer::WlBuffer, wl_shm::Format as ShmFormat, wl_surface::WlSurface},
        Resource,
    },
    utils::{IsAlive, Logical, Physical, Rectangle, Scale, Transform},
    wayland::{
        dmabuf::get_dmabuf,
        seat::WaylandFocus,
        shm::{with_buffer_contents, with_buffer_contents_mut},
    },
    xwayland::XWaylandClientData,
};

use crate::{
    backend::render::{
        cursor,
        element::{AsGlowRenderer, CosmicElement},
        render_output, render_workspace, CursorMode, CLEAR_COLOR,
    },
    shell::{CosmicMappedRenderElement, CosmicSurface},
    state::{BackendData, ClientState, Common, State},
    utils::prelude::OutputExt,
    wayland::protocols::{
        screencopy::{
            delegate_screencopy, BufferInfo, BufferParams, CursorMode as ScreencopyCursorMode,
            CursorSession, ScreencopyHandler, Session, SessionType,
        },
        workspace::WorkspaceHandle,
    },
};

use super::data_device::get_dnd_icon;

pub type PendingScreencopyBuffers = RefCell<Vec<(Session, BufferParams)>>;

#[derive(Debug, Default)]
pub struct ScreencopySessions(pub RefCell<Vec<DropableSession>>);

#[derive(Debug)]
pub struct DropableSession(Session, FailureReason);
impl Deref for DropableSession {
    type Target = Session;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for DropableSession {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl Drop for DropableSession {
    fn drop(&mut self) {
        self.0.failed(self.1);
    }
}
impl PartialEq<Session> for DropableSession {
    fn eq(&self, other: &Session) -> bool {
        &self.0 == other
    }
}

pub type SessionDTR = RefCell<DamageTrackedRenderer>;

impl ScreencopyHandler for State {
    fn capture_output(&mut self, output: Output, session: Session) -> Vec<BufferInfo> {
        let formats = match formats_for_output(&output, &mut self.backend) {
            Ok(formats) => formats,
            Err(reason) => {
                session.failed(reason);
                return Vec::new();
            }
        };

        for seat in self.common.seats() {
            if let Some(pointer) = seat.get_pointer() {
                if output
                    .geometry()
                    .contains(pointer.current_location().to_i32_round())
                {
                    session.cursor_enter(seat, InputType::Pointer);
                }
            }
        }

        session
            .user_data()
            .insert_if_missing(|| SessionDTR::new(DamageTrackedRenderer::from_output(&output)));
        output
            .user_data()
            .insert_if_missing(ScreencopySessions::default);
        output
            .user_data()
            .get::<ScreencopySessions>()
            .unwrap()
            .0
            .borrow_mut()
            .push(DropableSession(session, FailureReason::InvalidOutput));

        formats
    }

    fn capture_workspace(
        &mut self,
        handle: WorkspaceHandle,
        output: Output,
        session: Session,
    ) -> Vec<BufferInfo> {
        let formats = match formats_for_output(&output, &mut self.backend) {
            Ok(formats) => formats,
            Err(reason) => {
                session.failed(reason);
                return Vec::new();
            }
        };

        let workspace = match self.common.shell.space_for_handle_mut(&handle) {
            Some(workspace) => workspace,
            None => {
                session.failed(FailureReason::InvalidWorkspace);
                return Vec::new();
            }
        };

        session
            .user_data()
            .insert_if_missing(|| SessionDTR::new(DamageTrackedRenderer::from_output(&output)));

        workspace
            .screencopy_sessions
            .push(DropableSession(session, FailureReason::InvalidWorkspace));

        formats
    }

    fn capture_toplevel(&mut self, toplevel: CosmicSurface, session: Session) -> Vec<BufferInfo> {
        let Some(surface) = toplevel.wl_surface() else { return Vec::new() };
        let size = toplevel.geometry().size.to_buffer(1, Transform::Normal);

        let mut _kms_renderer = None;
        let renderer = match self.backend {
            BackendData::Kms(ref mut kms) => {
                let node = self
                    .common
                    .display_handle
                    .get_client(surface.id())
                    .ok()
                    .and_then(|client| {
                        if let Some(normal_client) = client.get_data::<ClientState>() {
                            return normal_client.drm_node.clone();
                        }
                        if let Some(xwayland_client) = client.get_data::<XWaylandClientData>() {
                            return xwayland_client.user_data().get::<DrmNode>().cloned();
                        }
                        None
                    })
                    .unwrap_or(kms.primary.clone());
                _kms_renderer = Some(kms.api.renderer::<Gles2Renderbuffer>(&node, &node).unwrap());
                _kms_renderer.as_mut().unwrap().as_mut()
            }
            BackendData::Winit(ref mut winit) => winit.backend.renderer(),
            BackendData::X11(ref mut x11) => &mut x11.renderer,
            _ => unreachable!(),
        };

        let mut formats = vec![
            BufferInfo::Shm {
                format: ShmFormat::Abgr8888,
                size,
                stride: size.w as u32 * 4,
            },
            BufferInfo::Shm {
                format: ShmFormat::Xbgr8888,
                size,
                stride: size.w as u32 * 4,
            },
        ];

        if let Some(node) = EGLDevice::device_for_display(renderer.egl_context().display())
            .ok()
            .and_then(|device| device.try_get_render_node().ok().flatten())
        {
            formats.extend(
                renderer
                    .egl_context()
                    .dmabuf_render_formats()
                    .iter()
                    .map(|format| format.code)
                    .collect::<HashSet<_>>()
                    .into_iter()
                    .map(|format| BufferInfo::Dmabuf { node, format, size }),
            );
        }

        let size = toplevel.geometry().size.to_physical(1);
        session.user_data().insert_if_missing(|| {
            SessionDTR::new(DamageTrackedRenderer::new(size, 1.0, Transform::Normal))
        });
        toplevel
            .user_data()
            .insert_if_missing(ScreencopySessions::default);
        toplevel
            .user_data()
            .get::<ScreencopySessions>()
            .unwrap()
            .0
            .borrow_mut()
            .push(DropableSession(session, FailureReason::InvalidToplevel));

        formats
    }

    fn capture_cursor(&mut self, _session: CursorSession) -> Vec<BufferInfo> {
        unimplemented!("We don't advertise the capture cursor mode")
    }

    fn buffer_attached(&mut self, session: Session, params: BufferParams, on_damage: bool) {
        // verify buffer size
        let buffer_size = match buffer_dimensions(&params.buffer) {
            Some(size) => size.to_logical(1, Transform::Normal),
            None => {
                slog_scope::warn!("Error during screencopy session: Buffer has no size");
                session.failed(FailureReason::InvalidBuffer);
                return;
            }
        };
        match session.session_type() {
            SessionType::Output(output) | SessionType::Workspace(output, _) => {
                let mode = match output.current_mode() {
                    Some(mode) => mode,
                    None => {
                        slog_scope::warn!("Error during screencopy session: Output has no mode");
                        session.failed(FailureReason::InvalidOutput);
                        return;
                    }
                }
                .size;

                if buffer_size.to_physical(1) != mode {
                    slog_scope::warn!("Error during screencopy session: Buffer size doesn't match");
                    session.failed(FailureReason::InvalidSize);
                    return;
                }
            }
            SessionType::Window(window) => {
                let geometry = window.geometry();
                if buffer_size != geometry.size {
                    slog_scope::warn!("Error during screencopy session: Buffer size doesn't match");
                    session.failed(FailureReason::InvalidSize);
                    return;
                }
            }
            _ => {}
        };

        if !matches!(
            buffer_type(&params.buffer),
            Some(BufferType::Shm) | Some(BufferType::Dma)
        ) {
            slog_scope::warn!("Error during screencopy session: Buffer is neither shm or dma");
            session.failed(FailureReason::InvalidBuffer);
            return;
        }

        if let Some(BufferType::Shm) = buffer_type(&params.buffer) {
            if with_buffer_contents(&params.buffer, |_, info| {
                info.format != ShmFormat::Abgr8888 && info.format != ShmFormat::Xbgr8888
            })
            .unwrap()
            {
                slog_scope::warn!("Error during screencopy session: Invalid shm buffer format");
                session.failed(FailureReason::InvalidBuffer);
                return;
            }
        }

        if on_damage {
            match session.session_type() {
                SessionType::Output(output) => {
                    output
                        .user_data()
                        .insert_if_missing(PendingScreencopyBuffers::default);
                    output
                        .user_data()
                        .get::<PendingScreencopyBuffers>()
                        .unwrap()
                        .borrow_mut()
                        .push((session, params));
                }
                SessionType::Workspace(_output, handle) => {
                    match self.common.shell.space_for_handle_mut(&handle) {
                        Some(workspace) => workspace.pending_buffers.push((session, params)),
                        None => session.failed(FailureReason::InvalidWorkspace),
                    };
                }
                SessionType::Window(window) => {
                    window
                        .user_data()
                        .insert_if_missing(PendingScreencopyBuffers::default);
                    window
                        .user_data()
                        .get::<PendingScreencopyBuffers>()
                        .unwrap()
                        .borrow_mut()
                        .push((session, params));
                }
                _ => unreachable!(),
            };
        } else {
            let buffer = params.buffer.clone();
            let result = match session.session_type() {
                SessionType::Output(output) => {
                    render_output_to_buffer(self, &session, params, &output)
                }
                SessionType::Workspace(output, handle) => {
                    render_workspace_to_buffer(self, &session, params, &output, &handle)
                }
                SessionType::Window(window) => {
                    render_window_to_buffer(self, &session, params, &window)
                }
                _ => unreachable!("Session types not supported"),
            };

            match result {
                Ok(false) => {
                    // client didn't wanna wait for damage, so it gets empty damage
                    session.commit_buffer(
                        match session.session_type() {
                            SessionType::Output(output) | SessionType::Workspace(output, _) => {
                                output.current_transform()
                            }
                            _ => Transform::Normal,
                        },
                        Vec::new(),
                        None,
                    );
                    buffer.release();
                }
                Ok(true) => {} // success
                Err((reason, error)) => {
                    slog_scope::warn!("Error during screencopy session: {}", error);
                    session.failed(reason);
                }
            }
        }
    }

    fn cursor_session_destroyed(&mut self, _session: CursorSession) {
        unreachable!("We currently don't support cursor sessions");
    }

    fn session_destroyed(&mut self, session: Session) {
        match session.session_type() {
            SessionType::Output(output) => {
                if let Some(pending_buffers) = output.user_data().get::<PendingScreencopyBuffers>()
                {
                    pending_buffers.borrow_mut().retain(|(s, _)| s != &session);
                }
                if let Some(sessions) = output.user_data().get::<ScreencopySessions>() {
                    sessions.0.borrow_mut().retain(|s| s != &session);
                }
            }
            SessionType::Workspace(_, handle) => {
                if let Some(workspace) = self.common.shell.space_for_handle_mut(&handle) {
                    workspace.pending_buffers.retain(|(s, _)| s != &session);
                    workspace.screencopy_sessions.retain(|s| s != &session);
                }
            }
            SessionType::Window(window) => {
                if let Some(pending_buffers) = window.user_data().get::<PendingScreencopyBuffers>()
                {
                    pending_buffers.borrow_mut().retain(|(s, _)| s != &session);
                }
                if let Some(sessions) = window.user_data().get::<ScreencopySessions>() {
                    sessions.0.borrow_mut().retain(|s| s != &session);
                }
            }
            _ => {}
        }
    }
}

fn formats_for_output(
    output: &Output,
    backend: &mut BackendData,
) -> Result<Vec<BufferInfo>, FailureReason> {
    let mode = match output.current_mode() {
        Some(mode) => mode.size.to_logical(1).to_buffer(1, Transform::Normal),
        None => {
            return Err(FailureReason::InvalidOutput);
        }
    };

    let mut _kms_renderer = None;
    let renderer = match backend {
        BackendData::Kms(ref mut kms) => {
            let node = kms.target_node_for_output(&output).unwrap_or(kms.primary);
            _kms_renderer = Some(kms.api.renderer::<Gles2Renderbuffer>(&node, &node).unwrap());
            _kms_renderer.as_mut().unwrap().as_mut()
        }
        BackendData::Winit(ref mut winit) => winit.backend.renderer(),
        BackendData::X11(ref mut x11) => &mut x11.renderer,
        _ => unreachable!(),
    };

    let mut formats = vec![
        BufferInfo::Shm {
            format: ShmFormat::Abgr8888,
            size: mode,
            stride: mode.w as u32 * 4,
        },
        BufferInfo::Shm {
            format: ShmFormat::Xbgr8888,
            size: mode,
            stride: mode.w as u32 * 4,
        },
    ];

    if let Some(node) = EGLDevice::device_for_display(renderer.egl_context().display())
        .ok()
        .and_then(|device| device.try_get_render_node().ok().flatten())
    {
        formats.extend(
            renderer
                .egl_context()
                .dmabuf_render_formats()
                .iter()
                .map(|format| format.code)
                .collect::<HashSet<_>>()
                .into_iter()
                .map(|format| BufferInfo::Dmabuf {
                    node,
                    format,
                    size: mode,
                }),
        );
    }

    Ok(formats)
}

fn node_from_params(
    params: &BufferParams,
    backend: &BackendData,
    output: Option<&Output>,
) -> Option<DrmNode> {
    match buffer_type(&params.buffer) {
        Some(BufferType::Dma) if params.node.is_some() => params.node.clone(),
        Some(BufferType::Shm) | Some(BufferType::Dma) => match backend {
            BackendData::Kms(kms) => Some(
                output
                    .and_then(|output| kms.target_node_for_output(output))
                    .unwrap_or(kms.primary),
            ),
            _ => None,
        },
        _ => unreachable!(),
    }
}

fn submit_buffer<R>(
    session: &Session,
    buffer: &WlBuffer,
    renderer: &mut R,
    transform: Transform,
    damage: Vec<Rectangle<i32, Physical>>,
) -> Result<(), <R as Renderer>::Error>
where
    R: ExportMem,
{
    if matches!(buffer_type(buffer), Some(BufferType::Shm)) {
        let buffer_size = buffer_dimensions(buffer).unwrap();
        with_buffer_contents_mut(buffer, |slice, data| {
            let offset = data.offset as i32;
            let width = data.width as i32;
            let height = data.height as i32;
            let stride = data.stride as i32;

            // number of bytes per pixel
            // TODO: compute from data.format
            let pixelsize = 4i32;

            // ensure consistency, the SHM handler of smithay should ensure this
            assert!((offset + (height - 1) * stride + width * pixelsize) as usize <= slice.len());

            let mapping =
                renderer.copy_framebuffer(Rectangle::from_loc_and_size((0, 0), buffer_size))?;
            let gl_data = renderer.map_texture(&mapping)?;
            assert!((width * height * pixelsize) as usize <= gl_data.len());

            for i in 0..height {
                unsafe {
                    std::ptr::copy_nonoverlapping::<u8>(
                        gl_data.as_ptr().offset((width * pixelsize * i) as isize),
                        slice.as_mut_ptr().offset((offset + stride * i) as isize),
                        (width * pixelsize) as usize,
                    );
                }
            }
            Ok(())
        })
        .unwrap()?;
    }

    session.commit_buffer(transform, damage, None);
    buffer.release();

    Ok(())
}

pub fn render_session<F, R>(
    node: Option<DrmNode>,
    renderer: &mut R,
    session: &Session,
    params: &BufferParams,
    transform: Transform,
    render_fn: F,
) -> Result<bool, DamageTrackedRendererError<R>>
where
    R: ExportMem,
    F: FnOnce(
        Option<&DrmNode>,
        &WlBuffer,
        &mut R,
        &mut DamageTrackedRenderer,
        usize,
    ) -> Result<
        (Option<Vec<Rectangle<i32, Physical>>>, RenderElementStates),
        DamageTrackedRendererError<R>,
    >,
{
    let mut dtr = session
        .user_data()
        .get::<SessionDTR>()
        .unwrap()
        .borrow_mut();

    let res = render_fn(
        node.as_ref(),
        &params.buffer,
        renderer,
        &mut *dtr,
        params.age as usize,
    )?;

    if let (Some(damage), _) = res {
        submit_buffer(session, &params.buffer, renderer, transform, damage)
            .map_err(DamageTrackedRendererError::Rendering)?;
        Ok(true)
    } else {
        Ok(false)
    }
}

pub fn render_output_to_buffer(
    state: &mut State,
    session: &Session,
    params: BufferParams,
    output: &Output,
) -> Result<bool, (FailureReason, anyhow::Error)> {
    let mode = output
        .current_mode()
        .map(|mode| mode.size.to_logical(1).to_buffer(1, Transform::Normal));
    let buffer_size = buffer_dimensions(&params.buffer).unwrap();
    if mode != Some(buffer_size) {
        return Err((FailureReason::InvalidSize, anyhow!("Output changed mode")));
    }

    fn render_fn<R>(
        node: Option<&DrmNode>,
        buffer: &WlBuffer,
        renderer: &mut R,
        dtr: &mut DamageTrackedRenderer,
        age: usize,
        common: &mut Common,
        session: &Session,
        output: &Output,
    ) -> Result<
        (Option<Vec<Rectangle<i32, Physical>>>, RenderElementStates),
        DamageTrackedRendererError<R>,
    >
    where
        R: Renderer
            + ImportAll
            + ImportMem
            + ExportMem
            + Bind<Dmabuf>
            + Offscreen<Gles2Renderbuffer>
            + Blit<Dmabuf>
            + AsGlowRenderer,
        <R as Renderer>::TextureId: Clone + 'static,
        <R as Renderer>::Error: From<Gles2Error>,
        CosmicElement<R>: RenderElement<R>,
        CosmicMappedRenderElement<R>: RenderElement<R>,
    {
        let cursor_mode = match session.cursor_mode() {
            ScreencopyCursorMode::Embedded => CursorMode::All,
            ScreencopyCursorMode::Captured(_) | ScreencopyCursorMode::None => CursorMode::None,
        };

        if let Ok(dmabuf) = get_dmabuf(buffer) {
            render_output::<_, _, Gles2Renderbuffer, Dmabuf>(
                node,
                renderer,
                dmabuf,
                dtr,
                age,
                common,
                &output,
                cursor_mode,
                None,
                None,
            )
        } else {
            let size = buffer_dimensions(buffer).unwrap();
            let render_buffer = Offscreen::<Gles2Renderbuffer>::create_buffer(renderer, size)
                .map_err(DamageTrackedRendererError::Rendering)?;
            render_output::<_, _, Gles2Renderbuffer, Dmabuf>(
                node,
                renderer,
                render_buffer,
                dtr,
                age,
                common,
                &output,
                cursor_mode,
                None,
                None,
            )
        }
    }

    let common = &mut state.common;
    let node = node_from_params(&params, &mut state.backend, Some(output));
    match &mut state.backend {
        BackendData::Kms(kms) => {
            let mut multirenderer = kms
                .api
                .renderer::<Gles2Renderbuffer>(node.as_ref().unwrap(), node.as_ref().unwrap())
                .map_err(|err| (FailureReason::Unspec, err.into()))?;
            render_session::<_, _>(
                node,
                &mut multirenderer,
                session,
                &params,
                output.current_transform(),
                |node, buffer, renderer, dtr, age| {
                    render_fn(node, buffer, renderer, dtr, age, common, session, output)
                },
            )
            .map_err(|err| match err {
                DamageTrackedRendererError::OutputNoMode(x) => (FailureReason::Unspec, x.into()),
                DamageTrackedRendererError::Rendering(x) => (FailureReason::Unspec, x.into()),
            })
        }
        BackendData::Winit(winit) => render_session::<_, _>(
            node,
            winit.backend.renderer(),
            session,
            &params,
            output.current_transform(),
            |node, buffer, renderer, dtr, age| {
                render_fn(node, buffer, renderer, dtr, age, common, session, output)
            },
        )
        .map_err(|err| (FailureReason::Unspec, err.into())),
        BackendData::X11(x11) => render_session::<_, _>(
            node,
            &mut x11.renderer,
            session,
            &params,
            output.current_transform(),
            |node, buffer, renderer, dtr, age| {
                render_fn(node, buffer, renderer, dtr, age, common, session, output)
            },
        )
        .map_err(|err| (FailureReason::Unspec, err.into())),
        _ => unreachable!(),
    }
}

pub fn render_workspace_to_buffer(
    state: &mut State,
    session: &Session,
    params: BufferParams,
    output: &Output,
    handle: &WorkspaceHandle,
) -> Result<bool, (FailureReason, anyhow::Error)> {
    let mode = output
        .current_mode()
        .map(|mode| mode.size.to_logical(1).to_buffer(1, Transform::Normal));
    let buffer_size = buffer_dimensions(&params.buffer).unwrap();
    if mode != Some(buffer_size) {
        return Err((FailureReason::InvalidSize, anyhow!("Output changed mode")));
    }

    fn render_fn<R>(
        node: Option<&DrmNode>,
        buffer: &WlBuffer,
        renderer: &mut R,
        dtr: &mut DamageTrackedRenderer,
        age: usize,
        common: &mut Common,
        session: &Session,
        output: &Output,
        handle: &WorkspaceHandle,
    ) -> Result<
        (Option<Vec<Rectangle<i32, Physical>>>, RenderElementStates),
        DamageTrackedRendererError<R>,
    >
    where
        R: Renderer
            + ImportAll
            + ImportMem
            + ExportMem
            + Bind<Dmabuf>
            + Offscreen<Gles2Renderbuffer>
            + Blit<Dmabuf>
            + AsGlowRenderer,
        <R as Renderer>::TextureId: Clone + 'static,
        <R as Renderer>::Error: From<Gles2Error>,
        CosmicElement<R>: RenderElement<R>,
        CosmicMappedRenderElement<R>: RenderElement<R>,
    {
        let cursor_mode = match session.cursor_mode() {
            ScreencopyCursorMode::Embedded => CursorMode::All,
            ScreencopyCursorMode::Captured(_) | ScreencopyCursorMode::None => CursorMode::None,
        };
        if let Ok(dmabuf) = get_dmabuf(buffer) {
            render_workspace::<_, _, Gles2Renderbuffer, Dmabuf>(
                node,
                renderer,
                dmabuf,
                dtr,
                age,
                common,
                &output,
                handle,
                cursor_mode,
                None,
                None,
            )
        } else {
            let size = buffer_dimensions(buffer).unwrap();
            let render_buffer = Offscreen::<Gles2Renderbuffer>::create_buffer(renderer, size)
                .map_err(DamageTrackedRendererError::Rendering)?;
            render_workspace::<_, _, Gles2Renderbuffer, Dmabuf>(
                node,
                renderer,
                render_buffer,
                dtr,
                age,
                common,
                &output,
                handle,
                cursor_mode,
                None,
                None,
            )
        }
    }

    let node = node_from_params(&params, &mut state.backend, Some(output));
    let common = &mut state.common;
    match &mut state.backend {
        BackendData::Kms(kms) => {
            let mut multirenderer = kms
                .api
                .renderer::<Gles2Renderbuffer>(node.as_ref().unwrap(), node.as_ref().unwrap())
                .map_err(|err| (FailureReason::Unspec, err.into()))?;
            render_session::<_, _>(
                node,
                &mut multirenderer,
                session,
                &params,
                output.current_transform(),
                |node, buffer, renderer, dtr, age| {
                    render_fn(
                        node, buffer, renderer, dtr, age, common, session, output, handle,
                    )
                },
            )
            .map_err(|err| match err {
                DamageTrackedRendererError::OutputNoMode(x) => (FailureReason::Unspec, x.into()),
                DamageTrackedRendererError::Rendering(x) => (FailureReason::Unspec, x.into()),
            })
        }
        BackendData::Winit(winit) => render_session::<_, _>(
            node,
            winit.backend.renderer(),
            session,
            &params,
            output.current_transform(),
            |node, buffer, renderer, dtr, age| {
                render_fn(
                    node, buffer, renderer, dtr, age, common, session, output, handle,
                )
            },
        )
        .map_err(|err| (FailureReason::Unspec, err.into())),
        BackendData::X11(x11) => render_session::<_, _>(
            node,
            &mut x11.renderer,
            session,
            &params,
            output.current_transform(),
            |node, buffer, renderer, dtr, age| {
                render_fn(
                    node, buffer, renderer, dtr, age, common, session, output, handle,
                )
            },
        )
        .map_err(|err| (FailureReason::Unspec, err.into())),
        _ => unreachable!(),
    }
}

smithay::render_elements! {
    pub WindowCaptureElement<R> where R: ImportAll;
    WaylandElement=WaylandSurfaceRenderElement<R>,
    CursorElement=cursor::CursorRenderElement<R>,
}

pub fn render_window_to_buffer(
    state: &mut State,
    session: &Session,
    params: BufferParams,
    window: &CosmicSurface,
) -> Result<bool, (FailureReason, anyhow::Error)> {
    let geometry = window.geometry();
    let buffer_size = buffer_dimensions(&params.buffer).unwrap();
    if buffer_size != geometry.size.to_buffer(1, Transform::Normal) {
        return Err((FailureReason::InvalidSize, anyhow!("Window changed size")));
    }

    fn render_fn<R>(
        buffer: &WlBuffer,
        renderer: &mut R,
        dtr: &mut DamageTrackedRenderer,
        age: usize,
        session: &Session,
        common: &mut Common,
        window: &CosmicSurface,
        geometry: Rectangle<i32, Logical>,
    ) -> Result<
        (Option<Vec<Rectangle<i32, Physical>>>, RenderElementStates),
        DamageTrackedRendererError<R>,
    >
    where
        R: Renderer
            + ImportAll
            + ImportMem
            + ExportMem
            + Bind<Dmabuf>
            + Offscreen<Gles2Renderbuffer>
            + Blit<Dmabuf>
            + AsGlowRenderer,
        <R as Renderer>::TextureId: Clone + 'static,
        <R as Renderer>::Error: From<Gles2Error>,
        CosmicElement<R>: RenderElement<R>,
        CosmicMappedRenderElement<R>: RenderElement<R>,
    {
        // TODO cursor elements!
        let mut elements = AsRenderElements::<R>::render_elements::<WindowCaptureElement<R>>(
            window,
            renderer,
            (-geometry.loc.x, -geometry.loc.y).into(),
            Scale::from(1.0),
        );

        for seat in common.seats() {
            if let Some(location) = {
                // we need to find the mapped element in that case
                if let Some(mapped) = common.shell.element_for_surface(&window) {
                    mapped.cursor_position(seat).and_then(|mut p| {
                        p -= mapped.active_window_offset().to_f64();
                        if p.x < 0. || p.y < 0. {
                            None
                        } else {
                            Some(p)
                        }
                    })
                } else {
                    None
                }
            } {
                if session.cursor_mode() == ScreencopyCursorMode::Embedded {
                    elements.extend(
                        cursor::draw_cursor(
                            renderer,
                            seat,
                            location,
                            1.0.into(),
                            common.clock.now(),
                            true,
                        )
                        .into_iter()
                        .map(WindowCaptureElement::from),
                    );
                }

                if let Some(wl_surface) = get_dnd_icon(seat) {
                    elements.extend(
                        cursor::draw_dnd_icon(renderer, &wl_surface, location.to_i32_round(), 1.0)
                            .into_iter()
                            .map(WindowCaptureElement::from),
                    );
                }
            }
        }

        if let Ok(dmabuf) = get_dmabuf(buffer) {
            renderer
                .bind(dmabuf)
                .map_err(DamageTrackedRendererError::Rendering)?;
        } else {
            let size = buffer_dimensions(buffer).unwrap();
            let render_buffer = Offscreen::<Gles2Renderbuffer>::create_buffer(renderer, size)
                .map_err(DamageTrackedRendererError::Rendering)?;
            renderer
                .bind(render_buffer)
                .map_err(DamageTrackedRendererError::Rendering)?;
        }

        dtr.render_output(renderer, age, &elements, CLEAR_COLOR, None)
    }

    let node = node_from_params(&params, &mut state.backend, None);
    let common = &mut state.common;
    match &mut state.backend {
        BackendData::Kms(kms) => {
            let mut multirenderer = kms
                .api
                .renderer::<Gles2Renderbuffer>(node.as_ref().unwrap(), node.as_ref().unwrap())
                .map_err(|err| (FailureReason::Unspec, err.into()))?;
            render_session::<_, _>(
                node,
                &mut multirenderer,
                session,
                &params,
                Transform::Normal,
                |_node, buffer, renderer, dtr, age| {
                    render_fn(
                        buffer, renderer, dtr, age, session, common, window, geometry,
                    )
                },
            )
            .map_err(|err| match err {
                DamageTrackedRendererError::OutputNoMode(x) => (FailureReason::Unspec, x.into()),
                DamageTrackedRendererError::Rendering(x) => (FailureReason::Unspec, x.into()),
            })
        }
        BackendData::Winit(winit) => render_session::<_, _>(
            node,
            winit.backend.renderer(),
            session,
            &params,
            Transform::Normal,
            |_node, buffer, renderer, dtr, age| {
                render_fn(
                    buffer, renderer, dtr, age, session, common, window, geometry,
                )
            },
        )
        .map_err(|err| (FailureReason::Unspec, err.into())),
        BackendData::X11(x11) => render_session::<_, _>(
            node,
            &mut x11.renderer,
            session,
            &params,
            Transform::Normal,
            |_node, buffer, renderer, dtr, age| {
                render_fn(
                    buffer, renderer, dtr, age, session, common, window, geometry,
                )
            },
        )
        .map_err(|err| (FailureReason::Unspec, err.into())),
        _ => unreachable!(),
    }
}

impl Common {
    pub fn still_pending(&mut self, session: Session, params: BufferParams) {
        match session.session_type() {
            SessionType::Output(output) => {
                if output
                    .user_data()
                    .get::<ScreencopySessions>()
                    .map_or(false, |sessions| {
                        sessions.0.borrow().iter().any(|s| &*s == &session)
                    })
                {
                    output
                        .user_data()
                        .get::<PendingScreencopyBuffers>()
                        .unwrap()
                        .borrow_mut()
                        .push((session, params));
                }
            }
            SessionType::Workspace(_output, handle) => {
                if let Some(space) = self.shell.space_for_handle_mut(&handle) {
                    if space.screencopy_sessions.iter().any(|s| s == &session) {
                        space.pending_buffers.push((session, params));
                    }
                }
            }
            SessionType::Window(window) => {
                if window
                    .user_data()
                    .get::<ScreencopySessions>()
                    .map_or(false, |sessions| {
                        sessions.0.borrow().iter().any(|s| &*s == &session)
                    })
                {
                    window
                        .user_data()
                        .get::<PendingScreencopyBuffers>()
                        .unwrap()
                        .borrow_mut()
                        .push((session, params));
                }
            }
            _ => {}
        }
    }
}

pub trait UserdataExt {
    fn sessions(&self) -> Vec<Session>;
    fn pending_buffers(
        &self,
    ) -> std::iter::Flatten<std::option::IntoIter<std::vec::IntoIter<(Session, BufferParams)>>>;
}

impl UserdataExt for Output {
    fn sessions(&self) -> Vec<Session> {
        self.user_data()
            .get::<ScreencopySessions>()
            .map_or(Vec::new(), |sessions| {
                sessions.0.borrow().iter().map(|s| s.0.clone()).collect()
            })
    }

    fn pending_buffers(
        &self,
    ) -> std::iter::Flatten<std::option::IntoIter<std::vec::IntoIter<(Session, BufferParams)>>>
    {
        self.user_data()
            .get::<PendingScreencopyBuffers>()
            .map(|pending| pending.borrow_mut().split_off(0).into_iter())
            .into_iter()
            .flatten()
    }
}

impl UserdataExt for CosmicSurface {
    fn sessions(&self) -> Vec<Session> {
        self.user_data()
            .get::<ScreencopySessions>()
            .map_or(Vec::new(), |sessions| {
                sessions.0.borrow().iter().map(|s| s.0.clone()).collect()
            })
    }

    fn pending_buffers(
        &self,
    ) -> std::iter::Flatten<std::option::IntoIter<std::vec::IntoIter<(Session, BufferParams)>>>
    {
        self.user_data()
            .get::<PendingScreencopyBuffers>()
            .map(|pending| pending.borrow_mut().split_off(0).into_iter())
            .into_iter()
            .flatten()
    }
}

impl State {
    pub fn schedule_window_session(&mut self, surface: &WlSurface) {
        if let Some(element) = surface
            .wl_surface()
            .and_then(|surface| self.common.shell.element_for_wl_surface(&surface).cloned())
        {
            let active = element.active_window();
            if active.wl_surface().as_ref() == Some(surface) {
                for (session, params) in active.pending_buffers() {
                    let window = active.clone();
                    self.common.event_loop_handle.insert_idle(move |data| {
                        if !session.alive() {
                            return;
                        }

                        match render_window_to_buffer(
                            &mut data.state,
                            &session,
                            params.clone(),
                            &window,
                        ) {
                            // rendering yielded no damage, buffer is still pending
                            Ok(false) => data.state.common.still_pending(session, params),
                            Ok(true) => {} // success
                            Err((reason, err)) => {
                                slog_scope::warn!("Screencopy session failed: {}", err);
                                session.failed(reason);
                            }
                        }
                    });
                }
            }
        }
    }

    pub fn workspace_session_for_output(
        &mut self,
        output: &Output,
    ) -> Option<Vec<(Session, BufferParams)>> {
        let workspace = self.common.shell.active_space_mut(output);
        if !workspace.pending_buffers.is_empty() {
            Some(std::mem::take(&mut workspace.pending_buffers))
        } else {
            None
        }
    }

    pub fn schedule_workspace_sessions(
        &mut self,
        surface: &WlSurface,
    ) -> Option<Vec<(Session, BufferParams)>> {
        // here we store additional workspace_sessions, we should handle, when rendering the corresponding output anyway
        let mut output_sessions: Option<Vec<(Session, BufferParams)>> = None;

        // lets check which workspaces this surface belongs to
        let active_spaces = self
            .common
            .shell
            .outputs()
            .map(|o| (o.clone(), self.common.shell.active_space(o).handle.clone()))
            .collect::<Vec<_>>();
        for (handle, output) in self.common.shell.workspaces_for_surface(surface) {
            let workspace = self.common.shell.space_for_handle_mut(&handle).unwrap();
            if !workspace.pending_buffers.is_empty() {
                // TODO: replace with drain_filter....
                let mut i = 0;
                while i < workspace.pending_buffers.len() {
                    if let SessionType::Workspace(o, w) =
                        workspace.pending_buffers[i].0.session_type()
                    {
                        if active_spaces.contains(&(o.clone(), w)) {
                            // surface is on an active workspace/output combo, add to workspace_sessions
                            let (session, params) = workspace.pending_buffers.remove(i);
                            output_sessions
                                .get_or_insert_with(Vec::new)
                                .push((session, params));
                        } else if handle == w && output == o {
                            // surface is visible on an offscreen workspace session, schedule a new render
                            let (session, params) = workspace.pending_buffers.remove(i);
                            let output = output.clone();
                            self.common.event_loop_handle.insert_idle(move |data| {
                                if !session.alive() {
                                    return;
                                }
                                if !data.state.common.shell.outputs.contains(&output) {
                                    return;
                                }
                                match render_workspace_to_buffer(
                                    &mut data.state,
                                    &session,
                                    params.clone(),
                                    &output,
                                    &handle,
                                ) {
                                    Ok(false) => {
                                        // rendering yielded no new damage, buffer still pending
                                        data.state.common.still_pending(session, params);
                                    }
                                    Ok(true) => {}
                                    Err((reason, err)) => {
                                        slog_scope::warn!("Screencopy session failed: {}", err);
                                        session.failed(reason);
                                    }
                                }
                            });
                        } else {
                            i += 1;
                        }
                    } else {
                        unreachable!();
                    }
                }
            }
        }

        output_sessions
    }
}

delegate_screencopy!(State);
