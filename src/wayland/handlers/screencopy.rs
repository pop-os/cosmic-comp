use std::{
    cell::RefCell,
    collections::HashSet,
    ops::{Deref, DerefMut},
};

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
                surface::WaylandSurfaceRenderElement, AsRenderElements, RenderElementStates,
            },
            gles2::{Gles2Renderbuffer, Gles2Renderer},
            Bind, BufferType, ExportMem, Offscreen, Renderer,
        },
    },
    desktop::Window,
    output::Output,
    reexports::wayland_server::{
        protocol::{wl_buffer::WlBuffer, wl_shm::Format as ShmFormat},
        Resource,
    },
    utils::{Physical, Rectangle, Scale, Transform},
    wayland::{
        dmabuf::get_dmabuf,
        shm::{with_buffer_contents, with_buffer_contents_mut},
    },
};

use crate::{
    backend::render::{render_output, render_workspace, CursorMode, CLEAR_COLOR},
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
            .push(DropableSession(session, FailureReason::OutputDisabled));

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
                session.failed(FailureReason::Unspec);
                return Vec::new();
            }
        };

        session
            .user_data()
            .insert_if_missing(|| SessionDTR::new(DamageTrackedRenderer::from_output(&output)));

        workspace
            .screencopy_sessions
            .push(DropableSession(session, FailureReason::InvalidOutput));

        formats
    }

    fn capture_toplevel(&mut self, toplevel: Window, session: Session) -> Vec<BufferInfo> {
        let surface = toplevel.toplevel().wl_surface();
        let size = toplevel
            .bbox_with_popups()
            .size
            .to_buffer(1, Transform::Normal);

        let mut _kms_renderer = None;
        let renderer = match self.backend {
            BackendData::Kms(ref mut kms) => {
                let node = self
                    .common
                    .display_handle
                    .get_client(surface.id())
                    .ok()
                    .and_then(|client| client.get_data::<ClientState>().unwrap().drm_node.clone())
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
                format: ShmFormat::Argb8888,
                size,
                stride: size.w as u32 * 4,
            },
            BufferInfo::Shm {
                format: ShmFormat::Xrgb8888,
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
            .push(DropableSession(session, FailureReason::ToplevelDestroyed));

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
                    session.failed(FailureReason::InvalidBuffer);
                    return;
                }
            }
            SessionType::Window(window) => {
                let geometry = window.geometry();
                if buffer_size != geometry.size {
                    slog_scope::warn!("Error during screencopy session: Buffer size doesn't match");
                    session.failed(FailureReason::InvalidBuffer);
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
                info.format != ShmFormat::Argb8888 && info.format != ShmFormat::Xrgb8888
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
                        None => session.failed(FailureReason::OutputDisabled),
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
            return Err(FailureReason::OutputDisabled);
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
            format: ShmFormat::Argb8888,
            size: mode,
            stride: mode.w as u32 * 4,
        },
        BufferInfo::Shm {
            format: ShmFormat::Xrgb8888,
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

fn prepare_renderer<R, Target>(
    renderer: &mut R,
    buffer: &WlBuffer,
) -> Result<(), <R as Renderer>::Error>
where
    R: Bind<Dmabuf> + Offscreen<Target>,
{
    if let Ok(dmabuf) = get_dmabuf(buffer) {
        renderer.bind(dmabuf)?;
    } else {
        let size = buffer_dimensions(buffer).unwrap();
        let render_buffer = renderer.create_buffer(size)?;
        renderer.bind(render_buffer)?;
    };
    Ok(())
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
        with_buffer_contents_mut(buffer, |data, _info| {
            let mapping =
                renderer.copy_framebuffer(Rectangle::from_loc_and_size((0, 0), buffer_size))?;
            let gl_data = renderer.map_texture(&mapping)?;
            data.copy_from_slice(gl_data);
            Ok(())
        })
        .unwrap()?;
    }

    session.commit_buffer(transform, damage, None);

    Ok(())
}

pub fn render_to_buffer<F, R, Target>(
    node: Option<DrmNode>,
    renderer: &mut R,
    session: &Session,
    params: &BufferParams,
    transform: Transform,
    render_fn: F,
) -> Result<bool, DamageTrackedRendererError<R>>
where
    R: Bind<Dmabuf> + Offscreen<Target> + ExportMem,
    F: FnOnce(
        Option<&DrmNode>,
        &mut R,
        &mut DamageTrackedRenderer,
        usize,
    ) -> Result<
        (Option<Vec<Rectangle<i32, Physical>>>, RenderElementStates),
        DamageTrackedRendererError<R>,
    >,
{
    prepare_renderer(renderer, &params.buffer).map_err(DamageTrackedRendererError::Rendering)?;

    let mut dtr = session
        .user_data()
        .get::<SessionDTR>()
        .unwrap()
        .borrow_mut();

    let res = render_fn(node.as_ref(), renderer, &mut *dtr, params.age as usize)?;

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
    let node = node_from_params(&params, &mut state.backend, Some(output));
    let mut _tmp_multirenderer = None;
    let renderer = match &mut state.backend {
        BackendData::Kms(kms) => {
            _tmp_multirenderer = Some(
                kms.api
                    .renderer::<Gles2Renderbuffer>(node.as_ref().unwrap(), node.as_ref().unwrap())
                    .map_err(|err| (FailureReason::Unspec, err.into()))?,
            );
            _tmp_multirenderer.as_mut().unwrap().as_mut()
        }
        BackendData::Winit(winit) => winit.backend.renderer(),
        BackendData::X11(x11) => &mut x11.renderer,
        _ => unreachable!(),
    };

    let common = &mut state.common;
    render_to_buffer::<_, _, Gles2Renderbuffer>(
        node,
        renderer,
        session,
        &params,
        output.current_transform(),
        |node, renderer, dtr, age| {
            render_output::<_, Gles2Renderbuffer, Dmabuf>(
                node,
                renderer,
                dtr,
                age,
                common,
                &output,
                match session.cursor_mode() {
                    ScreencopyCursorMode::Embedded => CursorMode::All,
                    ScreencopyCursorMode::Captured(_) | ScreencopyCursorMode::None => {
                        CursorMode::None
                    }
                },
                None,
            )
        },
    )
    .map_err(|err| (FailureReason::Unspec, err.into()))
}

pub fn render_workspace_to_buffer(
    state: &mut State,
    session: &Session,
    params: BufferParams,
    output: &Output,
    handle: &WorkspaceHandle,
) -> Result<bool, (FailureReason, anyhow::Error)> {
    let node = node_from_params(&params, &mut state.backend, Some(output));
    let mut _tmp_multirenderer = None;
    let renderer = match &mut state.backend {
        BackendData::Kms(kms) => {
            _tmp_multirenderer = Some(
                kms.api
                    .renderer::<Gles2Renderbuffer>(node.as_ref().unwrap(), node.as_ref().unwrap())
                    .map_err(|err| (FailureReason::Unspec, err.into()))?,
            );
            _tmp_multirenderer.as_mut().unwrap().as_mut()
        }
        BackendData::Winit(winit) => winit.backend.renderer(),
        BackendData::X11(x11) => &mut x11.renderer,
        _ => unreachable!(),
    };

    let common = &mut state.common;
    render_to_buffer::<_, _, Gles2Renderbuffer>(
        node,
        renderer,
        session,
        &params,
        output.current_transform(),
        |node, renderer, dtr, age| {
            render_workspace::<_, Gles2Renderbuffer, Dmabuf>(
                node,
                renderer,
                dtr,
                age,
                common,
                &output,
                handle,
                match session.cursor_mode() {
                    ScreencopyCursorMode::Embedded => CursorMode::All,
                    ScreencopyCursorMode::Captured(_) | ScreencopyCursorMode::None => {
                        CursorMode::None
                    }
                },
                None,
            )
        },
    )
    .map_err(|err| (FailureReason::Unspec, err.into()))
}

pub fn render_window_to_buffer(
    state: &mut State,
    session: &Session,
    params: BufferParams,
    window: &Window,
) -> Result<bool, (FailureReason, anyhow::Error)> {
    let geometry = window.geometry();

    let node = node_from_params(&params, &mut state.backend, None);
    let mut _tmp_multirenderer = None;
    let renderer = match &mut state.backend {
        BackendData::Kms(kms) => {
            _tmp_multirenderer = Some(
                kms.api
                    .renderer::<Gles2Renderbuffer>(node.as_ref().unwrap(), node.as_ref().unwrap())
                    .map_err(|err| (FailureReason::Unspec, err.into()))?,
            );
            _tmp_multirenderer.as_mut().unwrap().as_mut()
        }
        BackendData::Winit(winit) => winit.backend.renderer(),
        BackendData::X11(x11) => &mut x11.renderer,
        _ => unreachable!(),
    };

    render_to_buffer::<_, _, Gles2Renderbuffer>(
        node,
        renderer,
        session,
        &params,
        Transform::Normal,
        |_node, renderer, dtr, age| {
            // TODO cursor elements!
            let elements =
                AsRenderElements::<Gles2Renderer>::render_elements::<WaylandSurfaceRenderElement>(
                    window,
                    (-geometry.loc.x, -geometry.loc.y).into(),
                    Scale::from(1.0),
                );

            dtr.render_output(renderer, age, &elements, CLEAR_COLOR, None)
        },
    )
    .map_err(|err| (FailureReason::Unspec, err.into()))
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

impl UserdataExt for Window {
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

delegate_screencopy!(State);
