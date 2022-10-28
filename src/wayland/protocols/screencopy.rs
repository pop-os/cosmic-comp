// SPDX-License-Identifier: GPL-3.0-only

use std::{
    sync::{Arc, Mutex},
    time::Duration,
};

use cosmic_protocols::screencopy::v1::server::{
    zcosmic_screencopy_manager_v1::{self, CursorMode, ZcosmicScreencopyManagerV1},
    zcosmic_screencopy_session_v1::{self, FailureReason, InputType, ZcosmicScreencopySessionV1},
};
use smithay::{
    backend::{
        allocator::Fourcc as DrmFourcc,
        drm::{DrmNode, NodeType},
    },
    desktop::Window,
    reexports::wayland_server::{
        protocol::{
            wl_buffer::WlBuffer, wl_output::WlOutput, wl_seat::WlSeat, wl_shm::Format as ShmFormat,
        },
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource,
    },
    utils::{user_data::UserDataMap, Buffer, Point, Rectangle, Size, Transform},
};
use wayland_backend::{protocol::WEnum, server::GlobalId};

use super::{
    toplevel_info::window_from_handle,
    workspace::{WorkspaceHandle, WorkspaceHandler},
};

/// Screencopy global state
pub struct ScreencopyState {
    global: GlobalId,
}

pub struct ScreencopyGlobalData {
    cursor_modes: Vec<CursorMode>,
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

impl ScreencopyState {
    /// Create a new screencopy global
    pub fn new<D, I, F>(
        display: &DisplayHandle,
        cursor_modes: I,
        client_filter: F,
    ) -> ScreencopyState
    where
        D: GlobalDispatch<ZcosmicScreencopyManagerV1, ScreencopyGlobalData>
            + Dispatch<ZcosmicScreencopyManagerV1, ()>
            + Dispatch<ZcosmicScreencopySessionV1, ()>
            + ScreencopyHandler
            + WorkspaceHandler
            + 'static,
        I: IntoIterator<Item = CursorMode>,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        ScreencopyState {
            global: display.create_global::<D, ZcosmicScreencopyManagerV1, _>(
                1,
                ScreencopyGlobalData {
                    cursor_modes: Vec::from_iter(cursor_modes),
                    filter: Box::new(client_filter),
                },
            ),
        }
    }

    /// Returns the screencopy global id
    pub fn global(&self) -> GlobalId {
        self.global.clone()
    }
}

#[derive(Debug)]
pub enum BufferInfo {
    Shm {
        format: ShmFormat,
        size: Size<i32, Buffer>,
        stride: u32,
    },
    Dmabuf {
        node: DrmNode,
        format: DrmFourcc,
        size: Size<i32, Buffer>,
    },
}

#[derive(Debug)]
pub struct SessionDataInnerInner {
    gone: bool,
    pending_buffer: Option<BufferParams>,
    aux: AuxData,
}

impl SessionDataInnerInner {
    pub fn is_cursor(&self) -> bool {
        match self.aux {
            AuxData::Cursor { .. } => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
enum AuxData {
    Normal { cursor_sessions: Vec<CursorSession> },
    Cursor { seat: WlSeat },
}

impl AuxData {
    pub fn seat(&self) -> &WlSeat {
        match self {
            AuxData::Cursor { seat } => seat,
            _ => unreachable!("Unwrapped seat from aux data"),
        }
    }

    pub fn cursor_sessions(&self) -> &[CursorSession] {
        match self {
            AuxData::Normal { cursor_sessions } => &cursor_sessions,
            _ => unreachable!("Unwrapped cursor_session from aux data"),
        }
    }
}

#[derive(Debug)]
pub struct SessionDataInner {
    inner: Mutex<SessionDataInnerInner>,
    user_data: UserDataMap,
}

pub type SessionData = Arc<SessionDataInner>;

#[derive(Debug, Clone)]
pub struct Session {
    obj: ZcosmicScreencopySessionV1,
    data: SessionData,
}

impl PartialEq for Session {
    fn eq(&self, other: &Self) -> bool {
        self.obj == other.obj
    }
}

impl Session {
    pub fn cursor_enter(&self, seat: &WlSeat, input_type: InputType) {
        self.obj.cursor_enter(seat, input_type)
    }

    pub fn cursor_info(
        &self,
        seat: &WlSeat,
        input_type: InputType,
        geometry: Rectangle<i32, Buffer>,
        offset: Point<i32, Buffer>,
    ) {
        self.obj.cursor_info(
            seat,
            input_type,
            geometry.loc.x,
            geometry.loc.y,
            geometry.size.w,
            geometry.size.h,
            offset.x,
            offset.y,
        );
        let data = self.data.inner.lock().unwrap();
        for cursor_session in data.aux.cursor_sessions() {
            cursor_session.obj.cursor_info(
                seat,
                input_type,
                geometry.loc.x,
                geometry.loc.y,
                geometry.size.w,
                geometry.size.h,
                offset.x,
                offset.y,
            );
        }
    }

    pub fn cursor_leave(&self, seat: &WlSeat, input_type: InputType) {
        self.obj.cursor_leave(seat, input_type)
    }

    pub fn cursor_sessions(&self) -> impl Iterator<Item = CursorSession> {
        self.data
            .inner
            .lock()
            .unwrap()
            .aux
            .cursor_sessions()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .into_iter()
    }

    pub fn commit_buffer(
        &self,
        transform: Transform,
        damage: Vec<Rectangle<i32, Buffer>>,
        time: Option<Duration>,
    ) {
        self.obj.transform(transform.into());
        for rect in damage {
            self.obj.damage(
                rect.loc.x as u32,
                rect.loc.y as u32,
                rect.size.w as u32,
                rect.size.h as u32,
            );
        }
        if let Some(time) = time {
            let tv_sec_hi = (time.as_secs() >> 32) as u32;
            let tv_sec_lo = (time.as_secs() & 0xFFFFFFFF) as u32;
            self.obj
                .commit_time(tv_sec_hi, tv_sec_lo, time.subsec_nanos());
        }
        self.obj.ready()
    }

    pub fn failed(&self, reason: FailureReason) {
        self.obj.failed(reason);
        self.data.inner.lock().unwrap().gone = true;
    }

    pub fn user_data(&self) -> &UserDataMap {
        &self.data.user_data
    }
}

#[derive(Debug, Clone)]
pub struct CursorSession {
    obj: ZcosmicScreencopySessionV1,
    data: SessionData,
}

impl PartialEq for CursorSession {
    fn eq(&self, other: &Self) -> bool {
        self.obj == other.obj
    }
}

impl CursorSession {
    pub fn seat(&self) -> WlSeat {
        self.data.inner.lock().unwrap().aux.seat().clone()
    }

    pub fn buffer_waiting(&self) -> Option<BufferParams> {
        self.data.inner.lock().unwrap().pending_buffer.take()
    }

    pub fn commit_buffer<'a>(
        &self,
        transform: Transform,
        damage: impl Iterator<Item = &'a Rectangle<i32, Buffer>> + 'a,
    ) {
        self.obj.transform(transform.into());
        for rect in damage {
            self.obj.damage(
                rect.loc.x as u32,
                rect.loc.y as u32,
                rect.size.w as u32,
                rect.size.h as u32,
            );
        }
    }

    pub fn failed(&self, reason: FailureReason) {
        self.obj.failed(reason);
        self.data.inner.lock().unwrap().gone = true;
    }

    pub fn user_data(&self) -> &UserDataMap {
        &self.data.user_data
    }
}

#[derive(Debug)]
pub struct BufferParams {
    pub buffer: WlBuffer,
    pub node: Option<DrmNode>,
    pub age: u32,
}

pub trait ScreencopyHandler {
    fn capture_output(
        &mut self,
        output: WlOutput,
        cursor: CursorMode,
        session: Session,
    ) -> Vec<BufferInfo>;

    fn capture_workspace(
        &mut self,
        workspace: WorkspaceHandle,
        output: WlOutput,
        cursor: CursorMode,
        session: Session,
    ) -> Vec<BufferInfo>;

    fn capture_toplevel(
        &mut self,
        toplevel: Window,
        cursor: CursorMode,
        session: Session,
    ) -> Vec<BufferInfo>;

    fn capture_cursor(&mut self, session: CursorSession) -> Vec<BufferInfo>;

    fn buffer_attached(&mut self, session: Session, buffer: BufferParams, on_damage: bool);

    fn session_destroyed(&mut self, session: Session);
}

impl<D> GlobalDispatch<ZcosmicScreencopyManagerV1, ScreencopyGlobalData, D> for ScreencopyState
where
    D: GlobalDispatch<ZcosmicScreencopyManagerV1, ScreencopyGlobalData>
        + Dispatch<ZcosmicScreencopyManagerV1, ()>
        + Dispatch<ZcosmicScreencopySessionV1, SessionData>
        + ScreencopyHandler
        + WorkspaceHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<ZcosmicScreencopyManagerV1>,
        global_data: &ScreencopyGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        let global = data_init.init(resource, ());
        for mode in &global_data.cursor_modes {
            global.supported_cursor_mode(*mode);
        }
    }

    fn can_view(client: Client, global_data: &ScreencopyGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

fn init_session<D>(
    data_init: &mut DataInit<'_, D>,
    session: New<ZcosmicScreencopySessionV1>,
    cursor: WEnum<CursorMode>,
) -> Option<(Session, CursorMode)>
where
    D: GlobalDispatch<ZcosmicScreencopyManagerV1, ScreencopyGlobalData>
        + Dispatch<ZcosmicScreencopyManagerV1, ()>
        + Dispatch<ZcosmicScreencopySessionV1, SessionData>
        + ScreencopyHandler
        + WorkspaceHandler
        + 'static,
{
    let data = Arc::new(SessionDataInner {
        inner: Mutex::new(SessionDataInnerInner {
            gone: false,
            pending_buffer: None,
            aux: AuxData::Normal {
                cursor_sessions: Vec::new(),
            },
        }),
        user_data: UserDataMap::new(),
    });
    let session = data_init.init(session, data.clone());

    let cursor_mode = match cursor.into_result() {
        Ok(mode) => mode,
        Err(err) => {
            slog_scope::warn!("Client did send unknown cursor mode: {}", err);
            session.failed(FailureReason::UnknownInput);
            return None;
        }
    };

    let session = Session { obj: session, data };

    Some((session, cursor_mode))
}

impl<D> Dispatch<ZcosmicScreencopyManagerV1, (), D> for ScreencopyState
where
    D: GlobalDispatch<ZcosmicScreencopyManagerV1, ScreencopyGlobalData>
        + Dispatch<ZcosmicScreencopyManagerV1, ()>
        + Dispatch<ZcosmicScreencopySessionV1, SessionData>
        + ScreencopyHandler
        + WorkspaceHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &ZcosmicScreencopyManagerV1,
        request: <ZcosmicScreencopyManagerV1 as smithay::reexports::wayland_server::Resource>::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_screencopy_manager_v1::Request::CaptureOutput {
                session,
                output,
                cursor,
            } => {
                let (session, cursor_mode) = match init_session(data_init, session, cursor) {
                    Some(result) => result,
                    None => {
                        return;
                    }
                };
                let formats = state.capture_output(output, cursor_mode, session.clone());
                if !session.data.inner.lock().unwrap().gone {
                    send_formats(&session.obj, formats);
                }
            }
            zcosmic_screencopy_manager_v1::Request::CaptureToplevel {
                session,
                toplevel,
                cursor,
            } => {
                let (session, cursor_mode) = match init_session(data_init, session, cursor) {
                    Some(result) => result,
                    None => {
                        return;
                    }
                };
                match window_from_handle(toplevel) {
                    Some(window) => {
                        let formats = state.capture_toplevel(window, cursor_mode, session.clone());
                        if !session.data.inner.lock().unwrap().gone {
                            send_formats(&session.obj, formats);
                        }
                    }
                    None => {
                        session.obj.failed(FailureReason::ToplevelDestroyed);
                        return;
                    }
                }
            }
            zcosmic_screencopy_manager_v1::Request::CaptureWorkspace {
                session,
                workspace,
                output,
                cursor,
            } => {
                let (session, cursor_mode) = match init_session(data_init, session, cursor) {
                    Some(result) => result,
                    None => {
                        return;
                    }
                };
                match state.workspace_state().workspace_handle(&workspace) {
                    Some(handle) => {
                        let formats =
                            state.capture_workspace(handle, output, cursor_mode, session.clone());
                        if !session.data.inner.lock().unwrap().gone {
                            send_formats(&session.obj, formats);
                        }
                    }
                    None => {
                        session.failed(FailureReason::InvalidOutput);
                        return;
                    }
                }
            }
            _ => {}
        }
    }
}

impl<D> Dispatch<ZcosmicScreencopySessionV1, SessionData, D> for ScreencopyState
where
    D: GlobalDispatch<ZcosmicScreencopyManagerV1, ScreencopyGlobalData>
        + Dispatch<ZcosmicScreencopyManagerV1, ()>
        + Dispatch<ZcosmicScreencopySessionV1, SessionData>
        + ScreencopyHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &ZcosmicScreencopySessionV1,
        request: <ZcosmicScreencopySessionV1 as Resource>::Request,
        data: &SessionData,
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_screencopy_session_v1::Request::CaptureCursor { session, seat } => {
                {
                    let resource_data = data.inner.lock().unwrap();
                    if resource_data.is_cursor() || resource_data.gone {
                        resource.failed(FailureReason::Unspec);
                        return;
                    }
                }

                let data = Arc::new(SessionDataInner {
                    inner: Mutex::new(SessionDataInnerInner {
                        gone: false,
                        pending_buffer: None,
                        aux: AuxData::Cursor { seat },
                    }),
                    user_data: UserDataMap::new(),
                });
                let session = data_init.init(session, data.clone());

                let cursor_session = CursorSession { obj: session, data };
                let formats = state.capture_cursor(cursor_session.clone());
                if !cursor_session.data.inner.lock().unwrap().gone {
                    send_formats(&cursor_session.obj, formats);
                }
            }
            zcosmic_screencopy_session_v1::Request::AttachBuffer { buffer, node, age } => {
                if data.inner.lock().unwrap().gone {
                    resource.failed(FailureReason::Unspec);
                    return;
                }
                let params = BufferParams {
                    buffer,
                    node: node.and_then(|p| DrmNode::from_path(p).ok()),
                    age,
                };
                data.inner.lock().unwrap().pending_buffer = Some(params);
            }
            zcosmic_screencopy_session_v1::Request::Commit { options } => {
                {
                    let resource_data = data.inner.lock().unwrap();
                    if resource_data.is_cursor() || resource_data.gone {
                        resource.failed(FailureReason::Unspec);
                        return;
                    }
                }

                if let Some(buffer) = data.inner.lock().unwrap().pending_buffer.take() {
                    let session = Session {
                        obj: resource.clone(),
                        data: data.clone(),
                    };
                    state.buffer_attached(
                        session,
                        buffer,
                        options
                            .into_result()
                            .ok()
                            .map(|v| v.contains(zcosmic_screencopy_session_v1::Options::OnDamage))
                            .unwrap_or(false),
                    );
                } else {
                    resource.failed(FailureReason::InvalidBuffer);
                }
            }
            zcosmic_screencopy_session_v1::Request::Destroy => {
                data.inner.lock().unwrap().gone = true;
            }
            _ => {}
        }
    }
}

fn send_formats(session: &ZcosmicScreencopySessionV1, formats: Vec<BufferInfo>) {
    for format in formats {
        match format {
            BufferInfo::Dmabuf { node, format, size } => {
                if let Some(node_path) = node
                    .dev_path_with_type(NodeType::Render)
                    .or_else(|| node.dev_path())
                {
                    session.buffer_info(
                        zcosmic_screencopy_session_v1::BufferType::Dmabuf,
                        Some(node_path.as_os_str().to_string_lossy().into_owned()),
                        format as u32,
                        size.w as u32,
                        size.h as u32,
                        0,
                    );
                }
            }
            BufferInfo::Shm {
                format,
                size,
                stride,
            } => session.buffer_info(
                zcosmic_screencopy_session_v1::BufferType::WlShm,
                None,
                format as u32,
                size.w as u32,
                size.h as u32,
                stride,
            ),
        }
    }

    session.init_done();
}