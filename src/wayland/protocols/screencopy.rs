// SPDX-License-Identifier: GPL-3.0-only

use std::{
    sync::{Arc, Mutex},
    time::Duration,
};

use cosmic_protocols::screencopy::v1::server::{
    zcosmic_screencopy_manager_v1::{self, CursorMode as WlCursorMode, ZcosmicScreencopyManagerV1},
    zcosmic_screencopy_session_v1::{
        self, BufferType, FailureReason, InputType, ZcosmicScreencopySessionV1,
    },
};
use smithay::{
    backend::{
        allocator::Fourcc as DrmFourcc,
        drm::{DrmNode, NodeType},
    },
    desktop::Window,
    input::{Seat, SeatHandler},
    output::Output,
    reexports::wayland_server::{
        protocol::{wl_buffer::WlBuffer, wl_output, wl_seat::WlSeat, wl_shm::Format as ShmFormat},
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource,
    },
    utils::{user_data::UserDataMap, Buffer, IsAlive, Physical, Point, Rectangle, Size, Transform},
};
use wayland_backend::{
    protocol::WEnum,
    server::{GlobalId, ObjectId},
};

use crate::state::State;

use super::{
    toplevel_info::window_from_handle,
    workspace::{WorkspaceHandle, WorkspaceHandler},
};

/// Screencopy global state
pub struct ScreencopyState {
    global: GlobalId,
}

pub struct ScreencopyGlobalData {
    cursor_modes: Vec<WlCursorMode>,
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
            + Dispatch<ZcosmicScreencopySessionV1, SessionData>
            + ScreencopyHandler
            + WorkspaceHandler
            + 'static,
        I: IntoIterator<Item = WlCursorMode>,
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
    _type: SessionType,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SessionType {
    Output(Output),
    Workspace(Output, WorkspaceHandle),
    Window(Window),
    Cursor(Seat<State>),
    #[doc(hidden)]
    Unknown,
}

#[derive(Debug)]
enum AuxData {
    Normal { cursor: CursorMode },
    Cursor { seat: WlSeat },
}

#[derive(Debug, Clone, PartialEq)]
pub enum CursorMode {
    Captured(Vec<CursorSession>),
    Embedded,
    None,
}

impl AuxData {
    pub fn seat(&self) -> &WlSeat {
        match self {
            AuxData::Cursor { seat } => seat,
            _ => unreachable!("Unwrapped seat from aux data"),
        }
    }

    pub fn cursor(&self) -> &CursorMode {
        match self {
            AuxData::Normal { cursor } => &cursor,
            _ => unreachable!("Unwrapped cursor from aux data"),
        }
    }
}

impl CursorMode {
    pub fn sessions<'a>(&'a self) -> impl Iterator<Item = &'a CursorSession> {
        match self {
            CursorMode::Captured(sessions) => Some(sessions.iter()).into_iter().flatten(),
            _ => None.into_iter().flatten(),
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
    obj: SessionResource,
    data: SessionData,
}

#[derive(Debug, Clone)]
enum SessionResource {
    Alive(ZcosmicScreencopySessionV1),
    Destroyed(ObjectId),
}

impl SessionResource {
    fn client(&self) -> Option<Client> {
        match self {
            SessionResource::Alive(obj) => obj.client(),
            _ => None,
        }
    }

    fn buffer_info(
        &self,
        _type: BufferType,
        node: Option<String>,
        format: u32,
        width: u32,
        height: u32,
        stride: u32,
    ) {
        if let SessionResource::Alive(obj) = self {
            obj.buffer_info(_type, node, format, width, height, stride)
        }
    }

    fn init_done(&self) {
        if let SessionResource::Alive(obj) = self {
            obj.init_done()
        }
    }

    fn transform(&self, transform: wl_output::Transform) {
        if let SessionResource::Alive(obj) = self {
            obj.transform(transform)
        }
    }

    fn damage(&self, x: u32, y: u32, w: u32, h: u32) {
        if let SessionResource::Alive(obj) = self {
            obj.damage(x, y, w, h)
        }
    }

    fn commit_time(&self, time_sec_hi: u32, time_sec_lo: u32, time_nsec: u32) {
        if let SessionResource::Alive(obj) = self {
            obj.commit_time(time_sec_hi, time_sec_lo, time_nsec)
        }
    }

    fn ready(&self) {
        if let SessionResource::Alive(obj) = self {
            obj.ready()
        }
    }

    fn failed(&self, reason: FailureReason) {
        if let SessionResource::Alive(obj) = self {
            obj.failed(reason)
        }
    }

    fn cursor_enter(&self, wl_seat: &WlSeat, input_type: InputType) {
        if let SessionResource::Alive(obj) = self {
            obj.cursor_enter(wl_seat, input_type)
        }
    }

    fn cursor_info(
        &self,
        wl_seat: &WlSeat,
        input_type: InputType,
        x: i32,
        y: i32,
        w: i32,
        h: i32,
        dx: i32,
        dy: i32,
    ) {
        if let SessionResource::Alive(obj) = self {
            obj.cursor_info(wl_seat, input_type, x, y, w, h, dx, dy)
        }
    }

    fn cursor_leave(&self, wl_seat: &WlSeat, input_type: InputType) {
        if let SessionResource::Alive(obj) = self {
            obj.cursor_leave(wl_seat, input_type)
        }
    }
}

impl PartialEq for SessionResource {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (SessionResource::Alive(obj1), SessionResource::Alive(obj2)) => obj1 == obj2,
            (SessionResource::Alive(obj), SessionResource::Destroyed(id))
            | (SessionResource::Destroyed(id), SessionResource::Alive(obj)) => obj.id() == *id,
            (SessionResource::Destroyed(id1), SessionResource::Destroyed(id2)) => id1 == id2,
        }
    }
}

impl PartialEq for Session {
    fn eq(&self, other: &Self) -> bool {
        self.obj == other.obj
    }
}

// TODO: Handle Alive

// TODO: Better errors

impl Session {
    pub fn cursor_enter<D: SeatHandler + 'static>(&self, seat: &Seat<D>, input_type: InputType) {
        if !self.alive() {
            return;
        }
        if let Some(client) = self.obj.client() {
            for wl_seat in seat.client_seats(&client) {
                self.obj.cursor_enter(&wl_seat, input_type)
            }
        }
    }

    pub fn cursor_info<D: SeatHandler + 'static>(
        &self,
        seat: &Seat<D>,
        input_type: InputType,
        geometry: Rectangle<i32, Buffer>,
        offset: Point<i32, Buffer>,
    ) {
        if !self.alive() {
            return;
        }
        if let Some(client) = self.obj.client() {
            for wl_seat in seat.client_seats(&client) {
                self.obj.cursor_info(
                    &wl_seat,
                    input_type,
                    geometry.loc.x,
                    geometry.loc.y,
                    geometry.size.w,
                    geometry.size.h,
                    offset.x,
                    offset.y,
                );
                let data = self.data.inner.lock().unwrap();
                for cursor_session in data.aux.cursor().sessions() {
                    cursor_session.obj.cursor_info(
                        &wl_seat,
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
        }
    }

    pub fn cursor_leave<D: SeatHandler + 'static>(&self, seat: &Seat<D>, input_type: InputType) {
        if !self.alive() {
            return;
        }
        if let Some(client) = self.obj.client() {
            for wl_seat in seat.client_seats(&client) {
                self.obj.cursor_leave(&wl_seat, input_type)
            }
        }
    }

    pub fn cursor_sessions(&self) -> impl Iterator<Item = CursorSession> {
        if !self.alive() {
            return Vec::new().into_iter();
        }
        self.data
            .inner
            .lock()
            .unwrap()
            .aux
            .cursor()
            .sessions()
            .cloned()
            .collect::<Vec<_>>()
            .into_iter()
    }

    pub fn commit_buffer(
        &self,
        transform: Transform,
        damage: Vec<Rectangle<i32, Physical>>,
        time: Option<Duration>,
    ) {
        if !self.alive() {
            return;
        }
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
        if !self.alive() {
            return;
        }
        self.obj.failed(reason);
        self.data.inner.lock().unwrap().gone = true;
    }

    pub fn user_data(&self) -> &UserDataMap {
        &self.data.user_data
    }

    pub fn session_type(&self) -> SessionType {
        self.data.inner.lock().unwrap()._type.clone()
    }

    pub fn cursor_mode(&self) -> CursorMode {
        self.data.inner.lock().unwrap().aux.cursor().clone()
    }
}

impl IsAlive for Session {
    fn alive(&self) -> bool {
        !self.data.inner.lock().unwrap().gone
    }
}

#[derive(Debug, Clone)]
pub struct CursorSession {
    obj: SessionResource,
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

impl IsAlive for CursorSession {
    fn alive(&self) -> bool {
        !self.data.inner.lock().unwrap().gone
    }
}

#[derive(Debug, Clone)]
pub struct BufferParams {
    pub buffer: WlBuffer,
    pub node: Option<DrmNode>,
    pub age: u32,
}

pub trait ScreencopyHandler {
    fn capture_output(&mut self, output: Output, session: Session) -> Vec<BufferInfo>;

    fn capture_workspace(
        &mut self,
        workspace: WorkspaceHandle,
        output: Output,
        session: Session,
    ) -> Vec<BufferInfo>;

    fn capture_toplevel(&mut self, toplevel: Window, session: Session) -> Vec<BufferInfo>;

    fn capture_cursor(&mut self, session: CursorSession) -> Vec<BufferInfo>;

    fn buffer_attached(&mut self, session: Session, buffer: BufferParams, on_damage: bool);

    fn cursor_session_destroyed(&mut self, session: CursorSession) {
        let _ = session;
    }

    fn session_destroyed(&mut self, session: Session) {
        let _ = session;
    }
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
    cursor: WEnum<WlCursorMode>,
    _type: SessionType,
) -> Option<Session>
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
                cursor: match cursor.into_result() {
                    Ok(WlCursorMode::Capture) => CursorMode::Captured(Vec::new()),
                    Ok(WlCursorMode::Embedded) => CursorMode::Embedded,
                    _ => CursorMode::None,
                },
            },
            _type,
        }),
        user_data: UserDataMap::new(),
    });
    let session = data_init.init(session, data.clone());

    if let Err(err) = cursor.into_result() {
        slog_scope::warn!("Client did send unknown cursor mode: {}", err);
        session.post_error(
            zcosmic_screencopy_session_v1::Error::InvalidCursorMode,
            "Unknown cursor mode, wrong protocol version?",
        );
        return None;
    };
    let session = Session {
        obj: SessionResource::Alive(session),
        data,
    };

    Some(session)
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
            } => match Output::from_resource(&output) {
                Some(output) => {
                    let session = match init_session(
                        data_init,
                        session,
                        cursor,
                        SessionType::Output(output.clone()),
                    ) {
                        Some(result) => result,
                        None => {
                            return;
                        }
                    };
                    let formats = state.capture_output(output, session.clone());
                    if !session.data.inner.lock().unwrap().gone {
                        send_formats(&session.obj, formats);
                    }
                }
                None => {
                    let session =
                        match init_session(data_init, session, cursor, SessionType::Unknown) {
                            Some(result) => result,
                            None => {
                                return;
                            }
                        };
                    session.failed(FailureReason::InvalidOutput);
                    return;
                }
            },
            zcosmic_screencopy_manager_v1::Request::CaptureToplevel {
                session,
                toplevel,
                cursor,
            } => match window_from_handle(toplevel) {
                Some(window) => {
                    let session = match init_session(
                        data_init,
                        session,
                        cursor,
                        SessionType::Window(window.clone()),
                    ) {
                        Some(result) => result,
                        None => {
                            return;
                        }
                    };

                    let formats = state.capture_toplevel(window, session.clone());
                    if !session.data.inner.lock().unwrap().gone {
                        send_formats(&session.obj, formats);
                    }
                }
                None => {
                    let session =
                        match init_session(data_init, session, cursor, SessionType::Unknown) {
                            Some(result) => result,
                            None => {
                                return;
                            }
                        };
                    session.obj.failed(FailureReason::InvalidToplevel);
                    return;
                }
            },
            zcosmic_screencopy_manager_v1::Request::CaptureWorkspace {
                session,
                workspace,
                output,
                cursor,
            } => match Output::from_resource(&output) {
                Some(output) => match state.workspace_state().workspace_handle(&workspace) {
                    Some(handle) => {
                        let session = match init_session(
                            data_init,
                            session,
                            cursor,
                            SessionType::Workspace(output.clone(), handle.clone()),
                        ) {
                            Some(result) => result,
                            None => {
                                return;
                            }
                        };
                        let formats = state.capture_workspace(handle, output, session.clone());
                        if !session.data.inner.lock().unwrap().gone {
                            send_formats(&session.obj, formats);
                        }
                    }
                    None => {
                        let session =
                            match init_session(data_init, session, cursor, SessionType::Unknown) {
                                Some(result) => result,
                                None => {
                                    return;
                                }
                            };
                        session.failed(FailureReason::InvalidWorkspace);
                        return;
                    }
                },
                None => {
                    let session =
                        match init_session(data_init, session, cursor, SessionType::Unknown) {
                            Some(result) => result,
                            None => {
                                return;
                            }
                        };
                    session.failed(FailureReason::InvalidOutput);
                    return;
                }
            },
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
        + WorkspaceHandler
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
            zcosmic_screencopy_session_v1::Request::CaptureCursor {
                session,
                seat: wl_seat,
            } => match Seat::from_resource(&wl_seat) {
                Some(seat) => {
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
                            aux: AuxData::Cursor { seat: wl_seat },
                            _type: SessionType::Cursor(seat),
                        }),
                        user_data: UserDataMap::new(),
                    });
                    let session = data_init.init(session, data.clone());

                    let cursor_session = CursorSession {
                        obj: SessionResource::Alive(session),
                        data,
                    };
                    let formats = state.capture_cursor(cursor_session.clone());
                    if !cursor_session.data.inner.lock().unwrap().gone {
                        send_formats(&cursor_session.obj, formats);
                    }
                }
                None => {
                    let session = match init_session(
                        data_init,
                        session,
                        WEnum::Value(WlCursorMode::Capture),
                        SessionType::Unknown,
                    ) {
                        Some(result) => result,
                        None => {
                            return;
                        }
                    };
                    session.failed(FailureReason::InvalidSeat);
                    return;
                }
            },
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
                let buffer = {
                    let mut resource_data = data.inner.lock().unwrap();
                    if resource_data.is_cursor() || resource_data.gone {
                        resource.failed(FailureReason::Unspec);
                        return;
                    }
                    resource_data.pending_buffer.take()
                };

                if let Some(buffer) = buffer {
                    let session = Session {
                        obj: SessionResource::Alive(resource.clone()),
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

    fn destroyed(
        state: &mut D,
        _client: wayland_backend::server::ClientId,
        resource: wayland_backend::server::ObjectId,
        data: &SessionData,
    ) {
        if data.inner.lock().unwrap().is_cursor() {
            let session = CursorSession {
                obj: SessionResource::Destroyed(resource),
                data: data.clone(),
            };
            state.cursor_session_destroyed(session)
        } else {
            let session = Session {
                obj: SessionResource::Destroyed(resource),
                data: data.clone(),
            };
            state.session_destroyed(session)
        }
    }
}

fn send_formats(session: &SessionResource, formats: Vec<BufferInfo>) {
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

macro_rules! delegate_screencopy {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::screencopy::v1::server::zcosmic_screencopy_manager_v1::ZcosmicScreencopyManagerV1: $crate::wayland::protocols::screencopy::ScreencopyGlobalData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::screencopy::v1::server::zcosmic_screencopy_manager_v1::ZcosmicScreencopyManagerV1: ()
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::screencopy::v1::server::zcosmic_screencopy_session_v1::ZcosmicScreencopySessionV1: $crate::wayland::protocols::screencopy::SessionData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
    };
}
pub(crate) use delegate_screencopy;
