use std::{
    sync::{Arc, Mutex},
    time::Duration,
};

pub use cosmic_protocols::screencopy::v2::server::zcosmic_screencopy_frame_v2::FailureReason;
use cosmic_protocols::screencopy::v2::server::{
    zcosmic_screencopy_cursor_session_v2::{self, ZcosmicScreencopyCursorSessionV2},
    zcosmic_screencopy_frame_v2::{self, ZcosmicScreencopyFrameV2},
    zcosmic_screencopy_manager_v2::{self, ZcosmicScreencopyManagerV2},
    zcosmic_screencopy_session_v2::{self, ZcosmicScreencopySessionV2},
};
use smithay::{
    backend::{
        allocator::{Buffer, Fourcc, Modifier},
        drm::DrmNode,
        renderer::{buffer_type, BufferType},
    },
    utils::{user_data::UserDataMap, Buffer as BufferCoords, IsAlive, Size, Transform},
    wayland::{dmabuf::get_dmabuf, shm::with_buffer_contents},
};
use smithay::{reexports::wayland_server::protocol::wl_buffer::WlBuffer, utils::Point};
use smithay::{
    reexports::wayland_server::{
        protocol::wl_shm, Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource,
    },
    utils::Rectangle,
};
use tracing::debug;
use wayland_backend::server::GlobalId;

use super::image_source::ImageSourceData;

#[derive(Debug)]
pub struct ScreencopyState {
    global: GlobalId,
    known_sessions: Vec<Session>,
    known_cursor_sessions: Vec<CursorSession>,
}

impl ScreencopyState {
    pub fn new<D, F>(display: &DisplayHandle, client_filter: F) -> ScreencopyState
    where
        D: GlobalDispatch<ZcosmicScreencopyManagerV2, ScreencopyGlobalData>
            + Dispatch<ZcosmicScreencopyManagerV2, ScreencopyData>
            + Dispatch<ZcosmicScreencopySessionV2, SessionData>
            + Dispatch<ZcosmicScreencopySessionV2, CursorSessionData>
            + Dispatch<ZcosmicScreencopyCursorSessionV2, CursorSessionData>
            + Dispatch<ZcosmicScreencopyFrameV2, FrameData>
            + ScreencopyHandler
            + 'static,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        ScreencopyState {
            global: display.create_global::<D, ZcosmicScreencopyManagerV2, _>(
                1,
                ScreencopyGlobalData {
                    filter: Box::new(client_filter),
                },
            ),
            known_sessions: Vec::new(),
            known_cursor_sessions: Vec::new(),
        }
    }

    pub fn global_id(&self) -> &GlobalId {
        &self.global
    }
}

#[derive(Debug, Clone)]
pub struct BufferConstraints {
    pub size: Size<i32, BufferCoords>,
    pub shm: Vec<wl_shm::Format>,
    pub dma: Option<DmabufConstraints>,
}

#[derive(Debug, Clone)]
pub struct DmabufConstraints {
    pub node: DrmNode,
    pub formats: Vec<(Fourcc, Vec<Modifier>)>,
}

#[derive(Debug, Clone)]
pub struct Session {
    obj: ZcosmicScreencopySessionV2,
    inner: Arc<Mutex<SessionInner>>,
    user_data: Arc<UserDataMap>,
}

impl PartialEq for Session {
    fn eq(&self, other: &Self) -> bool {
        self.obj == other.obj
    }
}

#[derive(Debug)]
struct SessionInner {
    stopped: bool,
    constraints: Option<BufferConstraints>,
    draw_cursors: bool,
    source: ImageSourceData,
    active_frames: Vec<Frame>,
}

impl SessionInner {
    fn new(source: ImageSourceData, draw_cursors: bool) -> SessionInner {
        SessionInner {
            stopped: false,
            constraints: None,
            draw_cursors,
            source,
            active_frames: Vec::new(),
        }
    }
}

impl IsAlive for Session {
    fn alive(&self) -> bool {
        self.obj.is_alive()
    }
}

impl Session {
    pub fn update_constraints(&self, constraints: BufferConstraints) {
        let mut inner = self.inner.lock().unwrap();

        if !self.obj.is_alive() || inner.stopped {
            return;
        }

        self.obj
            .buffer_size(constraints.size.w as u32, constraints.size.h as u32);
        for fmt in &constraints.shm {
            self.obj.shm_format(*fmt as u32);
        }
        if let Some(dma) = constraints.dma.as_ref() {
            let node = Vec::from(dma.node.dev_id().to_ne_bytes());
            self.obj.dmabuf_device(node);
            for (fmt, modifiers) in &dma.formats {
                let mut modifiers = modifiers.clone();
                let modifiers: Vec<u8> = {
                    let ptr = modifiers.as_mut_ptr() as *mut u8;
                    let len = modifiers.len() * 4;
                    let cap = modifiers.capacity() * 4;
                    std::mem::forget(modifiers);
                    unsafe { Vec::from_raw_parts(ptr, len, cap) }
                };
                self.obj.dmabuf_format(*fmt as u32, modifiers);
            }
        }
        self.obj.done();

        inner.constraints = Some(constraints);
    }

    pub fn current_constraints(&self) -> Option<BufferConstraints> {
        self.inner.lock().unwrap().constraints.clone()
    }

    pub fn source(&self) -> ImageSourceData {
        self.inner.lock().unwrap().source.clone()
    }

    pub fn draw_cursor(&self) -> bool {
        self.inner.lock().unwrap().draw_cursors
    }

    pub fn user_data(&self) -> &UserDataMap {
        &*self.user_data
    }

    pub fn stop(self) {
        let mut inner = self.inner.lock().unwrap();

        if !self.obj.is_alive() || inner.stopped {
            return;
        }

        for frame in inner.active_frames.drain(..) {
            let mut inner = frame.inner.lock().unwrap();
            if inner.failed.replace(FailureReason::Stopped).is_none() && inner.capture_requested {
                frame.obj.failed(FailureReason::Stopped);
            }
        }

        self.obj.stopped();
        inner.constraints.take();
        inner.stopped = true;
    }
}

#[derive(Debug, Clone)]
pub struct CursorSession {
    obj: ZcosmicScreencopyCursorSessionV2,
    inner: Arc<Mutex<CursorSessionInner>>,
    user_data: Arc<UserDataMap>,
}

impl PartialEq for CursorSession {
    fn eq(&self, other: &Self) -> bool {
        self.obj == other.obj
    }
}

#[derive(Debug)]
struct CursorSessionInner {
    session: Option<ZcosmicScreencopySessionV2>,
    stopped: bool,
    constraints: Option<BufferConstraints>,
    source: ImageSourceData,
    position: Option<Point<i32, BufferCoords>>,
    hotspot: Point<i32, BufferCoords>,
    active_frames: Vec<Frame>,
}

impl CursorSessionInner {
    fn new(source: ImageSourceData) -> CursorSessionInner {
        CursorSessionInner {
            session: None,
            stopped: false,
            constraints: None,
            source,
            position: None,
            hotspot: Point::from((0, 0)),
            active_frames: Vec::new(),
        }
    }
}

impl IsAlive for CursorSession {
    fn alive(&self) -> bool {
        self.obj.is_alive()
    }
}

impl CursorSession {
    pub fn update_constraints(&self, constrains: BufferConstraints) {
        let mut inner = self.inner.lock().unwrap();

        if !self.obj.is_alive() || inner.stopped {
            return;
        }

        if let Some(session_obj) = inner.session.as_ref() {
            session_obj.buffer_size(constrains.size.w as u32, constrains.size.h as u32);
            for fmt in &constrains.shm {
                session_obj.shm_format(*fmt as u32);
            }
            if let Some(dma) = constrains.dma.as_ref() {
                let node = Vec::from(dma.node.dev_id().to_ne_bytes());
                session_obj.dmabuf_device(node);
                for (fmt, modifiers) in &dma.formats {
                    let mut modifiers = modifiers.clone();
                    let modifiers: Vec<u8> = {
                        let ptr = modifiers.as_mut_ptr() as *mut u8;
                        let len = modifiers.len() * 4;
                        let cap = modifiers.capacity() * 4;
                        std::mem::forget(modifiers);
                        unsafe { Vec::from_raw_parts(ptr, len, cap) }
                    };
                    session_obj.dmabuf_format(*fmt as u32, modifiers);
                }
            }
            session_obj.done();
        }

        inner.constraints = Some(constrains);
    }

    pub fn current_constraints(&self) -> Option<BufferConstraints> {
        self.inner.lock().unwrap().constraints.clone()
    }

    pub fn source(&self) -> ImageSourceData {
        self.inner.lock().unwrap().source.clone()
    }

    pub fn has_cursor(&self) -> bool {
        self.inner.lock().unwrap().position.is_some()
    }

    pub fn set_cursor_pos(&self, position: Option<Point<i32, BufferCoords>>) {
        if !self.obj.is_alive() {
            return;
        }

        let mut inner = self.inner.lock().unwrap();

        if inner.position == position {
            return;
        }

        if inner.position.is_none() {
            self.obj.enter();
            self.obj.hotspot(inner.hotspot.x, inner.hotspot.y)
        }

        if let Some(new_pos) = position {
            self.obj.position(new_pos.x, new_pos.y);
        } else {
            self.obj.leave()
        }

        inner.position = position;
    }

    pub fn set_cursor_hotspot(&self, hotspot: impl Into<Point<i32, BufferCoords>>) {
        if !self.obj.is_alive() {
            return;
        }

        let hotspot = hotspot.into();

        let mut inner = self.inner.lock().unwrap();

        if inner.hotspot == hotspot {
            return;
        }

        inner.hotspot = hotspot;
        if inner.position.is_some() {
            self.obj.hotspot(hotspot.x, hotspot.y);
        }
    }

    pub fn user_data(&self) -> &UserDataMap {
        &*self.user_data
    }

    pub fn stop(self) {
        let mut inner = self.inner.lock().unwrap();

        if !self.obj.is_alive() || inner.stopped {
            return;
        }

        if let Some(session_obj) = inner.session.as_ref() {
            session_obj.stopped();
        }
        inner.constraints.take();

        for frame in inner.active_frames.drain(..) {
            let mut inner = frame.inner.lock().unwrap();
            if inner.failed.replace(FailureReason::Stopped).is_none() && inner.capture_requested {
                frame.obj.failed(FailureReason::Stopped);
            }
        }

        inner.stopped = true;
    }
}

#[derive(Debug)]
pub struct Frame {
    obj: ZcosmicScreencopyFrameV2,
    inner: Arc<Mutex<FrameInner>>,
}

impl PartialEq for Frame {
    fn eq(&self, other: &Self) -> bool {
        self.obj == other.obj
    }
}

impl Frame {
    pub fn buffer(&self) -> WlBuffer {
        self.inner.lock().unwrap().buffer.clone().unwrap()
    }

    pub fn damage(&self) -> Vec<Rectangle<i32, BufferCoords>> {
        self.inner.lock().unwrap().damage.clone()
    }

    pub fn has_failed(&self) -> bool {
        self.inner.lock().unwrap().failed.is_some()
    }

    pub fn success(
        self,
        transform: impl Into<Transform>,
        damage: impl Into<Option<Vec<Rectangle<i32, BufferCoords>>>>,
        presented: impl Into<Duration>,
    ) {
        {
            let inner = self.inner.lock().unwrap();
            if !inner.capture_requested || inner.failed.is_some() {
                return;
            }
        }

        self.obj.transform(transform.into().into());
        for damage in damage.into().into_iter().flatten() {
            self.obj
                .damage(damage.loc.x, damage.loc.y, damage.size.w, damage.size.h);
        }

        let time = presented.into();
        let tv_sec_hi = (time.as_secs() >> 32) as u32;
        let tv_sec_lo = (time.as_secs() & 0xFFFFFFFF) as u32;
        let tv_nsec = time.subsec_nanos();
        self.obj.presentation_time(tv_sec_hi, tv_sec_lo, tv_nsec);

        self.obj.ready()
    }

    pub fn fail(self, reason: FailureReason) {
        let mut inner = self.inner.lock().unwrap();
        inner.failed = Some(reason);
        if inner.capture_requested {
            self.obj.failed(reason);
        }
    }
}

#[derive(Debug)]
struct FrameInner {
    constraints: Option<BufferConstraints>,
    buffer: Option<WlBuffer>,
    damage: Vec<Rectangle<i32, BufferCoords>>,
    obj: ZcosmicScreencopySessionV2,
    capture_requested: bool,
    failed: Option<FailureReason>,
}

impl FrameInner {
    fn new(
        obj: ZcosmicScreencopySessionV2,
        constraints: impl Into<Option<BufferConstraints>>,
    ) -> Self {
        FrameInner {
            constraints: constraints.into(),
            buffer: None,
            damage: Vec::new(),
            obj,
            capture_requested: false,
            failed: None,
        }
    }
}

pub trait ScreencopyHandler {
    fn screencopy_state(&mut self) -> &mut ScreencopyState;

    fn capture_source(&mut self, source: &ImageSourceData) -> Option<BufferConstraints>;
    fn capture_cursor_source(&mut self, source: &ImageSourceData) -> Option<BufferConstraints>;

    fn new_session(&mut self, session: Session);
    fn new_cursor_session(&mut self, session: CursorSession);

    fn frame(&mut self, session: Session, frame: Frame);
    fn cursor_frame(&mut self, session: CursorSession, frame: Frame);

    fn frame_aborted(&mut self, frame: Frame);
    fn session_destroyed(&mut self, session: Session) {
        let _ = session;
    }
    fn cursor_session_destroyed(&mut self, session: CursorSession) {
        let _ = session;
    }
}

pub struct ScreencopyGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

pub struct ScreencopyData;

pub struct SessionData {
    inner: Arc<Mutex<SessionInner>>,
}

pub struct CursorSessionData {
    inner: Arc<Mutex<CursorSessionInner>>,
}
pub struct FrameData {
    inner: Arc<Mutex<FrameInner>>,
}

impl<D> GlobalDispatch<ZcosmicScreencopyManagerV2, ScreencopyGlobalData, D> for ScreencopyState
where
    D: GlobalDispatch<ZcosmicScreencopyManagerV2, ScreencopyGlobalData>
        + Dispatch<ZcosmicScreencopyManagerV2, ScreencopyData>
        + Dispatch<ZcosmicScreencopySessionV2, SessionData>
        + Dispatch<ZcosmicScreencopySessionV2, CursorSessionData>
        + Dispatch<ZcosmicScreencopyCursorSessionV2, CursorSessionData>
        + Dispatch<ZcosmicScreencopyFrameV2, FrameData>
        + ScreencopyHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<ZcosmicScreencopyManagerV2>,
        _global_data: &ScreencopyGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ScreencopyData);
    }

    fn can_view(client: Client, global_data: &ScreencopyGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ZcosmicScreencopyManagerV2, ScreencopyData, D> for ScreencopyState
where
    D: Dispatch<ZcosmicScreencopyManagerV2, ScreencopyData>
        + Dispatch<ZcosmicScreencopySessionV2, SessionData>
        + Dispatch<ZcosmicScreencopySessionV2, CursorSessionData>
        + Dispatch<ZcosmicScreencopyCursorSessionV2, CursorSessionData>
        + Dispatch<ZcosmicScreencopyFrameV2, FrameData>
        + ScreencopyHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &ZcosmicScreencopyManagerV2,
        request: <ZcosmicScreencopyManagerV2 as Resource>::Request,
        _data: &ScreencopyData,
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_screencopy_manager_v2::Request::CreateSession {
                session,
                source,
                options,
            } => {
                if let Some(src) = source.data::<ImageSourceData>() {
                    if *src != ImageSourceData::Destroyed {
                        if let Some(buffer_constraints) = state.capture_source(src) {
                            let session_data = Arc::new(Mutex::new(SessionInner::new(
                                src.clone(),
                                Into::<u32>::into(options) == 1,
                            )));
                            let obj = data_init.init(
                                session,
                                SessionData {
                                    inner: session_data.clone(),
                                },
                            );

                            let session = Session {
                                obj,
                                inner: session_data,
                                user_data: Arc::new(UserDataMap::new()),
                            };
                            session.update_constraints(buffer_constraints);
                            state
                                .screencopy_state()
                                .known_sessions
                                .push(session.clone());
                            state.new_session(session);
                            return;
                        }
                    }
                }

                let session_data = Arc::new(Mutex::new(SessionInner::new(
                    ImageSourceData::Destroyed,
                    false,
                )));
                let obj = data_init.init(
                    session,
                    SessionData {
                        inner: session_data.clone(),
                    },
                );
                let session = Session {
                    obj,
                    inner: session_data,
                    user_data: Arc::new(UserDataMap::new()),
                };
                session.stop();
            }
            zcosmic_screencopy_manager_v2::Request::CreatePointerCursorSession {
                session,
                source,
                pointer: _,
                options: _,
            } => {
                // TODO: use pointer, but we need new smithay api for that.

                if let Some(src) = source.data::<ImageSourceData>() {
                    if *src != ImageSourceData::Destroyed {
                        if let Some(buffer_constraints) = state.capture_cursor_source(src) {
                            let session_data =
                                Arc::new(Mutex::new(CursorSessionInner::new(src.clone())));
                            let obj = data_init.init(
                                session,
                                CursorSessionData {
                                    inner: session_data.clone(),
                                },
                            );

                            let session = CursorSession {
                                obj,
                                inner: session_data,
                                user_data: Arc::new(UserDataMap::new()),
                            };
                            session.update_constraints(buffer_constraints);
                            state
                                .screencopy_state()
                                .known_cursor_sessions
                                .push(session.clone());
                            state.new_cursor_session(session);
                            return;
                        }
                    }
                }

                let session_data = Arc::new(Mutex::new(CursorSessionInner::new(
                    ImageSourceData::Destroyed,
                )));
                let obj = data_init.init(
                    session,
                    CursorSessionData {
                        inner: session_data.clone(),
                    },
                );
                let session = CursorSession {
                    obj,
                    inner: session_data,
                    user_data: Arc::new(UserDataMap::new()),
                };
                session.stop();
            }
            _ => {}
        }
    }

    fn destroyed(
        _state: &mut D,
        _client: wayland_backend::server::ClientId,
        _resource: &ZcosmicScreencopyManagerV2,
        _data: &ScreencopyData,
    ) {
    }
}

impl<D> Dispatch<ZcosmicScreencopySessionV2, SessionData, D> for ScreencopyState
where
    D: Dispatch<ZcosmicScreencopySessionV2, SessionData>
        + Dispatch<ZcosmicScreencopyFrameV2, FrameData>
        + ScreencopyHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &ZcosmicScreencopySessionV2,
        request: <ZcosmicScreencopySessionV2 as Resource>::Request,
        data: &SessionData,
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_screencopy_session_v2::Request::CreateFrame { frame } => {
                let inner = Arc::new(Mutex::new(FrameInner::new(
                    resource.clone(),
                    data.inner.lock().unwrap().constraints.clone(),
                )));
                let obj = data_init.init(
                    frame,
                    FrameData {
                        inner: inner.clone(),
                    },
                );
                data.inner
                    .lock()
                    .unwrap()
                    .active_frames
                    .push(Frame { obj, inner });
            }
            _ => {}
        }
    }

    fn destroyed(
        state: &mut D,
        _client: wayland_backend::server::ClientId,
        resource: &ZcosmicScreencopySessionV2,
        _data: &SessionData,
    ) {
        let scpy = state.screencopy_state();
        if let Some(pos) = scpy
            .known_sessions
            .iter()
            .position(|session| session.obj == *resource)
        {
            let session = scpy.known_sessions.remove(pos);
            state.session_destroyed(session);
        }
    }
}

impl<D> Dispatch<ZcosmicScreencopyCursorSessionV2, CursorSessionData, D> for ScreencopyState
where
    D: Dispatch<ZcosmicScreencopyCursorSessionV2, CursorSessionData>
        + Dispatch<ZcosmicScreencopySessionV2, CursorSessionData>
        + Dispatch<ZcosmicScreencopyFrameV2, FrameData>
        + ScreencopyHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &ZcosmicScreencopyCursorSessionV2,
        request: <ZcosmicScreencopyCursorSessionV2 as Resource>::Request,
        data: &CursorSessionData,
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_screencopy_cursor_session_v2::Request::GetScreencopySession { session } => {
                let new_data = CursorSessionData {
                    inner: data.inner.clone(),
                };
                let session = data_init.init(session, new_data);

                let mut inner = data.inner.lock().unwrap();
                if inner.session.is_some() {
                    resource.post_error(
                        zcosmic_screencopy_cursor_session_v2::Error::DuplicateSession,
                        "Duplicate session",
                    );
                    return;
                }

                if inner.stopped {
                    session.stopped();
                } else if let Some(constraints) = inner.constraints.as_ref() {
                    session.buffer_size(constraints.size.w as u32, constraints.size.h as u32);
                    for fmt in &constraints.shm {
                        session.shm_format(*fmt as u32);
                    }
                    if let Some(dma) = constraints.dma.as_ref() {
                        let node = Vec::from(dma.node.dev_id().to_ne_bytes());
                        session.dmabuf_device(node);
                        for (fmt, modifiers) in &dma.formats {
                            let mut modifiers = modifiers.clone();
                            let modifiers: Vec<u8> = {
                                let ptr = modifiers.as_mut_ptr() as *mut u8;
                                let len = modifiers.len() * 4;
                                let cap = modifiers.capacity() * 4;
                                std::mem::forget(modifiers);
                                unsafe { Vec::from_raw_parts(ptr, len, cap) }
                            };
                            session.dmabuf_format(*fmt as u32, modifiers);
                        }
                    }
                    session.done();
                }
                inner.session = Some(session);
            }
            _ => {}
        }
    }

    fn destroyed(
        state: &mut D,
        _client: wayland_backend::server::ClientId,
        resource: &ZcosmicScreencopyCursorSessionV2,
        _data: &CursorSessionData,
    ) {
        let scpy = state.screencopy_state();
        if let Some(pos) = scpy
            .known_cursor_sessions
            .iter()
            .position(|session| session.obj == *resource)
        {
            let session = scpy.known_cursor_sessions.remove(pos);
            state.cursor_session_destroyed(session);
        }
    }
}

impl<D> Dispatch<ZcosmicScreencopySessionV2, CursorSessionData, D> for ScreencopyState
where
    D: Dispatch<ZcosmicScreencopySessionV2, SessionData>
        + Dispatch<ZcosmicScreencopyFrameV2, FrameData>
        + ScreencopyHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &ZcosmicScreencopySessionV2,
        request: <ZcosmicScreencopySessionV2 as Resource>::Request,
        data: &CursorSessionData,
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_screencopy_session_v2::Request::CreateFrame { frame } => {
                let inner = Arc::new(Mutex::new(FrameInner::new(
                    resource.clone(),
                    data.inner.lock().unwrap().constraints.clone(),
                )));
                let obj = data_init.init(
                    frame,
                    FrameData {
                        inner: inner.clone(),
                    },
                );
                data.inner
                    .lock()
                    .unwrap()
                    .active_frames
                    .push(Frame { obj, inner });
            }
            _ => {}
        }
    }

    fn destroyed(
        _state: &mut D,
        _client: wayland_backend::server::ClientId,
        _resource: &ZcosmicScreencopySessionV2,
        _data: &CursorSessionData,
    ) {
    }
}

impl<D> Dispatch<ZcosmicScreencopyFrameV2, FrameData, D> for ScreencopyState
where
    D: Dispatch<ZcosmicScreencopyFrameV2, FrameData> + ScreencopyHandler + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &ZcosmicScreencopyFrameV2,
        request: <ZcosmicScreencopyFrameV2 as Resource>::Request,
        data: &FrameData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_screencopy_frame_v2::Request::AttachBuffer { buffer } => {
                let mut inner = data.inner.lock().unwrap();

                if inner.capture_requested {
                    resource.post_error(
                        zcosmic_screencopy_frame_v2::Error::AlreadyCaptured,
                        "Frame was captured previously",
                    );
                }

                inner.buffer = Some(buffer);
            }
            zcosmic_screencopy_frame_v2::Request::DamageBuffer {
                x,
                y,
                width,
                height,
            } => {
                let mut inner = data.inner.lock().unwrap();

                if inner.capture_requested {
                    resource.post_error(
                        zcosmic_screencopy_frame_v2::Error::AlreadyCaptured,
                        "Frame was captured previously",
                    );
                }

                if x < 0 || y < 0 || width <= 0 || height <= 0 {
                    resource.post_error(
                        zcosmic_screencopy_frame_v2::Error::InvalidBufferDamage,
                        "Coordinates negative or size equal to zero",
                    );
                    return;
                }

                inner
                    .damage
                    .push(Rectangle::from_loc_and_size((x, y), (width, height)));
            }
            zcosmic_screencopy_frame_v2::Request::Capture => {
                let mut inner = data.inner.lock().unwrap();

                if inner.capture_requested {
                    resource.post_error(
                        zcosmic_screencopy_frame_v2::Error::AlreadyCaptured,
                        "Frame was captured previously",
                    );
                }

                if inner.buffer.is_none() {
                    resource.post_error(
                        zcosmic_screencopy_frame_v2::Error::NoBuffer,
                        "Attempting to capture frame without a buffer",
                    );
                }

                inner.capture_requested = true;

                if let Some(reason) = inner.failed {
                    resource.failed(reason);
                    return;
                }

                if let Some(constraints) = inner.constraints.as_ref() {
                    let buffer = inner.buffer.as_ref().unwrap();
                    match buffer_type(buffer) {
                        Some(BufferType::Dma) => {
                            let Some(dma_constraints) = constraints.dma.as_ref() else {
                                debug!("dma buffer not specified for screencopy");
                                inner.failed = Some(FailureReason::BufferConstraints);
                                resource.failed(FailureReason::BufferConstraints);
                                return;
                            };

                            let dmabuf = match get_dmabuf(buffer) {
                                Ok(buf) => buf,
                                Err(err) => {
                                    debug!(?err, "Error accessing dma buffer for screencopy");
                                    inner.failed = Some(FailureReason::Unknown);
                                    resource.failed(FailureReason::Unknown);
                                    return;
                                }
                            };

                            let buffer_size = dmabuf.size();
                            if buffer_size.w < constraints.size.w
                                || buffer_size.h < constraints.size.h
                            {
                                debug!(?buffer_size, ?constraints.size, "buffer too small for screencopy");
                                inner.failed = Some(FailureReason::BufferConstraints);
                                resource.failed(FailureReason::BufferConstraints);
                                return;
                            }

                            let format = dmabuf.format();
                            if dma_constraints
                                .formats
                                .iter()
                                .find(|(fourcc, _)| *fourcc == format.code)
                                .filter(|(_, modifiers)| modifiers.contains(&format.modifier))
                                .is_none()
                            {
                                debug!(
                                    ?format,
                                    ?dma_constraints,
                                    "unsupported buffer format for screencopy"
                                );
                                inner.failed = Some(FailureReason::BufferConstraints);
                                resource.failed(FailureReason::BufferConstraints);
                                return;
                            }
                        }
                        Some(BufferType::Shm) => {
                            let buffer_data = match with_buffer_contents(buffer, |_, _, data| data)
                            {
                                Ok(data) => data,
                                Err(err) => {
                                    debug!(?err, "Error accessing shm buffer for screencopy");
                                    inner.failed = Some(FailureReason::Unknown);
                                    resource.failed(FailureReason::Unknown);
                                    return;
                                }
                            };

                            if buffer_data.width < constraints.size.w
                                || buffer_data.height < constraints.size.h
                            {
                                debug!(?buffer_data, ?constraints.size, "buffer too small for screencopy");
                                inner.failed = Some(FailureReason::BufferConstraints);
                                resource.failed(FailureReason::BufferConstraints);
                                return;
                            }

                            if !constraints.shm.contains(&buffer_data.format) {
                                debug!(?buffer_data.format, ?constraints.shm, "unsupported buffer format for screencopy");
                                inner.failed = Some(FailureReason::BufferConstraints);
                                resource.failed(FailureReason::BufferConstraints);
                                return;
                            }
                        }
                        x => {
                            debug!(?x, "Attempt to screencopy with unsupported buffer type");
                            inner.failed = Some(FailureReason::BufferConstraints);
                            resource.failed(FailureReason::BufferConstraints);
                            return;
                        }
                    }
                } else {
                    inner.failed = Some(FailureReason::Unknown);
                    resource.failed(FailureReason::Unknown);
                    return;
                }

                let frame = Frame {
                    obj: resource.clone(),
                    inner: data.inner.clone(),
                };

                let scpy = state.screencopy_state();
                if let Some(session) = scpy
                    .known_sessions
                    .iter()
                    .find(|session| session.obj == inner.obj)
                    .map(|s| Session {
                        obj: s.obj.clone(),
                        inner: s.inner.clone(),
                        user_data: s.user_data.clone(),
                    })
                {
                    if session.inner.lock().unwrap().stopped {
                        resource.failed(FailureReason::Stopped);
                        return;
                    }

                    std::mem::drop(inner);
                    state.frame(session, frame);
                } else if let Some(session) = scpy
                    .known_cursor_sessions
                    .iter()
                    .find(|session| {
                        session.inner.lock().unwrap().session.as_ref() == Some(&inner.obj)
                    })
                    .map(|s| CursorSession {
                        obj: s.obj.clone(),
                        inner: s.inner.clone(),
                        user_data: s.user_data.clone(),
                    })
                {
                    if session.inner.lock().unwrap().stopped {
                        resource.failed(FailureReason::Stopped);
                        return;
                    }

                    std::mem::drop(inner);
                    state.cursor_frame(session, frame);
                } else {
                    inner.failed = Some(FailureReason::Unknown);
                    resource.failed(FailureReason::Unknown);
                }
            }
            _ => {}
        }
    }

    fn destroyed(
        state: &mut D,
        _client: wayland_backend::server::ClientId,
        resource: &ZcosmicScreencopyFrameV2,
        data: &FrameData,
    ) {
        {
            let scpy = state.screencopy_state();
            for session in &mut scpy.known_sessions {
                session
                    .inner
                    .lock()
                    .unwrap()
                    .active_frames
                    .retain(|frame| frame.obj != *resource);
            }
            for cursor_session in &mut scpy.known_cursor_sessions {
                cursor_session
                    .inner
                    .lock()
                    .unwrap()
                    .active_frames
                    .retain(|frame| frame.obj != *resource);
            }
        }
        let frame = Frame {
            obj: resource.clone(),
            inner: data.inner.clone(),
        };
        state.frame_aborted(frame);
    }
}

macro_rules! delegate_screencopy {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::screencopy::v2::server::zcosmic_screencopy_manager_v2::ZcosmicScreencopyManagerV2: $crate::wayland::protocols::screencopy::ScreencopyGlobalData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::screencopy::v2::server::zcosmic_screencopy_manager_v2::ZcosmicScreencopyManagerV2: $crate::wayland::protocols::screencopy::ScreencopyData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::screencopy::v2::server::zcosmic_screencopy_session_v2::ZcosmicScreencopySessionV2: $crate::wayland::protocols::screencopy::SessionData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::screencopy::v2::server::zcosmic_screencopy_cursor_session_v2::ZcosmicScreencopyCursorSessionV2: $crate::wayland::protocols::screencopy::CursorSessionData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::screencopy::v2::server::zcosmic_screencopy_session_v2::ZcosmicScreencopySessionV2: $crate::wayland::protocols::screencopy::CursorSessionData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::screencopy::v2::server::zcosmic_screencopy_frame_v2::ZcosmicScreencopyFrameV2: $crate::wayland::protocols::screencopy::FrameData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
    };
}
pub(crate) use delegate_screencopy;
