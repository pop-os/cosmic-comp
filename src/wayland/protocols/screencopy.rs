use std::{
    ops,
    sync::{Arc, Mutex},
    time::Duration,
};

pub use smithay::reexports::wayland_protocols::ext::image_copy_capture::v1::server::{
    ext_image_copy_capture_frame_v1::FailureReason,
};
use smithay::reexports::wayland_protocols::ext::image_copy_capture::v1::server::{
    ext_image_copy_capture_cursor_session_v1::{self, ExtImageCopyCaptureCursorSessionV1},
    ext_image_copy_capture_frame_v1::{self, ExtImageCopyCaptureFrameV1},
    ext_image_copy_capture_manager_v1::{self, ExtImageCopyCaptureManagerV1},
    ext_image_copy_capture_session_v1::{self, ExtImageCopyCaptureSessionV1},
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
        Weak,
    },
    utils::Rectangle,
};
use tracing::debug;
use wayland_backend::server::GlobalId;

use super::image_capture_source::ImageCaptureSourceData;

#[derive(Debug)]
pub struct ScreencopyState {
    global: GlobalId,
    known_sessions: Vec<SessionRef>,
    known_cursor_sessions: Vec<CursorSessionRef>,
}

impl ScreencopyState {
    pub fn new<D, F>(display: &DisplayHandle, client_filter: F) -> ScreencopyState
    where
        D: GlobalDispatch<ExtImageCopyCaptureManagerV1, ScreencopyGlobalData>
            + Dispatch<ExtImageCopyCaptureManagerV1, ScreencopyData>
            + Dispatch<ExtImageCopyCaptureSessionV1, SessionData>
            + Dispatch<ExtImageCopyCaptureSessionV1, CursorSessionData>
            + Dispatch<ExtImageCopyCaptureCursorSessionV1, CursorSessionData>
            + Dispatch<ExtImageCopyCaptureFrameV1, FrameData>
            + ScreencopyHandler
            + 'static,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        ScreencopyState {
            global: display.create_global::<D, ExtImageCopyCaptureManagerV1, _>(
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
pub struct SessionRef {
    obj: ExtImageCopyCaptureSessionV1,
    inner: Arc<Mutex<SessionInner>>,
    user_data: Arc<UserDataMap>,
}

impl PartialEq for SessionRef {
    fn eq(&self, other: &Self) -> bool {
        self.obj == other.obj
    }
}

#[derive(Debug)]
struct SessionInner {
    stopped: bool,
    constraints: Option<BufferConstraints>,
    draw_cursors: bool,
    source: ImageCaptureSourceData,
    active_frames: Vec<FrameRef>,
}

impl SessionInner {
    fn new(source: ImageCaptureSourceData, draw_cursors: bool) -> SessionInner {
        SessionInner {
            stopped: false,
            constraints: None,
            draw_cursors,
            source,
            active_frames: Vec::new(),
        }
    }
}

impl IsAlive for SessionRef {
    fn alive(&self) -> bool {
        self.obj.is_alive()
    }
}

impl SessionRef {
    pub fn update_constraints(&self, constraints: BufferConstraints) {
        let mut inner = self.inner.lock().unwrap();

        if !self.obj.is_alive() || inner.stopped {
            return;
        }

        self.obj
            .buffer_size(constraints.size.w as u32, constraints.size.h as u32);
        for fmt in &constraints.shm {
            self.obj.shm_format(*fmt);
        }
        if let Some(dma) = constraints.dma.as_ref() {
            let node = Vec::from(dma.node.dev_id().to_ne_bytes());
            self.obj.dmabuf_device(node);
            for (fmt, modifiers) in &dma.formats {
                let modifiers = modifiers
                    .iter()
                    .flat_map(|modifier| u64::from(*modifier).to_ne_bytes())
                    .collect::<Vec<u8>>();
                self.obj.dmabuf_format(*fmt as u32, modifiers);
            }
        }
        self.obj.done();

        inner.constraints = Some(constraints);
    }

    pub fn current_constraints(&self) -> Option<BufferConstraints> {
        self.inner.lock().unwrap().constraints.clone()
    }

    pub fn source(&self) -> ImageCaptureSourceData {
        self.inner.lock().unwrap().source.clone()
    }

    pub fn draw_cursor(&self) -> bool {
        self.inner.lock().unwrap().draw_cursors
    }

    pub fn user_data(&self) -> &UserDataMap {
        &self.user_data
    }
}

#[derive(Debug)]
pub struct Session(SessionRef);

impl ops::Deref for Session {
    type Target = SessionRef;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PartialEq<SessionRef> for Session {
    fn eq(&self, other: &SessionRef) -> bool {
        self.0 == *other
    }
}

impl Drop for Session {
    fn drop(&mut self) {
        let mut inner = self.0.inner.lock().unwrap();

        if !self.obj.is_alive() || inner.stopped {
            return;
        }

        for frame in inner.active_frames.drain(..) {
            frame
                .inner
                .lock()
                .unwrap()
                .fail(&frame.obj, FailureReason::Stopped);
        }

        self.obj.stopped();
        inner.constraints.take();
        inner.stopped = true;
    }
}

impl Session {
    pub fn stop(self) {
        let _ = self;
    }
}

#[derive(Debug, Clone)]
pub struct CursorSessionRef {
    obj: ExtImageCopyCaptureCursorSessionV1,
    inner: Arc<Mutex<CursorSessionInner>>,
    user_data: Arc<UserDataMap>,
}

impl PartialEq for CursorSessionRef {
    fn eq(&self, other: &Self) -> bool {
        self.obj == other.obj
    }
}

#[derive(Debug)]
struct CursorSessionInner {
    session: Option<ExtImageCopyCaptureSessionV1>,
    stopped: bool,
    constraints: Option<BufferConstraints>,
    source: ImageCaptureSourceData,
    position: Option<Point<i32, BufferCoords>>,
    hotspot: Point<i32, BufferCoords>,
    active_frames: Vec<FrameRef>,
}

impl CursorSessionInner {
    fn new(source: ImageCaptureSourceData) -> CursorSessionInner {
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

impl IsAlive for CursorSessionRef {
    fn alive(&self) -> bool {
        self.obj.is_alive()
    }
}

impl CursorSessionRef {
    pub fn update_constraints(&self, constrains: BufferConstraints) {
        let mut inner = self.inner.lock().unwrap();

        if !self.obj.is_alive() || inner.stopped {
            return;
        }

        if let Some(session_obj) = inner.session.as_ref() {
            session_obj.buffer_size(constrains.size.w as u32, constrains.size.h as u32);
            for fmt in &constrains.shm {
                session_obj.shm_format(*fmt);
            }
            if let Some(dma) = constrains.dma.as_ref() {
                let node = Vec::from(dma.node.dev_id().to_ne_bytes());
                session_obj.dmabuf_device(node);
                for (fmt, modifiers) in &dma.formats {
                    let modifiers = modifiers
                        .iter()
                        .flat_map(|modifier| u64::from(*modifier).to_ne_bytes())
                        .collect::<Vec<u8>>();
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

    pub fn source(&self) -> ImageCaptureSourceData {
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
        &self.user_data
    }
}

#[derive(Debug)]
pub struct CursorSession(CursorSessionRef);

impl ops::Deref for CursorSession {
    type Target = CursorSessionRef;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PartialEq<CursorSessionRef> for CursorSession {
    fn eq(&self, other: &CursorSessionRef) -> bool {
        self.0 == *other
    }
}

impl Drop for CursorSession {
    fn drop(&mut self) {
        let mut inner = self.0.inner.lock().unwrap();

        if !self.0.obj.is_alive() || inner.stopped {
            return;
        }

        if let Some(session_obj) = inner.session.as_ref() {
            session_obj.stopped();
        }
        inner.constraints.take();

        for frame in inner.active_frames.drain(..) {
            frame
                .inner
                .lock()
                .unwrap()
                .fail(&frame.obj, FailureReason::Stopped);
        }

        inner.stopped = true;
    }
}

impl CursorSession {
    pub fn stop(self) {
        let _ = self;
    }
}

/// Un-owned reference to a frame
#[derive(Clone, Debug)]
pub struct FrameRef {
    obj: ExtImageCopyCaptureFrameV1,
    inner: Arc<Mutex<FrameInner>>,
}

impl PartialEq for FrameRef {
    fn eq(&self, other: &Self) -> bool {
        self.obj == other.obj
    }
}

impl FrameRef {
    pub fn buffer(&self) -> WlBuffer {
        self.inner.lock().unwrap().buffer.clone().unwrap()
    }

    pub fn damage(&self) -> Vec<Rectangle<i32, BufferCoords>> {
        self.inner.lock().unwrap().damage.clone()
    }

    pub fn has_failed(&self) -> bool {
        self.inner.lock().unwrap().failed.is_some()
    }
}

#[derive(Debug, PartialEq)]
pub struct Frame(FrameRef);

impl ops::Deref for Frame {
    type Target = FrameRef;

    fn deref(&self) -> &FrameRef {
        &self.0
    }
}

impl PartialEq<FrameRef> for Frame {
    fn eq(&self, other: &FrameRef) -> bool {
        self.0 == *other
    }
}

impl Frame {
    pub fn success(
        self,
        transform: impl Into<Transform>,
        damage: impl Into<Option<Vec<Rectangle<i32, BufferCoords>>>>,
        presented: impl Into<Duration>,
    ) {
        {
            let inner = self.0.inner.lock().unwrap();
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

        self.0.inner.lock().unwrap().ready = true;
        self.obj.ready()
    }

    pub fn fail(self, reason: FailureReason) {
        self.0.inner.lock().unwrap().fail(&self.obj, reason);
    }
}

impl Drop for Frame {
    fn drop(&mut self) {
        // Send `fail` is `sucesss` or `fail` not already called
        self.inner
            .lock()
            .unwrap()
            .fail(&self.obj, FailureReason::Unknown);
    }
}

#[derive(Debug)]
struct FrameInner {
    constraints: Option<BufferConstraints>,
    buffer: Option<WlBuffer>,
    damage: Vec<Rectangle<i32, BufferCoords>>,
    // `SessionInner` contains a `Vec<FrameRef>`, so use a weak reference here to
    // avoid a cycle.
    obj: Weak<ExtImageCopyCaptureSessionV1>,
    capture_requested: bool,
    failed: Option<FailureReason>,
    ready: bool,
}

impl FrameInner {
    fn new(
        obj: ExtImageCopyCaptureSessionV1,
        constraints: impl Into<Option<BufferConstraints>>,
    ) -> Self {
        FrameInner {
            constraints: constraints.into(),
            buffer: None,
            damage: Vec::new(),
            obj: obj.downgrade(),
            capture_requested: false,
            failed: None,
            ready: false,
        }
    }

    fn fail(&mut self, frame: &ExtImageCopyCaptureFrameV1, reason: FailureReason) {
        if self.ready || self.failed.is_some() {
            return;
        }
        self.failed = Some(reason);
        if self.capture_requested {
            frame.failed(reason);
        }
    }
}

pub trait ScreencopyHandler {
    fn screencopy_state(&mut self) -> &mut ScreencopyState;

    fn capture_source(&mut self, source: &ImageCaptureSourceData) -> Option<BufferConstraints>;
    fn capture_cursor_source(
        &mut self,
        source: &ImageCaptureSourceData,
    ) -> Option<BufferConstraints>;

    fn new_session(&mut self, session: Session);
    fn new_cursor_session(&mut self, session: CursorSession);

    fn frame(&mut self, session: SessionRef, frame: Frame);
    fn cursor_frame(&mut self, session: CursorSessionRef, frame: Frame);

    fn frame_aborted(&mut self, frame_handle: FrameRef);
    fn session_destroyed(&mut self, session: SessionRef) {
        let _ = session;
    }
    fn cursor_session_destroyed(&mut self, session: CursorSessionRef) {
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

impl<D> GlobalDispatch<ExtImageCopyCaptureManagerV1, ScreencopyGlobalData, D> for ScreencopyState
where
    D: GlobalDispatch<ExtImageCopyCaptureManagerV1, ScreencopyGlobalData>
        + Dispatch<ExtImageCopyCaptureManagerV1, ScreencopyData>
        + Dispatch<ExtImageCopyCaptureSessionV1, SessionData>
        + Dispatch<ExtImageCopyCaptureSessionV1, CursorSessionData>
        + Dispatch<ExtImageCopyCaptureCursorSessionV1, CursorSessionData>
        + Dispatch<ExtImageCopyCaptureFrameV1, FrameData>
        + ScreencopyHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<ExtImageCopyCaptureManagerV1>,
        _global_data: &ScreencopyGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ScreencopyData);
    }

    fn can_view(client: Client, global_data: &ScreencopyGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ExtImageCopyCaptureManagerV1, ScreencopyData, D> for ScreencopyState
where
    D: Dispatch<ExtImageCopyCaptureManagerV1, ScreencopyData>
        + Dispatch<ExtImageCopyCaptureSessionV1, SessionData>
        + Dispatch<ExtImageCopyCaptureSessionV1, CursorSessionData>
        + Dispatch<ExtImageCopyCaptureCursorSessionV1, CursorSessionData>
        + Dispatch<ExtImageCopyCaptureFrameV1, FrameData>
        + ScreencopyHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &ExtImageCopyCaptureManagerV1,
        request: <ExtImageCopyCaptureManagerV1 as Resource>::Request,
        _data: &ScreencopyData,
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            ext_image_copy_capture_manager_v1::Request::CreateSession {
                session,
                source,
                options,
            } => {
                if let Some(src) = source.data::<ImageCaptureSourceData>() {
                    if *src != ImageCaptureSourceData::Destroyed {
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

                            let session = SessionRef {
                                obj,
                                inner: session_data,
                                user_data: Arc::new(UserDataMap::new()),
                            };
                            session.update_constraints(buffer_constraints);
                            state
                                .screencopy_state()
                                .known_sessions
                                .push(session.clone());
                            state.new_session(Session(session));
                            return;
                        }
                    }
                }

                let session_data = Arc::new(Mutex::new(SessionInner::new(
                    ImageCaptureSourceData::Destroyed,
                    false,
                )));
                let obj = data_init.init(
                    session,
                    SessionData {
                        inner: session_data.clone(),
                    },
                );
                let session = Session(SessionRef {
                    obj,
                    inner: session_data,
                    user_data: Arc::new(UserDataMap::new()),
                });
                session.stop();
            }
            ext_image_copy_capture_manager_v1::Request::CreatePointerCursorSession {
                session,
                source,
                pointer: _,
            } => {
                // TODO: use pointer, but we need new smithay api for that.

                if let Some(src) = source.data::<ImageCaptureSourceData>() {
                    if *src != ImageCaptureSourceData::Destroyed {
                        if let Some(buffer_constraints) = state.capture_cursor_source(src) {
                            let session_data =
                                Arc::new(Mutex::new(CursorSessionInner::new(src.clone())));
                            let obj = data_init.init(
                                session,
                                CursorSessionData {
                                    inner: session_data.clone(),
                                },
                            );

                            let session = CursorSessionRef {
                                obj,
                                inner: session_data,
                                user_data: Arc::new(UserDataMap::new()),
                            };
                            session.update_constraints(buffer_constraints);
                            state
                                .screencopy_state()
                                .known_cursor_sessions
                                .push(session.clone());
                            state.new_cursor_session(CursorSession(session));
                            return;
                        }
                    }
                }

                let session_data = Arc::new(Mutex::new(CursorSessionInner::new(
                    ImageCaptureSourceData::Destroyed,
                )));
                let obj = data_init.init(
                    session,
                    CursorSessionData {
                        inner: session_data.clone(),
                    },
                );
                let session = CursorSession(CursorSessionRef {
                    obj,
                    inner: session_data,
                    user_data: Arc::new(UserDataMap::new()),
                });
                session.stop();
            }
            _ => {}
        }
    }

    fn destroyed(
        _state: &mut D,
        _client: wayland_backend::server::ClientId,
        _resource: &ExtImageCopyCaptureManagerV1,
        _data: &ScreencopyData,
    ) {
    }
}

impl<D> Dispatch<ExtImageCopyCaptureSessionV1, SessionData, D> for ScreencopyState
where
    D: Dispatch<ExtImageCopyCaptureSessionV1, SessionData>
        + Dispatch<ExtImageCopyCaptureFrameV1, FrameData>
        + ScreencopyHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &ExtImageCopyCaptureSessionV1,
        request: <ExtImageCopyCaptureSessionV1 as Resource>::Request,
        data: &SessionData,
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        if let ext_image_copy_capture_session_v1::Request::CreateFrame { frame } = request {
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
                .push(FrameRef { obj, inner });
        }
    }

    fn destroyed(
        state: &mut D,
        _client: wayland_backend::server::ClientId,
        resource: &ExtImageCopyCaptureSessionV1,
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

impl<D> Dispatch<ExtImageCopyCaptureCursorSessionV1, CursorSessionData, D> for ScreencopyState
where
    D: Dispatch<ExtImageCopyCaptureCursorSessionV1, CursorSessionData>
        + Dispatch<ExtImageCopyCaptureSessionV1, CursorSessionData>
        + Dispatch<ExtImageCopyCaptureFrameV1, FrameData>
        + ScreencopyHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &ExtImageCopyCaptureCursorSessionV1,
        request: <ExtImageCopyCaptureCursorSessionV1 as Resource>::Request,
        data: &CursorSessionData,
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        if let ext_image_copy_capture_cursor_session_v1::Request::GetCaptureSession { session } =
            request
        {
            let new_data = CursorSessionData {
                inner: data.inner.clone(),
            };
            let session = data_init.init(session, new_data);

            let mut inner = data.inner.lock().unwrap();
            if inner.session.is_some() {
                resource.post_error(
                    ext_image_copy_capture_cursor_session_v1::Error::DuplicateSession,
                    "Duplicate session",
                );
                return;
            }

            if inner.stopped {
                session.stopped();
            } else if let Some(constraints) = inner.constraints.as_ref() {
                session.buffer_size(constraints.size.w as u32, constraints.size.h as u32);
                for fmt in &constraints.shm {
                    session.shm_format(*fmt);
                }
                if let Some(dma) = constraints.dma.as_ref() {
                    let node = Vec::from(dma.node.dev_id().to_ne_bytes());
                    session.dmabuf_device(node);
                    for (fmt, modifiers) in &dma.formats {
                        let modifiers = modifiers
                            .iter()
                            .flat_map(|modifier| u64::from(*modifier).to_ne_bytes())
                            .collect::<Vec<u8>>();
                        session.dmabuf_format(*fmt as u32, modifiers);
                    }
                }
                session.done();
            }
            inner.session = Some(session);
        }
    }

    fn destroyed(
        state: &mut D,
        _client: wayland_backend::server::ClientId,
        resource: &ExtImageCopyCaptureCursorSessionV1,
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

impl<D> Dispatch<ExtImageCopyCaptureSessionV1, CursorSessionData, D> for ScreencopyState
where
    D: Dispatch<ExtImageCopyCaptureSessionV1, SessionData>
        + Dispatch<ExtImageCopyCaptureFrameV1, FrameData>
        + ScreencopyHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &ExtImageCopyCaptureSessionV1,
        request: <ExtImageCopyCaptureSessionV1 as Resource>::Request,
        data: &CursorSessionData,
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        if let ext_image_copy_capture_session_v1::Request::CreateFrame { frame } = request {
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
                .push(FrameRef { obj, inner });
        }
    }

    fn destroyed(
        _state: &mut D,
        _client: wayland_backend::server::ClientId,
        _resource: &ExtImageCopyCaptureSessionV1,
        _data: &CursorSessionData,
    ) {
    }
}

impl<D> Dispatch<ExtImageCopyCaptureFrameV1, FrameData, D> for ScreencopyState
where
    D: Dispatch<ExtImageCopyCaptureFrameV1, FrameData> + ScreencopyHandler + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &ExtImageCopyCaptureFrameV1,
        request: <ExtImageCopyCaptureFrameV1 as Resource>::Request,
        data: &FrameData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            ext_image_copy_capture_frame_v1::Request::AttachBuffer { buffer } => {
                let mut inner = data.inner.lock().unwrap();

                if inner.capture_requested {
                    resource.post_error(
                        ext_image_copy_capture_frame_v1::Error::AlreadyCaptured,
                        "Frame was captured previously",
                    );
                }

                inner.buffer = Some(buffer);
            }
            ext_image_copy_capture_frame_v1::Request::DamageBuffer {
                x,
                y,
                width,
                height,
            } => {
                let mut inner = data.inner.lock().unwrap();

                if inner.capture_requested {
                    resource.post_error(
                        ext_image_copy_capture_frame_v1::Error::AlreadyCaptured,
                        "Frame was captured previously",
                    );
                }

                if x < 0 || y < 0 || width <= 0 || height <= 0 {
                    resource.post_error(
                        ext_image_copy_capture_frame_v1::Error::InvalidBufferDamage,
                        "Coordinates negative or size equal to zero",
                    );
                    return;
                }

                inner
                    .damage
                    .push(Rectangle::new((x, y).into(), (width, height).into()));
            }
            ext_image_copy_capture_frame_v1::Request::Capture => {
                {
                    let inner = data.inner.lock().unwrap();

                    if inner.capture_requested {
                        resource.post_error(
                            ext_image_copy_capture_frame_v1::Error::AlreadyCaptured,
                            "Frame was captured previously",
                        );
                        return;
                    }

                    if inner.buffer.is_none() {
                        resource.post_error(
                            ext_image_copy_capture_frame_v1::Error::NoBuffer,
                            "Attempting to capture frame without a buffer",
                        );
                        return;
                    }
                }

                let frame = Frame(FrameRef {
                    obj: resource.clone(),
                    inner: data.inner.clone(),
                });
                if let Err(reason) = capture_frame(state, frame) {
                    data.inner.lock().unwrap().fail(resource, reason);
                }
            }
            _ => {}
        }
    }

    fn destroyed(
        state: &mut D,
        _client: wayland_backend::server::ClientId,
        resource: &ExtImageCopyCaptureFrameV1,
        data: &FrameData,
    ) {
        let frame_ref = FrameRef {
            obj: resource.clone(),
            inner: data.inner.clone(),
        };
        {
            let scpy = state.screencopy_state();
            for session in &mut scpy.known_sessions {
                session
                    .inner
                    .lock()
                    .unwrap()
                    .active_frames
                    .retain(|i| *i != frame_ref);
            }
            for cursor_session in &mut scpy.known_cursor_sessions {
                cursor_session
                    .inner
                    .lock()
                    .unwrap()
                    .active_frames
                    .retain(|i| *i != frame_ref);
            }
        }
        state.frame_aborted(frame_ref);
    }
}

fn capture_frame<D: ScreencopyHandler>(state: &mut D, frame: Frame) -> Result<(), FailureReason> {
    let mut inner = frame.0.inner.lock().unwrap();

    inner.capture_requested = true;

    if let Some(reason) = inner.failed {
        return Err(reason);
    }

    if let Some(constraints) = inner.constraints.as_ref() {
        let buffer = inner.buffer.as_ref().unwrap();
        match buffer_type(buffer) {
            Some(BufferType::Dma) => {
                let Some(dma_constraints) = constraints.dma.as_ref() else {
                    debug!("dma buffer not specified for screencopy");
                    return Err(FailureReason::BufferConstraints);
                };

                let dmabuf = match get_dmabuf(buffer) {
                    Ok(buf) => buf,
                    Err(err) => {
                        debug!(?err, "Error accessing dma buffer for screencopy");
                        return Err(FailureReason::Stopped);
                    }
                };

                let buffer_size = dmabuf.size();
                if buffer_size.w < constraints.size.w || buffer_size.h < constraints.size.h {
                    debug!(?buffer_size, ?constraints.size, "buffer too small for screencopy");
                    return Err(FailureReason::BufferConstraints);
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
                    return Err(FailureReason::BufferConstraints);
                }
            }
            Some(BufferType::Shm) => {
                let buffer_data = match with_buffer_contents(buffer, |_, _, data| data) {
                    Ok(data) => data,
                    Err(err) => {
                        debug!(?err, "Error accessing shm buffer for screencopy");
                        return Err(FailureReason::Unknown);
                    }
                };

                if buffer_data.width < constraints.size.w || buffer_data.height < constraints.size.h
                {
                    debug!(?buffer_data, ?constraints.size, "buffer too small for screencopy");
                    return Err(FailureReason::BufferConstraints);
                }

                if !constraints.shm.contains(&buffer_data.format) {
                    debug!(?buffer_data.format, ?constraints.shm, "unsupported buffer format for screencopy");
                    return Err(FailureReason::BufferConstraints);
                }
            }
            x => {
                debug!(?x, "Attempt to screencopy with unsupported buffer type");
                return Err(FailureReason::BufferConstraints);
            }
        }
    } else {
        return Err(FailureReason::Unknown);
    }

    let scpy = state.screencopy_state();
    if let Some(session) = scpy
        .known_sessions
        .iter()
        .find(|session| session.obj == inner.obj)
        .cloned()
    {
        if session.inner.lock().unwrap().stopped {
            return Err(FailureReason::Stopped);
        }

        std::mem::drop(inner);
        state.frame(session, frame);
        Ok(())
    } else if let Some(session) = scpy
        .known_cursor_sessions
        .iter()
        .find(|session| {
            session.inner.lock().unwrap().session.as_ref() == inner.obj.upgrade().ok().as_ref()
        })
        .cloned()
    {
        if session.inner.lock().unwrap().stopped {
            return Err(FailureReason::Stopped);
        }

        std::mem::drop(inner);
        state.cursor_frame(session, frame);
        Ok(())
    } else {
        Err(FailureReason::Unknown)
    }
}

macro_rules! delegate_screencopy {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::image_copy_capture::v1::server::ext_image_copy_capture_manager_v1::ExtImageCopyCaptureManagerV1: $crate::wayland::protocols::screencopy::ScreencopyGlobalData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::image_copy_capture::v1::server::ext_image_copy_capture_manager_v1::ExtImageCopyCaptureManagerV1: $crate::wayland::protocols::screencopy::ScreencopyData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::image_copy_capture::v1::server::ext_image_copy_capture_session_v1::ExtImageCopyCaptureSessionV1: $crate::wayland::protocols::screencopy::SessionData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::image_copy_capture::v1::server::ext_image_copy_capture_cursor_session_v1::ExtImageCopyCaptureCursorSessionV1: $crate::wayland::protocols::screencopy::CursorSessionData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::image_copy_capture::v1::server::ext_image_copy_capture_session_v1::ExtImageCopyCaptureSessionV1: $crate::wayland::protocols::screencopy::CursorSessionData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::image_copy_capture::v1::server::ext_image_copy_capture_frame_v1::ExtImageCopyCaptureFrameV1: $crate::wayland::protocols::screencopy::FrameData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
    };
}
pub(crate) use delegate_screencopy;
