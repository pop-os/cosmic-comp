use std::{
    sync::{Arc, Mutex},
    time::Duration,
};

pub use cosmic_protocols::screencopy::v2::server::zcosmic_screencopy_frame_v2::FailureReason;
use cosmic_protocols::screencopy::v2::server::{
    zcosmic_screencopy_cursor_session_v2::ZcosmicScreencopyCursorSessionV2,
    zcosmic_screencopy_frame_v2::ZcosmicScreencopyFrameV2,
    zcosmic_screencopy_manager_v2::ZcosmicScreencopyManagerV2,
    zcosmic_screencopy_session_v2::ZcosmicScreencopySessionV2,
};
use smithay::reexports::wayland_protocols::ext::screencopy::v1::server::{
    ext_screencopy_cursor_session_v1::ExtScreencopyCursorSessionV1,
    ext_screencopy_frame_v1::ExtScreencopyFrameV1,
    ext_screencopy_manager_v1::ExtScreencopyManagerV1,
    ext_screencopy_session_v1::ExtScreencopySessionV1,
};
use smithay::{
    backend::{
        allocator::{Fourcc, Modifier},
        drm::DrmNode,
    },
    utils::{user_data::UserDataMap, Buffer as BufferCoords, IsAlive, Size, Transform},
};
use smithay::{reexports::wayland_server::protocol::wl_buffer::WlBuffer, utils::Point};
use smithay::{
    reexports::wayland_server::{
        protocol::wl_shm, Client, Dispatch, DisplayHandle, GlobalDispatch,
    },
    utils::Rectangle,
};
use wayland_backend::server::GlobalId;

use super::image_source::ImageSourceData;

mod cosmic;
mod ext;
mod types;

use self::types::*;

#[derive(Debug)]
pub struct ScreencopyState {
    global: GlobalId,
    ext_global: GlobalId,
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
            + GlobalDispatch<ExtScreencopyManagerV1, ScreencopyGlobalData>
            + Dispatch<ExtScreencopyManagerV1, ScreencopyData>
            + Dispatch<ExtScreencopySessionV1, SessionData>
            + Dispatch<ExtScreencopySessionV1, CursorSessionData>
            + Dispatch<ExtScreencopyCursorSessionV1, CursorSessionData>
            + Dispatch<ExtScreencopyFrameV1, FrameData>
            + ScreencopyHandler
            + 'static,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + Clone + 'static,
    {
        ScreencopyState {
            global: display.create_global::<D, ZcosmicScreencopyManagerV2, _>(
                1,
                ScreencopyGlobalData {
                    filter: Box::new(client_filter.clone()),
                },
            ),
            ext_global: display.create_global::<D, ExtScreencopyManagerV1, _>(
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

    pub fn ext_global_id(&self) -> &GlobalId {
        &self.ext_global
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
    obj: SessionObj,
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
            self.obj.shm_format(*fmt);
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
    obj: CursorSessionObj,
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
    session: Option<SessionObj>,
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
                session_obj.shm_format(*fmt);
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
    obj: FrameObj,
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
    obj: SessionObj,
    capture_requested: bool,
    failed: Option<FailureReason>,
}

impl FrameInner {
    fn new(obj: SessionObj, constraints: impl Into<Option<BufferConstraints>>) -> Self {
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
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::screencopy::v1::server::ext_screencopy_manager_v1::ExtScreencopyManagerV1: $crate::wayland::protocols::screencopy::ScreencopyGlobalData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::screencopy::v1::server::ext_screencopy_manager_v1::ExtScreencopyManagerV1: $crate::wayland::protocols::screencopy::ScreencopyData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::screencopy::v1::server::ext_screencopy_session_v1::ExtScreencopySessionV1: $crate::wayland::protocols::screencopy::SessionData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::screencopy::v1::server::ext_screencopy_cursor_session_v1::ExtScreencopyCursorSessionV1: $crate::wayland::protocols::screencopy::CursorSessionData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::screencopy::v1::server::ext_screencopy_session_v1::ExtScreencopySessionV1: $crate::wayland::protocols::screencopy::CursorSessionData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::screencopy::v1::server::ext_screencopy_frame_v1::ExtScreencopyFrameV1: $crate::wayland::protocols::screencopy::FrameData
        ] => $crate::wayland::protocols::screencopy::ScreencopyState);
    };
}
pub(crate) use delegate_screencopy;
