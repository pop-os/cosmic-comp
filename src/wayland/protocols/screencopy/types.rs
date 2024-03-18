pub use cosmic_protocols::screencopy::v2::server::zcosmic_screencopy_frame_v2::FailureReason;
use cosmic_protocols::screencopy::v2::server::{
    zcosmic_screencopy_cursor_session_v2::ZcosmicScreencopyCursorSessionV2,
    zcosmic_screencopy_frame_v2::ZcosmicScreencopyFrameV2,
    zcosmic_screencopy_session_v2::ZcosmicScreencopySessionV2,
};
use smithay::reexports::wayland_protocols::ext::screencopy::v1::server::{
    ext_screencopy_cursor_session_v1::ExtScreencopyCursorSessionV1,
    ext_screencopy_frame_v1::ExtScreencopyFrameV1,
    ext_screencopy_session_v1::ExtScreencopySessionV1,
};
use smithay::reexports::wayland_server::{
    protocol::{wl_output, wl_shm},
    Resource,
};

#[derive(Debug, Clone, PartialEq)]
pub enum SessionObj {
    Cosmic(ZcosmicScreencopySessionV2),
    Ext(ExtScreencopySessionV1),
}
impl From<ZcosmicScreencopySessionV2> for SessionObj {
    fn from(val: ZcosmicScreencopySessionV2) -> Self {
        SessionObj::Cosmic(val)
    }
}
impl From<ExtScreencopySessionV1> for SessionObj {
    fn from(val: ExtScreencopySessionV1) -> Self {
        SessionObj::Ext(val)
    }
}
impl PartialEq<ZcosmicScreencopySessionV2> for SessionObj {
    fn eq(&self, other: &ZcosmicScreencopySessionV2) -> bool {
        match self {
            SessionObj::Cosmic(val) => val == other,
            _ => false,
        }
    }
}
impl PartialEq<ExtScreencopySessionV1> for SessionObj {
    fn eq(&self, other: &ExtScreencopySessionV1) -> bool {
        match self {
            SessionObj::Ext(val) => val == other,
            _ => false,
        }
    }
}
impl SessionObj {
    pub fn is_alive(&self) -> bool {
        match self {
            SessionObj::Cosmic(val) => val.is_alive(),
            SessionObj::Ext(val) => val.is_alive(),
        }
    }

    pub fn buffer_size(&self, w: u32, h: u32) {
        match self {
            SessionObj::Cosmic(val) => val.buffer_size(w, h),
            SessionObj::Ext(val) => val.buffer_size(w, h),
        }
    }

    pub fn shm_format(&self, format: wl_shm::Format) {
        match self {
            SessionObj::Cosmic(val) => val.shm_format(format as u32),
            SessionObj::Ext(val) => val.shm_format(format),
        }
    }

    pub fn dmabuf_device(&self, dev: Vec<u8>) {
        match self {
            SessionObj::Cosmic(val) => val.dmabuf_device(dev),
            SessionObj::Ext(val) => val.dmabuf_device(dev),
        }
    }

    pub fn dmabuf_format(&self, format: u32, modifiers: Vec<u8>) {
        match self {
            SessionObj::Cosmic(val) => val.dmabuf_format(format, modifiers),
            SessionObj::Ext(val) => val.dmabuf_format(format, modifiers),
        }
    }

    pub fn done(&self) {
        match self {
            SessionObj::Cosmic(val) => val.done(),
            SessionObj::Ext(val) => val.done(),
        }
    }

    pub fn stopped(&self) {
        match self {
            SessionObj::Cosmic(val) => val.done(),
            SessionObj::Ext(val) => val.done(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CursorSessionObj {
    Cosmic(ZcosmicScreencopyCursorSessionV2),
    Ext(ExtScreencopyCursorSessionV1),
}
impl From<ZcosmicScreencopyCursorSessionV2> for CursorSessionObj {
    fn from(val: ZcosmicScreencopyCursorSessionV2) -> Self {
        CursorSessionObj::Cosmic(val)
    }
}
impl From<ExtScreencopyCursorSessionV1> for CursorSessionObj {
    fn from(val: ExtScreencopyCursorSessionV1) -> Self {
        CursorSessionObj::Ext(val)
    }
}
impl PartialEq<ZcosmicScreencopyCursorSessionV2> for CursorSessionObj {
    fn eq(&self, other: &ZcosmicScreencopyCursorSessionV2) -> bool {
        match self {
            CursorSessionObj::Cosmic(val) => val == other,
            _ => false,
        }
    }
}
impl PartialEq<ExtScreencopyCursorSessionV1> for CursorSessionObj {
    fn eq(&self, other: &ExtScreencopyCursorSessionV1) -> bool {
        match self {
            CursorSessionObj::Ext(val) => val == other,
            _ => false,
        }
    }
}
impl CursorSessionObj {
    pub fn is_alive(&self) -> bool {
        match self {
            CursorSessionObj::Cosmic(val) => val.is_alive(),
            CursorSessionObj::Ext(val) => val.is_alive(),
        }
    }
    pub fn enter(&self) {
        match self {
            CursorSessionObj::Cosmic(val) => val.enter(),
            CursorSessionObj::Ext(val) => val.enter(),
        }
    }
    pub fn hotspot(&self, x: i32, y: i32) {
        match self {
            CursorSessionObj::Cosmic(val) => val.hotspot(x, y),
            CursorSessionObj::Ext(val) => val.hotspot(x, y),
        }
    }
    pub fn position(&self, x: i32, y: i32) {
        match self {
            CursorSessionObj::Cosmic(val) => val.position(x, y),
            CursorSessionObj::Ext(val) => val.position(x, y),
        }
    }
    pub fn leave(&self) {
        match self {
            CursorSessionObj::Cosmic(val) => val.leave(),
            CursorSessionObj::Ext(val) => val.leave(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FrameObj {
    Cosmic(ZcosmicScreencopyFrameV2),
    Ext(ExtScreencopyFrameV1),
}
impl From<ZcosmicScreencopyFrameV2> for FrameObj {
    fn from(val: ZcosmicScreencopyFrameV2) -> Self {
        FrameObj::Cosmic(val)
    }
}
impl From<ExtScreencopyFrameV1> for FrameObj {
    fn from(val: ExtScreencopyFrameV1) -> Self {
        FrameObj::Ext(val)
    }
}
impl PartialEq<ZcosmicScreencopyFrameV2> for FrameObj {
    fn eq(&self, other: &ZcosmicScreencopyFrameV2) -> bool {
        match self {
            FrameObj::Cosmic(val) => val == other,
            _ => false,
        }
    }
}
impl PartialEq<ExtScreencopyFrameV1> for FrameObj {
    fn eq(&self, other: &ExtScreencopyFrameV1) -> bool {
        match self {
            FrameObj::Ext(val) => val == other,
            _ => false,
        }
    }
}

impl FrameObj {
    pub fn transform(&self, transform: wl_output::Transform) {
        match self {
            FrameObj::Cosmic(val) => val.transform(transform),
            FrameObj::Ext(val) => val.transform(transform),
        }
    }
    pub fn damage(&self, x: i32, y: i32, w: i32, h: i32) {
        match self {
            FrameObj::Cosmic(val) => val.damage(x, y, w, h),
            FrameObj::Ext(val) => val.damage(x, y, w, h),
        }
    }
    pub fn presentation_time(&self, sec_hi: u32, sec_lo: u32, nsec: u32) {
        match self {
            FrameObj::Cosmic(val) => val.presentation_time(sec_hi, sec_lo, nsec),
            FrameObj::Ext(val) => val.presentation_time(sec_hi, sec_lo, nsec),
        }
    }
    pub fn ready(&self) {
        match self {
            FrameObj::Cosmic(val) => val.ready(),
            FrameObj::Ext(val) => val.ready(),
        }
    }
    pub fn failed(&self, reason: FailureReason) {
        match self {
            FrameObj::Cosmic(val) => val.failed(reason),
            FrameObj::Ext(val) => val.failed((reason as u32).try_into().unwrap()),
        }
    }
}
