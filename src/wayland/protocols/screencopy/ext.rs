use std::sync::{Arc, Mutex};

use smithay::reexports::wayland_protocols::ext::screencopy::v1::server::{
    ext_screencopy_cursor_session_v1::{self, ExtScreencopyCursorSessionV1},
    ext_screencopy_frame_v1::{self, ExtScreencopyFrameV1},
    ext_screencopy_manager_v1::{self, ExtScreencopyManagerV1},
    ext_screencopy_session_v1::{self, ExtScreencopySessionV1},
};
use smithay::{
    backend::{
        allocator::Buffer,
        renderer::{buffer_type, BufferType},
    },
    utils::user_data::UserDataMap,
    wayland::{dmabuf::get_dmabuf, shm::with_buffer_contents},
};
use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource,
    },
    utils::Rectangle,
};
use tracing::debug;

use super::{super::image_source::ImageSourceData, *};

impl<D> GlobalDispatch<ExtScreencopyManagerV1, ScreencopyGlobalData, D> for ScreencopyState
where
    D: GlobalDispatch<ExtScreencopyManagerV1, ScreencopyGlobalData>
        + Dispatch<ExtScreencopyManagerV1, ScreencopyData>
        + Dispatch<ExtScreencopySessionV1, SessionData>
        + Dispatch<ExtScreencopySessionV1, CursorSessionData>
        + Dispatch<ExtScreencopyCursorSessionV1, CursorSessionData>
        + Dispatch<ExtScreencopyFrameV1, FrameData>
        + ScreencopyHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<ExtScreencopyManagerV1>,
        _global_data: &ScreencopyGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ScreencopyData);
    }

    fn can_view(client: Client, global_data: &ScreencopyGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ExtScreencopyManagerV1, ScreencopyData, D> for ScreencopyState
where
    D: Dispatch<ExtScreencopyManagerV1, ScreencopyData>
        + Dispatch<ExtScreencopySessionV1, SessionData>
        + Dispatch<ExtScreencopySessionV1, CursorSessionData>
        + Dispatch<ExtScreencopyCursorSessionV1, CursorSessionData>
        + Dispatch<ExtScreencopyFrameV1, FrameData>
        + ScreencopyHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &ExtScreencopyManagerV1,
        request: <ExtScreencopyManagerV1 as Resource>::Request,
        _data: &ScreencopyData,
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            ext_screencopy_manager_v1::Request::CreateSession {
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
                                obj: obj.into(),
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
                    obj: obj.into(),
                    inner: session_data,
                    user_data: Arc::new(UserDataMap::new()),
                };
                session.stop();
            }
            ext_screencopy_manager_v1::Request::CreatePointerCursorSession {
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
                                obj: obj.into(),
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
                    obj: obj.into(),
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
        _resource: &ExtScreencopyManagerV1,
        _data: &ScreencopyData,
    ) {
    }
}

impl<D> Dispatch<ExtScreencopySessionV1, SessionData, D> for ScreencopyState
where
    D: Dispatch<ExtScreencopySessionV1, SessionData>
        + Dispatch<ExtScreencopyFrameV1, FrameData>
        + ScreencopyHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &ExtScreencopySessionV1,
        request: <ExtScreencopySessionV1 as Resource>::Request,
        data: &SessionData,
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            ext_screencopy_session_v1::Request::CreateFrame { frame } => {
                let inner = Arc::new(Mutex::new(FrameInner::new(
                    resource.clone().into(),
                    data.inner.lock().unwrap().constraints.clone(),
                )));
                let obj = data_init.init(
                    frame,
                    FrameData {
                        inner: inner.clone(),
                    },
                );
                data.inner.lock().unwrap().active_frames.push(Frame {
                    obj: obj.into(),
                    inner,
                });
            }
            _ => {}
        }
    }

    fn destroyed(
        state: &mut D,
        _client: wayland_backend::server::ClientId,
        resource: &ExtScreencopySessionV1,
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

impl<D> Dispatch<ExtScreencopyCursorSessionV1, CursorSessionData, D> for ScreencopyState
where
    D: Dispatch<ExtScreencopyCursorSessionV1, CursorSessionData>
        + Dispatch<ExtScreencopySessionV1, CursorSessionData>
        + Dispatch<ExtScreencopyFrameV1, FrameData>
        + ScreencopyHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &ExtScreencopyCursorSessionV1,
        request: <ExtScreencopyCursorSessionV1 as Resource>::Request,
        data: &CursorSessionData,
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            ext_screencopy_cursor_session_v1::Request::GetScreencopySession { session } => {
                let new_data = CursorSessionData {
                    inner: data.inner.clone(),
                };
                let session = data_init.init(session, new_data);

                let mut inner = data.inner.lock().unwrap();
                if inner.session.is_some() {
                    resource.post_error(
                        ext_screencopy_cursor_session_v1::Error::DuplicateSession,
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
                inner.session = Some(session.into());
            }
            _ => {}
        }
    }

    fn destroyed(
        state: &mut D,
        _client: wayland_backend::server::ClientId,
        resource: &ExtScreencopyCursorSessionV1,
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

impl<D> Dispatch<ExtScreencopySessionV1, CursorSessionData, D> for ScreencopyState
where
    D: Dispatch<ExtScreencopySessionV1, SessionData>
        + Dispatch<ExtScreencopyFrameV1, FrameData>
        + ScreencopyHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &ExtScreencopySessionV1,
        request: <ExtScreencopySessionV1 as Resource>::Request,
        data: &CursorSessionData,
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            ext_screencopy_session_v1::Request::CreateFrame { frame } => {
                let inner = Arc::new(Mutex::new(FrameInner::new(
                    resource.clone().into(),
                    data.inner.lock().unwrap().constraints.clone(),
                )));
                let obj = data_init.init(
                    frame,
                    FrameData {
                        inner: inner.clone(),
                    },
                );
                data.inner.lock().unwrap().active_frames.push(Frame {
                    obj: obj.into(),
                    inner,
                });
            }
            _ => {}
        }
    }

    fn destroyed(
        _state: &mut D,
        _client: wayland_backend::server::ClientId,
        _resource: &ExtScreencopySessionV1,
        _data: &CursorSessionData,
    ) {
    }
}

impl<D> Dispatch<ExtScreencopyFrameV1, FrameData, D> for ScreencopyState
where
    D: Dispatch<ExtScreencopyFrameV1, FrameData> + ScreencopyHandler + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &ExtScreencopyFrameV1,
        request: <ExtScreencopyFrameV1 as Resource>::Request,
        data: &FrameData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            ext_screencopy_frame_v1::Request::AttachBuffer { buffer } => {
                let mut inner = data.inner.lock().unwrap();

                if inner.capture_requested {
                    resource.post_error(
                        ext_screencopy_frame_v1::Error::AlreadyCaptured,
                        "Frame was captured previously",
                    );
                }

                inner.buffer = Some(buffer);
            }
            ext_screencopy_frame_v1::Request::DamageBuffer {
                x,
                y,
                width,
                height,
            } => {
                let mut inner = data.inner.lock().unwrap();

                if inner.capture_requested {
                    resource.post_error(
                        ext_screencopy_frame_v1::Error::AlreadyCaptured,
                        "Frame was captured previously",
                    );
                }

                if x < 0 || y < 0 || width <= 0 || height <= 0 {
                    resource.post_error(
                        ext_screencopy_frame_v1::Error::InvalidBufferDamage,
                        "Coordinates negative or size equal to zero",
                    );
                    return;
                }

                inner
                    .damage
                    .push(Rectangle::from_loc_and_size((x, y), (width, height)));
            }
            ext_screencopy_frame_v1::Request::Capture => {
                let mut inner = data.inner.lock().unwrap();

                if inner.capture_requested {
                    resource.post_error(
                        ext_screencopy_frame_v1::Error::AlreadyCaptured,
                        "Frame was captured previously",
                    );
                }

                if inner.buffer.is_none() {
                    resource.post_error(
                        ext_screencopy_frame_v1::Error::NoBuffer,
                        "Attempting to capture frame without a buffer",
                    );
                }

                inner.capture_requested = true;

                if let Some(reason) = inner.failed {
                    resource.failed((reason as u32).try_into().unwrap());
                    return;
                }

                if let Some(constraints) = inner.constraints.as_ref() {
                    let buffer = inner.buffer.as_ref().unwrap();
                    match buffer_type(buffer) {
                        Some(BufferType::Dma) => {
                            let Some(dma_constraints) = constraints.dma.as_ref() else {
                                debug!("dma buffer not specified for screencopy");
                                inner.failed = Some(FailureReason::BufferConstraints);
                                resource.failed(
                                    ext_screencopy_frame_v1::FailureReason::BufferConstraints,
                                );
                                return;
                            };

                            let dmabuf = match get_dmabuf(buffer) {
                                Ok(buf) => buf,
                                Err(err) => {
                                    debug!(?err, "Error accessing dma buffer for screencopy");
                                    inner.failed = Some(FailureReason::Unknown);
                                    resource
                                        .failed(ext_screencopy_frame_v1::FailureReason::Unknown);
                                    return;
                                }
                            };

                            let buffer_size = dmabuf.size();
                            if buffer_size.w < constraints.size.w
                                || buffer_size.h < constraints.size.h
                            {
                                debug!(?buffer_size, ?constraints.size, "buffer too small for screencopy");
                                inner.failed = Some(FailureReason::BufferConstraints);
                                resource.failed(
                                    ext_screencopy_frame_v1::FailureReason::BufferConstraints,
                                );
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
                                resource.failed(
                                    ext_screencopy_frame_v1::FailureReason::BufferConstraints,
                                );
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
                                    resource
                                        .failed(ext_screencopy_frame_v1::FailureReason::Unknown);
                                    return;
                                }
                            };

                            if buffer_data.width < constraints.size.w
                                || buffer_data.height < constraints.size.h
                            {
                                debug!(?buffer_data, ?constraints.size, "buffer too small for screencopy");
                                inner.failed = Some(FailureReason::BufferConstraints);
                                resource.failed(
                                    ext_screencopy_frame_v1::FailureReason::BufferConstraints,
                                );
                                return;
                            }

                            if !constraints.shm.contains(&buffer_data.format) {
                                debug!(?buffer_data.format, ?constraints.shm, "unsupported buffer format for screencopy");
                                inner.failed = Some(FailureReason::BufferConstraints);
                                resource.failed(
                                    ext_screencopy_frame_v1::FailureReason::BufferConstraints,
                                );
                                return;
                            }
                        }
                        x => {
                            debug!(?x, "Attempt to screencopy with unsupported buffer type");
                            inner.failed = Some(FailureReason::BufferConstraints);
                            resource
                                .failed(ext_screencopy_frame_v1::FailureReason::BufferConstraints);
                            return;
                        }
                    }
                } else {
                    inner.failed = Some(FailureReason::Unknown);
                    resource.failed(ext_screencopy_frame_v1::FailureReason::Unknown);
                    return;
                }

                let frame = Frame {
                    obj: resource.clone().into(),
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
                        resource.failed(ext_screencopy_frame_v1::FailureReason::Stopped);
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
                        resource.failed(ext_screencopy_frame_v1::FailureReason::Stopped);
                        return;
                    }

                    std::mem::drop(inner);
                    state.cursor_frame(session, frame);
                } else {
                    inner.failed = Some(FailureReason::Unknown);
                    resource.failed(ext_screencopy_frame_v1::FailureReason::Unknown);
                }
            }
            _ => {}
        }
    }

    fn destroyed(
        state: &mut D,
        _client: wayland_backend::server::ClientId,
        resource: &ExtScreencopyFrameV1,
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
            obj: resource.clone().into(),
            inner: data.inner.clone(),
        };
        state.frame_aborted(frame);
    }
}
