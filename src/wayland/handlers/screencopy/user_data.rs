use std::{
    cell::RefCell,
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use smithay::{
    backend::renderer::{damage::OutputDamageTracker, utils::CommitCounter},
    output::Output,
    reexports::wayland_server::{protocol::wl_buffer::WlBuffer, Resource, Weak},
};

use crate::{
    shell::{CosmicSurface, Workspace},
    wayland::protocols::screencopy::{CursorSession, FailureReason, Frame, Session},
};

type ScreencopySessionsData = RefCell<ScreencopySessions>;
type PendingScreencopyBuffers = RefCell<Vec<(Session, DropableFrame)>>;

pub type SessionData = RefCell<SessionUserData>;

pub struct SessionUserData {
    pub dt: OutputDamageTracker,
    commit_counter: CommitCounter,
    buffer_age: HashMap<Weak<WlBuffer>, CommitCounter>,
}

impl SessionUserData {
    pub fn new(tracker: OutputDamageTracker) -> SessionUserData {
        SessionUserData {
            dt: tracker,
            commit_counter: CommitCounter::default(),
            buffer_age: HashMap::new(),
        }
    }

    pub fn age_for_buffer(&mut self, buffer: &WlBuffer) -> usize {
        self.buffer_age.retain(|k, _| k.upgrade().is_ok());

        let weak = buffer.downgrade();
        let age = self
            .commit_counter
            .distance(self.buffer_age.get(&weak).copied())
            .unwrap_or(0);
        self.buffer_age.insert(weak, self.commit_counter);

        self.commit_counter.increment();
        age
    }

    pub fn reset(&mut self) {
        self.commit_counter = CommitCounter::default();
        self.buffer_age.clear();
    }
}

#[derive(Debug, Default)]
pub struct ScreencopySessions {
    sessions: Vec<DropableSession>,
    cursor_sessions: Vec<DropableCursorSession>,
}

pub trait SessionHolder {
    fn add_session(&mut self, session: Session);
    fn remove_session(&mut self, session: Session);
    fn sessions(&self) -> Vec<Session>;

    fn add_cursor_session(&mut self, session: CursorSession);
    fn remove_cursor_session(&mut self, session: CursorSession);
    fn cursor_sessions(&self) -> Vec<CursorSession>;
}

pub trait FrameHolder {
    fn add_frame(&mut self, session: Session, frame: Frame);
    fn remove_frame(&mut self, frame: &Frame);
    fn take_pending_frames(&self) -> Vec<(Session, Frame)>;
}

impl SessionHolder for Output {
    fn add_session(&mut self, session: Session) {
        self.user_data()
            .insert_if_missing(ScreencopySessionsData::default);
        self.user_data()
            .get::<ScreencopySessionsData>()
            .unwrap()
            .borrow_mut()
            .sessions
            .push(DropableSession(Some(session)));
    }

    fn remove_session(&mut self, session: Session) {
        self.user_data()
            .get::<ScreencopySessionsData>()
            .unwrap()
            .borrow_mut()
            .sessions
            .retain(|s| *s != session);
    }

    fn sessions(&self) -> Vec<Session> {
        self.user_data()
            .get::<ScreencopySessionsData>()
            .map_or(Vec::new(), |sessions| {
                sessions
                    .borrow()
                    .sessions
                    .iter()
                    .flat_map(|s| s.0.clone())
                    .collect()
            })
    }

    fn add_cursor_session(&mut self, session: CursorSession) {
        self.user_data()
            .insert_if_missing(ScreencopySessionsData::default);
        self.user_data()
            .get::<ScreencopySessionsData>()
            .unwrap()
            .borrow_mut()
            .cursor_sessions
            .push(DropableCursorSession(Some(session)));
    }

    fn remove_cursor_session(&mut self, session: CursorSession) {
        self.user_data()
            .get::<ScreencopySessionsData>()
            .unwrap()
            .borrow_mut()
            .cursor_sessions
            .retain(|s| *s != session);
    }

    fn cursor_sessions(&self) -> Vec<CursorSession> {
        self.user_data()
            .get::<ScreencopySessionsData>()
            .map_or(Vec::new(), |sessions| {
                sessions
                    .borrow()
                    .cursor_sessions
                    .iter()
                    .flat_map(|s| s.0.clone())
                    .collect()
            })
    }
}

impl FrameHolder for Output {
    fn add_frame(&mut self, session: Session, frame: Frame) {
        self.user_data()
            .insert_if_missing(PendingScreencopyBuffers::default);
        self.user_data()
            .get::<PendingScreencopyBuffers>()
            .unwrap()
            .borrow_mut()
            .push((session, DropableFrame(Some(frame))));
    }
    fn remove_frame(&mut self, frame: &Frame) {
        if let Some(pending) = self.user_data().get::<PendingScreencopyBuffers>() {
            pending.borrow_mut().retain(|(_, f)| f != frame);
        }
    }
    fn take_pending_frames(&self) -> Vec<(Session, Frame)> {
        self.user_data()
            .get::<PendingScreencopyBuffers>()
            .map(|pending| {
                pending
                    .borrow_mut()
                    .split_off(0)
                    .into_iter()
                    .map(|(s, mut f)| (s, f.0.take().unwrap()))
                    .collect()
            })
            .unwrap_or_default()
    }
}

impl SessionHolder for Workspace {
    fn add_session(&mut self, session: Session) {
        self.screencopy
            .sessions
            .push(DropableSession(Some(session)));
    }

    fn remove_session(&mut self, session: Session) {
        self.screencopy.sessions.retain(|s| *s != session);
    }
    fn sessions(&self) -> Vec<Session> {
        self.screencopy
            .sessions
            .iter()
            .flat_map(|s| s.0.clone())
            .collect()
    }

    fn add_cursor_session(&mut self, session: CursorSession) {
        self.screencopy
            .cursor_sessions
            .push(DropableCursorSession(Some(session)));
    }

    fn remove_cursor_session(&mut self, session: CursorSession) {
        self.screencopy.cursor_sessions.retain(|s| *s != session);
    }
    fn cursor_sessions(&self) -> Vec<CursorSession> {
        self.screencopy
            .cursor_sessions
            .iter()
            .flat_map(|s| s.0.clone())
            .collect()
    }
}

impl SessionHolder for CosmicSurface {
    fn add_session(&mut self, session: Session) {
        self.user_data()
            .insert_if_missing(ScreencopySessionsData::default);
        self.user_data()
            .get::<ScreencopySessionsData>()
            .unwrap()
            .borrow_mut()
            .sessions
            .push(DropableSession(Some(session)));
    }

    fn remove_session(&mut self, session: Session) {
        self.user_data()
            .get::<ScreencopySessionsData>()
            .unwrap()
            .borrow_mut()
            .sessions
            .retain(|s| *s != session);
    }
    fn sessions(&self) -> Vec<Session> {
        self.user_data()
            .get::<ScreencopySessionsData>()
            .map_or(Vec::new(), |sessions| {
                sessions
                    .borrow()
                    .sessions
                    .iter()
                    .flat_map(|s| s.0.clone())
                    .collect()
            })
    }

    fn add_cursor_session(&mut self, session: CursorSession) {
        self.user_data()
            .insert_if_missing(ScreencopySessionsData::default);
        self.user_data()
            .get::<ScreencopySessionsData>()
            .unwrap()
            .borrow_mut()
            .cursor_sessions
            .push(DropableCursorSession(Some(session)));
    }

    fn remove_cursor_session(&mut self, session: CursorSession) {
        self.user_data()
            .get::<ScreencopySessionsData>()
            .unwrap()
            .borrow_mut()
            .cursor_sessions
            .retain(|s| *s != session);
    }

    fn cursor_sessions(&self) -> Vec<CursorSession> {
        self.user_data()
            .get::<ScreencopySessionsData>()
            .map_or(Vec::new(), |sessions| {
                sessions
                    .borrow()
                    .cursor_sessions
                    .iter()
                    .flat_map(|s| s.0.clone())
                    .collect()
            })
    }
}

#[derive(Debug)]
struct DropableSession(Option<Session>);
impl Deref for DropableSession {
    type Target = Session;
    fn deref(&self) -> &Self::Target {
        self.0.as_ref().unwrap()
    }
}
impl DerefMut for DropableSession {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.as_mut().unwrap()
    }
}
impl PartialEq<Session> for DropableSession {
    fn eq(&self, other: &Session) -> bool {
        self.0.as_ref().map(|s| s == other).unwrap_or(false)
    }
}
impl Drop for DropableSession {
    fn drop(&mut self) {
        if let Some(s) = self.0.take() {
            s.stop();
        }
    }
}

#[derive(Debug)]
struct DropableCursorSession(Option<CursorSession>);
impl Deref for DropableCursorSession {
    type Target = CursorSession;
    fn deref(&self) -> &Self::Target {
        self.0.as_ref().unwrap()
    }
}
impl DerefMut for DropableCursorSession {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.as_mut().unwrap()
    }
}
impl PartialEq<CursorSession> for DropableCursorSession {
    fn eq(&self, other: &CursorSession) -> bool {
        self.0.as_ref().map(|s| s == other).unwrap_or(false)
    }
}
impl Drop for DropableCursorSession {
    fn drop(&mut self) {
        if let Some(s) = self.0.take() {
            s.stop();
        }
    }
}

#[derive(Debug)]
pub struct DropableFrame(Option<Frame>);
impl Deref for DropableFrame {
    type Target = Frame;
    fn deref(&self) -> &Self::Target {
        self.0.as_ref().unwrap()
    }
}
impl DerefMut for DropableFrame {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.as_mut().unwrap()
    }
}
impl PartialEq<Frame> for DropableFrame {
    fn eq(&self, other: &Frame) -> bool {
        self.0.as_ref().map(|f| f == other).unwrap_or(false)
    }
}
impl Drop for DropableFrame {
    fn drop(&mut self) {
        if let Some(f) = self.0.take() {
            f.fail(FailureReason::Unknown);
        }
    }
}
