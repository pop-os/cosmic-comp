use std::{cell::RefCell, collections::HashMap, sync::Mutex};

use smithay::{
    backend::renderer::{damage::OutputDamageTracker, utils::CommitCounter},
    output::Output,
    reexports::wayland_server::{Resource, Weak, protocol::wl_buffer::WlBuffer},
};

use crate::{
    shell::{CosmicSurface, Workspace},
    wayland::protocols::screencopy::{
        CursorSession, CursorSessionRef, Frame, FrameRef, Session, SessionRef,
    },
};

type ScreencopySessionsData = RefCell<ScreencopySessions>;
type PendingScreencopyBuffers = Mutex<Vec<(SessionRef, Frame)>>;

pub type SessionData = Mutex<SessionUserData>;

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
    sessions: Vec<Session>,
    cursor_sessions: Vec<CursorSession>,
}

pub trait SessionHolder {
    fn add_session(&mut self, session: Session);
    fn remove_session(&mut self, session: &SessionRef);
    fn sessions(&self) -> Vec<SessionRef>;

    fn add_cursor_session(&mut self, session: CursorSession);
    fn remove_cursor_session(&mut self, session: &CursorSessionRef);
    fn cursor_sessions(&self) -> Vec<CursorSessionRef>;
}

pub trait FrameHolder {
    fn add_frame(&mut self, session: SessionRef, frame: Frame);
    fn remove_frame(&mut self, frame: &FrameRef);
    fn take_pending_frames(&self) -> Vec<(SessionRef, Frame)>;
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
            .push(session);
    }

    fn remove_session(&mut self, session: &SessionRef) {
        self.user_data()
            .get::<ScreencopySessionsData>()
            .unwrap()
            .borrow_mut()
            .sessions
            .retain(|s| s != session);
    }

    fn sessions(&self) -> Vec<SessionRef> {
        self.user_data()
            .get::<ScreencopySessionsData>()
            .map_or(Vec::new(), |sessions| {
                sessions
                    .borrow()
                    .sessions
                    .iter()
                    .map(|s| (*s).clone())
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
            .push(session);
    }

    fn remove_cursor_session(&mut self, session: &CursorSessionRef) {
        self.user_data()
            .get::<ScreencopySessionsData>()
            .unwrap()
            .borrow_mut()
            .cursor_sessions
            .retain(|s| s != session);
    }

    fn cursor_sessions(&self) -> Vec<CursorSessionRef> {
        self.user_data()
            .get::<ScreencopySessionsData>()
            .map_or(Vec::new(), |sessions| {
                sessions
                    .borrow()
                    .cursor_sessions
                    .iter()
                    .map(|s| (*s).clone())
                    .collect()
            })
    }
}

impl FrameHolder for Output {
    fn add_frame(&mut self, session: SessionRef, frame: Frame) {
        self.user_data()
            .insert_if_missing_threadsafe(PendingScreencopyBuffers::default);
        self.user_data()
            .get::<PendingScreencopyBuffers>()
            .unwrap()
            .lock()
            .unwrap()
            .push((session, frame));
    }
    fn remove_frame(&mut self, frame: &FrameRef) {
        if let Some(pending) = self.user_data().get::<PendingScreencopyBuffers>() {
            pending.lock().unwrap().retain(|(_, f)| f != frame);
        }
    }
    fn take_pending_frames(&self) -> Vec<(SessionRef, Frame)> {
        self.user_data()
            .get::<PendingScreencopyBuffers>()
            .map(|pending| std::mem::take(&mut *pending.lock().unwrap()))
            .unwrap_or_default()
    }
}

impl SessionHolder for Workspace {
    fn add_session(&mut self, session: Session) {
        self.screencopy.sessions.push(session);
    }

    fn remove_session(&mut self, session: &SessionRef) {
        self.screencopy.sessions.retain(|s| s != session);
    }
    fn sessions(&self) -> Vec<SessionRef> {
        self.screencopy
            .sessions
            .iter()
            .map(|s| (*s).clone())
            .collect()
    }

    fn add_cursor_session(&mut self, session: CursorSession) {
        self.screencopy.cursor_sessions.push(session);
    }

    fn remove_cursor_session(&mut self, session: &CursorSessionRef) {
        self.screencopy.cursor_sessions.retain(|s| s != session);
    }
    fn cursor_sessions(&self) -> Vec<CursorSessionRef> {
        self.screencopy
            .cursor_sessions
            .iter()
            .map(|s| (*s).clone())
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
            .push(session);
    }

    fn remove_session(&mut self, session: &SessionRef) {
        self.user_data()
            .get::<ScreencopySessionsData>()
            .unwrap()
            .borrow_mut()
            .sessions
            .retain(|s| s != session);
    }
    fn sessions(&self) -> Vec<SessionRef> {
        self.user_data()
            .get::<ScreencopySessionsData>()
            .map_or(Vec::new(), |sessions| {
                sessions
                    .borrow()
                    .sessions
                    .iter()
                    .map(|s| (*s).clone())
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
            .push(session);
    }

    fn remove_cursor_session(&mut self, session: &CursorSessionRef) {
        self.user_data()
            .get::<ScreencopySessionsData>()
            .unwrap()
            .borrow_mut()
            .cursor_sessions
            .retain(|s| s != session);
    }

    fn cursor_sessions(&self) -> Vec<CursorSessionRef> {
        self.user_data()
            .get::<ScreencopySessionsData>()
            .map_or(Vec::new(), |sessions| {
                sessions
                    .borrow()
                    .cursor_sessions
                    .iter()
                    .map(|s| (*s).clone())
                    .collect()
            })
    }
}
