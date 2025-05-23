// SPDX-License-Identifier: GPL-3.0-only

use crate::{shell::SessionLock, state::State, utils::prelude::*};
use smithay::{
    delegate_session_lock,
    output::Output,
    reexports::wayland_server::{protocol::wl_output::WlOutput, Resource},
    utils::Size,
    wayland::session_lock::{
        LockSurface, SessionLockHandler, SessionLockManagerState, SessionLocker,
    },
};
use std::collections::HashMap;

impl SessionLockHandler for State {
    fn lock_state(&mut self) -> &mut SessionLockManagerState {
        &mut self.common.session_lock_manager_state
    }

    fn lock(&mut self, locker: SessionLocker) {
        let mut shell = self.common.shell.write();

        // Reject lock if sesion lock exists and is still valid
        if let Some(session_lock) = shell.session_lock.as_ref() {
            if self
                .common
                .display_handle
                .get_client(session_lock.ext_session_lock.id())
                .is_ok()
            {
                return;
            }
        }

        let ext_session_lock = locker.ext_session_lock().clone();
        locker.lock();
        shell.session_lock = Some(SessionLock {
            ext_session_lock,
            surfaces: HashMap::new(),
        });

        for output in shell.outputs() {
            self.backend.schedule_render(&output);
        }
    }

    fn unlock(&mut self) {
        let mut shell = self.common.shell.write();
        shell.session_lock = None;

        for output in shell.outputs() {
            self.backend.schedule_render(&output);
        }
    }

    fn new_surface(&mut self, lock_surface: LockSurface, wl_output: WlOutput) {
        let mut shell = self.common.shell.write();
        if let Some(session_lock) = &mut shell.session_lock {
            if let Some(output) = Output::from_resource(&wl_output) {
                lock_surface.with_pending_state(|states| {
                    let size = output.geometry().size;
                    states.size = Some(Size::from((size.w as u32, size.h as u32)));
                });
                lock_surface.send_configure();
                session_lock
                    .surfaces
                    .insert(output.clone(), lock_surface.clone());
            }
        }
    }
}

delegate_session_lock!(State);
