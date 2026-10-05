// SPDX-License-Identifier: GPL-3.0-only

//! Approval mode: while the trusted approver shows an approval window, the compositor keeps that
//! window above everything else (overlay layer surfaces included), gives it every key and click,
//! and lets no input injected through libei reach any surface.
//!
//! The approver is recognised by two facts fixed when the compositor is built: the executable its
//! Wayland client runs (`COSMIC_COMP_APPROVER_EXE`, from the client's credentials) and the app id
//! of its window (`COSMIC_COMP_APPROVER_APP_ID`). Without both, approval mode is off. Builds with
//! the `test-input` feature (test images only) let libei input through, so that test harnesses can
//! drive the window.

use std::path::Path;

use smithay::{
    desktop::space::SpaceElement,
    output::Output,
    reexports::wayland_server::{DisplayHandle, Resource},
    utils::{IsAlive, Point},
};

use crate::{
    shell::{CosmicSurface, SeatExt, Shell, focus::target::KeyboardFocusTarget},
    utils::prelude::{Global, OutputExt},
};

/// The approver's executable.
pub const APPROVER_EXE: Option<&str> = option_env!("COSMIC_COMP_APPROVER_EXE");
/// The app id of the approver's windows.
pub const APPROVER_APP_ID: Option<&str> = option_env!("COSMIC_COMP_APPROVER_APP_ID");

/// Whether input injected through libei reaches surfaces while an approval window is up.
pub const EI_DURING_APPROVAL: bool = cfg!(feature = "test-input");

/// Whether `exe` and `app_id` are the approver's.
pub fn matches(exe: &Path, app_id: &str) -> bool {
    match (APPROVER_EXE, APPROVER_APP_ID) {
        (Some(want_exe), Some(want_id)) => exe == Path::new(want_exe) && app_id == want_id,
        _ => false,
    }
}

/// Whether `window` is an approval window: a Wayland toplevel with the approver's app id whose
/// client runs the approver's executable.
pub fn is_approver(dh: &DisplayHandle, window: &CosmicSurface) -> bool {
    if APPROVER_EXE.is_none() || window.x11_surface().is_some() {
        return false;
    }
    let Some(toplevel) = window.0.toplevel() else {
        return false;
    };
    let Some(client) = toplevel.wl_surface().client() else {
        return false;
    };
    let Ok(credentials) = client.get_credentials(dh) else {
        return false;
    };
    std::fs::read_link(format!("/proc/{}/exe", credentials.pid))
        .is_ok_and(|exe| matches(&exe, &window.app_id()))
}

impl Shell {
    /// Whether an approval window is up.
    pub fn approval_active(&self) -> bool {
        self.approval.iter().any(IsAlive::alive)
    }

    /// Shows `window`, an approver's (see [`is_approver`]), as an approval window instead of
    /// mapping it on a workspace, and returns the focus it takes.
    pub fn map_approval(&mut self, window: CosmicSurface) -> KeyboardFocusTarget {
        self.pending_windows
            .retain(|pending| pending.surface != window);
        self.approval.retain(IsAlive::alive);
        window.set_activated(true);
        window.send_configure();
        self.approval.push(window.clone());
        KeyboardFocusTarget::Approval(window)
    }

    /// Forgets an approval window that went away; whether it was one.
    pub fn unmap_approval(&mut self, window: &CosmicSurface) -> bool {
        let before = self.approval.len();
        self.approval.retain(|w| w != window && w.alive());
        self.approval.len() != before
    }

    /// The newest approval window and where its surface goes on `output`: centred on the active
    /// output of the last active seat, on no other output.
    pub fn approval_on(&self, output: &Output) -> Option<(CosmicSurface, Point<i32, Global>)> {
        let window = self.approval.iter().rev().find(|w| w.alive())?;
        if &self.seats.last_active().active_output() != output {
            return None;
        }
        let area = output.geometry();
        let geometry = SpaceElement::geometry(window);
        let x = area.loc.x + (area.size.w - geometry.size.w).max(0) / 2 - geometry.loc.x;
        let y = area.loc.y + (area.size.h - geometry.size.h).max(0) / 2 - geometry.loc.y;
        Some((window.clone(), Point::from((x, y))))
    }
}

/// Whether libei input may reach surfaces now.
pub fn ei_allowed(approval_active: bool) -> bool {
    !approval_active || EI_DURING_APPROVAL
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn libei_input_waits_while_an_approval_is_up() {
        assert!(ei_allowed(false));
        assert_eq!(ei_allowed(true), EI_DURING_APPROVAL);
    }

    #[test]
    fn only_the_approver_matches() {
        match (APPROVER_EXE, APPROVER_APP_ID) {
            (Some(exe), Some(id)) => {
                assert!(matches(Path::new(exe), id));
                assert!(!matches(Path::new("/usr/bin/other"), id));
                assert!(!matches(Path::new(exe), "other.App"));
            }
            _ => assert!(!matches(Path::new("/usr/bin/x"), "x")),
        }
    }
}
