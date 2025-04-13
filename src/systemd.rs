// SPDX-License-Identifier: GPL-3.0-only

use crate::state::Common;
use libsystemd::daemon::{booted, notify, NotifyState};
use std::process::Command;
use tracing::{error, warn};

pub fn ready(common: &Common) {
    if booted() {
        match Command::new("systemctl")
            .args(["--user", "import-environment", "WAYLAND_DISPLAY", "DISPLAY"])
            .env("WAYLAND_DISPLAY", &common.socket)
            .env(
                "DISPLAY",
                &common
                    .xwayland_state
                    .as_ref()
                    .map(|s| format!(":{}", s.display))
                    .unwrap_or(String::new()),
            )
            .status()
        {
            Ok(x) if x.success() => {}
            Ok(x) => warn!(
                exit_code = ?x.code(),
                "Failed to import WAYLAND_DISPLAY/DISPLAY into systemd",
            ),
            Err(err) => error!(?err, "Failed to run systemctl although booted with systemd",),
        };

        if let Err(err) = notify(false, &[NotifyState::Ready]) {
            error!(?err, "Failed to notify systemd");
        }
    }
}
