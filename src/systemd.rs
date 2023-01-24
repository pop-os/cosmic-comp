// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use libsystemd::daemon::{booted, notify, NotifyState};
use std::process::Command;

pub fn ready(state: &State) {
    if booted() {
        match Command::new("systemctl")
            .args(["--user", "import-environment", "WAYLAND_DISPLAY", "DISPLAY"])
            .env("WAYLAND_DISPLAY", &state.common.socket)
            .env(
                "DISPLAY",
                &state
                    .common
                    .xwayland_state
                    .values()
                    .find_map(|s| s.display.map(|v| format!(":{}", v)))
                    .unwrap_or(String::new()),
            )
            .status()
        {
            Ok(x) if x.success() => {}
            Ok(x) => slog_scope::warn!(
                "Failed to import WAYLAND_DISPLAY/DISPLAY into systemd (exit code {:?})",
                x.code()
            ),
            Err(err) => slog_scope::error!(
                "Failed to run systemctl although booted with systemd: {}",
                err
            ),
        };

        if let Err(err) = notify(false, &[NotifyState::Ready]) {
            slog_scope::error!("Failed to notify systemd: {}", err);
        }
    }
}
