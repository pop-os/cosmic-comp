// SPDX-License-Identifier: GPL-3.0-only

//! Session-handoff timing marks.
//!
//! One `info!` line per boundary, so a login or logout can be reconstructed from the
//! journal (`journalctl -o short-unix | grep handoff-timing`). Each mark carries ms
//! since this process's `run()` entry; the journal timestamp aligns marks across the
//! outgoing and incoming compositors, and the gap between them is the greetd +
//! session-script + cosmic-session cost, which nothing else measures.

use std::sync::OnceLock;
use std::time::Instant;

static START: OnceLock<Instant> = OnceLock::new();

/// Start this process's clock; called once at the top of `run()`.
pub fn init() {
    let _ = START.set(Instant::now());
}

/// Record a handoff boundary.
pub fn mark(phase: &str) {
    let t_ms = START.get().map_or(0, |s| s.elapsed().as_millis());
    tracing::debug!("handoff-timing: {phase} (+{t_ms}ms)");
}
