// SPDX-License-Identifier: GPL-3.0-only

use std::str::FromStr;
use std::sync::{Arc, Mutex};

use anyhow::Result;

use tracing::{debug, info, warn};
use tracing_journald as journald;
use tracing_subscriber::{EnvFilter, filter::Directive, fmt, prelude::*};

/// Tracing target for the gaming/game-mode instrumentation (tearing/VRR/
/// frame-pacing). Enabled in debug builds only. Use
/// `info!(target: GAMING_TARGET, ...)`.
pub const GAMING_TARGET: &str = "gaming";

/// A simple `Write` sink over a shared file, so all layers can append.
#[derive(Clone)]
struct FileWriter(Arc<Mutex<std::fs::File>>);

impl std::io::Write for FileWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.lock().unwrap().write(buf)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.0.lock().unwrap().flush()
    }
}

/// Where to write the on-disk log. Defaults to `$XDG_RUNTIME_DIR/cosmic-comp.log`
/// (e.g. `/run/user/1000/cosmic-comp.log`) so it survives a cosmic-session launch
/// that swallows stdout; override with `COSMIC_COMP_LOG_FILE`.
fn log_file_path() -> String {
    std::env::var("COSMIC_COMP_LOG_FILE").unwrap_or_else(|_| {
        let dir = std::env::var("XDG_RUNTIME_DIR").unwrap_or_else(|_| "/tmp".to_string());
        format!("{dir}/cosmic-comp.log")
    })
}

pub fn init_logger() -> Result<()> {
    let level = if cfg!(debug_assertions) {
        "debug"
    } else {
        "warn"
    };
    // These are DEFAULTS: only applied for targets RUST_LOG didn't mention. add_directive
    // replaces a target's directive, so applying them unconditionally would clobber the
    // operator's own setting (`RUST_LOG=...,cosmic_comp=info` silently became warn in release).
    let env = std::env::var(EnvFilter::DEFAULT_ENV).ok();
    let unset = |target: &str| !env.as_deref().is_some_and(|e| e.contains(target));
    let mut filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| {
        EnvFilter::new(if cfg!(debug_assertions) {
            "info"
        } else {
            "warn"
        })
    });
    for (target, lvl) in [
        ("cosmic_text", "error"),
        ("calloop", "error"),
        ("smithay", level),
        ("cosmic_comp", level),
        // Session-handoff marks: a few lines per login, and the thing most often
        // needed on a machine where nobody set RUST_LOG.
        (crate::utils::timing::HANDOFF_TARGET, "info"),
        // Which tier night shift landed on per output. One line per change, and
        // otherwise invisible in release, where the base filter is `warn`.
        (crate::backend::kms::NIGHT_SHIFT_LOG_TARGET, "info"),
    ] {
        if unset(target) {
            filter = filter.add_directive(Directive::from_str(&format!("{target}={lvl}")).unwrap());
        }
    }

    // Game-mode / window instrumentation (target: GAMING_TARGET) is OPT-IN — in
    // debug builds too, so normal runs never carry the noise. Enable at runtime:
    //   COSMIC_GAME_TRACE=1     -> `gaming=debug` (state transitions + window/X11 lifecycle)
    //   COSMIC_GAME_TRACE=trace -> `gaming=trace` (adds the per-frame render firehose)
    // Left unset, the `gaming` target falls under the base filter (warn in release),
    // so these callsites cost a level check and emit nothing. With it set,
    // `grep gaming $XDG_RUNTIME_DIR/cosmic-comp.log` is the whole game-mode story.
    let game_trace_level = match std::env::var("COSMIC_GAME_TRACE").ok().as_deref() {
        Some("2") | Some("trace") | Some("verbose") => Some("trace"),
        Some(v) if ["1", "true", "yes", "y"].contains(&v.to_lowercase().as_str()) => Some("debug"),
        _ => None,
    };
    let filter = match game_trace_level {
        Some(level) => {
            filter.add_directive(Directive::from_str(&format!("{GAMING_TARGET}={level}")).unwrap())
        }
        None => filter,
    };

    let fmt_layer = fmt::layer().compact();

    // On-disk log layer (best-effort — skipped if the file can't be opened).
    let log_path = log_file_path();
    // Keep one previous generation: many game-mode repros need a compositor or
    // game relaunch, and truncate-on-launch would wipe the log that captured the
    // bug. Best-effort single rotation to `<log>.1` before we truncate.
    let _ = std::fs::rename(&log_path, format!("{log_path}.1"));
    let file_layer = std::fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(&log_path)
        .ok()
        .map(|file| {
            let writer = FileWriter(Arc::new(Mutex::new(file)));
            fmt::layer()
                .with_ansi(false)
                .with_writer(move || writer.clone())
        });

    // `file_layer` is applied first (directly on the registry) in both arms so
    // its subscriber type is consistently `Registry` (matching `fmt_layer`).
    match journald::layer() {
        Ok(journald_layer) => tracing_subscriber::registry()
            .with(file_layer)
            .with(fmt_layer)
            .with(journald_layer)
            .with(filter)
            .init(),
        Err(err) => {
            tracing_subscriber::registry()
                .with(file_layer)
                .with(fmt_layer)
                .with(filter)
                .init();
            warn!(?err, "Failed to init journald logging.");
        }
    };
    log_panics::init();

    info!("Logging to file: {log_path}");

    info!("Version: {}", std::env!("CARGO_PKG_VERSION"));
    if cfg!(feature = "debug") {
        debug!(
            "Debug build ({})",
            std::option_env!("GIT_HASH").unwrap_or("Unknown")
        );
    }

    // Log performance profiling environment variables
    info!("Performance profiling env vars:");
    info!(
        "  COSMIC_PERF_LOG={} (set =1 for periodic frame stats)",
        std::env::var("COSMIC_PERF_LOG").unwrap_or_else(|_| "unset".into())
    );
    info!(
        "  COSMIC_GPU_DIAG={} (set =1 for GPU diagnostics at startup)",
        std::env::var("COSMIC_GPU_DIAG").unwrap_or_else(|_| "unset".into())
    );
    info!(
        "  COSMIC_BLUR_DOWNSAMPLE_FACTOR={} (override blur downsample)",
        std::env::var("COSMIC_BLUR_DOWNSAMPLE_FACTOR").unwrap_or_else(|_| "auto".into())
    );
    info!(
        "  COSMIC_BLUR_DOWNSAMPLE={} (set =0 to disable blur downsampling)",
        std::env::var("COSMIC_BLUR_DOWNSAMPLE").unwrap_or_else(|_| "unset".into())
    );
    info!(
        "  COSMIC_GAME_TRACE={} (=1 game-mode/window trace in release; =trace adds per-frame)",
        std::env::var("COSMIC_GAME_TRACE").unwrap_or_else(|_| "unset".into())
    );

    Ok(())
}
