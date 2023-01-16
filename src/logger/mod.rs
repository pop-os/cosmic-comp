// SPDX-License-Identifier: GPL-3.0-only

use anyhow::Result;
use slog::Drain;

pub struct LogState {
    _guard: slog_scope::GlobalLoggerGuard,
}

pub fn init_logger() -> Result<LogState> {
    let decorator = slog_term::TermDecorator::new().stderr().build();
    let term_drain = slog_term::CompactFormat::new(decorator)
        .build()
        .ignore_res();
    let journald_drain = slog_journald::JournaldDrain.ignore_res();
    let drain = slog::Duplicate::new(term_drain, journald_drain);
    // usually we would not want to use a Mutex here, but this is usefull for a prototype,
    // to make sure we do not miss any in-flight messages, when we crash.
    let logger = slog::Logger::root(std::sync::Mutex::new(drain).fuse(), slog::o!());

    let _guard = slog_scope::set_global_logger(logger);
    slog_stdlog::init().unwrap();
    log_panics::init();

    slog_scope::info!("Version: {}", std::env!("CARGO_PKG_VERSION"));
    if cfg!(feature = "debug") {
        slog_scope::debug!(
            "Debug build ({})",
            std::option_env!("GIT_HASH").unwrap_or("Unknown")
        );
    }

    Ok(LogState { _guard })
}
