// SPDX-License-Identifier: GPL-3.0-only

use std::str::FromStr;

use anyhow::Result;

use tracing::{debug, info, warn};
use tracing_journald as journald;
use tracing_subscriber::{EnvFilter, filter::Directive, fmt, prelude::*};

pub fn init_logger() -> Result<()> {
    let level = if cfg!(debug_assertions) {
        "debug"
    } else {
        "warn"
    };
    let filter = EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| {
            EnvFilter::new(if cfg!(debug_assertions) {
                "info"
            } else {
                "warn"
            })
        })
        .add_directive(Directive::from_str("cosmic_text=error").unwrap())
        .add_directive(Directive::from_str("calloop=error").unwrap())
        .add_directive(Directive::from_str(&format!("smithay={level}")).unwrap())
        .add_directive(Directive::from_str(&format!("cosmic_comp={level}")).unwrap());

    let fmt_layer = fmt::layer().compact();

    match journald::layer() {
        Ok(journald_layer) => tracing_subscriber::registry()
            .with(fmt_layer)
            .with(journald_layer)
            .with(filter)
            .init(),
        Err(err) => {
            tracing_subscriber::registry()
                .with(fmt_layer)
                .with(filter)
                .init();
            warn!(?err, "Failed to init journald logging.");
        }
    };
    log_panics::init();

    info!("Version: {}", std::env!("CARGO_PKG_VERSION"));
    if cfg!(feature = "debug") {
        debug!(
            "Debug build ({})",
            std::option_env!("GIT_HASH").unwrap_or("Unknown")
        );
    }

    Ok(())
}
