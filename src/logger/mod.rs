// SPDX-License-Identifier: GPL-3.0-only

#[cfg(feature = "debug")]
use std::{
    collections::VecDeque,
    sync::{Arc, Mutex, atomic::{AtomicBool, Ordering}},
};

use anyhow::Result;
use slog::Drain;

#[cfg(feature = "debug")]
mod serializer;

#[cfg(feature = "debug")]
const MAX_RECORDS: usize = 1000;

#[cfg(feature = "debug")]
pub type LogBuffer = Arc<Mutex<VecDeque<OwnedRecord>>>;

#[cfg(feature = "debug")]
#[derive(Clone)]
struct DebugDrain {
    buffer: LogBuffer,
    dirty_flag:Arc<AtomicBool>,
}

pub struct LogState {
    _guard: slog_scope::GlobalLoggerGuard,
    #[cfg(feature = "debug")]
    pub dirty_flag:Arc<AtomicBool>,
    #[cfg(feature = "debug")]
    pub debug_buffer: LogBuffer,
}

#[cfg(feature = "debug")]
pub struct OwnedRecord {
    pub message: String,
    pub level: slog::Level,
    pub kv: serde_json::map::Map<String, serde_json::Value>,
}

#[cfg(feature = "debug")]
impl DebugDrain {
    fn new() -> (DebugDrain, LogBuffer, Arc<AtomicBool>) {
        let dirty_flag = Arc::new(AtomicBool::new(false));
        let buffer = Arc::new(Mutex::new(VecDeque::new()));
        (
            DebugDrain {
                buffer: buffer.clone(),
                dirty_flag: dirty_flag.clone()
            },
            buffer,
            dirty_flag,
        )
    }
}

#[cfg(feature = "debug")]
impl Drain for DebugDrain {
    type Ok = ();
    type Err = slog::Error;

    fn log(
        &self,
        record: &slog::Record<'_>,
        values: &slog::OwnedKVList
    ) -> Result<Self::Ok, Self::Err> {
        use slog::KV;
        use serializer::SerdeSerializer;
        use serde_json::value::{
            Serializer as ValueSerializer,
            Value,
        };

        let mut serializer = SerdeSerializer::start(ValueSerializer, None)?;
        values.serialize(record, &mut serializer)?;
        record.kv().serialize(record, &mut serializer)?;
        let value = match serializer.end().map_err(|_| slog::Error::Other)? {
            Value::Object(map) => map,
            _ => unreachable!(),
        };

        let mut buffer = self.buffer.lock().unwrap();
        buffer.push_front(OwnedRecord {
            message: format!("{}", record.msg()),
            level: record.level(),
            kv: value,
        });
        buffer.truncate(MAX_RECORDS);
        self.dirty_flag.store(true, Ordering::SeqCst);

        Ok(())
    }
}

pub fn init_logger() -> Result<LogState> {
    let decorator = slog_term::TermDecorator::new().stderr().build();
    // usually we would not want to use a Mutex here, but this is usefull for a prototype,
    // to make sure we do not miss any in-flight messages, when we crash.
    #[cfg(not(feature = "debug"))]
    let logger = slog::Logger::root(
        std::sync::Mutex::new(
            slog_term::CompactFormat::new(decorator)
                .build()
                .ignore_res(),
        )
        .fuse(),
        slog::o!(),
    );
    #[cfg(feature = "debug")]
    let (debug_drain, debug_buffer, dirty_flag) = DebugDrain::new();
    #[cfg(feature = "debug")]
    let logger = slog::Logger::root(
        slog::Duplicate::new(
            std::sync::Mutex::new(
                slog_term::CompactFormat::new(decorator)
                    .build()
                    .ignore_res()
            ),
            debug_drain,
        )
        .fuse(),
        slog::o!(),
    );

    let _guard = slog_scope::set_global_logger(logger);
    slog_stdlog::init().unwrap();

    slog_scope::info!("Version: {}", std::env!("CARGO_PKG_VERSION"));
    if cfg!(feature = "debug") {
        slog_scope::debug!(
            "Debug build ({})",
            std::option_env!("GIT_HASH").unwrap_or("Unknown")
        );
    }

    Ok(LogState {
        _guard,
        #[cfg(feature = "debug")]
        debug_buffer,
        #[cfg(feature = "debug")]
        dirty_flag,
    })
}