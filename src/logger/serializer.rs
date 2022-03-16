// SPDX-License-Identifier: MPL-2.0 OR MIT OR Apache-2.0
// Taken from slog_json:
// https://github.com/slog-rs/json/blob/c45d09422d7114222ee5b7f6b0123de2605c7864/src/lib.rs#L39-L160

use serde::ser::SerializeMap;
use serde::serde_if_integer128;
use slog::Key;
use std::{fmt, io, result};

use std::cell::RefCell;
use std::fmt::Write;

thread_local! {
    static TL_BUF: RefCell<String> = RefCell::new(String::with_capacity(128))
}

/// `slog::Serializer` adapter for `serde::Serializer`
///
/// Newtype to wrap serde Serializer, so that `Serialize` can be implemented
/// for it
pub struct SerdeSerializer<S: serde::Serializer> {
    /// Current state of map serializing: `serde::Serializer::MapState`
    ser_map: S::SerializeMap,
}

impl<S: serde::Serializer> SerdeSerializer<S> {
    /// Start serializing map of values
    pub fn start(ser: S, len: Option<usize>) -> result::Result<Self, slog::Error> {
        let ser_map = ser.serialize_map(len).map_err(|e| {
            io::Error::new(
                io::ErrorKind::Other,
                format!("serde serialization error: {}", e),
            )
        })?;
        Ok(SerdeSerializer { ser_map })
    }

    /// Finish serialization, and return the serializer
    pub fn end(self) -> result::Result<S::Ok, S::Error> {
        self.ser_map.end()
    }
}

macro_rules! impl_m(
    ($s:expr, $key:expr, $val:expr) => ({
        let k_s:  &str = $key.as_ref();
        $s.ser_map.serialize_entry(k_s, $val)
             .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("serde serialization error: {}", e)))?;
        Ok(())
    });
);

impl<S> slog::Serializer for SerdeSerializer<S>
where
    S: serde::Serializer,
{
    fn emit_bool(&mut self, key: Key, val: bool) -> slog::Result {
        impl_m!(self, key, &val)
    }

    fn emit_unit(&mut self, key: Key) -> slog::Result {
        impl_m!(self, key, &())
    }

    fn emit_char(&mut self, key: Key, val: char) -> slog::Result {
        impl_m!(self, key, &val)
    }

    fn emit_none(&mut self, key: Key) -> slog::Result {
        let val: Option<()> = None;
        impl_m!(self, key, &val)
    }
    fn emit_u8(&mut self, key: Key, val: u8) -> slog::Result {
        impl_m!(self, key, &val)
    }
    fn emit_i8(&mut self, key: Key, val: i8) -> slog::Result {
        impl_m!(self, key, &val)
    }
    fn emit_u16(&mut self, key: Key, val: u16) -> slog::Result {
        impl_m!(self, key, &val)
    }
    fn emit_i16(&mut self, key: Key, val: i16) -> slog::Result {
        impl_m!(self, key, &val)
    }
    fn emit_usize(&mut self, key: Key, val: usize) -> slog::Result {
        impl_m!(self, key, &val)
    }
    fn emit_isize(&mut self, key: Key, val: isize) -> slog::Result {
        impl_m!(self, key, &val)
    }
    fn emit_u32(&mut self, key: Key, val: u32) -> slog::Result {
        impl_m!(self, key, &val)
    }
    fn emit_i32(&mut self, key: Key, val: i32) -> slog::Result {
        impl_m!(self, key, &val)
    }
    fn emit_f32(&mut self, key: Key, val: f32) -> slog::Result {
        impl_m!(self, key, &val)
    }
    fn emit_u64(&mut self, key: Key, val: u64) -> slog::Result {
        impl_m!(self, key, &val)
    }
    fn emit_i64(&mut self, key: Key, val: i64) -> slog::Result {
        impl_m!(self, key, &val)
    }
    fn emit_f64(&mut self, key: Key, val: f64) -> slog::Result {
        impl_m!(self, key, &val)
    }
    serde_if_integer128! {
        fn emit_u128(&mut self, key: Key, val: u128) -> slog::Result {
            impl_m!(self, key, &val)
        }
        fn emit_i128(&mut self, key: Key, val: i128) -> slog::Result {
            impl_m!(self, key, &val)
        }
    }
    fn emit_str(&mut self, key: Key, val: &str) -> slog::Result {
        impl_m!(self, key, &val)
    }
    fn emit_arguments(&mut self, key: Key, val: &fmt::Arguments) -> slog::Result {
        TL_BUF.with(|buf| {
            let mut buf = buf.borrow_mut();

            buf.write_fmt(*val).unwrap();

            let res = { || impl_m!(self, key, &*buf) }();
            buf.clear();
            res
        })
    }
}
