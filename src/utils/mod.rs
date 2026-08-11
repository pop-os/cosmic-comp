// SPDX-License-Identifier: GPL-3.0-only

pub mod apply;
pub mod deadlock;
pub mod env;
mod ids;
pub mod memlog;
pub(crate) use self::ids::id_gen;
pub mod geometry;
pub mod global;
pub mod iced;
mod iced_keymap;
pub mod iced_profiler;
pub mod prelude;
pub mod process;
pub mod quirks;
pub mod rlimit;
pub mod screenshot;
pub mod timing;
pub mod tween;
pub mod xdg_icon;
