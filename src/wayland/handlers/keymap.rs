// SPDX-License-Identifier: GPL-3.0-only
//
use crate::{state::State, wayland::protocols::keymap::delegate_keymap};

delegate_keymap!(State);
