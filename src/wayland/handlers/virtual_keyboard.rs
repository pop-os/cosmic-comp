// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::delegate_virtual_keyboard_manager;

delegate_virtual_keyboard_manager!(State);
