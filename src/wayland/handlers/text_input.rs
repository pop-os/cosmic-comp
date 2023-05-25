// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::delegate_text_input_manager;

delegate_text_input_manager!(State);
