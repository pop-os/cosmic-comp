// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::delegate_tablet_manager;

delegate_tablet_manager!(State);
