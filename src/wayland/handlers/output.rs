// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{delegate_output, wayland::output::OutputHandler};

impl OutputHandler for State {}

delegate_output!(State);
