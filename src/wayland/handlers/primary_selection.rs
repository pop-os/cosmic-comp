// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    delegate_primary_selection,
    wayland::selection::primary_selection::{PrimarySelectionHandler, PrimarySelectionState},
};

impl PrimarySelectionHandler for State {
    fn primary_selection_state(&mut self) -> &mut PrimarySelectionState {
        &mut self.common.primary_selection_state
    }
}

delegate_primary_selection!(State);
