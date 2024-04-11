// SPDX-License-Identifier: GPL-3.0-only

use smithay::utils::user_data::UserDataMap;

use crate::{
    shell::CosmicSurface,
    state::State,
    wayland::protocols::toplevel_info::{
        delegate_toplevel_info, ToplevelInfoHandler, ToplevelInfoState, Window,
    },
};

impl ToplevelInfoHandler for State {
    type Window = CosmicSurface;

    fn toplevel_info_state(&self) -> &ToplevelInfoState<State, Self::Window> {
        &self.common.shell.toplevel_info_state
    }
    fn toplevel_info_state_mut(&mut self) -> &mut ToplevelInfoState<State, Self::Window> {
        &mut self.common.shell.toplevel_info_state
    }
}

impl Window for CosmicSurface {
    fn title(&self) -> String {
        CosmicSurface::title(self)
    }

    fn app_id(&self) -> String {
        CosmicSurface::app_id(self)
    }

    fn is_activated(&self) -> bool {
        !self.is_minimized() && CosmicSurface::is_activated(self, true)
    }

    fn is_maximized(&self) -> bool {
        !self.is_minimized() && CosmicSurface::is_maximized(self, false)
    }

    fn is_fullscreen(&self) -> bool {
        !self.is_minimized() && CosmicSurface::is_fullscreen(self, false)
    }

    fn is_minimized(&self) -> bool {
        CosmicSurface::is_minimized(self)
    }

    fn user_data(&self) -> &UserDataMap {
        CosmicSurface::user_data(self)
    }
}

delegate_toplevel_info!(State, CosmicSurface);
