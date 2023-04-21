// SPDX-License-Identifier: GPL-3.0-only

use smithay::utils::user_data::UserDataMap;

use crate::{
    shell::CosmicSurface,
    state::State,
    wayland::protocols::ext_foreign_toplevel_list::{
        delegate_foreign_toplevel_info, ToplevelInfoHandler, ToplevelInfoState, Window,
    },
};

impl ToplevelInfoHandler for State {
    type Window = CosmicSurface;

    fn toplevel_info_state(&self) -> &ToplevelInfoState<State, Self::Window> {
        &self.common.shell.foreign_toplevel_info_state
    }
    fn toplevel_info_state_mut(&mut self) -> &mut ToplevelInfoState<State, Self::Window> {
        &mut self.common.shell.foreign_toplevel_info_state
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
        CosmicSurface::is_activated(self)
    }

    fn is_maximized(&self) -> bool {
        CosmicSurface::is_maximized(self)
    }

    fn is_fullscreen(&self) -> bool {
        CosmicSurface::is_fullscreen(self)
    }

    fn is_minimized(&self) -> bool {
        false // TODO
    }

    fn user_data(&self) -> &UserDataMap {
        CosmicSurface::user_data(self)
    }
}

delegate_foreign_toplevel_info!(State, CosmicSurface);
