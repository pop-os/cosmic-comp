// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::{CosmicSurface, Shell},
    state::State,
};
use smithay::wayland::foreign_toplevel_list::{
    ForeignToplevelHandle, ForeignToplevelListHandler, ForeignToplevelListState,
};
use std::sync::Mutex;

impl ForeignToplevelListHandler for State {
    fn foreign_toplevel_list_state(&mut self) -> &mut ForeignToplevelListState {
        &mut self.common.foreign_toplevel_list
    }
}

pub fn new_foreign_toplevel(
    window: &CosmicSurface,
    foreign_toplevel_list: &mut ForeignToplevelListState,
) {
    let toplevel_handle =
        foreign_toplevel_list.new_toplevel::<State>(window.title(), window.app_id());
    *window
        .user_data()
        .get_or_insert::<Mutex<Option<ForeignToplevelHandle>>, _>(Default::default)
        .lock()
        .unwrap() = Some(toplevel_handle);
}

pub fn remove_foreign_toplevel(
    window: &CosmicSurface,
    foreign_toplevel_list: &mut ForeignToplevelListState,
) {
    if let Some(handle) = window
        .user_data()
        .get::<Mutex<Option<ForeignToplevelHandle>>>()
    {
        if let Some(handle) = handle.lock().unwrap().take() {
            foreign_toplevel_list.remove_toplevel(&handle);
        }
    }
}

pub fn refresh_foreign_toplevels(shell: &Shell) {
    for (window, _) in shell
        .workspaces
        .spaces()
        .flat_map(|workspace| workspace.mapped())
        .flat_map(|mapped| mapped.windows())
    {
        let foreign_toplevel_handle = window
            .user_data()
            .get::<Mutex<Option<ForeignToplevelHandle>>>()
            .and_then(|handle| handle.lock().unwrap().clone());
        if let Some(handle) = foreign_toplevel_handle {
            let app_id = window.app_id();
            let title = window.title();
            if handle.app_id() != app_id || handle.title() != title {
                handle.send_app_id(&app_id);
                handle.send_title(&title);
                handle.send_done();
            }
        }
    }
}

smithay::delegate_foreign_toplevel_list!(State);
