use calloop::LoopHandle;
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    sync::atomic::{AtomicBool, Ordering},
    time::{Duration, Instant},
};
use tracing::warn;
use wayland_backend::server::ClientId;

use cosmic_protocols::workspace::v1::server::zcosmic_workspace_handle_v1::State as WState;
use keyframe::{ease, functions::EaseInOutCubic};
use smithay::{
    desktop::{
        layer_map_for_output, space::SpaceElement, LayerSurface, PopupManager, WindowSurfaceType,
    },
    input::{
        pointer::{Focus, GrabStartData as PointerGrabStartData},
        Seat,
    },
    output::Output,
    reexports::wayland_server::{protocol::wl_surface::WlSurface, Client, DisplayHandle},
    utils::{Point, Rectangle, Serial, SERIAL_COUNTER},
    wayland::{
        compositor::with_states,
        seat::WaylandFocus,
        shell::{
            wlr_layer::{
                KeyboardInteractivity, Layer, LayerSurfaceCachedState, WlrLayerShellState,
            },
            xdg::XdgShellState,
        },
    },
    xwayland::X11Surface,
};

use crate::{
    config::{Config, KeyModifiers, KeyPattern, WorkspaceMode},
    state::client_has_security_context,
    utils::prelude::*,
    wayland::protocols::{
        toplevel_info::ToplevelInfoState,
        toplevel_management::{ManagementCapabilities, ToplevelManagementState},
        workspace::{
            WorkspaceCapabilities, WorkspaceGroupHandle, WorkspaceHandle, WorkspaceState,
            WorkspaceUpdateGuard,
        },
    },
};

pub mod element;
pub mod focus;
pub mod grabs;
pub mod layout;
mod workspace;
pub use self::element::{CosmicMapped, CosmicMappedRenderElement, CosmicSurface};
pub use self::workspace::*;
use self::{
    element::{
        resize_indicator::{resize_indicator, ResizeIndicator},
        swap_indicator::{swap_indicator, SwapIndicator},
        CosmicWindow,
    },
    focus::target::KeyboardFocusTarget,
    grabs::ResizeEdge,
    layout::{floating::ResizeState, tiling::NodeDesc},
};

const ANIMATION_DURATION: Duration = Duration::from_millis(200);

#[derive(Debug, Clone)]
pub enum Trigger {
    KeyboardSwap(KeyPattern, NodeDesc),
    KeyboardMove(KeyModifiers),
    Pointer(u32),
}

#[derive(Debug, Clone)]
pub enum OverviewMode {
    None,
    Started(Trigger, Instant),
    Ended(Option<Trigger>, Instant),
}

impl OverviewMode {
    pub fn alpha(&self) -> Option<f32> {
        match self {
            OverviewMode::Started(_, start) => {
                let percentage = Instant::now().duration_since(*start).as_millis() as f32
                    / ANIMATION_DURATION.as_millis() as f32;
                Some(ease(EaseInOutCubic, 0.0, 1.0, percentage))
            }
            OverviewMode::Ended(_, end) => {
                let percentage = Instant::now().duration_since(*end).as_millis() as f32
                    / ANIMATION_DURATION.as_millis() as f32;
                if percentage < 1.0 {
                    Some(ease(EaseInOutCubic, 1.0, 0.0, percentage))
                } else {
                    None
                }
            }
            OverviewMode::None => None,
        }
    }
}

#[derive(Debug, Clone, Copy, serde::Deserialize, PartialEq, Eq, Hash)]
pub enum ResizeDirection {
    Inwards,
    Outwards,
}

#[derive(Debug, Clone)]
pub enum ResizeMode {
    None,
    Started(KeyPattern, Instant, ResizeDirection),
    Ended(Instant, ResizeDirection),
}

impl ResizeMode {
    pub fn alpha(&self) -> Option<f32> {
        match self {
            ResizeMode::Started(_, start, _) => {
                let percentage = Instant::now().duration_since(*start).as_millis() as f32
                    / ANIMATION_DURATION.as_millis() as f32;
                Some(ease(EaseInOutCubic, 0.0, 1.0, percentage))
            }
            ResizeMode::Ended(end, _) => {
                let percentage = Instant::now().duration_since(*end).as_millis() as f32
                    / ANIMATION_DURATION.as_millis() as f32;
                if percentage < 1.0 {
                    Some(ease(EaseInOutCubic, 1.0, 0.0, percentage))
                } else {
                    None
                }
            }
            ResizeMode::None => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MaximizeMode {
    Floating,
    OnTop,
}

#[derive(Debug)]
pub struct Shell {
    pub workspaces: Workspaces,

    pub popups: PopupManager,
    pub maximize_mode: MaximizeMode,
    pub pending_windows: Vec<(CosmicSurface, Seat<State>, Option<Output>)>,
    pub pending_layers: Vec<(LayerSurface, Output, Seat<State>)>,
    pub override_redirect_windows: Vec<X11Surface>,

    // wayland_state
    pub layer_shell_state: WlrLayerShellState,
    pub toplevel_info_state: ToplevelInfoState<State, CosmicSurface>,
    pub toplevel_management_state: ToplevelManagementState,
    pub xdg_shell_state: XdgShellState,
    pub workspace_state: WorkspaceState<State>,

    theme: cosmic::Theme,
    overview_mode: OverviewMode,
    swap_indicator: Option<SwapIndicator>,
    resize_mode: ResizeMode,
    resize_state: Option<(
        KeyboardFocusTarget,
        ResizeDirection,
        ResizeEdge,
        i32,
        usize,
        Output,
    )>,
    resize_indicator: Option<ResizeIndicator>,
}

#[derive(Debug)]
pub struct WorkspaceSet {
    previously_active: Option<(usize, Instant)>,
    active: usize,
    group: WorkspaceGroupHandle,
    idx: usize,
    tiling_enabled: bool,
    output: Output,
    theme: cosmic::Theme,
    pub(crate) workspaces: Vec<Workspace>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum WorkspaceAmount {
    Dynamic,
    Static(u8),
}

fn create_workspace(
    state: &mut WorkspaceUpdateGuard<'_, State>,
    output: &Output,
    group_handle: &WorkspaceGroupHandle,
    active: bool,
    tiling: bool,
    theme: cosmic::Theme,
) -> Workspace {
    let workspace_handle = state.create_workspace(&group_handle).unwrap();
    if active {
        state.add_workspace_state(&workspace_handle, WState::Active);
    }
    state.set_workspace_capabilities(
        &workspace_handle,
        [WorkspaceCapabilities::Activate].into_iter(),
    );
    Workspace::new(workspace_handle, output.clone(), tiling, theme.clone())
}

impl WorkspaceSet {
    fn new(
        state: &mut WorkspaceUpdateGuard<'_, State>,
        group_handle: WorkspaceGroupHandle,
        output: &Output,
        amount: WorkspaceAmount,
        idx: usize,
        tiling_enabled: bool,
        theme: cosmic::Theme,
    ) -> WorkspaceSet {
        let workspaces = match amount {
            WorkspaceAmount::Dynamic => {
                let workspace = create_workspace(
                    state,
                    output,
                    &group_handle,
                    true,
                    tiling_enabled,
                    theme.clone(),
                );
                workspace_set_idx(state, 1, idx, &workspace.handle);
                state.set_workspace_capabilities(
                    &workspace.handle,
                    [WorkspaceCapabilities::Activate].into_iter(),
                );
                vec![workspace]
            }
            WorkspaceAmount::Static(len) => (0..len)
                .map(|i| {
                    let workspace = create_workspace(
                        state,
                        output,
                        &group_handle,
                        i == 0,
                        tiling_enabled,
                        theme.clone(),
                    );
                    workspace_set_idx(state, i + 1, idx, &workspace.handle);
                    state.set_workspace_capabilities(
                        &workspace.handle,
                        [WorkspaceCapabilities::Activate].into_iter(),
                    );
                    workspace
                })
                .collect(),
        };

        WorkspaceSet {
            previously_active: None,
            active: 0,
            group: group_handle,
            idx,
            tiling_enabled,
            theme,
            workspaces,
            output: output.clone(),
        }
    }

    fn activate(
        &mut self,
        idx: usize,
        state: &mut WorkspaceUpdateGuard<'_, State>,
    ) -> Result<bool, InvalidWorkspaceIndex> {
        if idx >= self.workspaces.len() {
            return Err(InvalidWorkspaceIndex);
        }

        if self.active != idx {
            let old_active = self.active;
            state.remove_workspace_state(&self.workspaces[old_active].handle, WState::Active);
            state.add_workspace_state(&self.workspaces[idx].handle, WState::Active);
            self.previously_active = Some((old_active, Instant::now()));
            self.active = idx;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn set_output(
        &mut self,
        new_output: &Output,
        toplevel_info: &mut ToplevelInfoState<State, CosmicSurface>,
    ) {
        for workspace in &mut self.workspaces {
            workspace.set_output(new_output, toplevel_info);
        }
        self.output = new_output.clone();
    }

    fn refresh<'a>(&mut self) {
        if let Some((_, start)) = self.previously_active {
            if Instant::now().duration_since(start).as_millis() >= ANIMATION_DURATION.as_millis() {
                self.previously_active = None;
            }
        } else {
            self.workspaces[self.active].refresh();
        }
    }

    fn add_empty_workspace(&mut self, state: &mut WorkspaceUpdateGuard<State>) {
        let workspace = create_workspace(
            state,
            &self.output,
            &self.group,
            false,
            self.tiling_enabled,
            self.theme.clone(),
        );
        workspace_set_idx(
            state,
            self.workspaces.len() as u8 + 1,
            self.idx,
            &workspace.handle,
        );
        self.workspaces.push(workspace);
    }

    fn ensure_last_empty<'a>(&mut self, state: &mut WorkspaceUpdateGuard<State>) {
        // add empty at the end, if necessary
        if self
            .workspaces
            .last()
            .map(|last| last.windows().next().is_some())
            .unwrap_or(true)
        {
            self.add_empty_workspace(state);
        }

        // remove empty workspaces in between, if they are not active
        let len = self.workspaces.len();
        let mut keep = vec![true; len];
        for (i, workspace) in self.workspaces.iter().enumerate() {
            let has_windows = workspace.windows().next().is_some();

            if !has_windows && i != self.active && i != len - 1 {
                state.remove_workspace(workspace.handle);
                keep[i] = false;
            }
        }

        let mut iter = keep.iter();
        self.workspaces.retain(|_| *iter.next().unwrap());
        self.active -= keep
            .iter()
            .take(self.active + 1)
            .filter(|keep| !**keep)
            .count();

        if keep.iter().any(|val| *val == false) {
            for (i, workspace) in self.workspaces.iter().enumerate() {
                workspace_set_idx(state, i as u8 + 1, self.idx, &workspace.handle);
            }
        }
    }

    fn ensure_static(
        &mut self,
        amount: usize,
        state: &mut WorkspaceUpdateGuard<State>,
        toplevel_info: &mut ToplevelInfoState<State, CosmicSurface>,
    ) {
        if amount < self.workspaces.len() {
            // merge last ones
            let overflow = self.workspaces.split_off(amount);
            if self.active >= self.workspaces.len() {
                self.active = self.workspaces.len() - 1;
                state.add_workspace_state(&self.workspaces[self.active].handle, WState::Active);
            }
            let last_space = self.workspaces.last_mut().unwrap();

            for mut workspace in overflow {
                if last_space.fullscreen.is_some() {
                    // Don't handle the returned original workspace, for this nieche case.
                    let _ = workspace.remove_fullscreen();
                }

                for element in workspace.mapped() {
                    // fixup toplevel state
                    for (toplevel, _) in element.windows() {
                        toplevel_info.toplevel_leave_workspace(&toplevel, &workspace.handle);
                        toplevel_info.toplevel_enter_workspace(&toplevel, &last_space.handle);
                    }
                }
                last_space.tiling_layer.merge(workspace.tiling_layer);
                last_space.floating_layer.merge(workspace.floating_layer);
                if workspace.fullscreen.is_some() {
                    last_space.fullscreen = workspace.fullscreen;
                }
                state.remove_workspace(workspace.handle);
            }

            last_space.refresh();
        } else if amount > self.workspaces.len() {
            // add empty ones
            while amount > self.workspaces.len() {
                let workspace = create_workspace(
                    state,
                    &self.output,
                    &self.group,
                    false,
                    self.tiling_enabled,
                    self.theme.clone(),
                );
                workspace_set_idx(
                    state,
                    self.workspaces.len() as u8 + 1,
                    self.idx,
                    &workspace.handle,
                );
                state.set_workspace_capabilities(
                    &workspace.handle,
                    [WorkspaceCapabilities::Activate].into_iter(),
                );
                self.workspaces.push(workspace);
            }
        }
    }

    fn update_idx(&mut self, state: &mut WorkspaceUpdateGuard<'_, State>, idx: usize) {
        self.idx = idx;
        for (i, workspace) in self.workspaces.iter().enumerate() {
            workspace_set_idx(state, i as u8 + 1, idx, &workspace.handle);
        }
    }

    fn update_tiling_status(&mut self, seat: &Seat<State>, tiling_enabled: bool) {
        self.tiling_enabled = tiling_enabled;
        for workspace in &mut self.workspaces {
            if workspace.tiling_enabled != tiling_enabled {
                workspace.toggle_tiling(seat);
            }
        }
    }
}

#[derive(Debug)]
pub struct Workspaces {
    sets: IndexMap<Output, WorkspaceSet>,
    backup_set: Option<WorkspaceSet>,
    amount: WorkspaceAmount,
    mode: WorkspaceMode,
    tiling_enabled: bool,
    theme: cosmic::Theme,
}

impl Workspaces {
    pub fn new(config: &Config, theme: cosmic::Theme) -> Workspaces {
        Workspaces {
            sets: IndexMap::new(),
            backup_set: None,
            amount: config.static_conf.workspace_amount,
            mode: config.static_conf.workspace_mode,
            tiling_enabled: config.static_conf.tiling_enabled,
            theme,
        }
    }

    pub fn add_output(
        &mut self,
        output: &Output,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        toplevel_info_state: &mut ToplevelInfoState<State, CosmicSurface>,
    ) {
        if self.sets.contains_key(output) {
            return;
        }

        let set = self
            .backup_set
            .take()
            .map(|mut set| {
                set.set_output(output, toplevel_info_state);
                set
            })
            .unwrap_or_else(|| {
                let group_handle = match self.mode {
                    WorkspaceMode::Global => self
                        .sets
                        .first()
                        .map(|(_, w)| w.group.clone())
                        .unwrap_or(workspace_state.create_workspace_group()),
                    WorkspaceMode::OutputBound => workspace_state.create_workspace_group(),
                };
                WorkspaceSet::new(
                    workspace_state,
                    group_handle,
                    &output,
                    self.amount,
                    self.sets.len(),
                    self.tiling_enabled,
                    self.theme.clone(),
                )
            });
        workspace_state.add_group_output(&set.group, &output);

        self.sets.insert(output.clone(), set);
        for workspace in &mut self.sets.get_mut(output).unwrap().workspaces {
            workspace.set_output(output, toplevel_info_state);
        }
    }

    pub fn remove_output(
        &mut self,
        output: &Output,
        seats: impl Iterator<Item = Seat<State>>,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        toplevel_info_state: &mut ToplevelInfoState<State, CosmicSurface>,
    ) {
        if !self.sets.contains_key(output) {
            return;
        }

        if let Some(set) = self.sets.remove(output) {
            {
                let map = layer_map_for_output(output);
                for surface in map.layers() {
                    surface.layer_surface().send_close();
                }
            }

            // TODO: Heuristic which output to move to.
            // It is supposed to be the *most* internal, we just pick the first one for now
            // and hope enumeration order works in our favor.
            let new_output = self.sets.get_index(0).map(|(o, _)| o.clone());
            if let Some(new_output) = new_output {
                for seat in seats {
                    if &seat.active_output() == output {
                        seat.set_active_output(&new_output);
                    }
                }

                let new_set = self.sets.get_mut(&new_output).unwrap();
                let workspace_group = new_set.group;
                for mut workspace in set.workspaces {
                    // update workspace protocol state
                    workspace_state.remove_workspace(workspace.handle);
                    let workspace_handle =
                        workspace_state.create_workspace(&workspace_group).unwrap();
                    workspace_state.set_workspace_capabilities(
                        &workspace_handle,
                        [WorkspaceCapabilities::Activate].into_iter(),
                    );
                    let old_workspace_handle = workspace.handle;
                    workspace.handle = workspace_handle;

                    for window in workspace.mapped() {
                        for (surface, _) in window.windows() {
                            toplevel_info_state
                                .toplevel_leave_workspace(&surface, &old_workspace_handle);
                            toplevel_info_state
                                .toplevel_enter_workspace(&surface, &workspace.handle);
                        }
                    }

                    // update mapping
                    workspace.set_output(&new_output, toplevel_info_state);
                    workspace.refresh();

                    // TODO: merge if mode = static
                    new_set.workspaces.push(workspace);
                }
                if self.mode == WorkspaceMode::OutputBound {
                    workspace_state.remove_workspace_group(set.group);
                } else {
                    workspace_state.remove_group_output(&workspace_group, output);
                }

                for (i, set) in self.sets.values_mut().enumerate() {
                    set.update_idx(workspace_state, i);
                }
            } else {
                workspace_state.remove_group_output(&set.group, output);
                self.backup_set = Some(set);
            }

            self.refresh(workspace_state, toplevel_info_state)
        }
    }

    pub fn update_config(
        &mut self,
        config: &Config,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        toplevel_info_state: &mut ToplevelInfoState<State, CosmicSurface>,
    ) {
        if self.sets.len() <= 1 {
            return;
        }

        match (self.mode, config.static_conf.workspace_mode) {
            (WorkspaceMode::Global, WorkspaceMode::OutputBound) => {
                // we need to create new separate workspace groups
                let old_group = self.sets.first().map(|(_, w)| w.group.clone()).unwrap();
                for (i, set) in self.sets.values_mut().enumerate() {
                    let new_group = workspace_state.create_workspace_group();
                    workspace_state.add_group_output(&new_group, &set.output);

                    for workspace in &mut set.workspaces {
                        // TODO: new protocol version allows workspaces to be reassigned,
                        // that would also avoid updating toplevel-info
                        let old_workspace_handle = workspace.handle;
                        workspace_state.remove_workspace(old_workspace_handle);
                        workspace.handle = workspace_state.create_workspace(&new_group).unwrap();
                        workspace_state.set_workspace_capabilities(
                            &workspace.handle,
                            [WorkspaceCapabilities::Activate].into_iter(),
                        );

                        for window in workspace.mapped() {
                            for (surface, _) in window.windows() {
                                toplevel_info_state
                                    .toplevel_leave_workspace(&surface, &old_workspace_handle);
                                toplevel_info_state
                                    .toplevel_enter_workspace(&surface, &workspace.handle);
                            }
                        }
                    }

                    set.update_idx(workspace_state, i);
                    workspace_state.remove_workspace_group(old_group);
                }
            }
            (WorkspaceMode::OutputBound, WorkspaceMode::Global) => {
                // lets construct an iterator of all the pairs of workspaces we have to "merge"
                let mut pairs = Vec::new();
                if let Some(max) = self.sets.values().map(|set| set.workspaces.len()).max() {
                    let offset = self.sets.values().map(|set| set.active).max().unwrap();
                    for i in 0..max {
                        pairs.push(
                            self.sets
                                .values()
                                .map(|set| {
                                    let idx = set.active as isize + i as isize - offset as isize;
                                    if idx < 0 || idx >= set.workspaces.len() as isize {
                                        None
                                    } else {
                                        Some(idx)
                                    }
                                })
                                .collect::<Vec<_>>(),
                        );
                    }
                }

                let group = workspace_state.create_workspace_group();
                for output in self.sets.keys() {
                    workspace_state.add_group_output(&group, output);
                }

                let old_workspace_groups = self.sets.values().map(|w| w.group).collect::<Vec<_>>();
                for (j, pair) in pairs.iter().enumerate() {
                    for (i, x) in pair.iter().enumerate() {
                        // Fill up sets, where necessary
                        if x.is_none() {
                            // create missing workspace
                            let (output, set) = self.sets.get_index_mut(i).unwrap();
                            set.workspaces.insert(
                                j,
                                create_workspace(
                                    workspace_state,
                                    output,
                                    &group,
                                    false,
                                    config.static_conf.tiling_enabled,
                                    self.theme.clone(),
                                ),
                            );
                        // Otherwise just update
                        } else {
                            let set = &mut self.sets[i];
                            let workspace = &mut set.workspaces[j];
                            let old_workspace_handle = workspace.handle;
                            workspace_state.remove_workspace(old_workspace_handle);
                            workspace.handle = workspace_state.create_workspace(&group).unwrap();
                            workspace_state.set_workspace_capabilities(
                                &workspace.handle,
                                [WorkspaceCapabilities::Activate].into_iter(),
                            );

                            for window in workspace.mapped() {
                                for (surface, _) in window.windows() {
                                    toplevel_info_state
                                        .toplevel_leave_workspace(&surface, &old_workspace_handle);
                                    toplevel_info_state
                                        .toplevel_enter_workspace(&surface, &workspace.handle);
                                }
                            }
                        }
                    }
                }
                for (i, set) in self.sets.values_mut().enumerate() {
                    set.group = group.clone();
                    set.update_idx(workspace_state, i);
                }
                for old_group in old_workspace_groups {
                    workspace_state.remove_workspace_group(old_group);
                }
            }
            _ => {}
        };

        self.refresh(workspace_state, toplevel_info_state)
    }

    pub fn refresh(
        &mut self,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        toplevel_info_state: &mut ToplevelInfoState<State, CosmicSurface>,
    ) {
        match self.mode {
            WorkspaceMode::Global => {
                match self.amount {
                    WorkspaceAmount::Dynamic => {
                        // this should never happen
                        let max = self
                            .sets
                            .values()
                            .map(|set| set.workspaces.len())
                            .max()
                            .unwrap_or_default();
                        for set in self
                            .sets
                            .values_mut()
                            .filter(|set| set.workspaces.len() < max)
                        {
                            while set.workspaces.len() < max {
                                set.add_empty_workspace(workspace_state)
                            }
                        }

                        // add empty at the end, if necessary
                        if self
                            .sets
                            .values()
                            .flat_map(|set| set.workspaces.last())
                            .any(|w| w.mapped().next().is_some())
                        {
                            for set in self.sets.values_mut() {
                                set.add_empty_workspace(workspace_state);
                            }
                        }

                        // remove empty workspaces in between, if they are not active
                        let len = self.sets[0].workspaces.len();
                        let mut active = self.sets[0].active;
                        let mut keep = vec![true; len];
                        for i in 0..len {
                            let has_windows = self
                                .sets
                                .values()
                                .any(|s| s.workspaces[i].windows().next().is_some());

                            if !has_windows && i != active && i != len - 1 {
                                for workspace in self.sets.values().map(|s| &s.workspaces[i]) {
                                    workspace_state.remove_workspace(workspace.handle);
                                }
                                keep[i] = false;
                            }
                        }

                        self.sets.values_mut().for_each(|s| {
                            let mut iter = keep.iter();
                            s.workspaces.retain(|_| *iter.next().unwrap());
                        });
                        active -= keep.iter().take(active + 1).filter(|keep| !**keep).count();
                        self.sets.values_mut().for_each(|s| {
                            s.active = active;
                        });

                        if keep.iter().any(|val| *val == false) {
                            for set in self.sets.values_mut() {
                                for (i, workspace) in set.workspaces.iter().enumerate() {
                                    workspace_set_idx(
                                        workspace_state,
                                        i as u8 + 1,
                                        set.idx,
                                        &workspace.handle,
                                    );
                                }
                            }
                        }
                    }
                    WorkspaceAmount::Static(amount) => {
                        for set in self.sets.values_mut() {
                            set.ensure_static(amount as usize, workspace_state, toplevel_info_state)
                        }
                    }
                }
            }
            WorkspaceMode::OutputBound => match self.amount {
                WorkspaceAmount::Dynamic => {
                    for set in self.sets.values_mut() {
                        set.ensure_last_empty(workspace_state);
                    }
                }
                WorkspaceAmount::Static(amount) => {
                    for set in self.sets.values_mut() {
                        set.ensure_static(amount as usize, workspace_state, toplevel_info_state)
                    }
                }
            },
        }

        for set in self.sets.values_mut() {
            set.refresh()
        }
    }

    pub fn get(&self, num: usize, output: &Output) -> Option<&Workspace> {
        self.sets
            .get(output)
            .and_then(|set| set.workspaces.get(num))
    }

    pub fn get_mut(&mut self, num: usize, output: &Output) -> Option<&mut Workspace> {
        self.sets
            .get_mut(output)
            .and_then(|set| set.workspaces.get_mut(num))
    }

    pub fn active(&self, output: &Output) -> (Option<(&Workspace, Instant)>, &Workspace) {
        let set = self.sets.get(output).unwrap();
        (
            set.previously_active
                .map(|(idx, start)| (&set.workspaces[idx], start)),
            &set.workspaces[set.active],
        )
    }

    pub fn active_mut(&mut self, output: &Output) -> &mut Workspace {
        let set = self.sets.get_mut(output).unwrap();
        &mut set.workspaces[set.active]
    }

    pub fn active_num(&self, output: &Output) -> (Option<usize>, usize) {
        let set = self.sets.get(output).unwrap();
        (set.previously_active.map(|(idx, _)| idx), set.active)
    }

    pub fn len(&self, output: &Output) -> usize {
        let set = self.sets.get(output).unwrap();
        set.workspaces.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Output, &WorkspaceSet)> {
        self.sets.iter()
    }

    pub fn spaces(&self) -> impl Iterator<Item = &Workspace> {
        self.sets.values().flat_map(|set| set.workspaces.iter())
    }

    pub fn spaces_for_output(&self, output: &Output) -> impl Iterator<Item = &Workspace> {
        self.sets
            .get(output)
            .into_iter()
            .flat_map(|set| set.workspaces.iter())
    }

    pub fn spaces_mut(&mut self) -> impl Iterator<Item = &mut Workspace> {
        Box::new(
            self.sets
                .values_mut()
                .flat_map(|set| set.workspaces.iter_mut()),
        )
    }

    pub fn update_tiling_status(&mut self, seat: &Seat<State>, tiling: bool) {
        for set in self.sets.values_mut() {
            set.update_tiling_status(seat, tiling)
        }
    }

    pub fn set_theme(&mut self, theme: cosmic::Theme) {
        for (_, s) in &mut self.sets {
            s.theme = theme.clone();
            for mut w in &mut s.workspaces {
                w.tiling_layer.theme = theme.clone();

                w.mapped().for_each(|m| {
                    m.update_theme(theme.clone());
                    m.force_redraw();
                });

                w.refresh();
                w.dirty.store(true, Ordering::Relaxed);
                w.recalculate();
            }
        }
    }
}

pub struct InvalidWorkspaceIndex;

impl Shell {
    pub fn new(config: &Config, dh: &DisplayHandle) -> Self {
        // TODO: Privileged protocols
        let layer_shell_state = WlrLayerShellState::new::<State>(dh);
        let xdg_shell_state = XdgShellState::new::<State>(dh);
        let toplevel_info_state = ToplevelInfoState::new(
            dh,
            //|client| client.get_data::<ClientState>().map_or(false, |s| s.privileged),
            client_has_security_context,
        );
        let toplevel_management_state = ToplevelManagementState::new::<State, _>(
            dh,
            vec![
                ManagementCapabilities::Close,
                ManagementCapabilities::Activate,
            ],
            //|client| client.get_data::<ClientState>().map_or(false, |s| s.privileged),
            client_has_security_context,
        );
        let workspace_state = WorkspaceState::new(
            dh,
            //|client| client.get_data::<ClientState>().map_or(false, |s| s.privileged),
            client_has_security_context,
        );
        let theme = cosmic::theme::system_preference();

        Shell {
            popups: PopupManager::default(),
            workspaces: Workspaces::new(config, theme.clone()),
            maximize_mode: MaximizeMode::Floating,

            pending_windows: Vec::new(),
            pending_layers: Vec::new(),
            override_redirect_windows: Vec::new(),

            layer_shell_state,
            toplevel_info_state,
            toplevel_management_state,
            xdg_shell_state,
            workspace_state,

            theme,
            overview_mode: OverviewMode::None,
            swap_indicator: None,
            resize_mode: ResizeMode::None,
            resize_state: None,
            resize_indicator: None,
        }
    }

    pub fn add_output(&mut self, output: &Output) {
        self.workspaces.add_output(
            output,
            &mut self.workspace_state.update(),
            &mut self.toplevel_info_state,
        );
    }

    pub fn remove_output(&mut self, output: &Output, seats: impl Iterator<Item = Seat<State>>) {
        self.workspaces.remove_output(
            output,
            seats,
            &mut self.workspace_state.update(),
            &mut self.toplevel_info_state,
        );
        self.refresh(); // cleans up excess of workspaces and empty workspaces
    }

    pub fn update_config(&mut self, config: &Config) {
        let mut workspace_state = self.workspace_state.update();
        let toplevel_info_state = &mut self.toplevel_info_state;
        self.workspaces
            .update_config(config, &mut workspace_state, toplevel_info_state);
    }

    pub fn activate(
        &mut self,
        output: &Output,
        idx: usize,
    ) -> Result<Option<Point<i32, Global>>, InvalidWorkspaceIndex> {
        match &mut self.workspaces.mode {
            WorkspaceMode::OutputBound => {
                if let Some(set) = self.workspaces.sets.get_mut(output) {
                    if matches!(
                        self.overview_mode,
                        OverviewMode::Started(Trigger::Pointer(_), _)
                    ) {
                        set.workspaces[set.active].tiling_layer.cleanup_drag();
                    }
                    set.activate(idx, &mut self.workspace_state.update())?;

                    let output_geo = output.geometry();
                    Ok(Some(
                        output_geo.loc
                            + Point::from((output_geo.size.w / 2, output_geo.size.h / 2)),
                    ))
                } else {
                    Ok(None)
                }
            }
            WorkspaceMode::Global => {
                for set in self.workspaces.sets.values_mut() {
                    set.activate(idx, &mut self.workspace_state.update())?;
                }
                Ok(None)
            }
        }
    }

    pub fn active_space(&self, output: &Output) -> &Workspace {
        self.workspaces.active(output).1
    }

    pub fn active_space_mut(&mut self, output: &Output) -> &mut Workspace {
        self.workspaces.active_mut(output)
    }

    pub fn visible_outputs_for_surface<'a>(
        &'a self,
        surface: &'a WlSurface,
    ) -> impl Iterator<Item = Output> + 'a {
        match self
            .outputs()
            .find(|o| {
                let map = layer_map_for_output(o);
                map.layer_for_surface(surface, WindowSurfaceType::ALL)
                    .is_some()
            })
            .or_else(|| {
                self.pending_layers.iter().find_map(|(l, output, _)| {
                    let found = AtomicBool::new(false);
                    l.with_surfaces(|s, _| {
                        found.fetch_or(s == surface, Ordering::SeqCst);
                    });
                    found.load(Ordering::SeqCst).then_some(output)
                })
            }) {
            Some(output) => {
                Box::new(std::iter::once(output.clone())) as Box<dyn Iterator<Item = Output>>
            }
            None => Box::new(
                self.outputs()
                    .filter(|o| {
                        self.override_redirect_windows.iter().any(|or| {
                            if or.wl_surface().as_ref() == Some(surface) {
                                or.geometry()
                                    .as_global()
                                    .intersection(o.geometry())
                                    .is_some()
                            } else {
                                false
                            }
                        })
                    })
                    .cloned()
                    .chain(self.outputs().map(|o| self.active_space(o)).flat_map(|w| {
                        w.mapped()
                            .find(|e| e.has_surface(surface, WindowSurfaceType::ALL))
                            .map(|_| w.output().clone())
                            .into_iter()
                    })),
            ),
        }
    }

    pub fn workspaces_for_surface(
        &self,
        surface: &WlSurface,
    ) -> impl Iterator<Item = (WorkspaceHandle, Output)> {
        match self.outputs().find(|o| {
            let map = layer_map_for_output(o);
            map.layer_for_surface(surface, WindowSurfaceType::ALL)
                .is_some()
        }) {
            Some(output) => self
                .workspaces
                .spaces()
                .find(move |workspace| workspace.output() == output)
                .map(|w| (w.handle.clone(), output.clone())),
            None => self
                .workspaces
                .spaces()
                .find(|w| {
                    w.mapped()
                        .any(|e| e.has_surface(surface, WindowSurfaceType::ALL))
                })
                .map(|w| (w.handle.clone(), w.output().clone())),
        }
        .into_iter()
    }

    pub fn element_for_surface(&self, surface: &CosmicSurface) -> Option<&CosmicMapped> {
        self.workspaces
            .spaces()
            .find_map(|w| w.element_for_surface(surface))
    }

    pub fn element_for_wl_surface(&self, surface: &WlSurface) -> Option<&CosmicMapped> {
        self.workspaces
            .spaces()
            .find_map(|w| w.element_for_wl_surface(surface))
    }

    pub fn space_for(&self, mapped: &CosmicMapped) -> Option<&Workspace> {
        self.workspaces
            .spaces()
            .find(|workspace| workspace.mapped().any(|m| m == mapped))
    }

    pub fn space_for_mut(&mut self, mapped: &CosmicMapped) -> Option<&mut Workspace> {
        self.workspaces
            .spaces_mut()
            .find(|workspace| workspace.mapped().any(|m| m == mapped))
    }

    pub fn space_for_handle(&self, handle: &WorkspaceHandle) -> Option<&Workspace> {
        self.workspaces.spaces().find(|w| &w.handle == handle)
    }

    pub fn space_for_handle_mut(&mut self, handle: &WorkspaceHandle) -> Option<&mut Workspace> {
        self.workspaces.spaces_mut().find(|w| &w.handle == handle)
    }

    pub fn outputs(&self) -> impl DoubleEndedIterator<Item = &Output> {
        self.workspaces.sets.keys().chain(
            self.workspaces
                .backup_set
                .as_ref()
                .into_iter()
                .map(|set| &set.output),
        )
    }

    pub fn global_space(&self) -> Rectangle<i32, Global> {
        self.outputs()
            .fold(
                Option::<Rectangle<i32, Global>>::None,
                |maybe_geo, output| match maybe_geo {
                    Some(rect) => Some(rect.merge(output.geometry())),
                    None => Some(output.geometry()),
                },
            )
            .unwrap_or_else(Rectangle::default)
    }

    pub fn animations_going(&self) -> bool {
        self.workspaces
            .sets
            .values()
            .any(|set| set.previously_active.is_some())
            || !matches!(self.overview_mode, OverviewMode::None)
            || !matches!(self.resize_mode, ResizeMode::None)
            || self
                .workspaces
                .spaces()
                .any(|workspace| workspace.animations_going())
    }

    pub fn update_animations(&mut self) -> HashMap<ClientId, Client> {
        let mut clients = HashMap::new();
        for workspace in self.workspaces.spaces_mut() {
            clients.extend(workspace.update_animations());
        }
        clients
    }

    pub fn set_overview_mode(
        &mut self,
        enabled: Option<Trigger>,
        evlh: LoopHandle<'static, crate::state::State>,
    ) {
        if let Some(trigger) = enabled {
            if !matches!(self.overview_mode, OverviewMode::Started(_, _)) {
                if matches!(trigger, Trigger::KeyboardSwap(_, _)) {
                    self.swap_indicator = Some(swap_indicator(evlh, self.theme.clone()));
                }
                self.overview_mode = OverviewMode::Started(trigger, Instant::now());
            }
        } else {
            if !matches!(self.overview_mode, OverviewMode::Ended(_, _)) {
                let (reverse_duration, trigger) =
                    if let OverviewMode::Started(trigger, start) = self.overview_mode.clone() {
                        (
                            ANIMATION_DURATION
                                - Instant::now().duration_since(start).min(ANIMATION_DURATION),
                            Some(trigger),
                        )
                    } else {
                        (Duration::ZERO, None)
                    };
                self.overview_mode =
                    OverviewMode::Ended(trigger, Instant::now() - reverse_duration);
            }
        }
    }

    pub fn overview_mode(&mut self) -> (OverviewMode, Option<SwapIndicator>) {
        if let OverviewMode::Ended(_, timestamp) = self.overview_mode {
            if Instant::now().duration_since(timestamp) > ANIMATION_DURATION {
                self.overview_mode = OverviewMode::None;
                self.swap_indicator = None;
            }
        }

        (self.overview_mode.clone(), self.swap_indicator.clone())
    }

    pub fn set_resize_mode(
        &mut self,
        enabled: Option<(KeyPattern, ResizeDirection)>,
        config: &Config,
        evlh: LoopHandle<'static, crate::state::State>,
    ) {
        if let Some((pattern, direction)) = enabled {
            if let ResizeMode::Started(old_pattern, _, old_direction) = &mut self.resize_mode {
                *old_pattern = pattern;
                *old_direction = direction;
            } else {
                self.resize_mode = ResizeMode::Started(pattern, Instant::now(), direction);
            }
            self.resize_indicator = Some(resize_indicator(
                direction,
                config,
                evlh,
                self.theme.clone(),
            ));
        } else {
            if let ResizeMode::Started(_, _, direction) = &self.resize_mode {
                self.resize_mode = ResizeMode::Ended(Instant::now(), *direction);
                if let Some((_, direction, edge, _, _, _)) = self.resize_state.as_ref() {
                    self.finish_resize(*direction, *edge);
                }
            }
        }
    }

    pub fn resize_mode(&mut self) -> (ResizeMode, Option<ResizeIndicator>) {
        if let ResizeMode::Ended(timestamp, _) = self.resize_mode {
            if Instant::now().duration_since(timestamp) > ANIMATION_DURATION {
                self.resize_mode = ResizeMode::None;
                self.resize_indicator = None;
            }
        }

        (self.resize_mode.clone(), self.resize_indicator.clone())
    }

    pub fn refresh(&mut self) {
        #[cfg(feature = "debug")]
        puffin::profile_function!();

        self.popups.cleanup();

        self.workspaces.refresh(
            &mut self.workspace_state.update(),
            &mut self.toplevel_info_state,
        );

        for output in self.outputs() {
            let mut map = layer_map_for_output(output);
            map.cleanup();
        }

        self.override_redirect_windows.retain(|or| or.alive());
        self.override_redirect_windows
            .iter()
            .for_each(|or| or.refresh());

        self.toplevel_info_state
            .refresh(Some(&self.workspace_state));
    }

    pub fn remap_unfullscreened_window(
        &mut self,
        mapped: CosmicMapped,
        current_workspace: &WorkspaceHandle,
        previous_workspace: &WorkspaceHandle,
        target_layer: ManagedLayer,
    ) {
        if self.space_for_handle(previous_workspace).is_none() {
            return;
        }

        {
            let Some(workspace) = self.space_for_handle_mut(&current_workspace) else { return };
            let _ = workspace.unmap(&mapped);
        }

        let new_workspace_output = self
            .space_for_handle(&previous_workspace)
            .unwrap()
            .output()
            .clone();
        for (window, _) in mapped.windows() {
            self.toplevel_info_state
                .toplevel_enter_output(&window, &new_workspace_output);
            self.toplevel_info_state
                .toplevel_enter_workspace(&window, &previous_workspace);
        }

        let new_workspace = self.space_for_handle_mut(&previous_workspace).unwrap();
        match target_layer {
            ManagedLayer::Floating => new_workspace.floating_layer.map(mapped, None),
            ManagedLayer::Tiling => {
                new_workspace
                    .tiling_layer
                    .map(mapped, Option::<std::iter::Empty<_>>::None, None)
            }
        };
    }

    pub fn map_window(state: &mut State, window: &CosmicSurface) {
        let pos = state
            .common
            .shell
            .pending_windows
            .iter()
            .position(|(w, _, _)| w == window)
            .unwrap();
        let (window, seat, output) = state.common.shell.pending_windows.remove(pos);

        let should_be_fullscreen = output.is_some();
        let output = output.unwrap_or_else(|| seat.active_output());

        let workspace = state.common.shell.workspaces.active_mut(&output);
        if let Some((mapped, layer, previous_workspace)) = workspace.remove_fullscreen() {
            let old_handle = workspace.handle.clone();
            let new_workspace_handle = state
                .common
                .shell
                .space_for_handle(&previous_workspace)
                .is_some()
                .then_some(previous_workspace)
                .unwrap_or(old_handle);

            state.common.shell.remap_unfullscreened_window(
                mapped,
                &old_handle,
                &new_workspace_handle,
                layer,
            );
        };

        let workspace = state.common.shell.workspaces.active_mut(&output);
        state.common.shell.toplevel_info_state.new_toplevel(&window);
        state
            .common
            .shell
            .toplevel_info_state
            .toplevel_enter_output(&window, &output);
        state
            .common
            .shell
            .toplevel_info_state
            .toplevel_enter_workspace(&window, &workspace.handle);

        let mapped = CosmicMapped::from(CosmicWindow::new(
            window.clone(),
            state.common.event_loop_handle.clone(),
            state.common.theme.clone(),
        ));
        #[cfg(feature = "debug")]
        {
            mapped.set_debug(state.common.egui.active);
        }
        if layout::should_be_floating(&window) || !workspace.tiling_enabled {
            workspace.floating_layer.map(mapped.clone(), None);
        } else {
            for mapped in workspace
                .mapped()
                .filter(|m| m.maximized_state.lock().unwrap().is_some())
                .cloned()
                .collect::<Vec<_>>()
                .into_iter()
            {
                workspace.unmaximize_request(&mapped.active_window());
            }
            let focus_stack = workspace.focus_stack.get(&seat);
            workspace
                .tiling_layer
                .map(mapped.clone(), Some(focus_stack.iter()), None);
        }

        if should_be_fullscreen {
            workspace.fullscreen_request(&mapped.active_window(), None);
        }

        if let CosmicSurface::X11(ref surface) = window {
            if let Some(xwm) = state
                .common
                .xwayland_state
                .as_mut()
                .and_then(|state| state.xwm.as_mut())
            {
                if let Err(err) = xwm.raise_window(surface) {
                    warn!(?err, "Failed to update Xwayland stacking order.");
                }
            }
        }

        Shell::set_focus(state, Some(&KeyboardFocusTarget::from(mapped)), &seat, None);

        let active_space = state.common.shell.active_space(&output);
        for mapped in active_space.mapped() {
            state.common.shell.update_reactive_popups(mapped);
        }
    }

    pub fn map_override_redirect(state: &mut State, window: X11Surface) {
        let geo = window.geometry();
        for (output, overlap) in state.common.shell.outputs().cloned().filter_map(|o| {
            o.geometry()
                .as_logical()
                .intersection(geo)
                .map(|overlap| (o, overlap))
        }) {
            window.output_enter(&output, overlap);
        }

        state.common.shell.override_redirect_windows.push(window);
    }

    pub fn map_layer(state: &mut State, layer_surface: &LayerSurface) {
        let pos = state
            .common
            .shell
            .pending_layers
            .iter()
            .position(|(l, _, _)| l == layer_surface)
            .unwrap();
        let (layer_surface, output, seat) = state.common.shell.pending_layers.remove(pos);

        let wants_focus = {
            with_states(layer_surface.wl_surface(), |states| {
                let state = states.cached_state.current::<LayerSurfaceCachedState>();
                matches!(state.layer, Layer::Top | Layer::Overlay)
                    && state.keyboard_interactivity != KeyboardInteractivity::None
            })
        };

        {
            let mut map = layer_map_for_output(&output);
            map.map_layer(&layer_surface).unwrap();
        }
        for workspace in state.common.shell.workspaces.spaces_mut() {
            workspace.tiling_layer.recalculate();
        }

        if wants_focus {
            Shell::set_focus(state, Some(&layer_surface.into()), &seat, None)
        }
    }

    pub fn move_current_window(
        state: &mut State,
        seat: &Seat<State>,
        from_output: &Output,
        to: (&Output, Option<usize>),
        follow: bool,
        direction: Option<Direction>,
    ) -> Result<Option<Point<i32, Global>>, InvalidWorkspaceIndex> {
        let (to_output, to_idx) = to;
        let to_idx = to_idx.unwrap_or(state.common.shell.workspaces.active_num(to_output).1);
        if state
            .common
            .shell
            .workspaces
            .get(to_idx, to_output)
            .is_none()
        {
            return Err(InvalidWorkspaceIndex);
        }

        if from_output == to_output
            && to_idx == state.common.shell.workspaces.active_num(from_output).1
        {
            return Ok(None);
        }

        let from_workspace = state.common.shell.workspaces.active_mut(from_output);
        let maybe_window = from_workspace.focus_stack.get(seat).last().cloned();

        let Some(mapped) = maybe_window else {
            return Ok(None);
        };
        let Some(window_state) = from_workspace.unmap(&mapped) else {
            return Ok(None);
        };

        for (toplevel, _) in mapped.windows() {
            state
                .common
                .shell
                .toplevel_info_state
                .toplevel_leave_workspace(&toplevel, &from_workspace.handle);
            if from_output != to_output {
                state
                    .common
                    .shell
                    .toplevel_info_state
                    .toplevel_leave_output(&toplevel, from_output);
            }
        }
        let elements = from_workspace.mapped().cloned().collect::<Vec<_>>();

        for mapped in elements.into_iter() {
            state.common.shell.update_reactive_popups(&mapped);
        }
        let new_pos = if follow {
            seat.set_active_output(&to_output);
            state.common.shell.activate(to_output, to_idx)?
        } else {
            None
        };

        let mut to_workspace = state
            .common
            .shell
            .workspaces
            .get_mut(to_idx, to_output)
            .unwrap(); // checked above
        let focus_stack = to_workspace.focus_stack.get(&seat);
        if window_state.layer == ManagedLayer::Floating {
            to_workspace.floating_layer.map(mapped.clone(), None);
        } else {
            to_workspace
                .tiling_layer
                .map(mapped.clone(), Some(focus_stack.iter()), direction);
        }
        let focus_target = if let Some(f) = window_state.was_fullscreen {
            if to_workspace.fullscreen.is_some() {
                if let Some((mapped, layer, previous_workspace)) = to_workspace.remove_fullscreen()
                {
                    let old_handle = to_workspace.handle.clone();
                    let new_workspace_handle = state
                        .common
                        .shell
                        .space_for_handle(&previous_workspace)
                        .is_some()
                        .then_some(previous_workspace)
                        .unwrap_or(old_handle);

                    state.common.shell.remap_unfullscreened_window(
                        mapped,
                        &old_handle,
                        &new_workspace_handle,
                        layer,
                    );
                    to_workspace = state
                        .common
                        .shell
                        .workspaces
                        .get_mut(to_idx, to_output)
                        .unwrap(); // checked above
                }
            }

            to_workspace.fullscreen_request(&mapped.active_window(), f.previously);
            to_workspace
                .fullscreen
                .as_ref()
                .map(|f| KeyboardFocusTarget::from(f.surface.clone()))
                .unwrap_or_else(|| KeyboardFocusTarget::from(mapped.clone()))
        } else {
            KeyboardFocusTarget::from(mapped.clone())
        };

        for (toplevel, _) in mapped.windows() {
            if from_output != to_output {
                state
                    .common
                    .shell
                    .toplevel_info_state
                    .toplevel_enter_output(&toplevel, to_output);
            }
            state
                .common
                .shell
                .toplevel_info_state
                .toplevel_enter_workspace(&toplevel, &to_workspace.handle);
        }
        for mapped in to_workspace
            .mapped()
            .cloned()
            .collect::<Vec<_>>()
            .into_iter()
        {
            state.common.shell.update_reactive_popups(&mapped);
        }

        if follow {
            Common::set_focus(state, Some(&focus_target), &seat, None);
        }
        Ok(new_pos)
    }

    pub fn update_reactive_popups(&self, mapped: &CosmicMapped) {
        if let Some(workspace) = self.space_for(mapped) {
            let element_loc = workspace
                .element_geometry(mapped)
                .unwrap()
                .loc
                .to_global(&workspace.output);
            for (toplevel, offset) in mapped.windows() {
                if let CosmicSurface::Wayland(toplevel) = toplevel {
                    let window_geo_offset = toplevel.geometry().loc.as_global();
                    update_reactive_popups(
                        &toplevel,
                        element_loc + offset.as_global() + window_geo_offset,
                        self.outputs(),
                    );
                }
            }
        }
    }

    pub fn move_request(
        state: &mut State,
        surface: &WlSurface,
        seat: &Seat<State>,
        serial: impl Into<Option<Serial>>,
    ) {
        let serial = serial.into();
        if let Some(start_data) = check_grab_preconditions(&seat, surface, serial) {
            if let Some(mapped) = state.common.shell.element_for_wl_surface(surface).cloned() {
                if let Some(workspace) = state.common.shell.space_for_mut(&mapped) {
                    let output = seat.active_output();
                    let (window, _) = mapped
                        .windows()
                        .find(|(w, _)| w.wl_surface().as_ref() == Some(surface))
                        .unwrap();
                    let button = start_data.button;
                    let active_hint = state.common.theme.cosmic().active_hint as u8;
                    if let Some(grab) = workspace.move_request(
                        &window,
                        &seat,
                        &output,
                        start_data,
                        active_hint as u8,
                    ) {
                        let handle = workspace.handle;
                        state
                            .common
                            .shell
                            .toplevel_info_state
                            .toplevel_leave_workspace(&window, &handle);
                        state
                            .common
                            .shell
                            .toplevel_info_state
                            .toplevel_leave_output(&window, &output);
                        if grab.is_tiling_grab() {
                            state.common.shell.set_overview_mode(
                                Some(Trigger::Pointer(button)),
                                state.common.event_loop_handle.clone(),
                            );
                        }
                        seat.get_pointer().unwrap().set_grab(
                            state,
                            grab,
                            serial.unwrap_or_else(|| SERIAL_COUNTER.next_serial()),
                            Focus::Clear,
                        );
                    }
                }
            }
        }
    }

    pub fn resize_request(
        state: &mut State,
        surface: &WlSurface,
        seat: &Seat<State>,
        serial: impl Into<Option<Serial>>,
        edges: ResizeEdge,
    ) {
        let serial = serial.into();
        if let Some(start_data) = check_grab_preconditions(&seat, surface, serial) {
            if let Some(mapped) = state.common.shell.element_for_wl_surface(surface).cloned() {
                if let Some(workspace) = state.common.shell.space_for_mut(&mapped) {
                    if let Some(grab) = workspace.resize_request(&mapped, &seat, start_data, edges)
                    {
                        seat.get_pointer().unwrap().set_grab(
                            state,
                            grab,
                            serial.unwrap_or_else(|| SERIAL_COUNTER.next_serial()),
                            Focus::Clear,
                        );
                    }
                }
            }
        }
    }

    pub fn resize(&mut self, seat: &Seat<State>, direction: ResizeDirection, edge: ResizeEdge) {
        let output = seat.active_output();
        let (_, idx) = self.workspaces.active_num(&output);
        let Some(focused) = seat.get_keyboard().unwrap().current_focus() else {
            return;
        };

        if let Some(workspace) = self.workspaces.get_mut(idx, &output) {
            let amount = (self
                .resize_state
                .take()
                .map(|(_, _, _, amount, _, _)| amount)
                .unwrap_or(10)
                + 2)
            .min(20);
            if workspace.resize(&focused, direction, edge, amount) {
                self.resize_state = Some((focused, direction, edge, amount, idx, output));
            }
        }
    }

    pub fn finish_resize(&mut self, direction: ResizeDirection, edge: ResizeEdge) {
        if let Some((old_focused, old_direction, old_edge, _, idx, output)) =
            self.resize_state.take()
        {
            let workspace = self.workspaces.get(idx, &output).unwrap();
            if old_direction == direction && old_edge == edge {
                let Some(toplevel) = old_focused.toplevel() else {
                    return;
                };
                let Some(mapped) = workspace
                    .mapped()
                    .find(|m| m.has_surface(&toplevel, WindowSurfaceType::TOPLEVEL))
                    .cloned()
                else {
                    return;
                };
                let mut resize_state = mapped.resize_state.lock().unwrap();
                if let Some(ResizeState::Resizing(data)) = *resize_state {
                    *resize_state = Some(ResizeState::WaitingForCommit(data));
                }
            }
        }
    }

    pub(crate) fn set_theme(&mut self, theme: cosmic::Theme) {
        self.theme = theme.clone();
        self.refresh();
        self.workspaces.set_theme(theme.clone());
    }
}

fn workspace_set_idx<'a>(
    state: &mut WorkspaceUpdateGuard<'a, State>,
    idx: u8,
    output_pos: usize,
    handle: &WorkspaceHandle,
) {
    state.set_workspace_name(&handle, format!("{}", idx));
    state.set_workspace_coordinates(&handle, [Some(idx as u32), Some(output_pos as u32), None]);
}

pub fn check_grab_preconditions(
    seat: &Seat<State>,
    surface: &WlSurface,
    serial: Option<Serial>,
) -> Option<PointerGrabStartData<State>> {
    use smithay::reexports::wayland_server::Resource;

    // TODO: touch resize.
    let pointer = seat.get_pointer().unwrap();

    // Check that this surface has a click grab.
    if !match serial {
        Some(serial) => pointer.has_grab(serial),
        None => pointer.is_grabbed(),
    } {
        return None;
    }

    let start_data = pointer.grab_start_data().unwrap();

    // If the focus was for a different surface, ignore the request.
    if start_data.focus.is_none()
        || !start_data
            .focus
            .as_ref()
            .unwrap()
            .0
            .same_client_as(&surface.id())
    {
        return None;
    }

    Some(start_data)
}
