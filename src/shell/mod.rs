use calloop::LoopHandle;
use serde::{Deserialize, Serialize};
use std::{
    cell::RefCell,
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
    utils::{Logical, Point, Rectangle, Serial, SERIAL_COUNTER},
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
    config::{Config, KeyModifiers, KeyPattern, OutputConfig, WorkspaceMode as ConfigMode},
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
        CosmicWindow,
    },
    focus::target::KeyboardFocusTarget,
    grabs::ResizeEdge,
    layout::{
        floating::{FloatingLayout, ResizeState},
        tiling::{Direction, TilingLayout},
    },
};

const ANIMATION_DURATION: Duration = Duration::from_millis(200);

#[derive(Debug, Clone)]
pub enum OverviewMode {
    None,
    Started(KeyModifiers, Instant),
    Ended(Instant),
}

impl OverviewMode {
    pub fn alpha(&self) -> Option<f32> {
        match self {
            OverviewMode::Started(_, start) => {
                let percentage = Instant::now().duration_since(*start).as_millis() as f32
                    / ANIMATION_DURATION.as_millis() as f32;
                Some(ease(EaseInOutCubic, 0.0, 1.0, percentage))
            }
            OverviewMode::Ended(end) => {
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

#[derive(Debug)]
pub struct Shell {
    pub popups: PopupManager,
    pub outputs: Vec<Output>,
    pub workspaces: WorkspaceMode,
    pub tiling_enabled: bool,
    pub pending_windows: Vec<(CosmicSurface, Seat<State>)>,
    pub pending_layers: Vec<(LayerSurface, Output, Seat<State>)>,
    pub override_redirect_windows: Vec<X11Surface>,

    // wayland_state
    pub layer_shell_state: WlrLayerShellState,
    pub toplevel_info_state: ToplevelInfoState<State, CosmicSurface>,
    pub toplevel_management_state: ToplevelManagementState,
    pub xdg_shell_state: XdgShellState,
    pub workspace_state: WorkspaceState<State>,

    gaps: (u8, u8),
    overview_mode: OverviewMode,
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
    amount: WorkspaceAmount,
    group: WorkspaceGroupHandle,
    idx: usize,
    tiling_enabled: bool,
    gaps: (u8, u8),
    pub(crate) workspaces: Vec<Workspace>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum WorkspaceAmount {
    Dynamic,
    Static(u8),
}

fn create_workspace(
    state: &mut WorkspaceUpdateGuard<'_, State>,
    group_handle: &WorkspaceGroupHandle,
    active: bool,
    tiling: bool,
    gaps: (u8, u8),
) -> Workspace {
    let workspace_handle = state.create_workspace(&group_handle).unwrap();
    if active {
        state.add_workspace_state(&workspace_handle, WState::Active);
    }
    state.set_workspace_capabilities(
        &workspace_handle,
        [WorkspaceCapabilities::Activate].into_iter(),
    );
    Workspace::new(workspace_handle, tiling, gaps)
}

impl WorkspaceSet {
    fn new(
        state: &mut WorkspaceUpdateGuard<'_, State>,
        amount: WorkspaceAmount,
        idx: usize,
        tiling_enabled: bool,
        gaps: (u8, u8),
    ) -> WorkspaceSet {
        let group_handle = state.create_workspace_group();

        let workspaces = match amount {
            WorkspaceAmount::Dynamic => {
                let workspace = create_workspace(state, &group_handle, true, tiling_enabled, gaps);
                workspace_set_idx(state, 1, idx, &workspace.handle);
                state.set_workspace_capabilities(
                    &workspace.handle,
                    [WorkspaceCapabilities::Activate].into_iter(),
                );
                vec![workspace]
            }
            WorkspaceAmount::Static(len) => (0..len)
                .map(|i| {
                    let workspace =
                        create_workspace(state, &group_handle, i == 0, tiling_enabled, gaps);
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
            amount,
            group: group_handle,
            idx,
            tiling_enabled,
            gaps,
            workspaces,
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

    fn refresh<'a>(
        &mut self,
        state: &mut WorkspaceState<State>,
        toplevel_info: &mut ToplevelInfoState<State, CosmicSurface>,
        outputs: impl Iterator<Item = (&'a Output, Point<i32, Logical>)>,
    ) {
        if let Some((_, start)) = self.previously_active {
            if Instant::now().duration_since(start).as_millis() >= ANIMATION_DURATION.as_millis() {
                self.previously_active = None;
            }
        }

        match self.amount {
            WorkspaceAmount::Dynamic => self.ensure_last_empty(state, outputs),
            WorkspaceAmount::Static(len) => {
                self.ensure_static(len as usize, state, toplevel_info, outputs)
            }
        }

        self.workspaces[self.active].refresh();
    }

    fn ensure_last_empty<'a>(
        &mut self,
        state: &mut WorkspaceState<State>,
        outputs: impl Iterator<Item = (&'a Output, Point<i32, Logical>)>,
    ) {
        let mut state = state.update();

        // add empty at the end, if necessary
        if self.workspaces.last().unwrap().windows().next().is_some() {
            let mut workspace = create_workspace(
                &mut state,
                &self.group,
                false,
                self.tiling_enabled,
                self.gaps,
            );
            workspace_set_idx(
                &mut state,
                self.workspaces.len() as u8 + 1,
                self.idx,
                &workspace.handle,
            );
            state.set_workspace_capabilities(
                &workspace.handle,
                [WorkspaceCapabilities::Activate].into_iter(),
            );
            for (output, location) in outputs {
                workspace.map_output(output, location);
            }
            self.workspaces.push(workspace);
        }

        let len = self.workspaces.len();
        let mut keep = vec![true; len];
        // remove empty workspaces in between, if they are not active
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
                workspace_set_idx(&mut state, i as u8 + 1, self.idx, &workspace.handle);
            }
        }
    }

    fn ensure_static<'a>(
        &mut self,
        amount: usize,
        state: &mut WorkspaceState<State>,
        toplevel_info: &mut ToplevelInfoState<State, CosmicSurface>,
        outputs: impl Iterator<Item = (&'a Output, Point<i32, Logical>)>,
    ) {
        if amount < self.workspaces.len() {
            let mut state = state.update();
            // merge last ones
            let overflow = self.workspaces.split_off(amount);
            if self.active >= self.workspaces.len() {
                self.active = self.workspaces.len() - 1;
                state.add_workspace_state(&self.workspaces[self.active].handle, WState::Active);
            }
            let last_space = self.workspaces.last_mut().unwrap();

            for workspace in overflow {
                for element in workspace.mapped() {
                    // fixup toplevel state
                    for (toplevel, _) in element.windows() {
                        toplevel_info.toplevel_leave_workspace(&toplevel, &workspace.handle);
                        toplevel_info.toplevel_enter_workspace(&toplevel, &last_space.handle);
                    }
                }
                last_space.tiling_layer.merge(workspace.tiling_layer);
                last_space.floating_layer.merge(workspace.floating_layer);
                last_space
                    .fullscreen
                    .extend(workspace.fullscreen.into_iter());
                state.remove_workspace(workspace.handle);
            }

            last_space.refresh();
        } else if amount > self.workspaces.len() {
            let mut state = state.update();
            // add empty ones
            let outputs = outputs.collect::<Vec<_>>();
            while amount > self.workspaces.len() {
                let mut workspace = create_workspace(
                    &mut state,
                    &self.group,
                    false,
                    self.tiling_enabled,
                    self.gaps,
                );
                workspace_set_idx(
                    &mut state,
                    self.workspaces.len() as u8 + 1,
                    self.idx,
                    &workspace.handle,
                );
                state.set_workspace_capabilities(
                    &workspace.handle,
                    [WorkspaceCapabilities::Activate].into_iter(),
                );
                for &(output, location) in outputs.iter() {
                    workspace.map_output(output, location);
                }
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
pub enum WorkspaceMode {
    OutputBound(HashMap<Output, WorkspaceSet>, WorkspaceAmount),
    Global(WorkspaceSet),
}

impl WorkspaceMode {
    pub fn new(
        config: crate::config::WorkspaceMode,
        amount: WorkspaceAmount,
        state: &mut WorkspaceUpdateGuard<'_, State>,
        tiling_enabled: bool,
        gaps: (u8, u8),
    ) -> WorkspaceMode {
        match config {
            crate::config::WorkspaceMode::Global => {
                WorkspaceMode::Global(WorkspaceSet::new(state, amount, 0, tiling_enabled, gaps))
            }
            crate::config::WorkspaceMode::OutputBound => {
                WorkspaceMode::OutputBound(HashMap::new(), amount)
            }
        }
    }

    pub fn get(&self, num: usize, output: &Output) -> Option<&Workspace> {
        match self {
            WorkspaceMode::Global(set) => set.workspaces.get(num),
            WorkspaceMode::OutputBound(sets, _) => {
                sets.get(output).and_then(|set| set.workspaces.get(num))
            }
        }
    }

    pub fn get_mut(&mut self, num: usize, output: &Output) -> Option<&mut Workspace> {
        match self {
            WorkspaceMode::Global(set) => set.workspaces.get_mut(num),
            WorkspaceMode::OutputBound(sets, _) => sets
                .get_mut(output)
                .and_then(|set| set.workspaces.get_mut(num)),
        }
    }

    pub fn active(&self, output: &Output) -> (Option<(&Workspace, Instant)>, &Workspace) {
        match self {
            WorkspaceMode::Global(set) => (
                set.previously_active
                    .map(|(idx, start)| (&set.workspaces[idx], start)),
                &set.workspaces[set.active],
            ),
            WorkspaceMode::OutputBound(sets, _) => {
                let set = sets.get(output).unwrap();
                (
                    set.previously_active
                        .map(|(idx, start)| (&set.workspaces[idx], start)),
                    &set.workspaces[set.active],
                )
            }
        }
    }

    pub fn active_mut(&mut self, output: &Output) -> &mut Workspace {
        match self {
            WorkspaceMode::Global(set) => &mut set.workspaces[set.active],
            WorkspaceMode::OutputBound(sets, _) => {
                let set = sets.get_mut(output).unwrap();
                &mut set.workspaces[set.active]
            }
        }
    }

    pub fn active_num(&self, output: &Output) -> (Option<usize>, usize) {
        match self {
            WorkspaceMode::Global(set) => (set.previously_active.map(|(idx, _)| idx), set.active),
            WorkspaceMode::OutputBound(sets, _) => {
                let set = sets.get(output).unwrap();
                (set.previously_active.map(|(idx, _)| idx), set.active)
            }
        }
    }

    pub fn len(&self, output: &Output) -> usize {
        match self {
            WorkspaceMode::Global(set) => set.workspaces.len(),
            WorkspaceMode::OutputBound(sets, _) => {
                let set = sets.get(output).unwrap();
                set.workspaces.len()
            }
        }
    }

    pub fn spaces(&self) -> impl Iterator<Item = &Workspace> {
        match self {
            WorkspaceMode::Global(set) => {
                Box::new(set.workspaces.iter()) as Box<dyn Iterator<Item = &Workspace>>
            }
            WorkspaceMode::OutputBound(sets, _) => {
                Box::new(sets.values().flat_map(|set| set.workspaces.iter()))
            }
        }
    }

    pub fn spaces_for_output(&self, output: &Output) -> impl Iterator<Item = &Workspace> {
        match self {
            WorkspaceMode::Global(set) => {
                Box::new(set.workspaces.iter()) as Box<dyn Iterator<Item = &Workspace>>
            }
            WorkspaceMode::OutputBound(sets, _) => Box::new(
                sets.get(output)
                    .into_iter()
                    .flat_map(|set| set.workspaces.iter()),
            ),
        }
    }

    pub fn spaces_mut(&mut self) -> impl Iterator<Item = &mut Workspace> {
        match self {
            WorkspaceMode::Global(set) => {
                Box::new(set.workspaces.iter_mut()) as Box<dyn Iterator<Item = &mut Workspace>>
            }
            WorkspaceMode::OutputBound(sets, _) => {
                Box::new(sets.values_mut().flat_map(|set| set.workspaces.iter_mut()))
            }
        }
    }

    pub fn update_tiling_status(&mut self, seat: &Seat<State>, tiling: bool) {
        match self {
            WorkspaceMode::Global(set) => set.update_tiling_status(seat, tiling),
            WorkspaceMode::OutputBound(sets, _) => {
                for set in sets.values_mut() {
                    set.update_tiling_status(seat, tiling)
                }
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
            |_| true,
        );
        let toplevel_management_state = ToplevelManagementState::new::<State, _>(
            dh,
            vec![
                ManagementCapabilities::Close,
                ManagementCapabilities::Activate,
            ],
            //|client| client.get_data::<ClientState>().map_or(false, |s| s.privileged),
            |_| true,
        );
        let mut workspace_state = WorkspaceState::new(
            dh,
            //|client| client.get_data::<ClientState>().map_or(false, |s| s.privileged),
            |_| true,
        );

        let tiling_enabled = config.static_conf.tiling_enabled;
        let mode = WorkspaceMode::new(
            config.static_conf.workspace_mode,
            config.static_conf.workspace_amount,
            &mut workspace_state.update(),
            tiling_enabled,
            config.static_conf.gaps,
        );

        Shell {
            popups: PopupManager::default(),
            outputs: Vec::new(),
            workspaces: mode,
            tiling_enabled,

            pending_windows: Vec::new(),
            pending_layers: Vec::new(),
            override_redirect_windows: Vec::new(),

            layer_shell_state,
            toplevel_info_state,
            toplevel_management_state,
            xdg_shell_state,
            workspace_state,

            gaps: config.static_conf.gaps,
            overview_mode: OverviewMode::None,
            resize_mode: ResizeMode::None,
            resize_state: None,
            resize_indicator: None,
        }
    }

    pub fn add_output(&mut self, output: &Output) {
        if self.outputs.contains(output) {
            return;
        }

        self.outputs.push(output.clone());
        let mut state = self.workspace_state.update();

        match &mut self.workspaces {
            WorkspaceMode::OutputBound(sets, amount) => {
                // TODO: Restore previously assigned workspaces, if possible!
                if !sets.contains_key(output) {
                    let set = WorkspaceSet::new(
                        &mut state,
                        *amount,
                        sets.len(),
                        self.tiling_enabled,
                        self.gaps,
                    );
                    state.add_group_output(&set.group, &output);
                    sets.insert(output.clone(), set);
                }
                for workspace in &mut sets.get_mut(output).unwrap().workspaces {
                    workspace.map_output(output, (0, 0).into());
                }
            }
            WorkspaceMode::Global(set) => {
                // TODO: Restore any window positions from previous outputs ???
                state.add_group_output(&set.group, output);
                for workspace in &mut set.workspaces {
                    workspace.map_output(
                        output,
                        output
                            .user_data()
                            .get::<RefCell<OutputConfig>>()
                            .unwrap()
                            .borrow()
                            .position
                            .into(),
                    );
                }
            }
        }
    }

    pub fn remove_output(&mut self, output: &Output, seats: impl Iterator<Item = Seat<State>>) {
        if let Some(first_output) = self.outputs.get(0) {
            for seat in seats {
                if &seat.active_output() == output {
                    seat.set_active_output(first_output);
                }
            }
        }

        if !self.outputs.contains(output) {
            return;
        }

        {
            let map = layer_map_for_output(output);
            for surface in map.layers() {
                surface.layer_surface().send_close();
            }
        }

        let mut state = self.workspace_state.update();
        self.outputs.retain(|o| o != output);

        match &mut self.workspaces {
            WorkspaceMode::OutputBound(sets, _) => {
                // TODO:
                // If amount::static merge them instead of appending

                if let Some(set) = sets.remove(output) {
                    // TODO: Heuristic which output to move to.
                    // It is supposed to be the *most* internal, we just pick the first one for now
                    // and hope enumeration order works in our favor.
                    if let Some(new_output) = self.outputs.get(0) {
                        let new_set = sets.get_mut(new_output).unwrap();
                        let workspace_group = new_set.group;
                        for mut workspace in set.workspaces {
                            // update workspace protocol state
                            state.remove_workspace(workspace.handle);
                            let workspace_handle =
                                state.create_workspace(&workspace_group).unwrap();
                            state.set_workspace_capabilities(
                                &workspace_handle,
                                [WorkspaceCapabilities::Activate].into_iter(),
                            );
                            workspace.handle = workspace_handle;

                            // update mapping
                            workspace.map_output(new_output, (0, 0).into());
                            workspace.unmap_output(output, &mut self.toplevel_info_state);
                            workspace.refresh();

                            new_set.workspaces.push(workspace);
                        }
                        state.remove_workspace_group(set.group);
                    }
                    // if there is no output, we are going to quit anyway, just drop the workspace set
                }
                for (i, set) in sets.values_mut().enumerate() {
                    set.update_idx(&mut state, i);
                }
                std::mem::drop(state);
                self.refresh(); // cleans up excess of workspaces and empty workspaces
            }
            WorkspaceMode::Global(set) => {
                state.remove_group_output(&set.group, output);
                for workspace in &mut set.workspaces {
                    workspace.unmap_output(output, &mut self.toplevel_info_state);
                    workspace.refresh();
                }
            }
        };
    }

    pub fn refresh_outputs(&mut self) {
        if let WorkspaceMode::Global(set) = &mut self.workspaces {
            for workspace in &mut set.workspaces {
                for output in self.outputs.iter() {
                    workspace.map_output(
                        output,
                        output
                            .user_data()
                            .get::<RefCell<OutputConfig>>()
                            .unwrap()
                            .borrow()
                            .position
                            .into(),
                    );
                }
            }
        }
    }

    pub fn set_mode(&mut self, mode: ConfigMode) {
        let mut state = self.workspace_state.update();

        match (&mut self.workspaces, mode) {
            (dst @ WorkspaceMode::OutputBound(_, _), ConfigMode::Global) => {
                // rustc should really be able to infer that this doesn't need an if.
                let (sets, amount) =
                    if let &mut WorkspaceMode::OutputBound(ref mut sets, ref amount) = dst {
                        (sets, *amount)
                    } else {
                        unreachable!()
                    };

                // in this case we have to merge our sets, preserving placing of windows as nicely as possible
                let mut new_set = WorkspaceSet::new(
                    &mut state,
                    WorkspaceAmount::Static(0),
                    0,
                    self.tiling_enabled,
                    self.gaps,
                );
                for output in &self.outputs {
                    state.add_group_output(&new_set.group, output);
                }

                // lets construct an iterator of all the pairs of workspaces we have to merge
                // we first split of the part of the workspaces that contain the currently active one
                let mut second_half = sets
                    .iter_mut()
                    .map(|(output, set)| (output.clone(), set.workspaces.split_off(set.active)))
                    .collect::<Vec<_>>();

                let mut first_half = std::iter::repeat(())
                    // we continuously pop the last elements from the first half and group them together.
                    .map(|_| {
                        sets.iter_mut()
                            .flat_map(|(o, w)| w.workspaces.pop().map(|w| (o.clone(), w)))
                            .collect::<Vec<_>>()
                    })
                    // we stop once there is no workspace anymore in the entire set
                    .filter(|vec| !vec.is_empty())
                    .fuse()
                    .collect::<Vec<_>>();
                // we reverse those then to get the proper order
                first_half.reverse();

                let mergers = first_half
                    .into_iter()
                    // we need to know, which is supposed to be active and we loose that info by chaining, so lets add a bool
                    .map(|w| (w, false))
                    .chain(
                        (0..)
                            // here we continuously remove the first element
                            .map(|i| {
                                (
                                    second_half
                                        .iter_mut()
                                        .flat_map(|&mut (ref o, ref mut w)| {
                                            if !w.is_empty() {
                                                Some((o.clone(), w.remove(0)))
                                            } else {
                                                None
                                            }
                                        })
                                        .collect::<Vec<_>>(),
                                    i == 0,
                                )
                            })
                            .filter(|(vec, _)| !vec.is_empty())
                            .fuse(),
                    );

                for (i, (workspaces, active)) in mergers.into_iter().enumerate() {
                    // and then we can merge each vector into one and put that into our new set.
                    let workspace_handle = state.create_workspace(&new_set.group).unwrap();
                    state.set_workspace_capabilities(
                        &workspace_handle,
                        [WorkspaceCapabilities::Activate].into_iter(),
                    );
                    workspace_set_idx(&mut state, i as u8 + 1, 0, &workspace_handle);

                    let mut new_workspace =
                        Workspace::new(workspace_handle, self.tiling_enabled, self.gaps);
                    for output in self.outputs.iter() {
                        new_workspace.map_output(output, output.current_location());
                    }
                    new_workspace.tiling_enabled = workspaces.iter().any(|(_, w)| w.tiling_enabled);

                    for (_output, workspace) in workspaces.into_iter() {
                        for toplevel in workspace.windows() {
                            self.toplevel_info_state
                                .toplevel_leave_workspace(&toplevel, &workspace.handle);
                            self.toplevel_info_state
                                .toplevel_enter_workspace(&toplevel, &new_workspace.handle);
                        }
                        new_workspace.tiling_layer.merge(workspace.tiling_layer);
                        new_workspace.floating_layer.merge(workspace.floating_layer);
                        new_workspace
                            .fullscreen
                            .extend(workspace.fullscreen.into_iter());
                        state.remove_workspace(workspace.handle);
                    }

                    if active {
                        new_set.active = new_set.workspaces.len();
                    }
                    new_set.workspaces.push(new_workspace);
                }

                for group in sets.values().map(|set| set.group) {
                    state.remove_workspace_group(group);
                }

                new_set.amount = amount;
                *dst = WorkspaceMode::Global(new_set);
            }
            (dst @ WorkspaceMode::Global(_), ConfigMode::OutputBound) => {
                // rustc should really be able to infer that this doesn't need an if.
                let set = if let &mut WorkspaceMode::Global(ref mut set) = dst {
                    set
                } else {
                    unreachable!()
                };

                // split workspaces apart, preserving window positions relative to their outputs
                let mut sets = HashMap::new();
                for (i, output) in self.outputs.iter().enumerate() {
                    let set = WorkspaceSet::new(
                        &mut state,
                        WorkspaceAmount::Static(0),
                        i,
                        self.tiling_enabled,
                        self.gaps,
                    );
                    state.add_group_output(&set.group, output);
                    sets.insert(output.clone(), set);
                }
                for (i, workspace) in set.workspaces.drain(..).enumerate() {
                    for (idx, output) in self.outputs.iter().enumerate() {
                        // copy over everything and then remove other outputs to preserve state
                        let new_set = sets.get_mut(output).unwrap();
                        let new_workspace_handle = state.create_workspace(&new_set.group).unwrap();
                        state.set_workspace_capabilities(
                            &new_workspace_handle,
                            [WorkspaceCapabilities::Activate].into_iter(),
                        );
                        workspace_set_idx(&mut state, i as u8 + 1, idx, &new_workspace_handle);

                        let mut old_tiling_layer = workspace.tiling_layer.clone();
                        let mut new_floating_layer = FloatingLayout::new();
                        let mut new_tiling_layer = TilingLayout::new(self.gaps);

                        for element in workspace.mapped() {
                            for (toplevel, _) in element.windows() {
                                self.toplevel_info_state
                                    .toplevel_leave_workspace(&toplevel, &workspace.handle);
                            }

                            if workspace
                                .floating_layer
                                .most_overlapped_output_for_element(element)
                                .as_ref()
                                == Some(output)
                            {
                                if let Some(mut old_mapped_loc) =
                                    workspace.floating_layer.space.element_location(element)
                                {
                                    let old_output_geo = workspace
                                        .floating_layer
                                        .space
                                        .output_geometry(output)
                                        .unwrap();
                                    old_mapped_loc -= old_output_geo.loc;
                                    new_floating_layer.map_internal(
                                        element.clone(),
                                        output,
                                        Some(old_mapped_loc),
                                    );
                                }
                            } else {
                                old_tiling_layer.unmap(element);
                            }
                        }

                        new_floating_layer.map_output(output, (0, 0).into());
                        new_tiling_layer.map_output(output, (0, 0).into());
                        new_tiling_layer.merge(old_tiling_layer);

                        let mut new_workspace = Workspace {
                            tiling_layer: new_tiling_layer,
                            floating_layer: new_floating_layer,
                            tiling_enabled: workspace.tiling_enabled,
                            fullscreen: workspace
                                .fullscreen
                                .iter()
                                .filter(|(key, _)| *key == output)
                                .map(|(o, w)| (o.clone(), w.clone()))
                                .collect(),
                            ..Workspace::new(new_workspace_handle, true, self.gaps)
                        };
                        for toplevel in new_workspace.windows() {
                            self.toplevel_info_state
                                .toplevel_enter_workspace(&toplevel, &new_workspace_handle);
                        }
                        new_workspace.refresh();

                        new_set.workspaces.push(new_workspace);
                        new_set.active = set.active;
                    }
                    state.remove_workspace(workspace.handle);
                }
                state.remove_workspace_group(set.group);

                for new_set in sets.values_mut() {
                    new_set.amount = set.amount;
                }
                *dst = WorkspaceMode::OutputBound(sets, set.amount);
            }
            _ => {}
        }

        std::mem::drop(state);
        self.refresh(); // get rid of empty workspaces and enforce potential maximum
    }

    pub fn activate(
        &mut self,
        output: &Output,
        idx: usize,
    ) -> Result<Option<Point<i32, Logical>>, InvalidWorkspaceIndex> {
        if match &mut self.workspaces {
            WorkspaceMode::OutputBound(sets, _) => {
                if let Some(set) = sets.get_mut(output) {
                    set.activate(idx, &mut self.workspace_state.update())?
                } else {
                    false
                }
            }
            WorkspaceMode::Global(set) => set.activate(idx, &mut self.workspace_state.update())?,
        } {
            let output_geo = output.geometry();
            Ok(Some(
                output_geo.loc + Point::from((output_geo.size.w / 2, output_geo.size.h / 2)),
            ))
        } else {
            Ok(None)
        }
    }

    pub fn active_space(&self, output: &Output) -> &Workspace {
        match &self.workspaces {
            WorkspaceMode::OutputBound(sets, _) => {
                let set = sets.get(output).unwrap();
                &set.workspaces[set.active]
            }
            WorkspaceMode::Global(set) => &set.workspaces[set.active],
        }
    }

    pub fn active_space_mut(&mut self, output: &Output) -> &mut Workspace {
        match &mut self.workspaces {
            WorkspaceMode::OutputBound(sets, _) => {
                let set = sets.get_mut(output).unwrap();
                &mut set.workspaces[set.active]
            }
            WorkspaceMode::Global(set) => &mut set.workspaces[set.active],
        }
    }

    pub fn visible_outputs_for_surface<'a>(
        &'a self,
        surface: &'a WlSurface,
    ) -> impl Iterator<Item = Output> + 'a {
        match self
            .outputs
            .iter()
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
                                or.geometry().intersection(o.geometry()).is_some()
                            } else {
                                false
                            }
                        })
                    })
                    .cloned()
                    .chain(self.outputs().map(|o| self.active_space(o)).flat_map(|w| {
                        w.mapped()
                            .find(|e| e.has_surface(surface, WindowSurfaceType::ALL))
                            .into_iter()
                            .flat_map(|e| w.outputs_for_element(e))
                    })),
            ),
        }
    }

    pub fn workspaces_for_surface(
        &self,
        surface: &WlSurface,
    ) -> impl Iterator<Item = (WorkspaceHandle, Output)> {
        match self.outputs.iter().find(|o| {
            let map = layer_map_for_output(o);
            map.layer_for_surface(surface, WindowSurfaceType::ALL)
                .is_some()
        }) {
            Some(output) => self
                .workspaces
                .spaces()
                .filter(move |workspace| {
                    workspace
                        .floating_layer
                        .space
                        .outputs()
                        .any(|o| o == output)
                })
                .map(|w| (w.handle.clone(), output.clone()))
                .collect::<Vec<_>>(),
            None => self
                .workspaces
                .spaces()
                .filter_map(|w| {
                    if let Some(mapped) = w
                        .mapped()
                        .find(|e| e.has_surface(surface, WindowSurfaceType::ALL))
                    {
                        let outputs = w.outputs_for_element(mapped);
                        Some(std::iter::repeat(w.handle.clone()).zip(outputs).fuse())
                    } else {
                        None
                    }
                })
                .flatten()
                .collect::<Vec<_>>(),
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

    pub fn outputs(&self) -> impl Iterator<Item = &Output> {
        self.outputs.iter()
    }

    pub fn global_space(&self) -> Rectangle<i32, Logical> {
        self.outputs
            .iter()
            .fold(
                Option::<Rectangle<i32, Logical>>::None,
                |maybe_geo, output| match maybe_geo {
                    Some(rect) => Some(rect.merge(output.geometry())),
                    None => Some(output.geometry()),
                },
            )
            .unwrap_or_else(|| Rectangle::from_loc_and_size((0, 0), (0, 0)))
    }

    pub fn map_global_to_space<C: smithay::utils::Coordinate>(
        &self,
        global_loc: impl Into<Point<C, Logical>>,
        output: &Output,
    ) -> Point<C, Logical> {
        match self.workspaces {
            WorkspaceMode::Global(_) => global_loc.into(),
            WorkspaceMode::OutputBound(_, _) => {
                let p = global_loc.into().to_f64() - output.current_location().to_f64();
                (C::from_f64(p.x), C::from_f64(p.y)).into()
            }
        }
    }

    pub fn animations_going(&self) -> bool {
        (match &self.workspaces {
            WorkspaceMode::Global(set) => set.previously_active.is_some(),
            WorkspaceMode::OutputBound(sets, _) => {
                sets.values().any(|set| set.previously_active.is_some())
            }
        }) || !matches!(self.overview_mode, OverviewMode::None)
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

    pub fn set_overview_mode(&mut self, enabled: Option<KeyModifiers>) {
        if let Some(modifiers) = enabled {
            if !matches!(self.overview_mode, OverviewMode::Started(_, _)) {
                self.overview_mode = OverviewMode::Started(modifiers, Instant::now());
            }
        } else {
            if !matches!(self.overview_mode, OverviewMode::Ended(_)) {
                let reverse_duration = if let OverviewMode::Started(_, start) = self.overview_mode {
                    ANIMATION_DURATION
                        - Instant::now().duration_since(start).min(ANIMATION_DURATION)
                } else {
                    Duration::ZERO
                };
                self.overview_mode = OverviewMode::Ended(Instant::now() - reverse_duration);
            }
        }
    }

    pub fn overview_mode(&mut self) -> OverviewMode {
        if let OverviewMode::Ended(timestamp) = self.overview_mode {
            if Instant::now().duration_since(timestamp) > ANIMATION_DURATION {
                self.overview_mode = OverviewMode::None;
            }
        }

        self.overview_mode.clone()
    }

    pub fn set_resize_mode(
        &mut self,
        enabled: Option<(KeyPattern, ResizeDirection)>,
        config: &Config,
        evlh: LoopHandle<'static, crate::state::Data>,
    ) {
        if let Some((pattern, direction)) = enabled {
            if let ResizeMode::Started(old_pattern, _, old_direction) = &mut self.resize_mode {
                *old_pattern = pattern;
                *old_direction = direction;
            } else {
                self.resize_mode = ResizeMode::Started(pattern, Instant::now(), direction);
            }
            self.resize_indicator = Some(resize_indicator(direction, config, evlh));
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

        match &mut self.workspaces {
            WorkspaceMode::OutputBound(sets, _) => {
                for (output, set) in sets.iter_mut() {
                    set.refresh(
                        &mut self.workspace_state,
                        &mut self.toplevel_info_state,
                        std::iter::once((output, (0, 0).into())),
                    );
                }
            }
            WorkspaceMode::Global(set) => set.refresh(
                &mut self.workspace_state,
                &mut self.toplevel_info_state,
                self.outputs.iter().map(|o| (o, o.current_location())),
            ),
        }

        for output in &self.outputs {
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

    pub fn map_window(state: &mut State, window: &CosmicSurface, output: &Output) {
        let pos = state
            .common
            .shell
            .pending_windows
            .iter()
            .position(|(w, _)| w == window)
            .unwrap();
        let (window, seat) = state.common.shell.pending_windows.remove(pos);

        let workspace = state.common.shell.workspaces.active_mut(output);
        workspace.set_fullscreen(None, output);
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
        ));
        #[cfg(feature = "debug")]
        {
            mapped.set_debug(state.common.egui.active);
        }
        if layout::should_be_floating(&window) || !workspace.tiling_enabled {
            workspace.floating_layer.map(mapped.clone(), &seat, None);
        } else {
            let focus_stack = workspace.focus_stack.get(&seat);
            workspace
                .tiling_layer
                .map(mapped.clone(), &seat, focus_stack.iter(), None);
        }

        if let CosmicSurface::X11(surface) = window {
            if let Some(xwm) = state
                .common
                .xwayland_state
                .as_mut()
                .and_then(|state| state.xwm.as_mut())
            {
                if let Err(err) = xwm.raise_window(&surface) {
                    warn!(?err, "Failed to update Xwayland stacking order.");
                }
            }
        }

        Shell::set_focus(state, Some(&KeyboardFocusTarget::from(mapped)), &seat, None);

        let active_space = state.common.shell.active_space(output);
        for mapped in active_space.mapped() {
            state.common.shell.update_reactive_popups(mapped);
        }
    }

    pub fn map_override_redirect(state: &mut State, window: X11Surface) {
        let geo = window.geometry();
        for (output, overlap) in state
            .common
            .shell
            .outputs()
            .cloned()
            .filter_map(|o| o.geometry().intersection(geo).map(|overlap| (o, overlap)))
        {
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
            workspace.tiling_layer.recalculate(&output);
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
    ) -> Result<Option<Point<i32, Logical>>, InvalidWorkspaceIndex> {
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

        let Some(mapped) = maybe_window else { return Ok(None); };
        let Some(window_state) = from_workspace.unmap(&mapped) else { return Ok(None); };

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

        let to_workspace = state
            .common
            .shell
            .workspaces
            .get_mut(to_idx, to_output)
            .unwrap(); // checked above
        let focus_stack = to_workspace.focus_stack.get(&seat);
        if window_state == ManagedState::Floating {
            to_workspace.floating_layer.map(mapped.clone(), &seat, None);
        } else {
            to_workspace
                .tiling_layer
                .map(mapped.clone(), &seat, focus_stack.iter(), direction);
        }
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
            Common::set_focus(state, Some(&KeyboardFocusTarget::from(mapped)), &seat, None);
        }
        Ok(new_pos)
    }

    pub fn update_reactive_popups(&self, mapped: &CosmicMapped) {
        if let Some(workspace) = self.space_for(mapped) {
            let element_loc = workspace.element_geometry(mapped).unwrap().loc;
            for (toplevel, offset) in mapped.windows() {
                if let CosmicSurface::Wayland(toplevel) = toplevel {
                    let window_geo_offset = toplevel.geometry().loc;
                    update_reactive_popups(
                        &toplevel,
                        element_loc + offset + window_geo_offset,
                        self.outputs.iter(),
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
                    if let Some(grab) = workspace.move_request(
                        &window,
                        &seat,
                        &output,
                        start_data,
                        state.common.config.static_conf.active_hint,
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
        let Some(focused) = seat.get_keyboard().unwrap().current_focus() else { return };

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
                let Some(toplevel) = old_focused.toplevel() else { return };
                let Some(mapped) = workspace
                    .mapped()
                    .find(|m| m.has_surface(&toplevel, WindowSurfaceType::TOPLEVEL))
                    .cloned()
                else { return };
                let mut resize_state = mapped.resize_state.lock().unwrap();
                if let Some(ResizeState::Resizing(data)) = *resize_state {
                    *resize_state = Some(ResizeState::WaitingForCommit(data));
                }
            }
        }
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
