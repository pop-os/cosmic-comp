use serde::{Deserialize, Serialize};
use std::{cell::RefCell, collections::HashMap};
use tracing::warn;

use cosmic_protocols::workspace::v1::server::zcosmic_workspace_handle_v1::State as WState;
use smithay::{
    desktop::{
        layer_map_for_output, space::SpaceElement, LayerSurface, PopupManager, WindowSurfaceType,
    },
    input::{
        pointer::{Focus, GrabStartData as PointerGrabStartData},
        Seat,
    },
    output::Output,
    reexports::wayland_server::{protocol::wl_surface::WlSurface, DisplayHandle},
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
    config::{Config, OutputConfig, WorkspaceMode as ConfigMode},
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

mod element;
pub mod focus;
pub mod grabs;
pub mod layout;
mod workspace;
pub use self::element::{CosmicMapped, CosmicMappedRenderElement, CosmicSurface};
pub use self::workspace::*;
use self::{
    element::CosmicWindow,
    focus::target::KeyboardFocusTarget,
    grabs::ResizeEdge,
    layout::{floating::FloatingLayout, tiling::TilingLayout},
};

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
}

#[derive(Debug)]
pub struct WorkspaceSet {
    active: usize,
    amount: WorkspaceAmount,
    group: WorkspaceGroupHandle,
    idx: usize,
    tiling_enabled: bool,
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
) -> Workspace {
    let workspace_handle = state.create_workspace(&group_handle).unwrap();
    if active {
        state.add_workspace_state(&workspace_handle, WState::Active);
    }
    state.set_workspace_capabilities(
        &workspace_handle,
        [WorkspaceCapabilities::Activate].into_iter(),
    );
    Workspace::new(workspace_handle, tiling)
}

impl WorkspaceSet {
    fn new(
        state: &mut WorkspaceUpdateGuard<'_, State>,
        amount: WorkspaceAmount,
        idx: usize,
        tiling_enabled: bool,
    ) -> WorkspaceSet {
        let group_handle = state.create_workspace_group();

        let workspaces = match amount {
            WorkspaceAmount::Dynamic => {
                let workspace = create_workspace(state, &group_handle, true, tiling_enabled);
                workspace_set_idx(state, 1, idx, &workspace.handle);
                state.set_workspace_capabilities(
                    &workspace.handle,
                    [WorkspaceCapabilities::Activate].into_iter(),
                );
                vec![workspace]
            }
            WorkspaceAmount::Static(len) => (0..len)
                .map(|i| {
                    let workspace = create_workspace(state, &group_handle, i == 0, tiling_enabled);
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
            active: 0,
            amount,
            group: group_handle,
            idx,
            tiling_enabled,
            workspaces,
        }
    }

    fn activate(&mut self, idx: usize, state: &mut WorkspaceUpdateGuard<'_, State>) {
        if idx < self.workspaces.len() && self.active != idx {
            let old_active = self.active;
            state.remove_workspace_state(&self.workspaces[old_active].handle, WState::Active);
            state.add_workspace_state(&self.workspaces[idx].handle, WState::Active);
            self.active = idx;
        }
    }

    fn refresh<'a>(
        &mut self,
        state: &mut WorkspaceState<State>,
        toplevel_info: &mut ToplevelInfoState<State, CosmicSurface>,
        outputs: impl Iterator<Item = (&'a Output, Point<i32, Logical>)>,
    ) {
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
            let mut workspace =
                create_workspace(&mut state, &self.group, false, self.tiling_enabled);
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
                let mut workspace =
                    create_workspace(&mut state, &self.group, false, self.tiling_enabled);
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
    ) -> WorkspaceMode {
        match config {
            crate::config::WorkspaceMode::Global => {
                WorkspaceMode::Global(WorkspaceSet::new(state, amount, 0, tiling_enabled))
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

    pub fn active(&self, output: &Output) -> &Workspace {
        match self {
            WorkspaceMode::Global(set) => &set.workspaces[set.active],
            WorkspaceMode::OutputBound(sets, _) => {
                let set = sets.get(output).unwrap();
                &set.workspaces[set.active]
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

    pub fn active_num(&self, output: &Output) -> usize {
        match self {
            WorkspaceMode::Global(set) => set.active,
            WorkspaceMode::OutputBound(sets, _) => {
                let set = sets.get(output).unwrap();
                set.active
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
                    let set =
                        WorkspaceSet::new(&mut state, *amount, sets.len(), self.tiling_enabled);
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

                    let mut new_workspace = Workspace::new(workspace_handle, self.tiling_enabled);
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
                        let mut new_tiling_layer = TilingLayout::new();

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
                            ..Workspace::new(new_workspace_handle, true)
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

    pub fn activate(&mut self, output: &Output, idx: usize) -> Option<Point<i32, Logical>> {
        match &mut self.workspaces {
            WorkspaceMode::OutputBound(sets, _) => {
                if let Some(set) = sets.get_mut(output) {
                    set.activate(idx, &mut self.workspace_state.update());
                }
            }
            WorkspaceMode::Global(set) => {
                set.activate(idx, &mut self.workspace_state.update());
            }
        }

        let output_geo = output.geometry();
        Some(output_geo.loc + Point::from((output_geo.size.w / 2, output_geo.size.h / 2)))
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
        match self.outputs.iter().find(|o| {
            let map = layer_map_for_output(o);
            map.layer_for_surface(surface, WindowSurfaceType::ALL)
                .is_some()
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

    pub fn refresh(&mut self) {
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
                .map(mapped.clone(), &seat, focus_stack.iter());
        }

        if let CosmicSurface::X11(surface) = window {
            if let Some(xwm) = state
                .common
                .xwayland_state
                .values_mut()
                .flat_map(|state| state.xwm.as_mut())
                .find(|xwm| Some(xwm.id()) == surface.xwm_id())
            {
                if let Err(err) = xwm.update_stacking_order_downwards(workspace.mapped()) {
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

        let mut map = layer_map_for_output(&output);
        map.map_layer(&layer_surface).unwrap();

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
    ) -> Option<Point<i32, Logical>> {
        let (to_output, to_idx) = to;
        let to_idx = to_idx.unwrap_or(state.common.shell.workspaces.active_num(to_output));
        if state
            .common
            .shell
            .workspaces
            .get(to_idx, to_output)
            .is_none()
        {
            return None;
        }

        if from_output == to_output
            && to_idx == state.common.shell.workspaces.active_num(from_output)
        {
            return None;
        }

        let from_workspace = state.common.shell.workspaces.active_mut(from_output);
        let maybe_window = from_workspace.focus_stack.get(seat).last().cloned();

        let Some(mapped) = maybe_window else { return None; };
        let Some(window_state) = from_workspace.unmap(&mapped) else { return None; };

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
        std::mem::drop(from_workspace);
        for mapped in elements.into_iter() {
            state.common.shell.update_reactive_popups(&mapped);
        }
        let new_pos = if follow {
            seat.set_active_output(&to_output);
            state.common.shell.activate(to_output, to_idx)
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
                .map(mapped.clone(), &seat, focus_stack.iter());
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
        new_pos
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
                    if let Some(grab) = workspace.move_request(&window, &seat, &output, start_data)
                    {
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
