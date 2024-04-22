use calloop::LoopHandle;
use indexmap::IndexMap;
use std::{
    collections::HashMap,
    sync::atomic::Ordering,
    time::{Duration, Instant},
};
use wayland_backend::server::ClientId;

use cosmic_comp_config::{workspace::WorkspaceMode, TileBehavior};
use cosmic_protocols::workspace::v1::server::zcosmic_workspace_handle_v1::{
    State as WState, TilingState,
};
use keyframe::{ease, functions::EaseInOutCubic};
use smithay::{
    backend::input::TouchSlot,
    desktop::{
        layer_map_for_output, space::SpaceElement, utils::surface_primary_scanout_output,
        LayerSurface, PopupKind, WindowSurface, WindowSurfaceType,
    },
    input::{
        pointer::{Focus, GrabStartData as PointerGrabStartData},
        Seat,
    },
    output::Output,
    reexports::{
        wayland_protocols::ext::session_lock::v1::server::ext_session_lock_v1::ExtSessionLockV1,
        wayland_server::{protocol::wl_surface::WlSurface, Client},
    },
    utils::{IsAlive, Logical, Point, Rectangle, Serial, Size},
    wayland::{
        compositor::with_states,
        seat::WaylandFocus,
        session_lock::LockSurface,
        shell::wlr_layer::{KeyboardInteractivity, Layer, LayerSurfaceCachedState},
        xdg_activation::XdgActivationState,
    },
    xwayland::X11Surface,
};

use crate::{
    backend::render::animations::spring::{Spring, SpringParams},
    config::{Config, KeyModifiers, KeyPattern},
    utils::prelude::*,
    wayland::{
        handlers::{
            toplevel_management::minimize_rectangle, xdg_activation::ActivationContext,
            xdg_shell::popup::get_popup_toplevel,
        },
        protocols::{
            toplevel_info::{
                toplevel_enter_output, toplevel_enter_workspace, toplevel_leave_output,
                toplevel_leave_workspace, ToplevelInfoState,
            },
            workspace::{
                WorkspaceCapabilities, WorkspaceGroupHandle, WorkspaceHandle, WorkspaceState,
                WorkspaceUpdateGuard,
            },
        },
    },
};

pub mod element;
pub mod focus;
pub mod grabs;
pub mod layout;
mod seats;
mod workspace;
pub use self::element::{CosmicMapped, CosmicMappedRenderElement, CosmicSurface};
pub use self::seats::*;
pub use self::workspace::*;
use self::{
    element::{
        resize_indicator::{resize_indicator, ResizeIndicator},
        swap_indicator::{swap_indicator, SwapIndicator},
        CosmicWindow, MaximizedState,
    },
    focus::{
        target::{KeyboardFocusTarget, PointerFocusTarget},
        FocusDirection,
    },
    grabs::{
        tab_items, window_items, GrabStartData, Item, MenuGrab, MoveGrab, ReleaseMode, ResizeEdge,
        ResizeGrab,
    },
    layout::{
        floating::{FloatingLayout, ResizeState},
        tiling::{NodeDesc, ResizeForkGrab, TilingLayout},
    },
};

const ANIMATION_DURATION: Duration = Duration::from_millis(200);
const GESTURE_MAX_LENGTH: f64 = 150.0;
const GESTURE_POSITION_THRESHOLD: f64 = 0.5;
const GESTURE_VELOCITY_THRESHOLD: f64 = 0.02;

#[derive(Debug, Clone)]
pub enum Trigger {
    KeyboardSwap(KeyPattern, NodeDesc),
    KeyboardMove(KeyModifiers),
    Pointer(u32),
    Touch(TouchSlot),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ActivationKey {
    Wayland(WlSurface),
    X11(u32),
}

impl From<&CosmicSurface> for ActivationKey {
    fn from(value: &CosmicSurface) -> Self {
        match value.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                ActivationKey::Wayland(toplevel.wl_surface().clone())
            }
            WindowSurface::X11(s) => ActivationKey::X11(s.window_id()),
        }
    }
}

#[derive(Debug)]
pub struct Shell {
    pub workspaces: Workspaces,

    pub pending_windows: Vec<(CosmicSurface, Seat<State>, Option<Output>)>,
    pub pending_layers: Vec<(LayerSurface, Output, Seat<State>)>,
    pub pending_activations: HashMap<ActivationKey, ActivationContext>,
    pub override_redirect_windows: Vec<X11Surface>,
    pub session_lock: Option<SessionLock>,
    pub seats: Seats,

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
pub struct SessionLock {
    pub ext_session_lock: ExtSessionLockV1,
    pub surfaces: HashMap<Output, LockSurface>,
}

#[derive(Debug, Clone, Copy)]
pub enum WorkspaceDelta {
    Shortcut(Instant),
    Gesture(f64),
    GestureEnd(Instant, Spring),
    // InvalidGesture(f64), TODO
    // InvalidGestureEnd(Instant, Spring), TODO
}

impl WorkspaceDelta {
    pub fn new_gesture() -> Self {
        WorkspaceDelta::Gesture(0.0)
    }

    pub fn new_gesture_end(delta: f64, velocity: f64) -> Self {
        let params: SpringParams = SpringParams::new(1.0, 1000.0, 0.0001);
        WorkspaceDelta::GestureEnd(
            Instant::now(),
            Spring {
                from: delta,
                to: 1.0,
                initial_velocity: velocity,
                params,
            },
        )
    }

    pub fn new_shortcut() -> Self {
        WorkspaceDelta::Shortcut(Instant::now())
    }

    pub fn is_animating(&self) -> bool {
        matches!(
            self,
            WorkspaceDelta::Shortcut(_) | WorkspaceDelta::GestureEnd(_, _)
        )
    }
}

#[derive(Debug)]
pub struct WorkspaceSet {
    previously_active: Option<(usize, WorkspaceDelta)>,
    pub active: usize,
    pub group: WorkspaceGroupHandle,
    idx: usize,
    tiling_enabled: bool,
    output: Output,
    theme: cosmic::Theme,
    pub sticky_layer: FloatingLayout,
    pub minimized_windows: Vec<MinimizedWindow>,
    pub workspaces: Vec<Workspace>,
}

fn create_workspace(
    state: &mut WorkspaceUpdateGuard<'_, State>,
    output: &Output,
    group_handle: &WorkspaceGroupHandle,
    active: bool,
    tiling: bool,
    theme: cosmic::Theme,
) -> Workspace {
    let workspace_handle = state
        .create_workspace(
            &group_handle,
            if tiling {
                TilingState::TilingEnabled
            } else {
                TilingState::FloatingOnly
            },
        )
        .unwrap();
    if active {
        state.add_workspace_state(&workspace_handle, WState::Active);
    }
    state.set_workspace_capabilities(
        &workspace_handle,
        [WorkspaceCapabilities::Activate].into_iter(),
    );
    Workspace::new(workspace_handle, output.clone(), tiling, theme.clone())
}

fn move_workspace_to_group(
    workspace: &mut Workspace,
    group: &WorkspaceGroupHandle,
    workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
) {
    let old_workspace_handle = workspace.handle;
    workspace.handle = workspace_state
        .create_workspace(
            group,
            if workspace.tiling_enabled {
                TilingState::TilingEnabled
            } else {
                TilingState::FloatingOnly
            },
        )
        .unwrap();
    workspace_state.set_workspace_capabilities(
        &workspace.handle,
        [WorkspaceCapabilities::Activate].into_iter(),
    );
    for window in workspace.mapped() {
        for (surface, _) in window.windows() {
            toplevel_leave_workspace(&surface, &old_workspace_handle);
            toplevel_enter_workspace(&surface, &workspace.handle);
        }
    }
    for window in workspace.minimized_windows.iter() {
        for (surface, _) in window.window.windows() {
            toplevel_leave_workspace(&surface, &old_workspace_handle);
            toplevel_enter_workspace(&surface, &workspace.handle);
        }
    }
    workspace_state.remove_workspace(old_workspace_handle);
}

/* We will probably need this again at some point
fn merge_workspaces(
    mut workspace: Workspace,
    into: &mut Workspace,
    workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
    toplevel_info_state: &mut ToplevelInfoState<State, CosmicSurface>,
) {
    if into.fullscreen.is_some() {
        // Don't handle the returned original workspace, for this nieche case.
        let _ = workspace.remove_fullscreen();
    }

    for element in workspace.mapped() {
        // fixup toplevel state
        for (toplevel, _) in element.windows() {
            toplevel_info_state.toplevel_leave_workspace(&toplevel, &workspace.handle);
            toplevel_info_state.toplevel_enter_workspace(&toplevel, &into.handle);
        }
    }
    // TODO: merge minimized windows
    into.tiling_layer.merge(workspace.tiling_layer);
    into.floating_layer.merge(workspace.floating_layer);
    workspace_state.remove_workspace(workspace.handle);
}
*/

impl WorkspaceSet {
    fn new(
        state: &mut WorkspaceUpdateGuard<'_, State>,
        output: &Output,
        idx: usize,
        tiling_enabled: bool,
        theme: cosmic::Theme,
    ) -> WorkspaceSet {
        let group_handle = state.create_workspace_group();
        let workspaces = {
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
        };
        let sticky_layer = FloatingLayout::new(theme.clone(), output);

        WorkspaceSet {
            previously_active: None,
            active: 0,
            group: group_handle,
            idx,
            tiling_enabled,
            theme,
            sticky_layer,
            minimized_windows: Vec::new(),
            workspaces,
            output: output.clone(),
        }
    }

    fn activate(
        &mut self,
        idx: usize,
        workspace_delta: WorkspaceDelta,
        state: &mut WorkspaceUpdateGuard<'_, State>,
    ) -> Result<bool, InvalidWorkspaceIndex> {
        if idx >= self.workspaces.len() {
            return Err(InvalidWorkspaceIndex);
        }

        if self.active != idx {
            let old_active = self.active;
            state.remove_workspace_state(&self.workspaces[old_active].handle, WState::Active);
            state.remove_workspace_state(&self.workspaces[old_active].handle, WState::Urgent);
            state.remove_workspace_state(&self.workspaces[idx].handle, WState::Urgent);
            state.add_workspace_state(&self.workspaces[idx].handle, WState::Active);
            self.previously_active = Some((old_active, workspace_delta));
            self.active = idx;
            Ok(true)
        } else {
            if let Some((p_idx, _)) = self.previously_active {
                self.previously_active = Some((p_idx, workspace_delta));
                return Ok(true);
            }
            Ok(false)
        }
    }

    fn activate_previous(
        &mut self,
        workspace_delta: WorkspaceDelta,
        state: &mut WorkspaceUpdateGuard<'_, State>,
    ) -> Result<bool, InvalidWorkspaceIndex> {
        if let Some((idx, _)) = self.previously_active {
            return self.activate(idx, workspace_delta, state);
        }
        Err(InvalidWorkspaceIndex)
    }

    fn update_workspace_delta(&mut self, delta: f64) {
        let easing = delta.clamp(0.0, GESTURE_MAX_LENGTH).abs() / GESTURE_MAX_LENGTH;
        if let Some((idx, _)) = self.previously_active {
            self.previously_active = Some((idx, WorkspaceDelta::Gesture(easing)));
        }
    }

    fn set_output(&mut self, new_output: &Output) {
        self.sticky_layer.set_output(new_output);
        for window in self.sticky_layer.windows() {
            toplevel_leave_output(&window, &self.output);
            toplevel_enter_output(&window, &new_output);
        }
        for workspace in &mut self.workspaces {
            workspace.set_output(new_output);
        }
        self.output = new_output.clone();
    }

    fn refresh<'a>(&mut self, xdg_activation_state: &XdgActivationState) {
        if let Some((_, start)) = self.previously_active {
            match start {
                WorkspaceDelta::Shortcut(st) => {
                    if Instant::now().duration_since(st).as_millis() as f32
                        >= ANIMATION_DURATION.as_millis() as f32
                    {
                        self.previously_active = None;
                    }
                }
                WorkspaceDelta::GestureEnd(st, spring) => {
                    if Instant::now().duration_since(st).as_millis() > spring.duration().as_millis()
                    {
                        self.previously_active = None;
                    }
                }
                _ => {}
            }
        } else {
            self.workspaces[self.active].refresh(xdg_activation_state);
        }
        self.sticky_layer.refresh();
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
            // this method is only used by code paths related to dynamic workspaces, so this should be fine
        );
        self.workspaces.push(workspace);
    }

    fn ensure_last_empty<'a>(&mut self, state: &mut WorkspaceUpdateGuard<State>) {
        // add empty at the end, if necessary
        if self
            .workspaces
            .last()
            .map(|last| !last.is_empty())
            .unwrap_or(true)
        {
            self.add_empty_workspace(state);
        }

        // remove empty workspaces in between, if they are not active
        let len = self.workspaces.len();
        let mut keep = vec![true; len];
        for (i, workspace) in self.workspaces.iter().enumerate() {
            if workspace.is_empty() && i != self.active && i != len - 1 {
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

    fn update_idx(&mut self, state: &mut WorkspaceUpdateGuard<'_, State>, idx: usize) {
        self.idx = idx;
        for (i, workspace) in self.workspaces.iter().enumerate() {
            workspace_set_idx(state, i as u8 + 1, idx, &workspace.handle);
        }
    }
}

#[derive(Debug)]
pub struct Workspaces {
    pub sets: IndexMap<Output, WorkspaceSet>,
    backup_set: Option<WorkspaceSet>,
    mode: WorkspaceMode,
    autotile: bool,
    autotile_behavior: TileBehavior,
    theme: cosmic::Theme,
}

impl Workspaces {
    pub fn new(config: &Config, theme: cosmic::Theme) -> Workspaces {
        Workspaces {
            sets: IndexMap::new(),
            backup_set: None,
            mode: config.cosmic_conf.workspaces.workspace_mode,
            autotile: config.cosmic_conf.autotile,
            autotile_behavior: config.cosmic_conf.autotile_behavior,
            theme,
        }
    }

    pub fn add_output(
        &mut self,
        output: &Output,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        xdg_activation_state: &XdgActivationState,
    ) {
        if self.sets.contains_key(output) {
            return;
        }

        let set = self
            .backup_set
            .take()
            .map(|mut set| {
                set.set_output(output);
                set
            })
            .unwrap_or_else(|| {
                WorkspaceSet::new(
                    workspace_state,
                    &output,
                    self.sets.len(),
                    self.autotile,
                    self.theme.clone(),
                )
            });
        workspace_state.add_group_output(&set.group, &output);

        self.sets.insert(output.clone(), set);
        let mut moved_workspaces = Vec::new();
        for set in self.sets.values_mut() {
            let (preferrs, doesnt) = set
                .workspaces
                .drain(..)
                .partition(|w| w.preferrs_output(output));
            moved_workspaces.extend(preferrs);
            set.workspaces = doesnt;
            if set.workspaces.is_empty() {
                set.add_empty_workspace(workspace_state);
            }
            set.active = set.active.min(set.workspaces.len() - 1);
        }
        {
            let set = self.sets.get_mut(output).unwrap();
            for workspace in &mut moved_workspaces {
                move_workspace_to_group(workspace, &set.group, workspace_state);
            }
            set.workspaces.extend(moved_workspaces);
            for (i, workspace) in set.workspaces.iter_mut().enumerate() {
                workspace.set_output(output);
                workspace.refresh(xdg_activation_state);
                workspace_set_idx(workspace_state, i as u8 + 1, set.idx, &workspace.handle);
                if i == set.active {
                    workspace_state.add_workspace_state(&workspace.handle, WState::Active);
                }
            }
        }
    }

    pub fn remove_output<'a>(
        &mut self,
        output: &Output,
        seats: impl Iterator<Item = &'a Seat<State>>,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        xdg_activation_state: &XdgActivationState,
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
                for mut workspace in set.workspaces.into_iter() {
                    // update workspace protocol state
                    move_workspace_to_group(&mut workspace, &workspace_group, workspace_state);

                    // update mapping
                    workspace.set_output(&new_output);
                    workspace.refresh(xdg_activation_state);
                    new_set.workspaces.push(workspace);
                }

                for window in set.sticky_layer.mapped() {
                    for (surface, _) in window.windows() {
                        toplevel_leave_output(&surface, output);
                        toplevel_enter_output(&surface, &new_output);
                    }
                }
                new_set.sticky_layer.merge(set.sticky_layer);
                for window in set.minimized_windows.iter() {
                    for (surface, _) in window.window.windows() {
                        toplevel_leave_output(&surface, output);
                        toplevel_enter_output(&surface, &new_output);
                    }
                }
                new_set.minimized_windows.extend(set.minimized_windows);

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

            self.refresh(workspace_state, xdg_activation_state)
        }
    }

    fn migrate_workspace(
        &mut self,
        from: &Output,
        to: &Output,
        handle: &WorkspaceHandle,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        xdg_activation_state: &XdgActivationState,
    ) {
        if !self.sets.contains_key(to) {
            return;
        }

        if let Some(mut workspace) = self.sets.get_mut(from).and_then(|set| {
            let pos = set.workspaces.iter().position(|w| &w.handle == handle)?;
            Some(set.workspaces.remove(pos))
        }) {
            let new_set = self.sets.get_mut(to).unwrap();
            move_workspace_to_group(&mut workspace, &new_set.group, workspace_state);
            workspace.set_output(to);
            workspace.refresh(xdg_activation_state);
            new_set.workspaces.insert(new_set.active + 1, workspace)
        }
    }

    pub fn update_config(
        &mut self,
        config: &Config,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        xdg_activation_state: &XdgActivationState,
    ) {
        let old_mode = self.mode;
        self.mode = config.cosmic_conf.workspaces.workspace_mode;

        if self.sets.len() <= 1 {
            return;
        }

        match (old_mode, self.mode) {
            (WorkspaceMode::Global, WorkspaceMode::OutputBound) => {
                // We basically just unlink the existing spaces, so nothing needs to be updated
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
                                    &set.group,
                                    false,
                                    config.cosmic_conf.autotile,
                                    self.theme.clone(),
                                ),
                            );
                        }
                        // Otherwise we are fine
                    }
                }
                // fixup indices
                for (i, set) in self.sets.values_mut().enumerate() {
                    set.update_idx(workspace_state, i);
                }
            }
            _ => {}
        };

        self.refresh(workspace_state, xdg_activation_state)
    }

    pub fn recalculate(&mut self) {
        for set in self.sets.values_mut() {
            set.sticky_layer.refresh();
            set.workspaces.iter_mut().for_each(|w| w.recalculate());
        }
    }

    pub fn refresh(
        &mut self,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        xdg_activation_state: &XdgActivationState,
    ) {
        match self.mode {
            WorkspaceMode::Global => {
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
                    let has_windows = self.sets.values().any(|s| !s.workspaces[i].is_empty());

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
            WorkspaceMode::OutputBound => {
                for set in self.sets.values_mut() {
                    set.ensure_last_empty(workspace_state);
                }
            }
        }

        for set in self.sets.values_mut() {
            set.refresh(xdg_activation_state)
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

    pub fn active(&self, output: &Output) -> (Option<(&Workspace, WorkspaceDelta)>, &Workspace) {
        let set = self.sets.get(output).or(self.backup_set.as_ref()).unwrap();
        (
            set.previously_active
                .map(|(idx, start)| (&set.workspaces[idx], start)),
            &set.workspaces[set.active],
        )
    }

    pub fn active_mut(&mut self, output: &Output) -> &mut Workspace {
        let set = self
            .sets
            .get_mut(output)
            .or(self.backup_set.as_mut())
            .unwrap();
        &mut set.workspaces[set.active]
    }

    pub fn active_num(&self, output: &Output) -> (Option<usize>, usize) {
        let set = self.sets.get(output).or(self.backup_set.as_ref()).unwrap();
        (set.previously_active.map(|(idx, _)| idx), set.active)
    }

    pub fn idx_for_handle(&self, output: &Output, handle: &WorkspaceHandle) -> Option<usize> {
        let set = self.sets.get(output).unwrap();
        set.workspaces
            .iter()
            .enumerate()
            .find_map(|(i, w)| (&w.handle == handle).then_some(i))
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

    pub fn space_for_handle(&self, handle: &WorkspaceHandle) -> Option<&Workspace> {
        self.spaces().find(|w| &w.handle == handle)
    }

    pub fn space_for_handle_mut(&mut self, handle: &WorkspaceHandle) -> Option<&mut Workspace> {
        self.spaces_mut().find(|w| &w.handle == handle)
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

    pub fn set_theme(&mut self, theme: cosmic::Theme, xdg_activation_state: &XdgActivationState) {
        for (_, s) in &mut self.sets {
            s.theme = theme.clone();

            s.sticky_layer.theme = theme.clone();
            s.sticky_layer.mapped().for_each(|m| {
                m.update_theme(theme.clone());
                m.force_redraw();
            });
            s.sticky_layer.refresh();

            for w in &mut s.workspaces {
                w.tiling_layer.theme = theme.clone();
                w.floating_layer.theme = theme.clone();

                w.mapped().for_each(|m| {
                    m.update_theme(theme.clone());
                    m.force_redraw();
                });

                w.refresh(xdg_activation_state);
                w.dirty.store(true, Ordering::Relaxed);
                w.recalculate();
            }
        }
    }

    pub fn update_autotile_behavior<'a>(
        &mut self,
        behavior: TileBehavior,
        guard: &mut WorkspaceUpdateGuard<'_, State>,
        seats: impl Iterator<Item = &'a Seat<State>>,
    ) {
        self.autotile_behavior = behavior;
        self.apply_tile_change(guard, seats);
    }

    fn apply_tile_change<'a>(
        &mut self,
        guard: &mut WorkspaceUpdateGuard<'_, State>,
        seats: impl Iterator<Item = &'a Seat<State>>,
    ) {
        let seats = seats.cloned().collect::<Vec<_>>();
        for (_, set) in &mut self.sets {
            set.tiling_enabled = self.autotile;

            if matches!(self.autotile_behavior, TileBehavior::Global) {
                // must apply change to all workspaces now
                for w in &mut set.workspaces {
                    if w.tiling_enabled == self.autotile {
                        continue;
                    }
                    for s in &seats {
                        w.toggle_tiling(s, guard);
                    }
                }
            }
        }
    }

    pub fn update_autotile<'a>(
        &mut self,
        autotile: bool,
        guard: &mut WorkspaceUpdateGuard<'_, State>,
        seats: impl Iterator<Item = &'a Seat<State>>,
    ) {
        self.autotile = autotile;
        self.apply_tile_change(guard, seats);
    }
}

#[derive(Debug)]
pub struct InvalidWorkspaceIndex;

impl Common {
    pub fn add_output(&mut self, output: &Output) {
        let mut shell = self.shell.write().unwrap();
        shell.workspaces.add_output(
            output,
            &mut self.workspace_state.update(),
            &self.xdg_activation_state,
        );

        std::mem::drop(shell);
        self.refresh(); // fixes indicies of any moved workspaces
    }

    pub fn remove_output(&mut self, output: &Output) {
        let mut shell = self.shell.write().unwrap();
        let shell_ref = &mut *shell;
        shell_ref.workspaces.remove_output(
            output,
            shell_ref.seats.iter(),
            &mut self.workspace_state.update(),
            &self.xdg_activation_state,
        );

        std::mem::drop(shell);
        self.refresh(); // cleans up excess of workspaces and empty workspaces
    }

    pub fn migrate_workspace(&mut self, from: &Output, to: &Output, handle: &WorkspaceHandle) {
        if from == to {
            return;
        }

        let mut shell = self.shell.write().unwrap();
        shell.workspaces.migrate_workspace(
            from,
            to,
            handle,
            &mut self.workspace_state.update(),
            &self.xdg_activation_state,
        );

        std::mem::drop(shell);
        self.refresh(); // fixes index of moved workspace
    }

    pub fn update_config(&mut self) {
        let mut shell = self.shell.write().unwrap();
        let mut workspace_state = self.workspace_state.update();
        shell.workspaces.update_config(
            &self.config,
            &mut workspace_state,
            &self.xdg_activation_state,
        );
    }

    #[profiling::function]
    pub fn refresh(&mut self) {
        self.xdg_activation_state.retain_tokens(|_, data| {
            Instant::now().duration_since(data.timestamp) < Duration::from_secs(5)
        });
        self.shell.write().unwrap().refresh(
            &self.xdg_activation_state,
            &mut self.workspace_state.update(),
        );
        self.popups.cleanup();
        self.toplevel_info_state.refresh(&self.workspace_state);
        self.refresh_idle_inhibit();
    }

    pub fn refresh_idle_inhibit(&mut self) {
        self.idle_inhibiting_surfaces.retain(|s| s.alive());

        let is_inhibited = self.idle_inhibiting_surfaces.iter().any(|surface| {
            with_states(surface, |states| {
                surface_primary_scanout_output(surface, states).is_some()
            })
        });
        self.idle_notifier_state.set_is_inhibited(is_inhibited);
    }

    pub fn on_commit(&mut self, surface: &WlSurface) {
        if let Some(mapped) = self.shell.read().unwrap().element_for_surface(surface) {
            mapped.on_commit(surface);
        }
        self.popups.commit(surface);
    }
}

impl Shell {
    pub fn new(config: &Config) -> Self {
        let theme = cosmic::theme::system_preference();

        Shell {
            workspaces: Workspaces::new(config, theme.clone()),
            seats: Seats::new(),

            pending_windows: Vec::new(),
            pending_layers: Vec::new(),
            pending_activations: HashMap::new(),
            override_redirect_windows: Vec::new(),
            session_lock: None,

            theme,
            overview_mode: OverviewMode::None,
            swap_indicator: None,
            resize_mode: ResizeMode::None,
            resize_state: None,
            resize_indicator: None,
        }
    }

    pub fn activate(
        &mut self,
        output: &Output,
        idx: usize,
        workspace_delta: WorkspaceDelta,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
    ) -> Result<Option<Point<i32, Global>>, InvalidWorkspaceIndex> {
        match &mut self.workspaces.mode {
            WorkspaceMode::OutputBound => {
                if let Some(set) = self.workspaces.sets.get_mut(output) {
                    if matches!(
                        self.overview_mode,
                        OverviewMode::Started(Trigger::Pointer(_) | Trigger::Touch(_), _)
                    ) {
                        set.workspaces[set.active].tiling_layer.cleanup_drag();
                    }
                    set.activate(idx, workspace_delta, workspace_state)?;

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
                    set.activate(idx, workspace_delta, workspace_state)?;
                }
                Ok(None)
            }
        }
    }

    pub fn update_workspace_delta(&mut self, output: &Output, delta: f64) {
        match &mut self.workspaces.mode {
            WorkspaceMode::OutputBound => {
                if let Some(set) = self.workspaces.sets.get_mut(output) {
                    set.update_workspace_delta(delta);
                }
            }
            WorkspaceMode::Global => {
                for set in self.workspaces.sets.values_mut() {
                    set.update_workspace_delta(delta);
                }
            }
        }
    }

    pub fn end_workspace_swipe(
        &mut self,
        output: &Output,
        velocity: f64,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
    ) -> Result<Option<Point<i32, Global>>, InvalidWorkspaceIndex> {
        match &mut self.workspaces.mode {
            WorkspaceMode::OutputBound => {
                if let Some(set) = self.workspaces.sets.get_mut(output) {
                    if matches!(
                        self.overview_mode,
                        OverviewMode::Started(Trigger::Pointer(_) | Trigger::Touch(_), _)
                    ) {
                        set.workspaces[set.active].tiling_layer.cleanup_drag();
                    }
                    if let Some((_, workspace_delta)) = set.previously_active {
                        match workspace_delta {
                            WorkspaceDelta::Gesture(delta) => {
                                if (velocity > 0.0 && velocity.abs() >= GESTURE_VELOCITY_THRESHOLD)
                                    || (velocity.abs() < GESTURE_VELOCITY_THRESHOLD
                                        && delta.abs() > GESTURE_POSITION_THRESHOLD)
                                {
                                    set.activate(
                                        set.active,
                                        WorkspaceDelta::new_gesture_end(
                                            delta.abs(),
                                            velocity.abs(),
                                        ),
                                        workspace_state,
                                    )?;
                                } else {
                                    set.activate_previous(
                                        WorkspaceDelta::new_gesture_end(
                                            1.0 - delta.abs(),
                                            velocity.abs(),
                                        ),
                                        workspace_state,
                                    )?;
                                }
                            }
                            _ => {} // Do nothing
                        }
                    }

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
                    if let Some((_, workspace_delta)) = set.previously_active {
                        match workspace_delta {
                            WorkspaceDelta::Gesture(delta) => {
                                if (velocity > 0.0 && velocity.abs() >= GESTURE_VELOCITY_THRESHOLD)
                                    || (velocity.abs() < GESTURE_VELOCITY_THRESHOLD
                                        && delta.abs() > GESTURE_POSITION_THRESHOLD)
                                {
                                    set.activate(
                                        set.active,
                                        WorkspaceDelta::new_gesture_end(
                                            delta.abs(),
                                            velocity.abs(),
                                        ),
                                        workspace_state,
                                    )?;
                                } else {
                                    set.activate_previous(
                                        WorkspaceDelta::new_gesture_end(
                                            1.0 - delta.abs(),
                                            velocity.abs(),
                                        ),
                                        workspace_state,
                                    )?;
                                }
                            }
                            _ => {} // Do nothing
                        }
                    }
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

    pub fn refresh_active_space(
        &mut self,
        output: &Output,
        xdg_activation_state: &XdgActivationState,
    ) {
        self.workspaces
            .active_mut(output)
            .refresh(xdg_activation_state)
    }

    pub fn visible_output_for_surface(&self, surface: &WlSurface) -> Option<&Output> {
        if let Some(session_lock) = &self.session_lock {
            return session_lock
                .surfaces
                .iter()
                .find(|(_, v)| v.wl_surface() == surface)
                .map(|(k, _)| k);
        }

        self.outputs()
            // layer map surface?
            .find(|o| {
                let map = layer_map_for_output(o);
                map.layer_for_surface(surface, WindowSurfaceType::ALL)
                    .is_some()
            })
            // pending layer map surface?
            .or_else(|| {
                self.pending_layers.iter().find_map(|(l, output, _)| {
                    let mut found = false;
                    l.with_surfaces(|s, _| {
                        if s == surface {
                            found = true;
                        }
                    });
                    found.then_some(output)
                })
            })
            // override redirect window?
            .or_else(|| {
                self.outputs().find(|o| {
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
            })
            // sticky window ?
            .or_else(|| {
                self.outputs().find(|o| {
                    self.workspaces.sets[*o]
                        .sticky_layer
                        .mapped()
                        .any(|e| e.has_surface(surface, WindowSurfaceType::ALL))
                })
            })
            // normal window?
            .or_else(|| {
                self.outputs().find(|o| {
                    self.active_space(o)
                        .mapped()
                        .any(|e| e.has_surface(surface, WindowSurfaceType::ALL))
                })
            })
    }

    pub fn workspace_for_surface(&self, surface: &WlSurface) -> Option<(WorkspaceHandle, Output)> {
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
                        || w.minimized_windows
                            .iter()
                            .any(|m| m.window.has_surface(surface, WindowSurfaceType::ALL))
                })
                .map(|w| (w.handle.clone(), w.output().clone())),
        }
    }

    pub fn element_for_surface<S>(&self, surface: &S) -> Option<&CosmicMapped>
    where
        CosmicSurface: PartialEq<S>,
    {
        self.workspaces.sets.values().find_map(|set| {
            set.minimized_windows
                .iter()
                .map(|w| &w.window)
                .chain(set.sticky_layer.mapped())
                .find(|w| w.windows().any(|(s, _)| &s == surface))
                .or_else(|| {
                    set.workspaces
                        .iter()
                        .find_map(|w| w.element_for_surface(surface))
                })
        })
    }

    pub fn space_for(&self, mapped: &CosmicMapped) -> Option<&Workspace> {
        self.workspaces.spaces().find(|workspace| {
            workspace.mapped().any(|m| m == mapped)
                || workspace
                    .minimized_windows
                    .iter()
                    .any(|m| &m.window == mapped)
        })
    }

    pub fn space_for_mut(&mut self, mapped: &CosmicMapped) -> Option<&mut Workspace> {
        self.workspaces.spaces_mut().find(|workspace| {
            workspace.mapped().any(|m| m == mapped)
                || workspace
                    .minimized_windows
                    .iter()
                    .any(|m| &m.window == mapped)
        })
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

    pub fn next_output(&self, current_output: &Output, direction: Direction) -> Option<&Output> {
        let current_output_geo = current_output.geometry();
        self.outputs()
            .filter(|o| *o != current_output)
            .filter(|o| {
                let geo = o.geometry();
                match direction {
                    Direction::Left | Direction::Right => {
                        !(geo.loc.y + geo.size.h < current_output_geo.loc.y
                            || geo.loc.y > current_output_geo.loc.y + current_output_geo.size.h)
                    }
                    Direction::Up | Direction::Down => {
                        !(geo.loc.x + geo.size.w < current_output_geo.loc.x
                            || geo.loc.x > current_output_geo.loc.x + current_output_geo.size.w)
                    }
                }
            })
            .filter_map(|o| {
                let origin = o.geometry().loc;
                let res = match direction {
                    Direction::Up => current_output_geo.loc.y - origin.y,
                    Direction::Down => origin.y - current_output_geo.loc.y,
                    Direction::Left => current_output_geo.loc.x - origin.x,
                    Direction::Right => origin.x - current_output_geo.loc.x,
                };
                if res > 0 {
                    Some((o, res))
                } else {
                    None
                }
            })
            .min_by_key(|(_, res)| *res)
            .map(|(o, _)| o)
    }

    pub fn builtin_output(&self) -> Option<&Output> {
        self.outputs().find(|output| {
            let name = output.name();
            name.starts_with("eDP-") || name.starts_with("LVDS-") || name.starts_with("DSI-")
        })
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
        self.workspaces.sets.values().any(|set| {
            set.previously_active
                .as_ref()
                .is_some_and(|(_, delta)| delta.is_animating())
                || set.sticky_layer.animations_going()
        }) || !matches!(self.overview_mode, OverviewMode::None)
            || !matches!(self.resize_mode, ResizeMode::None)
            || self
                .workspaces
                .spaces()
                .any(|workspace| workspace.animations_going())
    }

    pub fn update_animations(&mut self) -> HashMap<ClientId, Client> {
        let mut clients = HashMap::new();
        for set in self.workspaces.sets.values_mut() {
            set.sticky_layer.update_animation_state();
        }
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
            if matches!(self.overview_mode, OverviewMode::Started(_, _)) {
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

    pub fn overview_mode(&self) -> (OverviewMode, Option<SwapIndicator>) {
        if let OverviewMode::Ended(_, timestamp) = self.overview_mode {
            if Instant::now().duration_since(timestamp) > ANIMATION_DURATION {
                return (OverviewMode::None, None);
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

    pub fn resize_mode(&self) -> (ResizeMode, Option<ResizeIndicator>) {
        if let ResizeMode::Ended(timestamp, _) = self.resize_mode {
            if Instant::now().duration_since(timestamp) > ANIMATION_DURATION {
                return (ResizeMode::None, None);
            }
        }

        (self.resize_mode.clone(), self.resize_indicator.clone())
    }

    pub fn stacking_indicator(
        &self,
        output: &Output,
        layer: ManagedLayer,
    ) -> Option<Rectangle<i32, Local>> {
        match layer {
            ManagedLayer::Sticky => self
                .workspaces
                .sets
                .get(output)
                .and_then(|set| set.sticky_layer.stacking_indicator()),
            ManagedLayer::Floating => self
                .active_space(output)
                .floating_layer
                .stacking_indicator(),
            ManagedLayer::Tiling => self.active_space(output).tiling_layer.stacking_indicator(),
        }
    }

    fn refresh(
        &mut self,
        xdg_activation_state: &XdgActivationState,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
    ) {
        if let OverviewMode::Ended(_, timestamp) = self.overview_mode {
            if Instant::now().duration_since(timestamp) > ANIMATION_DURATION {
                self.overview_mode = OverviewMode::None;
                self.swap_indicator = None;
            }
        }
        if let ResizeMode::Ended(timestamp, _) = self.resize_mode {
            if Instant::now().duration_since(timestamp) > ANIMATION_DURATION {
                self.resize_mode = ResizeMode::None;
                self.resize_indicator = None;
            }
        }

        self.workspaces
            .refresh(workspace_state, xdg_activation_state);

        for output in self.outputs() {
            let mut map = layer_map_for_output(output);
            map.cleanup();
        }

        self.override_redirect_windows.retain(|or| or.alive());
        self.override_redirect_windows
            .iter()
            .for_each(|or| or.refresh());

        self.pending_layers.retain(|(s, _, _)| s.alive());
        self.pending_windows.retain(|(s, _, _)| s.alive());
    }

    pub fn remap_unfullscreened_window(
        &mut self,
        mapped: CosmicMapped,
        current_workspace: &WorkspaceHandle,
        previous_workspace: &WorkspaceHandle,
        target_layer: ManagedLayer,
    ) {
        if self
            .workspaces
            .space_for_handle(previous_workspace)
            .is_none()
        {
            return;
        }

        {
            let Some(workspace) = self.workspaces.space_for_handle_mut(&current_workspace) else {
                return;
            };
            let _ = workspace.unmap(&mapped);
        }

        let new_workspace_output = self
            .workspaces
            .space_for_handle(&previous_workspace)
            .unwrap()
            .output()
            .clone();
        for (window, _) in mapped.windows() {
            toplevel_enter_output(&window, &new_workspace_output);
            toplevel_enter_workspace(&window, &previous_workspace);
        }

        let new_workspace = self
            .workspaces
            .space_for_handle_mut(&previous_workspace)
            .unwrap();
        match target_layer {
            ManagedLayer::Sticky => {
                let output = new_workspace.output().clone();
                self.workspaces
                    .sets
                    .get_mut(&output)
                    .unwrap()
                    .sticky_layer
                    .map(mapped, None)
            }
            ManagedLayer::Tiling if new_workspace.tiling_enabled => {
                new_workspace
                    .tiling_layer
                    .map(mapped, Option::<std::iter::Empty<_>>::None, None)
            }
            _ => new_workspace.floating_layer.map(mapped, None),
        };
    }

    #[must_use]
    pub fn map_window(
        &mut self,
        window: &CosmicSurface,
        toplevel_info: &mut ToplevelInfoState<State, CosmicSurface>,
        workspace_state: &mut WorkspaceState<State>,
        evlh: &LoopHandle<'static, State>,
    ) -> Option<KeyboardFocusTarget> {
        let pos = self
            .pending_windows
            .iter()
            .position(|(w, _, _)| w == window)
            .unwrap();
        let (window, seat, output) = self.pending_windows.remove(pos);

        let parent_is_sticky = if let Some(toplevel) = window.0.toplevel() {
            if let Some(parent) = toplevel.parent() {
                if let Some(elem) = self.element_for_surface(&parent) {
                    self.workspaces
                        .sets
                        .values()
                        .any(|set| set.sticky_layer.mapped().any(|m| m == elem))
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        };

        let pending_activation = self.pending_activations.remove(&(&window).into());
        let workspace_handle = match pending_activation {
            Some(ActivationContext::Workspace(handle)) => Some(handle),
            _ => None,
        };

        let should_be_fullscreen = output.is_some();
        let mut output = output.unwrap_or_else(|| seat.active_output());

        // this is beyond stupid, just to make the borrow checker happy
        let workspace = if let Some(handle) = workspace_handle.filter(|handle| {
            self.workspaces
                .spaces()
                .any(|space| &space.handle == handle)
        }) {
            self.workspaces
                .spaces_mut()
                .find(|space| space.handle == handle)
                .unwrap()
        } else {
            self.workspaces.active_mut(&output)
        };
        if output != workspace.output {
            output = workspace.output.clone();
        }

        if let Some((mapped, layer, previous_workspace)) = workspace.remove_fullscreen() {
            let old_handle = workspace.handle.clone();
            let new_workspace_handle = self
                .workspaces
                .space_for_handle(&previous_workspace)
                .is_some()
                .then_some(previous_workspace)
                .unwrap_or(old_handle);

            self.remap_unfullscreened_window(mapped, &old_handle, &new_workspace_handle, layer);
        };

        let active_handle = self.active_space(&output).handle;
        let workspace = if let Some(handle) = workspace_handle.filter(|handle| {
            self.workspaces
                .spaces()
                .any(|space| &space.handle == handle)
        }) {
            self.workspaces
                .spaces_mut()
                .find(|space| space.handle == handle)
                .unwrap()
        } else {
            self.workspaces.active_mut(&output)
        };

        toplevel_info.new_toplevel(&window, workspace_state);
        toplevel_enter_output(&window, &output);
        toplevel_enter_workspace(&window, &workspace.handle);

        let mut workspace_state = workspace_state.update();

        let workspace_output = workspace.output.clone();
        let was_activated = workspace_handle.is_some()
            && (workspace_output != seat.active_output() || active_handle != workspace.handle);
        let workspace_handle = workspace.handle;
        let is_dialog = layout::is_dialog(&window);
        let floating_exception = layout::has_floating_exception(&window);

        let maybe_focused = workspace.focus_stack.get(&seat).iter().next().cloned();
        if let Some(focused) = maybe_focused {
            if (focused.is_stack() && !is_dialog && !should_be_fullscreen)
                && !(workspace.is_tiled(&focused) && floating_exception)
            {
                focused.stack_ref().unwrap().add_window(window, None);
                if was_activated {
                    workspace_state.add_workspace_state(&workspace_handle, WState::Urgent);
                }
                return None;
            }
        }

        let mapped = CosmicMapped::from(CosmicWindow::new(
            window.clone(),
            evlh.clone(),
            self.theme.clone(),
        ));
        #[cfg(feature = "debug")]
        {
            mapped.set_debug(state.common.egui.active);
        }

        let workspace_empty = workspace.mapped().next().is_none();
        if is_dialog || floating_exception || !workspace.tiling_enabled {
            workspace.floating_layer.map(mapped.clone(), None);
        } else {
            for mapped in workspace
                .mapped()
                .filter(|m| m.maximized_state.lock().unwrap().is_some())
                .cloned()
                .collect::<Vec<_>>()
                .into_iter()
            {
                workspace.unmaximize_request(&mapped);
            }
            let focus_stack = workspace.focus_stack.get(&seat);
            workspace
                .tiling_layer
                .map(mapped.clone(), Some(focus_stack.iter()), None);
        }

        if !parent_is_sticky && should_be_fullscreen {
            let from = minimize_rectangle(&output, &mapped.active_window());

            workspace.fullscreen_request(&mapped.active_window(), None, from, &seat);
        }

        if parent_is_sticky {
            self.toggle_sticky(&seat, &mapped);
        }

        let new_target = if (workspace_output == seat.active_output()
            && active_handle == workspace_handle)
            || parent_is_sticky
        {
            // TODO: enforce focus stealing prevention by also checking the same rules as for the else case.
            Some(KeyboardFocusTarget::from(mapped.clone()))
        } else {
            if workspace_empty || was_activated || should_be_fullscreen {
                self.append_focus_stack(&mapped, &seat);
                workspace_state.add_workspace_state(&workspace_handle, WState::Urgent);
            }
            None
        };

        let active_space = self.active_space(&output);
        for mapped in active_space.mapped() {
            self.update_reactive_popups(mapped);
        }

        new_target
    }

    pub fn map_override_redirect(&mut self, window: X11Surface) {
        let geo = window.geometry();
        for (output, overlap) in self.outputs().cloned().filter_map(|o| {
            o.geometry()
                .as_logical()
                .intersection(geo)
                .map(|overlap| (o, overlap))
        }) {
            window.output_enter(&output, overlap);
        }

        self.override_redirect_windows.push(window);
    }

    #[must_use]
    pub fn map_layer(&mut self, layer_surface: &LayerSurface) -> Option<KeyboardFocusTarget> {
        let pos = self
            .pending_layers
            .iter()
            .position(|(l, _, _)| l == layer_surface)
            .unwrap();
        let (layer_surface, output, _seat) = self.pending_layers.remove(pos);

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
        for workspace in self.workspaces.spaces_mut() {
            workspace.tiling_layer.recalculate();
        }

        wants_focus.then(|| layer_surface.into())
    }

    pub fn unmap_surface<S>(
        &mut self,
        surface: &S,
        seat: &Seat<State>,
        toplevel_info: &mut ToplevelInfoState<State, CosmicSurface>,
    ) where
        CosmicSurface: PartialEq<S>,
    {
        for set in self.workspaces.sets.values_mut() {
            let sticky_res = set.sticky_layer.mapped().find_map(|m| {
                m.windows()
                    .position(|(s, _)| &s == surface)
                    .map(|idx| (idx, m.clone()))
            });
            let surface = if let Some((idx, mut mapped)) = sticky_res {
                if mapped.is_stack() {
                    mapped.stack_ref_mut().unwrap().remove_idx(idx)
                } else {
                    set.sticky_layer.unmap(&mapped);
                    Some(mapped.active_window())
                }
            } else if let Some(idx) = set
                .minimized_windows
                .iter()
                .map(|w| &w.window)
                .position(|w| w.windows().any(|(s, _)| &s == surface))
            {
                if set.minimized_windows.get(idx).unwrap().window.is_stack() {
                    let window = &mut set.minimized_windows.get_mut(idx).unwrap().window;
                    let stack = window.stack_ref_mut().unwrap();
                    let idx = stack.surfaces().position(|s| &s == surface);
                    idx.and_then(|idx| stack.remove_idx(idx))
                } else {
                    Some(set.minimized_windows.remove(idx).window.active_window())
                }
            } else if let Some((workspace, mut elem)) = set.workspaces.iter_mut().find_map(|w| {
                w.element_for_surface(&surface)
                    .cloned()
                    .map(|elem| (w, elem))
            }) {
                if elem.is_stack() {
                    let stack = elem.stack_ref_mut().unwrap();
                    let idx = stack.surfaces().position(|s| &s == surface);
                    idx.and_then(|idx| stack.remove_idx(idx))
                } else {
                    workspace.unmap(&elem);
                    Some(elem.active_window())
                }
            } else {
                None
            };

            if let Some(surface) = surface {
                toplevel_info.remove_toplevel(&surface);
                self.pending_windows.push((surface, seat.clone(), None));
                return;
            }
        }
    }

    pub fn element_under(
        &mut self,
        location: Point<f64, Global>,
        output: &Output,
    ) -> Option<KeyboardFocusTarget> {
        self.workspaces.sets.get_mut(output).and_then(|set| {
            set.sticky_layer
                .space
                .element_under(location.to_local(output).as_logical())
                .map(|(mapped, _)| mapped.clone().into())
                .or_else(|| set.workspaces[set.active].element_under(location))
        })
    }
    pub fn surface_under(
        &mut self,
        location: Point<f64, Global>,
        output: &Output,
    ) -> Option<(PointerFocusTarget, Point<i32, Global>)> {
        let overview = self.overview_mode.clone();
        self.workspaces.sets.get_mut(output).and_then(|set| {
            set.sticky_layer
                .surface_under(location.to_local(output))
                .map(|(target, offset)| (target, offset.to_global(output)))
                .or_else(|| set.workspaces[set.active].surface_under(location, overview))
        })
    }

    #[must_use]
    pub fn move_window(
        &mut self,
        seat: Option<&Seat<State>>,
        mapped: &CosmicMapped,
        from: &WorkspaceHandle,
        to: &WorkspaceHandle,
        follow: bool,
        direction: Option<Direction>,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
    ) -> Option<(KeyboardFocusTarget, Point<i32, Global>)> {
        let from_output = self.workspaces.space_for_handle(from)?.output.clone();
        let to_output = self.workspaces.space_for_handle(to)?.output.clone();

        let from_workspace = self.workspaces.space_for_handle_mut(from).unwrap(); // checked above
        let window_state = from_workspace.unmap(mapped)?;
        let elements = from_workspace.mapped().cloned().collect::<Vec<_>>();

        for (toplevel, _) in mapped.windows() {
            toplevel_leave_workspace(&toplevel, from);
            if from_output != to_output {
                toplevel_leave_output(&toplevel, &from_output);
            }
        }
        for mapped in elements.into_iter() {
            self.update_reactive_popups(&mapped);
        }
        let new_pos = if follow {
            if let Some(seat) = seat {
                seat.set_active_output(&to_output);
            }
            self.workspaces
                .idx_for_handle(&to_output, to)
                .and_then(|to_idx| {
                    self.activate(
                        &to_output,
                        to_idx,
                        WorkspaceDelta::new_shortcut(),
                        workspace_state,
                    )
                    .unwrap()
                })
        } else {
            None
        };

        let any_seat = seat.unwrap_or(self.seats.last_active()).clone();
        let mut to_workspace = self.workspaces.space_for_handle_mut(to).unwrap(); // checked above
        let focus_stack = seat.map(|seat| to_workspace.focus_stack.get(&seat));
        if window_state.layer == ManagedLayer::Floating || !to_workspace.tiling_enabled {
            to_workspace.floating_layer.map(mapped.clone(), None);
        } else {
            to_workspace.tiling_layer.map(
                mapped.clone(),
                focus_stack.as_ref().map(|x| x.iter()),
                direction,
            );
        }

        let focus_target = if let Some(f) = window_state.was_fullscreen {
            if to_workspace.fullscreen.is_some() {
                if let Some((mapped, layer, previous_workspace)) = to_workspace.remove_fullscreen()
                {
                    let old_handle = to.clone();
                    let new_workspace_handle = self
                        .workspaces
                        .space_for_handle(&previous_workspace)
                        .is_some()
                        .then_some(previous_workspace)
                        .unwrap_or(old_handle);

                    self.remap_unfullscreened_window(
                        mapped,
                        &old_handle,
                        &new_workspace_handle,
                        layer,
                    );
                    to_workspace = self.workspaces.space_for_handle_mut(to).unwrap();
                    // checked above
                }
            }

            let from = minimize_rectangle(&to_output, &mapped.active_window());

            to_workspace.fullscreen_request(&mapped.active_window(), f.previously, from, &any_seat);
            to_workspace
                .fullscreen
                .as_ref()
                .map(|f| KeyboardFocusTarget::from(f.surface.clone()))
                .unwrap_or_else(|| KeyboardFocusTarget::from(mapped.clone()))
        } else {
            KeyboardFocusTarget::from(mapped.clone())
        };

        for mapped in to_workspace
            .mapped()
            .cloned()
            .collect::<Vec<_>>()
            .into_iter()
        {
            self.update_reactive_popups(&mapped);
        }
        for (toplevel, _) in mapped.windows() {
            if from_output != to_output {
                toplevel_enter_output(&toplevel, &to_output);
            }
            toplevel_enter_workspace(&toplevel, to);
        }

        new_pos.map(|pos| (focus_target, pos))
    }

    #[must_use]
    pub fn move_current_window(
        &mut self,
        seat: &Seat<State>,
        from_output: &Output,
        to: (&Output, Option<usize>),
        follow: bool,
        direction: Option<Direction>,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
    ) -> Result<Option<(KeyboardFocusTarget, Point<i32, Global>)>, InvalidWorkspaceIndex> {
        let (to_output, to_idx) = to;
        let to_idx = to_idx.unwrap_or(self.workspaces.active_num(to_output).1);

        if from_output == to_output && to_idx == self.workspaces.active_num(from_output).1 {
            return Ok(None);
        }

        let to = self
            .workspaces
            .get(to_idx, to_output)
            .map(|ws| ws.handle)
            .ok_or(InvalidWorkspaceIndex)?;

        let from_workspace = self.workspaces.active_mut(from_output);
        let maybe_window = from_workspace.focus_stack.get(seat).last().cloned();

        let Some(mapped) = maybe_window else {
            return Ok(None);
        };

        let from = from_workspace.handle;

        Ok(self.move_window(
            Some(seat),
            &mapped,
            &from,
            &to,
            follow,
            direction,
            workspace_state,
        ))
    }

    pub fn update_reactive_popups(&self, mapped: &CosmicMapped) {
        if let Some(workspace) = self.space_for(mapped) {
            if let Some(element_loc) = workspace
                .element_geometry(mapped)
                .map(|geo| geo.loc.to_global(&workspace.output))
            {
                for (window, offset) in mapped.windows() {
                    if let Some(toplevel) = window.0.toplevel() {
                        let window_geo_offset = window.geometry().loc.as_global();
                        update_reactive_popups(
                            toplevel,
                            element_loc + offset.as_global() + window_geo_offset,
                            self.outputs(),
                        );
                    }
                }
            }
        }
    }

    pub fn menu_request(
        &self,
        surface: &WlSurface,
        seat: &Seat<State>,
        serial: impl Into<Option<Serial>>,
        location: Point<i32, Logical>,
        target_stack: bool,
        config: &Config,
        evlh: &LoopHandle<'static, State>,
    ) -> Option<(MenuGrab, Focus)> {
        let serial = serial.into();
        let Some(GrabStartData::Pointer(start_data)) =
            check_grab_preconditions(&seat, surface, serial, ReleaseMode::NoMouseButtons)
        else {
            return None;
        };

        let mapped = self.element_for_surface(surface).cloned()?;
        let (_, relative_loc) = mapped
            .windows()
            .find(|(w, _)| w.wl_surface().as_ref() == Some(surface))
            .unwrap();

        let (global_position, edge, is_tiled, is_stacked, is_sticky, tiling_enabled) =
            if let Some(set) = self
                .workspaces
                .sets
                .values()
                .find(|set| set.sticky_layer.mapped().any(|m| m == &mapped))
            {
                let output = set.output.clone();
                let global_position = (set.sticky_layer.element_geometry(&mapped).unwrap().loc
                    + relative_loc.as_local()
                    + location.as_local())
                .to_global(&output);
                (
                    global_position,
                    ResizeEdge::all(),
                    false,
                    mapped.is_stack(),
                    true,
                    false,
                )
            } else if let Some(workspace) = self.space_for(&mapped) {
                let output = seat.active_output();

                let elem_geo = workspace.element_geometry(&mapped)?;
                let global_position =
                    (elem_geo.loc + relative_loc.as_local() + location.as_local())
                        .to_global(&output);
                let is_tiled = workspace.is_tiled(&mapped);
                let edge = if is_tiled {
                    mapped
                        .tiling_node_id
                        .lock()
                        .unwrap()
                        .clone()
                        .map(|node_id| {
                            TilingLayout::possible_resizes(workspace.tiling_layer.tree(), node_id)
                        })
                        .unwrap_or(ResizeEdge::empty())
                } else {
                    ResizeEdge::all()
                };

                (
                    global_position,
                    edge,
                    is_tiled,
                    mapped.is_stack(),
                    false,
                    workspace.tiling_enabled,
                )
            } else {
                return None;
            };

        let grab = MenuGrab::new(
            start_data,
            seat,
            if target_stack || !is_stacked {
                Box::new(window_items(
                    &mapped,
                    is_tiled,
                    is_stacked,
                    is_sticky,
                    tiling_enabled,
                    edge,
                    &config.static_conf,
                )) as Box<dyn Iterator<Item = Item>>
            } else {
                let (tab, _) = mapped
                    .windows()
                    .find(|(s, _)| s.wl_surface().as_ref() == Some(surface))
                    .unwrap();
                Box::new(tab_items(&mapped, &tab, is_tiled, &config.static_conf))
                    as Box<dyn Iterator<Item = Item>>
            },
            global_position,
            evlh.clone(),
            self.theme.clone(),
        );

        Some((grab, Focus::Keep))
    }

    pub fn move_request(
        &mut self,
        surface: &WlSurface,
        seat: &Seat<State>,
        serial: impl Into<Option<Serial>>,
        release: ReleaseMode,
        move_out_of_stack: bool,
        config: &Config,
        evlh: &LoopHandle<'static, State>,
        xdg_activation_state: &XdgActivationState,
    ) -> Option<(MoveGrab, Focus)> {
        let serial = serial.into();

        let mut start_data = check_grab_preconditions(&seat, surface, serial, release)?;
        let mut old_mapped = self.element_for_surface(surface).cloned()?;
        if old_mapped.is_minimized() {
            return None;
        }

        for workspace in self.workspaces.spaces_mut() {
            for seat in self.seats.iter() {
                let mut stack = workspace.focus_stack.get_mut(seat);
                stack.remove(&old_mapped);
            }
        }

        let (window, _) = old_mapped
            .windows()
            .find(|(w, _)| w.wl_surface().as_ref() == Some(surface))
            .unwrap();

        let mapped = if move_out_of_stack {
            let new_mapped: CosmicMapped =
                CosmicWindow::new(window.clone(), evlh.clone(), self.theme.clone()).into();
            start_data.set_focus(new_mapped.focus_under((0., 0.).into()));
            new_mapped
        } else {
            old_mapped.clone()
        };

        let trigger = match &start_data {
            GrabStartData::Pointer(start_data) => Trigger::Pointer(start_data.button),
            GrabStartData::Touch(start_data) => Trigger::Touch(start_data.slot),
        };
        let active_hint = if config.cosmic_conf.active_hint {
            self.theme.cosmic().active_hint as u8
        } else {
            0
        };
        let pointer = seat.get_pointer().unwrap();
        let pos = pointer.current_location().as_global();

        let cursor_output = if let Some(output) =
            self.outputs()
                .find(|output| {
                    output.geometry().as_logical().overlaps_or_touches(
                        Rectangle::from_loc_and_size(start_data.location().to_i32_floor(), (0, 0)),
                    )
                })
                .cloned()
        {
            output
        } else {
            seat.active_output()
        };

        let (initial_window_location, layer, workspace_handle) = if let Some(workspace) =
            self.space_for_mut(&old_mapped)
        {
            if workspace
                .fullscreen
                .as_ref()
                .is_some_and(|f| f.surface == window)
            {
                let _ = workspace.remove_fullscreen(); // We are moving this window, we don't need to send it back to it's original workspace
            }

            let elem_geo = workspace.element_geometry(&old_mapped)?;
            let mut initial_window_location = elem_geo.loc.to_global(workspace.output());

            if mapped.maximized_state.lock().unwrap().is_some() {
                // If surface is maximized then unmaximize it
                let new_size = workspace.unmaximize_request(&mapped);
                let output = workspace.output();
                let ratio = pos.to_local(&output).x / output.geometry().size.w as f64;

                initial_window_location = new_size
                    .map(|size| (pos.x - (size.w as f64 * ratio), pos.y).into())
                    .unwrap_or_else(|| pos)
                    .to_i32_round();
            }

            let layer = if mapped == old_mapped {
                let was_floating = workspace.floating_layer.unmap(&mapped);
                let was_tiled = workspace.tiling_layer.unmap_as_placeholder(&mapped);
                assert!(was_floating != was_tiled.is_some());
                was_tiled.is_some()
            } else {
                workspace
                    .tiling_layer
                    .mapped()
                    .any(|(m, _)| m == &old_mapped)
            }
            .then_some(ManagedLayer::Tiling)
            .unwrap_or(ManagedLayer::Floating);

            (initial_window_location, layer, workspace.handle)
        } else if let Some(sticky_layer) = self
            .workspaces
            .sets
            .get_mut(&cursor_output)
            .filter(|set| set.sticky_layer.mapped().any(|m| m == &old_mapped))
            .map(|set| &mut set.sticky_layer)
        {
            let mut initial_window_location = sticky_layer
                .element_geometry(&old_mapped)
                .unwrap()
                .loc
                .to_global(&cursor_output);

            if let Some(state) = mapped.maximized_state.lock().unwrap().take() {
                // If surface is maximized then unmaximize it
                mapped.set_maximized(false);
                let new_size = state.original_geometry.size.as_logical();
                sticky_layer.map_internal(
                    mapped.clone(),
                    Some(state.original_geometry.loc),
                    Some(new_size),
                    None,
                );

                let ratio = pos.to_local(&cursor_output).x / cursor_output.geometry().size.w as f64;
                initial_window_location =
                    Point::<f64, _>::from((pos.x - (new_size.w as f64 * ratio), pos.y))
                        .to_i32_round();
            }

            if mapped == old_mapped {
                sticky_layer.unmap(&mapped);
            }

            (
                initial_window_location,
                ManagedLayer::Sticky,
                self.active_space(&cursor_output).handle,
            )
        } else {
            return None;
        };

        toplevel_leave_workspace(&window, &workspace_handle);
        toplevel_leave_output(&window, &cursor_output);

        if move_out_of_stack {
            old_mapped.stack_ref_mut().unwrap().remove_window(&window);
            self.workspaces
                .space_for_handle_mut(&workspace_handle)
                .unwrap()
                .refresh(xdg_activation_state);
        }

        let grab = MoveGrab::new(
            start_data,
            mapped,
            seat,
            initial_window_location,
            cursor_output,
            active_hint as u8,
            layer,
            release,
            evlh.clone(),
        );

        if grab.is_tiling_grab() {
            self.set_overview_mode(Some(trigger), evlh.clone());
        }

        Some((grab, Focus::Clear))
    }

    #[must_use]
    pub fn next_focus<'a>(&self, direction: FocusDirection, seat: &Seat<State>) -> FocusResult {
        let overview = self.overview_mode().0;
        let output = seat.active_output();
        let workspace = self.active_space(&output);

        if workspace.fullscreen.is_some() {
            return FocusResult::None;
        }

        let Some(target) = seat.get_keyboard().unwrap().current_focus() else {
            return FocusResult::None;
        };

        let set = self.workspaces.sets.get(&output).unwrap();
        let sticky_layer = &set.sticky_layer;
        let workspace = &set.workspaces[set.active];

        let Some(focused) = (match target {
            KeyboardFocusTarget::Popup(popup) => {
                let Some(toplevel_surface) = (match popup {
                    PopupKind::Xdg(xdg) => get_popup_toplevel(&xdg),
                    PopupKind::InputMethod(_) => unreachable!(),
                }) else {
                    return FocusResult::None;
                };
                sticky_layer
                    .space
                    .elements()
                    .chain(workspace.mapped())
                    .find(|elem| elem.wl_surface().as_ref() == Some(&toplevel_surface))
            }
            KeyboardFocusTarget::Element(elem) => sticky_layer
                .space
                .elements()
                .chain(workspace.mapped())
                .find(|e| *e == &elem),
            _ => None,
        })
        .cloned() else {
            return FocusResult::None;
        };

        if focused.handle_focus(direction, None) {
            return FocusResult::Handled;
        }

        if workspace.is_tiled(&focused) {
            let focus_stack = workspace.focus_stack.get(seat);
            let swap_desc = match overview {
                OverviewMode::Started(Trigger::KeyboardSwap(_, desc), _) => Some(desc),
                _ => None,
            };

            workspace
                .tiling_layer
                .next_focus(direction, seat, focus_stack.iter(), swap_desc)
        } else {
            let floating_layer = &set.workspaces[set.active].floating_layer;

            let geometry = sticky_layer
                .space
                .element_geometry(&focused)
                .or_else(|| floating_layer.space.element_geometry(&focused))
                .unwrap();

            let elements = sticky_layer
                .space
                .elements()
                .chain(floating_layer.space.elements())
                .filter(|elem| *elem != &focused)
                .map(|elem| {
                    (
                        elem,
                        sticky_layer
                            .space
                            .element_geometry(elem)
                            .or_else(|| floating_layer.space.element_geometry(elem))
                            .unwrap(),
                    )
                });

            let next = match direction {
                FocusDirection::Up => elements
                    .filter(|(_, other_geo)| other_geo.loc.y <= geometry.loc.y)
                    .min_by_key(|(_, other_geo)| {
                        let res = geometry.loc.y - other_geo.loc.y;
                        if res.is_positive() {
                            res
                        } else {
                            i32::MAX
                        }
                    }),
                FocusDirection::Down => elements
                    .filter(|(_, other_geo)| other_geo.loc.y > geometry.loc.y)
                    .max_by_key(|(_, other_geo)| {
                        let res = geometry.loc.y - other_geo.loc.y;
                        if res.is_negative() {
                            res
                        } else {
                            i32::MIN
                        }
                    }),
                FocusDirection::Left => elements
                    .filter(|(_, other_geo)| other_geo.loc.x <= geometry.loc.x)
                    .min_by_key(|(_, other_geo)| {
                        let res = geometry.loc.x - other_geo.loc.x;
                        if res.is_positive() {
                            res
                        } else {
                            i32::MAX
                        }
                    }),
                FocusDirection::Right => elements
                    .filter(|(_, other_geo)| other_geo.loc.x > geometry.loc.x)
                    .max_by_key(|(_, other_geo)| {
                        let res = geometry.loc.x - other_geo.loc.x;
                        if res.is_negative() {
                            res
                        } else {
                            i32::MIN
                        }
                    }),
                _ => return FocusResult::None,
            }
            .map(|(other, _)| other);

            next.map(|elem| FocusResult::Some(KeyboardFocusTarget::Element(elem.clone())))
                .unwrap_or(FocusResult::None)
        }
    }

    #[must_use]
    pub fn move_current_element<'a>(
        &mut self,
        direction: Direction,
        seat: &Seat<State>,
    ) -> MoveResult {
        let output = seat.active_output();
        let workspace = self.active_space(&output);
        let focus_stack = workspace.focus_stack.get(seat);
        let Some(last) = focus_stack.last().cloned() else {
            return MoveResult::None;
        };
        let fullscreen = workspace.fullscreen.as_ref().map(|f| f.surface.clone());

        if last
            .maximized_state
            .lock()
            .unwrap()
            .as_ref()
            .is_some_and(|state| state.original_layer == ManagedLayer::Tiling)
        {
            self.unmaximize_request(&last);
        }

        if let Some(surface) = fullscreen {
            MoveResult::MoveFurther(KeyboardFocusTarget::Fullscreen(surface))
        } else if let Some(set) = self
            .workspaces
            .sets
            .values_mut()
            .find(|set| set.sticky_layer.mapped().any(|m| m == &last))
        {
            set.sticky_layer.move_current_element(
                direction,
                seat,
                ManagedLayer::Sticky,
                self.theme.clone(),
            )
        } else {
            let theme = self.theme.clone();
            let workspace = self.active_space_mut(&output);
            workspace
                .floating_layer
                .move_current_element(direction, seat, ManagedLayer::Floating, theme)
                .or_else(|| workspace.tiling_layer.move_current_node(direction, seat))
        }
    }

    pub fn menu_resize_request(
        &mut self,
        mapped: &CosmicMapped,
        seat: &Seat<State>,
        edge: ResizeEdge,
    ) -> Option<(
        (
            Option<(PointerFocusTarget, Point<i32, Logical>)>,
            Point<i32, Global>,
        ),
        (ResizeGrab, Focus),
    )> {
        let surface = mapped.active_window().wl_surface()?;
        if mapped.is_fullscreen(true) || mapped.is_maximized(true) {
            return None;
        }

        let mut start_data = check_grab_preconditions(&seat, &surface, None, ReleaseMode::Click)?;

        let (floating_layer, geometry) = if let Some(set) = self
            .workspaces
            .sets
            .values_mut()
            .find(|set| set.sticky_layer.mapped().any(|m| m == mapped))
        {
            let geometry = set
                .sticky_layer
                .element_geometry(mapped)
                .unwrap()
                .to_global(&set.output);
            (&mut set.sticky_layer, geometry)
        } else if let Some(workspace) = self.space_for_mut(&mapped) {
            let geometry = workspace
                .element_geometry(&mapped)
                .unwrap()
                .to_global(workspace.output());
            (&mut workspace.floating_layer, geometry)
        } else {
            return None;
        };

        let new_loc = if edge.contains(ResizeEdge::LEFT) {
            Point::<i32, Global>::from((geometry.loc.x, geometry.loc.y + (geometry.size.h / 2)))
        } else if edge.contains(ResizeEdge::RIGHT) {
            Point::<i32, Global>::from((
                geometry.loc.x + geometry.size.w,
                geometry.loc.y + (geometry.size.h / 2),
            ))
        } else if edge.contains(ResizeEdge::TOP) {
            Point::<i32, Global>::from((geometry.loc.x + (geometry.size.w / 2), geometry.loc.y))
        } else if edge.contains(ResizeEdge::BOTTOM) {
            Point::<i32, Global>::from((
                geometry.loc.x + (geometry.size.w / 2),
                geometry.loc.y + geometry.size.h,
            ))
        } else {
            return None;
        };

        let element_offset = (new_loc - geometry.loc).as_logical();
        let focus = mapped
            .focus_under(element_offset.to_f64())
            .map(|(target, surface_offset)| (target, (surface_offset + element_offset)));
        start_data.set_location(new_loc.as_logical().to_f64());
        start_data.set_focus(focus.clone());

        let grab: ResizeGrab = if let Some(grab) = floating_layer.resize_request(
            mapped,
            seat,
            start_data.clone(),
            edge,
            ReleaseMode::Click,
        ) {
            grab.into()
        } else if let Some(ws) = self.space_for_mut(&mapped) {
            let node_id = mapped.tiling_node_id.lock().unwrap().clone()?;
            let (node, left_up_idx, orientation) = ws.tiling_layer.resize_request(node_id, edge)?;
            ResizeForkGrab::new(
                start_data,
                new_loc.to_f64(),
                node,
                left_up_idx,
                orientation,
                ws.output.downgrade(),
                ReleaseMode::Click,
            )
            .into()
        } else {
            return None;
        };

        Some(((focus, new_loc), (grab, Focus::Keep)))
    }

    pub fn maximize_toggle(&mut self, window: &CosmicMapped, seat: &Seat<State>) {
        if window.is_maximized(true) {
            self.unmaximize_request(window);
        } else {
            self.maximize_request(window, seat);
        }
    }

    pub fn minimize_request(&mut self, mapped: &CosmicMapped) {
        if let Some(set) = self
            .workspaces
            .sets
            .values_mut()
            .find(|set| set.sticky_layer.mapped().any(|m| m == mapped))
        {
            let to = minimize_rectangle(&set.output, &mapped.active_window());
            let (window, position) = set.sticky_layer.unmap_minimize(mapped, to).unwrap();
            set.minimized_windows.push(MinimizedWindow {
                window,
                previous_state: MinimizedState::Sticky { position },
                output_geo: set.output.geometry(),
                fullscreen: None,
            });
        } else if let Some(workspace) = self.workspaces.sets.values_mut().find_map(|set| {
            set.workspaces
                .iter_mut()
                .find(|workspace| workspace.mapped().any(|m| m == mapped))
        }) {
            let to = minimize_rectangle(workspace.output(), &mapped.active_window());
            if let Some(minimized) = workspace.minimize(&mapped, to) {
                workspace.minimized_windows.push(minimized);
            }
        }
    }

    pub fn unminimize_request(&mut self, mapped: &CosmicMapped, seat: &Seat<State>) {
        if let Some((set, window)) = self.workspaces.sets.values_mut().find_map(|set| {
            set.minimized_windows
                .iter()
                .position(|m| &m.window == mapped)
                .map(|i| set.minimized_windows.swap_remove(i))
                .map(|window| (set, window))
        }) {
            let from = minimize_rectangle(&set.output, &mapped.active_window());

            if let MinimizedState::Sticky { mut position } = window.previous_state {
                let current_output_size = set.output.geometry().size.as_logical();
                if current_output_size != window.output_geo.size.as_logical() {
                    position = Point::from((
                        (position.x as f64 / window.output_geo.size.w as f64
                            * current_output_size.w as f64)
                            .floor() as i32,
                        (position.y as f64 / window.output_geo.size.h as f64
                            * current_output_size.h as f64)
                            .floor() as i32,
                    ))
                };
                set.sticky_layer
                    .remap_minimized(window.window, from, position);
            } else {
                unreachable!("None sticky window in WorkspaceSet minimized_windows");
            }
        } else if let Some((workspace, window)) = self.workspaces.spaces_mut().find_map(|w| {
            w.minimized_windows
                .iter()
                .position(|m| &m.window == mapped)
                .map(|i| w.minimized_windows.swap_remove(i))
                .map(|window| (w, window))
        }) {
            let from = minimize_rectangle(workspace.output(), &mapped.active_window());

            workspace.unminimize(window, from, seat);
        }
    }

    pub fn maximize_request(&mut self, mapped: &CosmicMapped, seat: &Seat<State>) {
        self.unminimize_request(mapped, seat);
        let (original_layer, floating_layer, original_geometry) = if let Some(set) = self
            .workspaces
            .sets
            .values_mut()
            .find(|set| set.sticky_layer.mapped().any(|m| m == mapped))
        {
            let geometry = set.sticky_layer.element_geometry(mapped).unwrap();
            (ManagedLayer::Sticky, &mut set.sticky_layer, geometry)
        } else if let Some(workspace) = self.space_for_mut(&mapped) {
            let layer = if workspace.is_floating(&mapped) {
                ManagedLayer::Floating
            } else {
                ManagedLayer::Tiling
            };
            let geometry = workspace.element_geometry(mapped).unwrap();
            (layer, &mut workspace.floating_layer, geometry)
        } else {
            return;
        };

        let mut state = mapped.maximized_state.lock().unwrap();
        if state.is_none() {
            *state = Some(MaximizedState {
                original_geometry,
                original_layer,
            });
            std::mem::drop(state);
            floating_layer.map_maximized(mapped.clone(), original_geometry, true);
        }
    }

    pub fn unmaximize_request(&mut self, mapped: &CosmicMapped) -> Option<Size<i32, Logical>> {
        if let Some(set) = self.workspaces.sets.values_mut().find(|set| {
            set.sticky_layer.mapped().any(|m| m == mapped)
                || set.minimized_windows.iter().any(|m| &m.window == mapped)
        }) {
            let mut state = mapped.maximized_state.lock().unwrap();
            if let Some(state) = state.take() {
                assert_eq!(state.original_layer, ManagedLayer::Sticky);

                if let Some(minimized) = set
                    .minimized_windows
                    .iter_mut()
                    .find(|m| &m.window == mapped)
                {
                    minimized.unmaximize(state.original_geometry);
                } else {
                    mapped.set_maximized(false);
                    set.sticky_layer.map_internal(
                        mapped.clone(),
                        Some(state.original_geometry.loc),
                        Some(state.original_geometry.size.as_logical()),
                        None,
                    );
                }
                Some(state.original_geometry.size.as_logical())
            } else {
                None
            }
        } else if let Some(workspace) = self.space_for_mut(mapped) {
            workspace.unmaximize_request(mapped)
        } else {
            None
        }
    }

    pub fn resize_request(
        &mut self,
        surface: &WlSurface,
        seat: &Seat<State>,
        serial: impl Into<Option<Serial>>,
        edges: ResizeEdge,
    ) -> Option<(ResizeGrab, Focus)> {
        let serial = serial.into();
        let start_data =
            check_grab_preconditions(&seat, surface, serial, ReleaseMode::NoMouseButtons)?;
        let mapped = self.element_for_surface(surface).cloned()?;
        if mapped.is_fullscreen(true) || mapped.is_maximized(true) {
            return None;
        }

        let floating_layer = if let Some(set) = self
            .workspaces
            .sets
            .values_mut()
            .find(|set| set.sticky_layer.mapped().any(|m| m == &mapped))
        {
            &mut set.sticky_layer
        } else if let Some(workspace) = self.space_for_mut(&mapped) {
            &mut workspace.floating_layer
        } else {
            return None;
        };

        let grab: ResizeGrab = if let Some(grab) = floating_layer.resize_request(
            &mapped,
            seat,
            start_data.clone(),
            edges,
            ReleaseMode::NoMouseButtons,
        ) {
            grab.into()
        } else if let Some(ws) = self.space_for_mut(&mapped) {
            let node_id = mapped.tiling_node_id.lock().unwrap().clone()?;
            let (node, left_up_idx, orientation) =
                ws.tiling_layer.resize_request(node_id, edges)?;
            ResizeForkGrab::new(
                start_data,
                seat.get_pointer().unwrap().current_location().as_global(),
                node,
                left_up_idx,
                orientation,
                ws.output.downgrade(),
                ReleaseMode::NoMouseButtons,
            )
            .into()
        } else {
            return None;
        };

        Some((grab, Focus::Clear))
    }

    pub fn resize(&mut self, seat: &Seat<State>, direction: ResizeDirection, edge: ResizeEdge) {
        let output = seat.active_output();
        let (_, idx) = self.workspaces.active_num(&output);
        let Some(focused) = seat.get_keyboard().unwrap().current_focus() else {
            return;
        };
        let amount = (self
            .resize_state
            .take()
            .map(|(_, _, _, amount, _, _)| amount)
            .unwrap_or(10)
            + 2)
        .min(20);

        if self
            .workspaces
            .sets
            .get_mut(&output)
            .unwrap()
            .sticky_layer
            .resize(&focused, direction, edge, amount)
        {
            self.resize_state = Some((focused, direction, edge, amount, idx, output));
        } else if let Some(workspace) = self.workspaces.get_mut(idx, &output) {
            if workspace.resize(&focused, direction, edge, amount) {
                self.resize_state = Some((focused, direction, edge, amount, idx, output));
            }
        }
    }

    pub fn finish_resize(&mut self, direction: ResizeDirection, edge: ResizeEdge) {
        if let Some((old_focused, old_direction, old_edge, _, idx, output)) =
            self.resize_state.take()
        {
            if old_direction == direction && old_edge == edge {
                let Some(toplevel) = old_focused.toplevel() else {
                    return;
                };
                let Some(mapped) = self
                    .workspaces
                    .sets
                    .values()
                    .find_map(|set| {
                        set.sticky_layer
                            .mapped()
                            .find(|m| m.has_surface(&toplevel, WindowSurfaceType::TOPLEVEL))
                    })
                    .cloned()
                    .or_else(|| {
                        let workspace = self.workspaces.get(idx, &output).unwrap();
                        workspace
                            .mapped()
                            .find(|m| m.has_surface(&toplevel, WindowSurfaceType::TOPLEVEL))
                            .cloned()
                    })
                else {
                    return;
                };

                let mut resize_state = mapped.resize_state.lock().unwrap();
                if let Some(ResizeState::Resizing(data)) = *resize_state {
                    mapped.set_resizing(false);
                    *resize_state = Some(ResizeState::WaitingForCommit(data));
                }
            }
        }
    }

    #[must_use]
    pub fn toggle_stacking(&mut self, window: &CosmicMapped) -> Option<KeyboardFocusTarget> {
        if let Some(set) = self
            .workspaces
            .sets
            .values_mut()
            .find(|set| set.sticky_layer.mapped().any(|m| m == window))
        {
            set.sticky_layer.toggle_stacking(window)
        } else if let Some(workspace) = self.space_for_mut(window) {
            if workspace.tiling_layer.mapped().any(|(m, _)| m == window) {
                workspace.tiling_layer.toggle_stacking(window)
            } else if workspace.floating_layer.mapped().any(|w| w == window) {
                workspace.floating_layer.toggle_stacking(window)
            } else {
                None
            }
        } else {
            None
        }
    }

    #[must_use]
    pub fn toggle_stacking_focused(&mut self, seat: &Seat<State>) -> Option<KeyboardFocusTarget> {
        let set = self.workspaces.sets.get_mut(&seat.active_output()).unwrap();
        let workspace = &mut set.workspaces[set.active];
        let maybe_window = workspace.focus_stack.get(seat).iter().next().cloned();
        if let Some(window) = maybe_window {
            if set.sticky_layer.mapped().any(|m| m == &window) {
                set.sticky_layer
                    .toggle_stacking_focused(seat, workspace.focus_stack.get_mut(seat))
            } else if workspace.tiling_layer.mapped().any(|(m, _)| m == &window) {
                workspace
                    .tiling_layer
                    .toggle_stacking_focused(seat, workspace.focus_stack.get_mut(seat))
            } else if workspace.floating_layer.mapped().any(|w| w == &window) {
                workspace
                    .floating_layer
                    .toggle_stacking_focused(seat, workspace.focus_stack.get_mut(seat))
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn toggle_sticky<'a>(&mut self, seat: &Seat<State>, mapped: &CosmicMapped) {
        // clean from focus-stacks
        for workspace in self.workspaces.spaces_mut() {
            for seat in self.seats.iter() {
                let mut stack = workspace.focus_stack.get_mut(seat);
                stack.remove(mapped);
            }
        }

        if let Some(workspace) = self.space_for_mut(mapped) {
            if workspace.is_fullscreen(mapped) {
                // we are making it sticky, we don't need to move it to it's previous workspace
                let _ = workspace.remove_fullscreen();
            }
            let previous_layer = if workspace.is_tiled(mapped) {
                workspace.toggle_floating_window(seat, mapped);
                ManagedLayer::Tiling
            } else {
                ManagedLayer::Floating
            };
            let Some(geometry) = workspace.element_geometry(mapped) else {
                return;
            };
            workspace.unmap(mapped);

            *mapped.previous_layer.lock().unwrap() = Some(previous_layer);
            let output = workspace.output().clone();
            let handle = workspace.handle;

            for (window, _) in mapped.windows() {
                toplevel_leave_workspace(&window, &handle);
            }

            self.workspaces
                .sets
                .get_mut(&output)
                .unwrap()
                .sticky_layer
                .map(mapped.clone(), geometry.loc);
        } else if let Some(set) = self
            .workspaces
            .sets
            .values_mut()
            .find(|set| set.sticky_layer.mapped().any(|m| m == mapped))
        {
            let geometry = set.sticky_layer.element_geometry(mapped).unwrap();
            set.sticky_layer.unmap(mapped);

            let workspace = &mut set.workspaces[set.active];
            for (window, _) in mapped.windows() {
                toplevel_enter_workspace(&window, &workspace.handle);
            }

            match mapped
                .previous_layer
                .lock()
                .unwrap()
                .take()
                .unwrap_or(ManagedLayer::Floating)
            {
                ManagedLayer::Tiling if workspace.tiling_enabled => {
                    let focus_stack = workspace.focus_stack.get(seat);
                    workspace
                        .tiling_layer
                        .map(mapped.clone(), Some(focus_stack.iter()), None);
                }
                ManagedLayer::Sticky => unreachable!(),
                _ => workspace.floating_layer.map(mapped.clone(), geometry.loc),
            }
        }

        self.append_focus_stack(&mapped, seat);
    }

    pub fn toggle_sticky_current<'a>(&mut self, seat: &Seat<State>) {
        let set = self.workspaces.sets.get_mut(&seat.active_output()).unwrap();
        let workspace = &mut set.workspaces[set.active];
        let maybe_window = workspace.focus_stack.get(seat).iter().next().cloned();
        if let Some(mapped) = maybe_window {
            self.toggle_sticky(seat, &mapped);
        }
    }

    pub fn set_theme(
        &mut self,
        theme: cosmic::Theme,
        xdg_activation_state: &XdgActivationState,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
    ) {
        self.theme = theme.clone();
        self.refresh(xdg_activation_state, workspace_state);
        self.workspaces
            .set_theme(theme.clone(), xdg_activation_state);
    }
}

fn workspace_set_idx<'a>(
    state: &mut WorkspaceUpdateGuard<'a, State>,
    idx: u8,
    output_pos: usize,
    handle: &WorkspaceHandle,
) {
    state.set_workspace_name(handle, format!("{}", idx));
    state.set_workspace_coordinates(handle, [Some(idx as u32), Some(output_pos as u32), None]);
}

pub fn check_grab_preconditions(
    seat: &Seat<State>,
    surface: &WlSurface,
    serial: Option<Serial>,
    release: ReleaseMode,
) -> Option<GrabStartData> {
    use smithay::reexports::wayland_server::Resource;

    let pointer = seat.get_pointer().unwrap();
    let touch = seat.get_touch().unwrap();

    let start_data =
        if serial.map_or(false, |serial| touch.has_grab(serial)) {
            GrabStartData::Touch(touch.grab_start_data().unwrap())
        } else {
            GrabStartData::Pointer(pointer.grab_start_data().unwrap_or_else(|| {
                PointerGrabStartData {
                    focus: pointer.current_focus().map(|f| (f, Point::from((0, 0)))),
                    button: 0x110,
                    location: pointer.current_location(),
                }
            }))
        };

    if release == ReleaseMode::NoMouseButtons {
        // Check that this surface has a click or touch down grab.
        if !match serial {
            Some(serial) => pointer.has_grab(serial) || touch.has_grab(serial),
            None => pointer.is_grabbed() | touch.is_grabbed(),
        } {
            return None;
        }

        // If the focus was for a different surface, ignore the request.
        if start_data.focus().is_none()
            || !start_data.focus().unwrap().0.same_client_as(&surface.id())
        {
            return None;
        }
    }

    Some(start_data)
}
