use std::{
    cell::Cell,
    mem::MaybeUninit,
};

use smithay::{
    desktop::{
        LayerSurface,
        PopupManager,
        Window,
        WindowSurfaceType,
        layer_map_for_output,
    },
    reexports::wayland_server::{
        DisplayHandle,
        protocol::wl_surface::WlSurface,
    },
    wayland::{
        compositor::with_states,
        seat::{Seat, MotionEvent},
        shell::{
            wlr_layer::{WlrLayerShellState, Layer, LayerSurfaceCachedState, KeyboardInteractivity},
            xdg::XdgShellState,
        },
        output::Output,
        SERIAL_COUNTER,
    },
    utils::{Point, Rectangle, Logical},
};

use cosmic_protocols::workspace::v1::server::zcosmic_workspace_handle_v1::State as WState;

use crate::{
    config::{Config, WorkspaceMode as ConfigMode},
    wayland::protocols::{
        toplevel_info::ToplevelInfoState,
        workspace::{
            WorkspaceState,
            WorkspaceGroupHandle,
            WorkspaceHandle,
            WorkspaceCapabilities,
            WorkspaceUpdateGuard,
        },
    },
    utils::prelude::*,
};

pub const MAX_WORKSPACES: usize = 10;
pub mod layout;
mod workspace;
pub mod focus;
pub use self::workspace::*;

pub struct Shell {
    pub popups: PopupManager,
    pub spaces: [Workspace; MAX_WORKSPACES],
    pub outputs: Vec<Output>,
    pub workspace_mode: WorkspaceMode,
    pub shell_mode: ShellMode,

    pub pending_windows: Vec<(Window, Seat<State>)>,
    pub pending_layers: Vec<(LayerSurface, Output, Seat<State>)>,

    // wayland_state
    pub layer_shell_state: WlrLayerShellState,
    pub toplevel_info_state: ToplevelInfoState<State>,
    pub xdg_shell_state: XdgShellState,
    pub workspace_state: WorkspaceState<State>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum WorkspaceMode {
    OutputBound,
    Global {
        active: usize,
        group: WorkspaceGroupHandle,
    },
}

pub enum ShellMode {
    Normal,
    Resize,
    Adjust,
}

#[derive(Debug, Clone)]
pub struct OutputBoundState {
    active: Cell<usize>,
    group: Cell<WorkspaceGroupHandle>,
}

const UNINIT_SPACE: MaybeUninit<Workspace> = MaybeUninit::uninit();

impl Shell {
    pub fn new(config: &Config, dh: &DisplayHandle) -> Self {
        let layer_shell_state = WlrLayerShellState::new::<State, _>(dh, None);
        let toplevel_info_state = ToplevelInfoState::new(dh, |_| true);
        let xdg_shell_state = XdgShellState::new::<State, _>(dh, None);
        let mut workspace_state = WorkspaceState::new(dh, |_| true);

        let mut spaces = unsafe {
            let mut spaces = [UNINIT_SPACE; MAX_WORKSPACES];
            for (idx, space) in spaces.iter_mut().enumerate() {
                *space = MaybeUninit::new(Workspace::new(idx as u8, std::mem::zeroed() /* Will be initialized by init_mode */));
            }
            std::mem::transmute(spaces)
        };
        let mode = init_mode(&config.static_conf.workspace_mode, None, &[], &mut workspace_state, &mut spaces);

        Shell {
            popups: PopupManager::new(None),
            spaces,
            outputs: Vec::new(),
            workspace_mode: mode,
            shell_mode: ShellMode::Normal,

            pending_windows: Vec::new(),
            pending_layers: Vec::new(),

            layer_shell_state,
            toplevel_info_state,
            xdg_shell_state,
            workspace_state,
        }
    }

    pub fn add_output(&mut self, output: &Output) {
        let was_empty = self.outputs.is_empty();
        self.outputs.push(output.clone());
        let mut state = self.workspace_state.update();

        match self.workspace_mode {
            WorkspaceMode::OutputBound => {
                let idx = self.spaces
                    .iter()
                    .position(|x| x.space.outputs().next().is_none())
                    .expect("More then 10 outputs?");

                remap_output(output, &mut self.spaces, None, idx, Point::from((0, 0)), &mut self.toplevel_info_state);
                let mut workspace = &mut self.spaces[idx];
                
                let group = state.create_workspace_group();
                state.add_group_output(&group, output);
                state.remove_workspace(workspace.handle);
                let handle = init_workspace_handle(&mut state, &group, &mut workspace);
                state.add_workspace_state(&handle, WState::Active);

                let output_state = OutputBoundState {
                    active: Cell::new(workspace.idx as usize),
                    group: Cell::new(group),
                };

                if was_empty {
                    for workspace in self.spaces.iter_mut().skip(1) {
                        init_workspace_handle(&mut state, &group, workspace);
                    }
                }

                if !output.user_data().insert_if_missing(|| output_state.clone()) {
                    let existing_state = output.user_data().get::<OutputBoundState>().unwrap();
                    existing_state.active.set(output_state.active.get());
                    existing_state.group.set(output_state.group.get());
                }
            },
            WorkspaceMode::Global { active, group } => {
                state.add_group_output(&group, output);
                
                remap_output(output, &mut self.spaces, None, active, output.current_location(), &mut self.toplevel_info_state);
            }
        }
    }

    pub fn remove_output(&mut self, output: &Output) {
        let mut state = self.workspace_state.update();
        self.outputs.retain(|o| o != output);
        
        match self.workspace_mode {
            WorkspaceMode::OutputBound => {
                let output_state = output.user_data().get::<OutputBoundState>().unwrap();
                remap_output(output, &mut self.spaces, output_state.active.get(), None, None, &mut self.toplevel_info_state);

                // reassign workspaces to a different output
                let new_group = self.outputs.iter().next().map(|o| o.user_data().get::<OutputBoundState>().unwrap().group.get());
                for mut workspace in self.spaces.iter_mut() {
                    if state.workspace_belongs_to_group(&output_state.group.get(), &workspace.handle) {
                        state.remove_workspace(workspace.handle);
                        if let Some(new_group) = new_group {
                            init_workspace_handle(&mut state, &new_group, &mut workspace);
                        }
                    }
                }

                // destroy workspace group
                state.remove_workspace_group(output_state.group.get());
            },
            WorkspaceMode::Global { active, group } => {
                state.remove_group_output(&group, output);

                remap_output(output, &mut self.spaces, active, None, None, &mut self.toplevel_info_state);
            },
        };
    }

    pub fn refresh_outputs(&mut self) {
        if let WorkspaceMode::Global { active, .. } = self.workspace_mode {
            let workspace = &mut self.spaces[active];
            for output in self.outputs.iter() {
                workspace.space.map_output(output, output.current_location());
            }
        } else {
            for output in self.outputs.iter() {
                let active = output
                    .user_data()
                    .get::<OutputBoundState>()
                    .unwrap()
                    .active
                    .get();
                let workspace = &mut self.spaces[active];
                workspace.space.map_output(output, (0, 0));
            }
        }
    }

    pub fn set_mode(&mut self, mode: ConfigMode) {
        match (&mut self.workspace_mode, mode) {
            (WorkspaceMode::OutputBound, ConfigMode::Global) => {
                let new_active = 0;
                init_mode(&mode, Some(&WorkspaceMode::OutputBound), &self.outputs, &mut self.workspace_state, &mut self.spaces);
                for output in &self.outputs {
                    let old_active = output.user_data().get::<OutputBoundState>().unwrap().active.get();
                    remap_output(output, &mut self.spaces, old_active, new_active, output.current_location(), &mut self.toplevel_info_state);
                }
            },
            (x @ WorkspaceMode::Global { .. }, ConfigMode::OutputBound) => {
                // inits OutputBoundState if it not exists
                init_mode(&mode, Some(x), &self.outputs, &mut self.workspace_state, &mut self.spaces);
                if let WorkspaceMode::Global { ref active, .. } = x {
                    for output in &self.outputs {
                        let new_active = output.user_data().get::<OutputBoundState>().unwrap().active.get();
                        remap_output(output, &mut self.spaces, *active, new_active, Point::from((0, 0)), &mut self.toplevel_info_state);
                    }
                }
            },
            _ => {},
        }
    }

    pub fn activate(&mut self, _dh: &DisplayHandle, seat: &Seat<State>, output: &Output, idx: usize) -> Option<MotionEvent> {
        if idx > MAX_WORKSPACES {
            return None;
        }

        match self.workspace_mode {
            WorkspaceMode::OutputBound => {
                // if the workspace is active on a different output, move the cursor over
                for output in self.outputs.iter().filter(|o| o != &output) {
                    if output.user_data().get::<OutputBoundState>().unwrap().active.get() == idx {
                        let geometry = output.geometry();
                        set_active_output(seat, output);
                        return Some(MotionEvent {
                            location: Point::<i32, Logical>::from((
                                geometry.loc.x + (geometry.size.w / 2),
                                geometry.loc.y + (geometry.size.h / 2),
                            ))
                            .to_f64(),
                            focus: None, // This should actually be a surface, if there is one in the center
                            serial: SERIAL_COUNTER.next_serial(),
                            time: 0,
                        });
                    }
                }

                // else we exchange the workspace on the current output
                let output_state = output.user_data().get::<OutputBoundState>().unwrap();
                let old_active = output_state.active.get();
                if idx != old_active {
                    let mut state = self.workspace_state.update();
                    output_state.active.set(idx);

                    if !state.workspace_belongs_to_group(&output_state.group.get(), &self.spaces[idx].handle) {
                        state.remove_workspace(self.spaces[idx].handle);
                        init_workspace_handle(&mut state, &output_state.group.get(), &mut self.spaces[idx]);
                    }
                    
                    state.remove_workspace_state(&self.spaces[old_active].handle, WState::Active);
                    state.add_workspace_state(&self.spaces[idx].handle, WState::Active);

                    std::mem::drop(state);
                    remap_output(output, &mut self.spaces, old_active, idx, Point::from((0, 0)), &mut self.toplevel_info_state);
                }
            },
            WorkspaceMode::Global { ref mut active, .. } => {
                let old = *active;
                *active = idx;

                let mut state = self.workspace_state.update();
                for output in &self.outputs {
                    remap_output(output, &mut self.spaces, old, idx, output.current_location(), &mut self.toplevel_info_state);
                }
                state.remove_workspace_state(&self.spaces[old].handle, WState::Active);
                state.add_workspace_state(&self.spaces[idx].handle, WState::Active);
            }
        }

        None
    }

    pub fn active_space(&self, output: &Output) -> &Workspace {
        match &self.workspace_mode {
            WorkspaceMode::OutputBound => {
                let active = output
                    .user_data()
                    .get::<OutputBoundState>()
                    .unwrap()
                    .active
                    .get();
                &self.spaces[active]
            }
            WorkspaceMode::Global { active, .. } => &self.spaces[*active],
        }
    }

    pub fn active_space_mut(&mut self, output: &Output) -> &mut Workspace {
        match &self.workspace_mode {
            WorkspaceMode::OutputBound => {
                let active = output
                    .user_data()
                    .get::<OutputBoundState>()
                    .unwrap()
                    .active
                    .get();
                &mut self.spaces[active]
            }
            WorkspaceMode::Global { active, .. } => &mut self.spaces[*active],
        }
    }

    pub fn outputs_for_surface(&self, surface: &WlSurface) -> impl Iterator<Item=Output> {
        self.space_for_surface(surface)
            .and_then(|w| if let Some(window) = w.space.window_for_surface(surface, WindowSurfaceType::ALL) {
                Some(w.space.outputs_for_window(&window).into_iter())
            } else { None })
            .into_iter()
            .flatten()
    }

    pub fn space_for_surface(&self, surface: &WlSurface) -> Option<&Workspace> {
        self.spaces.iter().find(|workspace| {
            workspace.space.window_for_surface(surface, WindowSurfaceType::ALL).is_some()
        })
    }

    pub fn space_for_surface_mut(&mut self, surface: &WlSurface) -> Option<&mut Workspace> {
        self.spaces
            .iter_mut()
            .find(|workspace| workspace.space.window_for_surface(surface, WindowSurfaceType::ALL).is_some())
    }
    
    pub fn outputs(&self) -> impl Iterator<Item=&Output> {
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

    pub fn space_relative_output_geometry<C: smithay::utils::Coordinate>(
        &self,
        global_loc: impl Into<Point<C, Logical>>,
        output: &Output,
    ) -> Point<C, Logical> {
        match self.workspace_mode {
            WorkspaceMode::Global { .. } => global_loc.into(),
            WorkspaceMode::OutputBound => {
                let p = global_loc.into().to_f64() - output.current_location().to_f64();
                (C::from_f64(p.x), C::from_f64(p.y)).into()
            }
        }
    }

    pub fn refresh(&mut self, dh: &DisplayHandle) {
        self.popups.cleanup();
        let mut state = self.workspace_state.update();
        for output in &self.outputs {
            let workspace = match &self.workspace_mode {
                WorkspaceMode::OutputBound => {
                    let active = output
                        .user_data()
                        .get::<OutputBoundState>()
                        .unwrap()
                        .active
                        .get();
                    &mut self.spaces[active]
                }
                WorkspaceMode::Global { active, .. } => &mut self.spaces[*active],
            };
            workspace.refresh(dh);
            if workspace.space.windows().next().is_none() {
                state.add_workspace_state(&workspace.handle, WState::Hidden);
            }
            let mut map = layer_map_for_output(output);
            map.cleanup();
        }
        std::mem::drop(state);
        self.toplevel_info_state.refresh(Some(&self.workspace_state));
    }
    
    pub fn map_window(&mut self, window: &Window, output: &Output) {
        let pos = self.pending_windows.iter().position(|(w, _)| w == window).unwrap();
        let (window, seat) = self.pending_windows.remove(pos);

        let workspace = match &self.workspace_mode {
            WorkspaceMode::OutputBound => {
                let active = output
                    .user_data()
                    .get::<OutputBoundState>()
                    .unwrap()
                    .active
                    .get();
                &mut self.spaces[active]
            }
            WorkspaceMode::Global { active, .. } => &mut self.spaces[*active],
        };
        self.workspace_state.update().remove_workspace_state(&workspace.handle, WState::Hidden);
        self.toplevel_info_state.toplevel_enter_workspace(&window, &workspace.handle);
        let focus_stack = workspace.focus_stack(&seat);
        if layout::should_be_floating(&window) {
            workspace.floating_layer.map_window(&mut workspace.space, window, &seat);
        } else {
            workspace.tiling_layer.map_window(&mut workspace.space, window, &seat, focus_stack.iter());
        }
    }

    pub fn map_layer(&mut self, layer_surface: &LayerSurface, dh: &DisplayHandle) {
        let pos = self.pending_layers.iter().position(|(l, _, _)| l == layer_surface).unwrap();
        let (layer_surface, output, seat) = self.pending_layers.remove(pos);
            
        let surface = layer_surface.wl_surface();
        let wants_focus = {
            with_states(surface, |states| {
                let state = states.cached_state.current::<LayerSurfaceCachedState>();
                matches!(state.layer, Layer::Top | Layer::Overlay)
                    && dbg!(state.keyboard_interactivity) != KeyboardInteractivity::None
            })
        };

        let mut map = layer_map_for_output(&output);
        map.map_layer(dh, &layer_surface).unwrap();

        if wants_focus {
            self.set_focus(dh, Some(surface), &seat, None)
        }
    }

    pub fn move_current_window(&mut self, seat: &Seat<State>, output: &Output, idx: usize) {
        if idx > MAX_WORKSPACES {
            return;
        }

        let workspace = match &self.workspace_mode {
            WorkspaceMode::OutputBound => {
                let active = output
                    .user_data()
                    .get::<OutputBoundState>()
                    .unwrap()
                    .active
                    .get();
                &mut self.spaces[active]
            }
            WorkspaceMode::Global { active, .. } => &mut self.spaces[*active],
        };
        if idx == workspace.idx as usize {
            return;
        }

        let maybe_window = workspace.focus_stack(seat).last();
        if let Some(window) = maybe_window {
            workspace.floating_layer.unmap_window(&mut workspace.space, &window);
            workspace.tiling_layer.unmap_window(&mut workspace.space, &window);
            self.toplevel_info_state.toplevel_leave_workspace(&window, &workspace.handle);
            
            let new_workspace = &mut self.spaces[idx];
            self.toplevel_info_state.toplevel_enter_workspace(&window, &new_workspace.handle);
            let focus_stack = new_workspace.focus_stack(&seat);
            if layout::should_be_floating(&window) {
                new_workspace.floating_layer.map_window(&mut new_workspace.space, window, &seat);
            } else {
                new_workspace.tiling_layer.map_window(&mut new_workspace.space, window, &seat, focus_stack.iter());
            }
        }
    }
}

fn init_mode(
    config_mode: &ConfigMode,
    old_mode: Option<&WorkspaceMode>,
    outputs: &[Output],
    state: &mut WorkspaceState<State>,
    workspaces: &mut [Workspace; MAX_WORKSPACES]
) -> WorkspaceMode {
    let mut state = state.update();

    // cleanup
    for workspace in workspaces.iter_mut() {
        state.remove_workspace(workspace.handle);
    }
    
    match old_mode {
        Some(WorkspaceMode::Global { group, .. }) => state.remove_workspace_group(group.clone()),
        Some(WorkspaceMode::OutputBound) => for output in outputs {
            if let Some(old_state) = output.user_data().get::<OutputBoundState>() {
                state.remove_workspace_group(old_state.group.get());
            }
        },
        _ => {},
    };

    // set the new state (especially cosmic_workspace state)
    match config_mode {
        ConfigMode::Global => {
            let group = state.create_workspace_group();
            for output in outputs {
                state.add_group_output(&group, output)
            }
            for workspace in workspaces.iter_mut() {
                init_workspace_handle(&mut state, &group, workspace);
            }
            state.add_workspace_state(&workspaces[0].handle, WState::Active);
            state.remove_workspace_state(&workspaces[0].handle, WState::Hidden);
            WorkspaceMode::Global {
                active: 0,
                group,
            }
        },
        ConfigMode::OutputBound => {
            for (i, output) in outputs.iter().enumerate() {
                let group = state.create_workspace_group();
                state.add_group_output(&group, output);

                let workspace = workspaces.get_mut(i).expect("More then ten workspaces?!?");
                let handle = init_workspace_handle(&mut state, &group, workspace);
                state.add_workspace_state(&handle, WState::Active);
                state.remove_workspace_state(&handle, WState::Hidden);

                let output_state = OutputBoundState {
                    active: Cell::new(i),
                    group: Cell::new(group),
                };
                let map = output.user_data();
                if !map.insert_if_missing(|| output_state) {
                    let old_state = map.get::<OutputBoundState>().unwrap();
                    old_state.active.set(i);
                    old_state.group.set(group);
                }
            }
            if !outputs.is_empty() {
                for workspace in workspaces.iter_mut().skip(outputs.iter().count()) {
                    let group = outputs[0].user_data().get::<OutputBoundState>().unwrap().group.get();
                    init_workspace_handle(&mut state, &group, workspace);
                }
            }
            WorkspaceMode::OutputBound
        }
    }
}

fn init_workspace_handle<'a>(state: &mut WorkspaceUpdateGuard<'a, State>, group: &WorkspaceGroupHandle, workspace: &mut Workspace) -> WorkspaceHandle {
    let handle = state.create_workspace(&group).unwrap();
    state.set_workspace_capabilities(&handle, [WorkspaceCapabilities::Activate].into_iter());
    state.set_workspace_name(&handle, format!("{}", workspace.idx));
    state.set_workspace_coordinates(&handle, [Some(workspace.idx as u32), None, None]);
    if workspace.space.windows().next().is_none() {
        state.add_workspace_state(&handle, WState::Hidden);
    }
    workspace.handle = handle.clone();
    handle
}

fn remap_output(
    output: &Output,
    spaces: &mut [Workspace],
    old: impl Into<Option<usize>>,
    new: impl Into<Option<usize>>,
    pos: impl Into<Option<Point<i32, Logical>>>,
    info_state: &mut ToplevelInfoState<State>
) {
    if let Some(old) = old.into() {
        let old_space = &mut spaces[old].space;
        old_space.unmap_output(output);
        for window in old_space.windows() {
            info_state.toplevel_leave_output(window, output);
        }
    }
    if let Some(new) = new.into() {
        let new_space = &mut spaces[new].space;
        new_space.map_output(output, pos.into().expect("new requires pos"));
        for window in new_space.windows() {
            info_state.toplevel_enter_output(window, output);
        }
    }
}