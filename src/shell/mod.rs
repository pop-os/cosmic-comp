// SPDX-License-Identifier: GPL-3.0-only

#[cfg(feature = "experimental")]
use crate::wayland::workspace as ext_work;
use crate::{
    config::{Config, OutputConfig},
    input::{active_output, set_active_output},
    state::Common,
};
pub use smithay::{
    desktop::{layer_map_for_output, PopupGrab, PopupManager, PopupUngrabStrategy, Space, Window, WindowSurfaceType},
    reexports::wayland_server::{protocol::wl_surface::WlSurface, Display},
    utils::{Logical, Point, Rectangle, Size},
    wayland::{
        compositor::with_states,
        output::{Mode as OutputMode, Output, Scale},
        seat::Seat,
        shell::{
            wlr_layer::{KeyboardInteractivity, Layer, LayerSurfaceCachedState},
            xdg::XdgToplevelSurfaceRoleAttributes,
        },
        Serial, SERIAL_COUNTER,
    },
};

use std::{
    cell::{Cell, RefCell},
    mem::MaybeUninit,
    sync::Mutex,
};

pub const MAX_WORKSPACES: usize = 10; // TODO?

mod handler;
pub mod layout;
mod workspace;
pub use self::handler::{init_shell, PopupGrabData};
pub use self::layout::Layout;
pub use self::workspace::*;

pub struct ActiveWorkspace(Cell<Option<usize>>);
impl ActiveWorkspace {
    fn new() -> Self {
        ActiveWorkspace(Cell::new(None))
    }
    pub fn get(&self) -> Option<usize> {
        self.0.get()
    }
    fn set(&self, active: usize) -> Option<usize> {
        self.0.replace(Some(active))
    }
    fn clear(&self) -> Option<usize> {
        self.0.replace(None)
    }
}

#[derive(Debug, serde::Deserialize, PartialEq, Eq, Clone, Copy)]
pub enum Mode {
    OutputBound,
    Global {
        #[serde(default)]
        active: usize,
    },
}

impl Mode {
    pub fn output_bound() -> Mode {
        Mode::OutputBound
    }

    pub fn global() -> Mode {
        Mode::Global { active: 0 }
    }
}

pub struct Shell {
    popups: PopupManager,
    mode: Mode,
    outputs: Vec<Output>,
    pub spaces: [Workspace; MAX_WORKSPACES],
    #[cfg(feature = "experimental")]
    manager: ext_work::WorkspaceManager,
    #[cfg(feature = "experimental")]
    global_group: Option<ext_work::WorkspaceGroup>,
}

const UNINIT_SPACE: MaybeUninit<Workspace> = MaybeUninit::uninit();

impl Shell {
    fn new(config: &Config, _display: &mut Display) -> Self {
        Shell {
            popups: PopupManager::new(None),
            mode: config.static_conf.workspace_mode,
            outputs: Vec::new(),
            spaces: unsafe {
                let mut spaces = [UNINIT_SPACE; MAX_WORKSPACES];
                for (idx, space) in spaces.iter_mut().enumerate() {
                    *space = MaybeUninit::new(Workspace::new(idx as u8));
                }
                std::mem::transmute(spaces)
            },
            #[cfg(feature = "experimental")]
            manager: ext_work::init_ext_workspace(
                _display,
                |_, changes, mut ddata| {
                    let state = ddata.get::<crate::state::State>().unwrap();
                    if let Some((w, _)) = changes.into_iter().rev().find(|(_, p)| {
                        p.into_iter()
                            .rev()
                            .find(|o| {
                                **o == ext_work::PendingOperation::Activate
                                    || **o == ext_work::PendingOperation::Deactivate
                            })
                            .map(|o| *o == ext_work::PendingOperation::Activate)
                            .unwrap_or(false)
                    }) {
                        if let Some(idx) = state.common.shell.spaces.iter().position(|work| {
                            work.ext_workspace.as_ref().map(|e| e == w).unwrap_or(false)
                        }) {
                            state.common.event_loop_handle.insert_idle(move |state| {
                                let seat = &state.common.last_active_seat;
                                let output = active_output(&seat, &state.common);
                                state.common.shell.activate(seat, &output, idx);
                            });
                        }
                    }
                },
                |_| true,
            )
            .0,
            #[cfg(feature = "experimental")]
            global_group: None,
        }
    }

    pub fn refresh_outputs(&mut self) {
        if let Mode::Global { active } = self.mode {
            let workspace = &mut self.spaces[active];
            for output in self.outputs.iter() {
                let config = output
                    .user_data()
                    .get::<RefCell<OutputConfig>>()
                    .unwrap()
                    .borrow();
                workspace.space.map_output(output, config.position);
            }
        } else {
            for output in self.outputs.iter() {
                let active = output
                    .user_data()
                    .get::<ActiveWorkspace>()
                    .unwrap()
                    .get()
                    .unwrap();
                let workspace = &mut self.spaces[active];
                workspace.space.map_output(output, (0, 0));
            }
        }
    }

    fn assign_next_free_output<'a>(
        spaces: &'a mut [Workspace],
        output: &Output,
    ) -> (usize, &'a mut Workspace) {
        output
            .user_data()
            .insert_if_missing(|| ActiveWorkspace::new());
        let (idx, workspace) = spaces
            .iter_mut()
            .enumerate()
            .find(|(_, x)| x.space.outputs().next().is_none())
            .expect("More then 10 outputs?");
        output
            .user_data()
            .get::<ActiveWorkspace>()
            .unwrap()
            .set(idx);

        (idx, workspace)
    }

    pub fn add_output(&mut self, output: &Output) {
        self.outputs.push(output.clone());
        let config = output
            .user_data()
            .get::<RefCell<OutputConfig>>()
            .unwrap()
            .borrow();

        match self.mode {
            Mode::OutputBound => {
                let _idx = {
                    let (idx, workspace) = Self::assign_next_free_output(&mut self.spaces, output);
                    workspace.space.map_output(output, (0, 0));
                    idx
                };
                #[cfg(feature = "experimental")]
                {
                    let group = self
                        .manager
                        .new_group(|_, _, _| {}, std::iter::once(output.clone()));
                    if self.spaces.iter().all(|w| w.ext_workspace.is_none()) {
                        for (idx, workspace) in self.spaces.iter_mut().enumerate() {
                            let ext_workspace = group.create_workspace(format!("{}", idx + 1));
                            ext_workspace.set_coordinates(std::iter::once(idx as u32));
                            workspace.ext_workspace = Some(ext_workspace);
                        }
                        self.spaces[_idx]
                            .ext_workspace
                            .as_mut()
                            .unwrap()
                            .add_state(ext_work::State::Active);
                    } else {
                        let ext_workspace = group.create_workspace(format!("{}", _idx + 1));
                        ext_workspace.set_coordinates(std::iter::once(_idx as u32));
                        ext_workspace.add_state(ext_work::State::Active);
                        if let Some(old) = self.spaces[_idx].ext_workspace.replace(ext_workspace) {
                            old.remove();
                        }
                    }
                    output.user_data().insert_if_missing(|| {
                        RefCell::new(Option::<ext_work::WorkspaceGroup>::None)
                    });
                    *output
                        .user_data()
                        .get::<RefCell<Option<ext_work::WorkspaceGroup>>>()
                        .unwrap()
                        .borrow_mut() = Some(group);
                    self.manager.done();
                }
            }
            Mode::Global { active } => {
                #[cfg(feature = "experimental")]
                {
                    if self.global_group.is_none() {
                        self.global_group = Some(
                            self.manager
                                .new_group(|_, _, _| {}, std::iter::once(output.clone())),
                        );
                        for (idx, workspace) in self.spaces.iter_mut().enumerate() {
                            let ext_workspace = self
                                .global_group
                                .as_mut()
                                .unwrap()
                                .create_workspace(format!("{}", idx + 1));
                            ext_workspace.set_coordinates(std::iter::once(idx as u32));
                            if idx == active {
                                ext_workspace.add_state(ext_work::State::Active);
                            }
                            workspace.ext_workspace = Some(ext_workspace);
                        }
                    } else {
                        self.manager.update_outputs(|_, outputs| {
                            outputs.insert(output.clone());
                        });
                    }
                    self.manager.done();
                }

                let workspace = &mut self.spaces[active];
                workspace.space.map_output(output, config.position);
            }
        }
        output.change_current_state(None, None, Some(Scale::Fractional(config.scale)), None);
    }

    pub fn remove_output(&mut self, output: &Output) {
        match self.mode {
            Mode::OutputBound => {
                if let Some(idx) = output
                    .user_data()
                    .get::<ActiveWorkspace>()
                    .and_then(|a| a.get())
                {
                    self.spaces[idx].space.unmap_output(output);
                    self.outputs.retain(|o| o != output);
                }
                #[cfg(feature = "experimental")]
                {
                    let group = output
                        .user_data()
                        .get::<RefCell<Option<ext_work::WorkspaceGroup>>>()
                        .unwrap()
                        .borrow_mut()
                        .take()
                        .unwrap();
                    let mut alternative_group = self.outputs.last_mut().map(|o| {
                        o.user_data()
                            .get::<RefCell<Option<ext_work::WorkspaceGroup>>>()
                            .unwrap()
                    });
                    for (idx, workspace) in self.spaces.iter_mut().enumerate() {
                        if workspace
                            .ext_workspace
                            .as_ref()
                            .map(|w| group.belongs(w))
                            .unwrap_or(false)
                        {
                            workspace.ext_workspace.take().unwrap().remove();
                            if let Some(alternative) = alternative_group.as_mut() {
                                let ext_workspace = alternative
                                    .borrow_mut()
                                    .as_mut()
                                    .unwrap()
                                    .create_workspace(format!("{}", idx + 1));
                                ext_workspace.set_coordinates(std::iter::once(idx as u32));
                                workspace.ext_workspace = Some(ext_workspace);
                            }
                        }
                    }
                    self.manager.remove_group(&group);
                }
            }
            Mode::Global { active } => {
                #[cfg(feature = "experimental")]
                self.manager.update_outputs(|_, outputs| {
                    outputs.remove(output);
                });
                self.spaces[active].space.unmap_output(output);
                self.outputs.retain(|o| o != output);
                // TODO move windows and outputs farther on the right / or load save config for remaining monitors
            }
        }
    }

    pub fn output_size(&self, output: &Output) -> Size<i32, Logical> {
        let workspace = self.active_space(output);
        workspace
            .space
            .output_geometry(&output)
            .unwrap_or(Rectangle::from_loc_and_size((0, 0), (0, 0)))
            .size
    }

    pub fn global_space(&self) -> Rectangle<i32, Logical> {
        self.outputs
            .iter()
            .fold(
                Option::<Rectangle<i32, Logical>>::None,
                |maybe_geo, output| match maybe_geo {
                    Some(rect) => Some(rect.merge(self.output_geometry(output))),
                    None => Some(self.output_geometry(output)),
                },
            )
            .unwrap_or_else(|| Rectangle::from_loc_and_size((0, 0), (0, 0)))
    }

    pub fn space_relative_output_geometry<C: smithay::utils::Coordinate>(
        &self,
        global_loc: impl Into<Point<C, Logical>>,
        output: &Output,
    ) -> Point<C, Logical> {
        match self.mode {
            Mode::Global { .. } => global_loc.into(),
            Mode::OutputBound => {
                let p = global_loc.into().to_f64() - self.output_geometry(output).loc.to_f64();
                (C::from_f64(p.x), C::from_f64(p.y)).into()
            }
        }
    }

    pub fn output_geometry(&self, output: &Output) -> Rectangle<i32, Logical> {
        // due to our different modes, we cannot just ask the space for the global output coordinates,
        // because for `Mode::OutputBound` the origin will always be (0, 0)

        let config = output
            .user_data()
            .get::<RefCell<OutputConfig>>()
            .unwrap()
            .borrow();
        Rectangle::from_loc_and_size(config.position, self.output_size(output))
    }

    pub fn activate(&mut self, seat: &Seat, output: &Output, idx: usize) {
        if idx > MAX_WORKSPACES {
            return;
        }

        match self.mode {
            Mode::OutputBound => {
                for output in &self.outputs {
                    if output
                        .user_data()
                        .get::<ActiveWorkspace>()
                        .and_then(|i| i.get().map(|i| i == idx))
                        .unwrap_or(false)
                    {
                        let geometry = self.output_geometry(output);
                        if let Some(ptr) = seat.get_pointer() {
                            ptr.motion(
                                Point::<i32, Logical>::from((
                                    geometry.loc.x + (geometry.size.w / 2),
                                    geometry.loc.y + (geometry.size.h / 2),
                                ))
                                .to_f64(),
                                None,
                                SERIAL_COUNTER.next_serial(),
                                0,
                            );
                            set_active_output(seat, output);
                            return;
                        }
                    }
                }

                if let Some(active) = output.user_data().get::<ActiveWorkspace>() {
                    if let Some(old_idx) = active.set(idx) {
                        #[cfg(feature = "experimental")]
                        if let Some(ext_workspace) = self.spaces[old_idx].ext_workspace.as_mut() {
                            ext_workspace.remove_state(ext_work::State::Active);
                        }
                        self.spaces[old_idx].space.unmap_output(output);
                    }
                    self.spaces[idx].space.map_output(output, (0, 0));
                    #[cfg(feature = "experimental")]
                    {
                        let mut group_borrow = output
                            .user_data()
                            .get::<RefCell<Option<ext_work::WorkspaceGroup>>>()
                            .unwrap()
                            .borrow_mut();
                        let group = group_borrow.as_mut().unwrap();
                        if self.spaces[idx]
                            .ext_workspace
                            .as_ref()
                            .map(|x| !group.belongs(x))
                            .unwrap_or(true)
                        {
                            let ext_workspace = group.create_workspace(format!("{}", idx + 1));
                            ext_workspace.set_coordinates(std::iter::once(idx as u32));
                            if let Some(old) = self.spaces[idx].ext_workspace.replace(ext_workspace)
                            {
                                old.remove();
                            }
                        }
                        self.spaces[idx]
                            .ext_workspace
                            .as_mut()
                            .unwrap()
                            .add_state(ext_work::State::Active);
                        self.manager.done();
                    }
                    self.spaces[idx].refresh();
                }
            }
            Mode::Global { ref mut active } => {
                let old = *active;
                *active = idx;
                for output in &self.outputs {
                    self.spaces[old].space.unmap_output(output);
                    let config = output
                        .user_data()
                        .get::<RefCell<OutputConfig>>()
                        .unwrap()
                        .borrow();
                    self.spaces[*active]
                        .space
                        .map_output(output, config.position);
                }
                #[cfg(feature = "experimental")]
                {
                    for (idx, workspace) in self.spaces.iter_mut().enumerate() {
                        if idx == *active {
                            workspace
                                .ext_workspace
                                .as_mut()
                                .unwrap()
                                .add_state(ext_work::State::Active);
                        } else {
                            workspace
                                .ext_workspace
                                .as_mut()
                                .unwrap()
                                .remove_state(ext_work::State::Active);
                        }
                    }
                    self.manager.done();
                }
            }
        };
    }

    pub fn move_current_window(&mut self, seat: &Seat, output: &Output, idx: usize) {
        if idx > MAX_WORKSPACES {
            return;
        }

        let workspace = self.active_space_mut(output);
        if idx == workspace.idx as usize {
            return;
        }

        let maybe_window = workspace.focus_stack(seat).last();
        if let Some(window) = maybe_window {
            workspace.unmap_window(&window);
            self.spaces[idx].map_window(&window, seat);
        }
    }

    #[cfg(feature = "debug")]
    pub fn mode(&self) -> &Mode {
        &self.mode
    }

    pub fn set_mode(&mut self, mode: Mode) {
        match (&mut self.mode, mode) {
            (Mode::OutputBound, Mode::Global { .. }) => {
                let active = self
                    .outputs
                    .iter()
                    .next()
                    .map(|o| {
                        o.user_data()
                            .get::<ActiveWorkspace>()
                            .unwrap()
                            .get()
                            .unwrap()
                    })
                    .unwrap_or(0);

                for output in &self.outputs {
                    let old_active = output
                        .user_data()
                        .get::<ActiveWorkspace>()
                        .unwrap()
                        .clear()
                        .unwrap();
                    let config = output
                        .user_data()
                        .get::<RefCell<OutputConfig>>()
                        .unwrap()
                        .borrow();
                    self.spaces[old_active].space.unmap_output(output);
                    self.spaces[active]
                        .space
                        .map_output(output, config.position);
                    self.spaces[active].refresh();
                }

                self.mode = Mode::Global { active };
            }
            (Mode::Global { active }, new @ Mode::OutputBound) => {
                for output in &self.outputs {
                    self.spaces[*active].space.unmap_output(output);
                }

                let mut active = Some(active.clone());
                self.mode = new;
                for output in &self.outputs {
                    let workspace = if let Some(a) = active.take() {
                        output
                            .user_data()
                            .insert_if_missing(|| ActiveWorkspace::new());
                        output.user_data().get::<ActiveWorkspace>().unwrap().set(a);
                        &mut self.spaces[a]
                    } else {
                        Self::assign_next_free_output(&mut self.spaces, output).1
                    };
                    workspace.space.map_output(output, (0, 0));
                    workspace.refresh();
                }
            }
            _ => {}
        };
    }

    pub fn outputs(&self) -> impl Iterator<Item = &Output> {
        self.outputs.iter()
    }

    pub fn active_space(&self, output: &Output) -> &Workspace {
        match &self.mode {
            Mode::OutputBound => {
                let active = output
                    .user_data()
                    .get::<ActiveWorkspace>()
                    .unwrap()
                    .get()
                    .unwrap();
                &self.spaces[active]
            }
            Mode::Global { active } => &self.spaces[*active],
        }
    }

    pub fn active_space_mut(&mut self, output: &Output) -> &mut Workspace {
        match &self.mode {
            Mode::OutputBound => {
                let active = output
                    .user_data()
                    .get::<ActiveWorkspace>()
                    .unwrap()
                    .get()
                    .unwrap();
                &mut self.spaces[active]
            }
            Mode::Global { active } => &mut self.spaces[*active],
        }
    }

    pub fn space_for_surface(&self, surface: &WlSurface) -> Option<&Workspace> {
        self.spaces.iter().find(|workspace| {
            workspace
                .pending_windows
                .iter()
                .any(|(w, _)| w.toplevel().get_surface() == Some(surface))
                || workspace.space.window_for_surface(surface, WindowSurfaceType::ALL).is_some()
        })
    }

    pub fn space_for_surface_mut(&mut self, surface: &WlSurface) -> Option<&mut Workspace> {
        self.spaces
            .iter_mut()
            .find(|workspace| workspace.space.window_for_surface(surface, WindowSurfaceType::ALL).is_some())
    }

    pub fn refresh(&mut self) {
        self.popups.cleanup();
        for output in self.outputs.iter() {
            let workspace = match &self.mode {
                Mode::OutputBound => {
                    let active = output
                        .user_data()
                        .get::<ActiveWorkspace>()
                        .unwrap()
                        .get()
                        .unwrap();
                    &mut self.spaces[active]
                }
                Mode::Global { active } => &mut self.spaces[*active],
            };
            workspace.refresh();
            let mut map = layer_map_for_output(output);
            map.cleanup();
            map.arrange();
        }
    }

    pub fn commit<'a>(&mut self, surface: &WlSurface, seats: impl Iterator<Item = &'a Seat>) {
        let mut new_focus = None;
        for (idx, workspace) in self.spaces.iter_mut().enumerate() {
            if let Some((window, seat)) = workspace
                .pending_windows
                .iter()
                .find(|(w, _)| w.toplevel().get_surface() == Some(surface))
                .cloned()
            {
                workspace.map_window(&window, &seat);
                if match self.mode {
                    Mode::OutputBound => self.outputs.iter().any(|o| {
                        o.user_data()
                            .get::<ActiveWorkspace>()
                            .unwrap()
                            .get()
                            .unwrap()
                            == idx
                    }),
                    Mode::Global { active } => active == idx,
                } {
                    new_focus = Some(seat);
                }
                workspace.pending_windows.retain(|(w, _)| w != &window);
            }
            if let Some((layer, output, seat)) = workspace
                .pending_layers
                .iter()
                .find(|(l, _, _)| l.get_surface() == Some(surface))
                .cloned()
            {
                let focus = layer
                    .get_surface()
                    .map(|surface| {
                        with_states(surface, |states| {
                            let state = states.cached_state.current::<LayerSurfaceCachedState>();
                            matches!(state.layer, Layer::Top | Layer::Overlay)
                                && dbg!(state.keyboard_interactivity) != KeyboardInteractivity::None
                        })
                        .unwrap()
                    })
                    .unwrap_or(false);

                let mut map = layer_map_for_output(&output);
                map.map_layer(&layer).unwrap();

                if focus {
                    new_focus = Some(seat);
                }
                workspace.pending_layers.retain(|(l, _, _)| l != &layer);
            }
            workspace.space.commit(surface);
        }
        if let Some(seat) = new_focus {
            self.set_focus(Some(surface), &seat, seats, None)
        }
        self.popups.commit(&surface);
    }

    pub fn set_orientation(
        &mut self,
        seat: &Seat,
        output: &Output,
        orientation: layout::Orientation,
    ) {
        self.active_space_mut(output)
            .update_orientation(seat, orientation)
    }

    pub fn move_focus<'a>(
        &mut self,
        seat: &Seat,
        output: &Output,
        focus: layout::FocusDirection,
        seats: impl Iterator<Item = &'a Seat>,
    ) {
        if let Some(surface) = self
            .active_space_mut(output)
            .move_focus(seat, focus)
            .and_then(|window| window.toplevel().get_surface().cloned())
        {
            self.set_focus(Some(&surface), seat, seats, None)
        }
    }

    fn set_focus<'a>(
        &mut self,
        surface: Option<&WlSurface>,
        active_seat: &Seat,
        seats: impl Iterator<Item = &'a Seat>,
        serial: Option<Serial>,
    ) {
        // update FocusStack and notify layouts about new focus (if any window)
        if let Some(surface) = surface {
            if let Some(workspace) = self.space_for_surface_mut(surface) {
                if let Some(window) = workspace.space.window_for_surface(surface, WindowSurfaceType::ALL) {
                    let mut focus_stack = workspace.focus_stack_mut(active_seat);
                    if Some(window) != focus_stack.last().as_ref() {
                        slog_scope::debug!("Focusing window: {:?}", window);
                        focus_stack.append(window);
                        // also remove popup grabs, if we are switching focus
                        if let Some(mut popup_grab) = active_seat
                            .user_data()
                            .get::<PopupGrabData>()
                            .and_then(|x| x.take())
                        {
                            if !popup_grab.has_ended() {
                                popup_grab.ungrab(PopupUngrabStrategy::All);
                            }
                        }
                    }
                }
            }
        }

        // update keyboard focus
        if let Some(keyboard) = active_seat.get_keyboard() {
            ActiveFocus::set(active_seat, surface.cloned());
            keyboard.set_focus(
                surface,
                serial.unwrap_or_else(|| SERIAL_COUNTER.next_serial()),
            );
        }

        self.update_active(seats)
    }

    fn update_active<'a>(&self, seats: impl Iterator<Item = &'a Seat>) {
        // update activate status
        let focused_windows = seats
            .flat_map(|seat| {
                self.outputs
                    .iter()
                    .flat_map(|o| self.active_space(o).focus_stack(seat).last().clone())
            })
            .collect::<Vec<_>>();

        for output in self.outputs() {
            let workspace = self.active_space(output);
            for window in workspace.space.windows() {
                window.set_activated(focused_windows.contains(window));
                window.configure();
            }
        }
    }
}

pub struct ActiveFocus(RefCell<Option<WlSurface>>);

impl ActiveFocus {
    fn set(seat: &Seat, surface: Option<WlSurface>) {
        if !seat
            .user_data()
            .insert_if_missing(|| ActiveFocus(RefCell::new(surface.clone())))
        {
            *seat
                .user_data()
                .get::<ActiveFocus>()
                .unwrap()
                .0
                .borrow_mut() = surface;
        }
    }

    fn get(seat: &Seat) -> Option<WlSurface> {
        seat.user_data()
            .get::<ActiveFocus>()
            .and_then(|a| a.0.borrow().clone())
    }
}

impl Common {
    pub fn set_focus(
        &mut self,
        surface: Option<&WlSurface>,
        active_seat: &Seat,
        serial: Option<Serial>,
    ) {
        self.shell
            .set_focus(surface, active_seat, self.seats.iter(), serial)
    }

    pub fn refresh_focus(&mut self) {
        for seat in &self.seats {
            let mut fixup = false;
            let output = active_output(seat, &self);
            let last_known_focus = ActiveFocus::get(seat);

            if let Some(surface) = last_known_focus {
                if surface.as_ref().is_alive() {
                    let is_toplevel = with_states(&surface, |states| {
                        states
                            .data_map
                            .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
                            .is_some()
                    })
                    .unwrap_or(false);
                    if !is_toplevel {
                        continue;
                    }

                    let workspace = self.shell.active_space(&output);
                    if let Some(window) = workspace.space.window_for_surface(&surface, WindowSurfaceType::ALL) {
                        let focus_stack = workspace.focus_stack(&seat);
                        if focus_stack.last().map(|w| &w != window).unwrap_or(true) {
                            fixup = true;
                        }
                    } else {
                        fixup = true;
                    }
                } else {
                    fixup = true;
                }
            }

            if fixup {
                // also remove popup grabs, if we are switching focus
                if let Some(mut popup_grab) = seat
                    .user_data()
                    .get::<PopupGrabData>()
                    .and_then(|x| x.take())
                {
                    if !popup_grab.has_ended() {
                        popup_grab.ungrab(PopupUngrabStrategy::All);
                    }
                }

                // update keyboard focus
                let surface = self
                    .shell
                    .active_space(&output)
                    .focus_stack(seat)
                    .last()
                    .and_then(|w| w.toplevel().get_surface().cloned());
                if let Some(keyboard) = seat.get_keyboard() {
                    keyboard.set_focus(surface.as_ref(), SERIAL_COUNTER.next_serial());
                    ActiveFocus::set(seat, surface);
                }
            }
        }

        self.shell.update_active(self.seats.iter())
    }
}
