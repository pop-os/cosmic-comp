use crate::{
    shell::{OutputBoundState, Shell, Workspace, WorkspaceMode},
    state::Common,
    utils::prelude::*,
    wayland::handlers::xdg_shell::PopupGrabData,
};
use indexmap::IndexSet;
use smithay::{
    desktop::{layer_map_for_output, PopupUngrabStrategy, Window, WindowSurfaceType},
    input::Seat,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    utils::{IsAlive, Serial, SERIAL_COUNTER},
    wayland::{
        compositor::get_role,
        shell::{wlr_layer::LAYER_SURFACE_ROLE, xdg::XDG_TOPLEVEL_ROLE},
    },
};
use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
};

pub mod target;

#[derive(Debug, serde::Deserialize, Clone, Copy, PartialEq, Eq)]
pub enum FocusDirection {
    Left,
    Right,
    Up,
    Down,
    In,
    Out,
}

pub struct FocusStack<'a>(Ref<'a, IndexSet<Window>>);
pub struct FocusStackMut<'a>(RefMut<'a, IndexSet<Window>>);

impl<'a> FocusStack<'a> {
    pub fn last(&self) -> Option<Window> {
        self.0.iter().rev().find(|w| w.toplevel().alive()).cloned()
    }

    pub fn iter(&self) -> impl Iterator<Item = &'_ Window> {
        self.0.iter().rev().filter(|w| w.toplevel().alive())
    }
}

impl<'a> FocusStackMut<'a> {
    pub fn append(&mut self, window: &Window) {
        self.0.retain(|w| w.toplevel().alive());
        self.0.shift_remove(window);
        self.0.insert(window.clone());
    }

    pub fn last(&self) -> Option<Window> {
        self.0.iter().rev().find(|w| w.toplevel().alive()).cloned()
    }

    pub fn iter(&self) -> impl Iterator<Item = &'_ Window> {
        self.0.iter().rev().filter(|w| w.toplevel().alive())
    }
}

type FocusStackData = RefCell<(HashMap<u8, IndexSet<Window>>, IndexSet<Window>)>;

impl Workspace {
    pub fn focus_stack<'a, 'b>(&'b self, seat: &'a Seat<State>) -> FocusStack<'a> {
        seat.user_data()
            .insert_if_missing(|| FocusStackData::new((HashMap::new(), IndexSet::new())));
        let idx = self.idx;
        FocusStack(Ref::map(
            seat.user_data().get::<FocusStackData>().unwrap().borrow(),
            |map| map.0.get(&idx).unwrap_or(&map.1), //TODO: workaround until Ref::filter_map goes stable
        ))
    }

    pub fn focus_stack_mut<'a, 'b>(&'b self, seat: &'a Seat<State>) -> FocusStackMut<'a> {
        seat.user_data()
            .insert_if_missing(|| FocusStackData::new((HashMap::new(), IndexSet::new())));
        let idx = self.idx;
        FocusStackMut(RefMut::map(
            seat.user_data()
                .get::<FocusStackData>()
                .unwrap()
                .borrow_mut(),
            |map| map.0.entry(idx).or_insert_with(|| IndexSet::new()),
        ))
    }
}

pub struct ActiveFocus(RefCell<Option<WlSurface>>);

impl ActiveFocus {
    fn set(seat: &Seat<State>, surface: Option<WlSurface>) {
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

    fn get(seat: &Seat<State>) -> Option<WlSurface> {
        seat.user_data()
            .get::<ActiveFocus>()
            .and_then(|a| a.0.borrow().clone())
    }
}

impl Shell {
    pub fn set_focus<'a>(
        state: &mut State,
        surface: Option<&WlSurface>,
        active_seat: &Seat<State>,
        serial: Option<Serial>,
    ) {
        // update FocusStack and notify layouts about new focus (if any window)
        if let Some(surface) = surface {
            if let Some(workspace) = state.common.shell.space_for_window_mut(surface) {
                if let Some(window) = workspace
                    .space
                    .window_for_surface(surface, WindowSurfaceType::ALL)
                {
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
                state,
                surface.cloned(),
                serial.unwrap_or_else(|| SERIAL_COUNTER.next_serial()),
            );
        }
    }

    fn update_active<'a>(&mut self, seats: impl Iterator<Item = &'a Seat<State>>) {
        // update activate status
        let focused_windows = seats
            .flat_map(|seat| {
                self.outputs
                    .iter()
                    .flat_map(|o| self.active_space(o).focus_stack(seat).last().clone())
            })
            .collect::<Vec<_>>();

        for output in self.outputs.iter() {
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
            for focused in focused_windows.iter() {
                workspace.space.raise_window(focused, true);
            }
            for window in workspace.space.windows() {
                window.set_activated(focused_windows.contains(window));
                window.configure();
            }
        }
    }
}

impl Common {
    pub fn set_focus(
        state: &mut State,
        surface: Option<&WlSurface>,
        active_seat: &Seat<State>,
        serial: Option<Serial>,
    ) {
        Shell::set_focus(state, surface, active_seat, serial);
        state.common.shell.update_active(state.common.seats.iter());
    }

    pub fn refresh_focus(state: &mut State) {
        let seats = state.common.seats.clone();
        for seat in seats {
            let output = active_output(&seat, &state.common);
            let last_known_focus = ActiveFocus::get(&seat);

            if let Some(surface) = last_known_focus {
                if surface.alive() {
                    let is_toplevel = matches!(get_role(&surface), Some(XDG_TOPLEVEL_ROLE));
                    let is_layer = matches!(get_role(&surface), Some(LAYER_SURFACE_ROLE));

                    if let Some(popup) = state.common.shell.popups.find_popup(&surface) {
                        if popup.alive() {
                            continue;
                        }
                    } else if is_layer {
                        if layer_map_for_output(&output)
                            .layer_for_surface(&surface, WindowSurfaceType::ALL)
                            .is_some()
                        {
                            continue; // Focus is valid
                        }
                    } else if is_toplevel {
                        let workspace = state.common.shell.active_space(&output);
                        if let Some(window) = workspace
                            .space
                            .window_for_surface(&surface, WindowSurfaceType::ALL)
                        {
                            let focus_stack = workspace.focus_stack(&seat);
                            if !focus_stack.last().map(|w| &w != window).unwrap_or(true) {
                                continue; // Focus is valid
                            } else {
                                slog_scope::debug!("Wrong Window, focus fixup");
                            }
                        } else {
                            slog_scope::debug!("Different workspaces Window, focus fixup");
                        }
                    } else {
                        // unknown surface type, fixup
                        slog_scope::debug!("Surface unmapped, focus fixup");
                    }
                } else {
                    slog_scope::debug!("Surface dead, focus fixup");
                }
            } else {
                continue;
            }

            // fixup focus
            {
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
                let surface = state
                    .common
                    .shell
                    .active_space(&output)
                    .focus_stack(&seat)
                    .last()
                    .map(|w| w.toplevel().wl_surface().clone());
                if let Some(keyboard) = seat.get_keyboard() {
                    slog_scope::info!("restoring focus to: {:?}", surface.as_ref());
                    keyboard.set_focus(state, surface.clone(), SERIAL_COUNTER.next_serial());
                    ActiveFocus::set(&seat, surface);
                }
            }
        }

        state.common.shell.update_active(state.common.seats.iter())
    }
}
