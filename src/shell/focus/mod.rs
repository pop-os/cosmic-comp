use crate::{
    shell::{element::CosmicMapped, Shell, Workspace},
    state::{Common, SelectionSources},
    utils::prelude::*,
    wayland::handlers::xdg_shell::PopupGrabData,
    xwayland::XWaylandState,
};
use indexmap::IndexSet;
use smithay::{
    desktop::{layer_map_for_output, PopupUngrabStrategy},
    input::Seat,
    utils::{IsAlive, Serial, SERIAL_COUNTER},
};
use std::cell::{RefCell, RefMut};
use tracing::{debug, trace};

use self::target::{KeyboardFocusTarget, WindowGroup};

use super::CosmicSurface;

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

pub struct FocusStack<'a>(pub(super) Option<&'a IndexSet<CosmicMapped>>);
pub struct FocusStackMut<'a>(pub(super) &'a mut IndexSet<CosmicMapped>);

impl<'a> FocusStack<'a> {
    pub fn last(&self) -> Option<&CosmicMapped> {
        self.0
            .as_ref()
            .and_then(|set| set.iter().rev().find(|w| w.alive()))
    }

    pub fn iter(&self) -> impl Iterator<Item = &'_ CosmicMapped> {
        self.0
            .iter()
            .flat_map(|set| set.iter().rev().filter(|w| w.alive()))
    }
}

impl<'a> FocusStackMut<'a> {
    pub fn append(&mut self, window: &CosmicMapped) {
        self.0.retain(|w| w.alive());
        self.0.shift_remove(window);
        self.0.insert(window.clone());
    }

    pub fn last(&self) -> Option<&CosmicMapped> {
        self.0.iter().rev().find(|w| w.alive())
    }

    pub fn iter(&self) -> impl Iterator<Item = &'_ CosmicMapped> {
        self.0.iter().rev().filter(|w| w.alive())
    }
}

impl Workspace {}

pub struct ActiveFocus(RefCell<Option<KeyboardFocusTarget>>);

impl ActiveFocus {
    fn set(seat: &Seat<State>, target: Option<KeyboardFocusTarget>) {
        if !seat
            .user_data()
            .insert_if_missing(|| ActiveFocus(RefCell::new(target.clone())))
        {
            *seat
                .user_data()
                .get::<ActiveFocus>()
                .unwrap()
                .0
                .borrow_mut() = target;
        }
    }

    fn get(seat: &Seat<State>) -> Option<KeyboardFocusTarget> {
        seat.user_data()
            .get::<ActiveFocus>()
            .and_then(|a| a.0.borrow().clone())
    }
}

impl Shell {
    pub fn set_focus<'a>(
        state: &mut State,
        target: Option<&KeyboardFocusTarget>,
        active_seat: &Seat<State>,
        serial: Option<Serial>,
    ) {
        // update FocusStack and notify layouts about new focus (if any window)
        if let Some(KeyboardFocusTarget::Element(mapped)) = target {
            if let Some(workspace) = state.common.shell.space_for_mut(mapped) {
                let mut focus_stack = workspace.focus_stack.get_mut(active_seat);
                if Some(mapped) != focus_stack.last() {
                    trace!(?mapped, "Focusing window.");
                    focus_stack.append(mapped);
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

        // update keyboard focus
        if let Some(keyboard) = active_seat.get_keyboard() {
            ActiveFocus::set(active_seat, target.cloned());
            keyboard.set_focus(
                state,
                target.cloned(),
                serial.unwrap_or_else(|| SERIAL_COUNTER.next_serial()),
            );
        }
    }

    fn update_active<'a, 'b>(
        &mut self,
        seats: impl Iterator<Item = &'a Seat<State>>,
        mut xwm: Option<&'b mut XWaylandState>,
    ) {
        // update activate status
        let focused_windows = seats
            .flat_map(|seat| {
                self.outputs.iter().flat_map(|o| {
                    let space = self.active_space(o);
                    let stack = space.focus_stack.get(seat);
                    stack.last().cloned()
                })
            })
            .collect::<Vec<_>>();

        for output in self.outputs.iter() {
            let workspace = self.workspaces.active_mut(output);
            for focused in focused_windows.iter() {
                if workspace.floating_layer.mapped().any(|m| m == focused) {
                    if let CosmicSurface::X11(window) = focused.active_window() {
                        if let Some(xwm) = xwm.as_mut().and_then(|state| state.xwm.as_mut()) {
                            let _ = xwm.raise_window(&window);
                        }
                    }
                    workspace.floating_layer.space.raise_element(focused, true);
                }
            }
            for window in workspace.mapped() {
                window.set_activated(focused_windows.contains(&window));
                window.configure();
            }
        }
    }
}

impl Common {
    fn selection_sources_inner(seat: &Seat<State>) -> &RefCell<SelectionSources> {
        seat.user_data()
            .insert_if_missing(|| RefCell::new(SelectionSources::default()));
        seat.user_data().get::<RefCell<SelectionSources>>().unwrap()
    }

    pub fn selection_sources(seat: &Seat<State>) -> SelectionSources {
        *Self::selection_sources_inner(seat).borrow()
    }

    pub fn update_selection_sources(
        seat: &Seat<State>,
        f: impl FnOnce(RefMut<'_, SelectionSources>),
    ) {
        let sources = Self::selection_sources_inner(seat).borrow_mut();
        f(sources)
    }

    pub fn set_focus(
        state: &mut State,
        target: Option<&KeyboardFocusTarget>,
        active_seat: &Seat<State>,
        serial: Option<Serial>,
    ) {
        Shell::set_focus(state, target, active_seat, serial);
        let seats = state.common.seats().cloned().collect::<Vec<_>>();
        state
            .common
            .shell
            .update_active(seats.iter(), state.common.xwayland_state.as_mut());
    }

    pub fn refresh_focus(state: &mut State) {
        let seats = state.common.seats().cloned().collect::<Vec<_>>();
        for seat in seats {
            let output = seat.active_output();
            if !state.common.shell.outputs.contains(&output) {
                seat.set_active_output(&state.common.shell.outputs[0]);
                continue;
            }
            let last_known_focus = ActiveFocus::get(&seat);

            if let Some(target) = last_known_focus {
                if target.alive() {
                    match target {
                        KeyboardFocusTarget::Element(mapped) => {
                            let workspace = state.common.shell.active_space(&output);
                            let focus_stack = workspace.focus_stack.get(&seat);
                            if focus_stack.last().map(|m| m == &mapped).unwrap_or(false)
                                && workspace.get_fullscreen(&output).is_none()
                            {
                                continue; // Focus is valid
                            } else {
                                trace!("Wrong Window, focus fixup");
                            }
                        }
                        KeyboardFocusTarget::LayerSurface(layer) => {
                            if layer_map_for_output(&output).layers().any(|l| l == &layer) {
                                continue; // Focus is valid
                            }
                        }
                        KeyboardFocusTarget::Group(WindowGroup {
                            output: weak_output,
                            ..
                        }) => {
                            if weak_output == output {
                                continue; // Focus is valid,
                            }
                        }
                        KeyboardFocusTarget::Fullscreen(surface) => {
                            let workspace = state.common.shell.active_space(&output);
                            let focus_stack = workspace.focus_stack.get(&seat);

                            if focus_stack
                                .last()
                                .map(|m| m.has_active_window(&surface))
                                .unwrap_or(false)
                                && workspace.get_fullscreen(&output).is_some()
                            {
                                continue; // Focus is valid
                            } else {
                                trace!("Wrong Window, focus fixup");
                            }
                        }
                        KeyboardFocusTarget::Popup(_) => {
                            continue; // Focus is valid
                        }
                    };
                } else {
                    trace!("Surface dead, focus fixup");
                }
            } else {
                let workspace = state.common.shell.active_space(&output);
                let focus_stack = workspace.focus_stack.get(&seat);

                if focus_stack.last().is_none() {
                    continue; // Focus is valid
                } else {
                    trace!("No previous window, focus fixup");
                }
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
                let target = state
                    .common
                    .shell
                    .active_space(&output)
                    .get_fullscreen(&output)
                    .cloned()
                    .map(KeyboardFocusTarget::Fullscreen)
                    .or_else(|| {
                        state
                            .common
                            .shell
                            .active_space(&output)
                            .focus_stack
                            .get(&seat)
                            .last()
                            .cloned()
                            .map(KeyboardFocusTarget::from)
                    });
                if let Some(keyboard) = seat.get_keyboard() {
                    debug!("Restoring focus to {:?}", target.as_ref());
                    keyboard.set_focus(state, target.clone(), SERIAL_COUNTER.next_serial());
                    ActiveFocus::set(&seat, target);
                }
            }
        }

        let seats = state.common.seats().cloned().collect::<Vec<_>>();
        state
            .common
            .shell
            .update_active(seats.iter(), state.common.xwayland_state.as_mut())
    }
}
