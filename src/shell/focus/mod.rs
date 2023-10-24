use crate::{
    shell::{element::CosmicMapped, Shell, Workspace},
    state::Common,
    utils::prelude::*,
    wayland::handlers::xdg_shell::PopupGrabData,
    xwayland::XWaylandState,
};
use indexmap::IndexSet;
use smithay::{
    desktop::{layer_map_for_output, PopupUngrabStrategy},
    input::Seat,
    utils::{IsAlive, Serial, SERIAL_COUNTER},
    wayland::seat::WaylandFocus,
};
use std::cell::RefCell;
use tracing::{debug, trace};

use self::target::{KeyboardFocusTarget, WindowGroup};

use super::{layout::floating::FloatingLayout, CosmicSurface};

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
        let element = match target {
            Some(KeyboardFocusTarget::Element(mapped)) => Some(mapped.clone()),
            Some(KeyboardFocusTarget::Fullscreen(window)) => {
                state.common.shell.element_for_surface(window).cloned()
            }
            _ => None,
        };

        if let Some(mapped) = element {
            if let Some(workspace) = state.common.shell.space_for_mut(&mapped) {
                let mut focus_stack = workspace.focus_stack.get_mut(active_seat);
                if Some(&mapped) != focus_stack.last() {
                    trace!(?mapped, "Focusing window.");
                    focus_stack.append(&mapped);
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
                if matches!(
                    seat.get_keyboard().unwrap().current_focus(),
                    Some(KeyboardFocusTarget::Group(_))
                ) {
                    return None;
                }

                Some(self.outputs().flat_map(|o| {
                    let space = self.active_space(o);
                    let stack = space.focus_stack.get(seat);
                    stack.last().cloned()
                }))
            })
            .flatten()
            .collect::<Vec<_>>();

        for output in self.outputs().cloned().collect::<Vec<_>>().into_iter() {
            // TODO: Add self.workspaces.active_workspaces()
            let workspace = self.workspaces.active_mut(&output);
            for focused in focused_windows.iter() {
                if let CosmicSurface::X11(window) = focused.active_window() {
                    if let Some(xwm) = xwm.as_mut().and_then(|state| state.xwm.as_mut()) {
                        let _ = xwm.raise_window(&window);
                    }
                }
                raise_with_children(&mut workspace.floating_layer, focused);
            }
            for window in workspace.mapped() {
                window.set_activated(focused_windows.contains(&window));
                window.configure();
            }
        }
    }
}

fn raise_with_children(floating_layer: &mut FloatingLayout, focused: &CosmicMapped) {
    if floating_layer.mapped().any(|m| m == focused) {
        floating_layer.space.raise_element(focused, true);
        for element in floating_layer
            .space
            .elements()
            .filter(|elem| {
                let parent = match elem.active_window() {
                    CosmicSurface::Wayland(w) => w.toplevel().parent(),
                    _ => None,
                };

                parent == focused.active_window().wl_surface()
            })
            .cloned()
            .collect::<Vec<_>>()
            .into_iter()
        {
            raise_with_children(floating_layer, &element);
        }
    }
}

impl Common {
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
            if !state.common.shell.outputs().any(|o| o == &output) {
                seat.set_active_output(&state.common.shell.outputs().next().unwrap());
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
                                && workspace.get_fullscreen().is_none()
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
                        KeyboardFocusTarget::Group(WindowGroup { node, .. }) => {
                            if state
                                .common
                                .shell
                                .workspaces
                                .active(&output)
                                .1
                                .tiling_layer
                                .has_node(&node)
                            {
                                continue; // Focus is valid,
                            }
                        }
                        KeyboardFocusTarget::Fullscreen(window) => {
                            let workspace = state.common.shell.active_space(&output);
                            let focus_stack = workspace.focus_stack.get(&seat);

                            if focus_stack
                                .last()
                                .map(|m| m.has_active_window(&window))
                                .unwrap_or(false)
                                && workspace.get_fullscreen().is_some()
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
                    if let KeyboardFocusTarget::Popup(_) = target {
                        if let Some(popup_grab) = seat
                            .user_data()
                            .get::<PopupGrabData>()
                            .and_then(|x| x.take())
                        {
                            if !popup_grab.has_ended() {
                                if let Some(new) = popup_grab.current_grab() {
                                    trace!("restore focus to previous popup grab");
                                    if let Some(keyboard) = seat.get_keyboard() {
                                        keyboard.set_focus(
                                            state,
                                            Some(new.clone()),
                                            SERIAL_COUNTER.next_serial(),
                                        );
                                    }
                                    ActiveFocus::set(&seat, Some(new));
                                    seat.user_data()
                                        .get_or_insert::<PopupGrabData, _>(PopupGrabData::default)
                                        .set(Some(popup_grab));
                                    continue;
                                }
                            }
                        }
                    }
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
                    .get_fullscreen()
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
