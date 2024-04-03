use crate::{
    shell::{element::CosmicMapped, Shell},
    state::Common,
    utils::prelude::*,
    wayland::handlers::xdg_shell::PopupGrabData,
};
use indexmap::IndexSet;
use smithay::{
    desktop::{layer_map_for_output, PopupUngrabStrategy},
    input::Seat,
    output::Output,
    reexports::wayland_server::Resource,
    utils::{IsAlive, Serial, SERIAL_COUNTER},
    wayland::{
        seat::WaylandFocus,
        selection::data_device::set_data_device_focus,
        selection::primary_selection::set_primary_focus,
        shell::wlr_layer::{KeyboardInteractivity, Layer},
    },
};
use std::{borrow::Cow, sync::Mutex};
use tracing::{debug, trace};

use self::target::{KeyboardFocusTarget, WindowGroup};

use super::{layout::floating::FloatingLayout, SeatExt};

pub mod target;

pub struct FocusStack<'a>(pub(super) Option<&'a IndexSet<CosmicMapped>>);
pub struct FocusStackMut<'a>(pub(super) &'a mut IndexSet<CosmicMapped>);

impl<'a> FocusStack<'a> {
    pub fn last(&self) -> Option<&CosmicMapped> {
        self.0
            .as_ref()
            .and_then(|set| set.iter().rev().find(|w| w.alive() && !w.is_minimized()))
    }

    pub fn iter(&self) -> impl Iterator<Item = &'_ CosmicMapped> {
        self.0
            .iter()
            .flat_map(|set| set.iter().rev().filter(|w| w.alive() && !w.is_minimized()))
    }
}

impl<'a> FocusStackMut<'a> {
    pub fn append(&mut self, window: &CosmicMapped) {
        self.0.retain(|w| w.alive());
        self.0.shift_remove(window);
        self.0.insert(window.clone());
    }

    pub fn remove(&mut self, window: &CosmicMapped) {
        self.0.retain(|w| w != window);
    }

    pub fn last(&self) -> Option<&CosmicMapped> {
        self.0.iter().rev().find(|w| w.alive() && !w.is_minimized())
    }

    pub fn iter(&self) -> impl Iterator<Item = &'_ CosmicMapped> {
        self.0
            .iter()
            .rev()
            .filter(|w| w.alive() && !w.is_minimized())
    }
}

pub struct ActiveFocus(Mutex<Option<KeyboardFocusTarget>>);

impl ActiveFocus {
    fn set(seat: &Seat<State>, target: Option<KeyboardFocusTarget>) {
        if !seat
            .user_data()
            .insert_if_missing_threadsafe(|| ActiveFocus(Mutex::new(target.clone())))
        {
            *seat
                .user_data()
                .get::<ActiveFocus>()
                .unwrap()
                .0
                .lock()
                .unwrap() = target;
        }
    }

    fn get(seat: &Seat<State>) -> Option<KeyboardFocusTarget> {
        seat.user_data()
            .get::<ActiveFocus>()
            .and_then(|a| a.0.lock().unwrap().clone())
    }
}

impl Shell {
    pub fn set_focus(
        state: &mut State,
        target: Option<&KeyboardFocusTarget>,
        seat: &Seat<State>,
        serial: Option<Serial>,
    ) {
        let element = match target {
            Some(KeyboardFocusTarget::Element(mapped)) => Some(mapped.clone()),
            Some(KeyboardFocusTarget::Fullscreen(window)) => state
                .common
                .shell
                .read()
                .unwrap()
                .element_for_surface(window)
                .cloned(),
            _ => None,
        };

        if let Some(mapped) = element {
            if mapped.is_minimized() {
                return;
            }
            state
                .common
                .shell
                .write()
                .unwrap()
                .append_focus_stack(&mapped, seat);
        }

        // update keyboard focus
        if let Some(keyboard) = seat.get_keyboard() {
            ActiveFocus::set(seat, target.cloned());
            keyboard.set_focus(
                state,
                target.cloned(),
                serial.unwrap_or_else(|| SERIAL_COUNTER.next_serial()),
            );
        }

        state.common.shell.write().unwrap().update_active();
    }

    pub fn append_focus_stack(&mut self, mapped: &CosmicMapped, seat: &Seat<State>) {
        if mapped.is_minimized() {
            return;
        }

        // update FocusStack and notify layouts about new focus (if any window)
        let workspace = self.space_for_mut(&mapped);
        let workspace = if workspace.is_none() {
            self.active_space_mut(&seat.active_output())
        } else {
            workspace.unwrap()
        };

        let mut focus_stack = workspace.focus_stack.get_mut(seat);
        if Some(mapped) != focus_stack.last() {
            trace!(?mapped, "Focusing window.");
            focus_stack.append(&mapped);
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
        }
    }

    fn update_active<'a, 'b>(&mut self) {
        // update activate status
        let focused_windows = self
            .seats
            .iter()
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
            let set = self.workspaces.sets.get_mut(&output).unwrap();
            for focused in focused_windows.iter() {
                raise_with_children(&mut set.sticky_layer, focused);
            }
            for window in set.sticky_layer.mapped() {
                window.set_activated(focused_windows.contains(&window));
                window.configure();
            }

            let workspace = self.workspaces.active_mut(&output);
            for focused in focused_windows.iter() {
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
            .filter(|elem| elem != &focused)
            .filter(|elem| {
                let parent = elem
                    .active_window()
                    .0
                    .toplevel()
                    .and_then(|toplevel| toplevel.parent());
                parent.is_some_and(|parent| {
                    focused
                        .active_window()
                        .wl_surface()
                        .map(Cow::into_owned)
                        .map(|focused| parent == focused)
                        .unwrap_or(false)
                })
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
    pub fn refresh_focus(state: &mut State) {
        let seats = state
            .common
            .shell
            .read()
            .unwrap()
            .seats
            .iter()
            .cloned()
            .collect::<Vec<_>>();
        for seat in &seats {
            let mut shell = state.common.shell.write().unwrap();
            let output = seat.active_output();
            if !shell.outputs().any(|o| o == &output) {
                if let Some(other) = shell.outputs().next() {
                    seat.set_active_output(other);
                }
                continue;
            }
            let last_known_focus = ActiveFocus::get(&seat);

            if let Some(target) = last_known_focus {
                if target.alive() {
                    if focus_target_is_valid(&mut *shell, &seat, &output, target) {
                        continue; // Focus is valid
                    } else {
                        trace!("Wrong Window, focus fixup");
                    }
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
                                    std::mem::drop(shell);

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
                let workspace = shell.active_space(&output);
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
                let target = update_focus_target(&*shell, &seat, &output);
                std::mem::drop(shell);

                if let Some(keyboard) = seat.get_keyboard() {
                    debug!("Restoring focus to {:?}", target.as_ref());
                    keyboard.set_focus(state, target.clone(), SERIAL_COUNTER.next_serial());
                    ActiveFocus::set(&seat, target);
                }
            }
        }

        // Update clipboard and primary focus
        //
        // For now, needs to be here instead of in `focus_changed` to update focus
        // when the active element of a stack changes.
        for seat in &seats {
            if let Some(keyboard) = seat.get_keyboard() {
                let focus = keyboard.current_focus();
                let client = focus
                    .as_ref()
                    .and_then(|t| t.wl_surface())
                    .and_then(|s| state.common.display_handle.get_client(s.id()).ok());
                set_data_device_focus(&state.common.display_handle, &seat, client.clone());
                set_primary_focus(&state.common.display_handle, &seat, client);
            }
        }

        state.common.shell.write().unwrap().update_active()
    }
}

fn focus_target_is_valid(
    shell: &mut Shell,
    seat: &Seat<State>,
    output: &Output,
    target: KeyboardFocusTarget,
) -> bool {
    // If a session lock is active, only lock surfaces can be focused
    if shell.session_lock.is_some() {
        return matches!(target, KeyboardFocusTarget::LockSurface(_));
    }

    // If an exclusive layer shell surface exists (on any output), only exclusive
    // shell surfaces can have focus, on the highest layer with exclusive surfaces.
    if let Some(layer) = exclusive_layer_surface_layer(shell) {
        return if let KeyboardFocusTarget::LayerSurface(layer_surface) = target {
            let data = layer_surface.cached_state();
            (data.keyboard_interactivity, data.layer) == (KeyboardInteractivity::Exclusive, layer)
        } else {
            false
        };
    }

    match target {
        KeyboardFocusTarget::Element(mapped) => {
            let is_sticky = shell
                .workspaces
                .sets
                .get(output)
                .unwrap()
                .sticky_layer
                .mapped()
                .any(|m| m == &mapped);

            let workspace = shell.active_space(&output);
            let focus_stack = workspace.focus_stack.get(&seat);
            let is_in_focus_stack = focus_stack.last().map(|m| m == &mapped).unwrap_or(false);
            let has_fullscreen = workspace.get_fullscreen().is_some();

            if is_sticky && !is_in_focus_stack {
                shell.append_focus_stack(&mapped, seat);
            }

            (is_sticky || is_in_focus_stack) && !has_fullscreen
        }
        KeyboardFocusTarget::LayerSurface(layer) => {
            layer_map_for_output(&output).layers().any(|l| l == &layer)
        }
        KeyboardFocusTarget::Group(WindowGroup { node, .. }) => shell
            .workspaces
            .active(&output)
            .1
            .tiling_layer
            .has_node(&node),
        KeyboardFocusTarget::Fullscreen(window) => {
            let workspace = shell.active_space(&output);
            let focus_stack = workspace.focus_stack.get(&seat);

            focus_stack
                .last()
                .map(|m| m.has_active_window(&window))
                .unwrap_or(false)
                && workspace.get_fullscreen().is_some()
        }
        KeyboardFocusTarget::Popup(_) => true,
        KeyboardFocusTarget::LockSurface(_) => false,
    }
}

fn update_focus_target(
    shell: &Shell,
    seat: &Seat<State>,
    output: &Output,
) -> Option<KeyboardFocusTarget> {
    if let Some(session_lock) = &shell.session_lock {
        session_lock
            .surfaces
            .get(output)
            .cloned()
            .map(KeyboardFocusTarget::from)
    } else if let Some(layer) = exclusive_layer_surface_layer(shell) {
        layer_map_for_output(output)
            .layers()
            .find(|layer_surface| {
                let data = layer_surface.cached_state();
                (data.keyboard_interactivity, data.layer)
                    == (KeyboardInteractivity::Exclusive, layer)
            })
            .cloned()
            .map(KeyboardFocusTarget::from)
    } else if let Some(surface) = shell.active_space(&output).get_fullscreen() {
        Some(KeyboardFocusTarget::Fullscreen(surface.clone()))
    } else {
        shell
            .active_space(&output)
            .focus_stack
            .get(&seat)
            .last()
            .cloned()
            .map(KeyboardFocusTarget::from)
    }
}

// Get the top-most layer, if any, with at least one surface with exclusive keyboard interactivity.
// Only considers surface in `Top` or `Overlay` layer.
fn exclusive_layer_surface_layer(shell: &Shell) -> Option<Layer> {
    let mut layer = None;
    for output in shell.outputs() {
        for layer_surface in layer_map_for_output(output).layers() {
            let data = layer_surface.cached_state();
            if data.keyboard_interactivity == KeyboardInteractivity::Exclusive {
                if data.layer as u32 >= layer.unwrap_or(Layer::Top) as u32 {
                    layer = Some(data.layer);
                }
            }
        }
    }
    layer
}
