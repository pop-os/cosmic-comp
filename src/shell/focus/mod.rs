use crate::{
    shell::{element::CosmicMapped, Shell},
    state::Common,
    utils::prelude::*,
    wayland::handlers::xdg_shell::PopupGrabData,
};
use indexmap::IndexSet;
use smithay::{
    desktop::{layer_map_for_output, PopupUngrabStrategy},
    input::{pointer::MotionEvent, Seat},
    output::Output,
    reexports::wayland_server::Resource,
    utils::{IsAlive, Point, Serial, SERIAL_COUNTER},
    wayland::{
        seat::WaylandFocus,
        selection::{data_device::set_data_device_focus, primary_selection::set_primary_focus},
        shell::wlr_layer::{KeyboardInteractivity, Layer},
    },
};
use std::{borrow::Cow, mem, sync::Mutex};

use tracing::{debug, trace};

pub use self::order::{render_input_order, Stage};
use self::target::{KeyboardFocusTarget, WindowGroup};

use super::{grabs::SeatMoveGrabState, layout::floating::FloatingLayout, SeatExt};

mod order;
pub mod target;

pub struct FocusStack<'a>(pub(super) Option<&'a IndexSet<CosmicMapped>>);
pub struct FocusStackMut<'a>(pub(super) &'a mut IndexSet<CosmicMapped>);

impl<'a> FocusStack<'a> {
    /// returns the last unminimized window in the focus stack that is still alive
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
    /// Set the keyboard focus to the given target
    /// Note: `update_cursor` is used to determine whether to update the pointer location if cursor_follows_focus is enabled
    /// if the focus change was due to a pointer event, this should be set to false
    pub fn set_focus(
        state: &mut State,
        target: Option<&KeyboardFocusTarget>,
        seat: &Seat<State>,
        serial: Option<Serial>,
        update_cursor: bool,
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

        update_focus_state(seat, target, state, serial, update_cursor);

        state.common.shell.write().unwrap().update_active();
    }

    pub fn append_focus_stack(&mut self, mapped: &CosmicMapped, seat: &Seat<State>) {
        if mapped.is_minimized() {
            return;
        }

        // update FocusStack and notify layouts about new focus (if any window)
        let workspace = match self.space_for_mut(mapped) {
            Some(workspace) => workspace,
            None => {
                //should this be the active output or the focused output?
                self.active_space_mut(&seat.focused_or_active_output())
            }
        };

        let mut focus_stack = workspace.focus_stack.get_mut(seat);
        if Some(mapped) != focus_stack.last() {
            trace!(?mapped, "Focusing window.");
            focus_stack.append(mapped);
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

    fn update_active(&mut self) {
        // update activate status
        let focused_windows = self
            .seats
            .iter()
            .filter_map(|seat| {
                if matches!(
                    seat.get_keyboard().unwrap().current_focus(),
                    Some(KeyboardFocusTarget::Group(_))
                ) {
                    return None;
                }

                let output = seat.focused_or_active_output();
                let space = self.active_space(&output);
                let stack = space.focus_stack.get(seat);
                stack.last().cloned()
            })
            .collect::<Vec<_>>();

        for output in self.outputs().cloned().collect::<Vec<_>>().into_iter() {
            let set = self.workspaces.sets.get_mut(&output).unwrap();
            for focused in focused_windows.iter() {
                raise_with_children(&mut set.sticky_layer, focused);
            }
            for window in set.sticky_layer.mapped() {
                window.set_activated(focused_windows.contains(window));
                window.configure();
            }
            for m in set.minimized_windows.iter() {
                m.window.set_activated(false);
                m.window.configure();
            }

            let workspace = &mut set.workspaces[set.active];
            for focused in focused_windows.iter() {
                raise_with_children(&mut workspace.floating_layer, focused);
            }
            for window in workspace.mapped() {
                window.set_activated(focused_windows.contains(window));
                window.configure();
            }
            for m in workspace.minimized_windows.iter() {
                m.window.set_activated(false);
                m.window.configure();
            }

            for (i, workspace) in set.workspaces.iter().enumerate() {
                if i == set.active {
                    continue;
                }
                for window in workspace.mapped() {
                    window.set_activated(false);
                    window.configure();
                }
            }
        }
    }
}

/// Internal, used to ensure that ActiveFocus, KeyboardFocusTarget, and FocusedOutput are all in sync
fn update_focus_state(
    seat: &Seat<State>,
    target: Option<&KeyboardFocusTarget>,
    state: &mut State,
    serial: Option<Serial>,
    should_update_cursor: bool,
) {
    // update keyboard focus
    if let Some(keyboard) = seat.get_keyboard() {
        if should_update_cursor
            && state.common.config.cosmic_conf.cursor_follows_focus
            && target.is_some()
        {
            //need to borrow mutably for surface under
            let shell = state.common.shell.read().unwrap();
            // get the top left corner of the target element
            let geometry = shell.focused_geometry(target.unwrap());
            if let Some(geometry) = geometry {
                // get the center of the target element
                let window_center = Point::from((geometry.size.w / 2, geometry.size.h / 2));
                let new_pos = (geometry.loc + window_center).to_f64();

                // create a pointer target from the target element
                let output = shell
                    .outputs()
                    .find(|output| output.geometry().to_f64().contains(new_pos))
                    .cloned()
                    .unwrap_or(seat.active_output());

                let focus = State::surface_under(new_pos, &output, &shell)
                    .map(|(focus, loc)| (focus, loc.as_logical()));
                //drop here to avoid multiple borrows
                mem::drop(shell);
                seat.get_pointer().unwrap().motion(
                    state,
                    focus,
                    &MotionEvent {
                        location: new_pos.as_logical(),
                        serial: SERIAL_COUNTER.next_serial(),
                        time: 0,
                    },
                );
            }
        }

        ActiveFocus::set(seat, target.cloned());
        keyboard.set_focus(
            state,
            target.cloned(),
            serial.unwrap_or_else(|| SERIAL_COUNTER.next_serial()),
        );
        std::mem::drop(keyboard);

        //update the focused output or set it to the active output
        if target.is_some() {
            // Get focused output calls visible_output_for_surface internally
            // what should happen if the target is some, but it's not visible?
            // should this be an error?
            seat.set_focused_output(
                state
                    .common
                    .shell
                    .read()
                    .unwrap()
                    .get_output_for_focus(seat)
                    .as_ref(),
            )
        } else {
            seat.set_focused_output(None);
        };
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
            update_pointer_focus(state, seat);

            let mut shell = state.common.shell.write().unwrap();
            let output = seat.focused_or_active_output();

            // If the focused or active output is not in the list of outputs, switch to the first output
            if !shell.outputs().any(|o| o == &output) {
                if let Some(other) = shell.outputs().next() {
                    seat.set_active_output(other);
                }
                continue;
            }

            let last_known_focus = ActiveFocus::get(seat);

            if let Some(target) = last_known_focus {
                if target.alive() {
                    if focus_target_is_valid(&mut shell, seat, &output, target) {
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
                                    // TODO: verify whether cursor should be updated at end of popup grab
                                    update_focus_state(
                                        seat,
                                        Some(&new),
                                        state,
                                        Some(SERIAL_COUNTER.next_serial()),
                                        false,
                                    );
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
                let focus_stack = workspace.focus_stack.get(seat);

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
                let target = update_focus_target(&shell, seat, &output);
                std::mem::drop(shell);
                //I can probably feature gate this condition
                debug!("Restoring focus to {:?}", target.as_ref());

                update_focus_state(seat, target.as_ref(), state, None, false);
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
                set_data_device_focus(&state.common.display_handle, seat, client.clone());
                set_primary_focus(&state.common.display_handle, seat, client);
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
            if seat
                .user_data()
                .get::<SeatMoveGrabState>()
                .is_some_and(|state| {
                    state
                        .lock()
                        .unwrap()
                        .as_ref()
                        .is_some_and(|state| state.element() == mapped)
                })
            {
                return true;
            }

            let is_sticky = shell
                .workspaces
                .sets
                .get(output)
                .unwrap()
                .sticky_layer
                .mapped()
                .any(|m| m == &mapped);

            let workspace = shell.active_space(output);
            let focus_stack = workspace.focus_stack.get(seat);
            let is_in_focus_stack = focus_stack.last().map(|m| m == &mapped).unwrap_or(false);
            let has_fullscreen = workspace.get_fullscreen().is_some();

            if is_sticky && !is_in_focus_stack {
                shell.append_focus_stack(&mapped, seat);
            }

            (is_sticky || is_in_focus_stack) && !has_fullscreen
        }
        KeyboardFocusTarget::LayerSurface(layer) => {
            layer_map_for_output(output).layers().any(|l| l == &layer)
        }
        KeyboardFocusTarget::Group(WindowGroup { node, .. }) => shell
            .workspaces
            .active(output)
            .1
            .tiling_layer
            .has_node(&node),
        KeyboardFocusTarget::Fullscreen(window) => {
            let workspace = shell.active_space(output);
            let focus_stack = workspace.focus_stack.get(seat);

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
    } else if let Some(surface) = shell.active_space(output).get_fullscreen() {
        Some(KeyboardFocusTarget::Fullscreen(surface.clone()))
    } else {
        shell
            .active_space(output)
            .focus_stack
            .get(seat)
            .last()
            .or_else(|| shell.active_space(output).mapped().next())
            .cloned()
            .map(KeyboardFocusTarget::from)
    }
}

fn update_pointer_focus(state: &mut State, seat: &Seat<State>) {
    if let Some(pointer) = seat.get_pointer() {
        let output = seat.active_output();
        let position = pointer.current_location().as_global();

        let shell = state.common.shell.write().unwrap();
        let under = State::surface_under(position, &output, &shell)
            .map(|(target, pos)| (target, pos.as_logical()));
        drop(shell);

        if pointer.current_focus().as_ref() != under.as_ref().map(|(target, _)| target) {
            pointer.motion(
                state,
                under,
                &MotionEvent {
                    location: pointer.current_location(),
                    serial: SERIAL_COUNTER.next_serial(),
                    time: state.common.clock.now().as_millis(),
                },
            );
        }
    }
}

// Get the top-most layer, if any, with at least one surface with exclusive keyboard interactivity.
// Only considers surface in `Top` or `Overlay` layer.
fn exclusive_layer_surface_layer(shell: &Shell) -> Option<Layer> {
    let mut layer = None;
    for output in shell.outputs() {
        for layer_surface in layer_map_for_output(output).layers() {
            let data = layer_surface.cached_state();
            if data.keyboard_interactivity == KeyboardInteractivity::Exclusive
                && data.layer as u32 >= layer.unwrap_or(Layer::Top) as u32
            {
                layer = Some(data.layer);
            }
        }
    }
    layer
}
