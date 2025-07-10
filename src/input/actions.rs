// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    config::{Action, PrivateAction},
    shell::{
        focus::{target::KeyboardFocusTarget, FocusTarget},
        layout::tiling::SwapWindowGrab,
        FocusResult, InvalidWorkspaceIndex, MoveResult, SeatExt, Trigger, WorkspaceDelta,
    },
    utils::prelude::*,
    wayland::{
        handlers::xdg_activation::ActivationContext, protocols::workspace::WorkspaceUpdateGuard,
    },
};
use cosmic_comp_config::{workspace::WorkspaceLayout, TileBehavior};
use cosmic_config::ConfigSet;
use cosmic_settings_config::shortcuts;
use cosmic_settings_config::shortcuts::action::{Direction, FocusDirection};
use smithay::{
    input::{pointer::MotionEvent, Seat},
    utils::{Point, Serial},
};
#[cfg(not(feature = "debug"))]
use tracing::info;
use tracing::{error, warn};

use std::{os::unix::process::CommandExt, thread};

use super::gestures;

fn propagate_by_default(action: &shortcuts::Action) -> bool {
    match action {
        shortcuts::Action::Focus(_) | shortcuts::Action::Move(_) => true,
        _ => false,
    }
}

impl State {
    pub fn handle_action(
        &mut self,
        action: Action,
        seat: &Seat<State>,
        serial: Serial,
        time: u32,
        pattern: shortcuts::Binding,
        direction: Option<Direction>,
    ) {
        // TODO: Detect if started from login manager or tty, and only allow
        // `Terminate` if it will return to login manager.
        if self.common.shell.read().session_lock.is_some()
            && !matches!(
                action,
                Action::Shortcut(shortcuts::Action::Terminate)
                    | Action::Shortcut(shortcuts::Action::Debug)
            )
        {
            return;
        }

        match action {
            Action::Shortcut(action) => {
                let propagate = propagate_by_default(&action);
                self.handle_shortcut_action(
                    action, seat, serial, time, pattern, direction, propagate,
                )
            }
            Action::Private(PrivateAction::Escape) => {
                {
                    let mut shell = self.common.shell.write();
                    shell.set_overview_mode(None, self.common.event_loop_handle.clone());
                    shell.set_resize_mode(
                        None,
                        &self.common.config,
                        self.common.event_loop_handle.clone(),
                    );
                }
                let pointer = seat.get_pointer().unwrap();
                let keyboard = seat.get_keyboard().unwrap();
                if pointer.is_grabbed() {
                    pointer.unset_grab(self, serial, time);
                }
                if keyboard.is_grabbed() {
                    keyboard.unset_grab(self);
                }
            }

            Action::Private(PrivateAction::Resizing(direction, edge, state)) => {
                if state == shortcuts::State::Pressed {
                    self.common
                        .shell
                        .write()
                        .resize(seat, direction, edge.into());
                } else {
                    self.common
                        .shell
                        .write()
                        .finish_resize(direction, edge.into());
                }
            }
        }
    }

    pub fn handle_swipe_action(&mut self, action: gestures::SwipeAction, seat: &Seat<State>) {
        use gestures::SwipeAction;

        match action {
            SwipeAction::NextWorkspace => {
                let _ = to_next_workspace(
                    &mut *self.common.shell.write(),
                    &seat,
                    true,
                    &mut self.common.workspace_state.update(),
                );
            }
            SwipeAction::PrevWorkspace => {
                let _ = to_previous_workspace(
                    &mut *self.common.shell.write(),
                    &seat,
                    true,
                    &mut self.common.workspace_state.update(),
                );
            }
        }
    }

    #[profiling::function]
    pub fn handle_shortcut_action(
        &mut self,
        action: shortcuts::Action,
        seat: &Seat<State>,
        serial: Serial,
        time: u32,
        pattern: shortcuts::Binding,
        direction: Option<Direction>,
        propagate: bool,
    ) {
        use shortcuts::Action;

        match action {
            Action::Terminate => {
                self.common.should_stop = true;
            }

            #[cfg(feature = "debug")]
            Action::Debug => {
                let mut shell = self.common.shell.write();
                shell.debug_active = !shell.debug_active;
                for mapped in shell.workspaces.spaces().flat_map(|w| w.mapped()) {
                    mapped.set_debug(shell.debug_active);
                }
            }

            #[cfg(not(feature = "debug"))]
            Action::Debug => {
                info!("Debug overlay not included in this build.")
            }

            Action::Close => {
                if let Some(focus_target) = seat.get_keyboard().unwrap().current_focus() {
                    self.common.shell.read().close_focused(&focus_target);
                }
            }

            Action::Workspace(key_num) => {
                let current_output = seat.active_output();
                let workspace = match key_num {
                    0 => 9,
                    x => x - 1,
                };
                let _ = self.common.shell.write().activate(
                    &current_output,
                    workspace as usize,
                    WorkspaceDelta::new_shortcut(),
                    &mut self.common.workspace_state.update(),
                );
            }

            Action::LastWorkspace => {
                let current_output = seat.active_output();
                let mut shell = self.common.shell.write();
                let workspace = shell.workspaces.len(&current_output).saturating_sub(1);
                let _ = shell.activate(
                    &current_output,
                    workspace,
                    WorkspaceDelta::new_shortcut(),
                    &mut self.common.workspace_state.update(),
                );
            }

            Action::NextWorkspace => {
                if let Some(direction) = pattern.inferred_direction() {
                    if ((direction == Direction::Left || direction == Direction::Right)
                        && self.common.config.cosmic_conf.workspaces.workspace_layout
                            == WorkspaceLayout::Vertical)
                        || ((direction == Direction::Up || direction == Direction::Down)
                            && self.common.config.cosmic_conf.workspaces.workspace_layout
                                == WorkspaceLayout::Horizontal)
                    {
                        return;
                    }
                }

                let next = to_next_workspace(
                    &mut *self.common.shell.write(),
                    seat,
                    false,
                    &mut self.common.workspace_state.update(),
                );
                if next.is_err() {
                    if propagate {
                        if let Some(inferred) = pattern.inferred_direction() {
                            self.handle_shortcut_action(
                                Action::SwitchOutput(inferred),
                                seat,
                                serial,
                                time,
                                pattern,
                                direction,
                                true,
                            )
                        };
                    } else {
                        self.handle_shortcut_action(
                            Action::Workspace(1),
                            seat,
                            serial,
                            time,
                            pattern,
                            direction,
                            false,
                        )
                    }
                }
            }

            Action::PreviousWorkspace => {
                if let Some(direction) = pattern.inferred_direction() {
                    if ((direction == Direction::Left || direction == Direction::Right)
                        && self.common.config.cosmic_conf.workspaces.workspace_layout
                            == WorkspaceLayout::Vertical)
                        || ((direction == Direction::Up || direction == Direction::Down)
                            && self.common.config.cosmic_conf.workspaces.workspace_layout
                                == WorkspaceLayout::Horizontal)
                    {
                        return;
                    }
                }

                let previous = to_previous_workspace(
                    &mut *self.common.shell.write(),
                    seat,
                    false,
                    &mut self.common.workspace_state.update(),
                );
                if previous.is_err() {
                    if propagate {
                        if let Some(inferred) = pattern.inferred_direction() {
                            self.handle_shortcut_action(
                                Action::SwitchOutput(inferred),
                                seat,
                                serial,
                                time,
                                pattern,
                                direction,
                                true,
                            )
                        };
                    } else {
                        self.handle_shortcut_action(
                            Action::LastWorkspace,
                            seat,
                            serial,
                            time,
                            pattern,
                            direction,
                            false,
                        )
                    }
                }
            }

            x @ Action::MoveToWorkspace(_) | x @ Action::SendToWorkspace(_) => {
                let Some(focused_output) = seat.focused_output() else {
                    return;
                };
                let follow = matches!(x, Action::MoveToWorkspace(_));
                let workspace = match x {
                    Action::MoveToWorkspace(0) | Action::SendToWorkspace(0) => 9,
                    Action::MoveToWorkspace(x) | Action::SendToWorkspace(x) => x - 1,
                    _ => unreachable!(),
                };
                let res = self.common.shell.write().move_current(
                    seat,
                    (&focused_output, Some(workspace as usize)),
                    follow,
                    None,
                    &mut self.common.workspace_state.update(),
                    &self.common.event_loop_handle,
                );
                if let Ok(Some((target, _point))) = res {
                    Shell::set_focus(
                        self,
                        Some(&target),
                        seat,
                        None,
                        matches!(x, Action::MoveToWorkspace(_)),
                    );
                }
            }

            x @ Action::MoveToLastWorkspace | x @ Action::SendToLastWorkspace => {
                let Some(focused_output) = seat.focused_output() else {
                    return;
                };
                let mut shell = self.common.shell.write();
                let workspace = shell.workspaces.len(&focused_output).saturating_sub(1);
                let res = shell.move_current(
                    seat,
                    (&focused_output, Some(workspace)),
                    matches!(x, Action::MoveToLastWorkspace),
                    None,
                    &mut self.common.workspace_state.update(),
                    &self.common.event_loop_handle,
                );
                // If the active workspace changed, the cursor_follows_focus should probably be checked
                if let Ok(Some((target, _point))) = res {
                    std::mem::drop(shell);
                    Shell::set_focus(
                        self,
                        Some(&target),
                        seat,
                        None,
                        matches!(x, Action::MoveToLastWorkspace),
                    );
                }
            }

            x @ Action::MoveToNextWorkspace | x @ Action::SendToNextWorkspace => {
                if let Some(direction) = pattern.inferred_direction() {
                    if ((direction == Direction::Left || direction == Direction::Right)
                        && self.common.config.cosmic_conf.workspaces.workspace_layout
                            == WorkspaceLayout::Vertical)
                        || ((direction == Direction::Up || direction == Direction::Down)
                            && self.common.config.cosmic_conf.workspaces.workspace_layout
                                == WorkspaceLayout::Horizontal)
                    {
                        return;
                    }
                }
                let Some(focused_output) = seat.focused_output() else {
                    return;
                };

                let res = {
                    let mut shell = self.common.shell.write();
                    shell
                        .workspaces
                        .active_num(&focused_output)
                        .1
                        .checked_add(1)
                        .ok_or(InvalidWorkspaceIndex)
                        .and_then(|workspace| {
                            shell.move_current(
                                seat,
                                (&focused_output, Some(workspace)),
                                matches!(x, Action::MoveToNextWorkspace),
                                direction,
                                &mut self.common.workspace_state.update(),
                                &self.common.event_loop_handle,
                            )
                        })
                };

                match res {
                    Ok(Some((target, _point))) => {
                        // If the active workspace changed, the cursor_follows_focus should probably be checked
                        Shell::set_focus(
                            self,
                            Some(&target),
                            seat,
                            None,
                            matches!(x, Action::MoveToNextWorkspace),
                        );
                    }
                    Ok(None) => {}
                    Err(_) if propagate => {
                        if let Some(inferred) = pattern.inferred_direction() {
                            self.handle_shortcut_action(
                                if matches!(x, Action::MoveToNextWorkspace) {
                                    Action::MoveToOutput(inferred)
                                } else {
                                    Action::SendToOutput(inferred)
                                },
                                seat,
                                serial,
                                time,
                                pattern,
                                direction,
                                true,
                            )
                        }
                    }
                    Err(_) => {
                        // cycle through
                        self.handle_shortcut_action(
                            if matches!(x, Action::MoveToNextWorkspace) {
                                Action::MoveToWorkspace(1)
                            } else {
                                Action::SendToWorkspace(1)
                            },
                            seat,
                            serial,
                            time,
                            pattern,
                            direction,
                            false,
                        )
                    }
                }
            }

            x @ Action::MoveToPreviousWorkspace | x @ Action::SendToPreviousWorkspace => {
                if let Some(direction) = pattern.inferred_direction() {
                    if ((direction == Direction::Left || direction == Direction::Right)
                        && self.common.config.cosmic_conf.workspaces.workspace_layout
                            == WorkspaceLayout::Vertical)
                        || ((direction == Direction::Up || direction == Direction::Down)
                            && self.common.config.cosmic_conf.workspaces.workspace_layout
                                == WorkspaceLayout::Horizontal)
                    {
                        return;
                    }
                }
                let Some(focused_output) = seat.focused_output() else {
                    return;
                };

                let res = {
                    let mut shell = self.common.shell.write();
                    shell
                        .workspaces
                        .active_num(&focused_output)
                        .1
                        .checked_sub(1)
                        .ok_or(InvalidWorkspaceIndex)
                        .and_then(|workspace| {
                            shell.move_current(
                                seat,
                                (&focused_output, Some(workspace)),
                                matches!(x, Action::MoveToPreviousWorkspace),
                                direction,
                                &mut self.common.workspace_state.update(),
                                &self.common.event_loop_handle,
                            )
                        })
                };

                match res {
                    Ok(Some((target, _point))) => {
                        Shell::set_focus(
                            self,
                            Some(&target),
                            seat,
                            None,
                            matches!(x, Action::MoveToPreviousWorkspace),
                        );
                    }
                    Ok(None) => {}
                    Err(_) if propagate => {
                        if let Some(inferred) = pattern.inferred_direction() {
                            self.handle_shortcut_action(
                                if matches!(x, Action::MoveToPreviousWorkspace) {
                                    Action::MoveToOutput(inferred)
                                } else {
                                    Action::SendToOutput(inferred)
                                },
                                seat,
                                serial,
                                time,
                                pattern,
                                direction,
                                true,
                            )
                        }
                    }
                    Err(_) => {
                        // cycle through
                        self.handle_shortcut_action(
                            if matches!(x, Action::MoveToPreviousWorkspace) {
                                Action::MoveToLastWorkspace
                            } else {
                                Action::SendToLastWorkspace
                            },
                            seat,
                            serial,
                            time,
                            pattern,
                            direction,
                            false,
                        )
                    }
                }
            }

            Action::SwitchOutput(direction) => {
                let current_output = seat.active_output();
                let mut shell = self.common.shell.write();

                let next_output = shell.next_output(&current_output, direction).cloned();

                if let Some(next_output) = next_output {
                    let res = {
                        let mut workspace_guard = self.common.workspace_state.update();
                        if propagate {
                            if let Some((serial, prev_output, prev_idx)) =
                                shell.previous_workspace_idx.take()
                            {
                                if seat.last_modifier_change().is_some_and(|s| s == serial)
                                    && prev_output == current_output
                                {
                                    let _ = shell.activate(
                                        &current_output,
                                        prev_idx,
                                        WorkspaceDelta::new_shortcut(),
                                        &mut workspace_guard,
                                    );
                                }
                            }
                        }

                        let idx = shell.workspaces.active_num(&next_output).1;
                        let res = shell.activate(
                            &next_output,
                            idx,
                            WorkspaceDelta::new_shortcut(),
                            &mut workspace_guard,
                        );
                        seat.set_active_output(&next_output);
                        res
                    };

                    if let Ok(new_pos) = res {
                        let workspace = shell.workspaces.active(&next_output).unwrap().1;
                        let new_target = workspace
                            .focus_stack
                            .get(&seat)
                            .last()
                            .cloned()
                            .map(Into::<KeyboardFocusTarget>::into);
                        std::mem::drop(shell);

                        let update_cursor = self.common.config.cosmic_conf.cursor_follows_focus;
                        Shell::set_focus(self, new_target.as_ref(), seat, None, update_cursor);

                        if let Some(ptr) = seat.get_pointer() {
                            // Update cursor position if `set_focus` didn't already
                            if !update_cursor {
                                ptr.motion(
                                    self,
                                    None,
                                    &MotionEvent {
                                        location: new_pos.to_f64().as_logical(),
                                        serial,
                                        time,
                                    },
                                );
                            }
                            ptr.frame(self);
                        }
                    }
                }
            }

            #[allow(deprecated)]
            Action::NextOutput => {
                warn!("Skipping deprecated shortcut NextOutput");
            }

            #[allow(deprecated)]
            Action::PreviousOutput => {
                warn!("Skipping deprecated shortcut PreviousOutput");
            }

            action @ Action::MoveToOutput(_) | action @ Action::SendToOutput(_) => {
                let is_move_action = matches!(action, Action::MoveToOutput(_));
                let direction = match action {
                    Action::MoveToOutput(dir) => dir,
                    Action::SendToOutput(dir) => dir,
                    _ => unreachable!(),
                };

                let Some(focused_output) = seat.focused_output() else {
                    return;
                };
                let mut shell = self.common.shell.write();
                let next_output = shell.next_output(&focused_output, direction).cloned();

                if let Some(next_output) = next_output {
                    let res = {
                        let mut workspace_guard = self.common.workspace_state.update();
                        let res = shell.move_current(
                            seat,
                            (&next_output, None),
                            is_move_action,
                            Some(direction),
                            &mut workspace_guard,
                            &self.common.event_loop_handle,
                        );

                        if is_move_action && propagate {
                            if let Some((_, prev_output, prev_idx)) =
                                shell.previous_workspace_idx.take()
                            {
                                if prev_output == focused_output {
                                    let _ = shell.activate(
                                        &focused_output,
                                        prev_idx,
                                        WorkspaceDelta::new_shortcut(),
                                        &mut workspace_guard,
                                    );
                                }
                            }
                        }
                        res
                    };

                    if let Ok(Some((target, new_pos))) = res {
                        std::mem::drop(shell);
                        Shell::set_focus(self, Some(&target), seat, None, is_move_action);
                        if let Some(ptr) = seat.get_pointer() {
                            ptr.motion(
                                self,
                                None,
                                &MotionEvent {
                                    location: new_pos.to_f64().as_logical(),
                                    serial,
                                    time,
                                },
                            );
                            ptr.frame(self);
                        }
                    }
                }
            }

            #[allow(deprecated)]
            Action::MoveToNextOutput | Action::SendToNextOutput => {
                warn!("Ignoring deprecated action Move/SendToNextOutput");
            }

            #[allow(deprecated)]
            Action::MoveToPreviousOutput | Action::SendToPreviousOutput => {
                warn!("Ignoring deprecated action Move/SendToPreviousOutput");
            }

            Action::MigrateWorkspaceToOutput(direction) => {
                let active_output = seat.active_output();
                let (active, next_output) = {
                    let shell = self.common.shell.read();

                    (
                        shell.active_space(&active_output).unwrap().handle,
                        shell.next_output(&active_output, direction).cloned(),
                    )
                };

                if let Some(next_output) = next_output {
                    let mut shell = self.common.shell.write();
                    let mut workspace_state = self.common.workspace_state.update();
                    shell.workspaces.migrate_workspace(
                        &active_output,
                        &next_output,
                        &active,
                        &mut workspace_state,
                    );
                    // Activate workspace on new set, and set that output as active
                    if let Some(new_idx) = shell
                        .workspaces
                        .sets
                        .get(&next_output)
                        .and_then(|set| set.workspaces.iter().position(|w| w.handle == active))
                    {
                        let res = shell.activate(
                            &next_output,
                            new_idx,
                            WorkspaceDelta::new_shortcut(),
                            &mut workspace_state,
                        );
                        drop(workspace_state);
                        drop(shell);
                        if res.is_ok() {
                            self.handle_shortcut_action(
                                Action::SwitchOutput(direction),
                                seat,
                                serial,
                                time,
                                pattern,
                                Some(direction),
                                true,
                            )
                        }
                    }
                }
            }

            #[allow(deprecated)]
            Action::MigrateWorkspaceToNextOutput => {
                warn!("Ignoring deprecated action MigrateWorkspaceToNextOutput");
            }

            #[allow(deprecated)]
            Action::MigrateWorkspaceToPreviousOutput => {
                warn!("Ignoring deprecated action MigrateWorkspaceToPreviousOutput");
            }

            Action::Focus(focus) => {
                let result = self.common.shell.read().next_focus(focus, seat);

                match result {
                    FocusResult::None => {
                        let dir = match focus {
                            FocusDirection::Down => Some(Direction::Down),
                            FocusDirection::Up => Some(Direction::Up),
                            FocusDirection::Left => Some(Direction::Left),
                            FocusDirection::Right => Some(Direction::Right),
                            _ => None,
                        };

                        if let Some(direction) = dir {
                            if let Some(last_mod_serial) = seat.last_modifier_change() {
                                let mut shell = self.common.shell.write();
                                if !shell
                                    .previous_workspace_idx
                                    .as_ref()
                                    .is_some_and(|(serial, _, _)| *serial == last_mod_serial)
                                {
                                    let current_output = seat.active_output();
                                    let workspace_idx =
                                        shell.workspaces.active_num(&current_output).1;
                                    shell.previous_workspace_idx = Some((
                                        last_mod_serial,
                                        current_output.downgrade(),
                                        workspace_idx,
                                    ));
                                }
                            }

                            let action = match (
                                direction,
                                self.common.config.cosmic_conf.workspaces.workspace_layout,
                            ) {
                                (Direction::Left, WorkspaceLayout::Horizontal)
                                | (Direction::Up, WorkspaceLayout::Vertical) => {
                                    Action::PreviousWorkspace
                                }
                                (Direction::Right, WorkspaceLayout::Horizontal)
                                | (Direction::Down, WorkspaceLayout::Vertical) => {
                                    Action::NextWorkspace
                                }
                                _ => Action::SwitchOutput(direction),
                            };

                            self.handle_shortcut_action(
                                action,
                                seat,
                                serial,
                                time,
                                pattern,
                                Some(direction),
                                true,
                            )
                        }
                    }
                    FocusResult::Handled => {}
                    FocusResult::Some(target) => {
                        Shell::set_focus(self, Some(&target), seat, None, true);
                    }
                }
            }

            Action::Move(direction) => {
                let res = self
                    .common
                    .shell
                    .write()
                    .move_current_element(direction, seat);
                match res {
                    MoveResult::MoveFurther(_move_further) => {
                        if let Some(last_mod_serial) = seat.last_modifier_change() {
                            let mut shell = self.common.shell.write();
                            if !shell
                                .previous_workspace_idx
                                .as_ref()
                                .is_some_and(|(serial, _, _)| *serial == last_mod_serial)
                            {
                                let current_output = seat.active_output();
                                let workspace_idx = shell.workspaces.active_num(&current_output).1;
                                shell.previous_workspace_idx = Some((
                                    last_mod_serial,
                                    current_output.downgrade(),
                                    workspace_idx,
                                ));
                            }
                        }

                        let action = match (
                            direction,
                            self.common.config.cosmic_conf.workspaces.workspace_layout,
                        ) {
                            (Direction::Left, WorkspaceLayout::Horizontal)
                            | (Direction::Up, WorkspaceLayout::Vertical) => {
                                Action::MoveToPreviousWorkspace
                            }
                            (Direction::Right, WorkspaceLayout::Horizontal)
                            | (Direction::Down, WorkspaceLayout::Vertical) => {
                                Action::MoveToNextWorkspace
                            }
                            _ => Action::MoveToOutput(direction),
                        };

                        self.handle_shortcut_action(
                            action,
                            seat,
                            serial,
                            time,
                            pattern,
                            Some(direction),
                            true,
                        )
                    }
                    MoveResult::ShiftFocus(shift) => {
                        Shell::set_focus(self, Some(&shift), seat, None, true);
                    }
                    _ => {
                        let current_output = seat.active_output();
                        let mut shell = self.common.shell.write();
                        let workspace = shell.active_space(&current_output).unwrap();
                        if let Some(FocusTarget::Window(focused_window)) =
                            workspace.focus_stack.get(seat).last()
                        {
                            if workspace.is_tiled(&focused_window.active_window()) {
                                shell.set_overview_mode(
                                    Some(Trigger::KeyboardMove(pattern.modifiers)),
                                    self.common.event_loop_handle.clone(),
                                );
                            }
                        }
                    }
                }
            }

            Action::SwapWindow => {
                let Some(focused_output) = seat.focused_output() else {
                    return;
                };
                let mut shell = self.common.shell.write();

                let workspace = shell.active_space_mut(&focused_output).unwrap();
                let keyboard_handle = seat.get_keyboard().unwrap();

                if let Some(focus) = keyboard_handle.current_focus() {
                    if let Some(descriptor) = workspace.node_desc(focus) {
                        let grab = SwapWindowGrab::new(seat.clone(), descriptor.clone());
                        drop(shell);
                        keyboard_handle.set_grab(self, grab, serial);
                        let mut shell = self.common.shell.write();
                        shell.set_overview_mode(
                            Some(Trigger::KeyboardSwap(pattern, descriptor)),
                            self.common.event_loop_handle.clone(),
                        );
                    }
                }
            }

            Action::Minimize => {
                let Some(focused_output) = seat.focused_output() else {
                    return;
                };
                let mut shell = self.common.shell.write();
                let workspace = shell.active_space_mut(&focused_output).unwrap();
                let focus_stack = workspace.focus_stack.get(seat);
                if let Some(surface) = focus_stack.last().and_then(FocusTarget::wl_surface) {
                    shell.minimize_request(&surface);
                }
            }

            Action::Maximize => {
                let Some(focused_output) = seat.focused_output() else {
                    return;
                };
                let mut shell = self.common.shell.write();
                let workspace = shell.active_space(&focused_output).unwrap();
                let focus_stack = workspace.focus_stack.get(seat);
                let focused_window = focus_stack.last().cloned();
                if let Some(FocusTarget::Window(window)) = focused_window {
                    shell.maximize_toggle(&window, seat, &self.common.event_loop_handle);
                }
            }

            Action::Fullscreen => {
                let Some(focused_output) = seat.focused_output() else {
                    return;
                };
                let mut shell = self.common.shell.write();
                let workspace = shell.active_space(&focused_output).unwrap();
                let focus_stack = workspace.focus_stack.get(seat);
                let focused_window = focus_stack.last().cloned();
                match focused_window {
                    Some(FocusTarget::Window(window)) => {
                        let output = workspace.output.clone();
                        if let Some(target) = shell.fullscreen_request(
                            &window.active_window(),
                            output,
                            &self.common.event_loop_handle,
                        ) {
                            std::mem::drop(shell);
                            Shell::set_focus(self, Some(&target), seat, Some(serial), true);
                        }
                    }
                    Some(FocusTarget::Fullscreen(surface)) => {
                        if let Some(target) =
                            shell.unfullscreen_request(&surface, &self.common.event_loop_handle)
                        {
                            std::mem::drop(shell);
                            Shell::set_focus(self, Some(&target), seat, Some(serial), true);
                        }
                    }
                    _ => {}
                }
            }

            Action::Resizing(direction) => self.common.shell.write().set_resize_mode(
                Some((pattern, direction)),
                &self.common.config,
                self.common.event_loop_handle.clone(),
            ),
            // NOTE: implementation currently assumes actions that apply to outputs should apply to the active output
            // rather than the output that has keyboard focus
            Action::ToggleOrientation => {
                let output = seat.active_output();
                let mut shell = self.common.shell.write();
                let workspace = shell.active_space_mut(&output).unwrap();
                workspace.tiling_layer.update_orientation(None, &seat);
            }

            Action::Orientation(orientation) => {
                let output = seat.active_output();
                let mut shell = self.common.shell.write();
                let workspace = shell.active_space_mut(&output).unwrap();
                workspace
                    .tiling_layer
                    .update_orientation(Some(orientation), &seat);
            }

            Action::ToggleStacking => {
                let res = self
                    .common
                    .shell
                    .write()
                    .toggle_stacking_focused(seat, &self.common.event_loop_handle);
                if let Some(new_focus) = res {
                    Shell::set_focus(self, Some(&new_focus), seat, Some(serial), false);
                }
            }

            Action::ToggleTiling => {
                if matches!(
                    self.common.config.cosmic_conf.autotile_behavior,
                    TileBehavior::Global
                ) {
                    let autotile = !self.common.config.cosmic_conf.autotile;
                    self.common.config.cosmic_conf.autotile = autotile;

                    {
                        let mut shell = self.common.shell.write();
                        let shell_ref = &mut *shell;
                        shell_ref.workspaces.update_autotile(
                            self.common.config.cosmic_conf.autotile,
                            &mut self.common.workspace_state.update(),
                            shell_ref.seats.iter(),
                        );
                    }
                    let config = self.common.config.cosmic_helper.clone();
                    thread::spawn(move || {
                        if let Err(err) = config.set("autotile", autotile) {
                            error!(?err, "Failed to update autotile key");
                        }
                    });
                } else {
                    let output = seat.active_output();
                    let mut shell = self.common.shell.write();
                    let workspace = shell.workspaces.active_mut(&output).unwrap();
                    let mut guard = self.common.workspace_state.update();
                    workspace.toggle_tiling(seat, &mut guard);
                }
            }

            Action::ToggleWindowFloating => {
                let Some(output) = seat.focused_output() else {
                    return;
                };
                let mut shell = self.common.shell.write();
                let workspace = shell.active_space_mut(&output).unwrap();
                workspace.toggle_floating_window_focused(seat);
            }

            Action::ToggleSticky => {
                self.common.shell.write().toggle_sticky_current(seat);
            }

            // Gets the configured command for a given system action.
            Action::System(system) => {
                if let Some(command) = self.common.config.system_actions.get(&system) {
                    self.spawn_command(command.clone());
                }
            }

            Action::Spawn(command) => self.spawn_command(command),

            x @ Action::ZoomIn | x @ Action::ZoomOut => {
                let change = {
                    let increment =
                        self.common.config.cosmic_conf.accessibility_zoom.increment as f64 / 100.0;
                    match x {
                        Action::ZoomIn => increment,
                        Action::ZoomOut => -increment,
                        _ => unreachable!(),
                    }
                };

                self.update_zoom(seat, change, true);
            }

            // Do nothing
            Action::Disable => (),
        }
    }

    pub fn spawn_command(&mut self, command: String) {
        let mut shell = self.common.shell.write();

        let (token, data) = self.common.xdg_activation_state.create_external_token(None);
        let (token, data) = (token.clone(), data.clone());

        let output = shell.seats.last_active().active_output();
        let workspace = shell.active_space_mut(&output).unwrap();
        let handle = workspace.handle;
        std::mem::drop(shell);
        data.user_data
            .insert_if_missing(move || ActivationContext::Workspace(handle));

        let wayland_display = self.common.socket.clone();
        let display = self
            .common
            .xwayland_state
            .as_ref()
            .map(|s| format!(":{}", s.display))
            .unwrap_or_default();

        let mut cmd = std::process::Command::new("/bin/sh");

        cmd.arg("-c")
            .arg(&command)
            .env("WAYLAND_DISPLAY", &wayland_display)
            .env("DISPLAY", &display)
            .env("XDG_ACTIVATION_TOKEN", &*token)
            .env("DESKTOP_STARTUP_ID", &*token)
            .env_remove("COSMIC_SESSION_SOCK");
        unsafe { cmd.pre_exec(|| Ok(crate::utils::rlimit::restore_nofile_limit())) };

        std::thread::spawn(move || match cmd.spawn() {
            Ok(mut child) => {
                let _res = child.wait();
            }
            Err(err) => {
                tracing::warn!(?err, "Failed to spawn \"{}\"", command);
            }
        });
    }

    pub fn update_zoom(&mut self, seat: &Seat<State>, change: f64, animate: bool) {
        let output = seat.active_output();
        let mut shell = self.common.shell.write();
        let (zoom_seat, current_level) = shell
            .zoom_state()
            .map(|state| (state.current_seat(), state.current_level(&output)))
            .unwrap_or_else(|| (seat.clone(), 1.0));

        if current_level == 1. && change <= 0. {
            return;
        }

        if zoom_seat == *seat {
            let new_level = (current_level + change).max(1.0);
            shell.trigger_zoom(
                &seat,
                Some(&output),
                new_level,
                &self.common.config.cosmic_conf.accessibility_zoom,
                animate,
                &self.common.event_loop_handle,
            );
        }
    }
}

fn to_next_workspace(
    shell: &mut Shell,
    seat: &Seat<State>,
    gesture: bool,
    workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
) -> Result<Point<i32, Global>, InvalidWorkspaceIndex> {
    let current_output = seat.active_output();
    let workspace = shell
        .workspaces
        .active_num(&current_output)
        .1
        .checked_add(1)
        .ok_or(InvalidWorkspaceIndex)?;

    shell.activate(
        &current_output,
        workspace,
        if gesture {
            WorkspaceDelta::new_gesture()
        } else {
            WorkspaceDelta::new_shortcut()
        },
        workspace_state,
    )
}

fn to_previous_workspace(
    shell: &mut Shell,
    seat: &Seat<State>,
    gesture: bool,
    workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
) -> Result<Point<i32, Global>, InvalidWorkspaceIndex> {
    let current_output = seat.active_output();
    let workspace = shell
        .workspaces
        .active_num(&current_output)
        .1
        .checked_sub(1)
        .ok_or(InvalidWorkspaceIndex)?;

    shell.activate(
        &current_output,
        workspace,
        if gesture {
            WorkspaceDelta::new_gesture()
        } else {
            WorkspaceDelta::new_shortcut()
        },
        workspace_state,
    )
}
