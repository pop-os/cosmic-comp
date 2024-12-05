// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    config::{Action, PrivateAction},
    shell::{
        focus::target::KeyboardFocusTarget, layout::tiling::SwapWindowGrab, FocusResult,
        InvalidWorkspaceIndex, MoveResult, SeatExt, Trigger, WorkspaceDelta,
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
use tracing::error;
#[cfg(not(feature = "debug"))]
use tracing::info;

use std::{os::unix::process::CommandExt, thread};

use super::gestures;

impl State {
    pub fn handle_action(
        &mut self,
        action: Action,
        seat: &Seat<State>,
        serial: Serial,
        time: u32,
        pattern: shortcuts::Binding,
        direction: Option<Direction>,
        propagate: bool,
    ) {
        // TODO: Detect if started from login manager or tty, and only allow
        // `Terminate` if it will return to login manager.
        if self.common.shell.read().unwrap().session_lock.is_some()
            && !matches!(
                action,
                Action::Shortcut(shortcuts::Action::Terminate)
                    | Action::Shortcut(shortcuts::Action::Debug)
            )
        {
            return;
        }

        match action {
            Action::Shortcut(action) => self
                .handle_shortcut_action(action, seat, serial, time, pattern, direction, propagate),

            Action::Private(PrivateAction::Escape) => {
                {
                    let mut shell = self.common.shell.write().unwrap();
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
                        .unwrap()
                        .resize(seat, direction, edge.into());
                } else {
                    self.common
                        .shell
                        .write()
                        .unwrap()
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
                    &mut *self.common.shell.write().unwrap(),
                    &seat,
                    true,
                    &mut self.common.workspace_state.update(),
                );
            }
            SwipeAction::PrevWorkspace => {
                let _ = to_previous_workspace(
                    &mut *self.common.shell.write().unwrap(),
                    &seat,
                    true,
                    &mut self.common.workspace_state.update(),
                );
            }
        }
    }

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
                let mut shell = self.common.shell.write().unwrap();
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
                    self.common
                        .shell
                        .read()
                        .unwrap()
                        .close_focused(&focus_target);
                }
            }

            Action::Workspace(key_num) => {
                let current_output = seat.active_output();
                let workspace = match key_num {
                    0 => 9,
                    x => x - 1,
                };
                let _ = self.common.shell.write().unwrap().activate(
                    &current_output,
                    workspace as usize,
                    WorkspaceDelta::new_shortcut(),
                    &mut self.common.workspace_state.update(),
                );
            }

            Action::LastWorkspace => {
                let current_output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();
                let workspace = shell.workspaces.len(&current_output).saturating_sub(1);
                let _ = shell.activate(
                    &current_output,
                    workspace,
                    WorkspaceDelta::new_shortcut(),
                    &mut self.common.workspace_state.update(),
                );
            }

            Action::NextWorkspace => {
                let next = to_next_workspace(
                    &mut *self.common.shell.write().unwrap(),
                    seat,
                    false,
                    &mut self.common.workspace_state.update(),
                );
                if next.is_err() && propagate {
                    if let Some(inferred) = pattern.inferred_direction() {
                        self.handle_shortcut_action(
                            Action::SwitchOutput(inferred),
                            seat,
                            serial,
                            time,
                            pattern,
                            direction,
                            false,
                        )
                    };
                }
            }

            Action::PreviousWorkspace => {
                let previous = to_previous_workspace(
                    &mut *self.common.shell.write().unwrap(),
                    seat,
                    false,
                    &mut self.common.workspace_state.update(),
                );
                if previous.is_err() && propagate {
                    if let Some(inferred) = pattern.inferred_direction() {
                        self.handle_shortcut_action(
                            Action::SwitchOutput(inferred),
                            seat,
                            serial,
                            time,
                            pattern,
                            direction,
                            false,
                        )
                    };
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
                let res = self.common.shell.write().unwrap().move_current_window(
                    seat,
                    &focused_output,
                    (&focused_output, Some(workspace as usize)),
                    follow,
                    None,
                    &mut self.common.workspace_state.update(),
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
                let mut shell = self.common.shell.write().unwrap();
                let workspace = shell.workspaces.len(&focused_output).saturating_sub(1);
                let res = shell.move_current_window(
                    seat,
                    &focused_output,
                    (&focused_output, Some(workspace)),
                    matches!(x, Action::MoveToLastWorkspace),
                    None,
                    &mut self.common.workspace_state.update(),
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
                let Some(focused_output) = seat.focused_output() else {
                    return;
                };
                let res = {
                    let mut shell = self.common.shell.write().unwrap();
                    let workspace = shell
                        .workspaces
                        .active_num(&focused_output)
                        .1
                        .saturating_add(1);
                    shell.move_current_window(
                        seat,
                        &focused_output,
                        (&focused_output, Some(workspace)),
                        matches!(x, Action::MoveToNextWorkspace),
                        direction,
                        &mut self.common.workspace_state.update(),
                    )
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
                                false,
                            )
                        }
                    }
                    _ => {}
                }
            }

            x @ Action::MoveToPreviousWorkspace | x @ Action::SendToPreviousWorkspace => {
                let Some(focused_output) = seat.focused_output() else {
                    return;
                };
                let res = {
                    let mut shell = self.common.shell.write().unwrap();
                    let workspace = shell
                        .workspaces
                        .active_num(&focused_output)
                        .1
                        .saturating_sub(1);
                    // TODO: Possibly move to prev output, if idx < 0
                    shell.move_current_window(
                        seat,
                        &focused_output,
                        (&focused_output, Some(workspace)),
                        matches!(x, Action::MoveToPreviousWorkspace),
                        direction,
                        &mut self.common.workspace_state.update(),
                    )
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
                                false,
                            )
                        }
                    }
                    _ => {}
                }
            }

            Action::SwitchOutput(direction) => {
                let current_output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();

                let next_output = shell.next_output(&current_output, direction).cloned();

                if let Some(next_output) = next_output {
                    let idx = shell.workspaces.active_num(&next_output).1;
                    let res = shell.activate(
                        &next_output,
                        idx,
                        WorkspaceDelta::new_shortcut(),
                        &mut self.common.workspace_state.update(),
                    );
                    seat.set_active_output(&next_output);
                    shell.swapped_output = true;

                    if let Ok(Some(new_pos)) = res {
                        let new_target = shell
                            .workspaces
                            .active(&next_output)
                            .1
                            .focus_stack
                            .get(&seat)
                            .last()
                            .cloned()
                            .map(KeyboardFocusTarget::from);
                        std::mem::drop(shell);

                        let move_cursor = if let Some(under) = new_target {
                            let update_cursor = self.common.config.cosmic_conf.focus_follows_cursor;
                            Shell::set_focus(self, Some(&under), seat, None, update_cursor);
                            !update_cursor
                        } else {
                            true
                        };

                        if let Some(ptr) = seat.get_pointer() {
                            if move_cursor {
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
                } else if propagate {
                    std::mem::drop(shell);

                    let action = match (
                        direction,
                        self.common.config.cosmic_conf.workspaces.workspace_layout,
                    ) {
                        (Direction::Left, WorkspaceLayout::Horizontal)
                        | (Direction::Up, WorkspaceLayout::Vertical) => {
                            Some(Action::PreviousWorkspace)
                        }
                        (Direction::Right, WorkspaceLayout::Horizontal)
                        | (Direction::Down, WorkspaceLayout::Vertical) => {
                            Some(Action::NextWorkspace)
                        }
                        _ => None,
                    };

                    if let Some(action) = action {
                        self.handle_shortcut_action(
                            action,
                            seat,
                            serial,
                            time,
                            pattern,
                            Some(direction),
                            false,
                        )
                    }
                }
            }

            Action::NextOutput => {
                let current_output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();

                let next_output = shell
                    .outputs()
                    .skip_while(|o| *o != &current_output)
                    .skip(1)
                    .next()
                    .cloned();
                if let Some(next_output) = next_output {
                    let idx = shell.workspaces.active_num(&next_output).1;
                    let res = shell.activate(
                        &next_output,
                        idx,
                        WorkspaceDelta::new_shortcut(),
                        &mut self.common.workspace_state.update(),
                    );
                    seat.set_active_output(&next_output);
                    shell.swapped_output = true;

                    if let Ok(Some(new_pos)) = res {
                        let new_target = shell
                            .workspaces
                            .active(&next_output)
                            .1
                            .focus_stack
                            .get(&seat)
                            .last()
                            .cloned()
                            .map(KeyboardFocusTarget::from);
                        std::mem::drop(shell);

                        let move_cursor = if let Some(under) = new_target {
                            let update_cursor = self.common.config.cosmic_conf.focus_follows_cursor;
                            Shell::set_focus(self, Some(&under), seat, None, update_cursor);
                            !update_cursor
                        } else {
                            true
                        };

                        if let Some(ptr) = seat.get_pointer() {
                            if move_cursor {
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

            Action::PreviousOutput => {
                let current_output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();

                let prev_output = shell
                    .outputs()
                    .rev()
                    .skip_while(|o| *o != &current_output)
                    .skip(1)
                    .next()
                    .cloned();
                if let Some(prev_output) = prev_output {
                    let idx = shell.workspaces.active_num(&prev_output).1;
                    let res = shell.activate(
                        &prev_output,
                        idx,
                        WorkspaceDelta::new_shortcut(),
                        &mut self.common.workspace_state.update(),
                    );
                    seat.set_active_output(&prev_output);
                    shell.swapped_output = true;

                    if let Ok(Some(new_pos)) = res {
                        let new_target = shell
                            .workspaces
                            .active(&prev_output)
                            .1
                            .focus_stack
                            .get(&seat)
                            .last()
                            .cloned()
                            .map(KeyboardFocusTarget::from);
                        std::mem::drop(shell);

                        let move_cursor = if let Some(under) = new_target {
                            let update_cursor = self.common.config.cosmic_conf.focus_follows_cursor;
                            Shell::set_focus(self, Some(&under), seat, None, update_cursor);
                            !update_cursor
                        } else {
                            true
                        };

                        if let Some(ptr) = seat.get_pointer() {
                            if move_cursor {
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
                let mut shell = self.common.shell.write().unwrap();
                let next_output = shell.next_output(&focused_output, direction).cloned();

                if let Some(next_output) = next_output {
                    let res = shell.move_current_window(
                        seat,
                        &focused_output,
                        (&next_output, None),
                        is_move_action,
                        Some(direction),
                        &mut self.common.workspace_state.update(),
                    );
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
                } else if propagate {
                    std::mem::drop(shell);
                    match (
                        direction,
                        self.common.config.cosmic_conf.workspaces.workspace_layout,
                    ) {
                        (Direction::Left, WorkspaceLayout::Horizontal)
                        | (Direction::Up, WorkspaceLayout::Vertical) => self
                            .handle_shortcut_action(
                                Action::MoveToPreviousWorkspace,
                                seat,
                                serial,
                                time,
                                pattern,
                                Some(direction),
                                false,
                            ),
                        (Direction::Right, WorkspaceLayout::Horizontal)
                        | (Direction::Down, WorkspaceLayout::Vertical) => self
                            .handle_shortcut_action(
                                Action::MoveToNextWorkspace,
                                seat,
                                serial,
                                time,
                                pattern,
                                Some(direction),
                                false,
                            ),

                        _ => {}
                    }
                }
            }

            x @ Action::MoveToNextOutput | x @ Action::SendToNextOutput => {
                let Some(focused_output) = seat.focused_output() else {
                    return;
                };
                let mut shell = self.common.shell.write().unwrap();

                let next_output = shell
                    .outputs()
                    .skip_while(|o| *o != &focused_output)
                    .skip(1)
                    .next()
                    .cloned();
                if let Some(next_output) = next_output {
                    let res = shell.move_current_window(
                        seat,
                        &focused_output,
                        (&next_output, None),
                        matches!(x, Action::MoveToNextOutput),
                        direction,
                        &mut self.common.workspace_state.update(),
                    );
                    if let Ok(Some((target, new_pos))) = res {
                        std::mem::drop(shell);
                        Shell::set_focus(
                            self,
                            Some(&target),
                            seat,
                            None,
                            matches!(x, Action::MoveToNextOutput),
                        );
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

            x @ Action::MoveToPreviousOutput | x @ Action::SendToPreviousOutput => {
                let Some(focused_output) = seat.focused_output() else {
                    return;
                };
                let mut shell = self.common.shell.write().unwrap();

                let prev_output = shell
                    .outputs()
                    .rev()
                    .skip_while(|o| *o != &focused_output)
                    .skip(1)
                    .next()
                    .cloned();
                if let Some(prev_output) = prev_output {
                    let res = shell.move_current_window(
                        seat,
                        &focused_output,
                        (&prev_output, None),
                        matches!(x, Action::MoveToPreviousOutput),
                        direction,
                        &mut self.common.workspace_state.update(),
                    );
                    if let Ok(Some((target, new_pos))) = res {
                        std::mem::drop(shell);
                        Shell::set_focus(
                            self,
                            Some(&target),
                            seat,
                            None,
                            matches!(x, Action::MoveToPreviousOutput),
                        );
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

            Action::MigrateWorkspaceToNextOutput => {
                let active_output = seat.active_output();
                let (active, next_output) = {
                    let shell = self.common.shell.read().unwrap();
                    let output = shell
                        .outputs()
                        .skip_while(|o| *o != &active_output)
                        .skip(1)
                        .next()
                        .cloned();

                    (shell.active_space(&active_output).handle, output)
                };
                if let Some(next_output) = next_output {
                    self.common
                        .migrate_workspace(&active_output, &next_output, &active);
                }
            }

            Action::MigrateWorkspaceToPreviousOutput => {
                let active_output = seat.active_output();
                let (active, prev_output) = {
                    let shell = self.common.shell.read().unwrap();
                    let output = shell
                        .outputs()
                        .rev()
                        .skip_while(|o| *o != &active_output)
                        .skip(1)
                        .next()
                        .cloned();

                    (shell.active_space(&active_output).handle, output)
                };
                if let Some(prev_output) = prev_output {
                    self.common
                        .migrate_workspace(&active_output, &prev_output, &active);
                }
            }

            Action::MigrateWorkspaceToOutput(direction) => {
                let active_output = seat.active_output();
                let (active, next_output) = {
                    let shell = self.common.shell.read().unwrap();

                    (
                        shell.active_space(&active_output).handle,
                        shell.next_output(&active_output, direction).cloned(),
                    )
                };

                if let Some(next_output) = next_output {
                    self.common
                        .migrate_workspace(&active_output, &next_output, &active);
                }
            }

            Action::Focus(focus) => {
                let result = self.common.shell.read().unwrap().next_focus(focus, seat);

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
                    .unwrap()
                    .move_current_element(direction, seat);
                match res {
                    MoveResult::MoveFurther(_move_further) => self.handle_shortcut_action(
                        Action::MoveToOutput(direction),
                        seat,
                        serial,
                        time,
                        pattern,
                        Some(direction),
                        true,
                    ),
                    MoveResult::ShiftFocus(shift) => {
                        Shell::set_focus(self, Some(&shift), seat, None, true);
                    }
                    _ => {
                        let current_output = seat.active_output();
                        let mut shell = self.common.shell.write().unwrap();
                        let workspace = shell.active_space(&current_output);
                        if let Some(focused_window) = workspace.focus_stack.get(seat).last() {
                            if workspace.is_tiled(focused_window) {
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
                let mut shell = self.common.shell.write().unwrap();

                let workspace = shell.active_space_mut(&focused_output);
                if workspace.get_fullscreen().is_some() {
                    return; // TODO, is this what we want? Maybe disengage fullscreen instead?
                }

                let keyboard_handle = seat.get_keyboard().unwrap();
                if let Some(focus) = keyboard_handle.current_focus() {
                    if let Some(descriptor) = workspace.node_desc(focus) {
                        let grab = SwapWindowGrab::new(seat.clone(), descriptor.clone());
                        drop(shell);
                        keyboard_handle.set_grab(self, grab, serial);
                        let mut shell = self.common.shell.write().unwrap();
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
                let mut shell = self.common.shell.write().unwrap();
                let workspace = shell.active_space_mut(&focused_output);
                let focus_stack = workspace.focus_stack.get(seat);
                let focused_window = focus_stack.last().cloned();
                if let Some(window) = focused_window {
                    shell.minimize_request(&window);
                }
            }

            Action::Maximize => {
                let Some(focused_output) = seat.focused_output() else {
                    return;
                };
                let mut shell = self.common.shell.write().unwrap();
                let workspace = shell.active_space(&focused_output);
                let focus_stack = workspace.focus_stack.get(seat);
                let focused_window = focus_stack.last().cloned();
                if let Some(window) = focused_window {
                    shell.maximize_toggle(&window, seat);
                }
            }

            Action::Resizing(direction) => self.common.shell.write().unwrap().set_resize_mode(
                Some((pattern, direction)),
                &self.common.config,
                self.common.event_loop_handle.clone(),
            ),
            // NOTE: implementation currently assumes actions that apply to outputs should apply to the active output
            // rather than the output that has keyboard focus
            Action::ToggleOrientation => {
                let output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();
                let workspace = shell.active_space_mut(&output);
                workspace.tiling_layer.update_orientation(None, &seat);
            }

            Action::Orientation(orientation) => {
                let output = seat.active_output();
                let mut shell = self.common.shell.write().unwrap();
                let workspace = shell.active_space_mut(&output);
                workspace
                    .tiling_layer
                    .update_orientation(Some(orientation), &seat);
            }

            Action::ToggleStacking => {
                let res = self
                    .common
                    .shell
                    .write()
                    .unwrap()
                    .toggle_stacking_focused(seat);
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
                        let mut shell = self.common.shell.write().unwrap();
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
                    let mut shell = self.common.shell.write().unwrap();
                    let workspace = shell.workspaces.active_mut(&output);
                    let mut guard = self.common.workspace_state.update();
                    workspace.toggle_tiling(seat, &mut guard);
                }
            }

            Action::ToggleWindowFloating => {
                let Some(output) = seat.focused_output() else {
                    return;
                };
                let mut shell = self.common.shell.write().unwrap();
                let workspace = shell.active_space_mut(&output);
                workspace.toggle_floating_window_focused(seat);
            }

            Action::ToggleSticky => {
                self.common
                    .shell
                    .write()
                    .unwrap()
                    .toggle_sticky_current(seat);
            }

            // Gets the configured command for a given system action.
            Action::System(system) => {
                if let Some(command) = self.common.config.system_actions.get(&system) {
                    self.spawn_command(command.clone());
                }
            }

            Action::Spawn(command) => self.spawn_command(command),

            // Do nothing
            Action::Disable => (),
        }
    }

    fn spawn_command(&mut self, command: String) {
        let mut shell = self.common.shell.write().unwrap();

        let (token, data) = self.common.xdg_activation_state.create_external_token(None);
        let (token, data) = (token.clone(), data.clone());

        let output = shell.seats.last_active().active_output();
        let workspace = shell.active_space_mut(&output);
        workspace.pending_tokens.insert(token.clone());
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
}

fn to_next_workspace(
    shell: &mut Shell,
    seat: &Seat<State>,
    gesture: bool,
    workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
) -> Result<Option<Point<i32, Global>>, InvalidWorkspaceIndex> {
    let current_output = seat.active_output();
    let workspace = shell
        .workspaces
        .active_num(&current_output)
        .1
        .saturating_add(1);

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
) -> Result<Option<Point<i32, Global>>, InvalidWorkspaceIndex> {
    let current_output = seat.active_output();
    let workspace = shell
        .workspaces
        .active_num(&current_output)
        .1
        .saturating_sub(1);

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
