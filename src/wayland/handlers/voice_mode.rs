// SPDX-License-Identifier: GPL-3.0-only

//! Handler implementation for the voice mode protocol

use crate::shell::focus::target::KeyboardFocusTarget;
use crate::shell::grabs::SeatMoveGrabState;
use crate::shell::{SeatExt, Shell};
use crate::state::State;
use crate::utils::geometry::{PointExt, RectGlobalExt, RectLocalExt, SizeExt};
use crate::utils::prelude::OutputExt;
use crate::wayland::protocols::voice_mode::{
    OrbState, VoiceModeHandler, VoiceModeState, delegate_voice_mode,
};
use smithay::desktop::space::SpaceElement;
use smithay::reexports::wayland_server::Resource;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::wayland::seat::WaylandFocus;
use tracing::{debug, info};

impl VoiceModeHandler for State {
    fn voice_mode_state(&mut self) -> &mut VoiceModeState {
        &mut self.common.voice_mode_state
    }

    fn activate_voice_mode(&mut self, focused_surface: Option<&WlSurface>) -> OrbState {
        // Block voice mode activation during session lock (idle/login screen)
        if self.common.shell.read().session_lock.is_some() {
            info!("Voice mode activation blocked - session is locked");
            return OrbState::Hidden;
        }

        info!(?focused_surface, "Activating voice mode");

        // Exit home mode when voice mode activates (orb should appear over everything)
        self.common.home_visibility_state.set_home(false);

        let mut shell = self.common.shell.write();

        // Also exit the shell's internal home mode so home_alpha goes to 0
        // This ensures home-only surfaces are hidden during voice mode
        shell.exit_home_visual_only();

        let seat = shell.seats.last_active().clone();

        // Find output under pointer for floating orb placement
        let pointer_pos = seat.get_pointer().map(|ptr| ptr.current_location());
        let output_under_pointer = pointer_pos.and_then(|pos| {
            shell
                .outputs()
                .find(|out| out.geometry().to_f64().contains(pos.as_global()))
                .cloned()
        });

        // Use the output under the pointer if available, otherwise fall back to active_output
        let output = output_under_pointer.unwrap_or_else(|| seat.active_output());

        // Check if a window is currently being grabbed/dragged
        let grabbed_window_info: Option<(
            WlSurface,
            smithay::utils::Rectangle<i32, smithay::utils::Logical>,
            String,
        )> = seat
            .user_data()
            .get::<SeatMoveGrabState>()
            .and_then(|grab_state| {
                grab_state.lock().ok().and_then(|guard| {
                    guard.as_ref().and_then(|state| {
                        let mapped = state.element();
                        let surface = mapped.active_window().wl_surface()?.into_owned();
                        let surface_id = surface.id().to_string();
                        // Get the grabbed window's current geometry (SpaceElement::geometry returns Logical)
                        let geo = mapped.geometry();
                        Some((surface, geo, surface_id))
                    })
                })
            });

        // Determine which receiver to use and get window geometry if attaching
        // Priority: grabbed window > focused window > default receiver
        let (receiver_surface, window_geo) =
            if let Some((grabbed_surface, geo, surface_id)) = &grabbed_window_info {
                // Check if grabbed window has a voice receiver
                if self
                    .common
                    .voice_mode_state
                    .has_receiver_for_surface(grabbed_surface)
                {
                    info!("Using grabbed window as voice receiver");
                    (
                        Some(grabbed_surface.clone()),
                        Some((*geo, surface_id.clone())),
                    )
                } else if let Some(surface) = focused_surface {
                    // Fall back to focused surface check
                    if self
                        .common
                        .voice_mode_state
                        .has_receiver_for_surface(surface)
                    {
                        let keyboard = seat.get_keyboard().unwrap();
                        let geo_and_id = keyboard.current_focus().and_then(|focus| match &focus {
                            crate::shell::focus::target::KeyboardFocusTarget::Element(mapped) => {
                                Self::find_element_geo_across_outputs(&shell, mapped)
                            }
                            _ => None,
                        });
                        (Some(surface.clone()), geo_and_id)
                    } else {
                        (None, None)
                    }
                } else {
                    (None, None)
                }
            } else if let Some(surface) = focused_surface {
                // No grabbed window, check focused surface
                if self
                    .common
                    .voice_mode_state
                    .has_receiver_for_surface(surface)
                {
                    let keyboard = seat.get_keyboard().unwrap();
                    let geo_and_id = keyboard.current_focus().and_then(|focus| match &focus {
                        crate::shell::focus::target::KeyboardFocusTarget::Element(mapped) => {
                            Self::find_element_geo_across_outputs(&shell, mapped)
                        }
                        _ => None,
                    });
                    (Some(surface.clone()), geo_and_id)
                } else {
                    // Focused surface doesn't have a receiver, try last-focused receiver
                    info!("Focused surface has no receiver, trying last-focused fallback");
                    self.find_last_focused_receiver_geo(&shell, &output)
                        .map_or((None, None), |(s, g)| (Some(s), g))
                }
            } else {
                // No focused surface, try last-focused receiver
                info!("No focused surface, trying last-focused fallback");
                self.find_last_focused_receiver_geo(&shell, &output)
                    .map_or((None, None), |(s, g)| (Some(s), g))
            };

        // If still no receiver, check maximized windows as last resort
        let (receiver_surface, window_geo) = if receiver_surface.is_some() {
            (receiver_surface, window_geo)
        } else {
            // Last resort: check for maximized windows with voice receivers
            let maximized_receiver = shell.active_space(&output).and_then(|workspace| {
                workspace.mapped().find_map(|mapped| {
                    // Check if window is maximized
                    if !mapped.is_maximized(false) {
                        return None;
                    }
                    // Check if it has a voice receiver
                    let active_window = mapped.active_window();
                    let wl_surface = active_window.wl_surface()?;
                    if !self
                        .common
                        .voice_mode_state
                        .has_receiver_for_surface(&wl_surface)
                    {
                        return None;
                    }
                    // Get geometry
                    let local_geo = workspace.element_geometry(mapped)?;
                    let geo = local_geo.to_global(&output).as_logical();
                    let surface_id = wl_surface.id().to_string();
                    info!("Found maximized window with voice receiver");
                    Some((wl_surface.into_owned(), geo, surface_id))
                })
            });

            if let Some((surface, geo, surface_id)) = maximized_receiver {
                (Some(surface), Some((geo, surface_id)))
            } else {
                (None, None)
            }
        };

        let orb_state = if let Some(ref surface) = receiver_surface {
            if let Some((geo, surface_id)) = window_geo {
                // Attach orb to the focused window
                info!(
                    "Attaching orb to receiver surface at {:?} (surface_id: {})",
                    geo, surface_id
                );
                let output_geo = output.geometry();
                // Request show attached - orb will burst directly in window
                shell.voice_orb_state.request_show_attached(
                    geo,
                    output_geo.size.as_logical(),
                    surface_id,
                );
                shell.enter_voice_mode();

                // Send start to the window-specific receiver
                drop(shell);
                self.common
                    .voice_mode_state
                    .send_start_to_surface(surface, OrbState::Attached);

                // Send orb_attached event
                self.common
                    .voice_mode_state
                    .send_orb_attached(surface, geo.loc.x, geo.loc.y, geo.size.w, geo.size.h);

                // Focus the receiver window so the voice orb is visible on screen
                let shell = self.common.shell.read();
                let seat = shell.seats.last_active().clone();
                let mapped = shell.element_for_surface(surface).cloned();
                drop(shell);
                if let Some(mapped) = mapped {
                    let focus_target = KeyboardFocusTarget::Element(mapped);
                    info!("Focusing receiver window for voice attach mode");
                    Shell::set_focus(self, Some(&focus_target), &seat, None, false);
                }

                OrbState::Attached
            } else {
                // Shouldn't happen, but fall back to floating
                shell.voice_orb_state.request_show_floating(output.name());
                shell.enter_voice_mode();
                drop(shell);
                self.common
                    .voice_mode_state
                    .send_start_to_default(OrbState::Floating);
                OrbState::Floating
            }
        } else {
            // No focused receiver, use default receiver with floating orb
            info!(
                "No receiver found after all fallbacks - using default receiver on output: {}",
                output.name()
            );
            // Request orb show - will start after window fade completes
            shell.voice_orb_state.request_show_floating(output.name());
            shell.enter_voice_mode();
            drop(shell);
            self.common
                .voice_mode_state
                .send_start_to_default(OrbState::Floating);
            OrbState::Floating
        };

        self.common.voice_mode_state.set_orb_state(orb_state);
        orb_state
    }

    fn deactivate_voice_mode(&mut self) {
        // Send will_stop to the active receiver and wait for ack_stop.
        // The client will respond with ack_stop(freeze=true) to enter thinking
        // mode, or ack_stop(freeze=false) to dismiss immediately.
        if let Some(serial) = self.common.voice_mode_state.send_will_stop() {
            info!(serial, "Sent will_stop, waiting for client ack_stop");
            // The ack response will call either freeze_orb() or complete_deactivation()
            // via the protocol handler. If it times out, check_pending_stop_timeout()
            // will call complete_deactivation().
        } else {
            info!("No active receiver for will_stop - immediate dismiss");
            self.complete_deactivation();
        }
    }

    fn cancel_voice_mode(&mut self) {
        info!("Cancelling voice mode");

        // Send cancel to active receiver
        self.common.voice_mode_state.send_cancel();

        let mut shell = self.common.shell.write();

        // Request orb hide - will start shrinking, then windows fade in
        shell.voice_orb_state.request_hide();

        // Start the exit sequence (orb shrinks first, then windows fade in)
        shell.exit_voice_mode();

        // Update protocol state
        drop(shell);
        self.common.voice_mode_state.set_orb_state(OrbState::Hidden);
    }

    fn freeze_orb(&mut self) {
        info!("Freezing orb for transcription processing (client requested freeze via ack_stop)");

        // Send stop to active receiver so it can trigger transcription
        self.common.voice_mode_state.send_stop();

        let mut shell = self.common.shell.write();

        // Freeze the orb in place (stops pulsing, stays visible)
        shell.voice_orb_state.freeze();

        // Update protocol state
        drop(shell);
        self.common.voice_mode_state.set_orb_state(OrbState::Frozen);
    }

    fn complete_deactivation(&mut self) {
        info!("Completing voice mode deactivation (proceeding with hide)");

        // Send stop to active receiver
        self.common.voice_mode_state.send_stop();

        let mut shell = self.common.shell.write();

        // Request orb hide - will start shrinking, then windows fade in
        shell.voice_orb_state.request_hide();

        // Start the exit sequence (orb shrinks first, then windows fade in)
        shell.exit_voice_mode();

        // Update protocol state
        drop(shell);
        self.common.voice_mode_state.set_orb_state(OrbState::Hidden);

        // Send focus_input to default receiver and grant keyboard focus to its
        // layer surface so the user can immediately type after voice input.
        if let Some(surface) = self
            .common
            .voice_mode_state
            .send_focus_input_to_surface_or_default(None)
        {
            let shell = self.common.shell.read();
            // Only focus a receiver the user can SEE. Voice activation exits home mode,
            // which drops a home-only receiver (the chat panel) to alpha 0 while it stays
            // in the layer map — focusing it then points the keyboard at an invisible
            // surface, and refresh_focus never repairs layer-surface focus. Skipping the
            // grant leaves the pre-voice focus in place, which is the correct fallback.
            let id = surface.id();
            let visible = !shell.is_surface_hidden(&id) && shell.surface_home_visibility(&id).0;
            if !visible {
                debug!("Default receiver is not visible, keeping previous focus");
            } else if let Some(layer_surface) = shell.find_layer_surface_by_wl_surface(&surface) {
                drop(shell);
                let seat = self.common.shell.read().seats.last_active().clone();
                let focus_target = KeyboardFocusTarget::from(layer_surface);
                info!("Setting keyboard focus to default receiver after voice deactivation");
                Shell::set_focus(self, Some(&focus_target), &seat, None, false);
            } else {
                debug!("Default receiver surface is not a layer surface, skipping focus");
            }
        }
    }

    fn check_pending_stop_timeout(&mut self) {
        if self.common.voice_mode_state.has_pending_stop_timeout() {
            info!("will_stop timeout - completing deactivation");
            self.complete_deactivation();
        }
    }

    fn check_frozen_timeout(&mut self) {
        let timed_out = {
            let shell = self.common.shell.read();
            shell.voice_orb_state.has_frozen_timeout()
        };
        if timed_out {
            info!("Frozen orb timeout - dismissing");
            self.dismiss_orb();
        }
    }

    fn on_voice_receiver_registered(&mut self, surface: &WlSurface) {
        // Check if orb is currently frozen - if so, we need to transition to the new window
        let shell = self.common.shell.read();
        if shell.voice_orb_state.orb_state != OrbState::Frozen {
            return;
        }
        drop(shell);

        info!("Voice receiver registered while orb is frozen - checking for transition");

        // Find the window that owns this surface
        let shell = self.common.shell.read();
        let seat = shell.seats.last_active().clone();
        let output = seat.active_output();

        // Search for the mapped element that contains this surface
        let workspace = shell.active_space(&output);
        let window_info = workspace.and_then(|ws| {
            ws.mapped().find_map(|mapped| {
                if let Some(wl_surface) = mapped.active_window().wl_surface()
                    && wl_surface.id() == surface.id()
                {
                    let window_geo = SpaceElement::geometry(mapped);
                    let surface_id = wl_surface.id().to_string();
                    return Some((window_geo, surface_id));
                }
                None
            })
        });
        drop(shell);

        if let Some((window_geo, surface_id)) = window_info {
            info!(
                ?window_geo,
                surface_id, "Transitioning frozen orb to newly registered receiver window"
            );

            let mut shell = self.common.shell.write();
            let output_size = output.geometry().size.as_logical();

            // Start the attach_and_transition animation
            shell
                .voice_orb_state
                .start_attach_and_transition(window_geo, output_size, surface_id);

            // Fade windows back in
            shell.voice_mode_fade_in_immediately();
        } else {
            info!("Could not find window for registered voice receiver surface");
        }
    }

    fn dismiss_orb(&mut self) {
        let shell = self.common.shell.read();
        let was_attached = shell.voice_orb_state.is_frozen_attached();
        let orb_state = shell.voice_orb_state.orb_state;
        let voice_mode = shell.voice_mode_debug();
        let position = shell.voice_orb_state.position;
        let scale = shell.voice_orb_state.scale;
        drop(shell);

        info!(
            "Dismissing frozen orb: was_attached={}, orb_state={:?}, voice_mode={}, position={:?}, scale={}",
            was_attached, orb_state, voice_mode, position, scale
        );

        let mut shell = self.common.shell.write();

        // Request orb hide - will start shrinking animation
        shell.voice_orb_state.request_hide();

        if was_attached {
            // When orb was attached, windows were already visible.
            // Use exit_from_attached() which goes directly to None without
            // any fade animation (exit() would trigger WaitingForOrbShrink → FadingOut
            // which flashes windows to alpha=0 then fades them back in).
            info!("dismiss_orb: was_attached=true, calling exit_voice_mode_from_attached()");
            shell.exit_voice_mode_from_attached();
        } else {
            // When orb was floating, windows were hidden — fade them back in.
            info!("dismiss_orb: was_attached=false, calling voice_mode_fade_in_immediately()");
            shell.voice_mode_fade_in_immediately();
        }

        // Update protocol state
        drop(shell);
        self.common.voice_mode_state.set_orb_state(OrbState::Hidden);
    }

    fn request_activate_voice_mode(&mut self, surface: &WlSurface) {
        info!("On-demand voice mode activation requested by client");

        // Mark as on-demand activation so key release behavior changes
        self.common.voice_mode_state.mark_activated_on_demand();

        // Activate voice mode using the requesting surface
        let orb_state = self.activate_voice_mode(Some(surface));
        if orb_state != OrbState::Hidden {
            self.common.voice_mode_state.mark_voice_activated();
            info!(?orb_state, "Voice mode activated on demand (button click)");
        } else {
            // Activation failed, clear on-demand flag
            self.common.voice_mode_state.clear_on_demand();
            info!("On-demand voice mode activation failed - no suitable receiver");
        }
    }
}

delegate_voice_mode!(State);

impl State {
    /// Find a mapped element's geometry by searching across all outputs.
    /// This is needed because the element may be on a different output than the pointer.
    fn find_element_geo_across_outputs(
        shell: &crate::shell::Shell,
        mapped: &crate::shell::CosmicMapped,
    ) -> Option<(
        smithay::utils::Rectangle<i32, smithay::utils::Logical>,
        String,
    )> {
        shell.outputs().find_map(|out| {
            let workspace = shell.active_space(out)?;
            let local_geo = workspace.element_geometry(mapped)?;
            let geo = local_geo.to_global(out).as_logical();
            let surface_id = mapped.active_window().wl_surface()?.id().to_string();
            Some((geo, surface_id))
        })
    }

    /// Try to find the last-focused voice receiver and its on-screen geometry.
    /// Returns `Some((surface, Some((geo, id))))` when the window is mapped on any output.
    fn find_last_focused_receiver_geo(
        &self,
        shell: &crate::shell::Shell,
        _output: &smithay::output::Output,
    ) -> Option<(
        WlSurface,
        Option<(
            smithay::utils::Rectangle<i32, smithay::utils::Logical>,
            String,
        )>,
    )> {
        let last_surface = self.common.voice_mode_state.get_last_focused_receiver()?;
        if !self
            .common
            .voice_mode_state
            .has_receiver_for_surface(&last_surface)
        {
            info!("Last-focused receiver surface no longer has a registered receiver");
            return None;
        }
        info!(
            "Using last-focused voice receiver (surface_id: {})",
            last_surface.id()
        );

        // Search all outputs for the mapped window matching this surface
        let geo_and_id = shell.outputs().find_map(|out| {
            let workspace = shell.active_space(out)?;
            workspace.mapped().find_map(|mapped| {
                let active = mapped.active_window();
                let wl = active.wl_surface()?;
                if *wl != last_surface {
                    return None;
                }
                let local_geo = workspace.element_geometry(mapped)?;
                let geo = local_geo.to_global(out).as_logical();
                let surface_id = wl.id().to_string();
                Some((geo, surface_id))
            })
        });
        // If we couldn't find geometry (window may be minimized), still return
        // the surface so it can be used; activate_voice_mode will fall back to floating.
        info!(
            geo_found = geo_and_id.is_some(),
            "Last-focused receiver geo search complete"
        );
        Some((last_surface, geo_and_id))
    }
}
