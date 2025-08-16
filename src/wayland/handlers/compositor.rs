// SPDX-License-Identifier: GPL-3.0-only

use crate::{shell::grabs::SeatMoveGrabState, state::ClientState, utils::prelude::*};
use calloop::Interest;
use smithay::{
    backend::renderer::{
        element::{surface::KindEvaluation, Kind},
        utils::{on_commit_buffer_handler, with_renderer_surface_state},
    },
    delegate_compositor,
    desktop::{layer_map_for_output, LayerSurface, PopupKind, WindowSurfaceType},
    reexports::wayland_server::{protocol::wl_surface::WlSurface, Client, Resource},
    utils::{Clock, Logical, Monotonic, Size, Time, SERIAL_COUNTER},
    wayland::{
        compositor::{
            add_blocker, add_post_commit_hook, add_pre_commit_hook, with_states,
            with_surface_tree_downward, BufferAssignment, CompositorClientState, CompositorHandler,
            CompositorState, SurfaceAttributes, SurfaceData, TraversalAction,
        },
        dmabuf::get_dmabuf,
        drm_syncobj::DrmSyncobjCachedState,
        seat::WaylandFocus,
        shell::{
            wlr_layer::LayerSurfaceAttributes,
            xdg::{
                ToplevelSurface, XdgPopupSurfaceRoleAttributes, XdgToplevelSurfaceRoleAttributes,
            },
        },
    },
    xwayland::XWaylandClientData,
};
use std::{collections::VecDeque, sync::Mutex, time::Duration};

fn toplevel_ensure_initial_configure(
    toplevel: &ToplevelSurface,
    size: Option<Size<i32, Logical>>,
) -> bool {
    // send the initial configure if relevant
    let initial_configure_sent = with_states(toplevel.wl_surface(), |states| {
        states
            .data_map
            .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
            .unwrap()
            .lock()
            .unwrap()
            .initial_configure_sent
    });
    if !initial_configure_sent {
        toplevel.with_pending_state(|states| states.size = size);
        toplevel.send_configure();
    }
    initial_configure_sent
}

fn xdg_popup_ensure_initial_configure(popup: &PopupKind) {
    if let PopupKind::Xdg(popup) = popup {
        let initial_configure_sent = with_states(popup.wl_surface(), |states| {
            states
                .data_map
                .get::<Mutex<XdgPopupSurfaceRoleAttributes>>()
                .unwrap()
                .lock()
                .unwrap()
                .initial_configure_sent
        });
        if !initial_configure_sent {
            // NOTE: This should never fail as the initial configure is always
            // allowed.
            popup.send_configure().expect("initial configure failed");
        }
    }
}

fn layer_surface_check_inital_configure(surface: &LayerSurface) -> bool {
    // send the initial configure if relevant
    let initial_configure_sent = with_states(surface.wl_surface(), |states| {
        states
            .data_map
            .get::<Mutex<LayerSurfaceAttributes>>()
            .unwrap()
            .lock()
            .unwrap()
            .initial_configure_sent
    });

    initial_configure_sent
}

pub fn client_compositor_state(client: &Client) -> &CompositorClientState {
    if let Some(state) = client.get_data::<XWaylandClientData>() {
        return &state.compositor_state;
    }
    if let Some(state) = client.get_data::<ClientState>() {
        return &state.compositor_client_state;
    }
    panic!("Unknown client data type")
}

#[derive(Debug)]
struct FrametimeData {
    last_commit: Option<Time<Monotonic>>,
    last_diffs: VecDeque<Duration>,
    estimation: Duration,
}

impl Default for FrametimeData {
    fn default() -> Self {
        FrametimeData {
            last_commit: None,
            last_diffs: VecDeque::with_capacity(100),
            estimation: Duration::MAX,
        }
    }
}

pub fn frame_time_estimation(clock: &Clock<Monotonic>, states: &SurfaceData) -> Option<Duration> {
    let data = states
        .data_map
        .get::<Mutex<FrametimeData>>()?
        .lock()
        .unwrap();
    if let Some(ref last) = data.last_commit {
        // if the time since the last commit is already higher than our estimation,
        // there is no reason to not use that as a better "guess"
        let diff = Time::elapsed(&last, clock.now());
        Some(diff.max(data.estimation))
    } else {
        Some(data.estimation)
    }
}

pub fn recursive_frame_time_estimation(
    clock: &Clock<Monotonic>,
    surface: &WlSurface,
) -> Option<Duration> {
    let mut overall_estimate = None;
    with_surface_tree_downward(
        surface,
        (),
        |_, _, _| TraversalAction::DoChildren(()),
        |_, data, _| {
            let surface_estimate = frame_time_estimation(clock, data);
            overall_estimate = match (overall_estimate, surface_estimate) {
                (x, None) => x,
                (None, Some(estimate)) => Some(estimate),
                (Some(a), Some(b)) => Some(a.min(b)),
            };
        },
        |_, _, _| true,
    );
    overall_estimate
}

pub const FRAME_TIME_FILTER: KindEvaluation = KindEvaluation::Dynamic({
    fn frame_time_filter_fn(states: &SurfaceData) -> Kind {
        let clock = Clock::<Monotonic>::new();
        const _20_FPS: Duration = Duration::from_nanos(1_000_000_000 / 20);

        if frame_time_estimation(&clock, states).is_some_and(|dur| dur <= _20_FPS) {
            Kind::ScanoutCandidate
        } else {
            Kind::Unspecified
        }
    }

    frame_time_filter_fn
});

impl CompositorHandler for State {
    fn compositor_state(&mut self) -> &mut CompositorState {
        &mut self.common.compositor_state
    }

    fn client_compositor_state<'a>(&self, client: &'a Client) -> &'a CompositorClientState {
        client_compositor_state(client)
    }

    fn new_surface(&mut self, surface: &WlSurface) {
        add_pre_commit_hook::<Self, _>(surface, move |state, _dh, surface| {
            let mut acquire_point = None;
            let maybe_dmabuf = with_states(surface, |surface_data| {
                acquire_point = surface_data
                    .cached_state
                    .get::<DrmSyncobjCachedState>()
                    .pending()
                    .acquire_point
                    .clone();
                surface_data
                    .cached_state
                    .get::<SurfaceAttributes>()
                    .pending()
                    .buffer
                    .as_ref()
                    .and_then(|assignment| match assignment {
                        BufferAssignment::NewBuffer(buffer) => get_dmabuf(buffer).ok().cloned(),
                        _ => None,
                    })
            });
            if let Some(dmabuf) = maybe_dmabuf {
                if let Some(acquire_point) = acquire_point {
                    if let Ok((blocker, source)) = acquire_point.generate_blocker() {
                        let client = surface.client().unwrap();
                        let res = state.common.event_loop_handle.insert_source(
                            source,
                            move |_, _, state| {
                                let dh = state.common.display_handle.clone();
                                state
                                    .client_compositor_state(&client)
                                    .blocker_cleared(state, &dh);
                                Ok(())
                            },
                        );
                        if res.is_ok() {
                            add_blocker(surface, blocker);
                            return;
                        }
                    }
                }
                if let Ok((blocker, source)) = dmabuf.generate_blocker(Interest::READ) {
                    let client = surface.client().unwrap();
                    let res =
                        state
                            .common
                            .event_loop_handle
                            .insert_source(source, move |_, _, state| {
                                let dh = state.common.display_handle.clone();
                                state
                                    .client_compositor_state(&client)
                                    .blocker_cleared(state, &dh);
                                Ok(())
                            });
                    if res.is_ok() {
                        add_blocker(surface, blocker);
                    }
                }
            }
        });

        add_post_commit_hook::<Self, _>(surface, |state, _dh, surface| {
            let now = state.common.clock.now();
            with_states(surface, |states| {
                let mut data = states
                    .data_map
                    .get_or_insert_threadsafe::<Mutex<FrametimeData>, _>(Default::default)
                    .lock()
                    .unwrap();
                if let Some(ref last) = data.last_commit {
                    let diff = Time::elapsed(last, now);
                    data.last_diffs.push_back(diff);
                    if data.last_diffs.len() > 100 {
                        data.last_diffs.pop_front();
                    }
                    data.estimation = data
                        .last_diffs
                        .iter()
                        .fold(Duration::ZERO, |acc, new| acc.saturating_add(*new))
                        / (data.last_diffs.len() as u32);
                }
                data.last_commit = Some(now);
            });
        });
    }

    fn commit(&mut self, surface: &WlSurface) {
        // first load the buffer for various smithay helper functions (which also initializes the RendererSurfaceState)
        on_commit_buffer_handler::<Self>(surface);

        // and refresh smithays internal state
        self.common.on_commit(surface);

        // handle initial configure events and map windows if necessary
        let mapped = self.send_initial_configure_and_map(surface);

        let mut shell = self.common.shell.write();

        // schedule a new render
        if let Some(output) = shell.visible_output_for_surface(surface) {
            self.backend.schedule_render(&output);
        }

        if mapped {
            return;
        }

        if let Some(popup) = self.common.popups.find_popup(surface) {
            xdg_popup_ensure_initial_configure(&popup);
            return;
        }

        if with_renderer_surface_state(surface, |state| state.buffer().is_none()).unwrap_or(false) {
            // handle null-commits causing weird conflicts:

            // session-lock disallows null commits

            // if it was a move-grab?
            let seat = shell.seats.last_active().clone();
            let moved_window = seat
                .user_data()
                .get::<SeatMoveGrabState>()
                .unwrap()
                .lock()
                .unwrap()
                .as_ref()
                .and_then(|state| {
                    state
                        .element()
                        .windows()
                        .any(|(s, _)| {
                            s.wl_surface()
                                .as_deref()
                                .map(|s| s == surface)
                                .unwrap_or(false)
                        })
                        .then(|| state.element())
                });
            if let Some(window) = moved_window {
                if window.is_stack() {
                    let stack = window.stack_ref().unwrap();
                    if let Some(i) = stack.surfaces().position(|s| {
                        s.wl_surface()
                            .as_deref()
                            .map(|s| s == surface)
                            .unwrap_or(false)
                    }) {
                        stack.remove_idx(i);
                    }
                } else {
                    std::mem::drop(shell);
                    seat.get_pointer()
                        .unwrap()
                        .unset_grab(self, SERIAL_COUNTER.next_serial(), 0);
                    return;
                }
            }

            // if it was a layer-shell surface?
            // ignore, that will affect recompute normally

            // if it was a sticky / floating / tiled window
            // we could unmap, I guess?

            // if it was an x11 surface => do nothing, we will get a separate UnmapNotify anyway
        } else {
            // handle some special cases, like grabs and changing layer surfaces

            // If we would re-position the window inside the grab we would get a weird jittery animation.
            // We only want to resize once the client has acknoledged & commited the new size,
            // so we need to carefully track the state through different handlers.
            if let Some(element) = shell.element_for_surface(surface).cloned() {
                crate::shell::layout::floating::ResizeSurfaceGrab::apply_resize_to_location(
                    element.clone(),
                    &mut *shell,
                );
            }
        }

        // re-arrange layer-surfaces (commits may change size and positioning)
        let layer_output = shell
            .outputs()
            .find(|o| {
                let map = layer_map_for_output(o);
                map.layer_for_surface(surface, WindowSurfaceType::ALL)
                    .is_some()
            })
            .cloned();

        if let Some(output) = layer_output {
            let changed = layer_map_for_output(&output).arrange();
            if changed {
                shell.workspaces.recalculate();
            }
        }
    }
}

impl State {
    fn send_initial_configure_and_map(&mut self, surface: &WlSurface) -> bool {
        let mut shell = self.common.shell.write();

        if let Some(pending) = shell
            .pending_windows
            .iter()
            .find(|pending| pending.surface.wl_surface().as_deref() == Some(surface))
        {
            if let Some(toplevel) = pending.surface.0.toplevel() {
                let initial_size = if let Some(output) = pending.fullscreen.as_ref() {
                    Some(output.geometry().size.as_logical())
                } else if pending.maximized {
                    let active_output = shell.seats.last_active().active_output();
                    let zone = layer_map_for_output(&active_output).non_exclusive_zone();
                    Some(zone.size)
                } else {
                    None
                };
                if toplevel_ensure_initial_configure(&toplevel, initial_size)
                    && with_renderer_surface_state(&surface, |state| state.buffer().is_some())
                        .unwrap_or(false)
                {
                    let window = pending.surface.clone();
                    window.on_commit();
                    let res = shell.map_window(
                        &window,
                        &mut self.common.toplevel_info_state,
                        &mut self.common.workspace_state,
                        &self.common.event_loop_handle,
                    );
                    if let Some(target) = res {
                        let seat = shell.seats.last_active().clone();
                        std::mem::drop(shell);
                        Shell::set_focus(self, Some(&target), &seat, None, true);
                        return true;
                    }
                }
            }
        }

        if let Some(layer_surface) = shell
            .pending_layers
            .iter()
            .find(|pending| pending.surface.wl_surface() == surface)
            .map(|pending| pending.surface.clone())
        {
            if !layer_surface_check_inital_configure(&layer_surface) {
                // compute initial dimensions by mapping
                if let Some(target) = shell.map_layer(&layer_surface) {
                    let seat = shell.seats.last_active().clone();
                    std::mem::drop(shell);
                    Shell::set_focus(self, Some(&target), &seat, None, false);
                }
                layer_surface.layer_surface().send_configure();
                return true;
            }
        };

        false
    }
}

delegate_compositor!(State);
