// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::grabs::SeatMoveGrabState,
    state::ClientState,
    utils::prelude::*,
    wayland::{
        handlers::background_effect::ComputedBlurRegionCachedState,
        protocols::corner_radius::{CacheableCorners, CacheablePadding},
    },
};
use calloop::Interest;
use smithay::{
    backend::{
        input::InputTime,
        renderer::{
            element::{Kind, surface::KindEvaluation},
            utils::{on_commit_buffer_handler, with_renderer_surface_state},
        },
    },
    desktop::{LayerSurface, PopupKind, WindowSurfaceType, layer_map_for_output},
    reexports::wayland_server::{Client, DisplayHandle, Resource, protocol::wl_surface::WlSurface},
    utils::{Clock, Logical, Monotonic, SERIAL_COUNTER, Size, Time},
    wayland::{
        alpha_modifier::AlphaModifierSurfaceCachedState,
        compositor::{
            BufferAssignment, Cacheable, CompositorClientState, CompositorHandler, CompositorState,
            RectangleKind, RegionAttributes, SUBSURFACE_ROLE, SubsurfaceCachedState,
            SurfaceAttributes, SurfaceData, TraversalAction, add_blocker, add_post_commit_hook,
            add_pre_commit_hook, get_children, get_parent, is_sync_subsurface, with_states,
            with_surface_tree_downward,
        },
        dmabuf::get_dmabuf,
        drm_syncobj::DrmSyncobjCachedState,
        presentation::PresentationFeedbackCachedState,
        seat::WaylandFocus,
        shell::{
            wlr_layer::LayerSurfaceAttributes,
            xdg::{
                SurfaceCachedState, ToplevelCachedState, ToplevelSurface, XDG_TOPLEVEL_ROLE,
                XdgPopupSurfaceRoleAttributes, XdgToplevelSurfaceRoleAttributes,
            },
        },
        viewporter::ViewportCachedState,
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
    with_states(surface.wl_surface(), |states| {
        states
            .data_map
            .get::<Mutex<LayerSurfaceAttributes>>()
            .unwrap()
            .lock()
            .unwrap()
            .initial_configure_sent
    })
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
        let diff = Time::elapsed(last, clock.now());
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

pub fn frame_time_filter_fn(states: &SurfaceData) -> Kind {
    let clock = Clock::<Monotonic>::new();
    const _20_FPS: Duration = Duration::from_nanos(1_000_000_000 / 20);

    if frame_time_estimation(&clock, states).is_some_and(|dur| dur <= _20_FPS) {
        Kind::ScanoutCandidate
    } else {
        Kind::Unspecified
    }
}

pub const FRAME_TIME_FILTER: KindEvaluation = KindEvaluation::Dynamic(frame_time_filter_fn);

#[derive(Debug, Default)]
struct SurfaceCommitClassification {
    recorded: bool,
    coalescible: bool,
    client_scale_bits: Option<u64>,
    subsurface_tree: Vec<(u32, Option<u32>, i32, i32)>,
}

impl Cacheable for SurfaceCommitClassification {
    fn commit(&mut self, _dh: &DisplayHandle) -> Self {
        std::mem::take(self)
    }

    fn merge_into(self, into: &mut Self, _dh: &DisplayHandle) {
        if !self.recorded {
            return;
        }
        if into.recorded {
            into.coalescible &= self.coalescible;
            into.client_scale_bits = self.client_scale_bits;
            into.subsurface_tree = self.subsurface_tree;
        } else {
            *into = self;
        }
    }
}

fn regions_equal(a: &Option<RegionAttributes>, b: &Option<RegionAttributes>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(a), Some(b)) if a.rects.len() == b.rects.len() => {
            a.rects
                .iter()
                .zip(&b.rects)
                .all(|((a_kind, a_rect), (b_kind, b_rect))| {
                    matches!(
                        (a_kind, b_kind),
                        (RectangleKind::Add, RectangleKind::Add)
                            | (RectangleKind::Subtract, RectangleKind::Subtract)
                    ) && a_rect == b_rect
                })
        }
        _ => false,
    }
}

fn subsurface_tree(surface: &WlSurface) -> Vec<(u32, Option<u32>, i32, i32)> {
    let mut root = surface.clone();
    while let Some(parent) = get_parent(&root) {
        root = parent;
    }

    let mut tree = Vec::new();
    with_surface_tree_downward(
        &root,
        None,
        |surface, _, _| TraversalAction::DoChildren(Some(surface.id().protocol_id())),
        |surface, states, parent| {
            let location = parent.map_or((0, 0), |_| {
                let mut state = states.cached_state.get::<SubsurfaceCachedState>();
                let location = state.pending().location;
                (location.x, location.y)
            });
            tree.push((surface.id().protocol_id(), *parent, location.0, location.1));
        },
        |_, _, _| true,
    );
    tree
}

fn surface_state_unchanged(states: &SurfaceData, frame_callback_required: bool) -> bool {
    if !matches!(states.role, Some(XDG_TOPLEVEL_ROLE | SUBSURFACE_ROLE)) {
        return false;
    }

    let surface_unchanged = {
        let mut state = states.cached_state.get::<SurfaceAttributes>();
        let current = {
            let current = state.current();
            (
                current.buffer_scale,
                current.buffer_transform,
                current.opaque_region.clone(),
                current.input_region.clone(),
            )
        };
        let pending = state.pending();
        (!frame_callback_required || !pending.frame_callbacks.is_empty())
            && pending.buffer.is_none()
            && pending.buffer_delta.is_none()
            && pending.damage.is_empty()
            && pending.buffer_scale == current.0
            && pending.buffer_transform == current.1
            && regions_equal(&pending.opaque_region, &current.2)
            && regions_equal(&pending.input_region, &current.3)
    };
    if !surface_unchanged {
        return false;
    }

    let viewport_unchanged = {
        let mut state = states.cached_state.get::<ViewportCachedState>();
        let current = {
            let current = state.current();
            (current.src, current.dst)
        };
        let pending = state.pending();
        (pending.src, pending.dst) == current
    };
    let alpha_unchanged = {
        let mut state = states.cached_state.get::<AlphaModifierSurfaceCachedState>();
        let current = state.current().multiplier();
        state.pending().multiplier() == current
    };
    let blur_unchanged = {
        let mut state = states.cached_state.get::<ComputedBlurRegionCachedState>();
        let current = state.current().blur_region.clone();
        state.pending().blur_region == current
    };
    let corners_unchanged = {
        let mut state = states.cached_state.get::<CacheableCorners>();
        let current = state
            .current()
            .0
            .map(|c| (c.top_left, c.top_right, c.bottom_right, c.bottom_left));
        state
            .pending()
            .0
            .map(|c| (c.top_left, c.top_right, c.bottom_right, c.bottom_left))
            == current
    };
    let padding_unchanged = {
        let mut state = states.cached_state.get::<CacheablePadding>();
        let current = state
            .current()
            .0
            .map(|p| (p.top, p.right, p.bottom, p.left));
        state
            .pending()
            .0
            .map(|p| (p.top, p.right, p.bottom, p.left))
            == current
    };
    let role_unchanged = match states.role {
        Some(XDG_TOPLEVEL_ROLE) => {
            let xdg_surface_unchanged = {
                let mut state = states.cached_state.get::<SurfaceCachedState>();
                let current = {
                    let current = state.current();
                    (current.geometry, current.min_size, current.max_size)
                };
                let pending = state.pending();
                (pending.geometry, pending.min_size, pending.max_size) == current
            };
            let toplevel_unchanged = {
                let mut state = states.cached_state.get::<ToplevelCachedState>();
                let current = state.current().last_acked.as_ref().map(|c| c.serial);
                states
                    .data_map
                    .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
                    .and_then(|role| role.lock().unwrap().last_acked.as_ref().map(|c| c.serial))
                    == current
            };
            xdg_surface_unchanged && toplevel_unchanged
        }
        Some(SUBSURFACE_ROLE) => {
            let mut state = states.cached_state.get::<SubsurfaceCachedState>();
            let current = state.current().location;
            state.pending().location == current
        }
        _ => false,
    };
    let no_presentation_feedback = states
        .cached_state
        .get::<PresentationFeedbackCachedState>()
        .pending()
        .callbacks
        .is_empty();
    let no_explicit_sync = {
        let mut state = states.cached_state.get::<DrmSyncobjCachedState>();
        let pending = state.pending();
        pending.acquire_point.is_none() && pending.release_point.is_none()
    };

    viewport_unchanged
        && alpha_unchanged
        && blur_unchanged
        && corners_unchanged
        && padding_unchanged
        && role_unchanged
        && no_presentation_feedback
        && no_explicit_sync
}

fn synchronized_subtree_unchanged(surface: &WlSurface) -> bool {
    if !with_states(surface, |states| surface_state_unchanged(states, false)) {
        return false;
    }

    get_children(surface)
        .iter()
        .all(synchronized_subtree_unchanged)
}

fn synchronized_descendants_unchanged(surface: &WlSurface) -> bool {
    get_children(surface)
        .into_iter()
        .filter(is_sync_subsurface)
        .all(|child| synchronized_subtree_unchanged(&child))
}

fn frame_callback_only_commit(
    states: &SurfaceData,
    client_scale_bits: Option<u64>,
    subsurface_tree: &[(u32, Option<u32>, i32, i32)],
    synchronized_descendants_unchanged: bool,
) -> bool {
    if !surface_state_unchanged(states, true) {
        return false;
    }

    let (previous_client_scale_bits, previous_subsurface_tree) = {
        let mut state = states.cached_state.get::<SurfaceCommitClassification>();
        let current = state.current();
        (current.client_scale_bits, current.subsurface_tree.clone())
    };

    client_scale_bits.is_some()
        && client_scale_bits == previous_client_scale_bits
        && subsurface_tree == previous_subsurface_tree
        && synchronized_descendants_unchanged
}

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
            let client_scale_bits = surface
                .client()
                .map(|client| client_compositor_state(&client).client_scale().to_bits());
            let subsurface_tree = subsurface_tree(surface);
            let synchronized_descendants_unchanged = synchronized_descendants_unchanged(surface);
            let maybe_dmabuf = with_states(surface, |surface_data| {
                let coalescible = frame_callback_only_commit(
                    surface_data,
                    client_scale_bits,
                    &subsurface_tree,
                    synchronized_descendants_unchanged,
                );
                *surface_data
                    .cached_state
                    .get::<SurfaceCommitClassification>()
                    .pending() = SurfaceCommitClassification {
                    recorded: true,
                    coalescible,
                    client_scale_bits,
                    subsurface_tree,
                };

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
                if let Some(acquire_point) = acquire_point
                    && let Ok((blocker, source)) = acquire_point.generate_blocker()
                {
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
                        return;
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
        let commit_classification = with_states(surface, |states| {
            let mut classification = states.cached_state.get::<SurfaceCommitClassification>();
            let classification = classification.current();
            let coalescible = classification
                .recorded
                .then_some(classification.coalescible);
            classification.recorded = false;
            classification.coalescible = false;
            coalescible
        });

        // first load the buffer for various smithay helper functions (which also initializes the RendererSurfaceState)
        on_commit_buffer_handler::<Self>(surface);

        // and refresh smithays internal state
        self.common.on_commit(surface);

        // handle initial configure events and map windows if necessary
        let mapped = self.send_initial_configure_and_map(surface);

        let mut shell = self.common.shell.write();

        // schedule a new render
        if let Some(output) = shell.visible_output_for_surface(surface) {
            match commit_classification {
                Some(coalescible) => self
                    .backend
                    .schedule_render_for_surface_commit(output, coalescible && !mapped),
                None if is_sync_subsurface(surface) => {}
                None => self.backend.schedule_render(output),
            }
        }

        if mapped {
            return;
        }

        if let Some(popup) = self.common.popups.find_popup(surface) {
            xdg_popup_ensure_initial_configure(&popup);
            // The IME popup need to be repositioned when the size changed
            if let PopupKind::InputMethod(_) = popup {
                shell.unconstrain_popup(&popup);
            }
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
                if let Some(stack) = window.stack_ref() {
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
                    seat.get_pointer().unwrap().unset_grab(
                        self,
                        SERIAL_COUNTER.next_serial(),
                        InputTime::now(),
                    );
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
                    &mut shell,
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
            && let Some(toplevel) = pending.surface.0.toplevel()
        {
            let initial_size = if let Some(output) = pending.fullscreen.as_ref() {
                Some(output.geometry().size.as_logical())
            } else if pending.maximized {
                let active_output = shell.seats.last_active().active_output();
                let zone = layer_map_for_output(&active_output).non_exclusive_zone();
                Some(zone.size)
            } else {
                None
            };
            if toplevel_ensure_initial_configure(toplevel, initial_size)
                && with_renderer_surface_state(surface, |state| state.buffer().is_some())
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

        if let Some(layer_surface) = shell
            .pending_layers
            .iter()
            .find(|pending| pending.surface.wl_surface() == surface)
            .map(|pending| pending.surface.clone())
            && !layer_surface_check_inital_configure(&layer_surface)
        {
            // compute initial dimensions by mapping
            if let Some(target) = shell.map_layer(&layer_surface) {
                let seat = shell.seats.last_active().clone();
                std::mem::drop(shell);
                Shell::set_focus(self, Some(&target), &seat, None, false);
            }
            layer_surface.layer_surface().send_configure();
            return true;
        };

        false
    }
}
