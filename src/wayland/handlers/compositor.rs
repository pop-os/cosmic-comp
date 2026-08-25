// SPDX-License-Identifier: GPL-3.0-only

use crate::{shell::grabs::SeatMoveGrabState, state::ClientState, utils::prelude::*};
use calloop::Interest;
use smithay::{
    backend::renderer::{
        element::{Kind, surface::KindEvaluation},
        utils::{
            CommitCounter, RendererSurfaceStateUserData, SurfaceView, on_commit_buffer_handler,
            with_renderer_surface_state,
        },
    },
    desktop::{
        LayerSurface, PopupKind, WindowSurfaceType, layer_map_for_output, space::SpaceElement,
    },
    reexports::wayland_server::{
        Client, Resource, backend::ObjectId, protocol::wl_surface::WlSurface,
    },
    utils::{Clock, Logical, Monotonic, Rectangle, SERIAL_COUNTER, Size, Time, Transform},
    wayland::{
        alpha_modifier::AlphaModifierSurfaceCachedState,
        compositor::{
            BufferAssignment, CompositorClientState, CompositorHandler, CompositorState,
            SurfaceAttributes, SurfaceData, TraversalAction, add_blocker, add_post_commit_hook,
            add_pre_commit_hook, with_states, with_surface_tree_downward,
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

#[derive(Debug, PartialEq)]
struct RendererStateSignature {
    commit: CommitCounter,
    buffer: Option<ObjectId>,
    view: Option<SurfaceView>,
    buffer_scale: i32,
    buffer_transform: Transform,
    opaque_regions: Vec<Rectangle<i32, Logical>>,
}

#[derive(Debug, PartialEq)]
struct SurfaceStateSignature {
    surface: ObjectId,
    alpha_multiplier: Option<u32>,
    renderer: Option<RendererStateSignature>,
}

type SurfaceTreeSignature = Vec<SurfaceStateSignature>;

#[derive(Debug, Default)]
struct LastSurfaceTreeSignature(Mutex<Option<SurfaceTreeSignature>>);

fn surface_tree_commit_state(surface: &WlSurface) -> (SurfaceTreeSignature, bool) {
    let mut signature = Vec::new();
    let mut has_frame_callbacks = false;

    with_surface_tree_downward(
        surface,
        (),
        |_, _, _| TraversalAction::DoChildren(()),
        |surface, states, _| {
            let alpha_multiplier = states
                .cached_state
                .get::<AlphaModifierSurfaceCachedState>()
                .current()
                .multiplier();
            has_frame_callbacks |= !states
                .cached_state
                .get::<SurfaceAttributes>()
                .current()
                .frame_callbacks
                .is_empty();
            let renderer = states
                .data_map
                .get::<RendererSurfaceStateUserData>()
                .map(|state| {
                    let state = state.lock().unwrap();
                    RendererStateSignature {
                        commit: state.current_commit(),
                        buffer: state.buffer().map(|buffer| Resource::id(&**buffer)),
                        view: state.view(),
                        buffer_scale: state.buffer_scale(),
                        buffer_transform: state.buffer_transform(),
                        opaque_regions: state.opaque_regions().unwrap_or_default().to_vec(),
                    }
                });

            signature.push(SurfaceStateSignature {
                surface: surface.id(),
                alpha_multiplier,
                renderer,
            });
        },
        |_, _, _| true,
    );

    (signature, has_frame_callbacks)
}

fn update_surface_tree_signature(surface: &WlSurface) -> (bool, bool) {
    let (current, has_frame_callbacks) = surface_tree_commit_state(surface);

    let changed = with_states(surface, |states| {
        let last = states
            .data_map
            .get_or_insert_threadsafe(LastSurfaceTreeSignature::default);
        let mut last = last.0.lock().unwrap();
        let changed = last.as_ref() != Some(&current);
        *last = Some(current);
        changed
    });

    (changed, has_frame_callbacks)
}

#[derive(Debug, PartialEq, Eq)]
enum CommitAction {
    Render,
    FrameCallbacksOnly,
    None,
}

fn commit_action(
    renderer_state_changed: bool,
    mapped: bool,
    geometry_before: Option<Rectangle<i32, Logical>>,
    geometry_after: Option<Rectangle<i32, Logical>>,
    has_frame_callbacks: bool,
) -> CommitAction {
    let needs_render = renderer_state_changed
        || mapped
        || geometry_before.is_none()
        || geometry_before != geometry_after;

    if needs_render {
        CommitAction::Render
    } else if has_frame_callbacks {
        CommitAction::FrameCallbacksOnly
    } else {
        CommitAction::None
    }
}

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
        let geometry_before = self
            .common
            .shell
            .read()
            .element_for_surface(surface)
            .map(SpaceElement::geometry);

        // first load the buffer for various smithay helper functions (which also initializes the RendererSurfaceState)
        on_commit_buffer_handler::<Self>(surface);
        let (renderer_state_changed, has_frame_callbacks) = update_surface_tree_signature(surface);

        // and refresh smithays internal state
        self.common.on_commit(surface);

        // handle initial configure events and map windows if necessary
        let mapped = self.send_initial_configure_and_map(surface);

        let mut shell = self.common.shell.write();
        let geometry_after = shell
            .element_for_surface(surface)
            .map(SpaceElement::geometry);

        // Only ordinary, already-mapped windows with unchanged renderer and geometry state can
        // prove that this commit has no visual effect. Keep every ambiguous surface role and
        // lifecycle transition on the existing render path.
        let action = commit_action(
            renderer_state_changed,
            mapped,
            geometry_before,
            geometry_after,
            has_frame_callbacks,
        );
        if let Some(output) = shell.visible_output_for_surface(surface) {
            match action {
                CommitAction::Render => self.backend.schedule_render(output),
                CommitAction::FrameCallbacksOnly => self.backend.schedule_frame_callbacks(output),
                CommitAction::None => {}
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

#[cfg(test)]
mod tests {
    use super::*;

    fn geometry() -> Rectangle<i32, Logical> {
        Rectangle::from_size((800, 600).into())
    }

    #[test]
    fn unchanged_mapped_window_commit_does_not_need_render() {
        let geometry = geometry();

        assert_eq!(
            commit_action(false, false, Some(geometry), Some(geometry), false,),
            CommitAction::None,
        );
    }

    #[test]
    fn unchanged_commit_with_callback_only_schedules_callbacks() {
        let geometry = geometry();

        assert_eq!(
            commit_action(false, false, Some(geometry), Some(geometry), true,),
            CommitAction::FrameCallbacksOnly,
        );
    }

    #[test]
    fn visual_or_ambiguous_commit_needs_render() {
        let geometry = geometry();
        let moved = Rectangle::new((1, 0).into(), geometry.size);

        assert_eq!(
            commit_action(true, false, Some(geometry), Some(geometry), false,),
            CommitAction::Render,
        );
        assert_eq!(
            commit_action(false, true, Some(geometry), Some(geometry), false,),
            CommitAction::Render,
        );
        assert_eq!(
            commit_action(false, false, Some(geometry), Some(moved), false),
            CommitAction::Render,
        );
        assert_eq!(
            commit_action(false, false, None, None, false),
            CommitAction::Render,
        );
        assert_eq!(
            commit_action(false, false, Some(geometry), None, false),
            CommitAction::Render,
        );
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
