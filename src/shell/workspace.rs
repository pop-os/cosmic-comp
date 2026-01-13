use crate::shell::focus::FocusTarget;
use crate::shell::layout::tiling::RestoreTilingState;
use crate::wayland::handlers::xdg_activation::ActivationContext;
use crate::{
    backend::render::{
        BackdropShader,
        element::{AsGlowRenderer, FromGlesError},
    },
    shell::{
        ANIMATION_DURATION, OverviewMode, SeatMoveGrabState,
        layout::{floating::FloatingLayout, tiling::TilingLayout},
    },
    state::State,
    utils::{prelude::*, tween::EaseRectangle},
    wayland::{
        handlers::screencopy::ScreencopySessions,
        protocols::{
            toplevel_info::{toplevel_enter_output, toplevel_leave_output},
            workspace::{WorkspaceHandle, WorkspaceUpdateGuard},
        },
    },
};
use cosmic_comp_config::AppearanceConfig;
use cosmic_comp_config::workspace::{OutputMatch, PinnedWorkspace};

use cosmic::theme::CosmicTheme;
use cosmic_protocols::workspace::v2::server::zcosmic_workspace_handle_v2::TilingState;
use id_tree::Tree;
use indexmap::IndexSet;
use keyframe::{ease, functions::EaseInOutCubic};
use smithay::output::WeakOutput;
use smithay::{
    backend::renderer::{
        ImportAll, ImportMem, Renderer,
        element::{
            Element, Id, RenderElement, surface::WaylandSurfaceRenderElement,
            texture::TextureRenderElement, utils::RescaleRenderElement,
        },
        gles::GlesTexture,
        glow::GlowRenderer,
        utils::{DamageSet, OpaqueRegions},
    },
    desktop::{WindowSurfaceType, layer_map_for_output, space::SpaceElement},
    input::Seat,
    output::Output,
    reexports::wayland_server::Client,
    utils::{Buffer as BufferCoords, IsAlive, Logical, Physical, Point, Rectangle, Scale, Size},
    wayland::xdg_activation::XdgActivationState,
};
use std::{
    collections::{HashMap, VecDeque},
    sync::atomic::{AtomicBool, Ordering},
    time::{Duration, Instant},
};
use wayland_backend::server::ClientId;

use super::{
    CosmicMappedRenderElement, CosmicSurface, ResizeDirection, ResizeMode,
    element::{
        CosmicMapped, MaximizedState, resize_indicator::ResizeIndicator,
        stack::CosmicStackRenderElement, swap_indicator::SwapIndicator,
        window::CosmicWindowRenderElement,
    },
    focus::{
        FocusStack, FocusStackMut,
        target::{KeyboardFocusTarget, PointerFocusTarget, WindowGroup},
    },
    grabs::ResizeEdge,
    layout::tiling::{Data, NodeDesc},
};

const FULLSCREEN_ANIMATION_DURATION: Duration = Duration::from_millis(200);

// For stable workspace id, generate random 24-bit integer, as a hex string
// Must be compared with existing workspaces work uniqueness.
pub fn random_workspace_id() -> String {
    let id = rand::random_range(0..(2 << 24));
    format!("{:x}", id)
}

fn output_match_for_output(output: &Output) -> OutputMatch {
    OutputMatch {
        name: output.name(),
        edid: output.edid().cloned(),
    }
}

// If `disambguate` is true, check that edid *and* connector name match.
// Otherwise, match only edid (if it exists)
fn output_matches(output_match: &OutputMatch, output: &Output, disambiguate: bool) -> bool {
    if output_match.edid.as_ref() != output.edid() {
        false
    } else if disambiguate || output_match.edid.is_none() {
        output_match.name == output.name()
    } else {
        true
    }
}

#[derive(Debug)]
pub struct Workspace {
    pub output: Output,
    pub tiling_layer: TilingLayout,
    pub floating_layer: FloatingLayout,
    pub minimized_windows: Vec<MinimizedWindow>,
    pub tiling_enabled: bool,
    pub fullscreen: Option<FullscreenSurface>,
    pub pinned: bool,
    pub id: Option<String>,

    pub handle: WorkspaceHandle,
    pub focus_stack: FocusStacks,
    pub screencopy: ScreencopySessions,
    output_stack: VecDeque<OutputMatch>,
    pub(super) backdrop_id: Id,
    pub dirty: AtomicBool,
}

#[derive(Debug)]
pub enum MinimizedWindow {
    Fullscreen {
        surface: CosmicSurface,
        previous: Option<FullscreenRestoreData>,
    },
    Floating {
        window: CosmicMapped,
        previous: FloatingRestoreData,
    },
    Tiling {
        window: CosmicMapped,
        previous: TilingRestoreData,
    },
}

impl PartialEq<CosmicMapped> for MinimizedWindow {
    fn eq(&self, other: &CosmicMapped) -> bool {
        self.mapped().is_some_and(|m| m == other)
    }
}

impl MinimizedWindow {
    pub fn mapped(&self) -> Option<&CosmicMapped> {
        match self {
            MinimizedWindow::Floating { window, .. } | MinimizedWindow::Tiling { window, .. } => {
                Some(window)
            }
            _ => None,
        }
    }

    pub fn mapped_mut(&mut self) -> Option<&mut CosmicMapped> {
        match self {
            MinimizedWindow::Floating { window, .. } | MinimizedWindow::Tiling { window, .. } => {
                Some(window)
            }
            _ => None,
        }
    }

    pub fn active_window(&self) -> CosmicSurface {
        match self {
            MinimizedWindow::Floating { window, .. } | MinimizedWindow::Tiling { window, .. } => {
                window.active_window()
            }
            MinimizedWindow::Fullscreen { surface, .. } => surface.clone(),
        }
    }

    pub fn windows(&self) -> impl Iterator<Item = CosmicSurface> + '_ {
        match self {
            MinimizedWindow::Floating { window, .. } | MinimizedWindow::Tiling { window, .. } => {
                Box::new(window.windows().map(|(s, _)| s))
                    as Box<dyn Iterator<Item = CosmicSurface>>
            }
            MinimizedWindow::Fullscreen { surface, .. } => {
                Box::new(std::iter::once(surface.clone())) as _
            }
        }
    }

    pub fn unmaximize(&mut self, original_geometry: Rectangle<i32, Local>) {
        match self {
            MinimizedWindow::Fullscreen { .. } => {}
            MinimizedWindow::Tiling {
                window, previous, ..
            } => {
                previous.was_maximized = false;
                window.set_maximized(false);
                window.configure();
            }
            MinimizedWindow::Floating {
                window, previous, ..
            } => {
                previous.geometry = original_geometry;
                window.set_maximized(false);
                window.configure();
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct FullscreenSurface {
    pub surface: CosmicSurface,
    pub previous_state: Option<FullscreenRestoreState>,
    pub previous_geometry: Option<Rectangle<i32, Local>>,
    start_at: Option<Instant>,
    pub ended_at: Option<Instant>,
}

impl PartialEq for FullscreenSurface {
    fn eq(&self, other: &Self) -> bool {
        self.surface == other.surface
    }
}

impl FullscreenSurface {
    pub fn is_animating(&self) -> bool {
        self.start_at.is_some() || self.ended_at.is_some()
    }
}

impl IsAlive for FullscreenSurface {
    fn alive(&self) -> bool {
        self.surface.alive()
    }
}

/// LIFO stack of focus targets
#[derive(Debug, Default)]
pub struct FocusStacks(HashMap<Seat<State>, IndexSet<FocusTarget>>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ManagedLayer {
    Fullscreen,
    Tiling,
    Floating,
    Sticky,
}

#[derive(Debug, Clone)]
pub enum FullscreenRestoreState {
    Tiling {
        workspace: WorkspaceHandle,
        state: TilingRestoreData,
    },
    Floating {
        workspace: WorkspaceHandle,
        state: FloatingRestoreData,
    },
    Sticky {
        output: WeakOutput,
        state: FloatingRestoreData,
    },
}

impl FullscreenRestoreState {
    pub fn was_maximized(&self) -> bool {
        match self {
            FullscreenRestoreState::Floating { state, .. }
            | FullscreenRestoreState::Sticky { state, .. } => state.was_maximized,
            FullscreenRestoreState::Tiling { state, .. } => state.was_maximized,
        }
    }
}

#[derive(Debug, Clone)]
pub enum WorkspaceRestoreData {
    Fullscreen(Option<FullscreenRestoreData>),
    Tiling(Option<TilingRestoreData>),
    Floating(Option<FloatingRestoreData>),
}

impl From<ManagedLayer> for WorkspaceRestoreData {
    fn from(value: ManagedLayer) -> Self {
        match value {
            ManagedLayer::Floating | ManagedLayer::Sticky => WorkspaceRestoreData::Floating(None),
            ManagedLayer::Tiling => WorkspaceRestoreData::Tiling(None),
            ManagedLayer::Fullscreen => WorkspaceRestoreData::Fullscreen(None),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FloatingRestoreData {
    pub geometry: Rectangle<i32, Local>,
    pub output_size: Size<i32, Logical>,
    pub was_maximized: bool,
}

impl FloatingRestoreData {
    pub fn position_relative(&self, output_size: Size<i32, Logical>) -> Point<i32, Local> {
        if self.output_size != output_size {
            Point::from((
                (self.geometry.loc.x as f64 / self.output_size.w as f64 * output_size.w as f64)
                    .floor() as i32,
                (self.geometry.loc.y as f64 / self.output_size.h as f64 * output_size.h as f64)
                    .floor() as i32,
            ))
        } else {
            self.geometry.loc
        }
    }
}

#[derive(Debug, Clone)]
pub struct TilingRestoreData {
    pub state: Option<RestoreTilingState>,
    pub was_maximized: bool,
}

#[derive(Debug, Clone)]
pub struct FullscreenRestoreData {
    pub previous_state: FullscreenRestoreState,
    pub previous_geometry: Rectangle<i32, Local>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FocusResult {
    None,
    Handled,
    Some(KeyboardFocusTarget),
}

impl FocusResult {
    pub fn or_else<F>(self, f: F) -> FocusResult
    where
        F: FnOnce() -> FocusResult,
    {
        match self {
            FocusResult::None => f(),
            x => x,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MoveResult {
    None,
    Done,
    MoveFurther(KeyboardFocusTarget),
    ShiftFocus(KeyboardFocusTarget),
}

impl MoveResult {
    pub fn or_else<F>(self, f: F) -> MoveResult
    where
        F: FnOnce() -> MoveResult,
    {
        match self {
            MoveResult::None => f(),
            x => x,
        }
    }
}

impl Workspace {
    pub fn new(
        handle: WorkspaceHandle,
        output: Output,
        tiling_enabled: bool,
        theme: cosmic::Theme,
        appearance: AppearanceConfig,
    ) -> Workspace {
        let tiling_layer = TilingLayout::new(theme.clone(), appearance, &output);
        let floating_layer = FloatingLayout::new(theme, appearance, &output);
        let output_match = output_match_for_output(&output);

        Workspace {
            output,
            tiling_layer,
            floating_layer,
            tiling_enabled,
            minimized_windows: Vec::new(),
            fullscreen: None,
            pinned: false,
            id: None,
            handle,
            focus_stack: FocusStacks::default(),
            screencopy: ScreencopySessions::default(),
            output_stack: {
                let mut queue = VecDeque::new();
                queue.push_back(output_match);
                queue
            },
            backdrop_id: Id::new(),
            dirty: AtomicBool::new(false),
        }
    }

    pub fn from_pinned(
        pinned: &PinnedWorkspace,
        handle: WorkspaceHandle,
        output: Output,
        theme: cosmic::Theme,
        appearance: AppearanceConfig,
    ) -> Self {
        let tiling_layer = TilingLayout::new(theme.clone(), appearance, &output);
        let floating_layer = FloatingLayout::new(theme, appearance, &output);
        let output_match = output_match_for_output(&output);

        Workspace {
            output,
            tiling_layer,
            floating_layer,
            tiling_enabled: pinned.tiling_enabled,
            minimized_windows: Vec::new(),
            fullscreen: None,
            pinned: true,
            id: pinned.id.clone(),
            handle,
            focus_stack: FocusStacks::default(),
            screencopy: ScreencopySessions::default(),
            output_stack: {
                let mut queue = VecDeque::new();
                queue.push_back(pinned.output.clone());
                if output_match != pinned.output {
                    queue.push_back(output_match);
                }
                queue
            },
            backdrop_id: Id::new(),
            dirty: AtomicBool::new(false),
        }
    }

    pub fn to_pinned(&self) -> Option<PinnedWorkspace> {
        debug_assert!(self.id.is_some());
        let output = self.explicit_output().clone();
        if self.pinned {
            Some(PinnedWorkspace {
                output: cosmic_comp_config::workspace::OutputMatch {
                    name: output.name,
                    edid: output.edid,
                },
                tiling_enabled: self.tiling_enabled,
                id: self.id.clone(),
            })
        } else {
            None
        }
    }

    #[profiling::function]
    pub fn refresh(&mut self) {
        self.fullscreen.take_if(|w| !w.alive());
        self.floating_layer.refresh();
        self.tiling_layer.refresh();
    }

    fn has_activation_token(&self, xdg_activation_state: &XdgActivationState) -> bool {
        xdg_activation_state.tokens().any(|(_, data)| {
            if let ActivationContext::Workspace(handle) =
                data.user_data.get::<ActivationContext>().unwrap()
            {
                *handle == self.handle
                    && data.timestamp.elapsed() < super::ACTIVATION_TOKEN_EXPIRE_TIME
            } else {
                false
            }
        })
    }

    // Auto-removal of workspaces is allowed if empty, unless blocked by an
    // unused and unexpired activation token, or pinned.
    pub fn can_auto_remove(&self, xdg_activation_state: &XdgActivationState) -> bool {
        self.is_empty() && !self.has_activation_token(xdg_activation_state) && !self.pinned
    }

    pub fn refresh_focus_stack(&mut self) {
        for (seat, stack) in self.focus_stack.0.iter_mut() {
            let fullscreen = self
                .fullscreen
                .as_ref()
                .filter(|f| f.alive())
                .filter(|f| f.ended_at.is_none())
                .map(|f| &f.surface);

            // Move grab is treated as focused, so don't change focus to a
            // window while grab exists.
            let move_grab_state = seat
                .user_data()
                .get::<SeatMoveGrabState>()
                .unwrap()
                .lock()
                .unwrap();
            let move_mapped = if let Some(move_grab_state) = &*move_grab_state {
                Some(move_grab_state.element())
            } else {
                None
            };

            let mapped = || {
                self.floating_layer
                    .mapped()
                    .chain(self.tiling_layer.mapped().map(|(w, _)| w))
                    .chain(move_mapped.iter())
            };
            stack.retain(|w| match w {
                FocusTarget::Fullscreen(s) => fullscreen.is_some_and(|f| f == s),
                FocusTarget::Window(w) => mapped().any(|m| w == m),
            });
        }
    }

    pub fn animations_going(&self) -> bool {
        self.tiling_layer.animations_going()
            || self.floating_layer.animations_going()
            || self
                .fullscreen
                .as_ref()
                .is_some_and(|f| f.start_at.is_some() || f.ended_at.is_some())
            || self.dirty.swap(false, Ordering::SeqCst)
    }

    pub fn update_animations(&mut self) -> HashMap<ClientId, Client> {
        if let Some(f) = self.fullscreen.as_mut() {
            if let Some(start) = f.start_at.as_ref() {
                let duration_since = Instant::now().duration_since(*start);
                if duration_since > FULLSCREEN_ANIMATION_DURATION {
                    f.start_at.take();
                    self.dirty.store(true, Ordering::SeqCst);
                }
            }

            if let Some(end) = f.ended_at {
                let duration_since = Instant::now().duration_since(end);
                if duration_since >= FULLSCREEN_ANIMATION_DURATION {
                    let _ = self.fullscreen.take();
                    self.dirty.store(true, Ordering::SeqCst);
                }
            }
        }

        let clients = self.tiling_layer.update_animation_state();
        self.floating_layer.update_animation_state();
        clients
    }

    pub fn output(&self) -> &Output {
        &self.output
    }

    /// Output workspace was originally created on, or explicitly moved to by the user
    fn explicit_output(&self) -> &OutputMatch {
        self.output_stack.front().unwrap()
    }

    // Set output the workspace is on
    //
    // If `explicit` is `true`, the user has explicitly moved the workspace
    // to this output, so previous outputs it was on can be forgotten.
    pub fn set_output(&mut self, output: &Output, explicit: bool) {
        self.tiling_layer.set_output(output);
        self.floating_layer.set_output(output);
        for mapped in self.mapped() {
            for (surface, _) in mapped.windows() {
                toplevel_leave_output(&surface, &self.output);
                toplevel_enter_output(&surface, output);
            }
        }
        for window in self.minimized_windows.iter() {
            for surface in window.windows() {
                toplevel_leave_output(&surface, &self.output);
                toplevel_enter_output(&surface, output);
            }
        }
        if let Some(f) = self.fullscreen.as_ref().filter(|f| f.ended_at.is_none()) {
            toplevel_leave_output(&f.surface, &self.output);
            toplevel_enter_output(&f.surface, output);
        }
        if explicit {
            self.output_stack.clear();
        }
        if let Some(pos) = self
            .output_stack
            .iter()
            .position(|i| output_matches(i, output, true))
        {
            // Matched edid and connector name
            self.output_stack.truncate(pos + 1);
        } else if let Some(pos) = self
            .output_stack
            .iter()
            .position(|i| output_matches(i, output, false))
        {
            // Matched edid but not connector name; truncate entries that don't match edid,
            // but keep old entry in case we see two outputs with the same edid.
            self.output_stack.truncate(pos + 1);
            self.output_stack.push_back(output_match_for_output(output));
        } else {
            self.output_stack.push_back(output_match_for_output(output));
        }
        self.output = output.clone();
    }

    pub fn prefers_output(&self, output: &Output) -> bool {
        // Disambiguate match by connector name if existing output has same edid
        let disambiguate = output
            .edid()
            .is_some_and(|edid| self.output().edid() == Some(edid));
        self.output_stack
            .iter()
            .any(|i| output_matches(i, output, disambiguate))
    }

    pub fn unmap_element(&mut self, mapped: &CosmicMapped) -> Option<WorkspaceRestoreData> {
        let was_maximized = if mapped.maximized_state.lock().unwrap().is_some() {
            // If surface is maximized then unmaximize it, so it is assigned to only one layer
            self.unmaximize_request(mapped)
        } else {
            None
        };

        self.focus_stack.0.values_mut().for_each(|set| {
            set.shift_remove(mapped);
        });

        if let Some(pos) = self.minimized_windows.iter().position(|m| m == mapped) {
            let state = self.minimized_windows.remove(pos);
            mapped.set_minimized(false);
            return Some(match state {
                MinimizedWindow::Floating { previous, .. } => {
                    WorkspaceRestoreData::Floating(Some(previous))
                }
                MinimizedWindow::Tiling { previous, .. } => {
                    WorkspaceRestoreData::Tiling(Some(previous))
                }
                MinimizedWindow::Fullscreen { .. } => unreachable!(),
            });
        }

        if let Ok(state) = self.tiling_layer.unmap(mapped, None) {
            return Some(WorkspaceRestoreData::Tiling(Some(TilingRestoreData {
                state,
                was_maximized: was_maximized.is_some(),
            })));
        }

        // unmaximize_request might have triggered a `floating_layer.refresh()`,
        // which may have already removed a non-alive surface.
        if let Some(floating_geometry) = self.floating_layer.unmap(mapped, None).or(was_maximized) {
            return Some(WorkspaceRestoreData::Floating(Some(FloatingRestoreData {
                geometry: floating_geometry,
                output_size: self.output.geometry().size.as_logical(),
                was_maximized: was_maximized.is_some(),
            })));
        };

        None
    }

    pub fn unmap_surface<S>(&mut self, surface: &S) -> Option<(CosmicSurface, WorkspaceRestoreData)>
    where
        CosmicSurface: PartialEq<S>,
    {
        if self
            .fullscreen
            .as_ref()
            .is_some_and(|f| f.ended_at.is_none() && &f.surface == surface)
        {
            let (surface, previous_state, previous_geometry) = self.remove_fullscreen().unwrap();
            return Some((
                surface,
                WorkspaceRestoreData::Fullscreen(previous_state.zip(previous_geometry).map(
                    |(previous_state, previous_geometry)| FullscreenRestoreData {
                        previous_state,
                        previous_geometry,
                    },
                )),
            ));
        }

        if let Some(pos) = self.minimized_windows.iter().position(|m| {
            if let MinimizedWindow::Fullscreen { surface: s, .. } = m {
                s == surface
            } else {
                false
            }
        }) {
            let MinimizedWindow::Fullscreen { surface, previous } =
                self.minimized_windows.remove(pos)
            else {
                unreachable!()
            };

            surface.set_minimized(false);
            return Some((surface, WorkspaceRestoreData::Fullscreen(previous)));
        }

        let mapped = self.element_for_surface(surface)?;
        let maybe_stack = mapped.stack_ref().filter(|s| s.len() > 1);
        if let Some(stack) = maybe_stack {
            if stack.len() > 1 {
                let idx = stack.surfaces().position(|s| &s == surface);
                let layer = if self.is_tiled(surface) {
                    ManagedLayer::Tiling
                } else {
                    ManagedLayer::Floating
                };
                return idx
                    .and_then(|idx| stack.remove_idx(idx))
                    .map(|s| (s, layer.into()));
            }
        }

        // we know mapped is no stack with more than one element now,
        // so we can treat mapped as containing only our surface.

        let mapped = mapped.clone();
        let layer = self.unmap_element(&mapped)?;
        Some((mapped.active_window(), layer))
    }

    pub fn fullscreen_geometry(&self) -> Option<Rectangle<i32, Local>> {
        self.fullscreen.as_ref().map(|fullscreen| {
            let bbox = fullscreen.surface.bbox().as_local();

            let mut full_geo = Rectangle::from_size(self.output.geometry().size.as_local());
            if bbox != full_geo {
                if bbox.size.w < full_geo.size.w {
                    full_geo.loc.x += (full_geo.size.w - bbox.size.w) / 2;
                    full_geo.size.w = bbox.size.w;
                }
                if bbox.size.h < full_geo.size.h {
                    full_geo.loc.y += (full_geo.size.h - bbox.size.h) / 2;
                    full_geo.size.h = bbox.size.h;
                }
            }

            full_geo
        })
    }

    pub fn element_for_surface<S>(&self, surface: &S) -> Option<&CosmicMapped>
    where
        CosmicSurface: PartialEq<S>,
    {
        self.floating_layer
            .mapped()
            .chain(self.tiling_layer.mapped().map(|(w, _)| w))
            .chain(self.minimized_windows.iter().flat_map(|w| w.mapped()))
            .find(|e| e.windows().any(|(w, _)| &w == surface))
    }

    pub fn popup_element_under(
        &self,
        location: Point<f64, Global>,
        seat: &Seat<State>,
    ) -> Option<KeyboardFocusTarget> {
        if !self.output.geometry().contains(location.to_i32_round()) {
            return None;
        }
        let location = location.to_local(&self.output);

        let fullscreen_element_under =
            |fullscreen: &FullscreenSurface, geometry: Rectangle<i32, Local>| {
                fullscreen
                    .surface
                    .0
                    .surface_under(
                        (location - geometry.loc.to_f64()).as_logical(),
                        WindowSurfaceType::POPUP | WindowSurfaceType::SUBSURFACE,
                    )
                    .is_some()
                    .then(|| KeyboardFocusTarget::Fullscreen(fullscreen.surface.clone()))
            };

        let stack = self.focus_stack.get(seat);
        let last_focused = stack.last();

        if let Some(fullscreen) = self.fullscreen.as_ref() {
            if last_focused.is_some_and(
                |t| matches!(t, FocusTarget::Fullscreen(f) if f == &fullscreen.surface),
            ) && !fullscreen.is_animating()
            {
                let geometry = self.fullscreen_geometry().unwrap();
                return fullscreen_element_under(fullscreen, geometry);
            }
        }

        self.floating_layer
            .popup_element_under(location)
            .or_else(|| self.tiling_layer.popup_element_under(location))
            .or_else(|| {
                if last_focused.is_none_or(|t| !matches!(t, FocusTarget::Fullscreen(_))) {
                    if let Some(fullscreen) = self.fullscreen.as_ref() {
                        let geometry = self.fullscreen_geometry().unwrap();
                        return fullscreen_element_under(fullscreen, geometry);
                    }
                }
                None
            })
    }

    pub fn toplevel_element_under(
        &self,
        location: Point<f64, Global>,
        seat: &Seat<State>,
    ) -> Option<KeyboardFocusTarget> {
        if !self.output.geometry().contains(location.to_i32_round()) {
            return None;
        }
        let location = location.to_local(&self.output);

        let fullscreen_element_under =
            |fullscreen: &FullscreenSurface, geometry: Rectangle<i32, Local>| {
                fullscreen
                    .surface
                    .0
                    .surface_under(
                        (location - geometry.loc.to_f64()).as_logical(),
                        WindowSurfaceType::TOPLEVEL | WindowSurfaceType::SUBSURFACE,
                    )
                    .is_some()
                    .then(|| KeyboardFocusTarget::Fullscreen(fullscreen.surface.clone()))
            };

        let stack = self.focus_stack.get(seat);
        let last_focused = stack.last();

        if let Some(fullscreen) = self.fullscreen.as_ref() {
            if last_focused.is_some_and(
                |t| matches!(t, FocusTarget::Fullscreen(f) if f == &fullscreen.surface),
            ) && !fullscreen.is_animating()
            {
                let geometry = self.fullscreen_geometry().unwrap();
                return fullscreen_element_under(fullscreen, geometry);
            }
        }

        self.floating_layer
            .toplevel_element_under(location)
            .or_else(|| self.tiling_layer.toplevel_element_under(location))
            .or_else(|| {
                if last_focused.is_none_or(|t| !matches!(t, FocusTarget::Fullscreen(_))) {
                    if let Some(fullscreen) = self.fullscreen.as_ref() {
                        let geometry = self.fullscreen_geometry().unwrap();
                        return fullscreen_element_under(fullscreen, geometry);
                    }
                }
                None
            })
    }

    pub fn popup_surface_under(
        &self,
        location: Point<f64, Global>,
        overview: OverviewMode,
        seat: &Seat<State>,
    ) -> Option<(PointerFocusTarget, Point<f64, Global>)> {
        if !self.output.geometry().contains(location.to_i32_round()) {
            return None;
        }
        let location = location.to_local(&self.output);

        let check_fullscreen = |fullscreen: &FullscreenSurface| {
            if !fullscreen.is_animating() {
                let geometry = self.fullscreen_geometry().unwrap();
                return fullscreen
                    .surface
                    .0
                    .surface_under(
                        (location - geometry.loc.to_f64()).as_logical(),
                        WindowSurfaceType::POPUP | WindowSurfaceType::SUBSURFACE,
                    )
                    .map(|(surface, surface_offset)| {
                        (
                            PointerFocusTarget::WlSurface {
                                surface,
                                toplevel: Some(fullscreen.surface.clone().into()),
                            },
                            (geometry.loc + surface_offset.as_local()).to_f64(),
                        )
                    });
            }
            None
        };

        let stack = self.focus_stack.get(seat);
        let last_focused = stack.last();

        self.fullscreen
            .as_ref()
            .filter(|f| last_focused.is_some_and(|t| t == &f.surface))
            .and_then(check_fullscreen)
            .or_else(|| self.floating_layer.popup_surface_under(location))
            .or_else(|| self.tiling_layer.popup_surface_under(location, overview))
            .or_else(|| {
                self.fullscreen
                    .as_ref()
                    .filter(|f| last_focused.is_none_or(|t| t != &f.surface))
                    .and_then(check_fullscreen)
            })
            .map(|(m, p)| (m, p.to_global(&self.output)))
    }

    pub fn toplevel_surface_under(
        &self,
        location: Point<f64, Global>,
        overview: OverviewMode,
        seat: &Seat<State>,
    ) -> Option<(PointerFocusTarget, Point<f64, Global>)> {
        if !self.output.geometry().contains(location.to_i32_round()) {
            return None;
        }
        let location = location.to_local(&self.output);

        let check_fullscreen = |fullscreen: &FullscreenSurface| {
            if !fullscreen.is_animating() {
                let geometry = self.fullscreen_geometry().unwrap();
                return fullscreen
                    .surface
                    .focus_under(
                        (location - geometry.loc.to_f64()).as_logical(),
                        WindowSurfaceType::TOPLEVEL | WindowSurfaceType::SUBSURFACE,
                    )
                    .map(|(target, surface_offset)| {
                        (target, (geometry.loc.to_f64() + surface_offset.as_local()))
                    });
            }

            None
        };

        let stack = self.focus_stack.get(seat);
        let last_focused = stack.last();

        self.fullscreen
            .as_ref()
            .filter(|f| last_focused.is_some_and(|t| t == &f.surface))
            .and_then(check_fullscreen)
            .or_else(|| self.floating_layer.toplevel_surface_under(location))
            .or_else(|| self.tiling_layer.toplevel_surface_under(location, overview))
            .or_else(|| {
                self.fullscreen
                    .as_ref()
                    .filter(|f| last_focused.is_none_or(|t| t != &f.surface))
                    .and_then(check_fullscreen)
            })
            .map(|(m, p)| (m, p.to_global(&self.output)))
    }

    pub fn update_pointer_position(
        &mut self,
        location: Option<Point<f64, Local>>,
        overview: OverviewMode,
    ) {
        self.floating_layer.update_pointer_position(location);
        self.tiling_layer
            .update_pointer_position(location, overview);
    }

    pub fn element_geometry(&self, elem: &CosmicMapped) -> Option<Rectangle<i32, Local>> {
        self.floating_layer
            .element_geometry(elem)
            .or_else(|| self.tiling_layer.element_geometry(elem))
    }

    pub fn recalculate(&mut self) {
        self.tiling_layer.recalculate();
        self.floating_layer.recalculate();
    }

    pub fn unmaximize_request(&mut self, elem: &CosmicMapped) -> Option<Rectangle<i32, Local>> {
        let mut state = elem.maximized_state.lock().unwrap();
        if let Some(state) = state.take() {
            if let Some(minimized) = self.minimized_windows.iter_mut().find(|m| *m == elem) {
                minimized.unmaximize(state.original_geometry);
                Some(state.original_geometry)
            } else {
                match state.original_layer {
                    ManagedLayer::Tiling if self.tiling_enabled => {
                        // should still be mapped in tiling
                        let geo = self.tiling_layer.element_geometry(elem);
                        self.floating_layer.unmap(elem, geo);
                        elem.output_enter(&self.output, elem.bbox());
                        elem.set_maximized(false);
                        elem.set_geometry(state.original_geometry.to_global(&self.output));
                        elem.configure();
                        self.tiling_layer.recalculate();
                        geo
                    }
                    ManagedLayer::Sticky => unreachable!(),

                    x => {
                        let use_geometry = !matches!(x, ManagedLayer::Tiling);
                        elem.set_maximized(false);
                        self.floating_layer.map_internal(
                            elem.clone(),
                            use_geometry.then_some(state.original_geometry.loc),
                            use_geometry.then_some(state.original_geometry.size.as_logical()),
                            None,
                        );
                        Some(state.original_geometry)
                    }
                }
            }
        } else {
            None
        }
    }

    pub fn minimize<S>(&mut self, surface: &S, to: Rectangle<i32, Local>) -> Option<MinimizedWindow>
    where
        CosmicSurface: PartialEq<S>,
    {
        if self.get_fullscreen().is_some_and(|s| s == surface) {
            let fullscreen_state = self.fullscreen.clone().unwrap();
            {
                let f = self.fullscreen.as_mut().unwrap();
                f.previous_geometry = Some(to);
                f.ended_at = Some(
                    Instant::now()
                        - (FULLSCREEN_ANIMATION_DURATION
                            - f.start_at
                                .take()
                                .map(|earlier| {
                                    Instant::now()
                                        .duration_since(earlier)
                                        .min(FULLSCREEN_ANIMATION_DURATION)
                                })
                                .unwrap_or(FULLSCREEN_ANIMATION_DURATION)),
                );
            }

            fullscreen_state.surface.set_minimized(true);
            return Some(MinimizedWindow::Fullscreen {
                surface: fullscreen_state.surface,
                previous: fullscreen_state
                    .previous_state
                    .zip(fullscreen_state.previous_geometry)
                    .map(
                        |(previous_state, previous_geometry)| FullscreenRestoreData {
                            previous_state,
                            previous_geometry,
                        },
                    ),
            });
        }

        let mapped = self
            .mapped()
            .find(|m| m.windows().any(|(ref s, _)| s == surface))
            .cloned()?;
        let was_maximized = if let Some(MaximizedState {
            original_geometry,
            original_layer,
        }) = mapped.maximized_state.lock().unwrap().take()
        {
            // we need to do this manually instead of calling `self.unmaximize_request`
            // to get the correct animation in the tiling case.
            match original_layer {
                ManagedLayer::Tiling if self.tiling_enabled => {
                    self.floating_layer.unmap(&mapped, Some(to));
                }
                _ => {}
            }
            mapped.set_geometry(original_geometry.to_global(&self.output));
            mapped.set_maximized(false);
            Some(original_geometry)
        } else {
            None
        };

        mapped.set_minimized(true);
        mapped.configure();

        if let Some(geometry) = self.floating_layer.unmap(&mapped, Some(to)) {
            return Some(MinimizedWindow::Floating {
                window: mapped,
                previous: FloatingRestoreData {
                    geometry: was_maximized.unwrap_or(geometry),
                    output_size: self.output.geometry().size.as_logical(),
                    was_maximized: was_maximized.is_some(),
                },
            });
        }

        if let Ok(state) = self
            .tiling_layer
            .unmap(&mapped, was_maximized.is_none().then_some(to))
        {
            return Some(MinimizedWindow::Tiling {
                window: mapped,
                previous: TilingRestoreData {
                    state,
                    was_maximized: was_maximized.is_some(),
                },
            });
        }

        unreachable!()
    }

    pub fn unminimize(
        &mut self,
        window: MinimizedWindow,
        from: Rectangle<i32, Local>,
        seat: &Seat<State>,
    ) -> Option<(
        CosmicSurface,
        Option<FullscreenRestoreState>,
        Option<Rectangle<i32, Local>>,
    )> {
        match window {
            MinimizedWindow::Fullscreen { previous, surface } => {
                let old_fullscreen = self.remove_fullscreen();
                surface.set_minimized(false);
                self.fullscreen = Some(FullscreenSurface {
                    surface,
                    previous_state: previous.clone().map(|p| p.previous_state),
                    previous_geometry: previous.map(|p| p.previous_geometry),
                    start_at: None,
                    ended_at: None,
                });
                old_fullscreen
            }
            MinimizedWindow::Floating { window, previous } => {
                let current_output_size = self.output.geometry().size.as_logical();
                let previous_position = previous.position_relative(current_output_size);

                window.set_minimized(false);
                self.floating_layer
                    .remap_minimized(window.clone(), from, previous_position);

                if previous.was_maximized {
                    let geometry = self.floating_layer.element_geometry(&window).unwrap();
                    let mut state = window.maximized_state.lock().unwrap();
                    *state = Some(MaximizedState {
                        original_geometry: geometry,
                        original_layer: ManagedLayer::Floating,
                    });
                    std::mem::drop(state);
                    self.floating_layer.map_maximized(window, geometry, true);
                }

                None
            }
            MinimizedWindow::Tiling {
                window,
                previous:
                    TilingRestoreData {
                        state,
                        was_maximized,
                    },
            } => {
                window.set_minimized(false);
                if self.tiling_enabled {
                    let focus_stack = self.focus_stack.get(seat);
                    self.tiling_layer.remap(
                        window.clone(),
                        (!was_maximized).then_some(from),
                        state,
                        Some(focus_stack.iter()),
                    );
                    if was_maximized {
                        let previous_geometry =
                            self.tiling_layer.element_geometry(&window).unwrap();
                        let mut state = window.maximized_state.lock().unwrap();
                        *state = Some(MaximizedState {
                            original_geometry: previous_geometry,
                            original_layer: ManagedLayer::Tiling,
                        });
                        std::mem::drop(state);
                        self.floating_layer.map_maximized(window, from, true);
                    }
                } else {
                    self.floating_layer.map(window.clone(), None);
                    let mut geometry = self.floating_layer.element_geometry(&window).unwrap();
                    if let Some(pending_size) = window.pending_size() {
                        geometry.size = pending_size.as_local();
                    }

                    if was_maximized {
                        let mut state = window.maximized_state.lock().unwrap();
                        *state = Some(MaximizedState {
                            original_geometry: geometry,
                            original_layer: ManagedLayer::Tiling,
                        });
                        std::mem::drop(state);
                        self.floating_layer.map_maximized(window, from, true);
                    } else {
                        // get the right animation
                        self.floating_layer
                            .remap_minimized(window.clone(), from, geometry.loc);
                    }
                }
                None
            }
        }
    }

    pub fn map_fullscreen<'a>(
        &mut self,
        window: &CosmicSurface,
        seat: impl Into<Option<&'a Seat<State>>>,
        restore: Option<FullscreenRestoreState>,
        previous_geometry: Option<Rectangle<i32, Local>>,
    ) -> Option<(
        CosmicSurface,
        Option<FullscreenRestoreState>,
        Option<Rectangle<i32, Local>>,
    )> {
        let res = self.remove_fullscreen();

        window.set_fullscreen(true);
        window.set_geometry(self.output.geometry(), 0);
        window.send_configure();
        window.output_enter(
            &self.output,
            Rectangle::new(Point::new(0, 0), self.output.geometry().size.as_logical()),
        );

        if let Some(seat) = seat.into() {
            self.focus_stack.get_mut(seat).append(window.clone());
        }

        self.fullscreen = Some(FullscreenSurface {
            surface: window.clone(),
            previous_state: restore,
            previous_geometry,
            start_at: Some(Instant::now()),
            ended_at: None,
        });

        res
    }

    #[must_use]
    pub fn remove_fullscreen(
        &mut self,
    ) -> Option<(
        CosmicSurface,
        Option<FullscreenRestoreState>,
        Option<Rectangle<i32, Local>>,
    )> {
        if let Some(surface) = self.fullscreen.as_mut() {
            if surface.ended_at.is_some() {
                return None;
            }

            if surface.surface.alive() {
                surface.surface.output_leave(&self.output);
                surface.surface.set_fullscreen(false);
                if let Some(previous_geometry) = surface.previous_geometry.as_ref() {
                    surface
                        .surface
                        .set_geometry(previous_geometry.to_global(&self.output), 0);
                }
                surface.surface.send_configure();
            }

            for focus_stack in self.focus_stack.0.values_mut() {
                focus_stack.retain(|t| t != &surface.surface);
            }

            surface.ended_at = Some(
                Instant::now()
                    - (FULLSCREEN_ANIMATION_DURATION
                        - surface
                            .start_at
                            .take()
                            .map(|earlier| {
                                Instant::now()
                                    .duration_since(earlier)
                                    .min(FULLSCREEN_ANIMATION_DURATION)
                            })
                            .unwrap_or(FULLSCREEN_ANIMATION_DURATION)),
            );

            Some((
                surface.surface.clone(),
                surface.previous_state.clone(),
                surface.previous_geometry,
            ))
        } else {
            None
        }
    }

    pub fn get_fullscreen(&self) -> Option<&CosmicSurface> {
        self.fullscreen
            .as_ref()
            .filter(|f| f.alive())
            .filter(|f| f.ended_at.is_none())
            .map(|f| &f.surface)
    }

    pub fn resize(
        &mut self,
        focused: &KeyboardFocusTarget,
        direction: ResizeDirection,
        edge: ResizeEdge,
        amount: i32,
    ) -> bool {
        if matches!(focused, KeyboardFocusTarget::Fullscreen(_)) {
            return false;
        }

        if !self.floating_layer.resize(focused, direction, edge, amount) {
            self.tiling_layer.resize(focused, direction, edge, amount)
        } else {
            true
        }
    }

    pub fn toggle_tiling(
        &mut self,
        seat: &Seat<State>,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
    ) {
        self.set_tiling(!self.tiling_enabled, seat, workspace_state)
    }

    pub fn set_tiling(
        &mut self,
        tiling: bool,
        seat: &Seat<State>,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
    ) {
        let mut maximized_windows = Vec::new();
        if tiling {
            let floating_windows = self.floating_layer.mapped().cloned().collect::<Vec<_>>();

            for window in floating_windows.iter().filter(|w| w.is_maximized(false)) {
                maximized_windows.push((window.clone(), ManagedLayer::Tiling));
            }

            let focus_stack = self.focus_stack.get(seat);
            for window in floating_windows.into_iter() {
                self.floating_layer.unmap(&window, None);
                self.tiling_layer
                    .map(window, Some(focus_stack.iter()), None)
            }
            workspace_state.set_workspace_tiling_state(&self.handle, TilingState::TilingEnabled);
            self.tiling_enabled = true;
        } else {
            for window in self
                .tiling_layer
                .mapped()
                .map(|(m, _)| m.clone())
                .collect::<Vec<_>>()
                .into_iter()
            {
                if window.is_maximized(false) {
                    self.unmaximize_request(&window);
                    maximized_windows.push((window.clone(), ManagedLayer::Floating));
                }
                let _ = self.tiling_layer.unmap(&window, None);
                self.floating_layer.map(window, None);
            }
            workspace_state.set_workspace_tiling_state(&self.handle, TilingState::FloatingOnly);
            self.tiling_enabled = false;
        }
        for (window, original_layer) in maximized_windows {
            let mut original_geometry = self.element_geometry(&window).unwrap();
            if let Some(pending_size) = window.pending_size() {
                original_geometry.size = pending_size.as_local();
            }
            let mut state = window.maximized_state.lock().unwrap();
            *state = Some(MaximizedState {
                original_geometry,
                original_layer,
            });
            std::mem::drop(state);

            self.floating_layer
                .map_maximized(window, original_geometry, false);
        }
    }

    pub fn toggle_floating_window(&mut self, seat: &Seat<State>, window: &CosmicMapped) {
        if self.tiling_enabled {
            if window.is_maximized(false) {
                self.unmaximize_request(window);
            }
            if self.tiling_layer.mapped().any(|(m, _)| m == window) {
                let _ = self.tiling_layer.unmap(window, None);
                self.floating_layer.map(window.clone(), None);
            } else if self.floating_layer.mapped().any(|w| w == window) {
                let focus_stack = self.focus_stack.get(seat);
                self.floating_layer.unmap(window, None);
                self.tiling_layer
                    .map(window.clone(), Some(focus_stack.iter()), None)
            }
        }
    }

    pub fn toggle_floating_window_focused(&mut self, seat: &Seat<State>) {
        if matches!(
            seat.get_keyboard().unwrap().current_focus(),
            Some(KeyboardFocusTarget::Fullscreen(_))
        ) {
            return;
        }
        let maybe_window = self.focus_stack.get(seat).iter().next().cloned();
        if let Some(FocusTarget::Window(window)) = maybe_window {
            self.toggle_floating_window(seat, &window);
        }
    }

    pub fn mapped(&self) -> impl Iterator<Item = &CosmicMapped> {
        self.floating_layer
            .mapped()
            .chain(self.tiling_layer.mapped().map(|(w, _)| w))
    }

    pub fn len(&self) -> usize {
        self.floating_layer.mapped().count()
            + self.tiling_layer.mapped().count()
            + self.minimized_windows.len()
            + if self.fullscreen.is_some() { 1 } else { 0 }
    }

    pub fn is_empty(&self) -> bool {
        self.floating_layer.mapped().next().is_none()
            && self.tiling_layer.mapped().next().is_none()
            && self.minimized_windows.is_empty()
            && self.fullscreen.is_none()
    }

    pub fn is_floating<S>(&self, surface: &S) -> bool
    where
        CosmicSurface: PartialEq<S>,
    {
        self.floating_layer
            .mapped()
            .any(|m| m.windows().any(|(ref s, _)| s == surface))
            || self.minimized_windows.iter().any(|m| {
                if let MinimizedWindow::Floating { window, .. } = m {
                    window.windows().any(|(ref s, _)| s == surface)
                } else {
                    false
                }
            })
    }

    pub fn is_tiled<S>(&self, surface: &S) -> bool
    where
        CosmicSurface: PartialEq<S>,
    {
        self.tiling_layer
            .mapped()
            .any(|(m, _)| m.windows().any(|(ref s, _)| s == surface))
            || self.minimized_windows.iter().any(|m| {
                if let MinimizedWindow::Tiling { window, .. } = m {
                    window.windows().any(|(ref s, _)| s == surface)
                } else {
                    false
                }
            })
    }

    pub fn node_desc(&self, focus: KeyboardFocusTarget) -> Option<NodeDesc> {
        match focus {
            KeyboardFocusTarget::Element(mapped) => {
                if mapped.is_maximized(false) {
                    return None;
                }
                self.tiling_layer.mapped().find_map(|(m, _)| {
                    if m == &mapped {
                        mapped
                            .tiling_node_id
                            .lock()
                            .unwrap()
                            .clone()
                            .map(|node_id| NodeDesc {
                                handle: self.handle,
                                node: node_id.clone(),
                                stack_window: if mapped
                                    .stack_ref()
                                    .map(|stack| !stack.whole_stack_focused())
                                    .unwrap_or(false)
                                {
                                    Some(mapped.active_window())
                                } else {
                                    None
                                },
                                focus_stack: vec![node_id],
                            })
                    } else {
                        None
                    }
                })
            }
            KeyboardFocusTarget::Group(WindowGroup {
                node, focus_stack, ..
            }) => Some(NodeDesc {
                handle: self.handle,
                node,
                stack_window: None,
                focus_stack,
            }),
            _ => None,
        }
    }

    #[profiling::function]
    pub fn render<'a, R>(
        &self,
        renderer: &mut R,
        last_active_seat: &Seat<State>,
        render_focus: bool,
        overview: (OverviewMode, Option<(SwapIndicator, Option<&Tree<Data>>)>),
        resize_indicator: Option<(ResizeMode, ResizeIndicator)>,
        indicator_thickness: u8,
        theme: &CosmicTheme,
    ) -> Result<Vec<WorkspaceRenderElement<R>>, OutputNotMapped>
    where
        R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        CosmicWindowRenderElement<R>: RenderElement<R>,
        CosmicStackRenderElement<R>: RenderElement<R>,
        WorkspaceRenderElement<R>: RenderElement<R>,
    {
        let mut elements = Vec::default();

        let output_scale = self.output.current_scale().fractional_scale();
        let zone = {
            let layer_map = layer_map_for_output(&self.output);
            layer_map.non_exclusive_zone().as_local()
        };
        let focused = self.focus_stack.get(last_active_seat).last().cloned();

        let mut fullscreen_elements = if let Some(fullscreen) = self.fullscreen.as_ref() {
            let fullscreen_geo = self.fullscreen_geometry().unwrap();
            let previous_geo = fullscreen
                .previous_geometry
                .as_ref()
                .unwrap_or(&fullscreen_geo);

            let (target_geo, alpha) = match (fullscreen.start_at, fullscreen.ended_at) {
                (Some(started), _) => {
                    let duration = Instant::now().duration_since(started).as_secs_f64()
                        / FULLSCREEN_ANIMATION_DURATION.as_secs_f64();
                    (
                        ease(
                            EaseInOutCubic,
                            EaseRectangle(*previous_geo),
                            EaseRectangle(fullscreen_geo),
                            duration,
                        )
                        .0,
                        ease(EaseInOutCubic, 0.0, 1.0, duration),
                    )
                }
                (_, Some(ended)) => {
                    let duration = Instant::now().duration_since(ended).as_secs_f64()
                        / FULLSCREEN_ANIMATION_DURATION.as_secs_f64();
                    (
                        ease(
                            EaseInOutCubic,
                            EaseRectangle(fullscreen_geo),
                            EaseRectangle(*previous_geo),
                            duration,
                        )
                        .0,
                        ease(EaseInOutCubic, 1.0, 0.0, duration),
                    )
                }
                (None, None) => (fullscreen_geo, 1.0),
            };

            let render_loc = target_geo
                .loc
                .as_logical()
                .to_physical_precise_round(output_scale);

            // Only rescale geometry when animating
            let animation_rescale = |elem| {
                if fullscreen.is_animating() {
                    let fullscreen_geo = fullscreen.surface.0.geometry();
                    let scale = Scale {
                        x: target_geo.size.w as f64 / fullscreen_geo.size.w as f64,
                        y: target_geo.size.h as f64 / fullscreen_geo.size.h as f64,
                    };

                    RescaleRenderElement::from_element(elem, render_loc, scale).into()
                } else {
                    Into::<WorkspaceRenderElement<_>>::into(elem)
                }
            };

            fullscreen
                .surface
                .render_elements::<R, CosmicWindowRenderElement<R>>(
                    renderer,
                    render_loc,
                    output_scale.into(),
                    alpha,
                    Some(true),
                )
                .into_iter()
                .map(animation_rescale)
                .collect::<Vec<_>>()
        } else {
            Vec::new()
        };

        if matches!(focused, Some(FocusTarget::Fullscreen(_))) {
            elements.append(&mut fullscreen_elements);
        }

        if !matches!(focused, Some(FocusTarget::Fullscreen(_)))
            || self
                .fullscreen
                .as_ref()
                .map(|f| f.start_at.is_some() || f.ended_at.is_some())
                .unwrap_or(true)
        {
            // floating surfaces
            let alpha = match &overview.0 {
                OverviewMode::Started(_, started) => {
                    (1.0 - (Instant::now().duration_since(*started).as_millis()
                        / ANIMATION_DURATION.as_millis()) as f32)
                        .max(0.0)
                        * 0.4
                        + 0.6
                }
                OverviewMode::Ended(_, ended) => {
                    ((Instant::now().duration_since(*ended).as_millis()
                        / ANIMATION_DURATION.as_millis()) as f32)
                        * 0.4
                        + 0.6
                }
                OverviewMode::Active(_) => 0.6,
                OverviewMode::None => 1.0,
            };

            elements.extend(
                self.floating_layer
                    .render::<R>(
                        renderer,
                        focused.as_ref().and_then(|target| {
                            if let FocusTarget::Window(mapped) = target {
                                Some(mapped)
                            } else {
                                None
                            }
                        }),
                        resize_indicator.clone(),
                        indicator_thickness,
                        alpha,
                        theme,
                    )
                    .into_iter()
                    .map(WorkspaceRenderElement::from),
            );

            let alpha = match &overview.0 {
                OverviewMode::Started(_, start) => Some(
                    (Instant::now().duration_since(*start).as_millis() as f64 / 100.0).min(1.0)
                        as f32,
                ),
                OverviewMode::Active(_) => Some(1.0),
                OverviewMode::Ended(_, ended) => Some(
                    1.0 - (Instant::now().duration_since(*ended).as_millis() as f64 / 100.0)
                        .min(1.0) as f32,
                ),
                OverviewMode::None => None,
            };

            //tiling surfaces
            elements.extend(
                self.tiling_layer
                    .render::<R>(
                        renderer,
                        render_focus.then_some(last_active_seat),
                        zone,
                        overview,
                        resize_indicator,
                        indicator_thickness,
                        theme,
                    )?
                    .into_iter()
                    .map(WorkspaceRenderElement::from),
            );

            if let Some(alpha) = alpha {
                elements.push(
                    Into::<CosmicMappedRenderElement<R>>::into(BackdropShader::element(
                        renderer,
                        self.backdrop_id.clone(),
                        Rectangle::from_size(self.output.geometry().size.as_local()),
                        0.,
                        alpha * 0.85,
                        [0.0, 0.0, 0.0],
                    ))
                    .into(),
                )
            }
        }

        if !matches!(focused, Some(FocusTarget::Fullscreen(_))) {
            elements.extend(fullscreen_elements.into_iter());
        }

        Ok(elements)
    }

    #[profiling::function]
    pub fn render_popups<'a, R>(
        &self,
        renderer: &mut R,
        last_active_seat: &Seat<State>,
        render_focus: bool,
        overview: (OverviewMode, Option<(SwapIndicator, Option<&Tree<Data>>)>),
        theme: &CosmicTheme,
    ) -> Result<Vec<WorkspaceRenderElement<R>>, OutputNotMapped>
    where
        R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        CosmicWindowRenderElement<R>: RenderElement<R>,
        CosmicStackRenderElement<R>: RenderElement<R>,
        WorkspaceRenderElement<R>: RenderElement<R>,
    {
        let mut elements = Vec::default();

        let output_scale = self.output.current_scale().fractional_scale();
        let zone = {
            let layer_map = layer_map_for_output(&self.output);
            layer_map.non_exclusive_zone().as_local()
        };

        if let Some(fullscreen) = self.fullscreen.as_ref() {
            let fullscreen_geo = self.fullscreen_geometry().unwrap();
            let previous_geo = fullscreen
                .previous_geometry
                .as_ref()
                .unwrap_or(&fullscreen_geo);

            let (target_geo, alpha) = match (fullscreen.start_at, fullscreen.ended_at) {
                (Some(started), _) => {
                    let duration = Instant::now().duration_since(started).as_secs_f64()
                        / FULLSCREEN_ANIMATION_DURATION.as_secs_f64();
                    (
                        ease(
                            EaseInOutCubic,
                            EaseRectangle(*previous_geo),
                            EaseRectangle(fullscreen_geo),
                            duration,
                        )
                        .0,
                        ease(EaseInOutCubic, 0.0, 1.0, duration),
                    )
                }
                (_, Some(ended)) => {
                    let duration = Instant::now().duration_since(ended).as_secs_f64()
                        / FULLSCREEN_ANIMATION_DURATION.as_secs_f64();
                    (
                        ease(
                            EaseInOutCubic,
                            EaseRectangle(fullscreen_geo),
                            EaseRectangle(*previous_geo),
                            duration,
                        )
                        .0,
                        ease(EaseInOutCubic, 1.0, 0.0, duration),
                    )
                }
                (None, None) => (fullscreen_geo, 1.0),
            };

            let render_loc = target_geo
                .loc
                .as_logical()
                .to_physical_precise_round(output_scale);

            elements.extend(
                fullscreen
                    .surface
                    .popup_render_elements::<R, CosmicWindowRenderElement<R>>(
                        renderer,
                        render_loc,
                        output_scale.into(),
                        alpha,
                    )
                    .into_iter()
                    .map(Into::into),
            );
        }

        let focus_stack = self.focus_stack.get(last_active_seat);
        if !matches!(focus_stack.last(), Some(FocusTarget::Fullscreen(_)))
            || self
                .fullscreen
                .as_ref()
                .map(|f| f.start_at.is_some() || f.ended_at.is_some())
                .unwrap_or(true)
        {
            // floating surfaces
            let alpha = match &overview.0 {
                OverviewMode::Started(_, started) => {
                    (1.0 - (Instant::now().duration_since(*started).as_millis()
                        / ANIMATION_DURATION.as_millis()) as f32)
                        .max(0.0)
                        * 0.4
                        + 0.6
                }
                OverviewMode::Ended(_, ended) => {
                    ((Instant::now().duration_since(*ended).as_millis()
                        / ANIMATION_DURATION.as_millis()) as f32)
                        * 0.4
                        + 0.6
                }
                OverviewMode::Active(_) => 0.6,
                OverviewMode::None => 1.0,
            };

            elements.extend(
                self.floating_layer
                    .render_popups::<R>(renderer, alpha)
                    .into_iter()
                    .map(WorkspaceRenderElement::from),
            );

            //tiling surfaces
            elements.extend(
                self.tiling_layer
                    .render_popups::<R>(
                        renderer,
                        render_focus.then_some(last_active_seat),
                        zone,
                        overview,
                        theme,
                    )?
                    .into_iter()
                    .map(WorkspaceRenderElement::from),
            );
        }

        Ok(elements)
    }
}

impl FocusStacks {
    pub fn get<'a>(&'a self, seat: &Seat<State>) -> FocusStack<'a> {
        FocusStack(self.0.get(seat))
    }

    pub fn get_mut<'a>(&'a mut self, seat: &Seat<State>) -> FocusStackMut<'a> {
        FocusStackMut(self.0.entry(seat.clone()).or_default())
    }
}

pub struct OutputNotMapped;

pub enum WorkspaceRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    R::TextureId: 'static,
{
    OverrideRedirect(WaylandSurfaceRenderElement<R>),
    Fullscreen(RescaleRenderElement<CosmicWindowRenderElement<R>>),
    FullscreenPopup(CosmicWindowRenderElement<R>),
    Window(CosmicMappedRenderElement<R>),
    Backdrop(TextureRenderElement<GlesTexture>),
}

impl<R> Element for WorkspaceRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    R::TextureId: 'static,
{
    fn id(&self) -> &smithay::backend::renderer::element::Id {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.id(),
            WorkspaceRenderElement::Fullscreen(elem) => elem.id(),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.id(),
            WorkspaceRenderElement::Window(elem) => elem.id(),
            WorkspaceRenderElement::Backdrop(elem) => elem.id(),
        }
    }

    fn current_commit(&self) -> smithay::backend::renderer::utils::CommitCounter {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.current_commit(),
            WorkspaceRenderElement::Fullscreen(elem) => elem.current_commit(),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.current_commit(),
            WorkspaceRenderElement::Window(elem) => elem.current_commit(),
            WorkspaceRenderElement::Backdrop(elem) => elem.current_commit(),
        }
    }

    fn src(&self) -> Rectangle<f64, smithay::utils::Buffer> {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.src(),
            WorkspaceRenderElement::Fullscreen(elem) => elem.src(),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.src(),
            WorkspaceRenderElement::Window(elem) => elem.src(),
            WorkspaceRenderElement::Backdrop(elem) => elem.src(),
        }
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, smithay::utils::Physical> {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.geometry(scale),
            WorkspaceRenderElement::Fullscreen(elem) => elem.geometry(scale),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.geometry(scale),
            WorkspaceRenderElement::Window(elem) => elem.geometry(scale),
            WorkspaceRenderElement::Backdrop(elem) => elem.geometry(scale),
        }
    }

    fn location(&self, scale: Scale<f64>) -> Point<i32, smithay::utils::Physical> {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.location(scale),
            WorkspaceRenderElement::Fullscreen(elem) => elem.location(scale),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.location(scale),
            WorkspaceRenderElement::Window(elem) => elem.location(scale),
            WorkspaceRenderElement::Backdrop(elem) => elem.location(scale),
        }
    }

    fn transform(&self) -> smithay::utils::Transform {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.transform(),
            WorkspaceRenderElement::Fullscreen(elem) => elem.transform(),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.transform(),
            WorkspaceRenderElement::Window(elem) => elem.transform(),
            WorkspaceRenderElement::Backdrop(elem) => elem.transform(),
        }
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<smithay::backend::renderer::utils::CommitCounter>,
    ) -> DamageSet<i32, smithay::utils::Physical> {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.damage_since(scale, commit),
            WorkspaceRenderElement::Fullscreen(elem) => elem.damage_since(scale, commit),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.damage_since(scale, commit),
            WorkspaceRenderElement::Window(elem) => elem.damage_since(scale, commit),
            WorkspaceRenderElement::Backdrop(elem) => elem.damage_since(scale, commit),
        }
    }

    fn opaque_regions(&self, scale: Scale<f64>) -> OpaqueRegions<i32, smithay::utils::Physical> {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.opaque_regions(scale),
            WorkspaceRenderElement::Fullscreen(elem) => elem.opaque_regions(scale),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.opaque_regions(scale),
            WorkspaceRenderElement::Window(elem) => elem.opaque_regions(scale),
            WorkspaceRenderElement::Backdrop(elem) => elem.opaque_regions(scale),
        }
    }

    fn alpha(&self) -> f32 {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.alpha(),
            WorkspaceRenderElement::Fullscreen(elem) => elem.alpha(),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.alpha(),
            WorkspaceRenderElement::Window(elem) => elem.alpha(),
            WorkspaceRenderElement::Backdrop(elem) => elem.alpha(),
        }
    }
}

impl<R> RenderElement<R> for WorkspaceRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    R::TextureId: 'static,
    R::Error: FromGlesError,
{
    fn draw(
        &self,
        frame: &mut R::Frame<'_, '_>,
        src: Rectangle<f64, BufferCoords>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, smithay::utils::Physical>],
        opaque_regions: &[Rectangle<i32, Physical>],
    ) -> Result<(), R::Error> {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => {
                elem.draw(frame, src, dst, damage, opaque_regions)
            }
            WorkspaceRenderElement::Fullscreen(elem) => {
                elem.draw(frame, src, dst, damage, opaque_regions)
            }
            WorkspaceRenderElement::FullscreenPopup(elem) => {
                elem.draw(frame, src, dst, damage, opaque_regions)
            }
            WorkspaceRenderElement::Window(elem) => {
                elem.draw(frame, src, dst, damage, opaque_regions)
            }
            WorkspaceRenderElement::Backdrop(elem) => RenderElement::<GlowRenderer>::draw(
                elem,
                R::glow_frame_mut(frame),
                src,
                dst,
                damage,
                opaque_regions,
            )
            .map_err(FromGlesError::from_gles_error),
        }
    }

    fn underlying_storage(
        &self,
        renderer: &mut R,
    ) -> Option<smithay::backend::renderer::element::UnderlyingStorage<'_>> {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.underlying_storage(renderer),
            WorkspaceRenderElement::Fullscreen(elem) => elem.underlying_storage(renderer),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.underlying_storage(renderer),
            WorkspaceRenderElement::Window(elem) => elem.underlying_storage(renderer),
            WorkspaceRenderElement::Backdrop(elem) => {
                elem.underlying_storage(renderer.glow_renderer_mut())
            }
        }
    }
}

impl<R> From<RescaleRenderElement<CosmicWindowRenderElement<R>>> for WorkspaceRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    R::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: RescaleRenderElement<CosmicWindowRenderElement<R>>) -> Self {
        WorkspaceRenderElement::Fullscreen(elem)
    }
}

impl<R> From<CosmicWindowRenderElement<R>> for WorkspaceRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    R::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: CosmicWindowRenderElement<R>) -> Self {
        WorkspaceRenderElement::FullscreenPopup(elem)
    }
}

impl<R> From<WaylandSurfaceRenderElement<R>> for WorkspaceRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    R::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: WaylandSurfaceRenderElement<R>) -> Self {
        WorkspaceRenderElement::OverrideRedirect(elem)
    }
}

impl<R> From<CosmicMappedRenderElement<R>> for WorkspaceRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    R::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: CosmicMappedRenderElement<R>) -> Self {
        WorkspaceRenderElement::Window(elem)
    }
}

impl<R> From<TextureRenderElement<GlesTexture>> for WorkspaceRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    R::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: TextureRenderElement<GlesTexture>) -> Self {
        WorkspaceRenderElement::Backdrop(elem)
    }
}
