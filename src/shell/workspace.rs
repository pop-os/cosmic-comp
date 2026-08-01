use crate::backend::render::wayland::SurfaceRenderElement;
use crate::shell::focus::FocusTarget;
use crate::shell::focus::order::GameModeView;
use crate::shell::layout::tiling::RestoreTilingState;
use crate::wayland::handlers::xdg_activation::ActivationContext;
use crate::{
    backend::render::{BackdropShader, element::AsGlowRenderer, voice_orb::VoiceOrbState},
    shell::{
        OverviewMode, SeatMoveGrabState,
        layout::{
            floating::{FloatingLayout, TiledCorners},
            tiling::TilingLayout,
        },
    },
    state::State,
    utils::{prelude::*, tween::EaseRectangle},
    wayland::{
        handlers::image_copy_capture::ImageCopySessions,
        protocols::{
            toplevel_info::{toplevel_enter_output, toplevel_leave_output},
            workspace::{WorkspaceHandle, WorkspaceUpdateGuard},
        },
    },
};
use cosmic_comp_config::AppearanceConfig;
use cosmic_comp_config::workspace::{OutputMatch, PinnedWorkspace};

use crate::comp_theme::CompTheme;
use cosmic_protocols::workspace::v2::server::zcosmic_workspace_handle_v2::TilingState;
use id_tree::Tree;
use indexmap::IndexSet;
use keyframe::{ease, functions::EaseInOutCubic};
use smallvec::SmallVec;
use smithay::backend::drm::DrmNode;
use smithay::backend::renderer::element::{Kind, NamespacedElement};
use smithay::output::WeakOutput;
use smithay::utils::user_data::UserDataMap;
use smithay::{
    backend::renderer::{
        element::{
            Element, Id, RenderElement, texture::TextureRenderElement, utils::RescaleRenderElement,
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
    time::Instant,
};
use wayland_backend::server::ClientId;

use super::{
    CosmicMappedRenderElement, CosmicSurface, ResizeDirection, ResizeMode,
    element::{
        CosmicMapped, CosmicMappedKey, MaximizedState, resize_indicator::ResizeIndicator,
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
    pub fullscreen_surfaces: Vec<FullscreenSurface>,
    pub pinned: bool,
    pub id: Option<String>,
    pub name: Option<String>,

    pub handle: WorkspaceHandle,
    pub focus_stack: FocusStacks,
    pub image_copy: ImageCopySessions,
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

    pub fn unmaximize(
        &mut self,
        original_geometry: Rectangle<i32, Local>,
        original_snapped: Option<TiledCorners>,
    ) {
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
                previous.was_snapped = original_snapped;
                window.set_maximized(false);
                window.configure();
            }
        }
    }
}

/// Smallest buffer dimension (px) still treated as a game framebuffer worth
/// upscaling to fill the output. Anything smaller is a launch artifact — a
/// loading banner or splash a game maps before its real window — and is centered
/// at native size instead of being stretched across the screen.
const MIN_UPSCALE_DIM: i32 = 360;

#[derive(Debug, Clone)]
pub struct FullscreenSurface {
    pub surface: CosmicSurface,
    pub previous_state: Option<FullscreenRestoreState>,
    pub previous_geometry: Option<Rectangle<i32, Local>>,
    start_at: Option<Instant>,
    pub ended_at: Option<Instant>,
    /// When `Some`, the surface is upscaled to this rect (a fill of
    /// a smaller game buffer). The wrapping `RescaleRenderElement` is scanout-
    /// shaped so smithay hands it to the DRM plane's hardware scaler; if the
    /// plane rejects the scale the KMS thread latches `game_mode_scale_rejected`
    /// and game mode clears this back to `None` (letterbox) so we never composite
    /// a scanout-only buffer to black. `None` = native/letterbox (default).
    pub scale_to: Option<Rectangle<i32, Local>>,
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
        was_stack: bool,
    },
    Floating {
        workspace: WorkspaceHandle,
        state: FloatingRestoreData,
        was_stack: bool,
    },
    Sticky {
        output: WeakOutput,
        state: FloatingRestoreData,
        was_stack: bool,
    },
    Stack {
        state: StackRestoreData,
    },
}

impl FullscreenRestoreState {
    pub fn was_maximized(&self) -> bool {
        match self {
            FullscreenRestoreState::Floating { state, .. }
            | FullscreenRestoreState::Sticky { state, .. } => state.was_maximized,
            FullscreenRestoreState::Tiling { state, .. } => state.was_maximized,
            FullscreenRestoreState::Stack { .. } => false,
        }
    }

    // Surface was previously a single-window stack
    pub fn was_stack(&self) -> bool {
        match self {
            FullscreenRestoreState::Floating { was_stack, .. }
            | FullscreenRestoreState::Sticky { was_stack, .. } => *was_stack,
            FullscreenRestoreState::Tiling { was_stack, .. } => *was_stack,
            // Stack wasn't removed; surface was removed from the stack
            FullscreenRestoreState::Stack { .. } => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum WorkspaceRestoreData {
    Fullscreen(Option<FullscreenRestoreData>),
    Tiling(TilingRestoreData),
    Floating(FloatingRestoreData),
    Stack(StackRestoreData),
}

#[derive(Debug, Clone)]
pub struct FloatingRestoreData {
    pub geometry: Rectangle<i32, Local>,
    pub output_size: Size<i32, Logical>,
    pub was_maximized: bool,
    pub was_snapped: Option<TiledCorners>,
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
pub struct StackRestoreData {
    pub stack: CosmicMappedKey,
    pub idx: usize,
}

#[derive(Debug, Clone)]
pub struct FullscreenRestoreData {
    pub previous_state: FullscreenRestoreState,
    pub previous_geometry: Rectangle<i32, Local>,
}

#[derive(Debug, Clone, PartialEq)]
#[allow(clippy::large_enum_variant)]
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
        theme: crate::comp_theme::CompTheme,
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
            fullscreen_surfaces: Vec::new(),
            pinned: false,
            id: None,
            name: None,
            handle,
            focus_stack: FocusStacks::default(),
            image_copy: ImageCopySessions::default(),
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
        theme: crate::comp_theme::CompTheme,
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
            fullscreen_surfaces: Vec::new(),
            pinned: true,
            id: pinned.id.clone(),
            name: pinned.name.clone(),
            handle,
            focus_stack: FocusStacks::default(),
            image_copy: ImageCopySessions::default(),
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
        let output = self.explicit_output().clone();
        if self.pinned {
            debug_assert!(self.id.is_some());
            Some(PinnedWorkspace {
                output: cosmic_comp_config::workspace::OutputMatch {
                    name: output.name,
                    edid: output.edid,
                },
                tiling_enabled: self.tiling_enabled,
                id: self.id.clone(),
                name: self.name.clone(),
            })
        } else {
            None
        }
    }

    #[profiling::function]
    pub fn refresh(&mut self) {
        // seems it removes dead windows
        // self.fullscreen.take_if(|w| !w.alive());
        self.fullscreen_surfaces.retain(|w| w.alive());

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

    /// cleans up any window that is not alive anymore
    pub fn refresh_focus_stack(&mut self) {
        for (seat, stack) in self.focus_stack.0.iter_mut() {
            let fullscreen_surfaces: Vec<&CosmicSurface> = self
                .fullscreen_surfaces
                .iter()
                .filter(|f| f.alive() && f.ended_at.is_none())
                .map(|f| &f.surface)
                .collect();

            // Move grab is treated as focused, so don't change focus to a
            // window while grab exists.
            let move_grab_state = seat
                .user_data()
                .get::<SeatMoveGrabState>()
                .unwrap()
                .lock()
                .unwrap();
            let move_mapped = (*move_grab_state)
                .as_ref()
                .map(|move_grab_state| move_grab_state.element());

            let mapped = || {
                self.floating_layer
                    .mapped()
                    .chain(self.tiling_layer.mapped().map(|(w, _)| w))
                    .chain(move_mapped.iter())
            };
            stack.retain(|w| match w {
                FocusTarget::Fullscreen(s) => fullscreen_surfaces.contains(&s),
                FocusTarget::Window(w) => mapped().any(|m| w == m),
            });
        }
    }

    pub fn animations_going(&self) -> bool {
        self.tiling_layer.animations_going()
            || self.floating_layer.animations_going()
            || self.fullscreen_surfaces.iter().any(|f| f.is_animating())
            || self.dirty.swap(false, Ordering::SeqCst)
    }

    pub fn update_animations(&mut self) -> HashMap<ClientId, Client> {
        for f in self.fullscreen_surfaces.iter_mut() {
            if let Some(start) = f.start_at.as_ref() {
                let duration_since = Instant::now().duration_since(*start);
                if duration_since > self.tiling_layer.theme.motion.fullscreen {
                    f.start_at.take();
                    self.dirty.store(true, Ordering::SeqCst);
                }
            }
        }

        self.fullscreen_surfaces.retain(|f| {
            if let Some(end) = f.ended_at
                && Instant::now().duration_since(end) >= self.tiling_layer.theme.motion.fullscreen
            {
                self.dirty.store(true, Ordering::SeqCst);
                return false;
            }
            true
        });

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
        for f in self
            .fullscreen_surfaces
            .iter()
            .filter(|f| f.ended_at.is_none())
        {
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
                    WorkspaceRestoreData::Floating(previous)
                }
                MinimizedWindow::Tiling { previous, .. } => WorkspaceRestoreData::Tiling(previous),
                MinimizedWindow::Fullscreen { .. } => unreachable!(),
            });
        }

        if let Ok(state) = self.tiling_layer.unmap(mapped, None) {
            return Some(WorkspaceRestoreData::Tiling(TilingRestoreData {
                state,
                was_maximized: was_maximized.is_some(),
            }));
        }

        let was_snapped = *mapped.floating_tiled.lock().unwrap();
        // unmaximize_request might have triggered a `floating_layer.refresh()`,
        // which may have already removed a non-alive surface.
        if let Some(floating_geometry) = self.floating_layer.unmap(mapped, None).or(was_maximized) {
            return Some(WorkspaceRestoreData::Floating(FloatingRestoreData {
                geometry: floating_geometry,
                output_size: self.output.geometry().size.as_logical(),
                was_maximized: was_maximized.is_some(),
                was_snapped,
            }));
        };

        None
    }

    pub fn unmap_surface<S>(&mut self, surface: &S) -> Option<(CosmicSurface, WorkspaceRestoreData)>
    where
        CosmicSurface: PartialEq<S>,
    {
        if let Some(idx) = self
            .fullscreen_surfaces
            .iter()
            .position(|f| f.ended_at.is_none() && &f.surface == surface)
        {
            let (surface, previous_state, previous_geometry) =
                self.remove_fullscreen_at(idx).unwrap();
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
        if let Some(stack) = mapped.stack_ref()
            && stack.len() > 1
        {
            let idx = stack.surfaces().position(|s| &s == surface)?;
            return Some((
                stack.remove_idx(idx)?,
                WorkspaceRestoreData::Stack(StackRestoreData {
                    stack: mapped.key(),
                    idx,
                }),
            ));
        }

        // we know mapped is no stack with more than one element now,
        // so we can treat mapped as containing only our surface.

        let mapped = mapped.clone();
        let layer = self.unmap_element(&mapped)?;
        Some((mapped.active_window(), layer))
    }

    pub fn fullscreen_geometry_for_surface(
        &self,
        surface: &CosmicSurface,
    ) -> Rectangle<i32, Local> {
        let bbox = surface.bbox().as_local();

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
    }
    pub fn fullscreen_geometry_for(&self, fullscreen: &FullscreenSurface) -> Rectangle<i32, Local> {
        if let Some(rect) = fullscreen.scale_to {
            return rect;
        }
        self.fullscreen_geometry_for_surface(&fullscreen.surface)
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
        if !self.output.geometry().contains(location.to_i32_floor()) {
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

        if let Some(fullscreen) = self.fullscreen_surfaces.iter().find(|f| {
            !f.is_animating()
                && last_focused
                    .is_some_and(|t| matches!(t, FocusTarget::Fullscreen(s) if s == &f.surface))
        }) {
            let geometry = self.fullscreen_geometry_for(fullscreen);
            return fullscreen_element_under(fullscreen, geometry);
        }

        self.floating_layer
            .popup_element_under(location, seat)
            .or_else(|| self.tiling_layer.popup_element_under(location, seat))
            .or_else(|| {
                if last_focused.is_none_or(|t| !matches!(t, FocusTarget::Fullscreen(_)))
                    && let Some(fullscreen) = self.get_fullscreen(seat)
                {
                    let geometry = self.fullscreen_geometry_for(fullscreen);
                    return fullscreen_element_under(fullscreen, geometry);
                }
                None
            })
    }

    pub fn toplevel_element_under(
        &self,
        location: Point<f64, Global>,
        seat: &Seat<State>,
    ) -> Option<KeyboardFocusTarget> {
        if !self.output.geometry().contains(location.to_i32_floor()) {
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

        if let Some(fullscreen) = self.fullscreen_surfaces.iter().find(|fs| {
            !fs.is_animating()
                && last_focused
                    .is_some_and(|t| matches!(t, FocusTarget::Fullscreen(f) if f == &fs.surface))
        }) {
            let geometry = self.fullscreen_geometry_for(fullscreen);
            return fullscreen_element_under(fullscreen, geometry);
        }

        self.floating_layer
            .toplevel_element_under(location, seat)
            .or_else(|| self.tiling_layer.toplevel_element_under(location, seat))
            .or_else(|| {
                if last_focused.is_none_or(|t| !matches!(t, FocusTarget::Fullscreen(_)))
                    && let Some(fullscreen) = self.get_fullscreen(seat)
                {
                    let geometry = self.fullscreen_geometry_for(fullscreen);
                    return fullscreen_element_under(fullscreen, geometry);
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
        if !self.output.geometry().contains(location.to_i32_floor()) {
            return None;
        }
        let location = location.to_local(&self.output);

        let check_fullscreen = |fullscreen: &FullscreenSurface| {
            if !fullscreen.is_animating() {
                let geometry = self.fullscreen_geometry_for(fullscreen);
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

        self.fullscreen_surfaces
            .iter()
            .find(|f| last_focused.is_some_and(|t| t == &f.surface))
            .and_then(check_fullscreen)
            .or_else(|| self.floating_layer.popup_surface_under(location, seat))
            .or_else(|| {
                self.tiling_layer
                    .popup_surface_under(location, overview, seat)
            })
            .or_else(|| self.get_fullscreen(seat).and_then(check_fullscreen))
            .map(|(m, p)| (m, p.to_global(&self.output)))
    }

    pub fn toplevel_surface_under(
        &self,
        location: Point<f64, Global>,
        overview: OverviewMode,
        seat: &Seat<State>,
    ) -> Option<(PointerFocusTarget, Point<f64, Global>)> {
        if !self.output.geometry().contains(location.to_i32_floor()) {
            return None;
        }
        let location = location.to_local(&self.output);

        let check_fullscreen = |fullscreen: &FullscreenSurface| {
            if !fullscreen.is_animating() {
                let geometry = self.fullscreen_geometry_for(fullscreen);
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

        self.fullscreen_surfaces
            .iter()
            .find(|f| last_focused.is_some_and(|t| t == &f.surface))
            .and_then(check_fullscreen)
            .or_else(|| self.floating_layer.toplevel_surface_under(location, seat))
            .or_else(|| {
                self.tiling_layer
                    .toplevel_surface_under(location, overview, seat)
            })
            .or_else(|| self.get_fullscreen(seat).and_then(check_fullscreen))
            .map(|(m, p)| (m, p.to_global(&self.output)))
    }

    /// Keyboard/click hit-test restricted to the game-mode controlled surface.
    ///
    /// Under strict game-mode control this workspace renders ONLY that surface
    /// (see `render`), so input must not reach anything else — otherwise a window
    /// that draws zero pixels (an un-adopted game, a dialog, a login window)
    /// could take focus and clicks while being invisible, which looks exactly
    /// like a hung game.
    pub fn controlled_element_under(
        &self,
        location: Point<f64, Global>,
        view: GameModeView<'_>,
        seat: &Seat<State>,
    ) -> Option<KeyboardFocusTarget> {
        if !self.output.geometry().contains(location.to_i32_round()) {
            return None;
        }
        // Children stack above the base, so they are hit-tested first (topmost =
        // last in the set), exactly matching the order `render` emits them in.
        for child in view.children.iter() {
            let Some((mapped, relative)) = self.controlled_child_at(view, child, location) else {
                continue;
            };
            if mapped
                .focus_under(
                    relative.as_logical(),
                    WindowSurfaceType::TOPLEVEL | WindowSurfaceType::SUBSURFACE,
                    seat,
                )
                .is_some()
            {
                return Some(KeyboardFocusTarget::from(mapped.clone()));
            }
        }
        let location = location.to_local(&self.output);
        let fullscreen = self
            .fullscreen_surfaces
            .iter()
            .find(|f| &f.surface == view.base)?;
        let (surface_point, _) = self.controlled_surface_transform(fullscreen, location);
        fullscreen
            .surface
            .0
            .surface_under(
                surface_point.as_logical(),
                WindowSurfaceType::TOPLEVEL | WindowSurfaceType::SUBSURFACE,
            )
            .is_some()
            .then(|| KeyboardFocusTarget::Fullscreen(fullscreen.surface.clone()))
    }

    /// Resolves a controlled-set child to its mapped element and the pointer
    /// position RELATIVE to where that child is actually rendered.
    ///
    /// Uses the identical origin as the child render loop in [`Workspace::render`]
    /// (`element_geometry().loc - mapped.geometry().loc`, the convention the
    /// floating and tiling layers also use). `element_geometry` alone is the
    /// window-geometry position, which differs from the surface/bbox origin by the
    /// client's frame extents — non-zero for exactly the clients this feature
    /// targets (XWayland CEF/Chromium login windows advertising _GTK_FRAME_EXTENTS,
    /// and CSD shadow insets), so hit-testing against it would be offset from what
    /// the user sees.
    fn controlled_child_at(
        &self,
        _view: GameModeView<'_>,
        child: &CosmicSurface,
        location: Point<f64, Global>,
    ) -> Option<(&CosmicMapped, Point<f64, Local>)> {
        let mapped = self
            .mapped()
            .find(|m| m.windows().any(|(surface, _)| &surface == child))?;
        let geometry = self.element_geometry(mapped)?;
        let render_origin = geometry.loc - mapped.geometry().loc.as_local();
        Some((
            mapped,
            location.to_local(&self.output) - render_origin.to_f64(),
        ))
    }

    /// Maps a point in output-local space into the controlled surface's own
    /// coordinate space, returning it alongside the (x, y) scale that was undone.
    ///
    /// This is the INVERSE of how `render` presents a controlled fullscreen: the
    /// surface is drawn at `fullscreen_geometry_for(..).loc` and, when `scale_to`
    /// is set, wrapped in a `RescaleRenderElement` scaling its buffer to that
    /// rect. Hit-testing has to undo BOTH — undoing only the offset makes clicks
    /// land at the wrong spot on an upscaled game (off by the scale ratio).
    fn controlled_surface_transform(
        &self,
        fullscreen: &FullscreenSurface,
        location: Point<f64, Local>,
    ) -> (Point<f64, Local>, (f64, f64)) {
        let geometry = self.fullscreen_geometry_for(fullscreen);
        let src = fullscreen.surface.bbox().size;
        let scale = if fullscreen.scale_to.is_some() && src.w > 0 && src.h > 0 {
            (
                geometry.size.w as f64 / src.w as f64,
                geometry.size.h as f64 / src.h as f64,
            )
        } else {
            (1.0, 1.0)
        };
        let relative = location - geometry.loc.to_f64();
        (
            Point::from((relative.x / scale.0, relative.y / scale.1)),
            scale,
        )
    }

    /// Pointer hit-test restricted to the game-mode controlled surface — the
    /// pointer counterpart of [`Workspace::controlled_element_under`].
    pub fn controlled_surface_under(
        &self,
        location: Point<f64, Global>,
        view: GameModeView<'_>,
        seat: &Seat<State>,
    ) -> Option<(PointerFocusTarget, Point<f64, Global>)> {
        if !self.output.geometry().contains(location.to_i32_round()) {
            return None;
        }
        // Children first (topmost last-in-set), mirroring `render`'s stacking.
        for child in view.children.iter() {
            let Some((mapped, relative)) = self.controlled_child_at(view, child, location) else {
                continue;
            };
            let render_origin = location.to_local(&self.output) - relative;
            if let Some((target, surface_offset)) = mapped.focus_under(
                relative.as_logical(),
                WindowSurfaceType::TOPLEVEL | WindowSurfaceType::SUBSURFACE,
                seat,
            ) {
                return Some((
                    target,
                    (render_origin + surface_offset.as_local()).to_global(&self.output),
                ));
            }
        }
        let location = location.to_local(&self.output);
        let fullscreen = self
            .fullscreen_surfaces
            .iter()
            .find(|f| &f.surface == view.base)?;
        if fullscreen.is_animating() {
            return None;
        }
        let geometry = self.fullscreen_geometry_for(fullscreen);
        let (surface_point, scale) = self.controlled_surface_transform(fullscreen, location);
        fullscreen
            .surface
            .focus_under(
                surface_point.as_logical(),
                WindowSurfaceType::TOPLEVEL | WindowSurfaceType::SUBSURFACE,
            )
            .map(|(target, surface_offset)| {
                // Re-apply the presentation transform so the reported position is
                // where the surface actually appears on screen.
                let offset = surface_offset.as_local();
                let presented = Point::<f64, Local>::from((
                    geometry.loc.x as f64 + offset.x * scale.0,
                    geometry.loc.y as f64 + offset.y * scale.1,
                ));
                (target, presented.to_global(&self.output))
            })
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
        self.unmaximize_request_with_options(elem)
    }

    /// Unmaximize with pipelined client-driven resize animation.
    pub fn unmaximize_request_with_options(
        &mut self,
        elem: &CosmicMapped,
    ) -> Option<Rectangle<i32, Local>> {
        let mut state = elem.maximized_state.lock().unwrap();
        if let Some(state) = state.take() {
            if let Some(minimized) = self.minimized_windows.iter_mut().find(|m| *m == elem) {
                minimized.unmaximize(state.original_geometry, state.original_snapped);
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

                        if use_geometry {
                            self.floating_layer
                                .start_pipelined_unmaximize(elem.clone(), state.original_geometry);
                        } else {
                            elem.set_maximized(false);
                            elem.set_tiled(false);
                            self.floating_layer
                                .map_internal(elem.clone(), None, None, None);
                        }
                        // Re-apply the snap if the window was snapped before maximizing
                        if let Some(corners) = state.original_snapped {
                            self.floating_layer.snap_to_corner(elem, &corners);
                        }
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
        if let Some(idx) = self
            .fullscreen_surfaces
            .iter()
            .position(|f| f.ended_at.is_none() && &f.surface == surface)
        {
            let fullscreen_state = self.fullscreen_surfaces.get(idx)?.clone();
            {
                let f = self.fullscreen_surfaces.get_mut(idx)?;
                f.previous_geometry = Some(to);
                f.ended_at = Some(
                    Instant::now()
                        - (self.tiling_layer.theme.motion.fullscreen
                            - f.start_at
                                .take()
                                .map(|earlier| {
                                    Instant::now()
                                        .duration_since(earlier)
                                        .min(self.tiling_layer.theme.motion.fullscreen)
                                })
                                .unwrap_or(self.tiling_layer.theme.motion.fullscreen)),
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
            original_snapped,
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
            // Restore the snap marker to lett unminimize re-apply it.
            if let Some(corners) = original_snapped {
                *mapped.floating_tiled.lock().unwrap() = Some(corners);
            }
            Some(original_geometry)
        } else {
            None
        };

        mapped.set_minimized(true);
        mapped.configure();

        let was_snapped = *mapped.floating_tiled.lock().unwrap();
        if let Some(geometry) = self.floating_layer.unmap(&mapped, Some(to)) {
            return Some(MinimizedWindow::Floating {
                window: mapped,
                previous: FloatingRestoreData {
                    geometry: was_maximized.unwrap_or(geometry),
                    output_size: self.output.geometry().size.as_logical(),
                    was_maximized: was_maximized.is_some(),
                    was_snapped,
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
                surface.set_minimized(false);
                // focus it so it's the top fullscreen window
                self.focus_stack
                    .get_mut(seat)
                    .append(FocusTarget::Fullscreen(surface.clone()));
                self.fullscreen_surfaces.push(FullscreenSurface {
                    surface,
                    previous_state: previous.clone().map(|p| p.previous_state),
                    previous_geometry: previous.map(|p| p.previous_geometry),
                    start_at: None,
                    ended_at: None,
                    scale_to: None,
                });
                self.dirty.store(true, Ordering::SeqCst);
                None
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
                        original_snapped: previous.was_snapped,
                    });
                    std::mem::drop(state);
                    self.floating_layer.map_maximized(window, geometry, true);
                } else if let Some(corners) = previous.was_snapped {
                    self.floating_layer.snap_to_corner(&window, &corners);
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
                            original_snapped: None,
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
                            original_snapped: None,
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
    ) {
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

        self.dirty.store(true, Ordering::SeqCst);
        self.fullscreen_surfaces.push(FullscreenSurface {
            surface: window.clone(),
            previous_state: restore,
            previous_geometry,
            start_at: Some(Instant::now()),
            ended_at: None,
            scale_to: None,
        });
        // Bug 1: the entrance animation clock starts NOW, before the client has
        // committed a fullscreen-sized buffer — until its first frame lands the
        // surface renders empty (grey) while the switch animates in.
        tracing::debug!(
            target: crate::logger::GAMING_TARGET,
            app_id = %window.app_id(),
            output = %self.output.name(),
            geo_w = self.output.geometry().size.w,
            geo_h = self.output.geometry().size.h,
            "map_fullscreen: surface promoted to fullscreen"
        );
    }

    /// Set (or clear) the upscale target for a tracked fullscreen
    /// surface. `scale` requests a fill: if the surface's committed buffer is
    /// smaller than the output, `scale_to` becomes the aspect-preserving fit rect
    /// (centered); otherwise (or when `scale` is false) it is cleared to `None`
    /// (native/letterbox). See `FullscreenSurface::scale_to`.
    /// Where a fullscreen surface should be presented on the output, for the
    /// requested scaling `mode`.
    ///
    /// `src` is what the game actually renders (its committed buffer, or the
    /// spoofed resolution it was configured with) and `out` the output. Returns
    /// `None` to present 1:1 with no wrapper, which is what keeps a native-size
    /// game on the direct-scanout path.
    fn scaling_rect(
        src: Size<i32, Logical>,
        out: Size<i32, Local>,
        mode: crate::dbus::game_mode::ScalingMode,
    ) -> Option<Rectangle<i32, Local>> {
        use crate::dbus::game_mode::ScalingMode;

        if src.w <= 0 || src.h <= 0 || out.w <= 0 || out.h <= 0 {
            return None;
        }
        let (sw, sh, ow, oh) = (src.w as f64, src.h as f64, out.w as f64, out.h as f64);
        let centered = |w: i32, h: i32| {
            Some(Rectangle::new(
                Point::from(((out.w - w) / 2, (out.h - h) / 2)),
                Size::from((w, h)),
            ))
        };
        match mode {
            // Present at its own size, centered. Nothing to do when it already
            // fills the output — and returning None there matters, because an
            // unwrapped element is the one that can take a DRM plane directly.
            ScalingMode::Native => {
                if src.w == out.w && src.h == out.h {
                    None
                } else {
                    centered(src.w, src.h)
                }
            }
            // Whole-pixel multiples only: no resampling, so the image stays sharp.
            ScalingMode::Integer => {
                let factor = f64::min(ow / sw, oh / sh).floor().max(1.0);
                centered((sw * factor) as i32, (sh * factor) as i32)
            }
            // Letterbox: fit entirely, preserving aspect.
            // FSR is the same geometry until the sharpening pass exists.
            ScalingMode::Fit | ScalingMode::Fsr => {
                let ratio = f64::min(ow / sw, oh / sh);
                centered((sw * ratio).round() as i32, (sh * ratio).round() as i32)
            }
            // Cover the output, preserving aspect; the overflow is cropped by the
            // render path's crop-to-output.
            ScalingMode::Fill => {
                let ratio = f64::max(ow / sw, oh / sh);
                centered((sw * ratio).round() as i32, (sh * ratio).round() as i32)
            }
            // Fill exactly, aspect be damned (the element scale is non-uniform).
            ScalingMode::Stretch => Some(Rectangle::new(
                Point::from((0, 0)),
                Size::from((out.w, out.h)),
            )),
        }
    }

    /// Set (or clear) the presentation target for a tracked fullscreen surface.
    ///
    /// `mode` is the requested scaling mode; `scale` is false when scaling must be
    /// suppressed entirely (the launcher, or a DRM plane that rejected the scale),
    /// in which case an undersized surface is centered at native size rather than
    /// stretched or corner-anchored.
    pub fn set_fullscreen_scale_to<S>(
        &mut self,
        surface: &S,
        scale: bool,
        mode: crate::dbus::game_mode::ScalingMode,
    ) where
        CosmicSurface: PartialEq<S>,
    {
        let out = self.output.geometry().size.as_local();
        if let Some(fs) = self
            .fullscreen_surfaces
            .iter_mut()
            .find(|f| f.ended_at.is_none() && &f.surface == surface)
        {
            let src = fs.surface.bbox().size;
            // Too small to be a game framebuffer — a loading banner or splash a
            // game maps before its real window — or scaling was refused: centre it
            // at native size instead of stretching it across the output.
            let scalable = scale && src.w >= MIN_UPSCALE_DIM && src.h >= MIN_UPSCALE_DIM;
            let mode = if scalable {
                mode
            } else {
                crate::dbus::game_mode::ScalingMode::Native
            };
            fs.scale_to = Self::scaling_rect(src, out, mode);
        }
    }

    #[must_use]
    pub fn take_fullscreen<S>(
        &mut self,
        surface: &S,
    ) -> Option<(
        CosmicSurface,
        Option<FullscreenRestoreState>,
        Option<Rectangle<i32, Local>>,
    )>
    where
        CosmicSurface: PartialEq<S>,
    {
        let idx = self
            .fullscreen_surfaces
            .iter()
            .position(|f| f.ended_at.is_none() && &f.surface == surface)?;
        let fs = self.fullscreen_surfaces.remove(idx);

        for focus_stack in self.focus_stack.0.values_mut() {
            focus_stack.retain(|t| t != &fs.surface);
        }

        Some((fs.surface, fs.previous_state, fs.previous_geometry))
    }

    #[must_use]
    pub fn remove_fullscreen_at(
        &mut self,
        idx: usize,
    ) -> Option<(
        CosmicSurface,
        Option<FullscreenRestoreState>,
        Option<Rectangle<i32, Local>>,
    )> {
        // if it doesn't exist we move on.
        let surface = self.fullscreen_surfaces.get_mut(idx)?;
        // if already being removed, do nothing
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
                - (self.tiling_layer.theme.motion.fullscreen
                    - surface
                        .start_at
                        .take()
                        .map(|earlier| {
                            Instant::now()
                                .duration_since(earlier)
                                .min(self.tiling_layer.theme.motion.fullscreen)
                        })
                        .unwrap_or(self.tiling_layer.theme.motion.fullscreen)),
        );

        Some((
            surface.surface.clone(),
            surface.previous_state.clone(),
            surface.previous_geometry,
        ))
    }

    #[must_use]
    pub fn remove_fullscreen_surface<S>(
        &mut self,
        surface: &S,
    ) -> Option<(
        CosmicSurface,
        Option<FullscreenRestoreState>,
        Option<Rectangle<i32, Local>>,
    )>
    where
        CosmicSurface: PartialEq<S>,
    {
        let idx = self
            .fullscreen_surfaces
            .iter()
            .position(|f| f.ended_at.is_none() && &f.surface == surface)?;
        self.remove_fullscreen_at(idx)
    }

    pub fn get_fullscreen(&self, seat: &Seat<State>) -> Option<&FullscreenSurface> {
        let stack = self.focus_stack.get(seat);
        stack
            .iter()
            .find_map(|t| {
                if let FocusTarget::Fullscreen(s) = t {
                    self.fullscreen_surfaces
                        .iter()
                        .find(|f| f.alive() && f.ended_at.is_none() && &f.surface == s)
                } else {
                    None
                }
            })
            .or_else(|| {
                self.fullscreen_surfaces
                    .iter()
                    .rev()
                    .find(|f| f.alive() && f.ended_at.is_none())
            })
    }

    pub fn get_fullscreen_surfaces(&self) -> impl Iterator<Item = &FullscreenSurface> {
        self.fullscreen_surfaces
            .iter()
            .filter(|f| f.alive() && f.ended_at.is_none())
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
                original_snapped: None,
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
            + self
                .fullscreen_surfaces
                .iter()
                .filter(|f| f.ended_at.is_none())
                .count()
    }

    pub fn is_empty(&self) -> bool {
        self.floating_layer.mapped().next().is_none()
            && self.tiling_layer.mapped().next().is_none()
            && self.minimized_windows.is_empty()
            && self.fullscreen_surfaces.is_empty()
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
        theme: &CompTheme,
        window_alpha: f32,
        attached_orb_state: Option<&VoiceOrbState>,
        scanout_node: Option<DrmNode>,
        game_mode_only: Option<GameModeView<'_>>,
        push: &mut dyn FnMut(WorkspaceRenderElement<R>),
    ) where
        R: AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        CosmicWindowRenderElement<R>: RenderElement<R>,
        CosmicStackRenderElement<R>: RenderElement<R>,
        WorkspaceRenderElement<R>: RenderElement<R>,
    {
        let output_scale = self.output.current_scale().fractional_scale();
        let zone = {
            let layer_map = layer_map_for_output(&self.output);
            layer_map.non_exclusive_zone().as_local()
        };
        let focused = self.focus_stack.get(last_active_seat).last().cloned();
        let fullscreen_focused = matches!(focused, Some(FocusTarget::Fullscreen(_)));

        // MERGE: upstream's `render_fullscreen` pushes straight through `push` when the
        // fullscreen is focused and only buffers otherwise. We always buffer, because the
        // strict game-mode path below has to emit the game *underneath* its own children
        // before returning early; the buffer is flushed at the equivalent points.
        let mut fullscreen_elements = SmallVec::<[WorkspaceRenderElement<R>; 2]>::new_const();
        {
            let mut render_fullscreen =
                |fullscreen: &FullscreenSurface, renderer: &mut R, output_scale: f64| {
                    let fullscreen_geo = self.fullscreen_geometry_for(fullscreen);
                    let previous_geo = fullscreen
                        .previous_geometry
                        .as_ref()
                        .unwrap_or(&fullscreen_geo);

                    let (target_geo, fullscreen_alpha) =
                        match (fullscreen.start_at, fullscreen.ended_at) {
                            (Some(started), _) => {
                                let duration = Instant::now().duration_since(started).as_secs_f64()
                                    / self.tiling_layer.theme.motion.fullscreen.as_secs_f64();
                                (
                                    ease(
                                        EaseInOutCubic,
                                        EaseRectangle(*previous_geo),
                                        EaseRectangle(fullscreen_geo),
                                        duration,
                                    )
                                    .0,
                                    ease(EaseInOutCubic, 0.0, 1.0, duration) * window_alpha,
                                )
                            }
                            (_, Some(ended)) => {
                                let duration = Instant::now().duration_since(ended).as_secs_f64()
                                    / self.tiling_layer.theme.motion.fullscreen.as_secs_f64();
                                (
                                    ease(
                                        EaseInOutCubic,
                                        EaseRectangle(fullscreen_geo),
                                        EaseRectangle(*previous_geo),
                                        duration,
                                    )
                                    .0,
                                    ease(EaseInOutCubic, 1.0, 0.0, duration) * window_alpha,
                                )
                            }
                            (None, None) => (fullscreen_geo, 1.0 * window_alpha),
                        };

                    let render_loc = target_geo
                        .loc
                        .as_logical()
                        .to_physical_precise_round(output_scale);

                    // Wrap in a RescaleRenderElement when animating (entrance/exit), or
                    // when a `scale_to` upscale is requested (fill). The
                    // wrapper forwards src/kind/underlying_storage, so smithay can still
                    // hand it to a DRM plane's HARDWARE scaler (no GLES composition) —
                    // and if the plane rejects the scale, the KMS thread latches
                    // `game_mode_scale_rejected` and game mode clears `scale_to`, so a
                    // settled game with no scale request is left UNWRAPPED (direct scanout,
                    // never composited-to-black for scanout-only Proton/Vulkan buffers).
                    let scaling = fullscreen.scale_to.is_some();
                    // Upscale source: the committed BUFFER (bbox) for a fill, or the
                    // window geometry for the entrance/exit animation.
                    //
                    // Resolved HERE rather than inside the callback below. Both
                    // `bbox()` and `geometry()` take the surface's state lock, and the
                    // callback runs inside `with_surface_tree_downward`, which already
                    // holds that same lock for the surface being visited -- so asking
                    // from in there deadlocks the render thread against itself. (The
                    // callback only became re-entrant on the traversal when the element
                    // API moved from returning a Vec to pushing.) It is also loop
                    // invariant, so this is one lookup instead of one per element.
                    let src = if scaling {
                        fullscreen.surface.bbox().size
                    } else {
                        fullscreen.surface.0.geometry().size
                    };
                    let is_animating = fullscreen.is_animating();
                    let animation_rescale = |elem| {
                        if (is_animating || scaling) && src.w > 0 && src.h > 0 {
                            let scale = Scale {
                                x: target_geo.size.w as f64 / src.w as f64,
                                y: target_geo.size.h as f64 / src.h as f64,
                            };

                            RescaleRenderElement::from_element(elem, render_loc, scale).into()
                        } else {
                            Into::<WorkspaceRenderElement<_>>::into(elem)
                        }
                    };

                    let mut fullscreen_push = |elem: SurfaceRenderElement<R>| {
                        fullscreen_elements.push(animation_rescale(elem.into()))
                    };
                    fullscreen.surface.push_render_elements(
                        renderer,
                        render_loc,
                        output_scale.into(),
                        fullscreen_alpha,
                        Some(true),
                        scanout_node,
                        false,
                        [0, 0, 0, 0],
                        0,
                        &mut fullscreen_push,
                        None,
                    );
                };

            // Strict game-mode fullscreen control: on the game-mode output this
            // workspace renders ONLY the compositor-controlled surface. A window that
            // raw-fullscreens itself (a Proton game before playserve tags it and game
            // mode adopts it) is in `fullscreen_surfaces` and would otherwise become the
            // top fullscreen and render — instead it stays completely invisible until it
            // becomes the controlled `game_surface`, at which point it renders (and its
            // entrance animation fades it in) like any adopted game. The game-mode
            // overlay (QAM/launcher) is a separate stage, so it still composites on top.
            if let Some(view) = game_mode_only {
                if let Some(fs) = self
                    .fullscreen_surfaces
                    .iter()
                    .find(|f| &f.surface == view.base)
                {
                    render_fullscreen(fs, renderer, output_scale);
                }
            } else {
                let top_fullscreen = self.get_fullscreen(last_active_seat);

                if let Some(fs) = top_fullscreen {
                    render_fullscreen(fs, renderer, output_scale)
                }
                // Also render any animating (entering/exiting) fullscreens
                for fs in self.fullscreen_surfaces.iter().filter(|f| f.is_animating()) {
                    if top_fullscreen.is_none_or(|top| top.surface != fs.surface) {
                        render_fullscreen(fs, renderer, output_scale);
                    };
                }
            }
        }

        if let Some(view) = game_mode_only {
            // The game's OWN children (dialogs, EULA/launcher windows, in-prefix
            // login/browser windows) stack ABOVE it. Elements are front-to-back, so
            // children are emitted first, topmost last-in-the-set first.
            let mut lower_elements = Vec::new();
            for child in view.children.iter() {
                let Some(mapped) = self
                    .mapped()
                    .find(|m| m.windows().any(|(surface, _)| &surface == child))
                else {
                    continue;
                };
                let Some(geometry) = self.element_geometry(mapped) else {
                    continue;
                };
                let render_location = geometry.loc - mapped.geometry().loc.as_local();
                mapped.push_render_elements(
                    renderer,
                    render_location
                        .as_logical()
                        .to_physical_precise_round(output_scale),
                    None,
                    output_scale.into(),
                    window_alpha,
                    None,
                    scanout_node,
                    &mut |elem| push(WorkspaceRenderElement::from(elem)),
                    &mut |elem| lower_elements.push(WorkspaceRenderElement::from(elem)),
                );
                for elem in lower_elements.drain(..) {
                    push(elem);
                }
            }

            // ...and the controlled game itself goes underneath them.
            for elem in fullscreen_elements {
                push(elem);
            }
            return;
        }

        if fullscreen_focused {
            for elem in fullscreen_elements.drain(..) {
                push(elem);
            }
        }

        let any_fullscreen_animating = self
            .fullscreen_surfaces
            .iter()
            .any(|f| f.start_at.is_some() || f.ended_at.is_some());
        if !fullscreen_focused
            || any_fullscreen_animating
            || self
                .fullscreen_surfaces
                .iter()
                .all(|f| !f.alive() || f.ended_at.is_some())
        {
            // floating surfaces
            let floating_alpha = match &overview.0 {
                OverviewMode::Started(_, started) => {
                    (1.0 - (Instant::now().duration_since(*started).as_millis()
                        / self.tiling_layer.theme.motion.animation.as_millis())
                        as f32)
                        .max(0.0)
                        * 0.4
                        + 0.6
                }
                OverviewMode::Ended(_, ended) => {
                    ((Instant::now().duration_since(*ended).as_millis()
                        / self.tiling_layer.theme.motion.animation.as_millis())
                        as f32)
                        * 0.4
                        + 0.6
                }
                OverviewMode::Active(_) => 0.6,
                OverviewMode::None => 1.0,
            } * window_alpha;

            // MERGE: dropped our `element_filter` argument — every remaining consumer of it
            // in the floating layer was blur capture, which upstream's frosted-glass
            // implementation replaces.
            self.floating_layer.render(
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
                floating_alpha,
                theme,
                attached_orb_state,
                scanout_node,
                &mut |elem| push(elem.into()),
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
            self.tiling_layer.render(
                renderer,
                render_focus.then_some(last_active_seat),
                render_focus
                    .then(|| {
                        focused.as_ref().and_then(|target| {
                            if let FocusTarget::Window(mapped) = target {
                                Some(mapped)
                            } else {
                                None
                            }
                        })
                    })
                    .flatten(),
                zone,
                overview,
                resize_indicator,
                indicator_thickness,
                theme,
                scanout_node,
                &mut |elem| push(elem.into()),
            );

            if let Some(alpha) = alpha {
                push(
                    Into::<CosmicMappedRenderElement<R>>::into(BackdropShader::element(
                        renderer,
                        self.backdrop_id.clone(),
                        Rectangle::from_size(self.output.geometry().size.as_local()),
                        [0.0; 4],
                        alpha * 0.85,
                        [0.0, 0.0, 0.0],
                    ))
                    .into(),
                )
            }
        }

        for elem in fullscreen_elements {
            push(elem);
        }
    }

    #[profiling::function]
    pub fn render_popups<'a, R>(
        &self,
        renderer: &mut R,
        last_active_seat: &Seat<State>,
        render_focus: bool,
        overview: (OverviewMode, Option<(SwapIndicator, Option<&Tree<Data>>)>),
        theme: &CompTheme,
        scanout_node: Option<DrmNode>,
        game_mode_only: Option<GameModeView<'_>>,
        push: &mut dyn FnMut(WorkspaceRenderElement<R>),
    ) where
        R: AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        CosmicWindowRenderElement<R>: RenderElement<R>,
        CosmicStackRenderElement<R>: RenderElement<R>,
        WorkspaceRenderElement<R>: RenderElement<R>,
    {
        let output_scale = self.output.current_scale().fractional_scale();
        let zone = {
            let layer_map = layer_map_for_output(&self.output);
            layer_map.non_exclusive_zone().as_local()
        };

        // Render popups for the top (most recently focused) fullscreen — but under
        // strict game-mode control, for the CONTROLLED base instead. Keying on the
        // seat's fullscreen there is wrong in both directions: a suppressed window's
        // popups would render over the game, and the game's own popups would vanish
        // the moment one of its dialogs took focus.
        let focus_stack = self.focus_stack.get(last_active_seat);
        let top_fullscreen = match game_mode_only {
            Some(view) => self
                .fullscreen_surfaces
                .iter()
                .find(|f| &f.surface == view.base),
            None => self.get_fullscreen(last_active_seat),
        };

        if let Some(fullscreen) = top_fullscreen {
            let fullscreen_geo = self.fullscreen_geometry_for(fullscreen);
            let previous_geo = fullscreen
                .previous_geometry
                .as_ref()
                .unwrap_or(&fullscreen_geo);

            let (target_geo, alpha) = match (fullscreen.start_at, fullscreen.ended_at) {
                (Some(started), _) => {
                    let duration = Instant::now().duration_since(started).as_secs_f64()
                        / self.tiling_layer.theme.motion.fullscreen.as_secs_f64();
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
                        / self.tiling_layer.theme.motion.fullscreen.as_secs_f64();
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

            fullscreen.surface.push_popup_render_elements(
                renderer,
                render_loc,
                output_scale.into(),
                alpha,
                scanout_node,
                0,
                &mut |elem| push(WorkspaceRenderElement::FullscreenPopup(elem.into())),
            );
        }

        let any_fullscreen_animating = self
            .fullscreen_surfaces
            .iter()
            .any(|f| f.start_at.is_some() || f.ended_at.is_some());
        if !matches!(focus_stack.last(), Some(FocusTarget::Fullscreen(_)))
            || any_fullscreen_animating
            || self
                .fullscreen_surfaces
                .iter()
                .all(|f| !f.alive() || f.ended_at.is_some())
        {
            // floating surfaces
            let alpha = match &overview.0 {
                OverviewMode::Started(_, started) => {
                    (1.0 - (Instant::now().duration_since(*started).as_millis()
                        / self.tiling_layer.theme.motion.animation.as_millis())
                        as f32)
                        .max(0.0)
                        * 0.4
                        + 0.6
                }
                OverviewMode::Ended(_, ended) => {
                    ((Instant::now().duration_since(*ended).as_millis()
                        / self.tiling_layer.theme.motion.animation.as_millis())
                        as f32)
                        * 0.4
                        + 0.6
                }
                OverviewMode::Active(_) => 0.6,
                OverviewMode::None => 1.0,
            };

            if let Some(view) = game_mode_only {
                // Under strict control the layers must NOT be asked for every
                // element's popups: that would render popups belonging to windows
                // this workspace deliberately hides, painting a suppressed window's
                // menu over the game. Emit popups for the controlled children only,
                // using the same origin the child itself is rendered at.
                for child in view.children.iter() {
                    let Some(mapped) = self
                        .mapped()
                        .find(|m| m.windows().any(|(surface, _)| &surface == child))
                    else {
                        continue;
                    };
                    let Some(geometry) = self.element_geometry(mapped) else {
                        continue;
                    };
                    let render_location = geometry.loc - mapped.geometry().loc.as_local();
                    mapped.push_popup_render_elements(
                        renderer,
                        render_location
                            .as_logical()
                            .to_physical_precise_round(output_scale),
                        output_scale.into(),
                        alpha,
                        scanout_node,
                        &mut |elem| push(WorkspaceRenderElement::from(elem)),
                    );
                }
            } else {
                self.floating_layer
                    .render_popups(renderer, alpha, scanout_node, &mut |elem| push(elem.into()));

                //tiling surfaces
                self.tiling_layer.render_popups(
                    renderer,
                    render_focus.then_some(last_active_seat),
                    zone,
                    overview,
                    theme,
                    scanout_node,
                    &mut |elem| push(elem.into()),
                );
            }
        }
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
    R: AsGlowRenderer,
    R::TextureId: Send + 'static,
{
    OverrideRedirect(SurfaceRenderElement<R>),
    LowerLayerShell(NamespacedElement<SurfaceRenderElement<R>>),
    Fullscreen(RescaleRenderElement<CosmicWindowRenderElement<R>>),
    FullscreenPopup(CosmicWindowRenderElement<R>),
    Window(CosmicMappedRenderElement<R>),
    Backdrop(TextureRenderElement<GlesTexture>),
}

impl<R> Element for WorkspaceRenderElement<R>
where
    R: AsGlowRenderer,
    R::TextureId: Send + 'static,
{
    fn id(&self) -> &smithay::backend::renderer::element::Id {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.id(),
            WorkspaceRenderElement::LowerLayerShell(elem) => elem.id(),
            WorkspaceRenderElement::Fullscreen(elem) => elem.id(),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.id(),
            WorkspaceRenderElement::Window(elem) => elem.id(),
            WorkspaceRenderElement::Backdrop(elem) => elem.id(),
        }
    }

    fn current_commit(&self) -> smithay::backend::renderer::utils::CommitCounter {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.current_commit(),
            WorkspaceRenderElement::LowerLayerShell(elem) => elem.current_commit(),
            WorkspaceRenderElement::Fullscreen(elem) => elem.current_commit(),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.current_commit(),
            WorkspaceRenderElement::Window(elem) => elem.current_commit(),
            WorkspaceRenderElement::Backdrop(elem) => elem.current_commit(),
        }
    }

    fn src(&self) -> Rectangle<f64, smithay::utils::Buffer> {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.src(),
            WorkspaceRenderElement::LowerLayerShell(elem) => elem.src(),
            WorkspaceRenderElement::Fullscreen(elem) => elem.src(),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.src(),
            WorkspaceRenderElement::Window(elem) => elem.src(),
            WorkspaceRenderElement::Backdrop(elem) => elem.src(),
        }
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, smithay::utils::Physical> {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.geometry(scale),
            WorkspaceRenderElement::LowerLayerShell(elem) => elem.geometry(scale),
            WorkspaceRenderElement::Fullscreen(elem) => elem.geometry(scale),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.geometry(scale),
            WorkspaceRenderElement::Window(elem) => elem.geometry(scale),
            WorkspaceRenderElement::Backdrop(elem) => elem.geometry(scale),
        }
    }

    fn location(&self, scale: Scale<f64>) -> Point<i32, smithay::utils::Physical> {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.location(scale),
            WorkspaceRenderElement::LowerLayerShell(elem) => elem.location(scale),
            WorkspaceRenderElement::Fullscreen(elem) => elem.location(scale),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.location(scale),
            WorkspaceRenderElement::Window(elem) => elem.location(scale),
            WorkspaceRenderElement::Backdrop(elem) => elem.location(scale),
        }
    }

    fn transform(&self) -> smithay::utils::Transform {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.transform(),
            WorkspaceRenderElement::LowerLayerShell(elem) => elem.transform(),
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
            WorkspaceRenderElement::LowerLayerShell(elem) => elem.damage_since(scale, commit),
            WorkspaceRenderElement::Fullscreen(elem) => elem.damage_since(scale, commit),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.damage_since(scale, commit),
            WorkspaceRenderElement::Window(elem) => elem.damage_since(scale, commit),
            WorkspaceRenderElement::Backdrop(elem) => elem.damage_since(scale, commit),
        }
    }

    fn opaque_regions(&self, scale: Scale<f64>) -> OpaqueRegions<i32, smithay::utils::Physical> {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.opaque_regions(scale),
            WorkspaceRenderElement::LowerLayerShell(elem) => elem.opaque_regions(scale),
            WorkspaceRenderElement::Fullscreen(elem) => elem.opaque_regions(scale),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.opaque_regions(scale),
            WorkspaceRenderElement::Window(elem) => elem.opaque_regions(scale),
            WorkspaceRenderElement::Backdrop(elem) => elem.opaque_regions(scale),
        }
    }

    fn alpha(&self) -> f32 {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.alpha(),
            WorkspaceRenderElement::LowerLayerShell(elem) => elem.alpha(),
            WorkspaceRenderElement::Fullscreen(elem) => elem.alpha(),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.alpha(),
            WorkspaceRenderElement::Window(elem) => elem.alpha(),
            WorkspaceRenderElement::Backdrop(elem) => elem.alpha(),
        }
    }

    fn kind(&self) -> Kind {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.kind(),
            WorkspaceRenderElement::LowerLayerShell(elem) => elem.kind(),
            WorkspaceRenderElement::Fullscreen(elem) => elem.kind(),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.kind(),
            WorkspaceRenderElement::Window(elem) => elem.kind(),
            WorkspaceRenderElement::Backdrop(elem) => elem.kind(),
        }
    }

    fn is_framebuffer_effect(&self) -> bool {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.is_framebuffer_effect(),
            WorkspaceRenderElement::LowerLayerShell(elem) => elem.is_framebuffer_effect(),
            WorkspaceRenderElement::Fullscreen(elem) => elem.is_framebuffer_effect(),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.is_framebuffer_effect(),
            WorkspaceRenderElement::Window(elem) => elem.is_framebuffer_effect(),
            WorkspaceRenderElement::Backdrop(elem) => elem.is_framebuffer_effect(),
        }
    }
}

impl<R> RenderElement<R> for WorkspaceRenderElement<R>
where
    R: AsGlowRenderer,
    R::TextureId: Send + 'static,
{
    fn draw(
        &self,
        frame: &mut R::Frame<'_, '_>,
        src: Rectangle<f64, BufferCoords>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, smithay::utils::Physical>],
        opaque_regions: &[Rectangle<i32, Physical>],
        cache: Option<&UserDataMap>,
    ) -> Result<(), R::Error> {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => {
                elem.draw(frame, src, dst, damage, opaque_regions, cache)
            }
            WorkspaceRenderElement::LowerLayerShell(elem) => {
                elem.draw(frame, src, dst, damage, opaque_regions, cache)
            }
            WorkspaceRenderElement::Fullscreen(elem) => {
                elem.draw(frame, src, dst, damage, opaque_regions, cache)
            }
            WorkspaceRenderElement::FullscreenPopup(elem) => {
                elem.draw(frame, src, dst, damage, opaque_regions, cache)
            }
            WorkspaceRenderElement::Window(elem) => {
                elem.draw(frame, src, dst, damage, opaque_regions, cache)
            }
            WorkspaceRenderElement::Backdrop(elem) => RenderElement::<GlowRenderer>::draw(
                elem,
                R::glow_frame_mut(frame),
                src,
                dst,
                damage,
                opaque_regions,
                cache,
            )
            .map_err(R::from_gles_error),
        }
    }

    fn underlying_storage(
        &self,
        renderer: &mut R,
    ) -> Option<smithay::backend::renderer::element::UnderlyingStorage<'_>> {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => elem.underlying_storage(renderer),
            WorkspaceRenderElement::LowerLayerShell(elem) => elem.underlying_storage(renderer),
            WorkspaceRenderElement::Fullscreen(elem) => elem.underlying_storage(renderer),
            WorkspaceRenderElement::FullscreenPopup(elem) => elem.underlying_storage(renderer),
            WorkspaceRenderElement::Window(elem) => elem.underlying_storage(renderer),
            WorkspaceRenderElement::Backdrop(elem) => {
                elem.underlying_storage(renderer.glow_renderer_mut())
            }
        }
    }

    fn capture_framebuffer(
        &self,
        frame: &mut R::Frame<'_, '_>,
        src: Rectangle<f64, BufferCoords>,
        dst: Rectangle<i32, Physical>,
        cache: &UserDataMap,
    ) -> Result<(), R::Error> {
        match self {
            WorkspaceRenderElement::OverrideRedirect(elem) => {
                elem.capture_framebuffer(frame, src, dst, cache)
            }
            WorkspaceRenderElement::LowerLayerShell(elem) => {
                elem.capture_framebuffer(frame, src, dst, cache)
            }
            WorkspaceRenderElement::Fullscreen(elem) => {
                elem.capture_framebuffer(frame, src, dst, cache)
            }
            WorkspaceRenderElement::FullscreenPopup(elem) => {
                elem.capture_framebuffer(frame, src, dst, cache)
            }
            WorkspaceRenderElement::Window(elem) => {
                elem.capture_framebuffer(frame, src, dst, cache)
            }
            WorkspaceRenderElement::Backdrop(elem) => {
                RenderElement::<GlowRenderer>::capture_framebuffer(
                    elem,
                    R::glow_frame_mut(frame),
                    src,
                    dst,
                    cache,
                )
                .map_err(R::from_gles_error)
            }
        }
    }
}

impl<R> From<RescaleRenderElement<CosmicWindowRenderElement<R>>> for WorkspaceRenderElement<R>
where
    R: AsGlowRenderer,
    R::TextureId: Send + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: RescaleRenderElement<CosmicWindowRenderElement<R>>) -> Self {
        WorkspaceRenderElement::Fullscreen(elem)
    }
}

impl<R> From<CosmicWindowRenderElement<R>> for WorkspaceRenderElement<R>
where
    R: AsGlowRenderer,
    R::TextureId: Send + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: CosmicWindowRenderElement<R>) -> Self {
        WorkspaceRenderElement::FullscreenPopup(elem)
    }
}

impl<R> From<SurfaceRenderElement<R>> for WorkspaceRenderElement<R>
where
    R: AsGlowRenderer,
    R::TextureId: Send + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: SurfaceRenderElement<R>) -> Self {
        WorkspaceRenderElement::OverrideRedirect(elem)
    }
}

impl<R> From<NamespacedElement<SurfaceRenderElement<R>>> for WorkspaceRenderElement<R>
where
    R: AsGlowRenderer,
    R::TextureId: Send + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: NamespacedElement<SurfaceRenderElement<R>>) -> Self {
        WorkspaceRenderElement::LowerLayerShell(elem)
    }
}

impl<R> From<CosmicMappedRenderElement<R>> for WorkspaceRenderElement<R>
where
    R: AsGlowRenderer,
    R::TextureId: Send + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: CosmicMappedRenderElement<R>) -> Self {
        WorkspaceRenderElement::Window(elem)
    }
}

impl<R> From<TextureRenderElement<GlesTexture>> for WorkspaceRenderElement<R>
where
    R: AsGlowRenderer,
    R::TextureId: Send + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: TextureRenderElement<GlesTexture>) -> Self {
        WorkspaceRenderElement::Backdrop(elem)
    }
}
