// SPDX-License-Identifier: GPL-3.0-only

use std::{
    collections::{HashMap, HashSet, VecDeque},
    sync::{
        Mutex,
        atomic::{AtomicBool, Ordering},
    },
    time::{Duration, Instant},
};

use cosmic_comp_config::AppearanceConfig;
use cosmic_settings_config::shortcuts::action::ResizeDirection;
use keyframe::{ease, functions::EaseInOutCubic};
use smithay::{
    backend::{
        allocator::Fourcc,
        drm::DrmNode,
        renderer::{
            Bind, Color32F, ImportAll, ImportMem, Offscreen, Renderer,
            element::{
                AsRenderElements, Kind, RenderElement,
                texture::{TextureRenderBuffer, TextureRenderElement},
                utils::{Relocate, RelocateRenderElement, RescaleRenderElement},
            },
            gles::{GlesError, GlesTexture},
            glow::GlowRenderer,
        },
    },
    desktop::{PopupKind, Space, WindowSurfaceType, layer_map_for_output, space::SpaceElement},
    input::Seat,
    output::Output,
    reexports::wayland_server::Resource,
    utils::{IsAlive, Logical, Physical, Point, Rectangle, Scale, Size, Transform},
    wayland::seat::WaylandFocus,
};

use crate::{
    backend::render::{
        BLUR_BORDER_STRENGTH, BLUR_FALLBACK_ALPHA, BLUR_FALLBACK_COLOR, BLUR_TINT_COLOR,
        BLUR_TINT_STRENGTH, BackdropShader, BlurredBackdropShader, ElementFilter, IndicatorShader,
        Key, Usage,
        element::AsGlowRenderer,
        get_cached_blur_texture_for_window,
        voice_orb::{VoiceOrbShader, VoiceOrbState},
    },
    shell::{
        CosmicSurface, Direction, ManagedLayer, MoveResult, ResizeMode,
        element::{
            CosmicMapped, CosmicMappedKey, CosmicMappedRenderElement, CosmicWindow, MaximizedState,
            resize_indicator::ResizeIndicator,
            stack::{CosmicStackRenderElement, MoveResult as StackMoveResult},
            window::CosmicWindowRenderElement,
        },
        focus::{
            FocusStackMut,
            target::{KeyboardFocusTarget, PointerFocusTarget},
        },
        grabs::{GrabStartData, ReleaseMode, ResizeEdge},
    },
    state::State,
    utils::{prelude::*, tween::EaseRectangle},
    wayland::handlers::xdg_shell::popup::get_popup_toplevel,
    wayland::protocols::backdrop_color::get_surface_backdrop_color,
    wayland::protocols::blur::{get_blur_border, get_blur_saturation, get_blur_tint},
};

mod grabs;
pub use self::grabs::*;

#[derive(Debug, Default)]
pub struct FloatingLayout {
    pub(crate) space: Space<CosmicMapped>,
    last_output_size: Size<i32, Local>,
    spawn_order: Vec<CosmicMapped>,
    animations: HashMap<CosmicMapped, Animation>,
    hovered_stack: Option<(CosmicMapped, Rectangle<i32, Local>)>,
    dirty: AtomicBool,
    pub theme: crate::comp_theme::CompTheme,
    pub appearance: AppearanceConfig,
    /// Original positions of windows before being pushed by exclusive zone changes
    /// (e.g., panel slide animations). Stored as (original_rect, last_computed_rect).
    /// When zone shrinks back, windows return to their original position.
    pre_slide_positions: HashMap<CosmicMapped, (Rectangle<i32, Local>, Rectangle<i32, Local>)>,
    /// When true, a layer slide animation is active. Windows skip `configure()`
    /// calls and get scaled in the render path instead, for smooth animation.
    pub slide_active: bool,
    /// Fraction of the running slide still to go (1.0 at transition start,
    /// 0.0 settled), eased. Drives the content crossfade so it tracks the
    /// motion exactly, including reversals. Set by the shell every tick.
    pub slide_fade: f32,
    /// Target geometries for windows during slide animation. Stored during recalculate()
    /// so the render path knows what size to scale to (since space.element_geometry()
    /// returns the buffer's committed size, not the animated target).
    slide_target_geometries: HashMap<CosmicMapped, Rectangle<i32, Local>>,
    /// Snapshots of pre-slide window content, captured in the render path
    /// (hence the interior mutability). Windows are configured to their final
    /// size the moment a slide starts; when the client's reflowed buffer
    /// arrives (the content swap), the snapshot is crossfaded out over it —
    /// without this the content would swap in a single frame, a visible blink
    /// on e.g. maximized editors. Until the swap, the snapshot stays hidden:
    /// the live (old) buffer is still on screen and shows identical pixels.
    slide_snapshots: Mutex<HashMap<CosmicMapped, SlideSnapshot>>,
    /// Windows whose old content must be snapshotted on the very next render
    /// frame, mapped to the buffer size they had when the final-size configure
    /// was sent. Armed by the slide-start layout pass: at that moment the
    /// animated target still equals the committed buffer, so the render path's
    /// size-mismatch check alone would capture too late (possibly after the
    /// new buffer landed — the recorded size lets us detect that race).
    pending_slide_snapshots: Mutex<HashMap<CosmicMapped, Size<i32, Local>>>,
    /// Windows whose final-size configure was DEFERRED at slide start: the
    /// geometry is already set to final, but the `configure()` is withheld until
    /// the old-content snapshot has been captured. Without this, a fast client
    /// (e.g. vscode under rapid toggling) can commit its reflowed buffer before
    /// the render frame snapshots the old content, so the slide-start content
    /// swap has no snapshot to crossfade and blinks. Holding the configure
    /// guarantees the old buffer is still committed when the capture runs.
    /// Flushed (via `force_configure`) once the window leaves
    /// `pending_slide_snapshots` (captured) — see `flush_deferred_slide_configures`.
    deferred_slide_configures: HashSet<CosmicMapped>,
}

/// Crossfade state for one window resized by a layer slide.
#[derive(Debug)]
struct SlideSnapshot {
    /// The window's content as it looked before the resize, captured at the
    /// committed buffer's size.
    texture: TextureRenderBuffer<GlesTexture>,
    /// Full extent of the captured texture in the element's logical space.
    /// Passed as the explicit `src` so the snapshot is scaled into the target
    /// rect — without it the element would crop instead of scale.
    src_size: Size<f64, Logical>,
    /// The committed buffer size at capture time. A change of the live buffer
    /// away from this size IS the content swap — it starts the crossfade,
    /// whether it happens mid-slide or long after.
    captured_size: Size<i32, Local>,
    /// Set at the content swap; the crossfade runs from here.
    fade_start: Option<Instant>,
}

#[derive(Debug)]
enum Animation {
    Tiled {
        start: Instant,
        previous_geometry: Rectangle<i32, Local>,
    },
    /// Client-driven pipelined resize animation.
    /// Sends configures along the animation curve, pipelined ahead by 1-2 frames.
    /// The client renders at intermediate sizes; the compositor positions the buffer
    /// at the time-interpolated location with minimal scaling.
    ClientPipelinedResize {
        start: Instant,
        previous_geometry: Rectangle<i32, Local>,
        target_geometry: Rectangle<i32, Local>,
        last_configure_time: Instant,
        /// `Some(true)` = maximize, `Some(false)` = unmaximize, `None` = arbitrary resize.
        is_maximize: Option<bool>,
        /// Set to true once the animation duration has elapsed and final state
        /// (maximized/tiled/geometry) has been applied. The animation is kept
        /// alive until the client's buffer matches the target size.
        finalized: bool,
    },
    Minimize {
        start: Instant,
        previous_geometry: Rectangle<i32, Local>,
        target_geometry: Rectangle<i32, Local>,
    },
    Unminimize {
        start: Instant,
        previous_geometry: Rectangle<i32, Local>,
        target_geometry: Rectangle<i32, Local>,
    },
    /// Fade-in animation for newly mapped maximized windows.
    /// Window starts at 0% opacity and fades to 100% over the animation duration.
    MapFadeIn {
        start: Instant,
        geometry: Rectangle<i32, Local>,
    },
}

impl Animation {
    fn start(&self) -> &Instant {
        match self {
            Animation::Tiled { start, .. } => start,
            Animation::ClientPipelinedResize { start, .. } => start,
            Animation::Minimize { start, .. } => start,
            Animation::Unminimize { start, .. } => start,
            Animation::MapFadeIn { start, .. } => start,
        }
    }

    fn alpha(&self, motion: crate::backend::render::animations::motion::Motion) -> f32 {
        match self {
            Animation::Tiled { .. } | Animation::ClientPipelinedResize { .. } => 1.0,
            Animation::Minimize { start, .. } => {
                let percentage = Instant::now()
                    .duration_since(*start)
                    .min(motion.minimize)
                    .as_secs_f32()
                    / motion.minimize.as_secs_f32();
                1.0 - ((percentage - 0.5).max(0.0) * 2.0)
            }
            Animation::Unminimize { start, .. } => {
                let percentage = Instant::now()
                    .duration_since(*start)
                    .min(motion.minimize)
                    .as_secs_f32()
                    / motion.minimize.as_secs_f32();
                (percentage * 2.0).min(1.0)
            }
            Animation::MapFadeIn { start, .. } => {
                let percentage = Instant::now()
                    .duration_since(*start)
                    .min(motion.animation)
                    .as_secs_f32()
                    / motion.animation.as_secs_f32();
                ease(EaseInOutCubic, 0.0, 1.0, percentage)
            }
        }
    }

    fn previous_geometry(&self) -> &Rectangle<i32, Local> {
        match self {
            Animation::Tiled {
                previous_geometry, ..
            } => previous_geometry,
            Animation::ClientPipelinedResize {
                previous_geometry, ..
            } => previous_geometry,
            Animation::Minimize {
                previous_geometry, ..
            } => previous_geometry,
            Animation::Unminimize {
                previous_geometry, ..
            } => previous_geometry,
            Animation::MapFadeIn { geometry, .. } => geometry,
        }
    }

    fn geometry(
        &self,
        output_geometry: Rectangle<i32, Logical>,
        current_geometry: Rectangle<i32, Local>,
        tiled_state: Option<&TiledCorners>,
        gaps: (i32, i32),
        motion: crate::backend::render::animations::motion::Motion,
    ) -> Rectangle<i32, Local> {
        let (duration, target_rect) = match self {
            Animation::Minimize {
                target_geometry, ..
            }
            | Animation::Unminimize {
                target_geometry, ..
            } => (motion.minimize, *target_geometry),
            Animation::MapFadeIn { geometry, .. } => {
                // MapFadeIn doesn't change geometry, just alpha.
                // Return the target geometry immediately.
                return *geometry;
            }
            Animation::Tiled { .. } => {
                let target_geometry = if let Some(target_rect) =
                    tiled_state.map(|state| state.relative_geometry(output_geometry, gaps))
                {
                    target_rect
                } else {
                    current_geometry
                };
                (motion.animation, target_geometry)
            }
            Animation::ClientPipelinedResize {
                target_geometry, ..
            } => (motion.animation, *target_geometry),
        };
        let previous_rect = *self.previous_geometry();
        let start = *self.start();
        let now = Instant::now();
        let progress =
            now.duration_since(start).min(duration).as_secs_f64() / duration.as_secs_f64();

        ease(
            EaseInOutCubic,
            EaseRectangle(previous_rect),
            EaseRectangle(target_rect),
            progress,
        )
        .unwrap()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TiledCorners {
    Top,
    TopRight,
    Right,
    BottomRight,
    Bottom,
    BottomLeft,
    Left,
    TopLeft,
}

impl TiledCorners {
    pub fn relative_geometry(
        &self,
        output_geometry: Rectangle<i32, Logical>,
        gaps: (i32, i32),
    ) -> Rectangle<i32, Local> {
        let (_, inner) = gaps;
        let (loc, size) = match self {
            TiledCorners::Bottom => (
                Point::from((
                    output_geometry.loc.x + inner,
                    output_geometry.loc.y + (output_geometry.size.h / 2) + inner / 2,
                )),
                Size::from((
                    output_geometry.size.w - inner * 2,
                    output_geometry.size.h / 2 - inner * 3 / 2,
                )),
            ),
            TiledCorners::BottomLeft => (
                Point::from((
                    output_geometry.loc.x + inner,
                    output_geometry.loc.y + (output_geometry.size.h / 2) + inner / 2,
                )),
                Size::from((
                    output_geometry.size.w / 2 - inner * 3 / 2,
                    output_geometry.size.h / 2 - inner * 3 / 2,
                )),
            ),
            TiledCorners::BottomRight => (
                Point::from((
                    output_geometry.loc.x + (output_geometry.size.w / 2) + inner / 2,
                    output_geometry.loc.y + (output_geometry.size.h / 2) + inner / 2,
                )),
                Size::from((
                    output_geometry.size.w / 2 - inner * 3 / 2,
                    output_geometry.size.h / 2 - inner * 3 / 2,
                )),
            ),
            TiledCorners::Left => (
                Point::from((output_geometry.loc.x + inner, output_geometry.loc.y + inner)),
                Size::from((
                    output_geometry.size.w / 2 - inner * 3 / 2,
                    output_geometry.size.h - inner * 2,
                )),
            ),
            TiledCorners::Top => (
                Point::from((output_geometry.loc.x + inner, output_geometry.loc.y + inner)),
                Size::from((
                    output_geometry.size.w - inner * 2,
                    output_geometry.size.h / 2 - inner * 3 / 2,
                )),
            ),
            TiledCorners::TopLeft => (
                Point::from((output_geometry.loc.x + inner, output_geometry.loc.y + inner)),
                Size::from((
                    output_geometry.size.w / 2 - inner * 3 / 2,
                    output_geometry.size.h / 2 - inner * 3 / 2,
                )),
            ),
            TiledCorners::TopRight => (
                Point::from((
                    output_geometry.loc.x + (output_geometry.size.w / 2) + inner / 2,
                    output_geometry.loc.y + inner,
                )),
                Size::from((
                    output_geometry.size.w / 2 - inner * 3 / 2,
                    output_geometry.size.h / 2 - inner * 3 / 2,
                )),
            ),
            TiledCorners::Right => (
                Point::from((
                    output_geometry.loc.x + (output_geometry.size.w / 2) + inner / 2,
                    output_geometry.loc.y + inner,
                )),
                Size::from((
                    output_geometry.size.w / 2 - inner * 3 / 2,
                    output_geometry.size.h - inner * 2,
                )),
            ),
        };

        Rectangle::new(loc, size).as_local()
    }
}

/// A group of consecutive blur windows that can share a single background capture.
/// Windows are grouped when there are no non-blur windows between them.
#[derive(Clone)]
pub struct BlurWindowGroup {
    /// The z-index threshold for capturing (lowest z-index in the group)
    pub capture_z_threshold: usize,
    /// Windows in this group: (key, geometry, alpha, z_index)
    pub windows: Vec<(CosmicMappedKey, Rectangle<i32, Local>, f32, usize)>,
    /// Whether every window here paints its backdrop over its full geometry with
    /// no rounded corners and no transparency of its own.
    ///
    /// Necessary but not sufficient for the window to occlude what is below it --
    /// the captured scene must also have been opaque, which is only known after
    /// the capture and is tracked separately on the cached texture.
    pub paints_full_geometry: bool,
}

impl FloatingLayout {
    pub fn new(
        theme: crate::comp_theme::CompTheme,
        appearance: AppearanceConfig,
        output: &Output,
    ) -> FloatingLayout {
        let mut layout = Self {
            theme,
            last_output_size: output.geometry().size.as_local(),
            appearance,
            ..Default::default()
        };
        layout.space.map_output(output, (0, 0));
        layout
    }

    pub fn set_output(&mut self, output: &Output) {
        let old_output = self.space.outputs().next().unwrap().clone();
        self.space.unmap_output(&old_output);
        self.space.map_output(output, (0, 0));

        let old_output_geometry = {
            let layers = layer_map_for_output(&old_output);
            layers.non_exclusive_zone()
        }
        .to_f64();
        let output_geometry = {
            let layers = layer_map_for_output(output);
            layers.non_exclusive_zone()
        };

        for mapped in self
            .space
            .elements()
            .filter(|w| w.alive())
            .cloned()
            .collect::<Vec<_>>()
            .into_iter()
        {
            let tiled_state = *mapped.floating_tiled.lock().unwrap();
            if let Some(tiled_state) = tiled_state {
                let geometry = tiled_state.relative_geometry(output_geometry, self.gaps());
                self.map_internal(
                    mapped,
                    Some(geometry.loc),
                    Some(geometry.size.as_logical()),
                    None,
                );
            } else {
                let Some(geometry) = self.space.element_geometry(&mapped) else {
                    continue;
                };
                let geometry = geometry.to_f64();
                let new_loc = (
                    ((geometry.loc.x - old_output_geometry.loc.x).max(0.)
                        / old_output_geometry.size.w
                        * output_geometry.size.w as f64)
                        .round() as i32
                        + output_geometry.loc.x,
                    ((geometry.loc.y - old_output_geometry.loc.y).max(0.)
                        / old_output_geometry.size.h
                        * output_geometry.size.h as f64)
                        .round() as i32
                        + output_geometry.loc.y,
                );
                self.map_internal(mapped, Some(Point::from(new_loc)), None, None);
            }
        }

        self.last_output_size = output.geometry().size.as_local();
        self.recalculate();
    }

    pub fn map(
        &mut self,
        mapped: impl Into<CosmicMapped>,
        position: impl Into<Option<Point<i32, Local>>>,
    ) {
        let mapped = mapped.into();
        let position = position.into();

        self.map_internal(mapped, position, None, None)
    }

    // ─────────────────────────────────────────────────────────────────────────────
    // Embedded child animation helpers
    // ─────────────────────────────────────────────────────────────────────────────

    /// Configure all embedded children to a given parent size.
    /// If `record_for_animation` is true, records the configure for animation sync.
    fn configure_embeds_to_parent_size(
        &self,
        parent_surface_id: Option<&String>,
        parent_width: i32,
        parent_height: i32,
        record_for_animation: bool,
    ) {
        let Some(sid) = parent_surface_id else { return };

        let embeds =
            crate::wayland::handlers::surface_embed::update_embedded_geometry_for_parent_by_surface_id(
                sid,
                parent_width,
                parent_height,
            );

        for (embedded_surface_id, new_geometry) in embeds {
            if let Some(embedded_elem) = self.space.elements().find(|e| {
                e.active_window()
                    .wl_surface()
                    .map(|s| s.id().to_string() == embedded_surface_id)
                    .unwrap_or(false)
            }) {
                let global_geo = Rectangle::new(
                    (new_geometry.loc.x, new_geometry.loc.y).into(),
                    (new_geometry.size.w, new_geometry.size.h).into(),
                );
                embedded_elem.active_window().set_geometry(global_geo, 0);
                embedded_elem.configure();

                if record_for_animation {
                    crate::wayland::handlers::surface_embed::record_embed_configure(
                        &embedded_surface_id,
                        new_geometry.size,
                    );
                }

                tracing::debug!(
                    embedded_surface_id,
                    size = ?new_geometry.size,
                    record_for_animation,
                    "Configured embedded window"
                );
            }
        }
    }

    /// Render embedded children of a parent window.
    /// Returns the render elements for all embedded children positioned relative to the parent.
    fn render_embedded_children<R>(
        &self,
        renderer: &mut R,
        parent_elem: &CosmicMapped,
        parent_geometry: Rectangle<i32, Local>,
        output_scale: f64,
        alpha: f32,
    ) -> Vec<CosmicMappedRenderElement<R>>
    where
        R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        CosmicWindowRenderElement<R>: RenderElement<R>,
        CosmicStackRenderElement<R>: RenderElement<R>,
    {
        let mut embedded_elements = Vec::new();

        let parent_window = parent_elem.active_window();
        let Some(parent_surface) = parent_window.wl_surface() else {
            return embedded_elements;
        };

        let parent_surface_id = parent_surface.id().to_string();
        let embedded_children =
            crate::wayland::handlers::surface_embed::get_children_for_parent_by_surface_id(
                &parent_surface_id,
            );

        for (embedded_surface_id, embed_info) in embedded_children {
            // Find the embedded element in the space
            let embedded_elem = self.space.elements().find(|e| {
                e.active_window()
                    .wl_surface()
                    .map(|s| s.id().to_string() == embedded_surface_id)
                    .unwrap_or(false)
            });

            let Some(embedded_elem) = embedded_elem else {
                let output_name = self
                    .space
                    .outputs()
                    .next()
                    .map(|o| o.name())
                    .unwrap_or_else(|| "unknown".to_string());
                tracing::warn!(
                    parent_app_id = %parent_elem.active_window().app_id(),
                    embedded_surface_id = %embedded_surface_id,
                    output = %output_name,
                    "Embedded child not found in this space - may be on different output"
                );
                continue;
            };

            // Calculate actual embed geometry based on parent's (possibly animated) size
            let actual_geometry = if let Some(ref anchor_config) = embed_info.anchor_config {
                anchor_config.calculate_geometry(parent_geometry.size.w, parent_geometry.size.h)
            } else {
                embed_info.geometry
            };

            // Render the embedded window at parent position + embed offset
            let embed_offset = smithay::utils::Point::<i32, smithay::utils::Logical>::from((
                actual_geometry.loc.x,
                actual_geometry.loc.y,
            ));
            let render_location = parent_geometry.loc.as_logical() + embed_offset;

            // Use actual_geometry.size as clip size to handle resize transitions
            let clip_size = Some(smithay::utils::Size::<i32, smithay::utils::Logical>::from(
                (actual_geometry.size.w, actual_geometry.size.h),
            ));

            tracing::debug!(
                embedded_app_id = %embedded_elem.active_window().app_id(),
                parent_app_id = %parent_elem.active_window().app_id(),
                parent_loc = ?parent_geometry.loc,
                parent_size = ?parent_geometry.size,
                embed_offset = ?embed_offset,
                render_location = ?render_location,
                clip_size = ?clip_size,
                "Rendering embedded window in front of parent"
            );

            let elements = embedded_elem.render_elements(
                renderer,
                render_location.to_physical_precise_round(output_scale),
                clip_size,
                output_scale.into(),
                alpha,
                None,
                None,
            );
            embedded_elements.extend(elements);
        }

        embedded_elements
    }

    /// Render popups for an embedded window at its correct visual position (inside parent)
    fn render_embedded_popups<R>(
        &self,
        renderer: &mut R,
        embedded_elem: &CosmicMapped,
        output_scale: f64,
        alpha: f32,
        scanout_node: Option<DrmNode>,
    ) -> Vec<CosmicMappedRenderElement<R>>
    where
        R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        CosmicWindowRenderElement<R>: RenderElement<R>,
        CosmicStackRenderElement<R>: RenderElement<R>,
    {
        let mut popup_elements = Vec::new();

        // Get the embedded window's surface ID
        let embedded_window = embedded_elem.active_window();
        let Some(embedded_surface) = embedded_window.wl_surface() else {
            return popup_elements;
        };
        let embedded_surface_id = embedded_surface.id().to_string();

        // Get the embed info to find the parent and embed geometry
        let Some(embed_info) = crate::wayland::handlers::surface_embed::get_embed_render_info_by_id(
            &embedded_surface_id,
        ) else {
            return popup_elements;
        };

        // Find the parent element
        let parent_elem = self.space.elements().find(|e| {
            e.active_window()
                .wl_surface()
                .map(|s| s.id().to_string() == embed_info.parent_surface_id)
                .unwrap_or(false)
        });

        let Some(parent_elem) = parent_elem else {
            return popup_elements;
        };

        // Get parent geometry (possibly animated)
        let parent_geometry = self
            .animations
            .get(parent_elem)
            .map(|anim| *anim.previous_geometry())
            .unwrap_or_else(|| self.space.element_geometry(parent_elem).unwrap().as_local());

        // Calculate actual embed geometry based on parent's size
        let actual_geometry = if let Some(ref anchor_config) = embed_info.anchor_config {
            anchor_config.calculate_geometry(parent_geometry.size.w, parent_geometry.size.h)
        } else {
            embed_info.geometry
        };

        // Calculate where the embedded window is visually rendered
        let embed_offset = smithay::utils::Point::<i32, smithay::utils::Logical>::from((
            actual_geometry.loc.x,
            actual_geometry.loc.y,
        ));
        let render_location = parent_geometry.loc.as_logical() + embed_offset;

        // popup_render_elements expects: target_render_loc - elem.geometry().loc
        let popup_render_offset = render_location - embedded_elem.geometry().loc;
        popup_elements.extend(embedded_elem.popup_render_elements(
            renderer,
            popup_render_offset.to_physical_precise_round(output_scale),
            output_scale.into(),
            alpha,
            scanout_node,
        ));

        popup_elements
    }

    pub fn map_maximized(
        &mut self,
        mapped: CosmicMapped,
        previous_geometry: Rectangle<i32, Local>,
        animate: bool,
    ) {
        self.map_maximized_internal(mapped, previous_geometry, animate, false)
    }

    /// Map a window as maximized with fade-in animation only (no geometry transition).
    /// Used for windows that start maximized - they appear at full size and fade in.
    pub fn map_maximized_fade_in(
        &mut self,
        mapped: CosmicMapped,
        previous_geometry: Rectangle<i32, Local>,
    ) {
        self.map_maximized_internal(mapped, previous_geometry, true, true)
    }

    fn map_maximized_internal(
        &mut self,
        mapped: CosmicMapped,
        previous_geometry: Rectangle<i32, Local>,
        animate: bool,
        fade_in_only: bool,
    ) {
        let output = self.space.outputs().next().unwrap().clone();
        let layers = layer_map_for_output(&output);
        let target_geometry = layers.non_exclusive_zone().as_local();

        mapped.set_bounds(target_geometry.size.as_logical());
        mapped.set_tiled(true);
        mapped.set_maximized(true);
        mapped.set_fills_output_zone(false);

        let parent_surface_id = mapped
            .active_window()
            .wl_surface()
            .map(|s| s.id().to_string());

        // Configure embedded children to final target size
        self.configure_embeds_to_parent_size(
            parent_surface_id.as_ref(),
            target_geometry.size.w,
            target_geometry.size.h,
            false,
        );

        mapped.set_geometry(target_geometry.to_global(&output));
        mapped.configure();

        if animate {
            if fade_in_only {
                // Pure fade-in: window starts at full size, just animate alpha
                self.animations.insert(
                    mapped.clone(),
                    Animation::MapFadeIn {
                        start: Instant::now(),
                        geometry: target_geometry,
                    },
                );
            } else {
                self.update_or_insert_tiled_animation(&mapped, previous_geometry, target_geometry);
            }
        } else {
            self.animations.remove(&mapped);
        }

        self.finalize_maximize_map(mapped, target_geometry);
    }

    /// Start a pipelined maximize animation.
    /// Sends configures along the animation curve so the client re-renders at
    /// intermediate sizes. set_maximized/set_tiled are deferred until completion.
    pub fn start_pipelined_maximize(
        &mut self,
        mapped: CosmicMapped,
        original_geometry: Rectangle<i32, Local>,
    ) {
        let output = self.space.outputs().next().unwrap().clone();
        let layers = layer_map_for_output(&output);
        let target_geometry = layers.non_exclusive_zone().as_local();

        mapped.set_bounds(target_geometry.size.as_logical());

        // Send first configure 1 frame ahead on the curve
        let frame_interval = Duration::from_millis(16);
        let first_progress =
            frame_interval.as_secs_f64() / self.theme.motion.animation.as_secs_f64();
        let first_geo: Rectangle<i32, Local> = ease(
            EaseInOutCubic,
            EaseRectangle(original_geometry),
            EaseRectangle(target_geometry),
            first_progress,
        )
        .unwrap();
        mapped.set_geometry(first_geo.to_global(&output));
        mapped.configure();

        tracing::debug!(
            app_id = %mapped.active_window().app_id(),
            prev = ?original_geometry,
            target = ?target_geometry,
            first_geo = ?first_geo,
            "[PIPELINE] Starting pipelined maximize"
        );

        let now = Instant::now();
        self.animations.insert(
            mapped.clone(),
            Animation::ClientPipelinedResize {
                start: now,
                previous_geometry: original_geometry,
                target_geometry,
                last_configure_time: now,
                is_maximize: Some(true),
                finalized: false,
            },
        );

        self.finalize_maximize_map(mapped, target_geometry);
    }

    /// Start a pipelined resize animation for an arbitrary geometry change.
    /// Like maximize/unmaximize but without changing any window state flags.
    pub fn start_pipelined_resize(
        &mut self,
        mapped: CosmicMapped,
        target_geometry: Rectangle<i32, Local>,
    ) {
        let current_geo = self
            .space
            .element_geometry(&mapped)
            .map(RectExt::as_local)
            .unwrap_or(target_geometry);

        let output = self.space.outputs().next().unwrap().clone();

        // Send first configure 1 frame ahead on the curve
        let frame_interval = Duration::from_millis(16);
        let first_progress =
            frame_interval.as_secs_f64() / self.theme.motion.animation.as_secs_f64();
        let first_geo: Rectangle<i32, Local> = ease(
            EaseInOutCubic,
            EaseRectangle(current_geo),
            EaseRectangle(target_geometry),
            first_progress,
        )
        .unwrap();
        mapped.set_geometry(first_geo.to_global(&output));
        mapped.configure();

        tracing::debug!(
            app_id = %mapped.active_window().app_id(),
            prev = ?current_geo,
            target = ?target_geometry,
            first_geo = ?first_geo,
            "[PIPELINE] Starting pipelined resize"
        );

        let now = Instant::now();
        self.animations.insert(
            mapped.clone(),
            Animation::ClientPipelinedResize {
                start: now,
                previous_geometry: current_geo,
                target_geometry,
                last_configure_time: now,
                is_maximize: None,
                finalized: false,
            },
        );
    }

    /// Start a pipelined unmaximize animation.
    /// Sends configures along the animation curve so the client re-renders at
    /// intermediate sizes. set_maximized/set_tiled are deferred until completion.
    pub fn start_pipelined_unmaximize(
        &mut self,
        mapped: CosmicMapped,
        target_geometry: Rectangle<i32, Local>,
    ) {
        // Use the intended maximize zone as the starting geometry rather than
        // element_geometry from the space, which reflects the committed buffer
        // size and may be stale if the client hasn't resized to the zone yet.
        let current_geo = if let Some(output) = self.space.outputs().next() {
            let layers = layer_map_for_output(output);
            layers.non_exclusive_zone().as_local()
        } else {
            self.space
                .element_geometry(&mapped)
                .map(RectExt::as_local)
                .unwrap_or(target_geometry)
        };

        let output = self.space.outputs().next().unwrap().clone();

        // Send first configure 1 frame ahead on the curve
        let frame_interval = Duration::from_millis(16);
        let first_progress =
            frame_interval.as_secs_f64() / self.theme.motion.animation.as_secs_f64();
        let first_geo: Rectangle<i32, Local> = ease(
            EaseInOutCubic,
            EaseRectangle(current_geo),
            EaseRectangle(target_geometry),
            first_progress,
        )
        .unwrap();
        mapped.set_geometry(first_geo.to_global(&output));
        mapped.configure();

        tracing::debug!(
            app_id = %mapped.active_window().app_id(),
            prev = ?current_geo,
            target = ?target_geometry,
            first_geo = ?first_geo,
            "[PIPELINE] Starting pipelined unmaximize"
        );

        // Clear any lingering slide target geometry — the window is now under
        // client-driven animation and should not be overridden by stale slide data.
        self.slide_target_geometries.remove(&mapped);
        self.slide_snapshots.lock().unwrap().remove(&mapped);
        self.pending_slide_snapshots.lock().unwrap().remove(&mapped);
        // Drop any withheld slide-start configure too: the pipelined resize now
        // drives this window's configures, so a deferred slide configure would
        // be a stale competing send.
        self.deferred_slide_configures.remove(&mapped);

        let now = Instant::now();
        self.animations.insert(
            mapped.clone(),
            Animation::ClientPipelinedResize {
                start: now,
                previous_geometry: current_geo,
                target_geometry,
                last_configure_time: now,
                is_maximize: Some(false),
                finalized: false,
            },
        );
    }

    /// Apply the state a pipelined resize would have applied when it completed.
    ///
    /// `ClientPipelinedResize` defers `set_maximized`/`set_tiled` to the frame the
    /// animation finalizes. Dropping the animation before then strands the toplevel's
    /// maximized flag out of sync with `maximized_state` — and since `maximize_toggle`
    /// dispatches on the flag while both request paths act on `maximized_state`, a
    /// desync wedges the window: maximize and unmaximize then both no-op forever.
    fn settle_pipelined_state(mapped: &CosmicMapped, anim: &Animation) {
        if let Animation::ClientPipelinedResize {
            is_maximize: Some(maximize),
            finalized: false,
            ..
        } = anim
        {
            mapped.set_maximized(*maximize);
            mapped.set_tiled(*maximize);
        }
    }

    /// Update an existing animation's target or insert a new animation.
    /// If geometries are the same (window mapped directly to final size), use fade-in.
    /// Otherwise use a Tiled animation for geometry transition.
    fn update_or_insert_tiled_animation(
        &mut self,
        mapped: &CosmicMapped,
        previous_geometry: Rectangle<i32, Local>,
        target_geometry: Rectangle<i32, Local>,
    ) {
        if let Some(existing_anim) = self.animations.get_mut(mapped) {
            match existing_anim {
                Animation::Unminimize {
                    target_geometry: tg,
                    ..
                } => {
                    *tg = target_geometry;
                }
                Animation::Minimize { .. }
                | Animation::Tiled { .. }
                | Animation::MapFadeIn { .. }
                | Animation::ClientPipelinedResize { .. } => {}
            }
        } else {
            // If geometries are the same, use fade-in animation instead of tiled
            // This happens when a window is mapped directly to its maximized size
            let animation = if previous_geometry == target_geometry {
                Animation::MapFadeIn {
                    start: Instant::now(),
                    geometry: target_geometry,
                }
            } else {
                Animation::Tiled {
                    start: Instant::now(),
                    previous_geometry,
                }
            };
            self.animations.insert(mapped.clone(), animation);
        }
    }

    /// Finalize mapping a maximized window (common cleanup for both animation modes)
    fn finalize_maximize_map(
        &mut self,
        mapped: CosmicMapped,
        target_geometry: Rectangle<i32, Local>,
    ) {
        if let Some(pos) = self.spawn_order.iter().position(|m| m == &mapped) {
            self.spawn_order.truncate(pos);
        }

        mapped.moved_since_mapped.store(true, Ordering::SeqCst);

        // If this window was snapped to a corner before being maximized, record both the
        // pre-snap geometry and the snapped corner so unmaximize can restore the snap.
        let snapped = mapped.floating_tiled.lock().unwrap().take();
        if let Some(snapped) = snapped
            && let Some(state) = mapped.maximized_state.lock().unwrap().as_mut()
        {
            if let Some(real_old_geo) = *mapped.last_geometry.lock().unwrap() {
                state.original_geometry = real_old_geo;
            }
            if state.original_snapped.is_none() {
                state.original_snapped = Some(snapped);
            }
        };
        self.space
            .map_element(mapped, target_geometry.loc.as_logical(), true);
        self.space.refresh();
    }

    pub(crate) fn map_internal(
        &mut self,
        mapped: CosmicMapped,
        position: Option<Point<i32, Local>>,
        size: Option<Size<i32, Logical>>,
        prev: Option<Rectangle<i32, Local>>,
    ) {
        let already_mapped = self.space.element_geometry(&mapped).map(RectExt::as_local);
        // (Re)mapping means this window is an active layout participant again, so it
        // is no longer "closing" — clear any stale flag left by an ignored close.
        mapped.closing.store(false, Ordering::SeqCst);
        let mut win_geo = mapped.geometry().as_local();

        let output = self.space.outputs().next().unwrap().clone();
        let layers = layer_map_for_output(&output);
        let output_geometry = layers.non_exclusive_zone();
        mapped.set_bounds(output_geometry.size);
        let last_geometry = *mapped.last_geometry.lock().unwrap();
        let min_size = mapped.min_size().unwrap_or((320, 240).into());

        if let Some(size) = size
            .map(SizeExt::as_local)
            .or(last_geometry.map(|g| g.size))
        {
            win_geo.size = size;
        } else {
            let max_size = mapped.max_size().unwrap_or(
                (
                    min_size.w.max(output_geometry.size.w / 3 * 2),
                    min_size.h.max(output_geometry.size.h / 3 * 2),
                )
                    .into(),
            );

            // if the current geometry is too large
            if win_geo.size.w > max_size.w {
                // try a more reasonable size
                let mut width = output_geometry.size.w / 3 * 2;
                if max_size.w != 0 {
                    // don't go larger then the max_size ...
                    width = std::cmp::min(max_size.w, width);
                }
                if min_size.w != 0 {
                    // ... but also don't go smaller than the min_size
                    width = std::cmp::max(min_size.w, width);
                }
                win_geo.size.w = width;
            }
            // but no matter the supported sizes, don't be larger than our non-exclusive-zone
            win_geo.size.w = std::cmp::min(win_geo.size.w, output_geometry.size.w);

            if win_geo.size.h > max_size.h {
                // try a more reasonable size
                let mut height = output_geometry.size.h / 3 * 2;
                if max_size.h != 0 {
                    // don't go larger then the max_size ...
                    height = std::cmp::min(max_size.h, height);
                }
                if min_size.h != 0 {
                    // ... but also don't go smaller than the min_size
                    height = std::cmp::max(min_size.h, height);
                }
                win_geo.size.h = height;
            }
            // but no matter the supported sizes, don't be larger than our non-exclusive-zone
            win_geo.size.h = std::cmp::min(win_geo.size.h, output_geometry.size.h);
        }

        let position = position
            .map(|pos| {
                // Clamp the position so the window stays within the non-exclusive zone.
                // This prevents windows from being dropped behind layer-shell panels.
                let geo = output_geometry.as_local();
                Point::from((pos.x.max(geo.loc.x), pos.y.max(geo.loc.y)))
            })
            .or_else(|| last_geometry.map(|g| g.loc))
            .unwrap_or_else(|| {
                // cleanup moved windows
                if let Some(pos) = self
                    .spawn_order
                    .iter()
                    .position(|w| !w.alive() || w.moved_since_mapped.load(Ordering::SeqCst))
                {
                    self.spawn_order.truncate(pos);
                }

                let three_fours_width = (output_geometry.size.w / 4 * 3).max(360);

                // figure out new position — anchor the cascade on the most recent
                // window that is genuinely still part of the layout. Skipping
                // closing windows is essential: closing a window is asynchronous, so
                // a window relaunched before the previous one finishes tearing down
                // would otherwise anchor on the dying window and get stacked 48px
                // below it instead of centered (windows drifting toward the bottom).
                let pos = self
                    .spawn_order
                    .iter()
                    .rev()
                    .find(|window| {
                        window.alive()
                            && !window.closing.load(Ordering::SeqCst)
                            && !window.moved_since_mapped.load(Ordering::SeqCst)
                    })
                    .and_then(|window| self.space.element_geometry(window))
                    .filter(|geo| {
                        geo.size.w < three_fours_width
                            && win_geo.size.w < three_fours_width
                            && output_geometry.contains_rect(*geo)
                    })
                    .map(|geometry| {
                        let mut geometry: Rectangle<u32, Logical> = Rectangle::new(
                            (geometry.loc.x as u32, geometry.loc.y as u32).into(),
                            (geometry.size.w as u32, geometry.size.h as u32).into(),
                        );

                        // move down
                        geometry.loc.y += 48;

                        // do we need to address the height?
                        let new_column = if geometry.loc.y + min_size.h as u32
                            <= (output_geometry.loc.y + output_geometry.size.h - 16) as u32
                        {
                            // alternate to the sides
                            let offset = if self
                                .spawn_order
                                .iter()
                                .flat_map(|w| self.space.element_geometry(w))
                                .filter(|geo| geo.size.w < three_fours_width)
                                .count()
                                % 2
                                == 0
                            {
                                (geometry.loc.x + geometry.size.w)
                                    .checked_sub(96 + (win_geo.size.w as u32))
                            } else {
                                (geometry.loc.x + geometry.size.w)
                                    .checked_sub((win_geo.size.w as u32).saturating_sub(48))
                            };

                            if let Some(offset) = offset {
                                geometry.loc.x = offset;
                                // do we need to resize?
                                if geometry.loc.y as i32 + win_geo.size.h
                                    > output_geometry.loc.y + output_geometry.size.h - 16
                                {
                                    win_geo.size.h =
                                        (output_geometry.loc.y + output_geometry.size.h - 16)
                                            - geometry.loc.y as i32;
                                }

                                false
                            } else {
                                true
                            }
                        } else {
                            true
                        };

                        if new_column {
                            let min_y = self
                                .spawn_order
                                .iter()
                                .flat_map(|w| {
                                    self.space
                                        .element_geometry(w)
                                        .filter(|geo| geo.size.w < three_fours_width)
                                        .map(|geo| geo.loc.y)
                                })
                                .min()
                                .unwrap() as u32;
                            geometry.loc.y = min_y.saturating_sub(16);

                            match geometry.loc.x.checked_sub(144) {
                                Some(new_x) => geometry.loc.x = new_x,
                                None => {
                                    // if we go out to the left, cycle around to the right
                                    geometry.loc.x =
                                        ((output_geometry.loc.x + output_geometry.size.w) as u32)
                                            .saturating_sub(geometry.size.w + 16)
                                }
                            };
                        }

                        // check padding again
                        if geometry.loc.x < (output_geometry.loc.x + 16) as u32 {
                            geometry.loc.x = (output_geometry.loc.x + 16) as u32;
                        }
                        if geometry.loc.y < (output_geometry.loc.y + 16) as u32 {
                            geometry.loc.y = (output_geometry.loc.y + 16) as u32;
                        }
                        // if the width would be too high, we wouldn't be here
                        if geometry.loc.y as i32 + win_geo.size.h
                            > (output_geometry.loc.y + output_geometry.size.h - 16)
                        {
                            win_geo.size.h = output_geometry.loc.y + output_geometry.size.h
                                - 16
                                - geometry.loc.y as i32;
                        }

                        Point::<i32, Logical>::from((geometry.loc.x as i32, geometry.loc.y as i32))
                    })
                    .unwrap_or_else(|| {
                        (
                            output_geometry.loc.x + output_geometry.size.w / 2 - win_geo.size.w / 2,
                            output_geometry.loc.y
                                + (output_geometry.size.h / 2 - win_geo.size.h / 2)
                                    .min(output_geometry.size.h / 8),
                        )
                            .into()
                    })
                    .as_local();

                mapped.moved_since_mapped.store(false, Ordering::SeqCst);
                self.spawn_order.push(mapped.clone());

                pos
            });

        mapped.set_tiled(false);
        // Mapping as an ordinary floating window must not leave a stale protocol
        // `maximized` flag behind; `maximized_state` is the source of truth.
        if mapped.maximized_state.lock().unwrap().is_none() {
            mapped.set_maximized(false);
        }
        let zone = output_geometry.as_local();

        // Keep newly-placed windows fully on-screen. The branches above clamp only the
        // top-left edge (`.max`), and the `last_geometry` restore is unclamped — so a
        // position/size carried over from a wider or differently-scaled output can push
        // the window past the right/bottom edge. Clamp against the non-exclusive zone.
        let clamped_position: Point<i32, Local> = Point::from((
            position.x.clamp(
                zone.loc.x,
                zone.loc.x + (zone.size.w - win_geo.size.w).max(0),
            ),
            position.y.clamp(
                zone.loc.y,
                zone.loc.y + (zone.size.h - win_geo.size.h).max(0),
            ),
        ));
        let position = clamped_position;

        mapped.set_fills_output_zone(
            position.x == zone.loc.x
                && position.y == zone.loc.y
                && win_geo.size.w >= zone.size.w
                && win_geo.size.h >= zone.size.h,
        );
        mapped.set_geometry(Rectangle::new(position, win_geo.size).to_global(&output));
        mapped.configure();

        let current_geometry = Rectangle::new(position, win_geo.size);
        if let Some(previous_geometry) = prev.or(already_mapped) {
            self.animations.insert(
                mapped.clone(),
                Animation::Tiled {
                    start: Instant::now(),
                    previous_geometry,
                },
            );
        } else {
            // Fade in newly mapped windows
            self.animations.insert(
                mapped.clone(),
                Animation::MapFadeIn {
                    start: Instant::now(),
                    geometry: current_geometry,
                },
            );
        }
        self.space.map_element(mapped, position.as_logical(), false);
        self.space.refresh();
    }

    pub fn remap_minimized(
        &mut self,
        mapped: CosmicMapped,
        from: Rectangle<i32, Local>,
        position: Point<i32, Local>,
    ) {
        if !mapped.alive() {
            return;
        }

        let output = self.space.outputs().next().unwrap().clone();
        let layers = layer_map_for_output(&output);
        let geometry = layers.non_exclusive_zone().as_local();
        mapped.set_bounds(geometry.size.as_logical());
        let window_size = mapped.geometry().size;

        if mapped.is_maximized(false) {
            mapped.set_geometry(geometry.to_global(&output));
            mapped.configure();
        } else {
            mapped.set_geometry(Rectangle::new(
                position.to_global(&output),
                window_size.as_global(),
            ));
        }

        self.space
            .map_element(mapped.clone(), position.as_logical(), true);
        self.space.refresh();
        let target_geometry = self.space.element_geometry(&mapped).unwrap().as_local();

        self.animations.insert(
            mapped,
            Animation::Unminimize {
                start: Instant::now(),
                previous_geometry: from,
                target_geometry,
            },
        );
    }

    pub fn unmap(
        &mut self,
        window: &CosmicMapped,
        to: Option<Rectangle<i32, Local>>,
    ) -> Option<Rectangle<i32, Local>> {
        let mut mapped_geometry = self.space.element_geometry(window).map(RectExt::as_local)?;
        // Settle before the `is_maximized` checks below — an unmaximize dropped here
        // would otherwise leave the flag set and skip the `last_geometry` save.
        if let Some(anim) = self.animations.remove(window) {
            Self::settle_pipelined_state(window, &anim);
        }

        if let Some(to) = to {
            self.animations.insert(
                window.clone(),
                Animation::Minimize {
                    start: Instant::now(),
                    previous_geometry: if window.is_maximized(false) {
                        let output = self.space.outputs().next().unwrap();
                        let layers = layer_map_for_output(output);
                        layers.non_exclusive_zone().as_local()
                    } else {
                        mapped_geometry
                    },
                    target_geometry: to,
                },
            );
        }

        if window.floating_tiled.lock().unwrap().take().is_some() {
            if let Some(last_size) = window.last_geometry.lock().unwrap().map(|geo| geo.size) {
                let geometry = Rectangle::new(mapped_geometry.loc, last_size);
                window.set_tiled(false);
                window.set_geometry(geometry.to_global(self.space.outputs().next().unwrap()));
                window.configure();
                mapped_geometry.size = last_size;
            }
        } else if !window.is_maximized(true) {
            if window.active_window().has_pending_changes()
                && let Some(pending_size) = window.pending_size()
            {
                mapped_geometry.size = pending_size.as_local();
            } else if let Some(server_size) = window.last_server_size() {
                mapped_geometry.size = server_size.as_local();
            }
            *window.last_geometry.lock().unwrap() = Some(mapped_geometry);
        }

        self.space.unmap_elem(window);
        if let Some(pos) = self.spawn_order.iter().position(|w| w == window) {
            self.spawn_order.truncate(pos);
        }
        window.moved_since_mapped.store(true, Ordering::SeqCst);
        Some(mapped_geometry)
    }

    pub fn drop_window(
        &mut self,
        window: CosmicMapped,
        position: Point<i32, Local>,
    ) -> (CosmicMapped, Point<i32, Local>) {
        if self
            .hovered_stack
            .as_ref()
            .is_some_and(|(stack, _)| stack == &window || !stack.alive())
        {
            let _ = self.hovered_stack.take();
        }

        if let Some((mapped, geo)) = self.hovered_stack.take() {
            let stack = mapped.stack_ref().unwrap();
            for surface in window.windows().map(|s| s.0) {
                stack.add_window(surface, None, None);
            }
            (mapped, geo.loc)
        } else {
            // Pass current geometry as `prev` so map_internal uses a position
            // animation instead of MapFadeIn. The window already existed — it
            // was just grabbed, not newly created.
            let geo = Rectangle::new(position, window.geometry().size.as_local());
            // Explicitly pass current size so map_internal uses it instead of
            // the stale last_geometry saved during unmap (which may be larger
            // if the window was resized during the grab).
            let size = Some(geo.size.as_logical());
            self.map_internal(window.clone(), Some(position), size, Some(geo));
            (window, position)
        }
    }

    pub fn element_geometry(&self, elem: &CosmicMapped) -> Option<Rectangle<i32, Local>> {
        // If the window is maximized, return the intended maximize zone geometry
        // rather than the committed buffer size, which may be stale if the client
        // hasn't yet committed at the full size (common with Electron apps).
        if elem.is_maximized(true)
            && let Some(output) = self.space.outputs().next()
        {
            let layers = layer_map_for_output(output);
            return Some(layers.non_exclusive_zone().as_local());
        }
        self.space.element_geometry(elem).map(RectExt::as_local)
    }

    // ─────────────────────────────────────────────────────────────────────────────
    // Embedded window hit-testing helpers
    // ─────────────────────────────────────────────────────────────────────────────

    /// Find an embedded element under the given location.
    /// Returns the element and its render location if found.
    /// Only returns an embedded element if its parent window is the topmost
    /// non-embedded window at that location (respects z-order).
    fn embedded_element_under(
        &self,
        location: Point<f64, Local>,
    ) -> Option<(&CosmicMapped, Point<i32, Logical>)> {
        // Find the topmost non-embedded window at this location for z-order checking
        let topmost_surface_id = self.topmost_parent_surface_id_at(location);

        // Now check embedded elements, but only if their parent is the topmost window
        for elem in self.space.elements() {
            if let Some(embed_info) = elem.windows().find_map(|(w, _)| {
                crate::wayland::handlers::surface_embed::get_embed_render_info(&w)
            }) {
                // Only consider this embedded window if its parent is the topmost window at this location
                if topmost_surface_id.as_ref() != Some(&embed_info.parent_surface_id) {
                    continue;
                }

                if let Some(render_location) =
                    self.calculate_embed_render_location(&embed_info, location)
                {
                    return Some((elem, render_location));
                }
            }
        }
        None
    }

    /// Calculate the render location of an embedded window if the given point is within its bounds.
    /// Returns None if the point is outside the embedded window.
    fn calculate_embed_render_location(
        &self,
        embed_info: &crate::wayland::handlers::surface_embed::EmbedRenderInfo,
        location: Point<f64, Local>,
    ) -> Option<Point<i32, Logical>> {
        let parent_geo = self
            .space
            .elements()
            .find(|e| {
                e.active_window()
                    .wl_surface()
                    .map(|s| s.id().to_string() == embed_info.parent_surface_id)
                    .unwrap_or(false)
            })
            .and_then(|parent| self.space.element_geometry(parent))?;

        let actual_geometry = embed_info
            .anchor_config
            .as_ref()
            .map(|anchor| anchor.calculate_geometry(parent_geo.size.w, parent_geo.size.h))
            .unwrap_or(embed_info.geometry);

        let render_location = parent_geo.loc + actual_geometry.loc;
        let embedded_bounds = Rectangle::new(render_location, actual_geometry.size);

        if embedded_bounds.to_f64().contains(location.as_logical()) {
            Some(render_location)
        } else {
            None
        }
    }

    /// Check if an element is embedded (has embed render info).
    fn is_embedded(elem: &CosmicMapped) -> bool {
        elem.windows().any(|(w, _)| {
            crate::wayland::handlers::surface_embed::get_embed_render_info(&w).is_some()
        })
    }

    /// Find the topmost non-embedded window at a given location.
    /// Returns the surface ID of that window if found.
    fn topmost_parent_surface_id_at(&self, location: Point<f64, Local>) -> Option<String> {
        self.space
            .elements()
            .rev()
            .filter(|e| !Self::is_embedded(e))
            .find(|e| {
                let render_location = self.space.element_location(e).unwrap() - e.geometry().loc;
                let mut bbox = e.bbox();
                bbox.loc += render_location;
                bbox.to_f64().contains(location.as_logical())
            })
            .and_then(|e| e.active_window().wl_surface().map(|s| s.id().to_string()))
    }

    pub fn popup_element_under(
        &self,
        location: Point<f64, Local>,
        seat: &Seat<State>,
    ) -> Option<KeyboardFocusTarget> {
        // First check popups for embedded windows - they render on top of their embedded position
        if let Some(target) = self.embedded_popup_element_under(location, seat) {
            return Some(target);
        }

        // Then check regular (non-embedded) windows' popups
        self.space
            .elements()
            .rev()
            .filter(|e| !Self::is_embedded(e))
            .map(|e| {
                (
                    e,
                    self.space.element_location(e).unwrap() - e.geometry().loc,
                )
            })
            .filter(|(e, render_location)| {
                let mut bbox = e.bbox();
                bbox.loc += *render_location;
                bbox.to_f64().contains(location.as_logical())
            })
            .find_map(|(e, render_location)| {
                let render_location = render_location.as_local().to_f64();
                let point = location - render_location;
                if e.focus_under(
                    point.as_logical(),
                    WindowSurfaceType::POPUP | WindowSurfaceType::SUBSURFACE,
                    seat,
                )
                .is_some()
                {
                    Some(e.clone().into())
                } else {
                    None
                }
            })
    }

    /// Check if the location hits a popup for any embedded window.
    /// Embedded windows render at a different location (inside their parent),
    /// so we need to check popups at the adjusted position.
    /// Only returns a popup if its parent window is the topmost non-embedded window
    /// at that location (respects z-order).
    fn embedded_popup_element_under(
        &self,
        location: Point<f64, Local>,
        seat: &Seat<State>,
    ) -> Option<KeyboardFocusTarget> {
        // Find the topmost non-embedded window at this location for z-order checking
        let topmost_surface_id = self.topmost_parent_surface_id_at(location);

        // Iterate through all parent windows that have embedded children
        for parent_elem in self.space.elements() {
            let parent_window = parent_elem.active_window();
            let parent_surface = parent_window.wl_surface()?;
            let parent_surface_id = parent_surface.id().to_string();

            // Only consider this parent if it's the topmost window at this location
            if topmost_surface_id.as_ref() != Some(&parent_surface_id) {
                continue;
            }

            let embedded_children =
                crate::wayland::handlers::surface_embed::get_children_for_parent_by_surface_id(
                    &parent_surface_id,
                );

            if embedded_children.is_empty() {
                continue;
            }

            // Get parent's geometry (in Logical coordinates from Space)
            let parent_geometry = self.space.element_geometry(parent_elem)?.as_local();

            for (embedded_surface_id, embed_info) in embedded_children {
                // Find the embedded element in the space
                let embedded_elem = self.space.elements().find(|e| {
                    e.active_window()
                        .wl_surface()
                        .map(|s| s.id().to_string() == embedded_surface_id)
                        .unwrap_or(false)
                })?;

                // Calculate where the embedded window is actually rendered
                // Both parent_geometry.loc and embed_offset are now Local
                let embed_offset = Point::<i32, Local>::from((
                    embed_info.geometry.loc.x,
                    embed_info.geometry.loc.y,
                ));
                let render_location = parent_geometry.loc + embed_offset;

                // Check if the location hits any popup of this embedded window
                let point = location - render_location.to_f64();

                if embedded_elem
                    .focus_under(
                        point.as_logical(),
                        WindowSurfaceType::POPUP | WindowSurfaceType::SUBSURFACE,
                        seat,
                    )
                    .is_some()
                {
                    return Some(embedded_elem.clone().into());
                }
            }
        }

        None
    }

    pub fn toplevel_element_under(
        &self,
        location: Point<f64, Local>,
        seat: &Seat<State>,
    ) -> Option<KeyboardFocusTarget> {
        // First check embedded windows - they render on top and should get keyboard focus priority
        if let Some((elem, render_location)) = self.embedded_element_under(location) {
            // Subtract geometry offset to get the coordinate origin for focus_under,
            // same as we do for non-embedded windows
            let adjusted_render_location = render_location - elem.geometry().loc;
            let render_location_local = adjusted_render_location.as_local().to_f64();
            let point = location - render_location_local;
            if elem
                .focus_under(
                    point.as_logical(),
                    WindowSurfaceType::TOPLEVEL | WindowSurfaceType::SUBSURFACE,
                    seat,
                )
                .is_some()
            {
                return Some(elem.clone().into());
            }
        }

        // Then check regular (non-embedded) windows
        self.space
            .elements()
            .rev()
            .filter(|e| !Self::is_embedded(e))
            .map(|e| {
                (
                    e,
                    self.space.element_location(e).unwrap() - e.geometry().loc,
                )
            })
            .filter(|(e, render_location)| {
                let mut bbox = e.bbox();
                bbox.loc += *render_location;
                bbox.to_f64().contains(location.as_logical())
            })
            .find_map(|(e, render_location)| {
                let render_location = render_location.as_local().to_f64();
                let point = location - render_location;
                if e.focus_under(
                    point.as_logical(),
                    WindowSurfaceType::TOPLEVEL | WindowSurfaceType::SUBSURFACE,
                    seat,
                )
                .is_some()
                {
                    Some(e.clone().into())
                } else {
                    None
                }
            })
    }

    pub fn popup_surface_under(
        &self,
        location: Point<f64, Local>,
        seat: &Seat<State>,
    ) -> Option<(PointerFocusTarget, Point<f64, Local>)> {
        // First check popups for embedded windows
        if let Some(result) = self.embedded_popup_surface_under(location, seat) {
            return Some(result);
        }

        // Then check regular (non-embedded) windows' popups
        self.space
            .elements()
            .rev()
            .filter(|e| !Self::is_embedded(e))
            .map(|e| {
                (
                    e,
                    self.space.element_location(e).unwrap() - e.geometry().loc,
                )
            })
            .filter(|(e, render_location)| {
                let mut bbox = e.bbox();
                bbox.loc += *render_location;
                bbox.to_f64().contains(location.as_logical())
            })
            .find_map(|(e, render_location)| {
                let render_location = render_location.as_local().to_f64();
                let point = location - render_location;
                e.focus_under(
                    point.as_logical(),
                    WindowSurfaceType::POPUP | WindowSurfaceType::SUBSURFACE,
                    seat,
                )
                .map(|(surface, surface_offset)| {
                    (surface, render_location + surface_offset.as_local())
                })
            })
    }

    /// Check if the location hits a popup surface for any embedded window.
    /// Returns the pointer focus target and the location relative to the window.
    /// Only returns a popup if its parent window is the topmost non-embedded window
    /// at that location (respects z-order).
    fn embedded_popup_surface_under(
        &self,
        location: Point<f64, Local>,
        seat: &Seat<State>,
    ) -> Option<(PointerFocusTarget, Point<f64, Local>)> {
        // Find the topmost non-embedded window at this location for z-order checking
        let topmost_surface_id = self.topmost_parent_surface_id_at(location);

        // Iterate through all parent windows that have embedded children
        for parent_elem in self.space.elements() {
            let parent_window = parent_elem.active_window();
            let parent_surface = parent_window.wl_surface()?;
            let parent_surface_id = parent_surface.id().to_string();

            // Only consider this parent if it's the topmost window at this location
            if topmost_surface_id.as_ref() != Some(&parent_surface_id) {
                continue;
            }

            let embedded_children =
                crate::wayland::handlers::surface_embed::get_children_for_parent_by_surface_id(
                    &parent_surface_id,
                );

            if embedded_children.is_empty() {
                continue;
            }

            // Get parent's geometry
            let parent_geometry = self.space.element_geometry(parent_elem)?.as_local();

            for (embedded_surface_id, embed_info) in embedded_children {
                // Find the embedded element in the space
                let embedded_elem = self.space.elements().find(|e| {
                    e.active_window()
                        .wl_surface()
                        .map(|s| s.id().to_string() == embedded_surface_id)
                        .unwrap_or(false)
                })?;

                // Calculate where the embedded window is actually rendered
                let embed_offset = Point::<i32, Local>::from((
                    embed_info.geometry.loc.x,
                    embed_info.geometry.loc.y,
                ));
                let render_location = parent_geometry.loc + embed_offset;
                // Subtract geometry offset to get the coordinate origin for focus_under,
                // same as we do for non-embedded windows
                let adjusted_render_location =
                    render_location - embedded_elem.geometry().loc.as_local();
                let render_location_f64 = adjusted_render_location.to_f64();

                // Check if the location hits any popup of this embedded window
                let point = location - render_location_f64;

                if let Some((surface, surface_offset)) = embedded_elem.focus_under(
                    point.as_logical(),
                    WindowSurfaceType::POPUP | WindowSurfaceType::SUBSURFACE,
                    seat,
                ) {
                    return Some((surface, render_location_f64 + surface_offset.as_local()));
                }
            }
        }

        None
    }

    pub fn toplevel_surface_under(
        &self,
        location: Point<f64, Local>,
        seat: &Seat<State>,
    ) -> Option<(PointerFocusTarget, Point<f64, Local>)> {
        // First check embedded windows - they render on top and should get priority
        if let Some((elem, render_location)) = self.embedded_element_under(location) {
            // Subtract geometry offset to get the coordinate origin for focus_under,
            // same as we do for non-embedded windows
            let adjusted_render_location = render_location - elem.geometry().loc;
            let render_location_local = adjusted_render_location.as_local().to_f64();
            let point = location - render_location_local;
            if let Some((surface, surface_offset)) = elem.focus_under(
                point.as_logical(),
                WindowSurfaceType::TOPLEVEL | WindowSurfaceType::SUBSURFACE,
                seat,
            ) {
                return Some((surface, render_location_local + surface_offset.as_local()));
            }
        }

        // Then check regular (non-embedded) windows
        self.space
            .elements()
            .rev()
            .filter(|e| !Self::is_embedded(e))
            .map(|e| {
                (
                    e,
                    self.space.element_location(e).unwrap() - e.geometry().loc,
                )
            })
            .filter(|(e, render_location)| {
                let mut bbox = e.bbox();
                bbox.loc += *render_location;
                bbox.to_f64().contains(location.as_logical())
            })
            .find_map(|(e, render_location)| {
                let render_location = render_location.as_local().to_f64();
                let point = location - render_location;
                e.focus_under(
                    point.as_logical(),
                    WindowSurfaceType::TOPLEVEL | WindowSurfaceType::SUBSURFACE,
                    seat,
                )
                .map(|(surface, surface_offset)| {
                    (surface, render_location + surface_offset.as_local())
                })
            })
    }

    pub fn update_pointer_position(&mut self, location: Option<Point<f64, Local>>) {
        let Some(location) = location else {
            self.hovered_stack.take();
            return;
        };

        let res = self
            .space
            .element_under(location.as_logical())
            .map(|(mapped, p)| (mapped.clone(), p.as_local()));

        if let Some((mapped, _)) = res.as_ref() {
            let geometry = self.space.element_geometry(mapped).unwrap();
            let offset = location.y.round() as i32 - geometry.loc.y;
            if mapped.is_stack()
                && offset.is_positive()
                && offset <= mapped.ssd_height(false).unwrap_or(0)
            {
                self.hovered_stack = Some((mapped.clone(), geometry.as_local()));
            } else {
                self.hovered_stack.take();
            }
        } else {
            self.hovered_stack.take();
        }
    }

    pub fn stacking_indicator(&self) -> Option<Rectangle<i32, Local>> {
        self.hovered_stack.as_ref().map(|(_, geo)| *geo)
    }

    pub fn resize_request(
        &mut self,
        mapped: &CosmicMapped,
        seat: &Seat<State>,
        start_data: GrabStartData,
        edges: ResizeEdge,
        edge_snap_threshold: u32,
        release: ReleaseMode,
    ) -> Option<ResizeSurfaceGrab> {
        if seat.get_pointer().is_some() {
            let location = self.space.element_location(mapped)?.as_local();
            let size = mapped.geometry().size;
            mapped.moved_since_mapped.store(true, Ordering::SeqCst);

            Some(grabs::ResizeSurfaceGrab::new(
                start_data,
                mapped.clone(),
                edges,
                self.space.outputs().next().cloned().unwrap(),
                edge_snap_threshold,
                location,
                size,
                seat,
                release,
            ))
        } else {
            None
        }
    }

    pub fn resize(
        &mut self,
        focused: &KeyboardFocusTarget,
        direction: ResizeDirection,
        edge: ResizeEdge,
        amount: i32,
    ) -> bool {
        let Some(toplevel) = focused.toplevel() else {
            return false;
        };
        let Some(mapped) = self
            .space
            .elements()
            .find(|m| m.has_surface(&toplevel, WindowSurfaceType::TOPLEVEL))
        else {
            return false;
        };
        if mapped.is_maximized(true) {
            return false;
        }

        let Some(original_geo) = self.space.element_geometry(mapped) else {
            return false; // we don't have that window
        };
        let mut geo = original_geo;

        if edge.contains(ResizeEdge::RIGHT) || edge.contains(ResizeEdge::LEFT) {
            if direction == ResizeDirection::Inwards {
                geo.size.w = (geo.size.w as u32).saturating_sub(amount as u32) as i32;
            } else {
                geo.size.w += amount;
            }
            if edge.contains(ResizeEdge::LEFT) {
                if direction == ResizeDirection::Inwards {
                    geo.loc.x += amount;
                } else {
                    geo.loc.x = (geo.loc.x as u32).saturating_sub(amount as u32) as i32;
                }
            }
        }
        if edge.contains(ResizeEdge::BOTTOM) || edge.contains(ResizeEdge::TOP) {
            if direction == ResizeDirection::Inwards {
                geo.size.h = (geo.size.h as u32).saturating_sub(amount as u32) as i32;
            } else {
                geo.size.h += amount;
            }
            if edge.contains(ResizeEdge::TOP) {
                if direction == ResizeDirection::Inwards {
                    geo.loc.y += amount;
                } else {
                    geo.loc.y = (geo.loc.y as u32).saturating_sub(amount as u32) as i32;
                }
            }
        }

        let bounding_box = self
            .space
            .output_geometry(self.space.outputs().next().unwrap())
            .unwrap();
        let (min_size, max_size) = (mapped.min_size(), mapped.max_size());
        let min_width = min_size.map(|s| s.w).unwrap_or(360);
        let min_height = min_size.map(|s| s.h).unwrap_or(240);
        let max_width = max_size.map(|s| s.w).unwrap_or(i32::MAX);
        let max_height = max_size.map(|s| s.h).unwrap_or(i32::MAX);

        geo.size.w = min_width.max(geo.size.w).min(max_width);
        geo.size.h = min_height.max(geo.size.h).min(max_height);
        geo = geo.intersection(bounding_box).unwrap();

        *mapped.resize_state.lock().unwrap() = Some(ResizeState::Resizing(ResizeData {
            edges: edge,
            initial_window_location: original_geo.loc.as_local(),
            initial_window_size: original_geo.size,
        }));

        mapped.moved_since_mapped.store(true, Ordering::SeqCst);
        mapped.set_resizing(true);
        mapped.set_geometry(
            geo.as_local()
                .to_global(self.space.outputs().next().unwrap()),
        );
        if mapped.latest_size_committed() {
            mapped.configure();
        }

        true
    }

    pub fn toggle_stacking(
        &mut self,
        mapped: &CosmicMapped,
        mut focus_stack: FocusStackMut,
    ) -> Option<KeyboardFocusTarget> {
        if !self.space.elements().any(|m| m == mapped) {
            return None;
        }

        let output = self.space.outputs().next().unwrap().clone();
        let mut mapped = mapped.clone();
        let geo = self.space.element_geometry(&mapped).unwrap();
        let location = geo.loc;

        if mapped.is_window() {
            // if it is just a window
            self.space.unmap_elem(&mapped);
            mapped.convert_to_stack(
                (&output, mapped.bbox()),
                self.theme.clone(),
                self.appearance,
            );
            self.map_internal(
                mapped.clone(),
                Some(location.as_local()),
                Some(geo.size),
                None,
            );
            focus_stack.append(mapped.clone());
            Some(KeyboardFocusTarget::Element(mapped))
        } else {
            // if we have a stack
            let mut surfaces = mapped.windows().map(|(s, _)| s).collect::<VecDeque<_>>();
            let first = surfaces.pop_front().expect("Stack without a window?");
            let focused = mapped.active_window();

            self.space.unmap_elem(&mapped);
            let handle = mapped.loop_handle();
            mapped.convert_to_surface(
                first,
                (&output, mapped.bbox()),
                self.theme.clone(),
                self.appearance,
            );
            let mut new_elements = vec![mapped.clone()];

            // map the rest
            for other in surfaces {
                other.try_force_undecorated(false);
                other.set_tiled(false);
                let focused = other == focused;
                let window = CosmicMapped::from(CosmicWindow::new(
                    other,
                    handle.clone(),
                    self.theme.clone(),
                    self.appearance,
                ));
                window.output_enter(&output, window.bbox());

                {
                    let layer_map = layer_map_for_output(&output);
                    window.set_bounds(layer_map.non_exclusive_zone().size);
                }

                if focused {
                    new_elements.insert(0, window.clone());
                } else {
                    new_elements.push(window.clone());
                }
                self.map(window, None);
            }
            self.space.map_element(mapped.clone(), location, false);
            self.space.refresh();

            for elem in new_elements.into_iter().rev() {
                focus_stack.append(elem);
            }

            Some(KeyboardFocusTarget::Element(mapped))
        }
    }

    pub fn toggle_stacking_focused(
        &mut self,
        seat: &Seat<State>,
        focus_stack: FocusStackMut,
    ) -> Option<KeyboardFocusTarget> {
        let Some(KeyboardFocusTarget::Element(elem)) = seat.get_keyboard().unwrap().current_focus()
        else {
            return None;
        };

        self.toggle_stacking(&elem, focus_stack)
    }

    pub fn move_element(
        &mut self,
        direction: Direction,
        seat: &Seat<State>,
        layer: ManagedLayer,
        theme: &crate::comp_theme::CompTheme,
        element: &CosmicMapped,
    ) -> MoveResult {
        match element.handle_move(direction) {
            StackMoveResult::Handled => MoveResult::Done,
            StackMoveResult::MoveOut(surface, loop_handle) => {
                let mapped: CosmicMapped =
                    CosmicWindow::new(surface, loop_handle, theme.clone(), self.appearance).into();
                let output = seat.active_output();
                let pos = self.space.element_geometry(element).unwrap().loc
                    + match direction {
                        Direction::Up => Point::from((5, -10)),
                        Direction::Down => Point::from((5, 10)),
                        Direction::Left => Point::from((-10, 5)),
                        Direction::Right => Point::from((10, 5)),
                    };
                let position = self
                    .space
                    .output_geometry(&output)
                    .unwrap()
                    .overlaps({
                        let mut geo = mapped.geometry();
                        geo.loc += pos;
                        geo
                    })
                    .then_some(pos);

                self.map_internal(mapped.clone(), position.map(PointExt::as_local), None, None);
                MoveResult::ShiftFocus(KeyboardFocusTarget::Element(mapped))
            }
            StackMoveResult::Default => {
                let mut tiled_state = element.floating_tiled.lock().unwrap();

                let output = self.space.outputs().next().unwrap().clone();
                let layers = layer_map_for_output(&output);
                let output_geometry = layers.non_exclusive_zone();
                std::mem::drop(layers);

                let current_geometry = self
                    .space
                    .element_geometry(element)
                    .map(RectExt::as_local)
                    .unwrap();
                let start_rectangle = if let Some(anim) = self.animations.remove(element) {
                    Self::settle_pipelined_state(element, &anim);
                    anim.geometry(
                        output_geometry,
                        current_geometry,
                        tiled_state.as_ref(),
                        self.gaps(),
                        self.theme.motion,
                    )
                } else {
                    current_geometry
                };

                let new_state = match (direction, &*tiled_state) {
                    // figure out if we are moving between workspaces/outputs
                    (
                        Direction::Up,
                        Some(TiledCorners::Top)
                        | Some(TiledCorners::TopLeft)
                        | Some(TiledCorners::TopRight),
                    )
                    | (
                        Direction::Down,
                        Some(TiledCorners::Bottom)
                        | Some(TiledCorners::BottomLeft)
                        | Some(TiledCorners::BottomRight),
                    )
                    | (
                        Direction::Left,
                        Some(TiledCorners::Left)
                        | Some(TiledCorners::TopLeft)
                        | Some(TiledCorners::BottomLeft),
                    )
                    | (
                        Direction::Right,
                        Some(TiledCorners::Right)
                        | Some(TiledCorners::TopRight)
                        | Some(TiledCorners::BottomRight),
                    ) => {
                        return MoveResult::MoveFurther(KeyboardFocusTarget::Element(
                            element.clone(),
                        ));
                    }

                    // to we go maximized?
                    (Direction::Up, Some(TiledCorners::Bottom))
                    | (Direction::Down, Some(TiledCorners::Top))
                    | (Direction::Left, Some(TiledCorners::Right))
                    | (Direction::Right, Some(TiledCorners::Left)) => {
                        std::mem::drop(tiled_state);

                        let mut maximized_state = element.maximized_state.lock().unwrap();
                        *maximized_state = Some(MaximizedState {
                            original_geometry: start_rectangle,
                            original_layer: layer,
                            original_snapped: None,
                        });
                        std::mem::drop(maximized_state);

                        self.map_maximized(element.clone(), start_rectangle, true);
                        return MoveResult::Done;
                    }

                    // figure out if we need to quater tile
                    (Direction::Up, Some(TiledCorners::Left))
                    | (Direction::Left, Some(TiledCorners::Top)) => TiledCorners::TopLeft,
                    (Direction::Right, Some(TiledCorners::Top))
                    | (Direction::Up, Some(TiledCorners::Right)) => TiledCorners::TopRight,
                    (Direction::Down, Some(TiledCorners::Left))
                    | (Direction::Left, Some(TiledCorners::Bottom)) => TiledCorners::BottomLeft,
                    (Direction::Right, Some(TiledCorners::Bottom))
                    | (Direction::Down, Some(TiledCorners::Right)) => TiledCorners::BottomRight,
                    // figure out if we need to extend a quater tile
                    (Direction::Up, Some(TiledCorners::BottomLeft))
                    | (Direction::Down, Some(TiledCorners::TopLeft)) => TiledCorners::Left,
                    (Direction::Up, Some(TiledCorners::BottomRight))
                    | (Direction::Down, Some(TiledCorners::TopRight)) => TiledCorners::Right,
                    (Direction::Left, Some(TiledCorners::TopRight))
                    | (Direction::Right, Some(TiledCorners::TopLeft)) => TiledCorners::Top,
                    (Direction::Left, Some(TiledCorners::BottomRight))
                    | (Direction::Right, Some(TiledCorners::BottomLeft)) => TiledCorners::Bottom,
                    // else we have a simple case
                    (Direction::Up, _) => TiledCorners::Top,
                    (Direction::Right, _) => TiledCorners::Right,
                    (Direction::Down, _) => TiledCorners::Bottom,
                    (Direction::Left, _) => TiledCorners::Left,
                };

                let new_geo = new_state.relative_geometry(output_geometry, self.gaps());
                let (new_pos, new_size) = (new_geo.loc, new_geo.size);
                element.set_tiled(true); // TODO: More fine grained?
                element.set_maximized(false);
                element.set_fills_output_zone(false);

                if tiled_state.is_none() {
                    let last_geometry = element
                        .maximized_state
                        .lock()
                        .unwrap()
                        .take()
                        .map(|state| state.original_geometry)
                        .or_else(|| self.space.element_geometry(element).map(RectExt::as_local));

                    *element.last_geometry.lock().unwrap() = last_geometry;
                }

                *tiled_state = Some(new_state);
                std::mem::drop(tiled_state);

                element.moved_since_mapped.store(true, Ordering::SeqCst);
                let element = element.clone();
                self.map_internal(
                    element,
                    Some(new_pos),
                    Some(new_size.as_logical()),
                    Some(start_rectangle),
                );

                MoveResult::Done
            }
        }
    }

    pub fn move_current_element(
        &mut self,
        direction: Direction,
        seat: &Seat<State>,
        layer: ManagedLayer,
        theme: crate::comp_theme::CompTheme,
    ) -> MoveResult {
        let Some(target) = seat.get_keyboard().unwrap().current_focus() else {
            return MoveResult::None;
        };

        let Some(focused) = (match target {
            KeyboardFocusTarget::Popup(popup) => {
                let Some(toplevel_surface) = (match popup {
                    PopupKind::Xdg(_) => get_popup_toplevel(&popup),
                    PopupKind::InputMethod(_) => unreachable!(),
                }) else {
                    return MoveResult::None;
                };
                self.space
                    .elements()
                    .find(|elem| elem.wl_surface().as_deref() == Some(&toplevel_surface))
            }
            KeyboardFocusTarget::Element(elem) => self.space.elements().find(|x| *x == &elem),
            _ => None,
        }) else {
            return MoveResult::None;
        };

        self.move_element(direction, seat, layer, &theme, &focused.clone())
    }

    pub fn mapped(&self) -> impl Iterator<Item = &CosmicMapped> {
        self.space.elements().rev()
    }

    pub fn windows(&self) -> impl Iterator<Item = CosmicSurface> + '_ {
        self.mapped().flat_map(|e| e.windows().map(|(w, _)| w))
    }

    pub fn recalculate(&mut self) {
        let _ = self.recalculate_collect_resized(false);
    }

    /// Like [`Self::recalculate`], but returns the windows that were sent a
    /// configure with a changed size, with `(old buffer size, new size)`.
    /// Used by the slide-start layout pass to know which windows need an
    /// old-content snapshot for the crossfade.
    pub fn recalculate_collect_resized(
        &mut self,
        defer_configures: bool,
    ) -> Vec<(CosmicMapped, Size<i32, Local>, Size<i32, Local>)> {
        let mut resized = Vec::new();
        let output = self.space.outputs().next().unwrap().clone();
        let output_size = output.geometry().size.as_local();
        let old_output_size = Some(self.last_output_size).filter(|size| *size != output_size);

        // The layer map already reflects the animated exclusive zone during
        // slides (cached-state overrides + arrange happen before recalculate).
        let geometry = layer_map_for_output(&output)
            .non_exclusive_zone()
            .as_local();

        // update elements
        for mapped in self
            .space
            .elements()
            .cloned()
            .collect::<Vec<_>>()
            .into_iter()
        {
            mapped.set_bounds(geometry.size.as_logical());
            let prev = self.space.element_geometry(&mapped).map(RectExt::as_local);

            let window_geometry = if mapped.is_maximized(false) {
                self.pre_slide_positions.remove(&mapped);
                geometry
            } else {
                prev.map(|mut rect| {
                    if let Some(old_size) = old_output_size {
                        rect = Rectangle::new(
                            Point::new(
                                (rect.loc.x as f64 + rect.size.w as f64 / 2.) / old_size.w as f64
                                    * output_size.w as f64
                                    - rect.size.w as f64 / 2.,
                                (rect.loc.y as f64 + rect.size.h as f64 / 2.) / old_size.h as f64
                                    * output_size.h as f64
                                    - rect.size.h as f64 / 2.,
                            ),
                            rect.size.to_f64(),
                        )
                        .to_i32_round();
                    }

                    self.compute_slide_constrained_position(&mapped, rect, geometry)
                })
                .unwrap_or_else(|| {
                    Rectangle::new(Point::from((0, 0)), mapped.geometry().size.as_local())
                })
            };
            // Capture committed buffer size for the size-change check below.
            let buffer_size = mapped.geometry().size.as_local();
            let is_activated = mapped.is_activated(false);
            // During slide animations, skip configure AND set_geometry for ANY size
            // change. We must not touch the toplevel's pending state at all — any
            // pending != last_sent would leak a configure via space.refresh(). The
            // render path uses slide_target_geometries for visual sizing (squish
            // when shrinking, stretch when growing) and one configure goes out when
            // the slide ends. Sending per-frame configures instead (the zone moves
            // every frame) made heavy clients re-render at many intermediate sizes,
            // and each lagging buffer flipped the render scale between squish and
            // stretch — visible texture jitter on e.g. maximized editors.
            // Position-only changes still flow through normally below.
            let size_changed =
                window_geometry.size.w != buffer_size.w || window_geometry.size.h != buffer_size.h;
            if self.slide_active && size_changed {
                tracing::debug!(
                    app_id = %mapped.active_window().app_id(),
                    buf_w = buffer_size.w, buf_h = buffer_size.h,
                    tgt_w = window_geometry.size.w, tgt_h = window_geometry.size.h,
                    "[SLIDE_RECALC] deferring configure during slide"
                );
            } else {
                // Defer ONLY size-changing configures at slide start: those
                // trigger the client reflow we must snapshot before. Position-
                // only updates carry no race and configure immediately.
                let defer_this = defer_configures && size_changed;
                if size_changed {
                    tracing::debug!(
                        app_id = %mapped.active_window().app_id(),
                        buf_w = buffer_size.w, buf_h = buffer_size.h,
                        tgt_w = window_geometry.size.w, tgt_h = window_geometry.size.h,
                        slide_active = self.slide_active,
                        "[SLIDE_RECALC] sending configure (size mismatch)"
                    );
                    resized.push((mapped.clone(), buffer_size, window_geometry.size));
                }
                mapped.set_geometry(window_geometry.to_global(&output));
                mapped.set_fills_output_zone(
                    !mapped.is_maximized(false)
                        && window_geometry.loc.x == geometry.loc.x
                        && window_geometry.loc.y == geometry.loc.y
                        && window_geometry.size.w >= geometry.size.w
                        && window_geometry.size.h >= geometry.size.h,
                );
                if defer_this {
                    // Geometry is final, but withhold the configure until the
                    // old content is snapshotted (flushed once the window leaves
                    // pending_slide_snapshots). Prevents the slide-start race.
                    self.deferred_slide_configures.insert(mapped.clone());
                } else {
                    mapped.configure();
                }
            }
            // Store target geometry for the render path during slide.
            // Also update when slide just ended (!slide_active) but entries still exist,
            // so they reflect the final target size (not the last animated frame's target).
            // Skip windows with active per-element animations (e.g., drag-unmaximize) —
            // their geometry is driven by the animation, not the slide.
            if (self.slide_active || self.slide_target_geometries.contains_key(&mapped))
                && !self.animations.contains_key(&mapped)
            {
                self.slide_target_geometries
                    .insert(mapped.clone(), window_geometry);
            } else if self.animations.contains_key(&mapped) {
                // Window has an animation — remove any stale slide target
                self.slide_target_geometries.remove(&mapped);
            }
            self.space
                .map_element(mapped, window_geometry.loc.as_logical(), is_activated);
        }

        self.last_output_size = output_size;
        if !self.slide_active {
            // Only clear target geometries for windows whose buffer has caught up.
            // This prevents a 1-frame gap when slide_active goes false but clients
            // haven't committed at the target size yet.
            self.slide_target_geometries.retain(|mapped, target_geo| {
                if !mapped.alive() {
                    return false;
                }
                let buffer_size = mapped.geometry().size.as_local();
                buffer_size.w != target_geo.size.w || buffer_size.h != target_geo.size.h
            });
        }
        self.pre_slide_positions.retain(|w, _| w.alive());
        self.slide_snapshots
            .lock()
            .unwrap()
            .retain(|w, _| w.alive());
        self.pending_slide_snapshots
            .lock()
            .unwrap()
            .retain(|w, _| w.alive());
        self.deferred_slide_configures.retain(|w| w.alive());
        self.refresh();
        resized
    }

    /// Arm old-content snapshot capture for `windows` on the next render
    /// frame, recording the buffer size each had when its final-size configure
    /// went out — the capture must happen before the clients' reflowed buffers
    /// can arrive, and the recorded size lets the render path detect when it
    /// lost that race.
    pub fn arm_slide_snapshots(
        &mut self,
        windows: Vec<(CosmicMapped, Size<i32, Local>, Size<i32, Local>)>,
    ) {
        if windows.is_empty() {
            return;
        }
        let mut pending = self.pending_slide_snapshots.lock().unwrap();
        for (mapped, old_size, _) in windows {
            pending.insert(mapped, old_size);
        }
    }

    /// Send the withheld slide-start configures for windows whose old-content
    /// snapshot has now been captured (they've left `pending_slide_snapshots`).
    /// `force_all` flushes every remaining one regardless — used at slide settle
    /// so a window that never rendered (e.g. on an inactive workspace) still
    /// gets its final size. Uses `force_configure` so the late send isn't
    /// dropped by XDG configure throttling.
    pub fn flush_deferred_slide_configures(&mut self, force_all: bool) {
        if self.deferred_slide_configures.is_empty() {
            return;
        }
        let pending = self.pending_slide_snapshots.lock().unwrap();
        self.deferred_slide_configures.retain(|mapped| {
            if !mapped.alive() {
                return false; // window gone — drop it
            }
            let captured = !pending.contains_key(mapped);
            if force_all || captured {
                mapped.force_configure();
                false // flushed — remove from the set
            } else {
                true // still awaiting capture — keep deferred
            }
        });
    }

    /// True while any captured slide snapshot is mid-crossfade (its `fade_start`
    /// has fired but the fade hasn't completed). Diagnostic probe for the
    /// redraw-scheduling path: the shell's `animations_going()` /
    /// `animating_outputs()` track slide *motion* only, so a fade that outlives
    /// the motion is invisible to them — this surfaces it.
    pub fn has_slide_fade_in_flight(&self) -> bool {
        self.slide_snapshots
            .lock()
            .unwrap()
            .values()
            .any(|s| s.fade_start.is_some())
    }

    /// Compute a window's position during/after a panel slide animation.
    ///
    /// Uses the saved original position as base (if available), detects manual
    /// user moves/resizes, constrains to the available zone, and manages the
    /// save/restore lifecycle in `pre_slide_positions`.
    fn compute_slide_constrained_position(
        &mut self,
        mapped: &CosmicMapped,
        rect: Rectangle<i32, Local>,
        zone: Rectangle<i32, Local>,
    ) -> Rectangle<i32, Local> {
        // Resolve base position: use saved original if not manually changed.
        let base = self.resolve_slide_base(mapped, rect);

        // Constrain to available zone, saving/restoring as needed.
        self.constrain_to_zone(mapped, base, zone)
    }

    /// Determine the base rectangle for slide positioning.
    /// Returns the saved original position unless the window was manually moved/resized.
    fn resolve_slide_base(
        &mut self,
        mapped: &CosmicMapped,
        rect: Rectangle<i32, Local>,
    ) -> Rectangle<i32, Local> {
        let Some((saved, last_computed)) = self.pre_slide_positions.get(mapped) else {
            return rect;
        };
        let saved = *saved;
        let last_computed = *last_computed;

        let manually_changed = if self.slide_active {
            // During slide: only position changes count (size mismatches are
            // expected from skipped configures and client latency).
            rect.loc != last_computed.loc
        } else {
            // After slide: treat as manual only if position moved unexpectedly
            // or size is outside the range between saved and last_computed
            // (accounts for client latency on rapid configures).
            let position_changed = rect.loc != last_computed.loc && rect.loc != saved.loc;
            let size_in_expected_range = rect.size.w >= saved.size.w.min(last_computed.size.w)
                && rect.size.w <= saved.size.w.max(last_computed.size.w)
                && rect.size.h >= saved.size.h.min(last_computed.size.h)
                && rect.size.h <= saved.size.h.max(last_computed.size.h);
            position_changed || !size_in_expected_range
        };

        if manually_changed {
            tracing::debug!(
                "[PRE_SLIDE] manually_changed: slide_active={} rect={rect:?} \
                 last_computed={last_computed:?} saved={saved:?}",
                self.slide_active
            );
            self.pre_slide_positions.remove(mapped);
            rect
        } else {
            saved
        }
    }

    /// Constrain a window rectangle to fit within `zone`, managing save/restore state.
    fn constrain_to_zone(
        &mut self,
        mapped: &CosmicMapped,
        base: Rectangle<i32, Local>,
        zone: Rectangle<i32, Local>,
    ) -> Rectangle<i32, Local> {
        let mut result = base;
        let zone_right = zone.loc.x + zone.size.w;
        let zone_bottom = zone.loc.y + zone.size.h;

        let out_of_bounds = result.loc.x + result.size.w > zone_right
            || result.loc.x < zone.loc.x
            || result.loc.y + result.size.h > zone_bottom
            || result.loc.y < zone.loc.y;

        if out_of_bounds {
            // Save original position on first push
            if !self.pre_slide_positions.contains_key(mapped) {
                tracing::debug!("[PRE_SLIDE] saving original: base={base:?} zone={zone:?}");
                self.pre_slide_positions
                    .insert(mapped.clone(), (base, base));
            }

            // Clamp horizontally
            if result.loc.x + result.size.w > zone_right {
                result.loc.x = zone_right - result.size.w;
            }
            if result.loc.x < zone.loc.x {
                result.loc.x = zone.loc.x;
                result.size.w = result.size.w.min(zone.size.w);
            }

            // Clamp vertically
            if result.loc.y + result.size.h > zone_bottom {
                result.loc.y = zone_bottom - result.size.h;
            }
            if result.loc.y < zone.loc.y {
                result.loc.y = zone.loc.y;
                result.size.h = result.size.h.min(zone.size.h);
            }

            // Track computed position for manual-change detection
            if let Some(entry) = self.pre_slide_positions.get_mut(mapped) {
                entry.1 = result;
            }
        } else if self.pre_slide_positions.contains_key(mapped) {
            if self.slide_active {
                // Still animating — keep tracking but update last_computed
                if let Some(entry) = self.pre_slide_positions.get_mut(mapped) {
                    entry.1 = result;
                }
            } else {
                // Animation ended — window fits at original, clear saved
                tracing::debug!("[PRE_SLIDE] restoring original: base={base:?} result={result:?}");
                self.pre_slide_positions.remove(mapped);
            }
        }

        result
    }

    #[profiling::function]
    pub fn refresh(&mut self) {
        self.space.refresh();

        if let Some(pos) = self.spawn_order.iter().position(|w| !w.alive()) {
            self.spawn_order.truncate(pos);
        }

        // Cleanup: Check if any parent windows that had embedded children are now gone
        // If so, clear the embedded state so those windows become visible again
        self.cleanup_orphaned_embeds();

        // Update embedded window geometries based on parent window sizes
        self.update_embedded_geometries();

        for element in self
            .space
            .elements()
            .filter(|e| self.space.outputs_for_element(e).is_empty())
            .cloned()
            .collect::<Vec<_>>()
            .into_iter()
        {
            // TODO what about windows leaving to the top with no headerbar to drag? can that happen? (Probably if the user is moving outputs down)
            *element.last_geometry.lock().unwrap() = None;
            self.map_internal(element, None, None, None);
        }
    }

    /// Cleanup orphaned embedded surfaces (where parent has closed)
    /// Uses global embed registry to verify parent is still valid (works across outputs)
    fn cleanup_orphaned_embeds(&mut self) {
        // Check each embedded surface and clear if its parent is gone
        for elem in self.space.elements() {
            let surface_id: Option<String> = elem
                .active_window()
                .wl_surface()
                .map(|s| s.id().to_string());

            if let Some(ref sid) = surface_id
                && let Some(embed_info) =
                    crate::wayland::handlers::surface_embed::get_embed_render_info(
                        &elem.active_window(),
                    )
            {
                // This element is embedded - check if its parent still exists
                // Check globally: parent is valid if it has embeds registered OR is being grabbed
                let parent_valid = crate::wayland::handlers::surface_embed::is_valid_embed_parent(
                    &embed_info.parent_surface_id,
                );
                let parent_grabbed = crate::wayland::handlers::surface_embed::is_parent_grabbed(
                    &embed_info.parent_surface_id,
                );

                // Only clear if parent is not found anywhere
                if !parent_valid && !parent_grabbed {
                    tracing::info!(
                        "Parent surface '{}' no longer valid (not in global registry, not grabbed), clearing embed for surface '{}' (app_id='{}')",
                        embed_info.parent_surface_id,
                        sid,
                        embed_info.embedded_app_id
                    );
                    crate::wayland::handlers::surface_embed::unmark_surface_embedded(sid);
                }
            }
        }
    }

    /// Update embedded window geometries when parent windows resize
    fn update_embedded_geometries(&mut self) {
        // Collect parent windows and their children that need updating
        let updates: Vec<_> = self
            .space
            .elements()
            .filter_map(|elem| {
                let surface_id = elem.active_window().wl_surface()?.id().to_string();
                let geometry = self.space.element_geometry(elem)?;
                // Check if this window has any embedded children (by surface_id)
                let children =
                    crate::wayland::handlers::surface_embed::get_children_for_parent_by_surface_id(
                        &surface_id,
                    );
                if children.is_empty() {
                    return None;
                }
                Some((surface_id, geometry.size, children))
            })
            .collect();

        // For each parent, update embedded geometry and configure embedded windows
        for (parent_surface_id, parent_size, _children) in updates {
            let updated =
                crate::wayland::handlers::surface_embed::update_embedded_geometry_for_parent_by_surface_id(
                    &parent_surface_id,
                    parent_size.w,
                    parent_size.h,
                );

            // Configure embedded windows with new sizes
            for (embedded_surface_id, new_geometry) in updated {
                // Find the embedded window in the space by matching surface ID
                if let Some(embedded_elem) = self.space.elements().find(|e| {
                    e.active_window()
                        .wl_surface()
                        .map(|s| s.id().to_string() == embedded_surface_id)
                        .unwrap_or(false)
                }) {
                    // Set the embedded window's geometry to match the new calculated size
                    let global_geo = Rectangle::new(
                        (new_geometry.loc.x, new_geometry.loc.y).into(),
                        (new_geometry.size.w, new_geometry.size.h).into(),
                    );
                    embedded_elem.active_window().set_geometry(global_geo, 0);
                    embedded_elem.configure();
                    tracing::trace!(
                        "Configured embedded '{}' to new size {}x{}",
                        embedded_surface_id,
                        new_geometry.size.w,
                        new_geometry.size.h
                    );
                }
            }
        }
    }

    pub fn remove_animation(&mut self, mapped: &CosmicMapped) {
        if let Some(anim) = self.animations.remove(mapped) {
            Self::settle_pipelined_state(mapped, &anim);
        }
    }

    pub fn animations_going(&self) -> bool {
        self.dirty.swap(false, Ordering::SeqCst)
            || !self.animations.is_empty()
            || self
                .slide_snapshots
                .lock()
                .unwrap()
                .values()
                .any(|snapshot| snapshot.fade_start.is_some())
    }

    pub fn update_animation_state(&mut self) {
        let was_empty = self.animations.is_empty();
        let now = Instant::now();

        let output = self.space.outputs().next().cloned();
        if let Some(ref output) = output {
            let frame_interval = Duration::from_millis(16);

            // Send pipelined configures for ClientPipelinedResize animations
            for (mapped, anim) in self.animations.iter_mut() {
                if let Animation::ClientPipelinedResize {
                    start,
                    previous_geometry,
                    target_geometry,
                    last_configure_time,
                    ..
                } = anim
                {
                    let elapsed = now.duration_since(*start);
                    let progress = elapsed.min(self.theme.motion.animation).as_secs_f64()
                        / self.theme.motion.animation.as_secs_f64();

                    // Only send if animation is still running and enough time passed
                    let time_since_last = now.duration_since(*last_configure_time);
                    if progress < 1.0 && time_since_last >= frame_interval {
                        // Back-pressure: don't flood the client
                        let pending = mapped.active_window().pending_configure_count();
                        if pending < 3 {
                            // Send configure for 1 frame ahead on the curve
                            let lookahead_progress = (elapsed + frame_interval)
                                .min(self.theme.motion.animation)
                                .as_secs_f64()
                                / self.theme.motion.animation.as_secs_f64();
                            let mut lookahead_geo: Rectangle<i32, Local> = ease(
                                EaseInOutCubic,
                                EaseRectangle(*previous_geometry),
                                EaseRectangle(*target_geometry),
                                lookahead_progress,
                            )
                            .unwrap();

                            // Snap to target when within 2px to avoid near-miss frames
                            if (lookahead_geo.size.w - target_geometry.size.w).abs() <= 2
                                && (lookahead_geo.size.h - target_geometry.size.h).abs() <= 2
                            {
                                lookahead_geo = *target_geometry;
                            }

                            mapped.set_geometry(lookahead_geo.to_global(output));
                            mapped.configure();
                            *last_configure_time = now;

                            tracing::debug!(
                                app_id = %mapped.active_window().app_id(),
                                elapsed_ms = elapsed.as_millis(),
                                progress = format!("{progress:.3}"),
                                pending = pending,
                                lookahead_w = lookahead_geo.size.w,
                                lookahead_h = lookahead_geo.size.h,
                                "[PIPELINE] Sent configure"
                            );
                        } else {
                            tracing::debug!(
                                app_id = %mapped.active_window().app_id(),
                                pending = pending,
                                "[PIPELINE] Back-pressure: skipping configure"
                            );
                        }
                    }
                }
            }

            // Finalize completed ClientPipelinedResize animations.
            // Set `finalized = true` so state is only applied once, but keep the
            // animation alive until the client's buffer catches up to the target size.
            for (mapped, anim) in self.animations.iter_mut() {
                if let Animation::ClientPipelinedResize {
                    start,
                    target_geometry,
                    is_maximize,
                    finalized,
                    ..
                } = anim
                    && !*finalized
                    && now.duration_since(*start) >= self.theme.motion.animation
                {
                    tracing::debug!(
                        app_id = %mapped.active_window().app_id(),
                        is_maximize = ?*is_maximize,
                        target = ?*target_geometry,
                        "[PIPELINE] Animation complete, applying final state"
                    );
                    if let Some(maximize) = *is_maximize {
                        mapped.set_maximized(maximize);
                        mapped.set_tiled(maximize);
                        if !maximize {
                            let layers = layer_map_for_output(output);
                            let zone = layers.non_exclusive_zone().as_local();
                            mapped.set_fills_output_zone(
                                target_geometry.loc.x == zone.loc.x
                                    && target_geometry.loc.y == zone.loc.y
                                    && target_geometry.size.w >= zone.size.w
                                    && target_geometry.size.h >= zone.size.h,
                            );
                        } else {
                            mapped.set_fills_output_zone(false);
                        }
                    }
                    mapped.set_geometry(target_geometry.to_global(output));
                    mapped.configure();
                    self.space
                        .map_element(mapped.clone(), target_geometry.loc.as_logical(), false);
                    *finalized = true;
                }
            }
        }

        self.animations.retain(|mapped, anim| {
            match anim {
                Animation::ClientPipelinedResize {
                    start,
                    target_geometry,
                    finalized,
                    ..
                } => {
                    if !*finalized {
                        // Animation still running — keep
                        return true;
                    }
                    // Safety: don't wait forever if client never reaches target
                    let total_elapsed = now.duration_since(*start);
                    if total_elapsed > self.theme.motion.animation * 3 {
                        tracing::warn!(
                            app_id = %mapped.active_window().app_id(),
                            "[PIPELINE] Buffer never reached target, force-removing animation"
                        );
                        return false;
                    }
                    // Finalized: keep alive until buffer matches target size
                    let buf_size = mapped.geometry().size.as_local();
                    let target_matches = buf_size.w == target_geometry.size.w
                        && buf_size.h == target_geometry.size.h;
                    if target_matches {
                        tracing::debug!(
                            app_id = %mapped.active_window().app_id(),
                            "[PIPELINE] Buffer reached target size, removing animation"
                        );
                    }
                    !target_matches
                }
                Animation::Tiled { .. } | Animation::MapFadeIn { .. } => {
                    now.duration_since(*anim.start()) < self.theme.motion.animation
                }
                _ => now.duration_since(*anim.start()) < self.theme.motion.minimize,
            }
        });
        if self.animations.is_empty() != was_empty {
            self.dirty.store(true, Ordering::SeqCst);
        }
    }

    pub fn merge(&mut self, other: FloatingLayout) {
        for element in other.space.elements() {
            let elem_loc = other
                .space
                .element_geometry(element)
                .unwrap()
                .loc
                .as_local();
            self.map_internal(element.clone(), Some(elem_loc), None, None);
        }
        self.refresh(); //fixup any out of bounds elements
    }

    /// Check if any windows in this layout have blur enabled
    pub fn has_blur_windows(&self) -> bool {
        self.space.elements().any(|elem| elem.has_blur())
    }

    pub fn has_ssd_windows(&self) -> bool {
        self.space
            .elements()
            .any(|elem| elem.has_ssd() && !elem.has_blur())
    }

    /// Get blur windows in Z-order (bottom to top) with their keys
    /// Returns (window_key, geometry, alpha, global_z_index) tuples
    /// global_z_index is the position among ALL windows where 0 = bottom and N-1 = top
    pub fn blur_windows_ordered(
        &self,
        alpha: f32,
    ) -> Vec<(CosmicMappedKey, Rectangle<i32, Local>, f32, usize)> {
        if self.space.outputs().next().is_none() {
            return Vec::new();
        }

        // Count minimizing animations and space elements to get total window count
        let minimizing_count = self
            .animations
            .iter()
            .filter(|(_, anim)| matches!(anim, Animation::Minimize { .. }))
            .count();
        let total_count = minimizing_count + self.space.elements().count();

        if total_count == 0 {
            return Vec::new();
        }

        self.animations
            .iter()
            .filter(|(_, anim)| matches!(anim, Animation::Minimize { .. }))
            .map(|(elem, _)| elem)
            .chain(self.space.elements().rev())
            .enumerate()
            .filter(|(_, elem)| elem.has_blur() || elem.has_ssd())
            .filter_map(|(front_to_back_idx, elem)| {
                // Convert front-to-back index to back-to-front z-index
                // Index 0 in iteration = topmost window = z-index (total-1)
                // Index (total-1) in iteration = bottom window = z-index 0
                let global_z_idx = total_count - 1 - front_to_back_idx;

                let anim_opt = self.animations.get(elem);
                let (geometry, elem_alpha) = if let Some(anim) = anim_opt {
                    (
                        *anim.previous_geometry(),
                        alpha * anim.alpha(self.theme.motion),
                    )
                } else {
                    let geo = self.space.element_geometry(elem)?;
                    (geo.as_local(), alpha)
                };
                Some((elem.key(), geometry, elem_alpha, global_z_idx))
            })
            .collect()
    }

    /// Get blur windows grouped by shared capture requirements.
    /// Consecutive blur windows (no non-blur windows between them) share a capture.
    /// This optimizes rendering by reducing the number of scene captures needed.
    ///
    /// Example:
    /// - Windows: [non-blur z=0, blur z=1, blur z=2, non-blur z=3, blur z=4]
    /// - Groups: [{threshold=1, windows=[z=1,z=2]}, {threshold=4, windows=[z=4]}]
    /// - Only 2 captures needed instead of 3 (when windows don't overlap)
    ///
    /// When consecutive blur windows OVERLAP geometrically, they need separate groups
    /// because the top window needs to capture the bottom window in its blur.
    pub fn blur_windows_grouped(&self, alpha: f32) -> Vec<BlurWindowGroup> {
        if self.space.outputs().next().is_none() {
            return Vec::new();
        }

        // Count minimizing animations and space elements to get total window count
        let minimizing_count = self
            .animations
            .iter()
            .filter(|(_, anim)| matches!(anim, Animation::Minimize { .. }))
            .count();
        let space_element_count = self.space.elements().count();
        let total_count = minimizing_count + space_element_count;

        // Trace: Log window counts to track z-order stability
        tracing::trace!(
            minimizing_count,
            space_element_count,
            total_count,
            "blur_windows_grouped: window counts"
        );

        if total_count == 0 {
            return Vec::new();
        }

        // Collect all windows with their blur status and z-index
        // We need to track non-blur windows to detect gaps between blur windows
        let all_windows: Vec<(
            Option<(CosmicMappedKey, Rectangle<i32, Local>, f32, bool)>,
            usize,
            bool,
        )> = self
            .animations
            .iter()
            .filter(|(_, anim)| matches!(anim, Animation::Minimize { .. }))
            .map(|(elem, _)| elem)
            .chain(self.space.elements().rev())
            .enumerate()
            .filter_map(|(front_to_back_idx, elem)| {
                let global_z_idx = total_count - 1 - front_to_back_idx;
                let needs_blur = elem.has_blur() || (elem.has_ssd() && !elem.has_blur());

                if needs_blur {
                    let anim_opt = self.animations.get(elem);
                    let (geometry, elem_alpha) = if let Some(anim) = anim_opt {
                        (
                            *anim.previous_geometry(),
                            alpha * anim.alpha(self.theme.motion),
                        )
                    } else {
                        let geo = self.space.element_geometry(elem)?;
                        (geo.as_local(), alpha)
                    };
                    // A settled, square-cornered, fully-opaque window covers every
                    // pixel of its geometry. Animating windows are excluded via
                    // elem_alpha, which is scaled by the animation.
                    let square_corners = elem
                        .blur_corner_radius(geometry.size.as_logical(), 0)
                        .iter()
                        .all(|r| *r == 0.0);
                    let paints_full = square_corners && elem_alpha >= 1.0;
                    Some((
                        Some((elem.key(), geometry, elem_alpha, paints_full)),
                        global_z_idx,
                        true,
                    ))
                } else {
                    // Non-blur window - we just need to track its position
                    Some((None, global_z_idx, false))
                }
            })
            .collect();

        // Group consecutive blur windows that don't overlap
        // If windows overlap, the top one needs to see the bottom one in its blur
        let mut groups: Vec<BlurWindowGroup> = Vec::new();
        let mut current_group: Option<BlurWindowGroup> = None;
        let mut last_z_idx: Option<usize> = None;

        // Sort by z-index (ascending = bottom to top)
        let mut sorted_windows = all_windows;
        sorted_windows.sort_by_key(|(_, z_idx, _)| *z_idx);

        for (window_data, z_idx, is_blur) in sorted_windows {
            if is_blur {
                if let Some((key, geometry, elem_alpha, paints_full)) = window_data {
                    // Check if this blur window is consecutive with the previous
                    let is_consecutive = last_z_idx.map(|last| z_idx == last + 1).unwrap_or(true);

                    // Check if this window overlaps with any window in the current group
                    let overlaps_with_group = current_group
                        .as_ref()
                        .map(|group| {
                            group.windows.iter().any(|(_, group_geo, _, _)| {
                                // Check if rectangles intersect
                                geometry.overlaps(*group_geo)
                            })
                        })
                        .unwrap_or(false);

                    if is_consecutive && !overlaps_with_group {
                        // Add to current group or start new one
                        if let Some(ref mut group) = current_group {
                            group.windows.push((key, geometry, elem_alpha, z_idx));
                            group.paints_full_geometry &= paints_full;
                        } else {
                            current_group = Some(BlurWindowGroup {
                                capture_z_threshold: z_idx,
                                windows: vec![(key, geometry, elem_alpha, z_idx)],
                                paints_full_geometry: paints_full,
                            });
                        }
                    } else {
                        // Gap detected OR windows overlap - finish current group and start new one
                        if let Some(group) = current_group.take() {
                            groups.push(group);
                        }
                        current_group = Some(BlurWindowGroup {
                            capture_z_threshold: z_idx,
                            windows: vec![(key, geometry, elem_alpha, z_idx)],
                            paints_full_geometry: paints_full,
                        });
                    }
                }
            } else {
                // Non-blur window creates a gap - finish current group
                if let Some(group) = current_group.take() {
                    groups.push(group);
                }
            }
            last_z_idx = Some(z_idx);
        }

        // Don't forget the last group
        if let Some(group) = current_group {
            groups.push(group);
        }

        groups
    }

    /// Get the geometries of all windows that have blur enabled
    /// Returns (geometry, alpha) tuples
    pub fn blur_window_geometries(
        &self,
        alpha: f32,
        ssd_blur: bool,
    ) -> Vec<(Rectangle<i32, Local>, f32)> {
        if self.space.outputs().next().is_none() {
            return Vec::new();
        }

        let mut geometries: Vec<(Rectangle<i32, Local>, f32)> = self
            .animations
            .iter()
            .filter(|(_, anim)| matches!(anim, Animation::Minimize { .. }))
            .map(|(elem, _)| elem)
            .chain(self.space.elements().rev())
            .filter(|elem| elem.has_blur())
            .filter_map(|elem| {
                let anim_opt = self.animations.get(elem);
                let (geometry, elem_alpha) = if let Some(anim) = anim_opt {
                    (
                        *anim.previous_geometry(),
                        alpha * anim.alpha(self.theme.motion),
                    )
                } else {
                    let geo = self.space.element_geometry(elem)?;
                    (geo.as_local(), alpha)
                };
                Some((geometry, elem_alpha))
            })
            .collect();

        // Add SSD header blur regions for windows with server-side decorations
        // that don't already have full-window blur
        if ssd_blur {
            for elem in self.space.elements().rev() {
                if elem.has_ssd()
                    && !elem.has_blur()
                    && let Some(geo) = self.space.element_geometry(elem)
                {
                    let header_geo = Rectangle::new(
                        geo.loc.as_local(),
                        Size::from((geo.size.w, elem.ssd_height(false).unwrap_or(0))),
                    );
                    geometries.push((header_geo, alpha));
                }
            }
        }

        geometries
    }

    #[profiling::function]
    pub fn render_popups<R>(
        &self,
        renderer: &mut R,
        alpha: f32,
        scanout_node: Option<DrmNode>,
    ) -> Vec<CosmicMappedRenderElement<R>>
    where
        R: AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        CosmicWindowRenderElement<R>: RenderElement<R>,
        CosmicStackRenderElement<R>: RenderElement<R>,
    {
        let output = self.space.outputs().next().unwrap();
        let output_scale = output.current_scale().fractional_scale();

        let mut elements = Vec::default();

        for elem in self
            .animations
            .iter()
            .filter(|(_, anim)| matches!(anim, Animation::Minimize { .. }))
            .map(|(elem, _)| elem)
            .chain(self.space.elements().rev())
        {
            // Check if this is an embedded window
            let is_embedded = elem
                .windows()
                .any(|(w, _)| crate::wayland::handlers::surface_embed::is_surface_embedded(&w));

            if is_embedded {
                // For embedded windows, we need to render popups at the embedded position
                // (inside the parent window), not at the embedded window's workspace position
                let embedded_popup_elements =
                    self.render_embedded_popups(renderer, elem, output_scale, alpha, scanout_node);
                elements.extend(embedded_popup_elements);
            } else {
                // Normal window - render popups at workspace position
                let (geometry, alpha) = self
                    .animations
                    .get(elem)
                    .map(|anim| {
                        (
                            *anim.previous_geometry(),
                            alpha * anim.alpha(self.theme.motion),
                        )
                    })
                    .unwrap_or_else(|| {
                        (self.space.element_geometry(elem).unwrap().as_local(), alpha)
                    });

                let render_location = geometry.loc - elem.geometry().loc.as_local();
                elements.extend(
                    elem.popup_render_elements(
                        renderer,
                        render_location
                            .as_logical()
                            .to_physical_precise_round(output_scale),
                        output_scale.into(),
                        alpha,
                        scanout_node,
                    ),
                );
            }
        }

        elements
    }

    /// Render `mapped`'s current content (at its committed buffer size) into
    /// an owned offscreen texture, for crossfading after a slide-driven
    /// resize. The window elements are rebuilt against the glow renderer so
    /// the capture works for any `R`; on setups where the surface textures
    /// aren't reachable through it the capture fails gracefully (`None`) and
    /// the content swap stays uncrossfaded.
    fn capture_slide_snapshot<R>(
        renderer: &mut R,
        mapped: &CosmicMapped,
        output_scale: f64,
    ) -> Option<(TextureRenderBuffer<GlesTexture>, Size<f64, Logical>)>
    where
        R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
    {
        use smithay::backend::renderer::{Frame as _, element::Element as _};

        let buffer_size = mapped.geometry().size;
        if buffer_size.w <= 0 || buffer_size.h <= 0 {
            return None;
        }
        let size_phys = buffer_size
            .to_f64()
            .to_physical(output_scale)
            .to_i32_round();
        // Window content positioned so its visual origin lands at (0, 0).
        let location = (Point::<i32, Logical>::from((0, 0)) - mapped.geometry().loc)
            .to_physical_precise_round(output_scale);

        let glow = renderer.glow_renderer_mut();
        let elements: Vec<CosmicMappedRenderElement<GlowRenderer>> =
            mapped.render_elements(glow, location, None, output_scale.into(), 1.0, None, None);
        if elements.is_empty() {
            return None;
        }

        let buffer_dims = size_phys.to_logical(1).to_buffer(1, Transform::Normal);
        let texture = Offscreen::<GlesTexture>::create_buffer(glow, Fourcc::Abgr8888, buffer_dims)
            .map_err(|err| {
                tracing::warn!(?err, "Failed to create slide snapshot texture");
                err
            })
            .ok()?;
        let mut texture_buffer =
            TextureRenderBuffer::from_texture(glow, texture, 1, Transform::Normal, None);

        texture_buffer
            .render()
            .draw::<_, GlesError>(|tex| {
                let mut target = glow.bind(tex)?;
                let mut frame = glow.render(&mut target, size_phys, Transform::Normal)?;
                let full = [Rectangle::from_size(size_phys)];
                frame.clear(Color32F::from([0.0, 0.0, 0.0, 0.0]), &full)?;
                // Elements are ordered front-to-back; draw back-to-front.
                for element in elements.iter().rev() {
                    let src = element.src();
                    let dst = element.geometry(output_scale.into());
                    RenderElement::<GlowRenderer>::draw(
                        element,
                        &mut frame,
                        src,
                        dst,
                        &full,
                        &[],
                        None,
                    )?;
                }
                drop(frame);
                Ok(vec![Rectangle::from_size(buffer_dims)])
            })
            .map_err(|err| {
                tracing::warn!(?err, "Failed to render slide snapshot");
                err
            })
            .ok()?;

        // The texture was created with texture_scale 1, so its logical extent
        // equals its pixel dimensions.
        let src_size = Size::from((size_phys.w as f64, size_phys.h as f64));
        Some((texture_buffer, src_size))
    }

    #[profiling::function]
    pub fn render<R>(
        &self,
        renderer: &mut R,
        focused: Option<&CosmicMapped>,
        mut resize_indicator: Option<(ResizeMode, ResizeIndicator)>,
        indicator_thickness: u8,
        alpha: f32,
        theme: &crate::comp_theme::CompTheme,
        element_filter: ElementFilter,
        attached_orb_state: Option<&VoiceOrbState>,
        scanout_node: Option<DrmNode>,
    ) -> Vec<CosmicMappedRenderElement<R>>
    where
        R: AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        CosmicWindowRenderElement<R>: RenderElement<R>,
        CosmicStackRenderElement<R>: RenderElement<R>,
    {
        let output = self.space.outputs().next().unwrap();
        let output_geometry = {
            let layers = layer_map_for_output(output);
            layers.non_exclusive_zone()
        };
        let output_scale = output.current_scale().fractional_scale();

        let mut elements = Vec::default();

        // Extract blur capture context if present, or grabbed window key from LayerBlurCapture
        let (blur_ctx, layer_blur_grabbed_key) = match &element_filter {
            ElementFilter::BlurCapture(ctx) => (Some(ctx), None),
            ElementFilter::LayerBlurCapture(_, grabbed_key) => (None, grabbed_key.as_ref()),
            _ => (None, None),
        };

        // Count total windows for z-index calculation
        let minimizing_count = self
            .animations
            .iter()
            .filter(|(_, anim)| matches!(anim, Animation::Minimize { .. }))
            .count();
        let total_window_count = minimizing_count + self.space.elements().count();

        // Iterate front-to-back (topmost first) using enumerate to track iteration index
        for (front_to_back_idx, elem) in self
            .animations
            .iter()
            .filter(|(_, anim)| matches!(anim, Animation::Minimize { .. }))
            .map(|(elem, _)| elem)
            .chain(self.space.elements().rev())
            .enumerate()
        {
            // Check if this is an embedded window - if so, get the embed render info
            let embed_info = elem.windows().find_map(|(w, _)| {
                crate::wayland::handlers::surface_embed::get_embed_render_info(&w)
            });

            // For now, skip embedded windows - we'll render them separately
            // TODO: Render embedded windows at their parent position + offset
            if embed_info.is_some() {
                tracing::info!(
                    app_id = %elem.active_window().app_id(),
                    "Skipping embedded window from floating render (will render at parent)"
                );
                continue;
            }

            // Check if this specific surface has a pending embed (hide until embedded)
            // This prevents the "flash" of window appearing before embed is fulfilled
            // We use surface_id (unique per window) instead of app_id to avoid hiding
            // multiple windows of the same app (only the one being embedded should hide)
            if let Some(surface_id) = elem
                .active_window()
                .wl_surface()
                .map(|s| s.id().to_string())
                && crate::wayland::handlers::surface_embed::is_surface_id_pending_embed(&surface_id)
            {
                tracing::debug!(
                    app_id = %elem.active_window().app_id(),
                    surface_id = %surface_id,
                    "Hiding window - pending embed for this surface"
                );
                continue;
            }

            // Convert front-to-back index to back-to-front z-index
            // Index 0 in iteration = topmost window = z-index (total-1)
            // Index (total-1) in iteration = bottom window = z-index 0
            let z_idx = if total_window_count > 0 {
                total_window_count - 1 - front_to_back_idx
            } else {
                0
            };

            // When capturing background for blur (iterative multi-pass):
            // - Skip grabbed/dragged windows (they're rendered on top and shouldn't blur themselves)
            // - Skip windows at or above the z-index threshold (blur window and everything above)
            // - For final render (not skipping backdrops), render all windows normally
            if let Some(ctx) = blur_ctx {
                // Skip grabbed/dragged window - it's always on top
                if ctx.is_window_grabbed(elem) {
                    tracing::trace!(
                        window_class = %elem.active_window().app_id(),
                        z_idx = z_idx,
                        "Skipping grabbed window during blur capture"
                    );
                    continue;
                }

                // Check if this window is at or above the z-index threshold
                if ctx.is_z_index_excluded(z_idx) {
                    tracing::trace!(
                        window_class = %elem.active_window().app_id(),
                        z_idx = z_idx,
                        "Excluding window during blur capture (z-index threshold)"
                    );
                    continue;
                }

                // This window WILL be rendered in blur capture
                tracing::trace!(
                    window_class = %elem.active_window().app_id(),
                    z_idx = z_idx,
                    "Including window in blur capture"
                );
            }

            // For LayerBlurCapture: skip grabbed/dragged window (they shouldn't appear in layer blur)
            if let Some(grabbed_key) = layer_blur_grabbed_key
                && elem.key() == *grabbed_key
            {
                tracing::debug!(
                    window_class = %elem.active_window().app_id(),
                    z_idx = z_idx,
                    "Skipping grabbed window during layer blur capture"
                );
                continue;
            }

            let anim_opt = self.animations.get(elem);
            let (mut geometry, alpha) = anim_opt
                .map(|anim| {
                    match anim {
                        Animation::ClientPipelinedResize {
                            previous_geometry,
                            target_geometry,
                            ..
                        } => {
                            // Position based on the buffer's actual committed size.
                            // No stretching — the window is displayed at 1:1.
                            let buffer_size = elem.geometry().size.as_local();
                            let size_range_w = target_geometry.size.w - previous_geometry.size.w;
                            let size_range_h = target_geometry.size.h - previous_geometry.size.h;
                            let progress = if size_range_w.abs() > 1 {
                                ((buffer_size.w - previous_geometry.size.w) as f64
                                    / size_range_w as f64)
                                    .clamp(0.0, 1.0)
                            } else if size_range_h.abs() > 1 {
                                ((buffer_size.h - previous_geometry.size.h) as f64
                                    / size_range_h as f64)
                                    .clamp(0.0, 1.0)
                            } else {
                                1.0
                            };
                            let loc_x = previous_geometry.loc.x
                                + ((target_geometry.loc.x - previous_geometry.loc.x) as f64
                                    * progress) as i32;
                            let loc_y = previous_geometry.loc.y
                                + ((target_geometry.loc.y - previous_geometry.loc.y) as f64
                                    * progress) as i32;
                            let geo = Rectangle::new(Point::from((loc_x, loc_y)), buffer_size);
                            tracing::trace!(
                                app_id = %elem.active_window().app_id(),
                                buf_w = buffer_size.w,
                                buf_h = buffer_size.h,
                                geo_x = loc_x,
                                geo_y = loc_y,
                                prev_x = previous_geometry.loc.x,
                                prev_y = previous_geometry.loc.y,
                                prev_w = previous_geometry.size.w,
                                prev_h = previous_geometry.size.h,
                                tgt_x = target_geometry.loc.x,
                                tgt_y = target_geometry.loc.y,
                                tgt_w = target_geometry.size.w,
                                tgt_h = target_geometry.size.h,
                                progress = format!("{progress:.3}"),
                                output_w = output_geometry.size.w,
                                output_h = output_geometry.size.h,
                                "[PIPELINE_RENDER] frame"
                            );
                            (geo, alpha * anim.alpha(self.theme.motion))
                        }
                        _ => {
                            let geo = *anim.previous_geometry();
                            (geo, alpha * anim.alpha(self.theme.motion))
                        }
                    }
                })
                .unwrap_or_else(|| (self.space.element_geometry(elem).unwrap().as_local(), alpha));

            // During slide animations (no per-element animation), override geometry
            // with the target from recalculate. This ensures shadow, blur, decorations,
            // and content ALL render at the correct animated size — not the buffer's
            // committed size. Eliminates visual snapping when client commits new frames.
            // Also applies post-slide while waiting for client buffers to catch up.
            if anim_opt.is_none()
                && let Some(target_geo) = self.slide_target_geometries.get(elem)
            {
                geometry = *target_geo;
            }

            // Pre-compute the animated geometry for Tiled animations.
            // This is used for blur/backdrop placement so they track the animated size,
            // while the actual buffer stays at previous_geometry and gets rescaled later.
            // ClientPipelinedResize does NOT use this — its geometry is already correct
            // (computed from buffer size above).
            let tiled_anim_geometry = anim_opt.and_then(|anim| {
                if matches!(anim, Animation::Tiled { .. }) {
                    Some(
                        anim.geometry(
                            output_geometry,
                            self.space
                                .element_geometry(elem)
                                .map(RectExt::as_local)
                                .unwrap_or(geometry),
                            elem.floating_tiled.lock().unwrap().as_ref(),
                            self.gaps(),
                            self.theme.motion,
                        ),
                    )
                } else {
                    None
                }
            });

            let render_location = geometry.loc - elem.geometry().loc.as_local();

            let mut window_elements = elem.render_elements(
                renderer,
                render_location
                    .as_logical()
                    .to_physical_precise_round(output_scale),
                None,
                output_scale.into(),
                alpha,
                None,
                scanout_node,
            );
            window_elements.extend(
                elem.shadow_render_element(
                    renderer,
                    render_location
                        .as_logical()
                        .to_physical_precise_round(output_scale),
                    None,
                    output_scale.into(),
                    1.,
                    alpha,
                ),
            );

            // Track animation info for later use (blur needs to be added before rescaling for minimize)
            // Tuple: (original_geo, scale, relocation, buffer_size)
            let minimize_anim_info: Option<(
                Rectangle<i32, Local>,
                Scale<f64>,
                Point<i32, Physical>,
                Size<i32, Local>,
            )> = if let Some(anim) = anim_opt {
                if matches!(
                    anim,
                    Animation::Minimize { .. } | Animation::Unminimize { .. }
                ) {
                    let original_geo = anim.previous_geometry();
                    let target_geometry = anim.geometry(
                        output_geometry,
                        self.space
                            .element_geometry(elem)
                            .map(RectExt::as_local)
                            .unwrap_or(geometry),
                        elem.floating_tiled.lock().unwrap().as_ref(),
                        self.gaps(),
                        self.theme.motion,
                    );

                    let buffer_size = elem.geometry().size.as_local();

                    // Use uniform scaling to maintain aspect ratio
                    let scale_x = target_geometry.size.w as f64 / buffer_size.w as f64;
                    let scale_y = target_geometry.size.h as f64 / buffer_size.h as f64;
                    let uniform_scale = scale_x.min(scale_y);

                    // Calculate centering offset
                    let scaled_w = (buffer_size.w as f64 * uniform_scale) as i32;
                    let scaled_h = (buffer_size.h as f64 * uniform_scale) as i32;
                    let offset_x = (target_geometry.size.w - scaled_w) / 2;
                    let offset_y = (target_geometry.size.h - scaled_h) / 2;

                    let scale = Scale {
                        x: uniform_scale,
                        y: uniform_scale,
                    };
                    let relocation = (target_geometry.loc - original_geo.loc
                        + Point::from((offset_x, offset_y)).as_local())
                    .as_logical()
                    .to_physical_precise_round(output_scale);

                    Some((*original_geo, scale, relocation, buffer_size))
                } else {
                    None
                }
            } else {
                None
            };

            // Compute window border radius from theme (used by both orb and blur)
            let radius_s = theme.radius_s()[0];
            let window_border_radius = if radius_s < 4.0 {
                radius_s
            } else {
                radius_s + 4.0
            };

            // If this window has the attached voice orb, insert it behind window content
            // (In front-to-back rendering: content -> shadow -> orb -> backdrop)
            if let Some(orb_state) = attached_orb_state
                && let Some(attached_surface_id) = orb_state.attached_surface_id_for_render()
            {
                let window_surface_id = elem
                    .active_window()
                    .wl_surface()
                    .map(|s| s.id().to_string());

                if window_surface_id.as_deref() == Some(attached_surface_id) {
                    let output_geo = output.geometry().as_logical();
                    let current_window_geo = geometry.as_logical();

                    if let Some(orb_element) = VoiceOrbShader::element_with_window_override(
                        renderer,
                        orb_state,
                        output_geo,
                        Some(current_window_geo),
                        Some(window_border_radius),
                    ) {
                        window_elements.push(orb_element.into());
                    }
                }
            }

            // Add blur backdrop for windows that request KDE blur (independent of focus state)
            // Design spec: background: rgba(255, 255, 255, 0.10), backdrop-filter: blur(50px)
            // Skip adding backdrop if we're capturing background for blur
            if elem.has_blur() && blur_ctx.is_none() {
                // For minimize/unminimize animation, calculate the scaled blur geometry
                // Use buffer_size (actual window size) not original_geo (which could be minimized size for unminimize)
                let blur_geometry = if let Some((_original_geo, scale, _relocation, buffer_size)) =
                    &minimize_anim_info
                {
                    // Calculate the animated size for blur based on buffer size and scale
                    let scaled_w = (buffer_size.w as f64 * scale.x) as i32;
                    let scaled_h = (buffer_size.h as f64 * scale.y) as i32;

                    // Get the target geometry from the animation (this is the interpolated position/size)
                    if let Some(anim) = anim_opt {
                        let anim_geometry = anim.geometry(
                            output_geometry,
                            self.space
                                .element_geometry(elem)
                                .map(RectExt::as_local)
                                .unwrap_or(geometry),
                            elem.floating_tiled.lock().unwrap().as_ref(),
                            self.gaps(),
                            self.theme.motion,
                        );

                        tracing::debug!(
                            buffer_w = buffer_size.w,
                            buffer_h = buffer_size.h,
                            scale_x = scale.x,
                            scale_y = scale.y,
                            scaled_w = scaled_w,
                            scaled_h = scaled_h,
                            anim_geo_x = anim_geometry.loc.x,
                            anim_geo_y = anim_geometry.loc.y,
                            anim_geo_w = anim_geometry.size.w,
                            anim_geo_h = anim_geometry.size.h,
                            "Minimize/unminimize blur geometry calculation"
                        );

                        // Center the scaled blur within the animation geometry
                        let offset_x = (anim_geometry.size.w - scaled_w) / 2;
                        let offset_y = (anim_geometry.size.h - scaled_h) / 2;
                        Rectangle::new(
                            Point::from((
                                anim_geometry.loc.x + offset_x,
                                anim_geometry.loc.y + offset_y,
                            )),
                            Size::from((scaled_w, scaled_h)),
                        )
                    } else {
                        geometry
                    }
                } else {
                    // For Tiled animations, use the interpolated geometry
                    tiled_anim_geometry.unwrap_or(geometry)
                };

                let corner_radius = elem.blur_corner_radius(
                    blur_geometry.size.as_logical(),
                    window_border_radius.round() as u8,
                );

                // Get the output name for looking up cached blur texture
                let output_name = output.name();
                let window_key = elem.key();
                let output_transform = output.current_transform();
                let output_scale = output.current_scale().fractional_scale();

                // Get per-window blur texture (iterative multi-pass blur)
                let blur_info = get_cached_blur_texture_for_window(&output_name, &window_key);

                if let Some(blur_info) = blur_info {
                    let active = elem.active_window();
                    let blur_saturation = active
                        .wl_surface()
                        .and_then(|s| get_blur_saturation(&s))
                        .unwrap_or(1.0);
                    let blur_tint = active
                        .wl_surface()
                        .and_then(|s| get_blur_tint(&s))
                        .unwrap_or(BLUR_TINT_STRENGTH);
                    let blur_border = active
                        .wl_surface()
                        .and_then(|s| get_blur_border(&s))
                        .unwrap_or(BLUR_BORDER_STRENGTH);
                    // Use BlurredBackdropShader with the cached blurred texture
                    let blur_backdrop = BlurredBackdropShader::element(
                        renderer,
                        &blur_info.texture,
                        blur_geometry,
                        blur_info.size,
                        blur_info.screen_size,
                        output_scale,
                        output_transform,
                        corner_radius,
                        alpha,
                        BLUR_TINT_COLOR,
                        blur_tint,
                        false, // No blur border for regular windows
                        blur_saturation,
                        blur_border,
                    );

                    window_elements.push(blur_backdrop.into());
                } else {
                    tracing::debug!(
                        output = %output_name,
                        "No cached blur texture available, using fallback"
                    );
                    // Fallback
                    let blur_backdrop = BackdropShader::element(
                        renderer,
                        Key::Window(Usage::Overlay, elem.key()),
                        blur_geometry,
                        corner_radius,
                        alpha * BLUR_FALLBACK_ALPHA,
                        BLUR_FALLBACK_COLOR,
                    );
                    window_elements.push(blur_backdrop.into());
                }
            }

            // Add blur backdrop for SSD header on windows without full-window blur
            if !elem.has_blur()
                && elem.has_ssd()
                && blur_ctx.is_none()
                && theme.header_backdrop_blur()
            {
                let header_geo = Rectangle::new(
                    geometry.loc,
                    Size::from((geometry.size.w, elem.ssd_height(false).unwrap_or(0))),
                );
                // Use window's top corner radius for the header (bottom corners are flat
                // since the header meets the window content below).
                // NOTE: The shader's rounded_box SDF uses y-up convention but screen
                // coords are y-down, so tr/br uniforms are swapped relative to screen.
                // Pass top-right radius in the br slot (index 2) for correct screen mapping.
                let full_radius = elem.blur_corner_radius(
                    geometry.size.as_logical(),
                    window_border_radius.round() as u8,
                );
                let ssd_corner_radius = [full_radius[0], 0.0, full_radius[1], 0.0];

                let output_name = output.name();
                let window_key = elem.key();
                let output_transform = output.current_transform();
                let output_scale = output.current_scale().fractional_scale();

                let blur_info = get_cached_blur_texture_for_window(&output_name, &window_key);
                if let Some(blur_info) = blur_info {
                    let active = elem.active_window();
                    let blur_saturation = active
                        .wl_surface()
                        .and_then(|s| get_blur_saturation(&s))
                        .unwrap_or(1.0);
                    let blur_tint = active
                        .wl_surface()
                        .and_then(|s| get_blur_tint(&s))
                        .unwrap_or(BLUR_TINT_STRENGTH);
                    let blur_border = active
                        .wl_surface()
                        .and_then(|s| get_blur_border(&s))
                        .unwrap_or(BLUR_BORDER_STRENGTH);
                    let blur_backdrop = BlurredBackdropShader::element(
                        renderer,
                        &blur_info.texture,
                        header_geo,
                        blur_info.size,
                        blur_info.screen_size,
                        output_scale,
                        output_transform,
                        ssd_corner_radius,
                        alpha,
                        BLUR_TINT_COLOR,
                        blur_tint,
                        false,
                        blur_saturation,
                        blur_border,
                    );
                    window_elements.push(blur_backdrop.into());
                } else {
                    let blur_backdrop = BackdropShader::element(
                        renderer,
                        Key::Window(Usage::Overlay, elem.key()),
                        header_geo,
                        ssd_corner_radius,
                        alpha * BLUR_FALLBACK_ALPHA,
                        BLUR_FALLBACK_COLOR,
                    );
                    window_elements.push(blur_backdrop.into());
                }
            }

            // Render backdrop color for windows that set the backdrop_color protocol
            // (only when blur is not already providing a backdrop)
            if (!elem.has_blur() || blur_ctx.is_some())
                && let Some(wl_surface) = elem.active_window().wl_surface()
                && let Some(color) = get_surface_backdrop_color(&wl_surface)
            {
                let backdrop_geo = tiled_anim_geometry.unwrap_or(geometry);
                let corner_radius =
                    elem.blur_corner_radius(backdrop_geo.size.as_logical(), indicator_thickness);
                let backdrop = BackdropShader::element(
                    renderer,
                    Key::Window(Usage::Overlay, elem.key()),
                    backdrop_geo,
                    corner_radius,
                    alpha * color.alpha_f32(),
                    color.to_rgb_f32(),
                );
                window_elements.push(backdrop.into());
            }

            // Now apply animation transformations
            if let Some(anim) = anim_opt {
                if matches!(anim, Animation::ClientPipelinedResize { .. }) {
                    // No rescaling for client-driven pipelined resize.
                    // Position is already computed from the buffer's actual size above.
                } else if let Some((original_geo, scale, relocation, _buffer_size)) =
                    minimize_anim_info
                {
                    // For minimize/unminimize: scale window elements with uniform scaling
                    // Blur is already rendered at scaled geometry above
                    window_elements = window_elements
                        .into_iter()
                        .map(|element| match element {
                            CosmicMappedRenderElement::Stack(elem) => {
                                CosmicMappedRenderElement::MovingStack({
                                    let rescaled = RescaleRenderElement::from_element(
                                        elem,
                                        original_geo
                                            .loc
                                            .as_logical()
                                            .to_physical_precise_round(output_scale),
                                        scale,
                                    );

                                    RelocateRenderElement::from_element(
                                        rescaled,
                                        relocation,
                                        Relocate::Relative,
                                    )
                                })
                            }
                            CosmicMappedRenderElement::Window(elem) => {
                                CosmicMappedRenderElement::MovingWindow({
                                    let rescaled = RescaleRenderElement::from_element(
                                        elem,
                                        original_geo
                                            .loc
                                            .as_logical()
                                            .to_physical_precise_round(output_scale),
                                        scale,
                                    );

                                    RelocateRenderElement::from_element(
                                        rescaled,
                                        relocation,
                                        Relocate::Relative,
                                    )
                                })
                            }
                            x => x,
                        })
                        .collect();
                } else {
                    // For other compositor-driven animations (like Tiled), use per-axis scaling
                    let original_geo = anim.previous_geometry();
                    geometry = anim.geometry(
                        output_geometry,
                        self.space
                            .element_geometry(elem)
                            .map(RectExt::as_local)
                            .unwrap_or(geometry),
                        elem.floating_tiled.lock().unwrap().as_ref(),
                        self.gaps(),
                        self.theme.motion,
                    );

                    let buffer_size = elem.geometry().size;
                    let scale = Scale {
                        x: geometry.size.w as f64 / buffer_size.w as f64,
                        y: geometry.size.h as f64 / buffer_size.h as f64,
                    };

                    let relocation = (geometry.loc - original_geo.loc)
                        .as_logical()
                        .to_physical_precise_round(output_scale);

                    window_elements = window_elements
                        .into_iter()
                        .map(|element| match element {
                            CosmicMappedRenderElement::Stack(elem) => {
                                CosmicMappedRenderElement::MovingStack({
                                    let rescaled = RescaleRenderElement::from_element(
                                        elem,
                                        original_geo
                                            .loc
                                            .as_logical()
                                            .to_physical_precise_round(output_scale),
                                        scale,
                                    );

                                    RelocateRenderElement::from_element(
                                        rescaled,
                                        relocation,
                                        Relocate::Relative,
                                    )
                                })
                            }
                            CosmicMappedRenderElement::Window(elem) => {
                                CosmicMappedRenderElement::MovingWindow({
                                    let rescaled = RescaleRenderElement::from_element(
                                        elem,
                                        original_geo
                                            .loc
                                            .as_logical()
                                            .to_physical_precise_round(output_scale),
                                        scale,
                                    );

                                    RelocateRenderElement::from_element(
                                        rescaled,
                                        relocation,
                                        Relocate::Relative,
                                    )
                                })
                            }
                            x => x,
                        })
                        .collect();
                }
            } else if self.slide_target_geometries.contains_key(elem) {
                // No per-element animation, but a layer slide is active (or just ended
                // and we're waiting for client catch-up). geometry is already overridden
                // to target_geo above, so we just need to scale the buffer content.
                let buffer_size = elem.geometry().size;
                if buffer_size.w > 0
                    && buffer_size.h > 0
                    && (buffer_size.w != geometry.size.w || buffer_size.h != geometry.size.h)
                {
                    let scale = Scale {
                        x: geometry.size.w as f64 / buffer_size.w as f64,
                        y: geometry.size.h as f64 / buffer_size.h as f64,
                    };
                    let render_loc_phys = geometry
                        .loc
                        .as_logical()
                        .to_physical_precise_round(output_scale);

                    window_elements = window_elements
                        .into_iter()
                        .map(|element| match element {
                            CosmicMappedRenderElement::Stack(elem) => {
                                CosmicMappedRenderElement::MovingStack({
                                    let rescaled = RescaleRenderElement::from_element(
                                        elem,
                                        render_loc_phys,
                                        scale,
                                    );
                                    RelocateRenderElement::from_element(
                                        rescaled,
                                        Point::from((0, 0)),
                                        Relocate::Relative,
                                    )
                                })
                            }
                            CosmicMappedRenderElement::Window(elem) => {
                                CosmicMappedRenderElement::MovingWindow({
                                    let rescaled = RescaleRenderElement::from_element(
                                        elem,
                                        render_loc_phys,
                                        scale,
                                    );
                                    RelocateRenderElement::from_element(
                                        rescaled,
                                        Point::from((0, 0)),
                                        Relocate::Relative,
                                    )
                                })
                            }
                            x => x,
                        })
                        .collect();
                }
            }

            // Slide content crossfade. Windows are configured to their final
            // size the moment a slide starts and their old content is
            // snapshotted; when the client's reflowed buffer arrives — the
            // content swap, whenever it happens — the snapshot fades out over
            // it. Until the swap the snapshot stays hidden: the live (old)
            // buffer is still on screen with identical pixels.
            if anim_opt.is_none() {
                let mut snapshots = self.slide_snapshots.lock().unwrap();
                let buffer_size = elem.geometry().size.as_local();

                let needs_capture = |snapshots: &HashMap<CosmicMapped, SlideSnapshot>| {
                    snapshots
                        .get(elem)
                        // A snapshot from an earlier, already-fading cycle is
                        // stale (it shows even older content) — replace it.
                        .map(|s| s.fade_start.is_some())
                        .unwrap_or(true)
                };

                // Armed by the slide-start layout pass: capture the old
                // content right now, before the client's reflowed buffer can
                // replace it.
                if let Some(expected_old) =
                    self.pending_slide_snapshots.lock().unwrap().remove(elem)
                {
                    if buffer_size != expected_old {
                        // The client committed its final buffer before we got
                        // a frame in — the old content is gone and this swap
                        // cannot be faded. Shows up as a blink at slide start.
                    } else if needs_capture(&snapshots)
                        && let Some((texture, src_size)) =
                            Self::capture_slide_snapshot(renderer, elem, output_scale)
                    {
                        snapshots.insert(
                            elem.clone(),
                            SlideSnapshot {
                                texture,
                                src_size,
                                captured_size: buffer_size,
                                fade_start: None,
                            },
                        );
                    }
                }

                // Fallback capture: the window became size-mismatched during
                // the slide without having been armed (e.g. it appeared late).
                // Strictly only when we hold NOTHING for this window: after
                // the content swap the live (final) buffer stays mismatched
                // with the still-animating target for the rest of the slide,
                // and replacing the snapshot then would abort the running
                // crossfade one frame in — a blink.
                if let Some(target_geo) = self.slide_target_geometries.get(elem)
                    && buffer_size != target_geo.size
                    && !snapshots.contains_key(elem)
                    && let Some((texture, src_size)) =
                        Self::capture_slide_snapshot(renderer, elem, output_scale)
                {
                    snapshots.insert(
                        elem.clone(),
                        SlideSnapshot {
                            texture,
                            src_size,
                            captured_size: buffer_size,
                            fade_start: None,
                        },
                    );
                }

                // The content swap: the live buffer no longer matches what the
                // snapshot captured. Start the crossfade — alpha 1.0 here is
                // pixel-continuous, since the previous frame showed the same
                // old content (live) that the snapshot holds.
                if let Some(snapshot) = snapshots.get_mut(elem)
                    && snapshot.fade_start.is_none()
                    && buffer_size != snapshot.captured_size
                {
                    snapshot.fade_start = Some(Instant::now());
                }

                // Snapshots that never see a swap (window ended at its old
                // size, e.g. after a reversal) are dropped once the slide
                // bookkeeping is gone.
                if !self.slide_active
                    && !self.slide_target_geometries.contains_key(elem)
                    && snapshots.get(elem).is_some_and(|s| s.fade_start.is_none())
                {
                    snapshots.remove(elem);
                }

                let mut remove = false;
                if let Some(snapshot) = snapshots.get(elem)
                    && let Some(start) = snapshot.fade_start
                {
                    let t = (start.elapsed().as_secs_f32()
                        / self.theme.motion.slide_crossfade.as_secs_f32())
                    .min(1.0);
                    if t >= 1.0 {
                        remove = true;
                    } else {
                        // Smoothstep opacity decay — reads as a gentle dissolve.
                        let fade = 1.0 - t * t * (3.0 - 2.0 * t);
                        let snapshot_elem = TextureRenderElement::from_texture_render_buffer(
                            geometry.loc.as_logical().to_f64().to_physical(output_scale),
                            &snapshot.texture,
                            Some(alpha * fade),
                            // Explicit full-texture src: with only `size` set
                            // the element derives src from it and CROPS the
                            // texture instead of scaling it into the rect.
                            Some(Rectangle::from_size(snapshot.src_size)),
                            Some(geometry.size.as_logical()),
                            Kind::Unspecified,
                        );
                        window_elements
                            .insert(0, CosmicMappedRenderElement::WindowSnapshot(snapshot_elem));
                    }
                }
                if remove {
                    snapshots.remove(elem);
                }
            }

            if focused == Some(elem) && !elem.is_maximized(false) {
                if let Some((mode, resize)) = resize_indicator.as_mut() {
                    let mut resize_geometry = geometry;
                    resize_geometry.loc -= (18, 18).into();
                    resize_geometry.size += (36, 36).into();

                    resize.resize(resize_geometry.size.as_logical());
                    resize.output_enter(output, Rectangle::default() /* unused */);
                    window_elements = resize
                        .render_elements::<CosmicWindowRenderElement<R>>(
                            renderer,
                            resize_geometry
                                .loc
                                .as_logical()
                                .to_physical_precise_round(output_scale),
                            output_scale.into(),
                            alpha * mode.alpha(self.theme.motion.animation).unwrap_or(1.0),
                        )
                        .into_iter()
                        .map(CosmicMappedRenderElement::Window)
                        .chain(window_elements.into_iter())
                        .collect();
                }

                let active_window_hint = theme.active_window_hint();
                let radius = elem.corner_radius(geometry.size.as_logical(), indicator_thickness);
                if indicator_thickness > 0 {
                    let element = IndicatorShader::focus_element(
                        renderer,
                        Key::Window(Usage::FocusIndicator, elem.key()),
                        geometry,
                        indicator_thickness,
                        radius,
                        alpha,
                        output_scale,
                        [
                            active_window_hint.red,
                            active_window_hint.green,
                            active_window_hint.blue,
                        ],
                    );
                    window_elements.insert(0, element.into());
                }
            }

            // Render embedded children in front of parent (they'll be on top in the z-order)
            let embedded_elements =
                self.render_embedded_children(renderer, elem, geometry, output_scale, alpha);

            // Log embedded children during blur capture
            if blur_ctx.is_some() && !embedded_elements.is_empty() {
                tracing::debug!(
                    parent_app_id = %elem.active_window().app_id(),
                    embedded_count = embedded_elements.len(),
                    "Blur capture: including embedded children for parent"
                );
            }

            // Combine: embedded first (on top), then parent's elements (behind)
            let mut all_window_elements = embedded_elements;
            all_window_elements.extend(window_elements);
            elements.extend(all_window_elements);
        }

        elements
    }

    pub fn snap_to_corner(&self, mapped: &CosmicMapped, corners: &TiledCorners) {
        *mapped.floating_tiled.lock().unwrap() = Some(*corners);
        mapped.set_tiled(true);
        let snapped_geo = self.snapped_geometry(corners);
        let output = self.space.outputs().next().unwrap();
        mapped.set_geometry(snapped_geo.to_global(output));
        mapped.configure();
    }

    fn snapped_geometry(&self, corners: &TiledCorners) -> Rectangle<i32, Local> {
        let output = self.space.outputs().next().unwrap().clone();
        let layers = layer_map_for_output(&output);
        let non_exclusive = layers.non_exclusive_zone();
        std::mem::drop(layers);
        corners.relative_geometry(non_exclusive, self.gaps())
    }

    fn gaps(&self) -> (i32, i32) {
        let g = self.theme.gaps;
        (g.0 as i32, g.1 as i32)
    }
}
