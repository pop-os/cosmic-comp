// SPDX-License-Identifier: GPL-3.0-only

use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::HashMap,
    ops::ControlFlow,
    sync::{Arc, Weak},
    time::Instant,
};
use wayland_backend::server::ObjectId;

#[cfg(feature = "debug")]
use crate::debug::fps_ui;
use crate::{
    backend::{
        kms::render::gles::GbmGlowBackend,
        render::{
            element::DamageElement,
            shadow::{SHADOW_SHADER, ShadowShader},
            wayland::{
                SurfaceRenderElement,
                blur_effect::BlurShaders,
                clipped_surface::{CLIPPING_SHADER, ClippingShader},
                push_render_elements_from_surface_tree,
            },
        },
    },
    config::ScreenFilter,
    shell::{
        CosmicMapped, CosmicMappedRenderElement, OutputId, OverviewMode, SeatExt, Trigger,
        WorkspaceDelta, WorkspaceRenderElement,
        element::{CosmicMappedKey, window::CosmicWindowRenderElement},
        focus::{FocusTarget, Stage, render_input_order, target::WindowGroup},
        grabs::{SeatMenuGrabState, SeatMoveGrabState},
        zoom::ZoomState,
    },
    utils::{prelude::*, quirks::workspace_overview_is_open},
    wayland::{
        handlers::{
            compositor::FRAME_TIME_FILTER,
            corner_radius::{pad_rect, surface_corners, surface_padding},
            data_device::get_dnd_icon,
            image_copy_capture::{
                FrameHolder, SessionData, render_element_buffers, render_session,
            },
        },
        protocols::{
            corner_radius::get_surface_corner_radius, layer_shadow::surface_has_shadow,
            workspace::WorkspaceHandle,
        },
    },
};

// MERGE: our fork replaced `cosmic::Theme` with `CompTheme` (icetron design tokens);
// upstream's `theme.cosmic()` accessors are translated to `CompTheme` methods below.
use crate::comp_theme::CompTheme;
use smithay::{
    backend::{
        allocator::Fourcc,
        drm::{DrmDeviceFd, DrmNode},
        renderer::{
            Color32F, Offscreen, Texture, TextureFilter,
            damage::{Error as RenderError, OutputDamageTracker, RenderOutputResult},
            element::{
                Element, Id, Kind, NamespacedElement, RenderElement, WeakId,
                texture::{TextureRenderBuffer, TextureRenderElement},
                utils::{
                    ConstrainAlign, ConstrainScaleBehavior, CropRenderElement, Relocate,
                    RelocateRenderElement, RescaleRenderElement, constrain_render_elements,
                },
            },
            gles::{
                GlesError, GlesPixelProgram, GlesRenderer, GlesTexProgram, GlesTexture, Uniform,
                UniformName, UniformType,
                element::{PixelShaderElement, TextureShaderElement},
            },
            glow::GlowRenderer,
            multigpu::{Error as MultiError, MultiFrame, MultiRenderer},
            sync::SyncPoint,
        },
    },
    desktop::utils::bbox_from_surface_tree,
    input::Seat,
    output::{Output, OutputModeSource, OutputNoMode},
    reexports::wayland_server::Resource,
    utils::{
        IsAlive, Logical, Monotonic, Physical, Point, Rectangle, Scale, Size, Time, Transform,
    },
    wayland::{
        compositor::with_states, dmabuf::get_dmabuf, seat::WaylandFocus, session_lock::LockSurface,
    },
};

#[cfg(feature = "debug")]
use smithay_egui::EguiState;

pub mod adaptive_foreground;
pub mod animations;
pub mod cursor;
pub mod element;
pub mod gpu_profiler;
pub mod perf_badge;
pub mod shadow;
pub mod voice_orb;
// MERGE: our `blur` + `clipped_surface` modules are replaced by upstream's
// `wayland::{blur_effect, clipped_surface}` (PR #2179 frosted glass).
pub mod wayland;
use self::element::{AsGlowRenderer, CosmicElement};

use super::kms::Timings;

pub type GlMultiRenderer<'a> =
    MultiRenderer<'a, 'a, GbmGlowBackend<DrmDeviceFd>, GbmGlowBackend<DrmDeviceFd>>;
pub type GlMultiFrame<'a, 'frame, 'buffer> =
    MultiFrame<'a, 'a, 'frame, 'buffer, GbmGlowBackend<DrmDeviceFd>, GbmGlowBackend<DrmDeviceFd>>;
pub type GlMultiError = MultiError<GbmGlowBackend<DrmDeviceFd>, GbmGlowBackend<DrmDeviceFd>>;

pub enum RendererRef<'a> {
    Glow(&'a mut GlowRenderer),
    GlMulti(GlMultiRenderer<'a>),
}

impl AsRef<GlowRenderer> for RendererRef<'_> {
    fn as_ref(&self) -> &GlowRenderer {
        match self {
            Self::Glow(renderer) => renderer,
            Self::GlMulti(renderer) => renderer.as_ref(),
        }
    }
}

impl AsMut<GlowRenderer> for RendererRef<'_> {
    fn as_mut(&mut self) -> &mut GlowRenderer {
        match self {
            Self::Glow(renderer) => renderer,
            Self::GlMulti(renderer) => renderer.as_mut(),
        }
    }
}

pub static CLEAR_COLOR: Color32F = Color32F::new(0.153, 0.161, 0.165, 1.0);
pub static OUTLINE_SHADER: &str = include_str!("./shaders/rounded_outline.frag");
pub static RECTANGLE_SHADER: &str = include_str!("./shaders/rounded_rectangle.frag");
pub static POSTPROCESS_SHADER: &str = include_str!("./shaders/offscreen.frag");
// MERGE: our dual-Kawase / blurred-backdrop shaders (fragment + compute) and the
// whole `blur` module re-export block are dropped — upstream's `wayland::blur_effect`
// (`BlurShaders`) replaces them, blitting the region under the element out of the
// live framebuffer instead of maintaining our own capture/cache pipeline.
pub mod nis_coefficients;

pub static FSR_EASU_SHADER: &str = include_str!("./shaders/fsr_easu.frag");
pub static FSR_RCAS_SHADER: &str = include_str!("./shaders/fsr_rcas.frag");
pub static GROUP_COLOR: [f32; 3] = [0.788, 0.788, 0.788];
pub static ACTIVE_GROUP_COLOR: [f32; 3] = [0.58, 0.922, 0.922];

pub struct IndicatorShader(pub GlesPixelProgram);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Usage {
    OverviewBackdrop,
    Overlay,
    MoveGrabIndicator,
    FocusIndicator,
    PotentialGroupIndicator,
    SnappingIndicator,
    Border,
}

#[derive(Clone)]
pub enum Key {
    Static(WeakId),
    Group(Weak<()>),
    Window(Usage, CosmicMappedKey),
    /// Layer surface key using protocol ID
    LayerSurface(u32),
}
impl std::hash::Hash for Key {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Key::Static(id) => id.hash(state),
            Key::Group(arc) => (arc.as_ptr() as usize).hash(state),
            Key::Window(usage, window) => {
                usage.hash(state);
                window.hash(state);
            }
            Key::LayerSurface(id) => id.hash(state),
        }
    }
}
impl PartialEq for Key {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Key::Static(s1), Key::Static(s2)) => s1 == s2,
            (Key::Group(g1), Key::Group(g2)) => Weak::ptr_eq(g1, g2),
            (Key::Window(u1, w1), Key::Window(u2, w2)) => u1 == u2 && w1 == w2,
            (Key::LayerSurface(id1), Key::LayerSurface(id2)) => id1 == id2,
            _ => false,
        }
    }
}
impl Eq for Key {}
impl From<WindowGroup> for Key {
    fn from(group: WindowGroup) -> Self {
        Key::Group(group.alive.clone())
    }
}
impl From<Id> for Key {
    fn from(id: Id) -> Self {
        Key::Static(id.downgrade())
    }
}

#[derive(PartialEq)]
struct IndicatorSettings {
    thickness: u8,
    outer_radius: [u8; 4],
    alpha: f32,
    color: [f32; 3],
    scale: f64,
}
type IndicatorCache = RefCell<HashMap<Key, (IndicatorSettings, PixelShaderElement)>>;

impl IndicatorShader {
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesPixelProgram {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<IndicatorShader>()
            .expect("Custom Shaders not initialized")
            .0
            .clone()
    }

    pub fn focus_element<R: AsGlowRenderer>(
        renderer: &R,
        key: impl Into<Key>,
        mut element_geo: Rectangle<i32, Local>,
        thickness: u8,
        inner_radius: [u8; 4],
        alpha: f32,
        scale: f64,
        active_window_hint: [f32; 3],
    ) -> PixelShaderElement {
        let t = thickness as i32;
        element_geo.loc -= (t, t).into();
        element_geo.size += (t * 2, t * 2).into();
        let outer_radius = inner_radius.map(|r| r + thickness);

        IndicatorShader::element(
            renderer,
            key,
            element_geo,
            thickness,
            outer_radius,
            alpha,
            scale,
            active_window_hint,
        )
    }

    pub fn element<R: AsGlowRenderer>(
        renderer: &R,
        key: impl Into<Key>,
        geo: Rectangle<i32, Local>,
        thickness: u8,
        outer_radius: [u8; 4],
        alpha: f32,
        scale: f64,
        color: [f32; 3],
    ) -> PixelShaderElement {
        let settings = IndicatorSettings {
            thickness,
            outer_radius,
            alpha,
            scale,
            color,
        };

        let user_data = Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data();

        user_data.insert_if_missing(|| IndicatorCache::new(HashMap::new()));
        let mut cache = user_data.get::<IndicatorCache>().unwrap().borrow_mut();
        cache.retain(|k, _| match k {
            Key::Static(w) => w.upgrade().is_some(),
            Key::Group(w) => w.upgrade().is_some(),
            Key::Window(_, w) => w.alive(),
            Key::LayerSurface(_) => true, // Layer surfaces are always considered alive
        });

        let key = key.into();
        if cache
            .get(&key)
            .filter(|(old_settings, _)| &settings == old_settings)
            .is_none()
        {
            let thickness: f32 = ((thickness as f64 * scale) / scale) as f32;
            let shader = Self::get(renderer);

            let elem = PixelShaderElement::new(
                shader,
                geo.as_logical(),
                None, //TODO
                alpha,
                vec![
                    Uniform::new(
                        "color",
                        [color[0] * alpha, color[1] * alpha, color[2] * alpha],
                    ),
                    Uniform::new("thickness", thickness),
                    Uniform::new(
                        "radius",
                        [
                            outer_radius[3] as f32,
                            outer_radius[1] as f32,
                            outer_radius[0] as f32,
                            outer_radius[2] as f32,
                        ],
                    ),
                    Uniform::new("scale", scale as f32),
                ],
                Kind::Unspecified,
            );
            cache.insert(key.clone(), (settings, elem));
        }

        let elem = &mut cache.get_mut(&key).unwrap().1;
        if elem.geometry(1.0.into()).to_logical(1) != geo.as_logical() {
            elem.resize(geo.as_logical(), None);
        }
        elem.clone()
    }
}

pub struct BackdropShader(pub GlesPixelProgram);

#[derive(PartialEq)]
struct BackdropSettings {
    corner_radius: [f32; 4],
    alpha: f32,
    color: [f32; 3],
}
type BackdropCache = RefCell<HashMap<Key, (BackdropSettings, PixelShaderElement)>>;

impl BackdropShader {
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesPixelProgram {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<BackdropShader>()
            .expect("Custom Shaders not initialized")
            .0
            .clone()
    }

    pub fn element<R: AsGlowRenderer>(
        renderer: &R,
        key: impl Into<Key>,
        geo: Rectangle<i32, Local>,
        corner_radius: [f32; 4],
        alpha: f32,
        color: [f32; 3],
    ) -> PixelShaderElement {
        let settings = BackdropSettings {
            corner_radius,
            alpha,
            color,
        };

        let user_data = Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data();

        user_data.insert_if_missing(|| BackdropCache::new(HashMap::new()));
        let mut cache = user_data.get::<BackdropCache>().unwrap().borrow_mut();
        cache.retain(|k, _| match k {
            Key::Static(w) => w.upgrade().is_some(),
            Key::Group(a) => a.upgrade().is_some(),
            Key::Window(_, w) => w.alive(),
            Key::LayerSurface(_) => true, // Layer surfaces are always considered alive
        });

        let key = key.into();
        if cache
            .get(&key)
            .filter(|(old_settings, _)| &settings == old_settings)
            .is_none()
        {
            let shader = Self::get(renderer);

            let elem = PixelShaderElement::new(
                shader,
                geo.as_logical(),
                None, // TODO
                alpha,
                vec![
                    Uniform::new(
                        "color",
                        [color[0] * alpha, color[1] * alpha, color[2] * alpha],
                    ),
                    Uniform::new("corner_radius_tl", corner_radius[0]),
                    Uniform::new("corner_radius_tr", corner_radius[1]),
                    Uniform::new("corner_radius_br", corner_radius[2]),
                    Uniform::new("corner_radius_bl", corner_radius[3]),
                ],
                Kind::Unspecified,
            );
            cache.insert(key.clone(), (settings, elem));
        }

        let elem = &mut cache.get_mut(&key).unwrap().1;
        if elem.geometry(1.0.into()).to_logical(1) != geo.as_logical() {
            elem.resize(geo.as_logical(), None);
        }
        elem.clone()
    }
}

pub struct PostprocessShader(pub GlesTexProgram);
/// FSR upscaling pass — see `shaders/fsr_easu.frag`.
pub struct FsrEasuShader(pub GlesTexProgram);
/// FSR sharpening pass — see `shaders/fsr_rcas.frag`.
pub struct FsrRcasShader(pub GlesTexProgram);

impl FsrEasuShader {
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesTexProgram {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<FsrEasuShader>()
            .expect("Custom Shaders not initialized")
            .0
            .clone()
    }
}

impl FsrRcasShader {
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesTexProgram {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<FsrRcasShader>()
            .expect("Custom Shaders not initialized")
            .0
            .clone()
    }
}

pub fn init_shaders(renderer: &mut GlesRenderer) -> Result<(), GlesError> {
    {
        let egl_context = renderer.egl_context();
        if egl_context.user_data().get::<IndicatorShader>().is_some()
            && egl_context.user_data().get::<BackdropShader>().is_some()
            && egl_context.user_data().get::<PostprocessShader>().is_some()
        {
            return Ok(());
        }
    }

    let outline_shader = renderer.compile_custom_pixel_shader(
        OUTLINE_SHADER,
        &[
            UniformName::new("color", UniformType::_3f),
            UniformName::new("thickness", UniformType::_1f),
            UniformName::new("scale", UniformType::_1f),
            UniformName::new("radius", UniformType::_4f),
        ],
    )?;
    let rectangle_shader = renderer.compile_custom_pixel_shader(
        RECTANGLE_SHADER,
        &[
            UniformName::new("color", UniformType::_3f),
            UniformName::new("corner_radius_tl", UniformType::_1f),
            UniformName::new("corner_radius_tr", UniformType::_1f),
            UniformName::new("corner_radius_br", UniformType::_1f),
            UniformName::new("corner_radius_bl", UniformType::_1f),
        ],
    )?;
    let postprocess_shader = renderer.compile_custom_texture_shader(
        POSTPROCESS_SHADER,
        &[
            UniformName::new("invert", UniformType::_1f),
            UniformName::new("color_mode", UniformType::_1f),
            UniformName::new("night_shift", UniformType::_1f),
        ],
    )?;
    let clipping_shader = renderer.compile_custom_texture_shader(
        CLIPPING_SHADER,
        &[
            UniformName::new("geo_size", UniformType::_2f),
            UniformName::new("corner_radius", UniformType::_4f),
            UniformName::new("input_to_geo", UniformType::Matrix3x3),
            UniformName::new("noise", UniformType::_1f),
            // The shader still declares and uses `scale` (half_px = 0.5 / scale).
            // Leaving it unregistered leaves it at 0, so the corner
            // antialiasing band becomes infinite and nothing is ever clipped.
            UniformName::new("scale", UniformType::_1f),
            // Frosted-glass appearance, from org_kde_kwin_blur.
            // Every call site building uniforms for this program must set
            // `saturation`: an unset uniform reads as 0, which is fully
            // greyscale rather than "unchanged".
            UniformName::new("saturation", UniformType::_1f),
            UniformName::new("frost_tint", UniformType::_1f),
            UniformName::new("border", UniformType::_1f),
        ],
    )?;
    let shadow_shader = renderer.compile_custom_pixel_shader(
        SHADOW_SHADER,
        &[
            // Primary shadow uniforms
            UniformName::new("shadow_color", UniformType::_4f),
            UniformName::new("sigma", UniformType::_1f),
            UniformName::new("input_to_geo", UniformType::Matrix3x3),
            UniformName::new("geo_size", UniformType::_2f),
            UniformName::new("corner_radius", UniformType::_4f),
            // Window cutout uniforms
            UniformName::new("window_input_to_geo", UniformType::Matrix3x3),
            UniformName::new("window_geo_size", UniformType::_2f),
            UniformName::new("window_corner_radius", UniformType::_4f),
        ],
    )?;
    let fsr_easu_shader = renderer.compile_custom_texture_shader(
        FSR_EASU_SHADER,
        &[
            UniformName::new("src_size", UniformType::_2f),
            UniformName::new("dst_size", UniformType::_2f),
        ],
    )?;
    let fsr_rcas_shader = renderer.compile_custom_texture_shader(
        FSR_RCAS_SHADER,
        &[
            UniformName::new("inv_size", UniformType::_2f),
            UniformName::new("sharpness", UniformType::_1f),
        ],
    )?;
    // MERGE: our dual-Kawase fragment/compute blur shaders are replaced by upstream's
    // `BlurShaders`.
    let blur_shaders = BlurShaders::compile(renderer)?;

    let egl_context = renderer.egl_context();
    egl_context
        .user_data()
        .insert_if_missing(|| IndicatorShader(outline_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| BackdropShader(rectangle_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| PostprocessShader(postprocess_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| ClippingShader(clipping_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| ShadowShader(shadow_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| FsrEasuShader(fsr_easu_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| FsrRcasShader(fsr_rcas_shader));
    egl_context.user_data().insert_if_missing(|| blur_shaders);

    // Initialize voice orb shader
    voice_orb::VoiceOrbShader::init(renderer)?;

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CursorMode {
    None,
    NotDefault,
    All,
}

#[profiling::function]
pub fn cursor_elements<'a, 'frame, R>(
    renderer: &mut R,
    seats: impl Iterator<Item = &'a Seat<State>>,
    zoom_state: Option<&ZoomState>,
    theme: &CompTheme,
    blur_strength: usize,
    now: Time<Monotonic>,
    output: &Output,
    mode: CursorMode,
    exclude_dnd_icon: bool,
    // Fork: embedded children follow their move-grabbed parent, and the voice orb
    // can be attached to the grabbed window.
    embedded_children_for_grabbed: &[(
        CosmicMapped,
        crate::wayland::handlers::surface_embed::EmbedRenderInfo,
    )],
    attached_orb_state: Option<&voice_orb::VoiceOrbState>,
    scanout_node: Option<DrmNode>,
    push: &mut dyn FnMut(CosmicElement<R>),
) where
    R: AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    let scale = output.current_scale().fractional_scale();
    let (focal_point, zoom_scale) = zoom_state
        .map(|state| {
            (
                state.animating_focal_point(Some(output)).to_local(output),
                state.animating_level(output),
            )
        })
        .unwrap_or_else(|| ((0., 0.).into(), 1.));

    for seat in seats {
        let pointer = match seat.get_pointer() {
            Some(ptr) => ptr,
            None => continue,
        };
        let location = pointer.current_location() - output.current_location().to_f64();

        if mode != CursorMode::None {
            cursor::draw_cursor(
                renderer,
                seat,
                location,
                scale.into(),
                zoom_scale,
                now,
                blur_strength,
                mode != CursorMode::NotDefault,
                &mut |elem, hotspot| {
                    push(CosmicElement::Cursor(RescaleRenderElement::from_element(
                        RelocateRenderElement::from_element(
                            elem,
                            Point::from((-hotspot.x, -hotspot.y)),
                            Relocate::Relative,
                        ),
                        focal_point
                            .as_logical()
                            .to_physical(output.current_scale().fractional_scale())
                            .to_i32_round(),
                        zoom_scale,
                    )))
                },
            );
        }

        if !exclude_dnd_icon && let Some(dnd_icon) = get_dnd_icon(seat) {
            cursor::draw_dnd_icon(
                renderer,
                &dnd_icon.surface,
                (location + dnd_icon.offset.to_f64()).to_i32_round(),
                scale,
                blur_strength,
                &mut |elem| push(CosmicElement::Dnd(elem)),
            );
        }

        // `theme` is already a `CompTheme`, so there is no `.cosmic()` inner layer.
        if let Some(grab_state) = seat
            .user_data()
            .get::<SeatMoveGrabState>()
            .unwrap()
            .lock()
            .unwrap()
            .as_ref()
        {
            grab_state.render(
                renderer,
                output,
                theme,
                embedded_children_for_grabbed,
                attached_orb_state,
                scanout_node,
                &mut |elem| {
                    push(CosmicElement::MoveGrab(RescaleRenderElement::from_element(
                        elem,
                        focal_point
                            .as_logical()
                            .to_physical(output.current_scale().fractional_scale())
                            .to_i32_round(),
                        zoom_scale,
                    )));
                },
            )
        }

        if let Some(grab_state) = seat
            .user_data()
            .get::<SeatMenuGrabState>()
            .unwrap()
            .lock()
            .unwrap()
            .as_ref()
        {
            let should_scale = !grab_state.is_in_screen_space();
            grab_state.render(renderer, output, &mut |elem| {
                push(CosmicElement::MoveGrab(RescaleRenderElement::from_element(
                    elem.into(),
                    if should_scale {
                        focal_point
                            .as_logical()
                            .to_physical(output.current_scale().fractional_scale())
                            .to_i32_round()
                    } else {
                        Point::from((0, 0))
                    },
                    if should_scale { zoom_scale } else { 1.0 },
                )));
            })
        }
    }
}

#[cfg(not(feature = "debug"))]
pub type EguiState = ();

/// Context for home visibility filtering in layer surfaces
#[derive(Clone, PartialEq)]
pub struct HomeVisibilityContext {
    /// Set of surface IDs that are "home-only" (only visible when in home mode)
    pub home_only_surfaces: std::collections::HashSet<ObjectId>,
    /// Set of surface IDs that are "hide-on-home" (hidden when in home mode)
    pub hide_on_home_surfaces: std::collections::HashSet<ObjectId>,
    /// Set of surface IDs that are explicitly hidden by client (layer_surface_visibility protocol)
    pub hidden_surfaces: std::collections::HashSet<ObjectId>,
    /// Set of surface IDs that currently have active slide animations
    pub sliding_surfaces: std::collections::HashSet<ObjectId>,
    /// Current home alpha (0.0 = home hidden, 1.0 = home fully visible)
    pub home_alpha: f32,
    /// Current voice mode window alpha (1.0 = full opacity, 0.15 = faded for voice mode)
    pub voice_mode_alpha: f32,
    /// Current voice mode layer shell alpha (0 during burst transition, otherwise same as voice_mode_alpha)
    /// Layer shells wait until burst animation completes so windows fade in first
    pub voice_mode_layer_alpha: f32,
    /// Layer surfaces currently fading in (surface ObjectId -> current alpha 0.0-1.0)
    pub layer_fade_in_alphas: std::collections::HashMap<ObjectId, f32>,
    /// Layer surfaces currently fading out (surface ObjectId -> current alpha 1.0-0.0)
    pub layer_fade_out_alphas: std::collections::HashMap<ObjectId, f32>,
}

impl HomeVisibilityContext {
    /// Create a new context from shell state
    pub fn from_shell(shell: &crate::shell::Shell) -> Self {
        Self {
            home_only_surfaces: shell.home_only_surfaces().clone(),
            hide_on_home_surfaces: shell.hide_on_home_surfaces().clone(),
            hidden_surfaces: shell.hidden_surfaces().clone(),
            sliding_surfaces: shell
                .layer_slides
                .iter()
                .map(|s| s.surface_id.clone())
                .collect(),
            home_alpha: shell.home_alpha(),
            voice_mode_alpha: shell.voice_mode_window_alpha(),
            voice_mode_layer_alpha: shell.voice_mode_layer_shell_alpha(),
            layer_fade_in_alphas: shell.layer_fade_in_alphas(),
            layer_fade_out_alphas: shell.layer_fade_out_alphas(),
        }
    }

    /// Get visibility and alpha for a surface based on home mode and voice mode
    /// Returns (visible, alpha) where visible indicates if surface should be rendered
    ///
    /// The `layer` parameter specifies the layer shell layer (if this is a layer surface).
    /// The `namespace` parameter is the app_id/namespace for layer surfaces.
    /// Voice mode alpha is NOT applied to:
    /// - Background layer surfaces (wallpaper should remain visible)
    /// - cosmic-panel (system panel should remain visible)
    /// All other surfaces (including Top layer like dock) fade during voice mode.
    ///
    /// Layer shell surfaces (layer is Some) use voice_mode_layer_alpha which stays at 0
    /// during burst transition so windows fade in first.
    pub fn surface_visibility(
        &self,
        surface_id: &ObjectId,
        layer: Option<smithay::wayland::shell::wlr_layer::Layer>,
        namespace: Option<&str>,
    ) -> (bool, f32) {
        use smithay::wayland::shell::wlr_layer::Layer;

        // Check if surface is explicitly hidden via layer_surface_visibility protocol
        if self.hidden_surfaces.contains(surface_id) {
            return (false, 0.0);
        }

        // Skip voice mode alpha for:
        // - Background layer surfaces (wallpaper like cosmic-bg)
        // - cosmic-panel (system panel should remain visible during voice mode)
        let is_background = matches!(layer, Some(Layer::Background));
        let is_cosmic_panel = namespace.is_some_and(|ns| ns == "cosmic-panel");
        let skip_voice_mode_alpha = is_background || is_cosmic_panel;

        // Use layer shell alpha for layer surfaces (waits until burst completes)
        // Use window alpha for regular windows (fades in during burst)
        let is_layer_shell = layer.is_some();
        let effective_voice_alpha = if is_layer_shell {
            self.voice_mode_layer_alpha
        } else {
            self.voice_mode_alpha
        };

        if self.home_only_surfaces.contains(surface_id) {
            // Home-only surface: visible only when home_alpha > 0
            if self.home_alpha > 0.0 {
                let alpha = if skip_voice_mode_alpha {
                    self.home_alpha
                } else {
                    self.home_alpha * effective_voice_alpha
                };
                if alpha > 0.0 {
                    (true, alpha)
                } else {
                    (false, 0.0)
                }
            } else {
                (false, 0.0)
            }
        } else if self.hide_on_home_surfaces.contains(surface_id) {
            // Hide-on-home surface: inverse of home_alpha
            let base_alpha = 1.0 - self.home_alpha;
            if base_alpha > 0.0 {
                let alpha = if skip_voice_mode_alpha {
                    base_alpha
                } else {
                    base_alpha * effective_voice_alpha
                };
                if alpha > 0.0 {
                    (true, alpha)
                } else {
                    (false, 0.0)
                }
            } else {
                (false, 0.0)
            }
        } else {
            // Regular surface (not home-only or hide-on-home)
            // Still respect skip_voice_mode_alpha for Background/Top layer surfaces like cosmic-bg
            if skip_voice_mode_alpha {
                // Background or Top layer surface - always visible, no voice mode fade
                (true, 1.0)
            } else if effective_voice_alpha > 0.0 {
                // Apply voice mode alpha (fades during voice mode)
                (true, effective_voice_alpha)
            } else {
                (false, 0.0)
            }
        }
    }
}

// MERGE: our blur-capture variants (`ExcludeBlurWindows`, `BlurCapture`,
// `LayerBlurCapture`) and the unused `home_visibility()` placeholder are dropped —
// upstream's blur reads the live framebuffer, so no capture pass (and no
// capture-only filter) exists any more. This is `Copy` again, like upstream.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ElementFilter {
    All,
    ExcludeWorkspaceOverview,
    LayerShellOnly,
}

pub fn output_elements<R>(
    _gpu: Option<&DrmNode>,
    renderer: &mut R,
    shell: &Arc<parking_lot::RwLock<Shell>>,
    now: Time<Monotonic>,
    output: &Output,
    cursor_mode: CursorMode,
    _fps: Option<(&EguiState, &Timings)>,
    scanout_node: Option<DrmNode>,
) -> Result<Vec<CosmicElement<R>>, RenderError<R::Error>>
where
    R: AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    #[cfg(feature = "debug")]
    let mut debug_elements = {
        let output_geo = output.geometry();
        let shell_guard = shell.read();
        let seats = shell_guard.seats.iter().cloned().collect::<Vec<_>>();
        let debug_active = shell_guard.debug_active;
        std::mem::drop(shell_guard);
        let scale = output.current_scale().fractional_scale();

        if let Some((state, timings)) = _fps {
            vec![
                fps_ui(
                    _gpu,
                    debug_active,
                    &seats,
                    renderer.glow_renderer_mut(),
                    state,
                    timings,
                    Rectangle::from_size(
                        (output_geo.size.w.min(400), output_geo.size.h.min(800)).into(),
                    ),
                    scale,
                )
                .map_err(R::from_gles_error)
                .map_err(RenderError::Rendering)?
                .into(),
            ]
        } else {
            Vec::new()
        }
    };

    let shell_guard = shell.read();
    let Some((previous_workspace, workspace)) = shell_guard.workspaces.active(output) else {
        #[cfg(not(feature = "debug"))]
        return Ok(Vec::new());
        #[cfg(feature = "debug")]
        return Ok(debug_elements);
    };

    let (previous_idx, idx) = shell_guard.workspaces.active_num(output);
    let previous_workspace = previous_workspace
        .zip(previous_idx)
        .map(|((w, start), idx)| (w.handle, idx, start));
    let workspace = (workspace.handle, idx);

    std::mem::drop(shell_guard);

    let element_filter = if workspace_overview_is_open(output) {
        ElementFilter::LayerShellOnly
    } else {
        ElementFilter::All
    };
    let zoom_state = shell.read().zoom_state().cloned();

    #[allow(unused_mut)]
    let workspace_elements = workspace_elements(
        _gpu,
        renderer,
        shell,
        zoom_state.as_ref(),
        now,
        output,
        previous_workspace,
        workspace,
        cursor_mode,
        element_filter,
        scanout_node,
    )?;

    #[cfg(feature = "debug")]
    {
        debug_elements.extend(workspace_elements);
        Ok(debug_elements)
    }
    #[cfg(not(feature = "debug"))]
    Ok(workspace_elements)
}

#[profiling::function]
pub fn workspace_elements<R>(
    _gpu: Option<&DrmNode>,
    renderer: &mut R,
    shell: &Arc<parking_lot::RwLock<Shell>>,
    zoom_level: Option<&ZoomState>,
    now: Time<Monotonic>,
    output: &Output,
    previous: Option<(WorkspaceHandle, usize, WorkspaceDelta)>,
    current: (WorkspaceHandle, usize),
    cursor_mode: CursorMode,
    element_filter: ElementFilter,
    scanout_node: Option<DrmNode>,
) -> Result<Vec<CosmicElement<R>>, RenderError<R::Error>>
where
    R: AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let _ws_span = tracing::debug_span!(
        "workspace_elements",
        output = %output.name(),
    )
    .entered();
    let ws_start = std::time::Instant::now();
    let mut elements = Vec::<CosmicElement<R>>::new();

    let shell_ref = shell.read();
    let seats = shell_ref.seats.iter().cloned().collect::<Vec<_>>();
    if seats.is_empty() {
        return Ok(Vec::new());
    }
    let theme = shell_ref.theme().clone();
    // Upstream derives this from a frosted-glass boolean, which reaches only two
    // of the available steps. Ours comes from the blur_intensity config value so
    // the strength is actually adjustable, with the theme flag as the floor.
    let blur_strength =
        wayland::blur_effect::configured_blur_strength(theme.header_backdrop_blur());
    let scale = output.current_scale().fractional_scale();

    // Gather embedded children for any move-grabbed window before dropping shell lock
    // This allows us to render embedded windows following the dragged parent
    let embedded_children_for_grabbed: Vec<(
        CosmicMapped,
        crate::wayland::handlers::surface_embed::EmbedRenderInfo,
    )> = {
        // Check if any seat has an active move grab and get the parent's surface_id
        seats
            .iter()
            .find_map(|seat| {
                seat.user_data()
                    .get::<SeatMoveGrabState>()
                    .and_then(|state| state.lock().ok())
                    .and_then(|state| {
                        state.as_ref().and_then(|s| {
                            s.element()
                                .active_window()
                                .wl_surface()
                                .map(|surf| surf.id().to_string())
                        })
                    })
            })
            .map(|parent_surface_id| {
                // Get all embedded children for this parent (by surface_id)
                let children =
                    crate::wayland::handlers::surface_embed::get_children_for_parent_by_surface_id(
                        &parent_surface_id,
                    );
                children
                    .into_iter()
                    .filter_map(|(child_surface_id, embed_info)| {
                        // Find the CosmicMapped for this embedded child (by surface_id)
                        shell_ref
                            .workspaces
                            .spaces()
                            .flat_map(|s| s.mapped())
                            .find(|mapped| {
                                mapped
                                    .active_window()
                                    .wl_surface()
                                    .map(|s| s.id().to_string() == child_surface_id)
                                    .unwrap_or(false)
                            })
                            .map(|mapped| (mapped.clone(), embed_info))
                    })
                    .collect()
            })
            .unwrap_or_default()
    };

    let grabbed_orb_state: Option<voice_orb::VoiceOrbState> =
        if shell_ref.voice_orb_state.should_render_at_window_level() {
            Some(shell_ref.voice_orb_state.clone())
        } else {
            None
        };

    // we don't want to hold a shell lock across `cursor_elements`,
    // that is prone to deadlock with the main-thread on some grabs.
    std::mem::drop(shell_ref);

    cursor_elements(
        renderer,
        seats.iter(),
        zoom_level,
        &theme,
        blur_strength,
        now,
        output,
        cursor_mode,
        element_filter == ElementFilter::ExcludeWorkspaceOverview,
        &embedded_children_for_grabbed,
        grabbed_orb_state.as_ref(),
        scanout_node,
        &mut |elem| elements.push(elem),
    );

    // Render voice orb - either globally (floating) or defer to window level (attached)
    let attached_orb_state: Option<voice_orb::VoiceOrbState>;
    {
        let shell_guard = shell.read();
        let output_geo = output.geometry().as_logical();

        // Check if orb should render at window level (attached mode in burst phase)
        if shell_guard.voice_orb_state.should_render_at_window_level() {
            // Don't render globally - will be rendered at window level
            attached_orb_state = Some(shell_guard.voice_orb_state.clone());
        } else {
            // Render globally (floating or not in burst phase yet)
            attached_orb_state = None;

            let target_output = shell_guard.voice_orb_state.target_output.as_deref();
            let should_render_here = target_output.map(|t| t == output.name()).unwrap_or(false);

            if should_render_here
                && let Some(orb_element) = voice_orb::VoiceOrbShader::element(
                    renderer,
                    &shell_guard.voice_orb_state,
                    output_geo,
                )
            {
                elements.push(orb_element.into());
            }
        }

        // Performance badge (top-left) during an F12 capture or F11 cold-start.
        if (crate::perf::is_capturing() || crate::perf::is_coldstart())
            && let Some(badge) = shell_guard.perf_badge.as_ref()
        {
            perf_badge::render(badge, renderer, output, &mut |elem| {
                elements.push(CosmicElement::from(elem))
            });
        }
        // Stress phase: force a full-output re-composite every frame (invisible)
        // so the compositor renders the whole scene flat-out and the report
        // measures the maximum sustainable frame rate.
        if crate::perf::is_stressing() {
            elements.push(DamageElement::new(output_geo).into());
        }
    }

    let shell = shell.read();
    let overview = shell.overview_mode();
    let (resize_mode, resize_indicator) = shell.resize_mode();
    let resize_indicator = resize_indicator.map(|indicator| (resize_mode, indicator));
    let swap_tree = if let Some(Trigger::KeyboardSwap(_, desc)) = overview.0.active_trigger() {
        if current.0 != desc.handle {
            shell
                .workspaces
                .space_for_handle(&desc.handle)
                .map(|w| w.tiling_layer.tree())
        } else {
            None
        }
    } else {
        None
    };
    let overview = (
        overview.0,
        overview.1.map(|indicator| (indicator, swap_tree)),
    );
    let last_active_seat = shell.seats.last_active();
    let move_active = last_active_seat
        .user_data()
        .get::<SeatMoveGrabState>()
        .unwrap()
        .lock()
        .unwrap()
        .is_some();
    let focused_output = last_active_seat.focused_or_active_output();
    let set = shell.workspaces.sets.get(output).ok_or(OutputNoMode)?;
    let workspace = set
        .workspaces
        .iter()
        .find(|w| w.handle == current.0)
        .ok_or(OutputNoMode)?;
    let is_active_space = workspace.output == focused_output;
    let active_hint = if shell.active_hint {
        theme.active_hint as u8
    } else {
        0
    };

    let output_size = output
        .geometry()
        .size
        .as_logical()
        .to_physical_precise_round(scale);
    let (focal_point, zoom_scale) = zoom_level
        .map(|state| {
            (
                state.animating_focal_point(Some(output)).to_local(output),
                state.animating_level(output),
            )
        })
        .unwrap_or_else(|| ((0., 0.).into(), 1.));

    let crop_to_output = |element: WorkspaceRenderElement<R>| {
        CropRenderElement::from_element(
            RescaleRenderElement::from_element(
                element,
                focal_point
                    .as_logical()
                    .to_physical(output.current_scale().fractional_scale())
                    .to_i32_round(),
                zoom_scale,
            ),
            scale,
            Rectangle::from_size(output_size),
        )
    };

    // Get voice mode window alpha for fading windows during voice mode
    let voice_mode_alpha = shell.voice_mode_window_alpha();

    // Fade factors for closing layer POPUPS (e.g. tooltips hidden via the
    // visibility protocol): 1.0→0.0 while fading, then 0.0 once parked in
    // `hidden_surfaces`. Applied to the popup's content and shadow below so they
    // fade out together instead of the surface vanishing instantly.
    let popup_fade_alphas = shell.layer_fade_out_alphas();
    // Opening counterpart of the above. Collected but never read after the
    // element-collection rework, so layer popups appeared fully formed instead
    // of fading in.
    let popup_fade_in_alphas = shell.layer_fade_in_alphas();
    let popup_hidden = shell.hidden_surfaces().clone();

    render_input_order::<()>(&shell, output, previous, current, element_filter, |stage| {
        match stage {
            Stage::ZoomUI => {
                ZoomState::render(renderer, output, &mut |elem| {
                    elements.push(CosmicElement::Zoom(elem))
                });
            }
            Stage::SessionLock(lock_surface) => {
                session_lock_elements(renderer, output, lock_surface, blur_strength, &mut |elem| {
                    elements.extend(crop_to_output(elem.into()).map(Into::into))
                })
            }
            Stage::LayerPopup {
                popup,
                location,
                workspace_idx,
                ..
            } => {
                let popup_wl_surface = popup.wl_surface();
                let popup_geo = popup.geometry();

                // Fade the whole popup (content + shadow) when it's closing via the
                // visibility protocol; 1.0 when not closing.
                // MERGE: the blur backdrop is no longer faded here — upstream's
                // frosted glass rides the surface's own alpha.
                // A surface is in at most one of the two maps: fading out takes
                // precedence, so a popup dismissed mid-open fades from where it
                // got to rather than jumping back to its opening alpha.
                let popup_alpha = popup_fade_alphas
                    .get(&popup_wl_surface.id())
                    .or_else(|| popup_fade_in_alphas.get(&popup_wl_surface.id()))
                    .copied()
                    .unwrap_or_else(|| {
                        if popup_hidden.contains(&popup_wl_surface.id()) {
                            0.0
                        } else {
                            1.0
                        }
                    });

                let mut geometry = popup_geo.as_global();
                geometry.loc += location;

                let radii = with_states(popup_wl_surface, |states| {
                    surface_corners(states, geometry.size.as_logical())
                })
                .unwrap_or([0; 4]);

                let namespace = output
                    .user_data()
                    .get::<OutputId>()
                    .map(|id| id.namespace_for_workspace(workspace_idx))
                    .unwrap_or(workspace_idx);

                // Render the popup surface content (upstream also emits the frosted
                // glass backdrop for it from here).
                push_render_elements_from_surface_tree(
                    renderer,
                    popup_wl_surface,
                    location
                        .to_local(output)
                        .as_logical()
                        .to_physical_precise_round(scale),
                    geometry.to_local(output).as_logical().to_f64(),
                    Scale::from(scale),
                    popup_alpha,
                    false,
                    radii,
                    None,
                    blur_strength,
                    FRAME_TIME_FILTER,
                    &mut |elem| {
                        elements.extend(
                            crop_to_output(NamespacedElement::new(elem, namespace).into())
                                .map(Into::into),
                        )
                    },
                    None,
                );

                let local_geo =
                    Rectangle::new(location.to_local(output), popup_geo.size.as_local());

                // Get corner radius from the popup surface
                let corner_radius = get_surface_corner_radius(popup_wl_surface, popup_geo.size);

                // Render shadow behind the popup if enabled via protocol
                let popup_surface_id = popup_wl_surface.id();
                if surface_has_shadow(popup_wl_surface) {
                    let shadow_layers = theme.shadow_window();
                    let shadow_radius = corner_radius.map(|r| r.round() as u8);

                    if let Some(shadow) = shadow_layers.first() {
                        let shadow_color = [
                            shadow.color.r,
                            shadow.color.g,
                            shadow.color.b,
                            shadow.color.a,
                        ];
                        let shadow_offset = [shadow.offset.x, shadow.offset.y];
                        let shadow_softness = shadow.blur_radius;

                        let shadow_element = ShadowShader::layer_element(
                            renderer,
                            &popup_surface_id,
                            local_geo,
                            shadow_radius,
                            popup_alpha,
                            scale,
                            shadow_color,
                            shadow_offset,
                            shadow_softness,
                        );

                        let shadow: WorkspaceRenderElement<R> =
                            Into::<CosmicMappedRenderElement<R>>::into(shadow_element).into();
                        if let Some(cropped) = crop_to_output(shadow) {
                            elements.push(cropped.into());
                        }
                    }
                }
            }
            Stage::LayerSurface {
                layer,
                location,
                workspace_idx,
                alpha,
                // MERGE: `blur_alpha` (our layer-blur fade) is no longer read here —
                // upstream's frosted glass rides the surface alpha.
                ..
            } => {
                // Apply auto-hide render offset.
                // Input hit-testing is also offset (in surface_under)
                // so hidden surfaces don't intercept clicks.
                let surface_id = layer.wl_surface().id();
                let layer_geo = layer.bbox();

                let (offset_x, offset_y) =
                    shell.get_auto_hide_offset(layer.wl_surface(), layer_geo.size.h);
                // Also apply layer slide offset (side-panel visibility animation).
                let (slide_x, slide_y) = shell.get_layer_slide_offset(&surface_id);
                // And the interactive-resize offset, which pins the anchored edge to
                // the output while the client's buffer catches up to the new width.
                let (resize_x, resize_y) =
                    shell.get_layer_resize_offset(&surface_id, layer_geo.size.w);
                // And the bottom-edge pin: keeps a bottom-anchored auto-size surface's
                // bottom glued to its arranged position while its buffer height catches
                // up to a just-requested grow/shrink (e.g. expanding the Wi-Fi list in
                // the settings popover), so the content doesn't jump-then-settle.
                let (pin_x, pin_y) =
                    shell.get_layer_bottom_pin_offset(layer.wl_surface(), layer_geo.size.h);
                // And the popover open/close animation translate + scale. The
                // open slides UP (+6px → 0) + scales 0.97→1.0; the close is the
                // exact reverse (0 → +6px, 1.0→0.97). Only ONE is ever active for
                // a given surface; both fold into the same scale render path.
                // All of (translate, scale, alpha) come from one eased factor.
                let is_opening = shell.is_layer_opening(&surface_id);
                let is_closing = shell.is_layer_closing(&surface_id);
                let (anim_x, anim_y, anim_scale) = if is_opening {
                    let (x, y) = shell.get_layer_open_offset(&surface_id);
                    (x, y, shell.get_layer_open_scale(&surface_id))
                } else if is_closing {
                    let (x, y) = shell.get_layer_close_offset(&surface_id);
                    (x, y, shell.get_layer_close_scale(&surface_id))
                } else {
                    (0, 0, 1.0)
                };
                // Whether to route through the center-scale render path this frame.
                let is_scaling = is_opening || is_closing;
                // One line per rendered frame while a surface animates, so a
                // "it just pops in" report can be told apart from "the state
                // machine never ran": no lines means the animation state never
                // reached the renderer, few lines spread over the duration means
                // the redraw loop is starving it.
                if is_scaling || alpha < 1.0 {
                    tracing::trace!(
                        surface_protocol_id = surface_id.protocol_id(),
                        ns = %layer.namespace(),
                        alpha = format!("{:.3}", alpha),
                        is_opening,
                        is_closing,
                        anim_scale = format!("{:.3}", anim_scale),
                        anim_y,
                        // Size of the buffer being faded. A fade over a surface
                        // that has no real content yet looks exactly like no
                        // fade at all, so these have to be read together: a
                        // size that only reaches its final value at the end of
                        // the ramp means the client, not the animation.
                        geo_w = layer_geo.size.w,
                        geo_h = layer_geo.size.h,
                        "layer_anim: rendering layer surface mid-animation"
                    );
                }
                let total_offset_x = offset_x + slide_x + anim_x + resize_x + pin_x;
                let total_offset_y = offset_y + slide_y + anim_y + resize_y + pin_y;
                let render_location = if total_offset_x != 0 || total_offset_y != 0 {
                    location + smithay::utils::Point::from((total_offset_x, total_offset_y))
                } else {
                    location
                };

                // Compute the physical location for surface rendering
                let mut surface_render_phys_loc: Point<i32, Physical> = render_location
                    .to_local(output)
                    .as_logical()
                    .to_physical_precise_round(scale);

                // Sub-pixel bottom pin (fractional-scale fix): a bottom-anchored
                // surface's physical top and physical height are rounded
                // independently, so their sum — the bottom edge — can wobble ±1px as
                // the surface resizes (an auto-size grow/shrink animation), because
                // round(top·s) + round(h·s) != round((top+h)·s). Pin the physical
                // BOTTOM to the rounded arranged bottom (constant through a resize)
                // and derive the top from the buffer's physical height, so the bottom
                // never jitters. Skipped while the surface animates its own position
                // (slide / open / close), which moves it on purpose.
                //
                // `get_layer_bottom_pin_offset` already aligned the LOGICAL bottom to
                // the configured size via `pin_y`; this removes the residual
                // sub-physical-pixel rounding a logical offset can't reach.
                if !is_scaling
                    && !shell.is_layer_sliding(&surface_id)
                    && shell.is_layer_bottom_anchored(layer.wl_surface())
                {
                    let top_local = render_location.to_local(output).as_logical().y as f64;
                    let buf_h = layer_geo.size.h as f64;
                    let bottom_phys = ((top_local + buf_h) * scale).round() as i32;
                    let buf_h_phys = (buf_h * scale).round() as i32;
                    surface_render_phys_loc.y = bottom_phys - buf_h_phys;
                }

                let local_geo =
                    Rectangle::new(render_location.to_local(output), layer_geo.size.as_local());

                // Upstream's frosted-glass pipeline needs the surface's logical
                // geometry, its padded (visible) rect and its corner radii; all are
                // taken from the OFFSET geometry so the backdrop tracks our
                // auto-hide / slide / resize / pin animations.
                let geometry = local_geo.as_logical();
                let padded = with_states(layer.wl_surface(), |states| {
                    surface_padding(states, geometry.size)
                        .and_then(|padding| pad_rect(geometry, &padding))
                })
                .unwrap_or(geometry);
                let radii = with_states(layer.wl_surface(), |states| {
                    surface_corners(states, padded.size)
                })
                .unwrap_or([0; 4]);

                // Namespacing keeps element ids unique when the same layer surface is
                // rendered for more than one workspace during a switch animation.
                let namespace = output
                    .user_data()
                    .get::<OutputId>()
                    .map(|id| id.namespace_for_workspace(workspace_idx))
                    .unwrap_or(workspace_idx);

                // Compositor-drawn edge resize sash for surfaces opted
                // into the layer_edge_resize protocol: a themed full-height bar at the
                // resting outer edge on hover, or at the dragged ghost edge while a
                // drag is in progress (the panel itself only resizes, animated, on
                // release). Pushed BEFORE the surface content so it draws on top, and
                // only output-cropped so the drag ghost can float over the desktop.
                if let Some(indicator) = shell.get_layer_edge_indicator(&surface_id) {
                    use crate::shell::EdgeIndicator;
                    let (edge_x, dragging) = match indicator {
                        EdgeIndicator::Hover { anchor_right } => {
                            let x = if anchor_right {
                                local_geo.loc.x
                            } else {
                                local_geo.loc.x + local_geo.size.w
                            };
                            (x, false)
                        }
                        EdgeIndicator::Drag {
                            anchor_right,
                            ghost_width,
                        } => {
                            // Ghost edge measured from the anchored (fixed) edge.
                            let x = if anchor_right {
                                local_geo.loc.x + local_geo.size.w - ghost_width
                            } else {
                                local_geo.loc.x + ghost_width
                            };
                            (x, true)
                        }
                    };
                    let bar_w: i32 = if dragging { 4 } else { 2 };
                    let mut bar_geo = local_geo;
                    bar_geo.loc.x = edge_x - bar_w / 2;
                    bar_geo.size.w = bar_w;
                    let c = theme.primary();
                    let bar_alpha = alpha * if dragging { 1.0 } else { 0.6 };
                    let bar_element = BackdropShader::element(
                        renderer,
                        Key::LayerSurface(surface_id.protocol_id()),
                        bar_geo,
                        [bar_w as f32 / 2.0; 4],
                        bar_alpha,
                        [c.r, c.g, c.b],
                    );
                    let bar: WorkspaceRenderElement<R> =
                        Into::<CosmicMappedRenderElement<R>>::into(bar_element).into();
                    if let Some(cropped) = crop_to_output(bar) {
                        elements.push(cropped.into());
                    }
                }

                // Focal point for the open animation's scale: the CENTER of the
                // surface in PHYSICAL coords. Scaling around a corner instead would
                // make the popover lunge sideways, so we must use the center.
                let open_origin_phys: Point<i32, Physical> = {
                    let center_local =
                        local_geo.loc + Point::from((local_geo.size.w / 2, local_geo.size.h / 2));
                    center_local.as_logical().to_physical_precise_round(scale)
                };

                // Neighbor squish (e.g. agentos-panel bottom bar tracking a side
                // panel slide). Computed once so both the surface content AND its
                // shadow scale by the same factor about the left edge.
                let neighbor_scale = shell
                    .get_layer_slide_neighbor_scale(
                        output,
                        layer.wl_surface(),
                        local_geo.loc.x,
                        layer_geo.size.w,
                    )
                    .filter(|s| (*s - 1.0).abs() > 0.0005);

                if is_scaling {
                    // OPENING or CLOSING: render the surface tree as bare Wayland
                    // elements (alpha already baked in via `alpha`), then wrap each
                    // in a RescaleRenderElement about the surface CENTER so it
                    // scales in place (open 0.97→1.0, close 1.0→0.97), and route
                    // through GrabbedWindow which carries exactly
                    // RescaleRenderElement<CosmicWindowRenderElement>.
                    push_render_elements_from_surface_tree(
                        renderer,
                        layer.wl_surface(),
                        surface_render_phys_loc,
                        geometry.to_f64(),
                        Scale::from(scale),
                        alpha,
                        false,
                        radii,
                        padded.to_f64(),
                        blur_strength,
                        FRAME_TIME_FILTER,
                        &mut |surf_elem| {
                            let win_elem: CosmicWindowRenderElement<R> = surf_elem.into();
                            let scaled = RescaleRenderElement::from_element(
                                win_elem,
                                open_origin_phys,
                                anim_scale as f64,
                            );
                            let mapped: CosmicMappedRenderElement<R> =
                                CosmicMappedRenderElement::GrabbedWindow(scaled);
                            if let Some(cropped) = crop_to_output(mapped.into()) {
                                elements.push(cropped.into());
                            }
                        },
                        None,
                    );
                } else if let Some(neighbor_scale) = neighbor_scale {
                    // NEIGHBOR SQUISH: a full-width bar (e.g. agentos-panel) being
                    // shrunk by an active side-panel slide. Scale its committed
                    // buffer about its FIXED left edge so its right edge tracks the
                    // panel's animated edge (pixel-locked via cached_factor),
                    // masking the bar client's reflow lag — the same squish-to-fit
                    // windows get. Mirrors the open/close scale wrap above, x-only.
                    push_render_elements_from_surface_tree(
                        renderer,
                        layer.wl_surface(),
                        surface_render_phys_loc,
                        geometry.to_f64(),
                        Scale::from(scale),
                        alpha,
                        false,
                        radii,
                        padded.to_f64(),
                        blur_strength,
                        FRAME_TIME_FILTER,
                        &mut |surf_elem| {
                            let win_elem: CosmicWindowRenderElement<R> = surf_elem.into();
                            let scaled = RescaleRenderElement::from_element(
                                win_elem,
                                surface_render_phys_loc,
                                Scale {
                                    x: neighbor_scale,
                                    y: 1.0,
                                },
                            );
                            let mapped: CosmicMappedRenderElement<R> =
                                CosmicMappedRenderElement::GrabbedWindow(scaled);
                            if let Some(cropped) = crop_to_output(mapped.into()) {
                                elements.push(cropped.into());
                            }
                        },
                        None,
                    );
                } else {
                    // First render the layer surface content (plus upstream's frosted
                    // glass backdrop, emitted from the same traversal).
                    let is_sliding = shell.is_layer_sliding(&surface_id);
                    let mut pre_crop_count = 0usize;
                    let elements_before = elements.len();
                    push_render_elements_from_surface_tree(
                        renderer,
                        layer.wl_surface(),
                        surface_render_phys_loc,
                        geometry.to_f64(),
                        Scale::from(scale),
                        alpha,
                        false,
                        radii,
                        padded.to_f64(),
                        blur_strength,
                        FRAME_TIME_FILTER,
                        &mut |elem| {
                            pre_crop_count += 1;
                            elements.extend(
                                crop_to_output(NamespacedElement::new(elem, namespace).into())
                                    .map(Into::into),
                            )
                        },
                        None,
                    );
                    if is_sliding {
                        let added = elements.len() - elements_before;
                        tracing::trace!(
                            pre_crop_count,
                            post_crop_count = added,
                            "layer_slide: element counts after crop"
                        );
                    }
                }

                // While opening OR closing, scale the shadow rect by the SAME factor
                // around the SAME center so the shadow tracks the surface instead of
                // staying full-size behind a scaled card. For a neighbor squish, scale
                // the shadow WIDTH about the left edge by the same x-factor so its
                // right edge tracks the squished content's right edge.
                let local_geo = if is_scaling && anim_scale != 1.0 {
                    scale_rect_about_center(local_geo, anim_scale)
                } else if let Some(ns) = neighbor_scale {
                    let mut g = local_geo;
                    g.size.w = (local_geo.size.w as f64 * ns).round() as i32;
                    g
                } else {
                    local_geo
                };

                // Get corner radius from the surface (same as windows)
                let corner_radius = get_surface_corner_radius(layer.wl_surface(), layer_geo.size);

                // Render shadow behind the layer surface if enabled
                // Alpha handles fading for home visibility surfaces.
                // Layer-shell popovers/glances use `shadow_popup` (a lighter
                // overlay shadow); it defaults to `shadow_window`, so themes that
                // don't override it are unaffected.
                if surface_has_shadow(layer.wl_surface()) {
                    let shadow_layers = theme.shadow_popup();
                    let shadow_radius = corner_radius.map(|r| r.round() as u8);

                    if let Some(shadow) = shadow_layers.first() {
                        let shadow_color = [
                            shadow.color.r,
                            shadow.color.g,
                            shadow.color.b,
                            shadow.color.a,
                        ];
                        let shadow_offset = [shadow.offset.x, shadow.offset.y];
                        let shadow_softness = shadow.blur_radius;

                        let shadow_element = ShadowShader::layer_element(
                            renderer,
                            &surface_id,
                            local_geo,
                            shadow_radius,
                            alpha,
                            scale,
                            shadow_color,
                            shadow_offset,
                            shadow_softness,
                        );

                        let shadow: WorkspaceRenderElement<R> =
                            Into::<CosmicMappedRenderElement<R>>::into(shadow_element).into();
                        if let Some(cropped) = crop_to_output(shadow) {
                            elements.push(cropped.into());
                        }
                    }
                }

                // MERGE: our cached-texture layer blur backdrop (per-region rects,
                // tint/saturation/border uniforms) is dropped — upstream emits the
                // frosted glass from `push_render_elements_from_surface_tree` above.
            }
            Stage::OverlaySurface { surface } => {
                // Composite the game-mode overlay (launcher / client overlay) at
                // the output origin, above the game. The surface carries its own
                // per-pixel alpha; scanout is forced off so it blends over the
                // game rather than being scanned out opaquely.
                let mut n_elements = 0usize;
                surface.push_render_elements(
                    renderer,
                    Point::default(),
                    Scale::from(scale),
                    1.0,
                    Some(false),
                    scanout_node,
                    false,
                    [0; 4],
                    blur_strength,
                    &mut |elem| {
                        if let Some(cropped) = crop_to_output(elem.into()) {
                            n_elements += 1;
                            elements.push(cropped.into());
                        }
                    },
                    None,
                );
                // Bug 2: a resolved overlay surface with no committed buffer yields
                // ZERO elements — the QAM stage runs but nothing is drawn over the game.
                tracing::trace!(
                    target: crate::logger::GAMING_TARGET,
                    overlay_app_id = %surface.app_id(),
                    n_elements,
                    "overlay stage composed"
                );
            }
            Stage::OverrideRedirect { surface, location } => {
                if let Some(wl_surface) = surface.wl_surface() {
                    let mut geometry = surface.geometry().as_global();
                    geometry.loc += location;

                    push_render_elements_from_surface_tree(
                        renderer,
                        &wl_surface,
                        location
                            .to_local(output)
                            .as_logical()
                            .to_physical_precise_round(scale),
                        geometry.to_local(output).as_logical().to_f64(),
                        Scale::from(scale),
                        1.0,
                        false,
                        [0; 4],
                        None,
                        blur_strength,
                        FRAME_TIME_FILTER,
                        &mut |elem| elements.extend(crop_to_output(elem.into()).map(Into::into)),
                        None,
                    );
                }
            }
            Stage::StickyPopups(layout) => {
                let alpha = match &overview.0 {
                    OverviewMode::Started(_, started) => {
                        (1.0 - (Instant::now().duration_since(*started).as_millis()
                            / theme.motion.animation.as_millis())
                            as f32)
                            .max(0.0)
                            * 0.4
                            + 0.6
                    }
                    OverviewMode::Ended(_, ended) => {
                        ((Instant::now().duration_since(*ended).as_millis()
                            / theme.motion.animation.as_millis()) as f32)
                            * 0.4
                            + 0.6
                    }
                    OverviewMode::Active(_) => 0.6,
                    OverviewMode::None => 1.0,
                };

                layout.render_popups(renderer, alpha, scanout_node, &mut |elem| {
                    if let Some(elem) = crop_to_output(elem.into()) {
                        elements.push(elem.into())
                    }
                });
            }
            Stage::Sticky(layout) => {
                let alpha = match &overview.0 {
                    OverviewMode::Started(_, started) => {
                        (1.0 - (Instant::now().duration_since(*started).as_millis()
                            / theme.motion.animation.as_millis())
                            as f32)
                            .max(0.0)
                            * 0.4
                            + 0.6
                    }
                    OverviewMode::Ended(_, ended) => {
                        ((Instant::now().duration_since(*ended).as_millis()
                            / theme.motion.animation.as_millis()) as f32)
                            * 0.4
                            + 0.6
                    }
                    OverviewMode::Active(_) => 0.6,
                    OverviewMode::None => 1.0,
                };

                let current_focus = (!move_active && is_active_space)
                    .then_some(last_active_seat)
                    .map(|seat| workspace.focus_stack.get(seat));

                layout.render(
                    renderer,
                    current_focus.as_ref().and_then(|stack| {
                        stack.last().and_then(|t| match t {
                            FocusTarget::Window(w) => Some(w),
                            _ => None,
                        })
                    }),
                    resize_indicator.clone(),
                    active_hint,
                    alpha,
                    &theme,
                    None, // No attached orb for sticky layer
                    scanout_node,
                    &mut |elem| {
                        if let Some(elem) = crop_to_output(elem.into()) {
                            elements.push(elem.into())
                        }
                    },
                );
            }
            Stage::WorkspacePopups {
                workspace,
                offset,
                game_mode_only,
            } => {
                workspace.render_popups(
                    renderer,
                    last_active_seat,
                    !move_active && is_active_space,
                    overview.clone(),
                    &theme,
                    scanout_node,
                    game_mode_only,
                    &mut |elem| {
                        if let Some(elem) = crop_to_output(elem) {
                            elements.push(CosmicElement::Workspace(
                                RelocateRenderElement::from_element(
                                    elem,
                                    offset.to_physical_precise_round(scale),
                                    Relocate::Relative,
                                ),
                            ));
                        }
                    },
                );
            }
            Stage::Workspace {
                workspace,
                offset,
                alpha,
                game_mode_only,
            } => {
                // Multiply the workspace-transition opacity (1.0 except
                // during a crossfade) into the existing window-alpha slot.
                let effective_alpha = voice_mode_alpha * alpha;

                // Voice mode holds windows at exactly 0.0 for the orb's entire
                // life (`VoiceMode::window_alpha`: WaitingForOrbGrow | Active |
                // WaitingForOrbShrink). Compositing them anyway costs a
                // full-screen pass per window for zero pixels — and because a
                // translucent surface reports no opaque region
                // (`is_likely_translucent`), nothing in the scene occludes
                // anything, so the wallpaper and every window below are drawn
                // in full too. Skipping the workspace restores real opaque
                // regions for what remains.
                //
                // The attached orb is rendered *inside* `workspace.render`, so
                // never skip while it is live at window level.
                //
                // Clients keep their per-frame callbacks across this: see the
                // matching zero-throttle in `Common::send_frames`.
                if effective_alpha > 0.0 || attached_orb_state.is_some() {
                    workspace.render(
                        renderer,
                        last_active_seat,
                        !move_active && is_active_space,
                        overview.clone(),
                        resize_indicator.clone(),
                        active_hint,
                        &theme,
                        effective_alpha,
                        attached_orb_state.as_ref(),
                        scanout_node,
                        game_mode_only,
                        &mut |elem| {
                            if let Some(elem) = crop_to_output(elem) {
                                elements.push(CosmicElement::Workspace(
                                    RelocateRenderElement::from_element(
                                        elem,
                                        offset.to_physical_precise_round(scale),
                                        Relocate::Relative,
                                    ),
                                ));
                            }
                        },
                    );
                }
            }
        };

        ControlFlow::Continue(())
    })?;

    let ws_elapsed = ws_start.elapsed();
    // Only log at debug level when workspace_elements takes a long time (>2ms)
    if ws_elapsed.as_micros() > 2000 {
        tracing::trace!(
            output = %output.name(),
            element_count = elements.len(),
            duration_us = ws_elapsed.as_micros() as u64,
            "workspace_elements SLOW composition"
        );
    } else {
        tracing::trace!(
            output = %output.name(),
            element_count = elements.len(),
            duration_us = ws_elapsed.as_micros() as u64,
            "workspace_elements composed"
        );
    }

    Ok(elements)
}

/// Scale a logical-`Local` rectangle by `scale` about its own center, so a
/// shadow rect tracks a surface being rendered with the same scale around the
/// same center (used by the popover open animation).
fn scale_rect_about_center(rect: Rectangle<i32, Local>, scale: f32) -> Rectangle<i32, Local> {
    // Use the SAME integer floor-division center as the GPU focal point
    // (`open_origin_phys` in the LayerSurface stage), which is computed as
    // `local_geo.loc + (size.w / 2, size.h / 2)`. A float center here would
    // drift ~0.5px from that on odd surface dimensions, mismatching the
    // shadow rect against the scaled surface content during the open.
    let cx = (rect.loc.x + rect.size.w / 2) as f32;
    let cy = (rect.loc.y + rect.size.h / 2) as f32;
    let new_w = rect.size.w as f32 * scale;
    let new_h = rect.size.h as f32 * scale;
    let new_x = (cx - new_w / 2.0).round() as i32;
    let new_y = (cy - new_h / 2.0).round() as i32;
    Rectangle::new(
        Point::from((new_x, new_y)),
        Size::from((new_w.round() as i32, new_h.round() as i32)),
    )
}

fn session_lock_elements<R>(
    renderer: &mut R,
    output: &Output,
    lock_surface: Option<&LockSurface>,
    blur_strength: usize,
    push: &mut dyn FnMut(SurfaceRenderElement<R>),
) where
    R: AsGlowRenderer,
    R::TextureId: Clone + 'static,
{
    if let Some(surface) = lock_surface {
        let scale = Scale::from(output.current_scale().fractional_scale());
        push_render_elements_from_surface_tree(
            renderer,
            surface.wl_surface(),
            (0, 0),
            bbox_from_surface_tree(surface.wl_surface(), (0, 0)).to_f64(),
            scale,
            1.0,
            false,
            [0; 4],
            None,
            blur_strength,
            FRAME_TIME_FILTER,
            push,
            None,
        )
    }
}

/// Map a scanout target's DRM fourcc to a format the GLES renderer can actually
/// allocate an offscreen texture/renderbuffer for.
///
/// `Offscreen::create_buffer` picks a GL internal format via smithay's
/// `fourcc_to_gl_formats`, which only knows the RGBA-ordered (`Abgr*`) variants
/// (`Abgr8888`, `Abgr2101010`, `Abgr16161616f`) plus their opaque `Xbgr*` twins.
/// Several KMS drivers scan out 10-bit panels in the BGRA-ordered `Argb2101010`/
/// `Xrgb2101010` (AR30/XR30) — observed on Qualcomm `msm`/Adreno (10-bit OLED)
/// and AMD — and that fourcc has no GLES-renderable internal format, so feeding
/// the raw scanout fourcc into `create_buffer` hard-errors with
/// `UnsupportedPixelFormat(AR30)` — freezing every frame while a screen filter
/// (night shift / invert / color filter) or blur is active.
///
/// The offscreen buffer is a GPU-internal scratch texture: it is rendered into,
/// sampled by the postprocess/blur shader, and drawn to the real scanout target
/// (which binds the AR30 dmabuf via EGLImage and works fine). It is never read
/// back to the CPU nor exported as a dmabuf, so its channel-order *name* is
/// irrelevant — only the bit depth matters. We therefore remap to the
/// byte-order-swapped `Abgr` variant of the same depth, preserving 10-bit / 16f
/// precision, and fall back to 8-bit `Abgr8888` for anything unrecognized.
///
/// Applied at the offscreen-allocation chokepoints (`PostprocessState::new_with_renderer`,
/// `PostprocessState::track_cursor`) so every
/// caller — the winit/x11 nested backend (`target.format()`) and the KMS backend
/// (`compositor.format()`) alike — is covered without wrapping each call site.
fn offscreen_render_format(target_format: Fourcc) -> Fourcc {
    match target_format {
        Fourcc::Argb2101010 | Fourcc::Xrgb2101010 | Fourcc::Abgr2101010 | Fourcc::Xbgr2101010 => {
            Fourcc::Abgr2101010
        }
        Fourcc::Argb16161616f
        | Fourcc::Xrgb16161616f
        | Fourcc::Abgr16161616f
        | Fourcc::Xbgr16161616f => Fourcc::Abgr16161616f,
        _ => Fourcc::Abgr8888,
    }
}

// Used for mirroring and postprocessing
#[derive(Debug)]
pub struct PostprocessState {
    pub texture: TextureRenderBuffer<GlesTexture>,
    pub damage_tracker: OutputDamageTracker,
    pub cursor_texture: Option<TextureRenderBuffer<GlesTexture>>,
    pub cursor_damage_tracker: Option<OutputDamageTracker>,
    pub output_config: PostprocessOutputConfig,
}

impl PostprocessState {
    pub fn new_with_renderer<R: AsGlowRenderer>(
        renderer: &mut R,
        format: Fourcc,
        output_config: PostprocessOutputConfig,
    ) -> Result<Self, R::Error> {
        let size = output_config.size;
        let buffer_size = size.to_logical(1).to_buffer(1, Transform::Normal);
        let opaque_regions = vec![Rectangle::from_size(buffer_size)];

        // A GLES offscreen can't be allocated in a BGRA-ordered 10-bit scanout
        // format (AR30/XR30) — remap to the renderable equivalent so a 10-bit
        // primary plane doesn't hard-fail postprocess allocation (which would
        // freeze the output when a screen filter such as night shift is active).
        let format = offscreen_render_format(format);
        let texture = Offscreen::<GlesTexture>::create_buffer(renderer, format, buffer_size)?;
        let texture_buffer = TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            texture,
            1,
            Transform::Normal,
            Some(opaque_regions),
        );

        // Don't use `from_output` to avoid applying output transform
        let damage_tracker =
            OutputDamageTracker::new(size, output_config.fractional_scale, Transform::Normal);

        Ok(PostprocessState {
            texture: texture_buffer,
            damage_tracker,
            cursor_texture: None,
            cursor_damage_tracker: None,
            output_config,
        })
    }

    pub fn track_cursor<R: AsGlowRenderer>(
        &mut self,
        renderer: &mut R,
        format: Fourcc,
        size: Size<i32, Physical>,
        scale: Scale<f64>,
    ) -> Result<(), R::Error> {
        let format = offscreen_render_format(format);
        let buffer_size = size.to_logical(1).to_buffer(1, Transform::Normal);

        if let (Some(tex), Some(tracker)) = (
            self.cursor_texture.as_ref(),
            self.cursor_damage_tracker.as_ref(),
        ) && tex.format().is_some_and(|f| f == format)
            && tracker.mode()
                == &(OutputModeSource::Static {
                    size,
                    scale,
                    transform: Transform::Normal,
                })
        {
            return Ok(());
        }

        let texture = Offscreen::<GlesTexture>::create_buffer(renderer, format, buffer_size)?;

        let texture_buffer = TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            texture,
            1,
            Transform::Normal,
            None,
        );

        let damage_tracker = OutputDamageTracker::new(size, scale, Transform::Normal);

        self.cursor_texture = Some(texture_buffer);
        self.cursor_damage_tracker = Some(damage_tracker);

        Ok(())
    }

    pub fn remove_cursor(&mut self) {
        self.cursor_texture.take();
        self.cursor_damage_tracker.take();
    }
}

#[derive(Debug, PartialEq)]
pub struct PostprocessOutputConfig {
    pub size: Size<i32, Physical>,
    pub fractional_scale: f64,
}

impl PostprocessOutputConfig {
    pub fn for_output_untransformed(output: &Output) -> Self {
        Self {
            // Apply inverse of output transform to mode size to get correct size
            // for an untransformed render.
            size: output.current_transform().invert().transform_size(
                output
                    .current_mode()
                    .map(|mode| mode.size)
                    .unwrap_or_default(),
            ),
            fractional_scale: output.current_scale().fractional_scale(),
        }
    }

    pub fn for_output(output: &Output) -> Self {
        Self {
            size: output
                .current_mode()
                .map(|mode| mode.size)
                .unwrap_or_default(),
            fractional_scale: output.current_scale().fractional_scale(),
        }
    }
}

#[derive(Debug, Default)]
pub struct ScreenFilterStorage {
    pub filter: ScreenFilter,
    pub state: Option<PostprocessState>,
}

#[profiling::function]
pub fn render_output<'d, R>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    target: &mut R::Framebuffer<'_>,
    damage_tracker: &'d mut OutputDamageTracker,
    age: usize,
    shell: &Arc<parking_lot::RwLock<Shell>>,
    now: Time<Monotonic>,
    output: &Output,
    cursor_mode: CursorMode,
    screen_filter: &'d mut ScreenFilterStorage,
    loop_handle: &calloop::LoopHandle<'static, State>,
) -> Result<RenderOutputResult<'d>, RenderError<R::Error>>
where
    R: AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    CosmicElement<R>: RenderElement<R>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    // MERGE: the whole per-window / per-layer blur capture + cache pre-pass that
    // used to run here is gone. Upstream's frosted glass blits the region under
    // each element straight out of the live framebuffer, so there is no capture
    // render, no `BlurRenderState` and no throttling/hashing bookkeeping.
    let shell_ref = shell.read();
    let (previous_workspace, workspace) = shell_ref
        .workspaces
        .active(output)
        .ok_or(RenderError::OutputNoMode(OutputNoMode))?;
    let (previous_idx, idx) = shell_ref.workspaces.active_num(output);
    let previous_workspace = previous_workspace
        .zip(previous_idx)
        .map(|((w, start), idx)| (w.handle, idx, start));
    let workspace = (workspace.handle, idx);
    let zoom_state = shell_ref.zoom_state().cloned();
    std::mem::drop(shell_ref);

    let element_filter = if workspace_overview_is_open(output) {
        ElementFilter::LayerShellOnly
    } else {
        ElementFilter::All
    };

    let mut postprocess_texture = None;
    let result = if !screen_filter.filter.is_noop() {
        if screen_filter.state.as_ref().is_none_or(|state| {
            state.output_config != PostprocessOutputConfig::for_output_untransformed(output)
        }) {
            screen_filter.state = Some(
                PostprocessState::new_with_renderer(
                    renderer,
                    target.format().unwrap_or(Fourcc::Abgr8888),
                    PostprocessOutputConfig::for_output_untransformed(output),
                )
                .map_err(RenderError::Rendering)?,
            );
        }

        let state = screen_filter.state.as_mut().unwrap();
        let mut result = Err(RenderError::OutputNoMode(OutputNoMode));
        state
            .texture
            .render()
            .draw::<_, RenderError<R::Error>>(|tex| {
                let mut target = renderer.bind(tex).map_err(RenderError::Rendering)?;
                result = render_workspace(
                    gpu,
                    renderer,
                    &mut target,
                    &mut state.damage_tracker,
                    1,
                    None,
                    shell,
                    zoom_state.as_ref(),
                    now,
                    output,
                    previous_workspace,
                    workspace,
                    cursor_mode,
                    element_filter,
                );
                std::mem::drop(target);
                postprocess_texture = Some(tex.clone());

                Ok(if let Ok((res, _)) = result.as_ref() {
                    renderer.wait(&res.sync).map_err(RenderError::Rendering)?;
                    let transform = output.current_transform();
                    let area = tex.size().to_logical(1, transform);

                    res.damage
                        .cloned()
                        .map(|v| {
                            v.into_iter()
                                .map(|r| r.to_logical(1).to_buffer(1, transform, &area))
                                .collect::<Vec<_>>()
                        })
                        .unwrap_or_default()
                } else {
                    Vec::new()
                })
            })?;

        if result.is_ok() {
            let texture_elem = TextureRenderElement::from_texture_render_buffer(
                (0., 0.),
                &state.texture,
                Some(1.0),
                None,
                None,
                Kind::Unspecified,
            );

            let postprocess_texture_shader = renderer
                .glow_renderer_mut()
                .egl_context()
                .user_data()
                .get::<PostprocessShader>()
                .expect("OffscreenShader should be available through `init_shaders`");
            let texture_geometry =
                texture_elem.geometry(output.current_scale().fractional_scale().into());
            let elements = {
                let texture_elem = TextureShaderElement::new(
                    texture_elem,
                    postprocess_texture_shader.0.clone(),
                    vec![
                        Uniform::new(
                            "invert",
                            if screen_filter.filter.inverted {
                                1.
                            } else {
                                0.
                            },
                        ),
                        Uniform::new(
                            "color_mode",
                            screen_filter
                                .filter
                                .color_filter
                                .map(|val| val as u8 as f32)
                                .unwrap_or(0.),
                        ),
                        Uniform::new("night_shift", f32::from(screen_filter.filter.night_shift)),
                    ],
                );
                constrain_render_elements(
                    std::iter::once(texture_elem),
                    (0, 0),
                    Rectangle::from_size(
                        output
                            .geometry()
                            .size
                            .as_logical()
                            .to_f64()
                            .to_physical(output.current_scale().fractional_scale())
                            .to_i32_round(),
                    ),
                    texture_geometry,
                    ConstrainScaleBehavior::Fit,
                    ConstrainAlign::CENTER,
                    1.0,
                )
                .map(CosmicElement::Postprocess)
                .collect::<Vec<_>>()
            };

            damage_tracker.render_output(renderer, target, age, &elements, CLEAR_COLOR)?;
        }

        result
    } else {
        render_workspace(
            gpu,
            renderer,
            target,
            damage_tracker,
            age,
            None,
            shell,
            zoom_state.as_ref(),
            now,
            output,
            previous_workspace,
            workspace,
            cursor_mode,
            element_filter,
        )
    };

    match result {
        Ok((res, mut elements)) => {
            for (session, frame) in output.take_pending_frames() {
                if let Some(pending_image_copy_data) = render_session(
                    renderer,
                    session.user_data().get::<SessionData>().unwrap(),
                    frame,
                    output.current_transform(),
                    |buffer, renderer, offscreen, dt, age, additional_damage| {
                        let old_len = if !additional_damage.is_empty() {
                            let area = output
                                .current_mode()
                                .ok_or(RenderError::OutputNoMode(OutputNoMode))
                                .map(
                                    |mode| {
                                        mode.size
                                            .to_logical(1)
                                            .to_buffer(1, Transform::Normal)
                                            .to_f64()
                                    }, /* TODO: Mode is Buffer..., why is this Physical in the first place */
                                )?;

                            let old_len = elements.len();
                            let additional_damage_elements: Vec<_> = additional_damage
                                .into_iter()
                                .map(|rect| {
                                    rect.to_f64()
                                        .to_logical(
                                            output.current_scale().fractional_scale(),
                                            output.current_transform(),
                                            &area,
                                        )
                                        .to_i32_round()
                                })
                                .map(DamageElement::new)
                                .collect();
                            dt.damage_output(age, &additional_damage_elements)?;

                            Some(old_len)
                        } else {
                            None
                        };

                        let res = dt.damage_output(age, &elements)?;

                        if let Some(old_len) = old_len {
                            elements.truncate(old_len);
                        }

                        let mut sync = SyncPoint::default();

                        if let (Some(damage), _) = &res {
                            // TODO: On Vulkan, may need to combine sync points instead of just using latest?
                            let blit_to_buffer =
                                |renderer: &mut R, blit_from: &mut R::Framebuffer<'_>| {
                                    if let Ok(dmabuf) = get_dmabuf(buffer) {
                                        let mut dmabuf_clone = dmabuf.clone();
                                        let mut fb = renderer.bind(&mut dmabuf_clone)?;
                                        for rect in damage.iter() {
                                            sync = renderer.blit(
                                                blit_from,
                                                &mut fb,
                                                *rect,
                                                *rect,
                                                TextureFilter::Nearest,
                                            )?;
                                        }
                                    } else {
                                        let fb = offscreen
                                            .expect("shm buffers should have offscreen target");
                                        for rect in damage.iter() {
                                            sync = renderer.blit(
                                                blit_from,
                                                fb,
                                                *rect,
                                                *rect,
                                                TextureFilter::Nearest,
                                            )?;
                                        }
                                    }

                                    Result::<_, R::Error>::Ok(())
                                };

                            // we would want to just assign a different framebuffer to a variable, depending on the code-path,
                            // but then rustc tries to equate the lifetime of target with the lifetime of our temporary fb...
                            // So instead of duplicating all the code, we use a closure..
                            if let Some(tex) = postprocess_texture.as_mut() {
                                let mut fb = renderer.bind(tex).map_err(RenderError::Rendering)?;
                                blit_to_buffer(renderer, &mut fb)
                                    .map_err(RenderError::Rendering)?;
                            } else {
                                blit_to_buffer(renderer, target).map_err(RenderError::Rendering)?;
                            }
                        }

                        let buffers = render_element_buffers(renderer, &elements);

                        Ok((
                            RenderOutputResult {
                                damage: res.0,
                                sync,
                                states: res.1,
                            },
                            buffers,
                        ))
                    },
                )? {
                    pending_image_copy_data.send_success_when_ready(
                        output.current_transform(),
                        loop_handle,
                        now,
                    );
                }
            }

            Ok(res)
        }
        Err(err) => Err(err),
    }
}

#[profiling::function]
pub fn render_workspace<'d, R>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    target: &mut R::Framebuffer<'_>,
    damage_tracker: &'d mut OutputDamageTracker,
    age: usize,
    additional_damage: Option<Vec<Rectangle<i32, Logical>>>,
    shell: &Arc<parking_lot::RwLock<Shell>>,
    zoom_level: Option<&ZoomState>,
    now: Time<Monotonic>,
    output: &Output,
    previous: Option<(WorkspaceHandle, usize, WorkspaceDelta)>,
    current: (WorkspaceHandle, usize),
    cursor_mode: CursorMode,
    element_filter: ElementFilter,
) -> Result<(RenderOutputResult<'d>, Vec<CosmicElement<R>>), RenderError<R::Error>>
where
    R: AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    CosmicElement<R>: RenderElement<R>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let mut elements: Vec<CosmicElement<R>> = if let Some(additional_damage) = additional_damage {
        let output_geo = output.geometry().to_local(output).as_logical();
        additional_damage
            .into_iter()
            .filter_map(|rect| rect.intersection(output_geo))
            .map(DamageElement::new)
            .map(CosmicElement::from)
            .collect()
    } else {
        Vec::new()
    };

    elements.extend(workspace_elements(
        gpu,
        renderer,
        shell,
        zoom_level,
        now,
        output,
        previous,
        current,
        cursor_mode,
        element_filter,
        None,
    )?);

    let res = damage_tracker.render_output(
        renderer,
        target,
        age,
        &elements,
        CLEAR_COLOR, // TODO use a theme neutral color
    );

    res.map(|res| (res, elements))
}
