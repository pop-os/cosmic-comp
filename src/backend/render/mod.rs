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
            clipped_surface::{CLIPPING_SHADER, ClippingShader},
            element::DamageElement,
            shadow::{SHADOW_SHADER, ShadowShader},
        },
    },
    config::ScreenFilter,
    shell::{
        CosmicMapped, CosmicMappedRenderElement, OverviewMode, SeatExt, Trigger, WorkspaceDelta,
        WorkspaceRenderElement,
        element::{CosmicMappedKey, window::CosmicWindowRenderElement},
        focus::{FocusTarget, Stage, render_input_order, target::WindowGroup},
        grabs::{SeatMenuGrabState, SeatMoveGrabState},
        zoom::ZoomState,
    },
    utils::{prelude::*, quirks::workspace_overview_is_open},
    wayland::{
        handlers::{
            compositor::FRAME_TIME_FILTER,
            data_device::get_dnd_icon,
            image_copy_capture::{FrameHolder, SessionData, render_session},
        },
        protocols::{
            blur::{
                get_blur_border, get_blur_saturation, get_blur_state, get_blur_tint,
                has_blur as surface_has_blur,
            },
            corner_radius::get_surface_corner_radius,
            layer_shadow::surface_has_shadow,
            workspace::WorkspaceHandle,
        },
    },
};

use crate::comp_theme::CompTheme;
use element::FromGlesError;
use smithay::{
    backend::{
        allocator::Fourcc,
        drm::{DrmDeviceFd, DrmNode},
        renderer::{
            Color32F, ImportAll, Offscreen, Renderer, Texture, TextureFilter,
            damage::{Error as RenderError, OutputDamageTracker, RenderOutputResult},
            element::{
                Element, Id, Kind, RenderElement, WeakId,
                surface::{WaylandSurfaceRenderElement, render_elements_from_surface_tree},
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
    input::Seat,
    output::{Output, OutputModeSource, OutputNoMode},
    reexports::wayland_server::Resource,
    utils::{
        IsAlive, Logical, Monotonic, Physical, Point, Rectangle, Scale, Size, Time, Transform,
    },
    wayland::{dmabuf::get_dmabuf, seat::WaylandFocus, session_lock::LockSurface},
};

#[cfg(feature = "debug")]
use smithay_egui::EguiState;

pub mod animations;
pub mod blur;
pub mod clipped_surface;

pub mod cursor;
pub mod element;
pub mod gpu_profiler;
pub mod perf_badge;
pub mod shadow;
pub mod voice_orb;
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
/// Clear color for blur *source* captures (the offscreen we render the below-layer
/// scene into before blurring). Must be fully TRANSPARENT, not `CLEAR_COLOR`: an
/// empty or partial-coverage capture then contributes nothing (the shader's
/// `final_alpha = blurred.a * …` makes zero-alpha regions invisible) so the client
/// glass shows the real content behind it, instead of caching opaque charcoal that
/// reads as a flat grey/black card. Whole-output frame clears stay `CLEAR_COLOR`.
pub static BLUR_CAPTURE_CLEAR_COLOR: Color32F = Color32F::new(0.0, 0.0, 0.0, 0.0);
pub static OUTLINE_SHADER: &str = include_str!("./shaders/rounded_outline.frag");
pub static RECTANGLE_SHADER: &str = include_str!("./shaders/rounded_rectangle.frag");
pub static POSTPROCESS_SHADER: &str = include_str!("./shaders/offscreen.frag");
pub static DUAL_KAWASE_DOWNSAMPLE_SHADER: &str =
    include_str!("./shaders/dual_kawase_downsample.frag");
pub static DUAL_KAWASE_UPSAMPLE_SHADER: &str = include_str!("./shaders/dual_kawase_upsample.frag");
pub static BLURRED_BACKDROP_SHADER: &str = include_str!("./shaders/blurred_backdrop.frag");
pub static GROUP_COLOR: [f32; 3] = [0.788, 0.788, 0.788];
pub static ACTIVE_GROUP_COLOR: [f32; 3] = [0.58, 0.922, 0.922];

// Blur module re-exports
pub use blur::{
    BLUR_BORDER_STRENGTH, BLUR_DOWNSAMPLE_FACTOR, BLUR_FALLBACK_ALPHA, BLUR_FALLBACK_COLOR,
    BLUR_TINT_COLOR, BLUR_TINT_STRENGTH, BlurCaptureContext, BlurRenderState, BlurredTextureInfo,
    CachedLayerSurface, HasBlur, LayerBlurSurfaceInfo, apply_dual_kawase_blur,
    blur_downsample_enabled, cache_blur_texture_for_layer, cache_blur_texture_for_window,
    clear_blur_textures_for_output, clear_cached_layer_surfaces,
    clear_layer_blur_textures_for_output, compute_element_content_hash,
    copy_blur_texture_for_cache, downsample_texture, get_blur_group_content_hash,
    get_cached_blur_texture_for_layer, get_cached_blur_texture_for_window,
    get_cached_layer_surfaces, get_layer_blur_content_hash, get_layer_blur_surfaces,
    output_has_layer_blur, set_cached_layer_surfaces, set_layer_blur_surfaces,
    should_throttle_blur, should_throttle_layer_blur, store_blur_group_content_hash,
    store_blur_group_last_update, store_layer_blur_content_hash, store_layer_blur_last_update,
};

/// Dual Kawase downsample shader (blur + halve resolution)
pub struct DualKawaseDownsampleShader(pub GlesTexProgram);

/// Dual Kawase upsample shader (blur + double resolution)
pub struct DualKawaseUpsampleShader(pub GlesTexProgram);

/// Shader for rendering blurred backdrop (samples from pre-blurred texture)
pub struct BlurredBackdropShader(pub GlesTexProgram);

pub struct IndicatorShader(pub GlesPixelProgram);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Usage {
    OverviewBackdrop,
    Overlay,
    BlurBackdrop,
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
            let thickness: f32 = ((thickness as f64 * scale).ceil() / scale) as f32;
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

impl DualKawaseDownsampleShader {
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesTexProgram {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<DualKawaseDownsampleShader>()
            .expect("Custom Shaders not initialized")
            .0
            .clone()
    }
}

impl DualKawaseUpsampleShader {
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesTexProgram {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<DualKawaseUpsampleShader>()
            .expect("Custom Shaders not initialized")
            .0
            .clone()
    }
}

impl BlurredBackdropShader {
    /// Get the blurred backdrop shader program
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesTexProgram {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<BlurredBackdropShader>()
            .expect("BlurredBackdropShader not initialized")
            .0
            .clone()
    }

    /// Create a blurred backdrop element from a pre-blurred texture
    ///
    /// This crops the correct region from the blurred background texture,
    /// applies a tint overlay, and masks with rounded corners.
    ///
    /// Supports downsampled blur textures - coordinates are scaled from
    /// screen_size to texture_size automatically.
    ///
    /// # Arguments
    /// * `renderer` - The renderer to use
    /// * `blurred_texture` - The pre-blurred background texture (may be downsampled)
    /// * `element_geo` - The geometry of the element (where the backdrop appears)
    /// * `texture_size` - The actual size of the blur texture (may be smaller than screen)
    /// * `screen_size` - The full screen size for coordinate mapping
    /// * `scale` - Output scale factor for coordinate conversion
    /// * `transform` - Output transform for coordinate conversion
    /// * `corner_radius` - Corner radii [top-left, top-right, bottom-right, bottom-left]
    /// * `alpha` - Overall opacity
    /// * `tint_color` - Tint overlay color
    /// * `tint_strength` - How much tint to apply (0.0 = none, 1.0 = full)
    /// * `border_enabled` - Whether to draw a 1px white border (for layer shells)
    pub fn element<R: AsGlowRenderer>(
        renderer: &R,
        blurred_texture: &TextureRenderBuffer<GlesTexture>,
        element_geo: Rectangle<i32, Local>,
        texture_size: Size<i32, Physical>,
        screen_size: Size<i32, Physical>,
        scale: f64,
        transform: Transform,
        corner_radius: [f32; 4],
        alpha: f32,
        tint_color: [f32; 3],
        tint_strength: f32,
        border_enabled: bool,
        saturation: f32,
        border_strength: f32,
    ) -> TextureShaderElement {
        use crate::utils::geometry::RectLocalExt;

        let shader = Self::get(renderer);

        // Convert from Local to Logical, then to Physical coordinates
        // The blur texture is captured in physical pixels, so we need physical coordinates
        // for the src_rect to crop the correct region
        let phys_geo: Rectangle<i32, Physical> =
            element_geo.as_logical().to_physical_precise_round(scale);

        // Position the element at the physical location (scaled from logical)
        // This is critical for HiDPI: element_geo is in logical coords, but rendering
        // happens in physical coords
        let location: Point<f64, Physical> = (phys_geo.loc.x as f64, phys_geo.loc.y as f64).into();

        // If transform is not Normal, we need to transform the coordinates
        // Transform affects how coordinates map to the buffer
        let transformed_phys_geo = if transform != Transform::Normal {
            // Transform the location within the screen bounds
            let transformed_loc = transform.transform_point_in(phys_geo.loc, &screen_size);
            let transformed_size = transform.transform_size(phys_geo.size);
            Rectangle::new(transformed_loc, transformed_size)
        } else {
            phys_geo
        };

        // Calculate scale factor for downsampled texture
        // If texture is smaller than screen, we need to scale coordinates
        let scale_x = texture_size.w as f64 / screen_size.w as f64;
        let scale_y = texture_size.h as f64 / screen_size.h as f64;

        // Calculate src_rect in texture coordinates (scaled from screen coordinates)
        // The blur texture may be downsampled, so we scale the crop region
        let src_rect = Rectangle::<f64, Logical>::new(
            (
                transformed_phys_geo.loc.x as f64 * scale_x,
                transformed_phys_geo.loc.y as f64 * scale_y,
            )
                .into(),
            (
                transformed_phys_geo.size.w as f64 * scale_x,
                transformed_phys_geo.size.h as f64 * scale_y,
            )
                .into(),
        );

        // Output size should be in logical coordinates - this is the size the element
        // appears on screen. The src_rect crops from physical pixels, but output_size
        // specifies the logical output dimensions.
        let output_size = Size::<i32, Logical>::from((element_geo.size.w, element_geo.size.h));

        // IMPORTANT: Pass None for alpha here - the shader handles alpha via uniform.
        // Passing Some(alpha) would cause Smithay to pre-multiply alpha before the
        // shader runs, resulting in double-application of alpha.
        let source_elem = TextureRenderElement::from_texture_render_buffer(
            location,
            blurred_texture,
            None,
            Some(src_rect),
            Some(output_size),
            Kind::Unspecified,
        );

        // Scale corner radii to physical pixels
        let scaled_corner_radius = [
            corner_radius[0] * scale as f32,
            corner_radius[1] * scale as f32,
            corner_radius[2] * scale as f32,
            corner_radius[3] * scale as f32,
        ];

        TextureShaderElement::new(
            source_elem,
            shader,
            vec![
                Uniform::new("blur_opacity", alpha),
                // Use physical size for shader calculations (corner radius, etc.)
                // The shader operates in physical pixel space
                Uniform::new("size", [phys_geo.size.w as f32, phys_geo.size.h as f32]),
                Uniform::new("screen_size", [screen_size.w as f32, screen_size.h as f32]),
                // Use physical position for shader calculations
                Uniform::new(
                    "element_pos",
                    [phys_geo.loc.x as f32, phys_geo.loc.y as f32],
                ),
                // Order: [top-left, top-right, bottom-right, bottom-left]
                Uniform::new("corner_radius_tl", scaled_corner_radius[0]),
                Uniform::new("corner_radius_tr", scaled_corner_radius[1]),
                Uniform::new("corner_radius_br", scaled_corner_radius[2]),
                Uniform::new("corner_radius_bl", scaled_corner_radius[3]),
                Uniform::new("tint_color", tint_color),
                Uniform::new("tint_strength", tint_strength),
                Uniform::new(
                    "border_enabled",
                    if border_enabled { 1.0f32 } else { 0.0f32 },
                ),
                Uniform::new("saturation", saturation),
                Uniform::new("border_strength", border_strength),
            ],
        )
    }
}

pub fn init_shaders(renderer: &mut GlesRenderer) -> Result<(), GlesError> {
    {
        let egl_context = renderer.egl_context();
        if egl_context.user_data().get::<IndicatorShader>().is_some()
            && egl_context.user_data().get::<BackdropShader>().is_some()
            && egl_context.user_data().get::<PostprocessShader>().is_some()
            && egl_context
                .user_data()
                .get::<BlurredBackdropShader>()
                .is_some()
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
            UniformName::new("scale", UniformType::_1f),
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
    let dual_kawase_downsample_shader = renderer.compile_custom_texture_shader(
        DUAL_KAWASE_DOWNSAMPLE_SHADER,
        &[
            UniformName::new("half_texel", UniformType::_2f),
            UniformName::new("offset", UniformType::_1f),
        ],
    )?;
    let dual_kawase_upsample_shader = renderer.compile_custom_texture_shader(
        DUAL_KAWASE_UPSAMPLE_SHADER,
        &[
            UniformName::new("half_texel", UniformType::_2f),
            UniformName::new("offset", UniformType::_1f),
        ],
    )?;
    let blurred_backdrop_shader = renderer.compile_custom_texture_shader(
        BLURRED_BACKDROP_SHADER,
        &[
            UniformName::new("blur_opacity", UniformType::_1f),
            UniformName::new("size", UniformType::_2f),
            UniformName::new("screen_size", UniformType::_2f),
            UniformName::new("element_pos", UniformType::_2f),
            UniformName::new("corner_radius_tl", UniformType::_1f),
            UniformName::new("corner_radius_tr", UniformType::_1f),
            UniformName::new("corner_radius_br", UniformType::_1f),
            UniformName::new("corner_radius_bl", UniformType::_1f),
            UniformName::new("tint_color", UniformType::_3f),
            UniformName::new("tint_strength", UniformType::_1f),
            UniformName::new("border_enabled", UniformType::_1f),
            UniformName::new("saturation", UniformType::_1f),
            UniformName::new("border_strength", UniformType::_1f),
        ],
    )?;

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
        .insert_if_missing(|| DualKawaseDownsampleShader(dual_kawase_downsample_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| DualKawaseUpsampleShader(dual_kawase_upsample_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| BlurredBackdropShader(blurred_backdrop_shader));

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
    now: Time<Monotonic>,
    output: &Output,
    mode: CursorMode,
    exclude_dnd_icon: bool,
    skip_move_grab: bool,
    embedded_children_for_grabbed: &[(
        CosmicMapped,
        crate::wayland::handlers::surface_embed::EmbedRenderInfo,
    )],
    attached_orb_state: Option<&voice_orb::VoiceOrbState>,
    scanout_node: Option<DrmNode>,
) -> Vec<CosmicElement<R>>
where
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
    let mut elements = Vec::new();

    for seat in seats {
        let pointer = match seat.get_pointer() {
            Some(ptr) => ptr,
            None => continue,
        };
        let location = pointer.current_location() - output.current_location().to_f64();

        if mode != CursorMode::None {
            elements.extend(
                cursor::draw_cursor(
                    renderer,
                    seat,
                    location,
                    scale.into(),
                    zoom_scale,
                    now,
                    mode != CursorMode::NotDefault,
                )
                .into_iter()
                .map(|(elem, hotspot)| {
                    CosmicElement::Cursor(RescaleRenderElement::from_element(
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
                    ))
                }),
            );
        }

        if !exclude_dnd_icon && let Some(dnd_icon) = get_dnd_icon(seat) {
            elements.extend(
                cursor::draw_dnd_icon(
                    renderer,
                    &dnd_icon.surface,
                    (location + dnd_icon.offset.to_f64()).to_i32_round(),
                    scale,
                )
                .into_iter()
                .map(CosmicElement::Dnd),
            );
        }

        // theme is already CompTheme, no inner layer
        // Skip move grab render when capturing for blur - the grabbed window
        // is always on top and shouldn't be included in the blur source
        if !skip_move_grab
            && let Some(grab_elements) = seat
                .user_data()
                .get::<SeatMoveGrabState>()
                .unwrap()
                .lock()
                .unwrap()
                .as_ref()
                .map(|state| {
                    state.render::<CosmicMappedRenderElement<R>, R>(
                        renderer,
                        output,
                        theme,
                        embedded_children_for_grabbed,
                        attached_orb_state,
                        scanout_node,
                    )
                })
        {
            elements.extend(grab_elements.into_iter().map(|elem| {
                CosmicElement::MoveGrab(RescaleRenderElement::from_element(
                    elem,
                    focal_point
                        .as_logical()
                        .to_physical(output.current_scale().fractional_scale())
                        .to_i32_round(),
                    zoom_scale,
                ))
            }));
        }

        if let Some((grab_elements, should_scale)) = seat
            .user_data()
            .get::<SeatMenuGrabState>()
            .unwrap()
            .lock()
            .unwrap()
            .as_ref()
            .map(|state| {
                (
                    state.render::<CosmicMappedRenderElement<R>, R>(renderer, output),
                    !state.is_in_screen_space(),
                )
            })
        {
            elements.extend(grab_elements.into_iter().map(|elem| {
                CosmicElement::MoveGrab(RescaleRenderElement::from_element(
                    elem,
                    if should_scale {
                        focal_point
                            .as_logical()
                            .to_physical(output.current_scale().fractional_scale())
                            .to_i32_round()
                    } else {
                        Point::from((0, 0))
                    },
                    if should_scale { zoom_scale } else { 1.0 },
                ))
            }));
        }
    }

    elements
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

#[derive(Clone, PartialEq)]
pub enum ElementFilter {
    All,
    ExcludeWorkspaceOverview,
    LayerShellOnly,
    /// Exclude windows with blur effect (for capturing background to blur)
    ExcludeBlurWindows,
    /// Blur capture mode with context (replaces thread-local globals)
    BlurCapture(BlurCaptureContext),
    /// Capture only elements below the specified layer (for layer shell blur)
    /// Background layer captures nothing, Bottom captures Background only, etc.
    /// The optional CosmicMappedKey is the grabbed/dragged window to exclude.
    LayerBlurCapture(
        smithay::wayland::shell::wlr_layer::Layer,
        Option<CosmicMappedKey>,
    ),
}

impl ElementFilter {
    /// Get home visibility context if available
    /// Returns None for filters that don't need home visibility (blur captures, etc.)
    pub fn home_visibility(&self) -> Option<&HomeVisibilityContext> {
        // Home visibility is handled separately - not stored in ElementFilter
        // This is a placeholder for when we might embed it
        None
    }
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
    R::Error: FromGlesError,
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
                .map_err(FromGlesError::from_gles_error)
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
        &element_filter,
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
    element_filter: &ElementFilter,
    scanout_node: Option<DrmNode>,
) -> Result<Vec<CosmicElement<R>>, RenderError<R::Error>>
where
    R: AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    R::Error: FromGlesError,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let _ws_span = tracing::debug_span!(
        "workspace_elements",
        output = %output.name(),
    )
    .entered();
    let ws_start = std::time::Instant::now();
    let mut elements = Vec::new();

    let shell_ref = shell.read();
    let seats = shell_ref.seats.iter().cloned().collect::<Vec<_>>();
    if seats.is_empty() {
        return Ok(Vec::new());
    }
    let theme = shell_ref.theme().clone();
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

    // Skip move grab for blur capture modes where windows shouldn't be included
    let skip_move_grab = match element_filter {
        ElementFilter::BlurCapture(_) | ElementFilter::LayerBlurCapture(_, _) => true,
        _ => false,
    };

    elements.extend(cursor_elements(
        renderer,
        seats.iter(),
        zoom_level,
        &theme,
        now,
        output,
        cursor_mode,
        *element_filter == ElementFilter::ExcludeWorkspaceOverview,
        skip_move_grab,
        &embedded_children_for_grabbed,
        grabbed_orb_state.as_ref(),
        scanout_node,
    ));

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
            elements.extend(perf_badge::render::<R, CosmicElement<R>>(
                badge, renderer, output,
            ));
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
    // `hidden_surfaces`. Applied to the popup's content, shadow and blur below so
    // they fade out together instead of the surface vanishing instantly.
    let popup_fade_alphas = shell.layer_fade_out_alphas();
    let popup_hidden = shell.hidden_surfaces().clone();

    render_input_order::<()>(&shell, output, previous, current, element_filter, |stage| {
        match stage {
            Stage::ZoomUI => {
                elements.extend(ZoomState::render(renderer, output));
            }
            Stage::SessionLock(lock_surface) => {
                elements.extend(
                    session_lock_elements(renderer, output, lock_surface)
                        .into_iter()
                        .map(Into::into)
                        .flat_map(crop_to_output)
                        .map(Into::into),
                );
            }
            Stage::LayerPopup {
                popup, location, ..
            } => {
                let popup_wl_surface = popup.wl_surface();
                let popup_geo = popup.geometry();

                // Fade the whole popup (content + shadow + blur) when it's
                // closing via the visibility protocol; 1.0 when not closing.
                let popup_alpha = popup_fade_alphas
                    .get(&popup_wl_surface.id())
                    .copied()
                    .unwrap_or_else(|| {
                        if popup_hidden.contains(&popup_wl_surface.id()) {
                            0.0
                        } else {
                            1.0
                        }
                    });

                // Render the popup surface content
                elements.extend(
                    render_elements_from_surface_tree::<_, WorkspaceRenderElement<_>>(
                        renderer,
                        popup_wl_surface,
                        location
                            .to_local(output)
                            .as_logical()
                            .to_physical_precise_round(scale),
                        Scale::from(scale),
                        popup_alpha,
                        FRAME_TIME_FILTER,
                    )
                    .into_iter()
                    .flat_map(crop_to_output)
                    .map(Into::into),
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

                // Render blur backdrop behind the popup if enabled
                if surface_has_blur(popup_wl_surface) {
                    let output_name = output.name();

                    if let Some(blur_info) =
                        get_cached_blur_texture_for_layer(&output_name, &popup_surface_id)
                    {
                        let output_transform = output.current_transform();

                        // Check for blur regions — if set, render per-region backdrops
                        let blur_regions = get_blur_state(popup_wl_surface)
                            .and_then(|state| state.data)
                            .and_then(|data| data.region);

                        let has_per_region_blur = blur_regions.is_some();
                        let blur_geos: Vec<Rectangle<i32, Local>> =
                            if let Some(regions) = blur_regions {
                                regions
                                    .iter()
                                    .filter_map(|region| {
                                        let geo = Rectangle::new(
                                            (
                                                local_geo.loc.x + region.loc.x,
                                                local_geo.loc.y + region.loc.y,
                                            )
                                                .into(),
                                            region.size.as_local(),
                                        );
                                        // Clamp blur rect to surface bounds
                                        geo.intersection(local_geo)
                                    })
                                    .collect()
                            } else {
                                vec![local_geo]
                            };

                        const PER_REGION_CORNER_RADIUS: f32 = 8.0;

                        for blur_geo in blur_geos {
                            let blur_corner_radius = if has_per_region_blur {
                                [PER_REGION_CORNER_RADIUS; 4]
                            } else {
                                corner_radius
                            };

                            let blurred_element = BlurredBackdropShader::element(
                                renderer,
                                &blur_info.texture,
                                blur_geo,
                                blur_info.size,
                                blur_info.screen_size,
                                blur_info.scale.x,
                                output_transform,
                                blur_corner_radius,
                                popup_alpha,
                                BLUR_TINT_COLOR,
                                get_blur_tint(popup_wl_surface).unwrap_or(BLUR_TINT_STRENGTH),
                                true, // Enable border for popups
                                get_blur_saturation(popup_wl_surface).unwrap_or(1.0),
                                get_blur_border(popup_wl_surface).unwrap_or(BLUR_BORDER_STRENGTH),
                            );

                            let backdrop_element: WorkspaceRenderElement<R> =
                                Into::<CosmicMappedRenderElement<R>>::into(blurred_element).into();
                            if let Some(cropped) = crop_to_output(backdrop_element) {
                                elements.push(cropped.into());
                            }
                        }
                    }
                }
            }
            Stage::LayerSurface {
                layer,
                location,
                alpha,
                blur_alpha,
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
                // shadow/blur backdrop scale by the same factor about the left edge.
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
                    let surface_elements =
                        render_elements_from_surface_tree::<_, WaylandSurfaceRenderElement<_>>(
                            renderer,
                            layer.wl_surface(),
                            surface_render_phys_loc,
                            Scale::from(scale),
                            alpha,
                            FRAME_TIME_FILTER,
                        );
                    elements.extend(
                        surface_elements
                            .into_iter()
                            .map(|surf_elem| {
                                let win_elem: CosmicWindowRenderElement<R> = surf_elem.into();
                                let scaled = RescaleRenderElement::from_element(
                                    win_elem,
                                    open_origin_phys,
                                    anim_scale as f64,
                                );
                                let mapped: CosmicMappedRenderElement<R> =
                                    CosmicMappedRenderElement::GrabbedWindow(scaled);
                                let wre: WorkspaceRenderElement<R> = mapped.into();
                                wre
                            })
                            .flat_map(crop_to_output)
                            .map(Into::into),
                    );
                } else if let Some(neighbor_scale) = neighbor_scale {
                    // NEIGHBOR SQUISH: a full-width bar (e.g. agentos-panel) being
                    // shrunk by an active side-panel slide. Scale its committed
                    // buffer about its FIXED left edge so its right edge tracks the
                    // panel's animated edge (pixel-locked via cached_factor),
                    // masking the bar client's reflow lag — the same squish-to-fit
                    // windows get. Mirrors the open/close scale wrap above, x-only.
                    let surface_elements =
                        render_elements_from_surface_tree::<_, WaylandSurfaceRenderElement<_>>(
                            renderer,
                            layer.wl_surface(),
                            surface_render_phys_loc,
                            Scale::from(scale),
                            alpha,
                            FRAME_TIME_FILTER,
                        );
                    elements.extend(
                        surface_elements
                            .into_iter()
                            .map(|surf_elem| {
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
                                let wre: WorkspaceRenderElement<R> = mapped.into();
                                wre
                            })
                            .flat_map(crop_to_output)
                            .map(Into::into),
                    );
                } else {
                    // First render the layer surface content
                    let surface_elements =
                        render_elements_from_surface_tree::<_, WorkspaceRenderElement<_>>(
                            renderer,
                            layer.wl_surface(),
                            surface_render_phys_loc,
                            Scale::from(scale),
                            alpha,
                            FRAME_TIME_FILTER,
                        );

                    let is_sliding = shell.is_layer_sliding(&surface_id);
                    let pre_crop_count = if is_sliding {
                        surface_elements.len()
                    } else {
                        0
                    };
                    let elements_before = elements.len();
                    elements.extend(
                        surface_elements
                            .into_iter()
                            .flat_map(crop_to_output)
                            .map(Into::into),
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

                // While opening OR closing, scale the shadow/blur rect by the SAME
                // factor around the SAME center so the frosted backdrop and shadow
                // track the surface instead of staying full-size behind a scaled card.
                // For a neighbor squish, scale the backdrop WIDTH about the left edge
                // by the same x-factor so the blur/shadow right edge tracks the
                // squished content's right edge (no backdrop overhang at the seam).
                //
                // Keep the PRE-scale geometry too: per-region (custom-rect) blur
                // positions its rects in this UNSCALED surface space and scales each
                // about the surface center itself (see the blur block below). The
                // single full-surface rect can be scaled about its own center; a
                // region can't, so it must be re-derived from the unscaled frame.
                let unscaled_geo = local_geo;
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

                // Then render blur backdrop behind the layer surface (and shadow)
                // blur_alpha may be more aggressively reduced than surface alpha during fade-in
                if blur_alpha > 0.001 && surface_has_blur(layer.wl_surface()) {
                    let output_name = output.name();

                    // Try to get cached blur texture for this layer surface
                    if let Some(blur_info) =
                        get_cached_blur_texture_for_layer(&output_name, &surface_id)
                    {
                        // Use the blurred backdrop shader with the cached texture
                        let output_transform = output.current_transform();
                        // Disable shader border if the client has set
                        // its own corner radius — it handles its own borders
                        let has_client_corner_radius = corner_radius.iter().any(|&r| r > 0.0);

                        // Check for blur regions — if set, render per-region backdrops
                        let blur_state_debug = get_blur_state(layer.wl_surface());
                        let blur_regions = blur_state_debug
                            .as_ref()
                            .and_then(|state| state.data.as_ref())
                            .and_then(|data| data.region.as_ref());

                        tracing::trace!(
                            surface_id = layer.wl_surface().id().protocol_id(),
                            has_state = blur_state_debug.is_some(),
                            has_data = blur_state_debug
                                .as_ref()
                                .and_then(|s| s.data.as_ref())
                                .is_some(),
                            has_region = blur_regions.is_some(),
                            region_count = blur_regions.map(|r| r.len()).unwrap_or(0),
                            "Layer blur rendering"
                        );

                        let has_per_region_blur = blur_regions.is_some();
                        // During the open/close animation the content scales about
                        // the surface CENTER (integer floor-division, matching
                        // `open_origin_phys`). Each per-region rect lives in the
                        // surface's UNSCALED space, so scale it about that SAME
                        // center; the full-surface rect below instead rides the
                        // already-scaled `local_geo`.
                        let region_scale_cx = (unscaled_geo.loc.x + unscaled_geo.size.w / 2) as f32;
                        let region_scale_cy = (unscaled_geo.loc.y + unscaled_geo.size.h / 2) as f32;
                        let blur_geos: Vec<Rectangle<i32, Local>> =
                            if let Some(regions) = blur_regions {
                                regions
                                    .iter()
                                    .filter_map(|region| {
                                        // While scaling (open/close), place + clamp
                                        // the rect in the UNSCALED surface space, then
                                        // scale it about the surface center so it
                                        // tracks the shrinking/growing card instead of
                                        // keeping its full resting size and clipping.
                                        // Otherwise use the resting / neighbour-scaled
                                        // `local_geo` exactly as before.
                                        let scaling = is_scaling && anim_scale != 1.0;
                                        let base = if scaling { unscaled_geo } else { local_geo };
                                        let geo = Rectangle::new(
                                            (base.loc.x + region.loc.x, base.loc.y + region.loc.y)
                                                .into(),
                                            region.size.as_local(),
                                        );
                                        // Clamp blur rect to surface bounds so
                                        // rects that arrive before a pending resize
                                        // never overflow the current buffer.
                                        let geo = geo.intersection(base)?;
                                        let geo = if scaling {
                                            scale_rect_about_point(
                                                geo,
                                                region_scale_cx,
                                                region_scale_cy,
                                                anim_scale,
                                            )
                                        } else {
                                            geo
                                        };
                                        tracing::trace!(
                                            surface_id = layer.wl_surface().id().protocol_id(),
                                            region_loc_x = region.loc.x,
                                            region_loc_y = region.loc.y,
                                            region_w = region.size.w,
                                            region_h = region.size.h,
                                            final_x = geo.loc.x,
                                            final_y = geo.loc.y,
                                            final_w = geo.size.w,
                                            final_h = geo.size.h,
                                            surface_x = base.loc.x,
                                            surface_y = base.loc.y,
                                            surface_w = base.size.w,
                                            surface_h = base.size.h,
                                            blur_alpha,
                                            "Layer blur: per-region rect"
                                        );
                                        Some(geo)
                                    })
                                    .collect()
                            } else {
                                vec![local_geo]
                            };

                        // Per-region blur falls back to a default card corner
                        // radius since the KDE blur protocol doesn't carry radii.
                        // If the client set its own corner radius (layer corner
                        // radius protocol), honor it so the blur backdrop matches
                        // the client's rounded corners, and let the client draw
                        // its own border.
                        const PER_REGION_CORNER_RADIUS: f32 = 8.0;

                        for blur_geo in blur_geos {
                            let (blur_corner_radius, blur_border) = if has_per_region_blur {
                                if has_client_corner_radius {
                                    (corner_radius, false)
                                } else {
                                    ([PER_REGION_CORNER_RADIUS; 4], true)
                                }
                            } else {
                                (corner_radius, !has_client_corner_radius)
                            };

                            let blurred_element = BlurredBackdropShader::element(
                                renderer,
                                &blur_info.texture,
                                blur_geo,
                                blur_info.size,
                                blur_info.screen_size,
                                blur_info.scale.x,
                                output_transform,
                                blur_corner_radius,
                                blur_alpha,
                                BLUR_TINT_COLOR,
                                get_blur_tint(layer.wl_surface()).unwrap_or(BLUR_TINT_STRENGTH),
                                blur_border,
                                get_blur_saturation(layer.wl_surface()).unwrap_or(1.0),
                                get_blur_border(layer.wl_surface()).unwrap_or(BLUR_BORDER_STRENGTH),
                            );

                            let backdrop_element: WorkspaceRenderElement<R> =
                                Into::<CosmicMappedRenderElement<R>>::into(blurred_element).into();
                            if let Some(cropped) = crop_to_output(backdrop_element) {
                                elements.push(cropped.into());
                            }
                        }
                    }
                }
            }
            Stage::OverlaySurface { surface } => {
                // Composite the game-mode overlay (launcher / client overlay) at
                // the output origin, above the game. The surface carries its own
                // per-pixel alpha; scanout is forced off so it blends over the
                // game rather than being scanned out opaquely.
                elements.extend(
                    surface
                        .render_elements::<R, WorkspaceRenderElement<R>>(
                            renderer,
                            Point::default(),
                            Scale::from(scale),
                            1.0,
                            Some(false),
                            scanout_node,
                        )
                        .into_iter()
                        .flat_map(crop_to_output)
                        .map(Into::into),
                );
            }
            Stage::OverrideRedirect { surface, location } => {
                elements.extend(surface.wl_surface().into_iter().flat_map(|surface| {
                    render_elements_from_surface_tree::<_, WorkspaceRenderElement<_>>(
                        renderer,
                        &surface,
                        location
                            .to_local(output)
                            .as_logical()
                            .to_physical_precise_round(scale),
                        Scale::from(scale),
                        1.0,
                        FRAME_TIME_FILTER,
                    )
                    .into_iter()
                    .flat_map(crop_to_output)
                    .map(Into::into)
                }));
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

                elements.extend(
                    layout
                        .render_popups(renderer, alpha, scanout_node)
                        .into_iter()
                        .map(Into::into)
                        .flat_map(crop_to_output)
                        .map(Into::into),
                );
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

                elements.extend(
                    layout
                        .render(
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
                            element_filter.clone(),
                            None, // No attached orb for sticky layer
                            scanout_node,
                        )
                        .into_iter()
                        .map(Into::into)
                        .flat_map(crop_to_output)
                        .map(Into::into),
                )
            }
            Stage::WorkspacePopups { workspace, offset } => {
                elements.extend(
                    match workspace.render_popups(
                        renderer,
                        last_active_seat,
                        !move_active && is_active_space,
                        overview.clone(),
                        &theme,
                        scanout_node,
                    ) {
                        Ok(elements) => {
                            elements
                                .into_iter()
                                .flat_map(crop_to_output)
                                .map(|element| {
                                    CosmicElement::Workspace(RelocateRenderElement::from_element(
                                        element,
                                        offset.to_physical_precise_round(scale),
                                        Relocate::Relative,
                                    ))
                                })
                        }
                        Err(_) => {
                            return ControlFlow::Break(Err(OutputNoMode));
                        }
                    },
                );
            }
            Stage::Workspace {
                workspace,
                offset,
                alpha,
            } => {
                elements.extend(
                    match workspace.render(
                        renderer,
                        last_active_seat,
                        !move_active && is_active_space,
                        overview.clone(),
                        resize_indicator.clone(),
                        active_hint,
                        &theme,
                        element_filter.clone(),
                        // Multiply the workspace-transition opacity (1.0 except
                        // during a crossfade) into the existing window-alpha slot.
                        voice_mode_alpha * alpha,
                        attached_orb_state.as_ref(),
                        scanout_node,
                    ) {
                        Ok(elements) => {
                            elements
                                .into_iter()
                                .flat_map(crop_to_output)
                                .map(|element| {
                                    CosmicElement::Workspace(RelocateRenderElement::from_element(
                                        element,
                                        offset.to_physical_precise_round(scale),
                                        Relocate::Relative,
                                    ))
                                })
                        }
                        Err(_) => {
                            return ControlFlow::Break(Err(OutputNoMode));
                        }
                    },
                );
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

/// Scale a logical-`Local` rectangle by `scale` about an arbitrary point
/// `(cx, cy)`. Per-region blur rects scale about the SURFACE center (shared by
/// every region of a surface) — not each rect's own center — so the custom-rect
/// backdrop tracks a surface rendered with the same center-scale during the
/// open/close animation. Pass the integer floor-division surface center used by
/// `open_origin_phys` so the blur lands exactly on the scaled content.
fn scale_rect_about_point(
    rect: Rectangle<i32, Local>,
    cx: f32,
    cy: f32,
    scale: f32,
) -> Rectangle<i32, Local> {
    let new_w = (rect.size.w as f32 * scale).round() as i32;
    let new_h = (rect.size.h as f32 * scale).round() as i32;
    let new_x = (cx + (rect.loc.x as f32 - cx) * scale).round() as i32;
    let new_y = (cy + (rect.loc.y as f32 - cy) * scale).round() as i32;
    Rectangle::new(Point::from((new_x, new_y)), Size::from((new_w, new_h)))
}

/// Scale a logical-`Local` rectangle by `scale` about its own center, so a
/// shadow/blur rect tracks a surface being rendered with the same scale around
/// the same center (used by the popover open animation).
fn scale_rect_about_center(rect: Rectangle<i32, Local>, scale: f32) -> Rectangle<i32, Local> {
    // Use the SAME integer floor-division center as the GPU focal point
    // (`open_origin_phys` in the LayerSurface stage), which is computed as
    // `local_geo.loc + (size.w / 2, size.h / 2)`. A float center here would
    // drift ~0.5px from that on odd surface dimensions, mismatching the
    // shadow/blur rect against the scaled surface content during the open.
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
) -> Vec<WaylandSurfaceRenderElement<R>>
where
    R: Renderer + ImportAll,
    R::TextureId: Clone + 'static,
{
    if let Some(surface) = lock_surface {
        let scale = Scale::from(output.current_scale().fractional_scale());
        render_elements_from_surface_tree(
            renderer,
            surface.wl_surface(),
            (0, 0),
            scale,
            1.0,
            FRAME_TIME_FILTER,
        )
    } else {
        Vec::new()
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
/// `PostprocessState::track_cursor`, `BlurRenderState::ensure_textures`) so every
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

/// Render output with blur support
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
    blur_state: &mut BlurRenderState,
) -> Result<RenderOutputResult<'d>, RenderError<R::Error>>
where
    R: AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    R::Error: FromGlesError,
    CosmicElement<R>: RenderElement<R>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let shell_ref = shell.read();
    let (previous_workspace, workspace_ref) = shell_ref
        .workspaces
        .active(output)
        .ok_or(RenderError::OutputNoMode(OutputNoMode))?;
    let (previous_idx, idx) = shell_ref.workspaces.active_num(output);
    let previous_workspace = previous_workspace
        .zip(previous_idx)
        .map(|((w, start), idx)| (w.handle, idx, start));
    let workspace = (workspace_ref.handle, idx);
    let zoom_state = shell_ref.zoom_state().cloned();

    // Check if there are blur windows and get their geometries
    let ssd_blur = shell_ref.theme().header_backdrop_blur();
    let blur_enabled = blur::blur_config_enabled();
    let has_workspace_blur = blur_enabled && workspace_ref.has_blur_windows(ssd_blur);
    let mut blur_geometries = if has_workspace_blur {
        workspace_ref.blur_window_geometries(1.0, ssd_blur)
    } else {
        Vec::new()
    };

    std::mem::drop(shell_ref);

    // Get layer blur surfaces from cache (populated by main thread, safe for render thread)
    use smithay::wayland::shell::wlr_layer::Layer;
    let cached_layer_blur = if blur_enabled {
        get_layer_blur_surfaces(&output.name())
    } else {
        Vec::new()
    };
    let layer_blur_surfaces: Vec<(ObjectId, Rectangle<i32, Local>, Layer, Option<f32>)> =
        cached_layer_blur
            .into_iter()
            .map(|info| {
                let geo = info.geometry.as_local();
                blur_geometries.push((geo, 1.0));
                (info.surface_id, geo, info.layer, info.blur_radius)
            })
            .collect();
    let has_layer_blur = !layer_blur_surfaces.is_empty();

    tracing::trace!(
        output = output.name(),
        layer_blur_count = layer_blur_surfaces.len(),
        has_workspace_blur,
        has_layer_blur,
        "Render frame has layer blur"
    );

    let has_blur = has_workspace_blur || has_layer_blur;

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
    } else if has_blur && !blur_geometries.is_empty() {
        // Iterative multi-pass blur (macOS-style):
        // For each blur window (bottom to top in Z-order):
        //   1. Capture scene up to (but excluding) that window
        //   2. Apply blur and cache per-window texture
        // Then render final scene where each blur window uses its cached texture

        let output_name = output.name();
        tracing::debug!(
            blur_geometry_count = blur_geometries.len(),
            output = %output_name,
            "Entering iterative multi-pass blur path"
        );

        let output_size = output
            .current_mode()
            .ok_or(RenderError::OutputNoMode(OutputNoMode))?
            .size
            .to_logical(1)
            .to_physical_precise_round(output.current_scale().fractional_scale());
        let scale = Scale::from(output.current_scale().fractional_scale());
        let format = target.format().unwrap_or(Fourcc::Abgr8888);

        // Ensure blur textures are allocated
        match blur_state.ensure_textures(renderer, format, output_size, scale) {
            Ok(true) => {} // Textures ready, continue
            Ok(false) => {
                // Allocation was disabled, fall back to render without blur
                return render_workspace(
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
                .map(|(res, _elements)| res);
            }
            Err(err) => {
                // Check if this is a pixel format error (permanent failure)
                let err_str = format!("{:?}", err);
                if err_str.contains("UnsupportedPixelFormat") && !blur_state.allocation_failed {
                    tracing::warn!(
                        ?err,
                        "Failed to allocate blur textures - disabling blur for this output"
                    );
                    blur_state.allocation_failed = true;
                }
                return Err(RenderError::Rendering(err));
            }
        }

        // Get blur windows GROUPED by shared capture requirements
        // Consecutive blur windows (no non-blur windows between them) share a single capture
        let blur_groups = {
            let shell_ref = shell.read();
            let (_, workspace_ref) = shell_ref
                .workspaces
                .active(output)
                .ok_or(RenderError::OutputNoMode(OutputNoMode))?;
            workspace_ref.blur_windows_grouped(1.0)
        };

        let total_windows: usize = blur_groups.iter().map(|g| g.windows.len()).sum();
        tracing::debug!(
            blur_group_count = blur_groups.len(),
            total_blur_windows = total_windows,
            "Processing blur windows in {} groups (optimized from {} captures to {})",
            blur_groups.len(),
            total_windows,
            blur_groups.len()
        );

        // Get the grabbed window key (if any) to exclude from blur capture
        let grabbed_window_key = {
            let shell_ref = shell.read();
            let last_active_seat = shell_ref.seats.last_active();
            last_active_seat
                .user_data()
                .get::<SeatMoveGrabState>()
                .unwrap()
                .lock()
                .unwrap()
                .as_ref()
                .map(|s| s.element().key())
        };

        // Optimized iterative blur: one capture per GROUP, then blur for each window in group
        // OPTIMIZATION: Skip re-blurring if background hasn't changed (no damage + same geometry)
        let mut any_blur_applied = false;
        let window_blur_start = std::time::Instant::now();
        for (group_idx, group) in blur_groups.iter().enumerate() {
            let group_start = std::time::Instant::now();

            // Create blur capture context for this group
            let blur_ctx =
                BlurCaptureContext::new(group.capture_z_threshold, grabbed_window_key.clone());
            let capture_filter = ElementFilter::BlurCapture(blur_ctx);

            // Capture scene elements ONCE for the entire group
            let capture_start = std::time::Instant::now();
            let capture_elements: Vec<CosmicElement<R>> = workspace_elements(
                gpu,
                renderer,
                shell,
                zoom_state.as_ref(),
                now,
                output,
                previous_workspace,
                workspace,
                cursor_mode,
                &capture_filter,
                None,
            )?;
            let capture_elapsed = capture_start.elapsed();

            // An empty capture would clear the background texture to the opaque
            // CLEAR_COLOR and cache flat charcoal (mirrors the layer branch guard).
            if capture_elements.is_empty() {
                tracing::warn!(
                    output = %output_name,
                    group = group_idx,
                    "Empty window blur capture; keeping cached texture"
                );
                any_blur_applied = true;
                continue;
            }

            // Compute content hash for cache invalidation
            // This includes element IDs (which change on content updates) and geometry
            let content_hash =
                compute_element_content_hash(group.capture_z_threshold, &capture_elements, scale);

            // Check if content has changed since last blur
            let stored_hash = get_blur_group_content_hash(&output_name, group.capture_z_threshold);
            let content_changed = stored_hash.is_none() || stored_hash != Some(content_hash);

            // Also verify all windows have cached textures
            let all_cached = group.windows.iter().all(|(window_key, _, _, _)| {
                get_cached_blur_texture_for_window(&output_name, window_key).is_some()
            });

            let can_reuse_cache = !content_changed && all_cached;

            if can_reuse_cache {
                let group_elapsed = group_start.elapsed();
                tracing::trace!(
                    group = group_idx,
                    capture_z_threshold = group.capture_z_threshold,
                    windows_in_group = group.windows.len(),
                    capture_us = capture_elapsed.as_micros(),
                    total_us = group_elapsed.as_micros(),
                    "Skipping blur for group - cache valid (content unchanged)"
                );
                any_blur_applied = true; // Cached textures are still valid
                continue;
            }

            tracing::trace!(
                group = group_idx,
                capture_z_threshold = group.capture_z_threshold,
                windows_in_group = group.windows.len(),
                capture_us = capture_elapsed.as_micros(),
                content_changed = content_changed,
                all_cached = all_cached,
                "Re-blurring group"
            );

            // Render captured elements to background texture (once per group)
            let bg_render_start = std::time::Instant::now();
            let bg_render_ok = if let Some(bg_texture) = blur_state.background_texture.as_mut() {
                let mut blur_dt = OutputDamageTracker::new(output_size, scale, Transform::Normal);

                let render_result = {
                    let mut gles_frame = bg_texture.render();

                    let render_res = gles_frame.draw::<_, RenderError<R::Error>>(|tex| {
                        let bound = renderer.bind(tex).map_err(RenderError::Rendering)?;
                        let mut bound_target = bound;
                        let res = blur_dt.render_output(
                            renderer,
                            &mut bound_target,
                            0, // Full redraw
                            &capture_elements,
                            // Transparent, not CLEAR_COLOR: an empty/partial window
                            // blur capture must blend away, not cache opaque charcoal.
                            BLUR_CAPTURE_CLEAR_COLOR,
                        );
                        match res {
                            Ok(_) => Ok(Vec::new()),
                            Err(e) => Err(e),
                        }
                    });

                    render_res.map_err(|_| ())
                };

                render_result.is_ok()
            } else {
                false
            };
            let bg_render_elapsed = bg_render_start.elapsed();

            // Downsample background to smaller texture for blur passes (if enabled)
            let downsample_start = std::time::Instant::now();
            let downsample_enabled = blur_downsample_enabled();
            let downsample_ok = if downsample_enabled && bg_render_ok {
                if let (Some(bg), Some(ds)) = (
                    blur_state.background_texture.as_ref().cloned(),
                    blur_state.downsampled_texture.as_mut(),
                ) {
                    let blur_size = blur_state.texture_size;
                    downsample_texture(renderer, &bg, ds, output_size, blur_size).is_ok()
                } else {
                    false
                }
            } else {
                !downsample_enabled && bg_render_ok // Skip downsample step when disabled
            };
            let downsample_elapsed = downsample_start.elapsed();

            // Apply Dual Kawase blur (progressive downsample/upsample)
            // Since they share the same background, they share the same blurred result
            let blur_passes_start = std::time::Instant::now();
            if downsample_ok && blur_state.is_ready() {
                let blur_size = blur_state.texture_size;
                // Source texture: downsampled if enabled, background if disabled
                let blur_source = if downsample_enabled {
                    blur_state.downsampled_texture.as_ref().cloned()
                } else {
                    blur_state.background_texture.as_ref().cloned()
                };

                if let (Some(src), Some(ping), Some(pong)) = (
                    blur_source,
                    blur_state.texture_a.as_mut(),
                    blur_state.texture_b.as_mut(),
                ) {
                    // Toplevel windows carry no per-surface blur radius (only layer
                    // surfaces do, via the blur protocol), so window blur always uses
                    // the global config intensity.
                    let blur_result = apply_dual_kawase_blur(
                        renderer,
                        &src,
                        ping,
                        pong,
                        blur_size,
                        blur::effective_blur_levels(),
                        blur::effective_blur_offset(),
                    );

                    if blur_result.is_ok() {
                        // Pass succeeded: this hash now describes the cached texture.
                        store_blur_group_content_hash(
                            &output_name,
                            group.capture_z_threshold,
                            content_hash,
                        );
                        // Cache the SAME blurred texture for ALL windows in this group
                        // Store both blur texture size and original screen size for coordinate mapping
                        for (window_key, geometry, _alpha, z_idx) in &group.windows {
                            cache_blur_texture_for_window(
                                &output_name,
                                window_key,
                                BlurredTextureInfo {
                                    texture: pong.clone(),
                                    size: blur_size,
                                    screen_size: output_size,
                                    scale,
                                    background_state_hash: content_hash,
                                    capture_size: (geometry.size.w, geometry.size.h),
                                },
                            );
                            tracing::trace!(
                                global_z_idx = z_idx,
                                blur_w = blur_size.w,
                                blur_h = blur_size.h,
                                downsampled = downsample_enabled,
                                "Cached blur texture for window in group"
                            );
                        }
                        any_blur_applied = true;
                    } else {
                        tracing::warn!(
                            capture_z_threshold = group.capture_z_threshold,
                            "Blur passes failed for group"
                        );
                    }
                }
            }
            let blur_passes_elapsed = blur_passes_start.elapsed();
            let group_elapsed = group_start.elapsed();

            tracing::trace!(
                group = group_idx,
                windows = group.windows.len(),
                capture_us = capture_elapsed.as_micros(),
                bg_render_us = bg_render_elapsed.as_micros(),
                downsample_us = downsample_elapsed.as_micros(),
                blur_passes_us = blur_passes_elapsed.as_micros(),
                total_us = group_elapsed.as_micros(),
                "Window blur group complete"
            );
        }

        let window_blur_elapsed = window_blur_start.elapsed();
        if !blur_groups.is_empty() {
            tracing::trace!(
                output = %output_name,
                window_blur_groups = blur_groups.len(),
                window_blur_us = window_blur_elapsed.as_micros(),
                "Window blur processing complete"
            );
        }

        // Process layer surface blur (if any)
        // For Bottom layer surfaces (like docks), we capture only Background layer
        // For Top layer surfaces, we capture Background + Bottom + windows
        // For Overlay layer surfaces, we capture everything below
        if has_layer_blur {
            if blur_state.is_ready() {
                tracing::trace!(
                    layer_count = layer_blur_surfaces.len(),
                    blur_ready = blur_state.is_ready(),
                    "Processing layer surface blur"
                );
            } else {
                tracing::debug!(
                    layer_count = layer_blur_surfaces.len(),
                    blur_ready = blur_state.is_ready(),
                    "Skipping layer blur: blur_state not ready"
                );
            }
        }
        if has_layer_blur && blur_state.is_ready() {
            // Group surfaces by layer type for proper capture.
            use std::collections::HashMap as StdHashMap;
            let layer_to_key = |l: Layer| -> u8 {
                match l {
                    Layer::Background => 0,
                    Layer::Bottom => 1,
                    Layer::Top => 2,
                    Layer::Overlay => 3,
                }
            };
            let key_to_layer = |k: u8| -> Layer {
                match k {
                    0 => Layer::Background,
                    1 => Layer::Bottom,
                    2 => Layer::Top,
                    _ => Layer::Overlay,
                }
            };

            // Group by (layer, radius bucket). Surfaces with different blur radii
            // need separate blur passes (the per-group blurred texture is shared by
            // all surfaces in the group), so a custom radius can't share a texture
            // with a differently-blurred surface. None/non-finite radius ⇒ u32::MAX
            // bucket ⇒ global config intensity (unchanged behaviour).
            let radius_bucket = |r: Option<f32>| -> u32 {
                match r {
                    Some(px) if px.is_finite() => px
                        .clamp(blur::BLUR_RADIUS_MIN_PX, blur::BLUR_RADIUS_MAX_PX)
                        .round() as u32,
                    _ => u32::MAX,
                }
            };
            let mut surfaces_by_group: StdHashMap<
                (u8, u32),
                Vec<(ObjectId, Rectangle<i32, Local>)>,
            > = StdHashMap::new();
            for (surface_id, geo, layer, blur_radius) in &layer_blur_surfaces {
                let key = (layer_to_key(*layer), radius_bucket(*blur_radius));
                surfaces_by_group
                    .entry(key)
                    .or_default()
                    .push((surface_id.clone(), *geo));
            }

            let layer_blur_start = std::time::Instant::now();

            // Force re-capture while a below-layer surface plays its open/fade
            // animation — the content hash can't see the compositor-applied
            // alpha/scale, so the backdrop would otherwise freeze at frame 1.
            // (Mirrors the KMS path.)
            let content_animating = shell.read().has_layer_open_or_fade_animations();

            // Process each group (by layer + radius bucket) that has blur surfaces
            for ((group_key, bucket), surfaces) in &surfaces_by_group {
                let layer_type = key_to_layer(*group_key);
                // Reconstruct the group's blur radius (None ⇒ global config intensity).
                let group_radius = if *bucket == u32::MAX {
                    None
                } else {
                    Some(*bucket as f32)
                };
                let layer_group_start = std::time::Instant::now();

                // Check if all surfaces in this layer have valid cached blur textures
                let all_cached = surfaces.iter().all(|(surface_id, _)| {
                    get_cached_blur_texture_for_layer(&output_name, surface_id).is_some()
                });

                // Did any surface change size since its texture was captured? The
                // content hash below only tracks what's BEHIND the surface, so a
                // resize wouldn't otherwise invalidate the cache — the stale,
                // smaller texture would be stretched over the newly-exposed area,
                // flashing the unblurred desktop. Force an immediate re-blur (no
                // throttle) so the backdrop grows in lockstep with the surface.
                let geometry_changed = surfaces.iter().any(|(surface_id, geo)| {
                    get_cached_blur_texture_for_layer(&output_name, surface_id)
                        .is_some_and(|info| info.capture_size != (geo.size.w, geo.size.h))
                });

                // EARLY THROTTLE CHECK: Skip capture entirely if all cached and recent.
                // Throttle/content-hash state is keyed per (output, layer, radius bucket)
                // so distinct buckets in the same layer don't clobber each other.
                let hash_key = format!("{}_{:?}_{}", output_name, layer_type, bucket);
                if all_cached
                    && !geometry_changed
                    && !content_animating
                    && should_throttle_layer_blur(&hash_key)
                {
                    any_blur_applied = true;
                    continue;
                }

                // Capture elements below this layer using LayerBlurCapture filter
                let capture_filter =
                    ElementFilter::LayerBlurCapture(layer_type, grabbed_window_key.clone());
                let capture_start = std::time::Instant::now();
                let layer_capture_elements: Vec<CosmicElement<R>> = workspace_elements(
                    gpu,
                    renderer,
                    shell,
                    zoom_state.as_ref(),
                    now,
                    output,
                    previous_workspace,
                    workspace,
                    CursorMode::None, // No cursor for blur capture
                    &capture_filter,
                    None,
                )?;
                let capture_elapsed = capture_start.elapsed();

                // Only fast-path (keep the existing texture) when EVERY surface already
                // has one — the transient case where overwriting a good blurred texture
                // with an empty scene would flash a grey/black card. When uncached (a
                // fresh output before anything mapped below), fall through and render:
                // the blur source clears to TRANSPARENT (below) so we cache a benign
                // invisible backdrop, and a later map re-blurs into a real one. (Mirrors
                // the KMS path; see the fresh-output black-panel regression.)
                if layer_capture_elements.is_empty() && all_cached {
                    tracing::warn!(
                        output = %output_name,
                        layer = ?layer_type,
                        "Empty layer blur capture; keeping cached texture"
                    );
                    // Throttle a sustained empty capture (see the KMS branch); the
                    // last-good texture keeps showing, so deferring the retry is free.
                    store_layer_blur_last_update(&hash_key);
                    any_blur_applied = true;
                    continue;
                }

                // Compute content hash for cache invalidation
                let content_hash = compute_element_content_hash(0, &layer_capture_elements, scale);

                // Check if content has changed since last blur
                let stored_hash = get_layer_blur_content_hash(&hash_key);
                let content_changed = stored_hash.is_none() || stored_hash != Some(content_hash);

                if !content_changed && !geometry_changed && !content_animating && all_cached {
                    let layer_group_elapsed = layer_group_start.elapsed();
                    tracing::trace!(
                        layer = ?layer_type,
                        surfaces = surfaces.len(),
                        capture_us = capture_elapsed.as_micros(),
                        total_us = layer_group_elapsed.as_micros(),
                        "Layer blur: content unchanged, using cache"
                    );
                    any_blur_applied = true;
                    continue;
                }

                // Content changed: apply throttle if we have cached textures. A
                // geometry change is never throttled — the backdrop must track the
                // surface size exactly on the frame it resizes.
                let is_dragging = grabbed_window_key.is_some();
                let throttled = content_changed
                    && !geometry_changed
                    && !content_animating
                    && all_cached
                    && !is_dragging
                    && should_throttle_layer_blur(&hash_key);

                if throttled {
                    any_blur_applied = true;
                    continue;
                }

                tracing::trace!(
                    layer = ?layer_type,
                    surfaces = surfaces.len(),
                    capture_elements = layer_capture_elements.len(),
                    content_changed = content_changed,
                    all_cached = all_cached,
                    capture_us = capture_elapsed.as_micros(),
                    "Re-blurring layer group"
                );

                // Update the throttle timestamp now (so a persistently failing group retries
                // once per throttle interval rather than every frame), but record the content
                // hash only once the pass has succeeded — otherwise the NEW hash is paired
                // with the OLD texture and the `!content_changed` skip above freezes the group
                // against a static desktop. (Mirrors the KMS path.)
                store_layer_blur_last_update(&hash_key);

                // Render captured elements to background texture
                let bg_render_start = std::time::Instant::now();
                let bg_render_ok = if let Some(bg_texture) = blur_state.background_texture.as_mut()
                {
                    let mut blur_dt =
                        OutputDamageTracker::new(output_size, scale, Transform::Normal);

                    let render_result = {
                        let mut gles_frame = bg_texture.render();

                        let render_res = gles_frame.draw::<_, RenderError<R::Error>>(|tex| {
                            let bound = renderer.bind(tex).map_err(RenderError::Rendering)?;
                            let mut bound_target = bound;
                            let res = blur_dt.render_output(
                                renderer,
                                &mut bound_target,
                                0, // Full redraw for blur capture
                                &layer_capture_elements,
                                // Transparent, not CLEAR_COLOR: an empty/partial capture
                                // must blend away rather than cache opaque charcoal.
                                BLUR_CAPTURE_CLEAR_COLOR,
                            );
                            match res {
                                Ok(_) => Ok(Vec::new()),
                                Err(e) => Err(e),
                            }
                        });

                        render_res.map_err(|_| ())
                    };

                    render_result.is_ok()
                } else {
                    false
                };
                let bg_render_elapsed = bg_render_start.elapsed();

                if !bg_render_ok {
                    tracing::warn!(layer = ?layer_type, "Failed to render background for layer blur");
                    continue;
                }

                // Downsample background to smaller texture for blur passes (if enabled)
                let downsample_start = std::time::Instant::now();
                let downsample_enabled = blur_downsample_enabled();
                let downsample_ok = if downsample_enabled {
                    if let (Some(bg), Some(ds)) = (
                        blur_state.background_texture.as_ref().cloned(),
                        blur_state.downsampled_texture.as_mut(),
                    ) {
                        let blur_size = blur_state.texture_size;
                        downsample_texture(renderer, &bg, ds, output_size, blur_size).is_ok()
                    } else {
                        false
                    }
                } else {
                    true // No downsampling needed
                };
                let downsample_elapsed = downsample_start.elapsed();

                if !downsample_ok {
                    tracing::warn!(layer = ?layer_type, "Failed to downsample for layer blur");
                    continue;
                }

                // Apply Dual Kawase blur using dedicated layer blur textures
                // (separate from window blur's texture_a/texture_b to prevent cache pollution)
                let blur_passes_start = std::time::Instant::now();
                let blur_size = blur_state.texture_size;
                let blur_source = if downsample_enabled {
                    blur_state.downsampled_texture.as_ref().cloned()
                } else {
                    blur_state.background_texture.as_ref().cloned()
                };

                if let (Some(src), Some(ping), Some(pong)) = (
                    blur_source,
                    blur_state.layer_texture_a.as_mut(),
                    blur_state.layer_texture_b.as_mut(),
                ) {
                    // Per-surface radius drives intensity; None ⇒ global config.
                    let (blur_levels, blur_offset) = blur::resolve_blur_params(group_radius);
                    let blur_result = apply_dual_kawase_blur(
                        renderer,
                        &src,
                        ping,
                        pong,
                        blur_size,
                        blur_levels,
                        blur_offset,
                    );

                    if blur_result.is_ok() {
                        // Copy the blurred result out of the shared ping/pong buffer
                        // BEFORE the next (layer, radius) group reuses layer_texture_b —
                        // otherwise this group's cached texture would be overwritten by
                        // the next group's blur pass. (Mirrors the KMS path; previously a
                        // bare pong.clone() was only safe with a single group per frame.)
                        let cached_texture = match copy_blur_texture_for_cache(
                            renderer, pong, blur_size,
                        ) {
                            Ok(tex) => tex,
                            Err(e) => {
                                tracing::warn!(
                                    output = %output_name,
                                    layer = ?layer_type,
                                    error = ?e,
                                    "Failed to copy layer blur texture for cache, using original"
                                );
                                pong.clone()
                            }
                        };

                        // The pass succeeded, so this hash now genuinely describes the
                        // texture being cached. See the note at the timestamp store above.
                        store_layer_blur_content_hash(&hash_key, content_hash);

                        // Cache the SAME blurred texture for all surfaces in this group
                        for (surface_id, geo) in surfaces {
                            cache_blur_texture_for_layer(
                                &output_name,
                                surface_id.clone(),
                                BlurredTextureInfo {
                                    texture: cached_texture.clone(),
                                    size: blur_size,
                                    screen_size: output_size,
                                    scale,
                                    background_state_hash: content_hash,
                                    capture_size: (geo.size.w, geo.size.h),
                                },
                            );
                            tracing::trace!(
                                ?surface_id,
                                layer = ?layer_type,
                                "Cached blur texture for layer surface"
                            );
                        }
                        any_blur_applied = true;
                        let blur_passes_elapsed = blur_passes_start.elapsed();
                        let layer_group_elapsed = layer_group_start.elapsed();
                        tracing::trace!(
                            layer = ?layer_type,
                            surfaces_count = surfaces.len(),
                            capture_us = capture_start.elapsed().as_micros(),
                            bg_render_us = bg_render_elapsed.as_micros(),
                            downsample_us = downsample_elapsed.as_micros(),
                            blur_passes_us = blur_passes_elapsed.as_micros(),
                            total_us = layer_group_elapsed.as_micros(),
                            "Layer group blur complete"
                        );
                    } else {
                        tracing::warn!(layer = ?layer_type, "Layer blur passes failed");
                    }
                }
            }

            let layer_blur_elapsed = layer_blur_start.elapsed();
            if any_blur_applied {
                tracing::trace!(
                    total_us = layer_blur_elapsed.as_micros(),
                    "Layer blur processing complete"
                );
            }
        }

        blur_state.blur_applied = any_blur_applied;

        // Final render: collect ALL elements (blur backdrops will find their per-window cached textures)
        let final_elements: Vec<CosmicElement<R>> = workspace_elements(
            gpu,
            renderer,
            shell,
            zoom_state.as_ref(),
            now,
            output,
            previous_workspace,
            workspace,
            cursor_mode,
            &element_filter,
            None,
        )?;

        // Render final scene with blur backdrops to actual target
        let result = damage_tracker.render_output(
            renderer,
            target,
            if any_blur_applied { 0 } else { age }, // Force full redraw if blur active
            &final_elements,
            CLEAR_COLOR,
        )?;

        Ok((result, final_elements))
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

                        Ok(RenderOutputResult {
                            damage: res.0,
                            sync,
                            states: res.1,
                        })
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
    R::Error: FromGlesError,
    CosmicElement<R>: RenderElement<R>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let elements: Vec<CosmicElement<R>> = workspace_elements(
        gpu,
        renderer,
        shell,
        zoom_level,
        now,
        output,
        previous,
        current,
        cursor_mode,
        &element_filter,
        None,
    )?;

    if let Some(additional_damage) = additional_damage {
        let output_geo = output.geometry().to_local(output).as_logical();
        let additional_damage_elements: Vec<_> = additional_damage
            .into_iter()
            .filter_map(|rect| rect.intersection(output_geo))
            .map(DamageElement::new)
            .collect();
        damage_tracker.damage_output(age, &additional_damage_elements)?;
    }

    let res = damage_tracker.render_output(
        renderer,
        target,
        age,
        &elements,
        CLEAR_COLOR, // TODO use a theme neutral color
    );

    res.map(|res| (res, elements))
}
