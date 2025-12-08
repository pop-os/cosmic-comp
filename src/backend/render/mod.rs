// SPDX-License-Identifier: GPL-3.0-only

use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::HashMap,
    ops::ControlFlow,
    sync::{Arc, Weak},
    time::Instant,
};

#[cfg(feature = "debug")]
use crate::debug::fps_ui;
use crate::{
    backend::{
        kms::render::gles::GbmGlowBackend,
        render::{
            clipped_surface::{CLIPPING_SHADER, ClippingShader},
            element::DamageElement,
        },
    },
    config::ScreenFilter,
    shell::{
        CosmicMappedRenderElement, OverviewMode, SeatExt, Trigger, WorkspaceDelta,
        WorkspaceRenderElement,
        element::CosmicMappedKey,
        focus::{FocusTarget, Stage, render_input_order, target::WindowGroup},
        grabs::{SeatMenuGrabState, SeatMoveGrabState},
        layout::tiling::ANIMATION_DURATION,
        zoom::ZoomState,
    },
    utils::{prelude::*, quirks::workspace_overview_is_open},
    wayland::{
        handlers::{
            compositor::FRAME_TIME_FILTER,
            data_device::get_dnd_icon,
            screencopy::{FrameHolder, SessionData, render_session},
        },
        protocols::workspace::WorkspaceHandle,
    },
};

use cosmic::Theme;
use element::FromGlesError;
use smithay::{
    backend::{
        allocator::{Fourcc, dmabuf::Dmabuf},
        drm::{DrmDeviceFd, DrmNode},
        renderer::{
            Bind, Blit, Color32F, ExportMem, ImportAll, ImportMem, Offscreen, Renderer, Texture,
            TextureFilter,
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
    utils::{
        IsAlive, Logical, Monotonic, Physical, Point, Rectangle, Scale, Size, Time, Transform,
    },
    wayland::{dmabuf::get_dmabuf, session_lock::LockSurface},
};

#[cfg(feature = "debug")]
use smithay_egui::EguiState;

pub mod animations;
pub mod clipped_surface;
pub mod cursor;
pub mod element;
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
}

#[derive(Clone)]
pub enum Key {
    Static(WeakId),
    Group(Weak<()>),
    Window(Usage, CosmicMappedKey),
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
        }
    }
}
impl PartialEq for Key {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Key::Static(s1), Key::Static(s2)) => s1 == s2,
            (Key::Group(g1), Key::Group(g2)) => Weak::ptr_eq(g1, g2),
            (Key::Window(u1, w1), Key::Window(u2, w2)) => u1 == u2 && w1 == w2,
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
    radius: [u8; 4],
    alpha: f32,
    color: [f32; 3],
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
        radius: [u8; 4],
        alpha: f32,
        active_window_hint: [f32; 3],
    ) -> PixelShaderElement {
        let t = thickness as i32;
        element_geo.loc -= (t, t).into();
        element_geo.size += (t * 2, t * 2).into();

        IndicatorShader::element(
            renderer,
            key,
            element_geo,
            thickness,
            radius,
            alpha,
            active_window_hint,
        )
    }

    pub fn element<R: AsGlowRenderer>(
        renderer: &R,
        key: impl Into<Key>,
        geo: Rectangle<i32, Local>,
        thickness: u8,
        radius: [u8; 4],
        alpha: f32,
        color: [f32; 3],
    ) -> PixelShaderElement {
        let settings = IndicatorSettings {
            thickness,
            radius,
            alpha,
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
        });

        let key = key.into();
        if cache
            .get(&key)
            .filter(|(old_settings, _)| &settings == old_settings)
            .is_none()
        {
            let thickness: f32 = thickness as f32;
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
                            radius[0] as f32 + thickness / 2.,
                            radius[1] as f32 + thickness / 2.,
                            radius[2] as f32 + thickness / 2.,
                            radius[3] as f32 + thickness / 2.,
                        ],
                    ),
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
    radius: f32,
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
        radius: f32,
        alpha: f32,
        color: [f32; 3],
    ) -> PixelShaderElement {
        let settings = BackdropSettings {
            radius,
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
                    Uniform::new("radius", radius),
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
            UniformName::new("radius", UniformType::_4f),
        ],
    )?;
    let rectangle_shader = renderer.compile_custom_pixel_shader(
        RECTANGLE_SHADER,
        &[
            UniformName::new("color", UniformType::_3f),
            UniformName::new("radius", UniformType::_1f),
        ],
    )?;
    let postprocess_shader = renderer.compile_custom_texture_shader(
        POSTPROCESS_SHADER,
        &[
            UniformName::new("invert", UniformType::_1f),
            UniformName::new("color_mode", UniformType::_1f),
        ],
    )?;
    let clipping_shader = renderer.compile_custom_texture_shader(
        CLIPPING_SHADER,
        &[
            UniformName::new("geo_size", UniformType::_2f),
            UniformName::new("corner_radius", UniformType::_4f),
            UniformName::new("input_to_geo", UniformType::Matrix3x3),
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
    theme: &Theme,
    now: Time<Monotonic>,
    output: &Output,
    mode: CursorMode,
    exclude_dnd_icon: bool,
) -> Vec<CosmicElement<R>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
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

        if !exclude_dnd_icon {
            if let Some(dnd_icon) = get_dnd_icon(seat) {
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
        }

        let theme = theme.cosmic();
        if let Some(grab_elements) = seat
            .user_data()
            .get::<SeatMoveGrabState>()
            .unwrap()
            .lock()
            .unwrap()
            .as_ref()
            .map(|state| state.render::<CosmicMappedRenderElement<R>, R>(renderer, output, theme))
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
) -> Result<Vec<CosmicElement<R>>, RenderError<R::Error>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
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
        element_filter,
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
) -> Result<Vec<CosmicElement<R>>, RenderError<R::Error>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    R::Error: FromGlesError,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let mut elements = Vec::new();

    let shell_ref = shell.read();
    let seats = shell_ref.seats.iter().cloned().collect::<Vec<_>>();
    if seats.is_empty() {
        return Ok(Vec::new());
    }
    let theme = shell_ref.theme().clone();
    let scale = output.current_scale().fractional_scale();
    // we don't want to hold a shell lock across `cursor_elements`,
    // that is prone to deadlock with the main-thread on some grabs.
    std::mem::drop(shell_ref);

    elements.extend(cursor_elements(
        renderer,
        seats.iter(),
        zoom_level,
        &theme,
        now,
        output,
        cursor_mode,
        element_filter == ElementFilter::ExcludeWorkspaceOverview,
    ));

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
        theme.cosmic().active_hint as u8
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
                elements.extend(
                    render_elements_from_surface_tree::<_, WorkspaceRenderElement<_>>(
                        renderer,
                        popup.wl_surface(),
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
                    .map(Into::into),
                );
            }
            Stage::LayerSurface { layer, location } => {
                elements.extend(
                    render_elements_from_surface_tree::<_, WorkspaceRenderElement<_>>(
                        renderer,
                        layer.wl_surface(),
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
                    layout
                        .render_popups(renderer, alpha)
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
                            theme.cosmic(),
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
                        theme.cosmic(),
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
            Stage::Workspace { workspace, offset } => {
                elements.extend(
                    match workspace.render(
                        renderer,
                        last_active_seat,
                        !move_active && is_active_space,
                        overview.clone(),
                        resize_indicator.clone(),
                        active_hint,
                        theme.cosmic(),
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

    Ok(elements)
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
    pub fn new_with_renderer<R: AsGlowRenderer + Offscreen<GlesTexture>>(
        renderer: &mut R,
        format: Fourcc,
        output_config: PostprocessOutputConfig,
    ) -> Result<Self, R::Error> {
        let size = output_config.size;
        let buffer_size = size.to_logical(1).to_buffer(1, Transform::Normal);
        let opaque_regions = vec![Rectangle::from_size(buffer_size)];

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

    pub fn track_cursor<R: AsGlowRenderer + Offscreen<GlesTexture>>(
        &mut self,
        renderer: &mut R,
        format: Fourcc,
        size: Size<i32, Physical>,
        scale: Scale<f64>,
    ) -> Result<(), R::Error> {
        let buffer_size = size.to_logical(1).to_buffer(1, Transform::Normal);

        if let (Some(tex), Some(tracker)) = (
            self.cursor_texture.as_ref(),
            self.cursor_damage_tracker.as_ref(),
        ) {
            if tex.format().is_some_and(|f| f == format)
                && tracker.mode()
                    == &(OutputModeSource::Static {
                        size,
                        scale,
                        transform: Transform::Normal,
                    })
            {
                return Ok(());
            }
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
    R: Renderer
        + ImportAll
        + ImportMem
        + ExportMem
        + Bind<Dmabuf>
        + Offscreen<GlesTexture>
        + Blit
        + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    R::Error: FromGlesError,
    CosmicElement<R>: RenderElement<R>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
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
                if let Some(pending_image_copy_data) = render_session::<_, _, GlesTexture>(
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
                            elements.extend(
                                additional_damage
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
                                    .map(Into::into),
                            );

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
    R: Renderer + ImportAll + ImportMem + ExportMem + Bind<Dmabuf> + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    R::Error: FromGlesError,
    CosmicElement<R>: RenderElement<R>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let mut elements: Vec<CosmicElement<R>> = workspace_elements(
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
    )?;

    if let Some(additional_damage) = additional_damage {
        let output_geo = output.geometry().to_local(output).as_logical();
        elements.extend(
            additional_damage
                .into_iter()
                .filter_map(|rect| rect.intersection(output_geo))
                .map(DamageElement::new)
                .map(Into::<CosmicElement<R>>::into),
        );
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
