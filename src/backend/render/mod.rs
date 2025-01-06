// SPDX-License-Identifier: GPL-3.0-only

use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::HashMap,
    ops::ControlFlow,
    sync::{Arc, RwLock, Weak},
    time::Instant,
};

#[cfg(feature = "debug")]
use crate::debug::fps_ui;
use crate::{
    backend::{kms::render::gles::GbmGlowBackend, render::element::DamageElement},
    shell::{
        element::CosmicMappedKey,
        focus::{render_input_order, target::WindowGroup, Stage},
        grabs::{SeatMenuGrabState, SeatMoveGrabState},
        layout::tiling::ANIMATION_DURATION,
        CosmicMappedRenderElement, OverviewMode, SeatExt, Trigger, WorkspaceDelta,
        WorkspaceRenderElement,
    },
    utils::{prelude::*, quirks::workspace_overview_is_open},
    wayland::{
        handlers::{
            data_device::get_dnd_icon,
            screencopy::{render_session, FrameHolder, SessionData},
        },
        protocols::workspace::WorkspaceHandle,
    },
};

use cosmic::Theme;
use element::FromGlesError;
use smithay::{
    backend::{
        allocator::dmabuf::Dmabuf,
        drm::{DrmDeviceFd, DrmNode},
        renderer::{
            buffer_dimensions,
            damage::{Error as RenderError, OutputDamageTracker, RenderOutputResult},
            element::{
                surface::{render_elements_from_surface_tree, WaylandSurfaceRenderElement},
                utils::{CropRenderElement, Relocate, RelocateRenderElement},
                AsRenderElements, Element, Id, Kind, RenderElement,
            },
            gles::{
                element::PixelShaderElement, GlesError, GlesPixelProgram, GlesRenderer, Uniform,
                UniformName, UniformType,
            },
            glow::GlowRenderer,
            multigpu::{Error as MultiError, MultiFrame, MultiRenderer},
            sync::SyncPoint,
            Bind, Blit, Color32F, ExportMem, ImportAll, ImportMem, Offscreen, Renderer,
            TextureFilter,
        },
    },
    input::Seat,
    output::{Output, OutputNoMode},
    utils::{IsAlive, Logical, Monotonic, Point, Rectangle, Scale, Time, Transform},
    wayland::{
        dmabuf::get_dmabuf,
        session_lock::LockSurface,
        shm::{shm_format_to_fourcc, with_buffer_contents},
    },
};

#[cfg(feature = "debug")]
use smithay_egui::EguiState;

pub mod animations;

pub mod cursor;
pub mod element;
use self::element::{AsGlowRenderer, CosmicElement};

use super::kms::Timings;

pub type GlMultiRenderer<'a> =
    MultiRenderer<'a, 'a, GbmGlowBackend<DrmDeviceFd>, GbmGlowBackend<DrmDeviceFd>>;
pub type GlMultiFrame<'a, 'frame> =
    MultiFrame<'a, 'a, 'frame, GbmGlowBackend<DrmDeviceFd>, GbmGlowBackend<DrmDeviceFd>>;
pub type GlMultiError = MultiError<GbmGlowBackend<DrmDeviceFd>, GbmGlowBackend<DrmDeviceFd>>;

pub enum RendererRef<'a> {
    Glow(&'a mut GlowRenderer),
    GlMulti(GlMultiRenderer<'a>),
}

impl<'a> AsRef<GlowRenderer> for RendererRef<'a> {
    fn as_ref(&self) -> &GlowRenderer {
        match self {
            Self::Glow(renderer) => renderer,
            Self::GlMulti(renderer) => renderer.as_ref(),
        }
    }
}

impl<'a> AsMut<GlowRenderer> for RendererRef<'a> {
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
    Static(Id),
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
        Key::Static(id)
    }
}

#[derive(PartialEq)]
struct IndicatorSettings {
    thickness: u8,
    radius: u8,
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
        scale: f64,
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
            thickness * 2,
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
        radius: u8,
        alpha: f32,
        scale: f64,
        color: [f32; 3],
    ) -> PixelShaderElement {
        let thickness = (thickness as f64 * scale).round() as u8;

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
            Key::Static(_) => true,
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
                    Uniform::new("radius", radius as f32),
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
            Key::Static(_) => true,
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

pub fn init_shaders(renderer: &mut GlesRenderer) -> Result<(), GlesError> {
    {
        let egl_context = renderer.egl_context();
        if egl_context.user_data().get::<IndicatorShader>().is_some()
            && egl_context.user_data().get::<BackdropShader>().is_some()
        {
            return Ok(());
        }
    }

    let outline_shader = renderer.compile_custom_pixel_shader(
        OUTLINE_SHADER,
        &[
            UniformName::new("color", UniformType::_3f),
            UniformName::new("thickness", UniformType::_1f),
            UniformName::new("radius", UniformType::_1f),
        ],
    )?;
    let rectangle_shader = renderer.compile_custom_pixel_shader(
        RECTANGLE_SHADER,
        &[
            UniformName::new("color", UniformType::_3f),
            UniformName::new("radius", UniformType::_1f),
        ],
    )?;

    let egl_context = renderer.egl_context();
    egl_context
        .user_data()
        .insert_if_missing(|| IndicatorShader(outline_shader));
    egl_context
        .user_data()
        .insert_if_missing(|| BackdropShader(rectangle_shader));

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
    theme: &Theme,
    now: Time<Monotonic>,
    output: &Output,
    mode: CursorMode,
    exclude_dnd_icon: bool,
) -> Vec<CosmicElement<R>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: Send + Clone + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    let scale = output.current_scale().fractional_scale();
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
                    &seat,
                    location,
                    scale.into(),
                    now,
                    mode != CursorMode::NotDefault,
                )
                .into_iter()
                .map(|(elem, hotspot)| {
                    CosmicElement::Cursor(RelocateRenderElement::from_element(
                        elem,
                        Point::from((-hotspot.x, -hotspot.y)),
                        Relocate::Relative,
                    ))
                }),
            );
        }

        if !exclude_dnd_icon {
            if let Some(dnd_icon) = get_dnd_icon(&seat) {
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
            .map(|state| state.render::<CosmicElement<R>, R>(renderer, output, theme))
        {
            elements.extend(grab_elements);
        }

        if let Some(grab_elements) = seat
            .user_data()
            .get::<SeatMenuGrabState>()
            .unwrap()
            .lock()
            .unwrap()
            .as_ref()
            .map(|state| state.render::<CosmicMappedRenderElement<R>, R>(renderer, output))
        {
            elements.extend(grab_elements.into_iter().map(Into::into));
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
    shell: &Arc<RwLock<Shell>>,
    now: Time<Monotonic>,
    output: &Output,
    cursor_mode: CursorMode,
    _fps: Option<(&EguiState, &Timings)>,
) -> Result<Vec<CosmicElement<R>>, RenderError<R::Error>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: Send + Clone + 'static,
    <R as Renderer>::Error: FromGlesError,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let shell_guard = shell.read().unwrap();
    #[cfg(feature = "debug")]
    let mut debug_elements = {
        let output_geo = output.geometry();
        let seats = shell_guard.seats.iter().cloned().collect::<Vec<_>>();
        let scale = output.current_scale().fractional_scale();

        if let Some((state, timings)) = _fps {
            let debug_active = shell_guard.debug_active;
            vec![fps_ui(
                _gpu,
                debug_active,
                &seats,
                renderer.glow_renderer_mut(),
                state,
                timings,
                Rectangle::from_loc_and_size(
                    (0, 0),
                    (output_geo.size.w.min(400), output_geo.size.h.min(800)),
                ),
                scale,
            )
            .map_err(FromGlesError::from_gles_error)
            .map_err(RenderError::Rendering)?
            .into()]
        } else {
            Vec::new()
        }
    };

    let Some((previous_workspace, workspace)) = shell_guard.workspaces.active(output) else {
        #[cfg(not(feature = "debug"))]
        return Ok(Vec::new());
        #[cfg(feature = "debug")]
        return Ok(debug_elements);
    };

    let (previous_idx, idx) = shell_guard.workspaces.active_num(&output);
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

    #[allow(unused_mut)]
    let workspace_elements = workspace_elements(
        _gpu,
        renderer,
        shell,
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
    shell: &Arc<RwLock<Shell>>,
    now: Time<Monotonic>,
    output: &Output,
    previous: Option<(WorkspaceHandle, usize, WorkspaceDelta)>,
    current: (WorkspaceHandle, usize),
    cursor_mode: CursorMode,
    element_filter: ElementFilter,
) -> Result<Vec<CosmicElement<R>>, RenderError<<R as Renderer>::Error>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: Send + Clone + 'static,
    <R as Renderer>::Error: FromGlesError,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let mut elements = Vec::new();

    let theme = shell.read().unwrap().theme().clone();
    let seats = shell
        .read()
        .unwrap()
        .seats
        .iter()
        .cloned()
        .collect::<Vec<_>>();
    let scale = output.current_scale().fractional_scale();

    elements.extend(cursor_elements(
        renderer,
        seats.iter(),
        &theme,
        now,
        output,
        cursor_mode,
        element_filter == ElementFilter::ExcludeWorkspaceOverview,
    ));

    let shell = shell.read().unwrap();

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
    let crop_to_output = |element: WorkspaceRenderElement<R>| {
        CropRenderElement::from_element(
            element.into(),
            scale,
            Rectangle::from_loc_and_size((0, 0), output_size),
        )
    };

    render_input_order(
        &*shell,
        output,
        previous,
        current,
        element_filter,
        |stage| {
            match stage {
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
                            Kind::Unspecified,
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
                            &layer.wl_surface(),
                            location
                                .to_local(output)
                                .as_logical()
                                .to_physical_precise_round(scale),
                            Scale::from(scale),
                            1.0,
                            Kind::Unspecified,
                        )
                        .into_iter()
                        .flat_map(crop_to_output)
                        .map(Into::into),
                    );
                }
                Stage::OverrideRedirect { surface, location } => {
                    elements.extend(
                        AsRenderElements::<R>::render_elements::<WorkspaceRenderElement<R>>(
                            surface,
                            renderer,
                            location
                                .to_local(output)
                                .as_logical()
                                .to_physical_precise_round(scale),
                            Scale::from(scale),
                            1.0,
                        )
                        .into_iter()
                        .flat_map(crop_to_output)
                        .map(Into::into),
                    );
                }
                Stage::StickyPopups(layout) => {
                    let alpha = match &overview.0 {
                        OverviewMode::Started(_, started) => {
                            (1.0 - (Instant::now().duration_since(*started).as_millis()
                                / ANIMATION_DURATION.as_millis())
                                as f32)
                                .max(0.0)
                                * 0.4
                                + 0.6
                        }
                        OverviewMode::Ended(_, ended) => {
                            ((Instant::now().duration_since(*ended).as_millis()
                                / ANIMATION_DURATION.as_millis())
                                as f32)
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
                                / ANIMATION_DURATION.as_millis())
                                as f32)
                                .max(0.0)
                                * 0.4
                                + 0.6
                        }
                        OverviewMode::Ended(_, ended) => {
                            ((Instant::now().duration_since(*ended).as_millis()
                                / ANIMATION_DURATION.as_millis())
                                as f32)
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
                                current_focus.as_ref().and_then(|stack| stack.last()),
                                resize_indicator.clone(),
                                active_hint,
                                alpha,
                                &theme.cosmic(),
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
                            (!move_active && is_active_space).then_some(last_active_seat),
                            overview.clone(),
                            &theme.cosmic(),
                        ) {
                            Ok(elements) => {
                                elements
                                    .into_iter()
                                    .flat_map(crop_to_output)
                                    .map(|element| {
                                        CosmicElement::Workspace(
                                            RelocateRenderElement::from_element(
                                                element,
                                                offset.to_physical_precise_round(scale),
                                                Relocate::Relative,
                                            ),
                                        )
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
                            (!move_active && is_active_space).then_some(last_active_seat),
                            overview.clone(),
                            resize_indicator.clone(),
                            active_hint,
                            &theme.cosmic(),
                        ) {
                            Ok(elements) => {
                                elements
                                    .into_iter()
                                    .flat_map(crop_to_output)
                                    .map(|element| {
                                        CosmicElement::Workspace(
                                            RelocateRenderElement::from_element(
                                                element,
                                                offset.to_physical_precise_round(scale),
                                                Relocate::Relative,
                                            ),
                                        )
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
        },
    )?;

    Ok(elements)
}

fn session_lock_elements<R>(
    renderer: &mut R,
    output: &Output,
    lock_surface: Option<&LockSurface>,
) -> Vec<WaylandSurfaceRenderElement<R>>
where
    R: Renderer + ImportAll,
    <R as Renderer>::TextureId: Clone + 'static,
{
    if let Some(surface) = lock_surface {
        let scale = Scale::from(output.current_scale().fractional_scale());
        render_elements_from_surface_tree(
            renderer,
            surface.wl_surface(),
            (0, 0),
            scale,
            1.0,
            Kind::Unspecified,
        )
    } else {
        Vec::new()
    }
}

#[profiling::function]
pub fn render_output<'d, R, Target, OffTarget>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    target: Target,
    damage_tracker: &'d mut OutputDamageTracker,
    age: usize,
    shell: &Arc<RwLock<Shell>>,
    now: Time<Monotonic>,
    output: &Output,
    cursor_mode: CursorMode,
) -> Result<RenderOutputResult<'d>, RenderError<<R as Renderer>::Error>>
where
    R: Renderer
        + ImportAll
        + ImportMem
        + ExportMem
        + Bind<Dmabuf>
        + Bind<Target>
        + Offscreen<OffTarget>
        + Blit<Target>
        + AsGlowRenderer,
    <R as Renderer>::TextureId: Send + Clone + 'static,
    <R as Renderer>::Error: FromGlesError,
    CosmicElement<R>: RenderElement<R>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
    Target: Clone,
{
    let shell_ref = shell.read().unwrap();
    let (previous_workspace, workspace) = shell_ref
        .workspaces
        .active(output)
        .ok_or(RenderError::OutputNoMode(OutputNoMode))?;
    let (previous_idx, idx) = shell_ref.workspaces.active_num(output);
    let previous_workspace = previous_workspace
        .zip(previous_idx)
        .map(|((w, start), idx)| (w.handle, idx, start));
    let workspace = (workspace.handle, idx);
    std::mem::drop(shell_ref);

    let element_filter = if workspace_overview_is_open(output) {
        ElementFilter::LayerShellOnly
    } else {
        ElementFilter::All
    };

    let result = render_workspace(
        gpu,
        renderer,
        target.clone(),
        damage_tracker,
        age,
        None,
        shell,
        now,
        output,
        previous_workspace,
        workspace,
        cursor_mode,
        element_filter,
    );

    match result {
        Ok((res, mut elements)) => {
            for (session, frame) in output.take_pending_frames() {
                if let Some((frame, damage)) = render_session(
                    renderer,
                    &session.user_data().get::<SessionData>().unwrap(),
                    frame,
                    output.current_transform(),
                    |buffer, renderer, dt, age, additional_damage| {
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

                        if let (Some(ref damage), _) = &res {
                            if let Ok(dmabuf) = get_dmabuf(buffer) {
                                renderer
                                    .bind(dmabuf.clone())
                                    .map_err(RenderError::Rendering)?;
                            } else {
                                let size = buffer_dimensions(buffer).unwrap();
                                let format = with_buffer_contents(buffer, |_, _, data| {
                                    shm_format_to_fourcc(data.format)
                                })
                                .map_err(|_| OutputNoMode)? // eh, we have to do some error
                                .expect(
                                    "We should be able to convert all hardcoded shm screencopy formats",
                                );
                                let render_buffer = renderer
                                    .create_buffer(format, size)
                                    .map_err(RenderError::Rendering)?;
                                renderer
                                    .bind(render_buffer)
                                    .map_err(RenderError::Rendering)?;
                            }
                            for rect in damage.iter() {
                                renderer
                                    .blit_from(target.clone(), *rect, *rect, TextureFilter::Nearest)
                                    .map_err(RenderError::Rendering)?;
                            }
                        }

                        Ok(RenderOutputResult {
                            damage: res.0,
                            sync: SyncPoint::default(),
                            states: res.1,
                        })
                    },
                )? {
                    frame.success(output.current_transform(), damage, now);
                }
            }

            Ok(res)
        }
        Err(err) => Err(err),
    }
}

#[profiling::function]
pub fn render_workspace<'d, R, Target, OffTarget>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    target: Target,
    damage_tracker: &'d mut OutputDamageTracker,
    age: usize,
    additional_damage: Option<Vec<Rectangle<i32, Logical>>>,
    shell: &Arc<RwLock<Shell>>,
    now: Time<Monotonic>,
    output: &Output,
    previous: Option<(WorkspaceHandle, usize, WorkspaceDelta)>,
    current: (WorkspaceHandle, usize),
    cursor_mode: CursorMode,
    element_filter: ElementFilter,
) -> Result<(RenderOutputResult<'d>, Vec<CosmicElement<R>>), RenderError<<R as Renderer>::Error>>
where
    R: Renderer
        + ImportAll
        + ImportMem
        + ExportMem
        + Bind<Dmabuf>
        + Bind<Target>
        + Offscreen<OffTarget>
        + AsGlowRenderer,
    <R as Renderer>::TextureId: Send + Clone + 'static,
    <R as Renderer>::Error: FromGlesError,
    CosmicElement<R>: RenderElement<R>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let mut elements: Vec<CosmicElement<R>> = workspace_elements(
        gpu,
        renderer,
        shell,
        now,
        output,
        previous,
        current,
        cursor_mode,
        element_filter,
    )?;

    if let Some(additional_damage) = additional_damage {
        let output_geo = output.geometry().to_local(&output).as_logical();
        elements.extend(
            additional_damage
                .into_iter()
                .filter_map(|rect| rect.intersection(output_geo))
                .map(DamageElement::new)
                .map(Into::<CosmicElement<R>>::into),
        );
    }

    renderer.bind(target).map_err(RenderError::Rendering)?;
    let res = damage_tracker.render_output(
        renderer,
        age,
        &elements,
        CLEAR_COLOR, // TODO use a theme neutral color
    );

    res.map(|res| (res, elements))
}
