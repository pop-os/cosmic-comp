// SPDX-License-Identifier: GPL-3.0-only

use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::HashMap,
    sync::{Arc, RwLock, Weak},
    time::Instant,
};

#[cfg(feature = "debug")]
use crate::debug::fps_ui;
use crate::{
    backend::{kms::render::gles::GbmGlowBackend, render::element::DamageElement},
    shell::{
        element::CosmicMappedKey,
        focus::target::WindowGroup,
        grabs::{SeatMenuGrabState, SeatMoveGrabState},
        layout::tiling::ANIMATION_DURATION,
        CosmicMappedRenderElement, OverviewMode, SeatExt, SessionLock, Trigger, WorkspaceDelta,
        WorkspaceRenderElement,
    },
    utils::prelude::*,
    wayland::{
        handlers::{
            data_device::get_dnd_icon,
            screencopy::{render_session, FrameHolder, SessionData, WORKSPACE_OVERVIEW_NAMESPACE},
        },
        protocols::workspace::WorkspaceHandle,
    },
};

use cosmic::Theme;
use cosmic_comp_config::workspace::WorkspaceLayout;
use element::FromGlesError;
use keyframe::{ease, functions::EaseInOutCubic};
use smithay::{
    backend::{
        allocator::dmabuf::Dmabuf,
        drm::{DrmDeviceFd, DrmNode},
        renderer::{
            buffer_dimensions,
            damage::{Error as RenderError, OutputDamageTracker, RenderOutputResult},
            element::{
                surface::{render_elements_from_surface_tree, WaylandSurfaceRenderElement},
                utils::{Relocate, RelocateRenderElement},
                AsRenderElements, Element, Id, Kind, RenderElement,
            },
            gles::{
                element::PixelShaderElement, GlesError, GlesPixelProgram, GlesRenderer, Uniform,
                UniformName, UniformType,
            },
            multigpu::{Error as MultiError, MultiFrame, MultiRenderer},
            sync::SyncPoint,
            Bind, Blit, ExportMem, ImportAll, ImportMem, Offscreen, Renderer, TextureFilter,
        },
    },
    desktop::{layer_map_for_output, PopupManager},
    input::Seat,
    output::{Output, OutputNoMode},
    utils::{IsAlive, Logical, Monotonic, Physical, Point, Rectangle, Scale, Time, Transform},
    wayland::{
        dmabuf::get_dmabuf,
        shell::wlr_layer::Layer,
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

pub static CLEAR_COLOR: [f32; 4] = [0.153, 0.161, 0.165, 1.0];
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
            if let Some(wl_surface) = get_dnd_icon(&seat) {
                elements.extend(
                    cursor::draw_dnd_icon(renderer, &wl_surface, location.to_i32_round(), scale)
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

#[derive(Clone, Debug)]
pub struct SplitRenderElements<E> {
    pub w_elements: Vec<E>,
    pub p_elements: Vec<E>,
}

impl<E> Default for SplitRenderElements<E> {
    fn default() -> Self {
        Self {
            w_elements: Vec::new(),
            p_elements: Vec::new(),
        }
    }
}

impl<E> SplitRenderElements<E> {
    pub fn extend(&mut self, other: Self) {
        self.w_elements.extend(other.w_elements);
        self.p_elements.extend(other.p_elements);
    }

    pub fn extend_map<E2, F: FnMut(E2) -> E>(&mut self, other: SplitRenderElements<E2>, mut f: F) {
        self.w_elements
            .extend(other.w_elements.into_iter().map(&mut f));
        self.p_elements
            .extend(other.p_elements.into_iter().map(&mut f));
    }

    pub fn join(mut self) -> Vec<E> {
        self.p_elements.extend(self.w_elements);
        self.p_elements
    }
}

impl<R> SplitRenderElements<CosmicElement<R>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn extend_from_workspace_elements<E: Into<WorkspaceRenderElement<R>>>(
        &mut self,
        other: SplitRenderElements<E>,
        offset: Point<i32, Physical>,
    ) {
        self.extend_map(other, |element| {
            CosmicElement::Workspace(RelocateRenderElement::from_element(
                element.into(),
                offset,
                Relocate::Relative,
            ))
        })
    }
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
    exclude_workspace_overview: bool,
    _fps: Option<(&EguiState, &Timings)>,
) -> Result<Vec<CosmicElement<R>>, RenderError<R>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: Send + Clone + 'static,
    <R as Renderer>::Error: FromGlesError,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let mut elements = SplitRenderElements::default();

    let theme = shell.read().unwrap().theme().clone();
    let seats = shell
        .read()
        .unwrap()
        .seats
        .iter()
        .cloned()
        .collect::<Vec<_>>();

    elements.p_elements.extend(cursor_elements(
        renderer,
        seats.iter(),
        &theme,
        now,
        output,
        cursor_mode,
        exclude_workspace_overview,
    ));

    #[cfg(feature = "debug")]
    {
        let output_geo = output.geometry();
        let scale = output.current_scale().fractional_scale();

        if let Some((state, timings)) = _fps {
            let debug_active = shell.read().unwrap().debug_active;
            let fps_overlay = fps_ui(
                _gpu,
                debug_active,
                seats.iter(),
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
            .map_err(RenderError::Rendering)?;
            elements.p_elements.push(fps_overlay.into());
        }
    }

    let shell = shell.read().unwrap();

    // If session locked, only show session lock surfaces
    if let Some(session_lock) = &shell.session_lock {
        elements.p_elements.extend(
            session_lock_elements(renderer, output, session_lock)
                .into_iter()
                .map(|x| WorkspaceRenderElement::from(x).into()),
        );
        return Ok(elements.join());
    }

    let theme = theme.cosmic();
    let overview = shell.overview_mode();
    let (resize_mode, resize_indicator) = shell.resize_mode();
    let resize_indicator = resize_indicator.map(|indicator| (resize_mode, indicator));
    let swap_tree = if let OverviewMode::Started(Trigger::KeyboardSwap(_, desc), _) = &overview.0 {
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
    let active_output = last_active_seat.active_output();
    let output_size = output.geometry().size;
    let output_scale = output.current_scale().fractional_scale();

    let set = shell.workspaces.sets.get(output).ok_or(OutputNoMode)?;
    let workspace = set
        .workspaces
        .iter()
        .find(|w| w.handle == current.0)
        .ok_or(OutputNoMode)?;
    let is_active_space = workspace.outputs().any(|o| o == &active_output);

    let has_fullscreen = workspace
        .fullscreen
        .as_ref()
        .filter(|f| !f.is_animating())
        .is_some();
    let overlay_elements =
        split_layer_elements(renderer, output, Layer::Overlay, exclude_workspace_overview);

    // overlay is above everything
    elements
        .p_elements
        .extend(overlay_elements.p_elements.into_iter().map(Into::into));
    elements
        .p_elements
        .extend(overlay_elements.w_elements.into_iter().map(Into::into));

    if !has_fullscreen {
        elements.extend_from_workspace_elements(
            split_layer_elements(renderer, output, Layer::Top, exclude_workspace_overview),
            (0, 0).into(),
        );
    };

    let active_hint = if shell.active_hint {
        theme.active_hint as u8
    } else {
        0
    };

    // overlay redirect windows
    // they need to be over sticky windows, because they could be popups of sticky windows,
    // and we can't differenciate that.
    elements.p_elements.extend(
        shell
            .override_redirect_windows
            .iter()
            .filter(|or| {
                (*or)
                    .geometry()
                    .as_global()
                    .intersection(workspace.output.geometry())
                    .is_some()
            })
            .flat_map(|or| {
                AsRenderElements::<R>::render_elements::<WorkspaceRenderElement<R>>(
                    or,
                    renderer,
                    (or.geometry().loc - workspace.output.geometry().loc.as_logical())
                        .to_physical_precise_round(output_scale),
                    Scale::from(output_scale),
                    1.0,
                )
            })
            .map(|p_element| p_element.into()),
    );

    // sticky windows
    if !has_fullscreen {
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
            OverviewMode::None => 1.0,
        };

        let current_focus = (!move_active && is_active_space)
            .then_some(last_active_seat)
            .map(|seat| workspace.focus_stack.get(seat));

        elements.extend_from_workspace_elements(
            set.sticky_layer.render(
                renderer,
                current_focus.as_ref().and_then(|stack| stack.last()),
                resize_indicator.clone(),
                active_hint,
                alpha,
                theme,
            ),
            (0, 0).into(),
        );
    }

    let offset = match previous.as_ref() {
        Some((previous, previous_idx, start)) => {
            let layout = shell.workspaces.layout;

            let workspace = shell
                .workspaces
                .space_for_handle(&previous)
                .ok_or(OutputNoMode)?;
            let has_fullscreen = workspace.fullscreen.is_some();
            let is_active_space = workspace.outputs().any(|o| o == &active_output);

            let percentage = match start {
                WorkspaceDelta::Shortcut(st) => ease(
                    EaseInOutCubic,
                    0.0,
                    1.0,
                    Instant::now().duration_since(*st).as_millis() as f32
                        / ANIMATION_DURATION.as_millis() as f32,
                ),
                WorkspaceDelta::Gesture(prog) => *prog as f32,
                WorkspaceDelta::GestureEnd(st, spring) => {
                    (spring.value_at(Instant::now().duration_since(*st)) as f32).clamp(0.0, 1.0)
                }
            };
            let offset = Point::<i32, Logical>::from(match (layout, *previous_idx < current.1) {
                (WorkspaceLayout::Vertical, true) => {
                    (0, (-output_size.h as f32 * percentage).round() as i32)
                }
                (WorkspaceLayout::Vertical, false) => {
                    (0, (output_size.h as f32 * percentage).round() as i32)
                }
                (WorkspaceLayout::Horizontal, true) => {
                    ((-output_size.w as f32 * percentage).round() as i32, 0)
                }
                (WorkspaceLayout::Horizontal, false) => {
                    ((output_size.w as f32 * percentage).round() as i32, 0)
                }
            });

            elements.extend_from_workspace_elements(
                workspace
                    .render::<R>(
                        renderer,
                        (!move_active && is_active_space).then_some(last_active_seat),
                        overview.clone(),
                        resize_indicator.clone(),
                        active_hint,
                        theme,
                    )
                    .map_err(|_| OutputNoMode)?,
                offset.to_physical_precise_round(output_scale),
            );

            if !has_fullscreen {
                elements.extend_from_workspace_elements(
                    background_layer_elements(renderer, output, exclude_workspace_overview),
                    offset.to_physical_precise_round(output_scale),
                );
            }

            Point::<i32, Logical>::from(match (layout, *previous_idx < current.1) {
                (WorkspaceLayout::Vertical, true) => (0, output_size.h + offset.y),
                (WorkspaceLayout::Vertical, false) => (0, -(output_size.h - offset.y)),
                (WorkspaceLayout::Horizontal, true) => (output_size.w + offset.x, 0),
                (WorkspaceLayout::Horizontal, false) => (-(output_size.w - offset.x), 0),
            })
        }
        None => (0, 0).into(),
    };

    elements.extend_from_workspace_elements(
        workspace
            .render::<R>(
                renderer,
                (!move_active && is_active_space).then_some(&last_active_seat),
                overview,
                resize_indicator,
                active_hint,
                theme,
            )
            .map_err(|_| OutputNoMode)?,
        offset.to_physical_precise_round(output_scale),
    );

    if !has_fullscreen {
        elements.extend_from_workspace_elements(
            background_layer_elements(renderer, output, exclude_workspace_overview),
            offset.to_physical_precise_round(output_scale),
        );
    }

    Ok(elements.join())
}

pub fn split_layer_elements<R>(
    renderer: &mut R,
    output: &Output,
    layer: Layer,
    exclude_workspace_overview: bool,
) -> SplitRenderElements<WorkspaceRenderElement<R>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: Clone + 'static,
    <R as Renderer>::Error: FromGlesError,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let layer_map = layer_map_for_output(output);
    let output_scale = output.current_scale().fractional_scale();

    let mut elements = SplitRenderElements::default();

    layer_map
        .layers_on(layer)
        .rev()
        .filter(|s| !(exclude_workspace_overview && s.namespace() == WORKSPACE_OVERVIEW_NAMESPACE))
        .filter_map(|surface| {
            layer_map
                .layer_geometry(surface)
                .map(|geo| (geo.loc, surface))
        })
        .for_each(|(location, surface)| {
            let location = location.to_physical_precise_round(output_scale);
            let surface = surface.wl_surface();
            let scale = Scale::from(output_scale);

            elements
                .p_elements
                .extend(PopupManager::popups_for_surface(surface).flat_map(
                    |(popup, popup_offset)| {
                        let offset = (popup_offset - popup.geometry().loc)
                            .to_f64()
                            .to_physical(scale)
                            .to_i32_round();

                        render_elements_from_surface_tree(
                            renderer,
                            popup.wl_surface(),
                            location + offset,
                            scale,
                            1.0,
                            Kind::Unspecified,
                        )
                    },
                ));

            elements
                .w_elements
                .extend(render_elements_from_surface_tree(
                    renderer,
                    surface,
                    location,
                    scale,
                    1.0,
                    Kind::Unspecified,
                ));
        });

    elements
}

// bottom and background layer surfaces
pub fn background_layer_elements<R>(
    renderer: &mut R,
    output: &Output,
    exclude_workspace_overview: bool,
) -> SplitRenderElements<WorkspaceRenderElement<R>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: Clone + 'static,
    <R as Renderer>::Error: FromGlesError,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let mut elements =
        split_layer_elements(renderer, output, Layer::Bottom, exclude_workspace_overview);
    elements.extend(split_layer_elements(
        renderer,
        output,
        Layer::Background,
        exclude_workspace_overview,
    ));
    elements
}

fn session_lock_elements<R>(
    renderer: &mut R,
    output: &Output,
    session_lock: &SessionLock,
) -> Vec<WaylandSurfaceRenderElement<R>>
where
    R: Renderer + ImportAll,
    <R as Renderer>::TextureId: Clone + 'static,
{
    if let Some(surface) = session_lock.surfaces.get(output) {
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
) -> Result<RenderOutputResult<'d>, RenderError<R>>
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
    let (previous_workspace, workspace) = shell_ref.workspaces.active(output);
    let (previous_idx, idx) = shell_ref.workspaces.active_num(output);
    let previous_workspace = previous_workspace
        .zip(previous_idx)
        .map(|((w, start), idx)| (w.handle, idx, start));
    let workspace = (workspace.handle, idx);
    std::mem::drop(shell_ref);

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
        false,
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
    exclude_workspace_overview: bool,
) -> Result<(RenderOutputResult<'d>, Vec<CosmicElement<R>>), RenderError<R>>
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
        exclude_workspace_overview,
        None,
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
