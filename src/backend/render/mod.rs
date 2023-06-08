// SPDX-License-Identifier: GPL-3.0-only

use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
    collections::HashMap,
    sync::Weak,
    time::Instant,
};

use crate::{
    config::WorkspaceLayout,
    shell::{
        focus::target::WindowGroup,
        layout::{floating::SeatMoveGrabState, tiling::ANIMATION_DURATION},
        CosmicMapped, CosmicMappedRenderElement, WorkspaceRenderElement,
    },
    state::{Common, Fps},
    utils::prelude::{OutputExt, SeatExt},
    wayland::{
        handlers::{
            data_device::get_dnd_icon,
            screencopy::{render_session, WORKSPACE_OVERVIEW_NAMESPACE},
        },
        protocols::{
            screencopy::{
                BufferParams, CursorMode as ScreencopyCursorMode, Session as ScreencopySession,
            },
            workspace::WorkspaceHandle,
        },
    },
};
#[cfg(feature = "debug")]
use crate::{
    debug::{fps_ui, profiler_ui},
    utils::prelude::*,
};

use cosmic_protocols::screencopy::v1::server::zcosmic_screencopy_session_v1::FailureReason;
use cosmic_time::{Cubic, Ease, Tween};
use smithay::{
    backend::{
        allocator::dmabuf::Dmabuf,
        drm::DrmNode,
        renderer::{
            buffer_dimensions,
            damage::{Error as RenderError, OutputDamageTracker, OutputNoMode},
            element::{
                utils::{Relocate, RelocateRenderElement},
                AsRenderElements, Element, Id, RenderElement, RenderElementStates,
            },
            gles::{
                element::PixelShaderElement, GlesError, GlesPixelProgram, GlesRenderer, Uniform,
                UniformName, UniformType,
            },
            glow::GlowRenderer,
            multigpu::{gbm::GbmGlesBackend, Error as MultiError, MultiFrame, MultiRenderer},
            Bind, Blit, ExportMem, ImportAll, ImportMem, Offscreen, Renderer, TextureFilter,
        },
    },
    desktop::layer_map_for_output,
    output::Output,
    utils::{IsAlive, Logical, Physical, Point, Rectangle, Scale},
    wayland::{
        dmabuf::get_dmabuf,
        shell::wlr_layer::Layer,
        shm::{shm_format_to_fourcc, with_buffer_contents},
    },
};
use tracing::warn;

pub mod cursor;
use self::cursor::CursorRenderElement;
pub mod element;
use self::element::{AsGlowRenderer, CosmicElement};

pub type GlMultiRenderer<'a, 'b> =
    MultiRenderer<'a, 'a, 'b, GbmGlesBackend<GlowRenderer>, GbmGlesBackend<GlowRenderer>>;
pub type GlMultiFrame<'a, 'b, 'frame> =
    MultiFrame<'a, 'a, 'b, 'frame, GbmGlesBackend<GlowRenderer>, GbmGlesBackend<GlowRenderer>>;
pub type GlMultiError = MultiError<GbmGlesBackend<GlowRenderer>, GbmGlesBackend<GlowRenderer>>;

pub static CLEAR_COLOR: [f32; 4] = [0.153, 0.161, 0.165, 1.0];
pub static GROUP_COLOR: [f32; 3] = [0.788, 0.788, 0.788];
pub static FOCUS_INDICATOR_COLOR: [f32; 3] = [0.580, 0.921, 0.921];

pub static OUTLINE_SHADER: &str = include_str!("./shaders/rounded_outline.frag");
pub static RECTANGLE_SHADER: &str = include_str!("./shaders/rounded_rectangle.frag");

pub struct IndicatorShader(pub GlesPixelProgram);

#[derive(Clone)]
pub enum Key {
    Static(Id),
    Group(Weak<()>),
    Window(CosmicMapped),
}
impl std::hash::Hash for Key {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Key::Static(id) => id.hash(state),
            Key::Group(arc) => (arc.as_ptr() as usize).hash(state),
            Key::Window(window) => window.hash(state),
        }
    }
}
impl PartialEq for Key {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Key::Static(s1), Key::Static(s2)) => s1 == s2,
            (Key::Group(g1), Key::Group(g2)) => Weak::ptr_eq(g1, g2),
            (Key::Window(w1), Key::Window(w2)) => w1 == w2,
            _ => false,
        }
    }
}
impl Eq for Key {}
impl From<CosmicMapped> for Key {
    fn from(window: CosmicMapped) -> Self {
        Key::Window(window)
    }
}
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
        mut element_geo: Rectangle<i32, Logical>,
        thickness: u8,
        alpha: f32,
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
            FOCUS_INDICATOR_COLOR,
        )
    }

    pub fn element<R: AsGlowRenderer>(
        renderer: &R,
        key: impl Into<Key>,
        geo: Rectangle<i32, Logical>,
        thickness: u8,
        radius: u8,
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
            Key::Static(_) => true,
            Key::Group(w) => w.upgrade().is_some(),
            Key::Window(w) => w.alive(),
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
                geo,
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
            );
            cache.insert(key.clone(), (settings, elem));
        }

        let elem = &mut cache.get_mut(&key).unwrap().1;
        if elem.geometry(1.0.into()).to_logical(1) != geo {
            elem.resize(geo, None);
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
        geo: Rectangle<i32, Logical>,
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
            Key::Window(w) => w.alive(),
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
                geo,
                None, // TODO
                alpha,
                vec![
                    Uniform::new(
                        "color",
                        [color[0] * alpha, color[1] * alpha, color[2] * alpha],
                    ),
                    Uniform::new("radius", radius),
                ],
            );
            cache.insert(key.clone(), (settings, elem));
        }

        let elem = &mut cache.get_mut(&key).unwrap().1;
        if elem.geometry(1.0.into()).to_logical(1) != geo {
            elem.resize(geo, None);
        }
        elem.clone()
    }
}

pub fn init_shaders<R: AsGlowRenderer>(renderer: &mut R) -> Result<(), GlesError> {
    let glow_renderer = renderer.glow_renderer_mut();
    let gles_renderer: &mut GlesRenderer = glow_renderer.borrow_mut();

    let outline_shader = gles_renderer.compile_custom_pixel_shader(
        OUTLINE_SHADER,
        &[
            UniformName::new("color", UniformType::_3f),
            UniformName::new("thickness", UniformType::_1f),
            UniformName::new("radius", UniformType::_1f),
        ],
    )?;
    let rectangle_shader = gles_renderer.compile_custom_pixel_shader(
        RECTANGLE_SHADER,
        &[
            UniformName::new("color", UniformType::_3f),
            UniformName::new("radius", UniformType::_1f),
        ],
    )?;

    let egl_context = gles_renderer.egl_context();
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

pub fn cursor_elements<'frame, E, R>(
    renderer: &mut R,
    state: &Common,
    output: &Output,
    mode: CursorMode,
) -> Vec<E>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: Clone + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    E: From<CursorRenderElement<R>> + From<CosmicMappedRenderElement<R>>,
{
    #[cfg(feature = "debug")]
    puffin::profile_function!();

    let scale = output.current_scale().fractional_scale();
    let mut elements = Vec::new();

    for seat in state.seats() {
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
                    state.clock.now(),
                    mode != CursorMode::NotDefault,
                )
                .into_iter()
                .map(E::from),
            );
        }

        if let Some(wl_surface) = get_dnd_icon(seat) {
            elements.extend(
                cursor::draw_dnd_icon(renderer, &wl_surface, location.to_i32_round(), scale)
                    .into_iter()
                    .map(E::from),
            );
        }

        if let Some(grab_elements) = seat
            .user_data()
            .get::<SeatMoveGrabState>()
            .unwrap()
            .borrow()
            .as_ref()
            .map(|state| state.render::<E, R>(renderer, seat, output))
        {
            elements.extend(grab_elements);
        }
    }

    elements
}

pub fn workspace_elements<R>(
    _gpu: Option<&DrmNode>,
    renderer: &mut R,
    state: &mut Common,
    output: &Output,
    previous: Option<(WorkspaceHandle, usize, Instant)>,
    current: (WorkspaceHandle, usize),
    cursor_mode: CursorMode,
    _fps: &mut Option<&mut Fps>,
    exclude_workspace_overview: bool,
) -> Result<Vec<CosmicElement<R>>, RenderError<R>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: Clone + 'static,
    <R as Renderer>::Error: From<GlesError>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    #[cfg(feature = "debug")]
    puffin::profile_function!();

    let mut elements = cursor_elements(renderer, state, output, cursor_mode);

    #[cfg(feature = "debug")]
    {
        let output_geo = output.geometry();
        let scale = output.current_scale().fractional_scale();

        if let Some(fps) = _fps.as_mut() {
            let fps_overlay = fps_ui(
                _gpu,
                state,
                renderer.glow_renderer_mut(),
                *fps,
                Rectangle::from_loc_and_size(
                    (0, 0),
                    (output_geo.size.w.min(400), output_geo.size.h.min(800)),
                ),
                scale,
            )
            .map_err(<R as Renderer>::Error::from)
            .map_err(RenderError::Rendering)?;
            elements.push(fps_overlay.into());
        }

        if state.shell.outputs.first() == Some(output) {
            if let Some(profiler_overlay) = profiler_ui(
                state,
                renderer.glow_renderer_mut(),
                Rectangle::from_loc_and_size((0, 0), output_geo.size),
                scale,
            )
            .map_err(<R as Renderer>::Error::from)
            .map_err(RenderError::Rendering)?
            {
                elements.push(profiler_overlay.into());
            }
        }
    }

    state
        .shell
        .space_for_handle_mut(&current.0)
        .ok_or(OutputNoMode)?
        .update_animations(&state.event_loop_handle);
    if let Some((previous, _, _)) = previous.as_ref() {
        state
            .shell
            .space_for_handle_mut(&previous)
            .ok_or(OutputNoMode)?
            .update_animations(&state.event_loop_handle);
    }
    let overview = state.shell.overview_mode();
    let last_active_seat = state.last_active_seat().clone();
    let move_active = last_active_seat
        .user_data()
        .get::<SeatMoveGrabState>()
        .unwrap()
        .borrow()
        .is_some();
    let active_output = last_active_seat.active_output();
    let output_size = output.geometry().size;
    let output_scale = output.current_scale().fractional_scale();

    let workspace = state
        .shell
        .space_for_handle(&current.0)
        .ok_or(OutputNoMode)?;
    let has_fullscreen = workspace.fullscreen.contains_key(output);

    // foreground layers are static
    elements.extend(
        foreground_layer_elements(renderer, output, has_fullscreen, exclude_workspace_overview)
            .into_iter()
            .map(Into::into),
    );

    let offset = match previous.as_ref() {
        Some((previous, previous_idx, start)) => {
            let layout = state.config.static_conf.workspace_layout;

            let workspace = state
                .shell
                .space_for_handle(&previous)
                .ok_or(OutputNoMode)?;
            let is_active_space = workspace.outputs().any(|o| o == &active_output);

            let percentage = {
                let percentage = Instant::now().duration_since(*start).as_millis() as f32
                    / ANIMATION_DURATION.as_millis() as f32;
                Ease::Cubic(Cubic::InOut).tween(percentage)
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

            elements.extend(
                workspace
                    .render_output::<R>(
                        renderer,
                        output,
                        &state.shell.override_redirect_windows,
                        state.xwayland_state.as_mut(),
                        (!move_active && is_active_space).then_some(&last_active_seat),
                        overview.clone(),
                        state.config.static_conf.active_hint,
                    )
                    .map_err(|_| OutputNoMode)?
                    .into_iter()
                    .map(|w_element| {
                        CosmicElement::Workspace(RelocateRenderElement::from_element(
                            w_element,
                            offset.to_physical_precise_round(output_scale),
                            Relocate::Relative,
                        ))
                    }),
            );

            elements.extend(
                background_layer_elements(renderer, output, exclude_workspace_overview)
                    .into_iter()
                    .map(|w_element| {
                        CosmicElement::Workspace(RelocateRenderElement::from_element(
                            w_element,
                            offset.to_physical_precise_round(output_scale),
                            Relocate::Relative,
                        ))
                    }),
            );

            Point::<i32, Logical>::from(match (layout, *previous_idx < current.1) {
                (WorkspaceLayout::Vertical, true) => (0, output_size.h + offset.y),
                (WorkspaceLayout::Vertical, false) => (0, -(output_size.h - offset.y)),
                (WorkspaceLayout::Horizontal, true) => (output_size.w + offset.x, 0),
                (WorkspaceLayout::Horizontal, false) => (-(output_size.w - offset.y), 0),
            })
        }
        None => (0, 0).into(),
    };

    let is_active_space = workspace.outputs().any(|o| o == &active_output);

    elements.extend(
        workspace
            .render_output::<R>(
                renderer,
                output,
                &state.shell.override_redirect_windows,
                state.xwayland_state.as_mut(),
                (!move_active && is_active_space).then_some(&last_active_seat),
                overview,
                state.config.static_conf.active_hint,
            )
            .map_err(|_| OutputNoMode)?
            .into_iter()
            .map(|w_element| {
                CosmicElement::Workspace(RelocateRenderElement::from_element(
                    w_element,
                    offset.to_physical_precise_round(output_scale),
                    Relocate::Relative,
                ))
            }),
    );

    elements.extend(
        background_layer_elements(renderer, output, exclude_workspace_overview)
            .into_iter()
            .map(|w_element| {
                CosmicElement::Workspace(RelocateRenderElement::from_element(
                    w_element,
                    offset.to_physical_precise_round(output_scale),
                    Relocate::Relative,
                ))
            }),
    );

    Ok(elements)
}

// bottom and background layer surfaces
pub fn foreground_layer_elements<R>(
    renderer: &mut R,
    output: &Output,
    has_fullscreen: bool,
    exclude_workspace_overview: bool,
) -> Vec<WorkspaceRenderElement<R>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: Clone + 'static,
    <R as Renderer>::Error: From<GlesError>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let layer_map = layer_map_for_output(output);
    let output_scale = output.current_scale().fractional_scale();

    layer_map
        .layers()
        .rev()
        .filter(|s| !(exclude_workspace_overview && s.namespace() == WORKSPACE_OVERVIEW_NAMESPACE))
        .filter(|s| {
            if has_fullscreen {
                matches!(s.layer(), Layer::Overlay)
            } else {
                matches!(s.layer(), Layer::Top | Layer::Overlay)
            }
        })
        .filter_map(|surface| {
            layer_map
                .layer_geometry(surface)
                .map(|geo| (geo.loc, surface))
        })
        .flat_map(|(loc, surface)| {
            AsRenderElements::<R>::render_elements::<WorkspaceRenderElement<R>>(
                surface,
                renderer,
                loc.to_physical_precise_round(output_scale),
                Scale::from(output_scale),
                1.0,
            )
        })
        .collect()
}

// bottom and background layer surfaces
pub fn background_layer_elements<R>(
    renderer: &mut R,
    output: &Output,
    exclude_workspace_overview: bool,
) -> Vec<WorkspaceRenderElement<R>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: Clone + 'static,
    <R as Renderer>::Error: From<GlesError>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let layer_map = layer_map_for_output(output);
    let output_scale = output.current_scale().fractional_scale();

    layer_map
        .layers()
        .rev()
        .filter(|s| !(exclude_workspace_overview && s.namespace() == WORKSPACE_OVERVIEW_NAMESPACE))
        .filter(|s| matches!(s.layer(), Layer::Background | Layer::Bottom))
        .filter_map(|surface| {
            layer_map
                .layer_geometry(surface)
                .map(|geo| (geo.loc, surface))
        })
        .flat_map(|(loc, surface)| {
            AsRenderElements::<R>::render_elements::<WorkspaceRenderElement<R>>(
                surface,
                renderer,
                loc.to_physical_precise_round(output_scale),
                Scale::from(output_scale),
                1.0,
            )
        })
        .collect()
}

pub fn render_output<R, Target, OffTarget, Source>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    target: Target,
    damage_tracker: &mut OutputDamageTracker,
    age: usize,
    state: &mut Common,
    output: &Output,
    cursor_mode: CursorMode,
    screencopy: Option<(Source, &[(ScreencopySession, BufferParams)])>,
    fps: Option<&mut Fps>,
) -> Result<(Option<Vec<Rectangle<i32, Physical>>>, RenderElementStates), RenderError<R>>
where
    R: Renderer
        + ImportAll
        + ImportMem
        + ExportMem
        + Bind<Dmabuf>
        + Bind<Target>
        + Offscreen<OffTarget>
        + Blit<Source>
        + AsGlowRenderer,
    <R as Renderer>::TextureId: Clone + 'static,
    <R as Renderer>::Error: From<GlesError>,
    CosmicElement<R>: RenderElement<R>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
    Source: Clone,
{
    let (previous_workspace, workspace) = state.shell.workspaces.active(output);
    let (previous_idx, idx) = state.shell.workspaces.active_num(output);
    let previous_workspace = previous_workspace
        .zip(previous_idx)
        .map(|((w, start), idx)| (w.handle, idx, start));
    let workspace = (workspace.handle, idx);

    let result = render_workspace(
        gpu,
        renderer,
        target,
        damage_tracker,
        age,
        state,
        output,
        previous_workspace,
        workspace,
        cursor_mode,
        screencopy,
        fps,
        false,
    );

    result
}

pub fn render_workspace<R, Target, OffTarget, Source>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    target: Target,
    damage_tracker: &mut OutputDamageTracker,
    age: usize,
    state: &mut Common,
    output: &Output,
    previous: Option<(WorkspaceHandle, usize, Instant)>,
    current: (WorkspaceHandle, usize),
    mut cursor_mode: CursorMode,
    screencopy: Option<(Source, &[(ScreencopySession, BufferParams)])>,
    mut fps: Option<&mut Fps>,
    exclude_workspace_overview: bool,
) -> Result<(Option<Vec<Rectangle<i32, Physical>>>, RenderElementStates), RenderError<R>>
where
    R: Renderer
        + ImportAll
        + ImportMem
        + ExportMem
        + Bind<Dmabuf>
        + Bind<Target>
        + Offscreen<OffTarget>
        + Blit<Source>
        + AsGlowRenderer,
    <R as Renderer>::TextureId: Clone + 'static,
    <R as Renderer>::Error: From<GlesError>,
    CosmicElement<R>: RenderElement<R>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
    Source: Clone,
{
    #[cfg(feature = "debug")]
    puffin::profile_function!();

    if let Some(ref mut fps) = fps {
        fps.start();
        #[cfg(feature = "debug")]
        if let Some(rd) = fps.rd.as_mut() {
            rd.start_frame_capture(
                renderer.glow_renderer().egl_context().get_context_handle(),
                std::ptr::null(),
            );
        }
    }

    let screencopy_contains_embedded = screencopy.as_ref().map_or(false, |(_, sessions)| {
        sessions
            .iter()
            .any(|(s, _)| s.cursor_mode() == ScreencopyCursorMode::Embedded)
    });
    // cursor handling without a cursor_plane in this case is horrible.
    // because what if some session disagree and/or the backend wants to render with a different mode?
    // It seems we would need to render to an offscreen buffer in those cases (and do multiple renders, which messes with damage tracking).
    // So for now, we just pick the worst mode (embedded), if any requires it.
    //
    // Once we move to a cursor_plane, the default framebuffer will never contain a cursor and we can just composite the cursor for each session separately on top (or not).
    if screencopy_contains_embedded {
        cursor_mode = CursorMode::All;
    };

    let elements: Vec<CosmicElement<R>> = workspace_elements(
        gpu,
        renderer,
        state,
        output,
        previous,
        current,
        cursor_mode,
        &mut fps,
        exclude_workspace_overview,
    )?;
    if let Some(fps) = fps.as_mut() {
        fps.elements();
    }

    renderer.bind(target).map_err(RenderError::Rendering)?;
    let res = damage_tracker.render_output(renderer, age, &elements, CLEAR_COLOR);

    if let Some(fps) = fps.as_mut() {
        fps.render();
    }

    if let Some((source, buffers)) = screencopy {
        if res.is_ok() {
            for (session, params) in buffers {
                match render_session(
                    gpu.cloned(),
                    renderer,
                    &session,
                    params,
                    output.current_transform(),
                    |_node, buffer, renderer, dt, age| {
                        let res = dt.damage_output(age, &elements)?;

                        if let (Some(ref damage), _) = &res {
                            if let Ok(dmabuf) = get_dmabuf(buffer) {
                                renderer.bind(dmabuf).map_err(RenderError::Rendering)?;
                            } else {
                                let size = buffer_dimensions(buffer).unwrap();
                                let format =
                                            with_buffer_contents(buffer, |_, _, data| shm_format_to_fourcc(data.format))
                                                .map_err(|_| OutputNoMode)? // eh, we have to do some error
                                                .expect("We should be able to convert all hardcoded shm screencopy formats");
                                let render_buffer = renderer
                                    .create_buffer(format, size)
                                    .map_err(RenderError::Rendering)?;
                                renderer
                                    .bind(render_buffer)
                                    .map_err(RenderError::Rendering)?;
                            }
                            for rect in damage {
                                renderer
                                    .blit_from(source.clone(), *rect, *rect, TextureFilter::Nearest)
                                    .map_err(RenderError::Rendering)?;
                            }
                        }

                        Ok(res)
                    },
                ) {
                    Ok(true) => {} // success
                    Ok(false) => state.still_pending(session.clone(), params.clone()),
                    Err(err) => {
                        warn!(?err, "Error rendering to screencopy session.");
                        session.failed(FailureReason::Unspec);
                    }
                }
            }
        }
        if let Some(fps) = fps.as_mut() {
            fps.screencopy();
        }
    }

    #[cfg(feature = "debug")]
    if let Some(ref mut fps) = fps {
        if let Some(rd) = fps.rd.as_mut() {
            rd.end_frame_capture(
                renderer.glow_renderer().egl_context().get_context_handle(),
                std::ptr::null(),
            );
        }

        puffin::GlobalProfiler::lock().new_frame();
    }

    res
}
