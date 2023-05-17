// SPDX-License-Identifier: GPL-3.0-only

use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
    collections::HashMap,
    sync::Weak,
};

#[cfg(feature = "debug")]
use crate::{
    debug::{fps_ui, profiler_ui},
    utils::prelude::*,
};
use crate::{
    shell::{
        element::window::CosmicWindowRenderElement, focus::target::WindowGroup,
        layout::floating::SeatMoveGrabState, CosmicMapped, CosmicMappedRenderElement,
    },
    state::{Common, Fps},
    utils::prelude::SeatExt,
    wayland::{
        handlers::{data_device::get_dnd_icon, screencopy::render_session},
        protocols::{
            screencopy::{
                BufferParams, CursorMode as ScreencopyCursorMode, Session as ScreencopySession,
            },
            workspace::WorkspaceHandle,
        },
    },
};

use cosmic_protocols::screencopy::v1::server::zcosmic_screencopy_session_v1::FailureReason;
use smithay::{
    backend::{
        allocator::dmabuf::Dmabuf,
        drm::DrmNode,
        renderer::{
            buffer_dimensions,
            damage::{Error as RenderError, OutputDamageTracker, OutputNoMode},
            element::{Element, RenderElement, RenderElementStates},
            gles::{
                element::PixelShaderElement, GlesError, GlesPixelProgram, GlesRenderer, Uniform,
                UniformName, UniformType,
            },
            glow::GlowRenderer,
            multigpu::{gbm::GbmGlesBackend, MultiFrame, MultiRenderer},
            Bind, Blit, ExportMem, ImportAll, ImportMem, Offscreen, Renderer, TextureFilter,
        },
    },
    output::Output,
    utils::{IsAlive, Logical, Physical, Point, Rectangle, Size},
    wayland::{
        dmabuf::get_dmabuf,
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

pub static CLEAR_COLOR: [f32; 4] = [0.153, 0.161, 0.165, 1.0];
pub static ACTIVE_GROUP_COLOR: [f32; 3] = [0.678, 0.635, 0.619];
pub static GROUP_COLOR: [f32; 3] = [0.431, 0.404, 0.396];
pub static FOCUS_INDICATOR_COLOR: [f32; 3] = [0.580, 0.921, 0.921];
pub static FOCUS_INDICATOR_SHADER: &str = include_str!("./shaders/focus_indicator.frag");

pub struct IndicatorShader(pub GlesPixelProgram);

#[derive(Clone)]
pub enum Key {
    Group(Weak<()>),
    Window(CosmicMapped),
}
impl std::hash::Hash for Key {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Key::Group(arc) => (arc.as_ptr() as usize).hash(state),
            Key::Window(window) => window.hash(state),
        }
    }
}
impl PartialEq for Key {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
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

#[derive(PartialEq)]
struct IndicatorSettings {
    thickness: u8,
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

    pub fn element<R: AsGlowRenderer>(
        renderer: &R,
        key: impl Into<Key>,
        geo: Rectangle<i32, Logical>,
        thickness: u8,
        alpha: f32,
        color: [f32; 3],
    ) -> PixelShaderElement {
        let settings = IndicatorSettings {
            thickness,
            alpha,
            color,
        };

        let user_data = Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data();

        user_data.insert_if_missing(|| IndicatorCache::new(HashMap::new()));
        let mut cache = user_data.get::<IndicatorCache>().unwrap().borrow_mut();
        cache.retain(|k, _| match k {
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
            let thickness_loc = (thickness as i32, thickness as i32);
            let thickness_size = ((thickness * 2.0) as i32, (thickness * 2.0) as i32);
            let geo = Rectangle::from_loc_and_size(
                geo.loc - Point::from(thickness_loc),
                geo.size + Size::from(thickness_size),
            );
            let shader = Self::get(renderer);

            let elem = PixelShaderElement::new(
                shader,
                geo,
                None, //TODO
                alpha,
                vec![
                    Uniform::new("color", color),
                    Uniform::new("thickness", thickness),
                    Uniform::new("radius", thickness * 2.0),
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

    let indicator_shader = gles_renderer.compile_custom_pixel_shader(
        FOCUS_INDICATOR_SHADER,
        &[
            UniformName::new("color", UniformType::_3f),
            UniformName::new("thickness", UniformType::_1f),
            UniformName::new("radius", UniformType::_1f),
        ],
    )?;

    let egl_context = gles_renderer.egl_context();
    egl_context
        .user_data()
        .insert_if_missing(|| IndicatorShader(indicator_shader));

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
    CosmicWindowRenderElement<R>: RenderElement<R>,
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
    handle: &WorkspaceHandle,
    cursor_mode: CursorMode,
    _fps: &mut Option<&mut Fps>,
    exclude_workspace_overview: bool,
) -> Result<Vec<CosmicElement<R>>, RenderError<R>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: Clone + 'static,
    <R as Renderer>::Error: From<GlesError>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    CosmicWindowRenderElement<R>: RenderElement<R>,
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
        .space_for_handle_mut(&handle)
        .ok_or(OutputNoMode)?
        .update_animations(&state.event_loop_handle);
    let workspace = state.shell.space_for_handle(&handle).ok_or(OutputNoMode)?;
    let last_active_seat = state.last_active_seat().clone();
    let move_active = last_active_seat
        .user_data()
        .get::<SeatMoveGrabState>()
        .unwrap()
        .borrow()
        .is_some();
    let active_output = last_active_seat.active_output();
    let is_active_space = workspace.outputs().any(|o| o == &active_output);

    elements.extend(
        workspace
            .render_output::<R>(
                renderer,
                output,
                &state.shell.override_redirect_windows,
                state.xwayland_state.as_mut(),
                (!move_active && is_active_space).then_some(&last_active_seat),
                true,
                state.config.static_conf.active_hint,
                exclude_workspace_overview,
            )
            .map_err(|_| OutputNoMode)?
            .into_iter()
            .map(Into::into),
    );

    Ok(elements)
}

pub fn render_output<'frame, R, Target, OffTarget, Source>(
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
    CosmicWindowRenderElement<R>: RenderElement<R>,
    Source: Clone,
{
    let handle = state.shell.workspaces.active(output).handle;
    let result = render_workspace(
        gpu,
        renderer,
        target,
        damage_tracker,
        age,
        state,
        output,
        &handle,
        cursor_mode,
        screencopy,
        fps,
        false,
    );

    result
}

pub fn render_workspace<'frame, R, Target, OffTarget, Source>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    target: Target,
    damage_tracker: &mut OutputDamageTracker,
    age: usize,
    state: &mut Common,
    output: &Output,
    handle: &WorkspaceHandle,
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
    CosmicWindowRenderElement<R>: RenderElement<R>,
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
        handle,
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
