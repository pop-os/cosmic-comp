use smithay::{
    backend::{
        allocator::{dmabuf::Dmabuf, format::get_transparent, Buffer, Fourcc},
        renderer::{
            buffer_dimensions, buffer_type,
            damage::{Error as DTError, OutputDamageTracker, RenderOutputResult},
            element::{
                surface::WaylandSurfaceRenderElement,
                utils::{Relocate, RelocateRenderElement},
                AsRenderElements, RenderElement,
            },
            gles::{GlesError, GlesRenderbuffer},
            sync::SyncPoint,
            utils::with_renderer_surface_state,
            Bind, Blit, BufferType, ExportMem, ImportAll, ImportMem, Offscreen, Renderer,
        },
    },
    desktop::space::SpaceElement,
    input::Seat,
    output::{Output, OutputNoMode},
    reexports::wayland_server::protocol::{wl_buffer::WlBuffer, wl_shm::Format as ShmFormat},
    utils::{
        Buffer as BufferCoords, IsAlive, Logical, Physical, Point, Rectangle, Scale, Size,
        Transform,
    },
    wayland::{
        dmabuf::get_dmabuf,
        seat::WaylandFocus,
        shm::{shm_format_to_fourcc, with_buffer_contents, with_buffer_contents_mut},
    },
};
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use tracing::warn;

use crate::{
    backend::render::{
        cursor,
        element::{AsGlowRenderer, CosmicElement, DamageElement},
        render_workspace, CursorMode, CLEAR_COLOR,
    },
    shell::{CosmicMappedRenderElement, CosmicSurface, WorkspaceRenderElement},
    state::{BackendData, Common, State},
    utils::prelude::SeatExt,
    wayland::{
        handlers::screencopy::{
            constraints_for_output, constraints_for_toplevel, SessionData, SessionUserData,
        },
        protocols::{
            screencopy::{BufferConstraints, CursorSession, FailureReason, Frame, Session},
            workspace::WorkspaceHandle,
        },
    },
};

use super::super::data_device::get_dnd_icon;

pub fn submit_buffer<R>(
    frame: Frame,
    renderer: &mut R,
    transform: Transform,
    damage: Option<Vec<Rectangle<i32, Physical>>>,
    sync: SyncPoint,
) -> Result<Option<(Frame, Vec<Rectangle<i32, BufferCoords>>)>, <R as Renderer>::Error>
where
    R: ExportMem,
{
    let Some(damage) = damage else {
        frame.success(
            transform,
            None,
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or(Duration::ZERO),
        );
        return Ok(None);
    };

    let buffer = frame.buffer();
    if matches!(buffer_type(&buffer), Some(BufferType::Shm)) {
        let buffer_size = buffer_dimensions(&buffer).unwrap();
        if let Err(err) = with_buffer_contents_mut(&buffer, |ptr, len, data| {
            let offset = data.offset as i32;
            let width = data.width as i32;
            let height = data.height as i32;
            let stride = data.stride as i32;
            let format = shm_format_to_fourcc(data.format)
                .expect("We should be able to convert all hardcoded shm screencopy formats");

            // number of bytes per pixel
            // TODO: compute from data.format
            let pixelsize = 4i32;

            // ensure consistency, the SHM handler of smithay should ensure this
            assert!((offset + (height - 1) * stride + width * pixelsize) as usize <= len);

            // ensure rendering is done
            renderer.wait(&sync)?;

            let format = get_transparent(format).unwrap_or(format);
            let mapping = renderer
                .copy_framebuffer(Rectangle::from_loc_and_size((0, 0), buffer_size), format)?;
            let gl_data = renderer.map_texture(&mapping)?;
            assert!((width * height * pixelsize) as usize <= gl_data.len());

            for i in 0..height {
                unsafe {
                    std::ptr::copy_nonoverlapping::<u8>(
                        gl_data.as_ptr().offset((width * pixelsize * i) as isize),
                        ptr.offset((offset + stride * i) as isize),
                        (width * pixelsize) as usize,
                    );
                }
            }
            Ok(())
        })
        .unwrap()
        {
            frame.fail(FailureReason::Unknown);
            return Err(err);
        }
    }

    Ok(Some((
        frame,
        damage
            .into_iter()
            .map(|rect| {
                let logical = rect.to_logical(1);
                logical.to_buffer(1, transform, &logical.size)
            })
            .collect(),
    )))
}

pub fn render_session<F, R>(
    renderer: &mut R,
    session: &SessionData,
    frame: Frame,
    transform: Transform,
    render_fn: F,
) -> Result<Option<(Frame, Vec<Rectangle<i32, BufferCoords>>)>, DTError<R>>
where
    R: ExportMem,
    F: FnOnce(
        &WlBuffer,
        &mut R,
        &mut OutputDamageTracker,
        usize,
        Vec<Rectangle<i32, BufferCoords>>,
    ) -> Result<RenderOutputResult, DTError<R>>,
{
    #[cfg(feature = "debug")]
    puffin::profile_function!();

    let mut session_damage_tracking = session.borrow_mut();

    let buffer = frame.buffer();
    let age = session_damage_tracking.age_for_buffer(&buffer);
    let res = render_fn(
        &frame.buffer(),
        renderer,
        &mut session_damage_tracking.dt,
        age,
        frame.damage(),
    );

    match res {
        Ok(result) => submit_buffer(frame, renderer, transform, result.damage, result.sync)
            .map_err(DTError::Rendering),
        Err(err) => {
            frame.fail(FailureReason::Unknown);
            Err(err)
        }
    }
}

pub fn render_workspace_to_buffer(
    state: &mut State,
    session: Session,
    frame: Frame,
    handle: WorkspaceHandle,
) {
    #[cfg(feature = "debug")]
    puffin::profile_function!();

    let Some(workspace) = state.common.shell.workspaces.space_for_handle(&handle) else {
        session.stop();
        return;
    };

    let output = workspace.output().clone();
    let idx = state
        .common
        .shell
        .workspaces
        .idx_for_handle(&output, &handle)
        .unwrap();
    let mode = output
        .current_mode()
        .map(|mode| mode.size.to_logical(1).to_buffer(1, Transform::Normal));

    let buffer = frame.buffer();
    let buffer_size = buffer_dimensions(&buffer).unwrap();
    if mode != Some(buffer_size) {
        let Some(constraints) = constraints_for_output(&output, &mut state.backend) else {
            session.stop();
            return;
        };
        session.update_constraints(constraints);
        if let Some(data) = session.user_data().get::<SessionData>() {
            *data.borrow_mut() = SessionUserData::new(OutputDamageTracker::from_output(&output));
        }
        frame.fail(FailureReason::BufferConstraints);
        return;
    }

    fn render_fn<R>(
        buffer: &WlBuffer,
        renderer: &mut R,
        dt: &mut OutputDamageTracker,
        age: usize,
        additional_damage: Vec<Rectangle<i32, BufferCoords>>,
        draw_cursor: bool,
        common: &mut Common,
        output: &Output,
        handle: (WorkspaceHandle, usize),
    ) -> Result<RenderOutputResult, DTError<R>>
    where
        R: Renderer
            + ImportAll
            + ImportMem
            + ExportMem
            + Bind<Dmabuf>
            + Offscreen<GlesRenderbuffer>
            + Blit<Dmabuf>
            + AsGlowRenderer,
        <R as Renderer>::TextureId: Clone + 'static,
        <R as Renderer>::Error: From<GlesError>,
        CosmicElement<R>: RenderElement<R>,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        WorkspaceRenderElement<R>: RenderElement<R>,
    {
        let cursor_mode = if draw_cursor {
            CursorMode::All
        } else {
            CursorMode::None
        };

        let area = output
            .current_mode()
            .ok_or(DTError::OutputNoMode(OutputNoMode))
            .map(
                |mode| {
                    mode.size
                        .to_logical(1)
                        .to_buffer(1, Transform::Normal)
                        .to_f64()
                }, /* TODO: Mode is Buffer..., why is this Physical in the first place */
            )?;
        let additional_damage = (!additional_damage.is_empty()).then(|| {
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
                .collect()
        });

        if let Ok(dmabuf) = get_dmabuf(buffer) {
            render_workspace::<_, _, GlesRenderbuffer>(
                None,
                renderer,
                dmabuf,
                dt,
                age,
                additional_damage,
                common,
                &output,
                None,
                handle,
                cursor_mode,
                None,
                true,
            )
            .map(|res| res.0)
        } else {
            let size = buffer_dimensions(buffer).unwrap();
            let format =
                with_buffer_contents(buffer, |_, _, data| shm_format_to_fourcc(data.format))
                    .map_err(|_| DTError::OutputNoMode(OutputNoMode))? // eh, we have to do some error
                    .expect("We should be able to convert all hardcoded shm screencopy formats");
            let render_buffer =
                Offscreen::<GlesRenderbuffer>::create_buffer(renderer, format, size)
                    .map_err(DTError::Rendering)?;
            render_workspace::<_, _, GlesRenderbuffer>(
                None,
                renderer,
                render_buffer,
                dt,
                age,
                additional_damage,
                common,
                &output,
                None,
                handle,
                cursor_mode,
                None,
                true,
            )
            .map(|res| res.0)
        }
    }

    let draw_cursor = session.draw_cursor();
    let transform = output.current_transform();
    let common = &mut state.common;

    let result = match &mut state.backend {
        BackendData::Kms(kms) => {
            let render_node = kms
                .target_node_for_output(&output)
                .unwrap_or(kms.primary_node);
            let target_node = get_dmabuf(&buffer)
                .ok()
                .and_then(|dma| dma.node())
                .unwrap_or(render_node);

            let buffer_format = match buffer_type(&buffer) {
                Some(BufferType::Dma) => Some(get_dmabuf(&buffer).unwrap().format().code),
                Some(BufferType::Shm) => {
                    with_buffer_contents(&buffer, |_, _, data| shm_format_to_fourcc(data.format))
                        .unwrap()
                }
                _ => None,
            };
            let mut multirenderer = match kms.api.renderer(
                &render_node,
                &target_node,
                buffer_format.unwrap_or(Fourcc::Abgr8888),
            ) {
                Ok(renderer) => renderer,
                Err(err) => {
                    warn!(?err, "Couldn't use nodes for screencopy");
                    frame.fail(FailureReason::Unknown);
                    return;
                }
            };

            match render_session::<_, _>(
                &mut multirenderer,
                session.user_data().get::<SessionData>().unwrap(),
                frame,
                transform,
                |buffer, renderer, dt, age, additional_damage| {
                    render_fn(
                        buffer,
                        renderer,
                        dt,
                        age,
                        additional_damage,
                        draw_cursor,
                        common,
                        &output,
                        (handle, idx),
                    )
                },
            ) {
                Ok(frame) => frame,
                Err(err) => {
                    tracing::warn!(?err, "Failed to render to screencopy buffer");
                    None
                }
            }
        }
        BackendData::Winit(winit) => match render_session::<_, _>(
            winit.backend.renderer(),
            session.user_data().get::<SessionData>().unwrap(),
            frame,
            transform,
            |buffer, renderer, dt, age, additional_damage| {
                render_fn(
                    buffer,
                    renderer,
                    dt,
                    age,
                    additional_damage,
                    draw_cursor,
                    common,
                    &output,
                    (handle, idx),
                )
            },
        ) {
            Ok(frame) => frame,
            Err(err) => {
                tracing::warn!(?err, "Failed to render to screencopy buffer");
                None
            }
        },
        BackendData::X11(x11) => match render_session::<_, _>(
            &mut x11.renderer,
            session.user_data().get::<SessionData>().unwrap(),
            frame,
            transform,
            |buffer, renderer, dt, age, additional_damage| {
                render_fn(
                    buffer,
                    renderer,
                    dt,
                    age,
                    additional_damage,
                    draw_cursor,
                    common,
                    &output,
                    (handle, idx),
                )
            },
        ) {
            Ok(frame) => frame,
            Err(err) => {
                tracing::warn!(?err, "Failed to render to screencopy buffer");
                None
            }
        },
        _ => unreachable!(),
    };

    if let Some((frame, damage)) = result {
        frame.success(transform, damage, common.clock.now())
    }
}

smithay::render_elements! {
    pub WindowCaptureElement<R> where R: ImportAll + ImportMem;
    WaylandElement=WaylandSurfaceRenderElement<R>,
    CursorElement=RelocateRenderElement<cursor::CursorRenderElement<R>>,
    AdditionalDamage=DamageElement,
}

pub fn render_window_to_buffer(
    state: &mut State,
    session: Session,
    frame: Frame,
    toplevel: &CosmicSurface,
) {
    #[cfg(feature = "debug")]
    puffin::profile_function!();

    if !toplevel.alive() {
        session.stop();
        return;
    }

    let buffer = frame.buffer();
    let geometry = toplevel.geometry();
    let buffer_size = buffer_dimensions(&buffer).unwrap();
    if buffer_size != geometry.size.to_buffer(1, Transform::Normal) {
        let Some(constraints) = constraints_for_toplevel(toplevel, &mut state.backend) else {
            session.stop();
            return;
        };
        session.update_constraints(constraints);
        if let Some(data) = session.user_data().get::<SessionData>() {
            let size = geometry.size.to_physical(1);
            *data.borrow_mut() =
                SessionUserData::new(OutputDamageTracker::new(size, 1.0, Transform::Normal));
        }
        frame.fail(FailureReason::BufferConstraints);
        return;
    }

    fn render_fn<R>(
        buffer: &WlBuffer,
        renderer: &mut R,
        dt: &mut OutputDamageTracker,
        age: usize,
        additional_damage: Vec<Rectangle<i32, BufferCoords>>,
        draw_cursor: bool,
        common: &mut Common,
        window: &CosmicSurface,
        geometry: Rectangle<i32, Logical>,
    ) -> Result<RenderOutputResult, DTError<R>>
    where
        R: Renderer
            + ImportAll
            + ImportMem
            + ExportMem
            + Bind<Dmabuf>
            + Offscreen<GlesRenderbuffer>
            + Blit<Dmabuf>
            + AsGlowRenderer,
        <R as Renderer>::TextureId: Clone + 'static,
        <R as Renderer>::Error: From<GlesError>,
        CosmicElement<R>: RenderElement<R>,
        CosmicMappedRenderElement<R>: RenderElement<R>,
    {
        let mut elements = AsRenderElements::<R>::render_elements::<WindowCaptureElement<R>>(
            window,
            renderer,
            (-geometry.loc.x, -geometry.loc.y).into(),
            Scale::from(1.0),
            1.0,
        );

        elements.extend(
            additional_damage
                .into_iter()
                .filter_map(|rect| {
                    let logical_rect = rect.to_logical(
                        1,
                        Transform::Normal,
                        &geometry.size.to_buffer(1, Transform::Normal),
                    );
                    logical_rect.intersection(Rectangle::from_loc_and_size((0, 0), geometry.size))
                })
                .map(DamageElement::new)
                .map(Into::<WindowCaptureElement<R>>::into),
        );

        let seat = common.last_active_seat().clone();
        if let Some(location) = {
            if let Some(mapped) = common.shell.element_for_surface(window) {
                mapped.cursor_position(&seat).and_then(|mut p| {
                    p -= mapped.active_window_offset().to_f64();
                    if p.x < 0. || p.y < 0. {
                        None
                    } else {
                        Some(p)
                    }
                })
            } else {
                None
            }
        } {
            if draw_cursor {
                elements.extend(
                    cursor::draw_cursor(
                        renderer,
                        &seat,
                        location,
                        1.0.into(),
                        common.clock.now(),
                        true,
                    )
                    .into_iter()
                    .map(|(elem, hotspot)| {
                        WindowCaptureElement::CursorElement(RelocateRenderElement::from_element(
                            elem,
                            Point::from((-hotspot.x, -hotspot.y)),
                            Relocate::Relative,
                        ))
                    }),
                );
            }

            if let Some(wl_surface) = get_dnd_icon(&seat) {
                elements.extend(
                    cursor::draw_dnd_icon(renderer, &wl_surface, location.to_i32_round(), 1.0)
                        .into_iter()
                        .map(WindowCaptureElement::from),
                );
            }
        }

        if let Ok(dmabuf) = get_dmabuf(buffer) {
            renderer.bind(dmabuf).map_err(DTError::Rendering)?;
        } else {
            let size = buffer_dimensions(buffer).unwrap();
            let format =
                with_buffer_contents(buffer, |_, _, data| shm_format_to_fourcc(data.format))
                    .map_err(|_| DTError::OutputNoMode(OutputNoMode))? // eh, we have to do some error
                    .expect("We should be able to convert all hardcoded shm screencopy formats");
            let render_buffer =
                Offscreen::<GlesRenderbuffer>::create_buffer(renderer, format, size)
                    .map_err(DTError::Rendering)?;
            renderer.bind(render_buffer).map_err(DTError::Rendering)?;
        }

        dt.render_output(
            renderer,
            age,
            &elements,
            CLEAR_COLOR, // TODO use a theme neutral color
        )
    }

    let common = &mut state.common;
    let draw_cursor = session.draw_cursor();

    let result = match &mut state.backend {
        BackendData::Kms(kms) => {
            let node = get_dmabuf(&buffer)
                .ok()
                .and_then(|dmabuf| dmabuf.node())
                .or_else(|| {
                    toplevel
                        .wl_surface()
                        .and_then(|wl_surface| {
                            with_renderer_surface_state(&wl_surface, |state| {
                                let buffer = state.buffer()?;
                                let dmabuf = get_dmabuf(&*buffer).ok()?;
                                dmabuf.node()
                            })
                        })
                        .flatten()
                })
                .unwrap_or(kms.primary_node);

            let mut multirenderer = match kms.api.single_renderer(&node) {
                Ok(renderer) => renderer,
                Err(err) => {
                    warn!(?err, "Couldn't use node for screencopy");
                    frame.fail(FailureReason::Unknown);
                    return;
                }
            };
            match render_session::<_, _>(
                &mut multirenderer,
                session.user_data().get::<SessionData>().unwrap(),
                frame,
                Transform::Normal,
                |buffer, renderer, dt, age, additional_damage| {
                    render_fn(
                        buffer,
                        renderer,
                        dt,
                        age,
                        additional_damage,
                        draw_cursor,
                        common,
                        toplevel,
                        geometry,
                    )
                },
            ) {
                Ok(frame) => frame,
                Err(err) => {
                    tracing::warn!(?err, "Failed to render to screencopy buffer");
                    None
                }
            }
        }
        BackendData::Winit(winit) => match render_session::<_, _>(
            winit.backend.renderer(),
            session.user_data().get::<SessionData>().unwrap(),
            frame,
            Transform::Normal,
            |buffer, renderer, dt, age, additional_damage| {
                render_fn(
                    buffer,
                    renderer,
                    dt,
                    age,
                    additional_damage,
                    draw_cursor,
                    common,
                    toplevel,
                    geometry,
                )
            },
        ) {
            Ok(frame) => frame,
            Err(err) => {
                tracing::warn!(?err, "Failed to render to screencopy buffer");
                None
            }
        },
        BackendData::X11(x11) => match render_session::<_, _>(
            &mut x11.renderer,
            session.user_data().get::<SessionData>().unwrap(),
            frame,
            Transform::Normal,
            |buffer, renderer, dt, age, additional_damage| {
                render_fn(
                    buffer,
                    renderer,
                    dt,
                    age,
                    additional_damage,
                    draw_cursor,
                    common,
                    toplevel,
                    geometry,
                )
            },
        ) {
            Ok(frame) => frame,
            Err(err) => {
                tracing::warn!(?err, "Failed to render to screencopy buffer");
                None
            }
        },
        _ => unreachable!(),
    };

    if let Some((frame, damage)) = result {
        frame.success(Transform::Normal, damage, common.clock.now())
    }
}

pub fn render_cursor_to_buffer(
    state: &mut State,
    session: &CursorSession,
    frame: Frame,
    seat: &Seat<State>,
) {
    let buffer = frame.buffer();
    let cursor_size = seat
        .cursor_geometry((0.0, 0.0), state.common.clock.now())
        .map(|(geo, _hotspot)| geo.size)
        .unwrap_or_else(|| Size::from((64, 64)));
    let buffer_size = buffer_dimensions(&buffer).unwrap();
    if buffer_size != cursor_size {
        let constraints = BufferConstraints {
            size: cursor_size,
            shm: vec![ShmFormat::Argb8888],
            dma: None,
        };
        session.update_constraints(constraints);
        if let Some(data) = session.user_data().get::<SessionData>() {
            *data.borrow_mut() = SessionUserData::new(OutputDamageTracker::new(
                cursor_size.to_logical(1, Transform::Normal).to_physical(1),
                1.0,
                Transform::Normal,
            ));
        }
        frame.fail(FailureReason::BufferConstraints);
        return;
    }

    fn render_fn<R>(
        buffer: &WlBuffer,
        renderer: &mut R,
        dt: &mut OutputDamageTracker,
        age: usize,
        additional_damage: Vec<Rectangle<i32, BufferCoords>>,
        common: &mut Common,
        seat: &Seat<State>,
    ) -> Result<RenderOutputResult, DTError<R>>
    where
        R: Renderer
            + ImportAll
            + ImportMem
            + ExportMem
            + Bind<Dmabuf>
            + Offscreen<GlesRenderbuffer>
            + Blit<Dmabuf>
            + AsGlowRenderer,
        <R as Renderer>::TextureId: Clone + 'static,
        <R as Renderer>::Error: From<GlesError>,
        CosmicElement<R>: RenderElement<R>,
        CosmicMappedRenderElement<R>: RenderElement<R>,
    {
        let mut elements = cursor::draw_cursor(
            renderer,
            &seat,
            Point::from((0.0, 0.0)),
            1.0.into(),
            common.clock.now(),
            true,
        )
        .into_iter()
        .map(|(elem, _)| RelocateRenderElement::from_element(elem, (0, 0), Relocate::Relative))
        .map(WindowCaptureElement::from)
        .collect::<Vec<_>>();

        elements.extend(
            additional_damage
                .into_iter()
                .filter_map(|rect| {
                    let logical_rect = rect.to_logical(1, Transform::Normal, &Size::from((64, 64)));
                    logical_rect.intersection(Rectangle::from_loc_and_size((0, 0), (64, 64)))
                })
                .map(DamageElement::new)
                .map(Into::<WindowCaptureElement<R>>::into),
        );

        if let Ok(dmabuf) = get_dmabuf(buffer) {
            renderer.bind(dmabuf).map_err(DTError::Rendering)?;
        } else {
            let size = buffer_dimensions(buffer).unwrap();
            let format =
                with_buffer_contents(buffer, |_, _, data| shm_format_to_fourcc(data.format))
                    .map_err(|_| DTError::OutputNoMode(OutputNoMode))? // eh, we have to do some error
                    .expect("We should be able to convert all hardcoded shm screencopy formats");
            let render_buffer =
                Offscreen::<GlesRenderbuffer>::create_buffer(renderer, format, size)
                    .map_err(DTError::Rendering)?;
            renderer.bind(render_buffer).map_err(DTError::Rendering)?;
        }

        dt.render_output(renderer, age, &elements, [0.0, 0.0, 0.0, 0.0])
    }

    let common = &mut state.common;
    let result = match &mut state.backend {
        BackendData::Kms(kms) => {
            let mut multirenderer = match kms.api.single_renderer(&kms.primary_node) {
                Ok(renderer) => renderer,
                Err(err) => {
                    warn!(?err, "Couldn't use node for screencopy");
                    frame.fail(FailureReason::Unknown);
                    return;
                }
            };
            match render_session::<_, _>(
                &mut multirenderer,
                session.user_data().get::<SessionData>().unwrap(),
                frame,
                Transform::Normal,
                |buffer, renderer, dt, age, additional_damage| {
                    render_fn(buffer, renderer, dt, age, additional_damage, common, seat)
                },
            ) {
                Ok(frame) => frame,
                Err(err) => {
                    tracing::warn!(?err, "Failed to render to screencopy buffer");
                    None
                }
            }
        }
        BackendData::Winit(winit) => match render_session::<_, _>(
            winit.backend.renderer(),
            session.user_data().get::<SessionData>().unwrap(),
            frame,
            Transform::Normal,
            |buffer, renderer, dt, age, additional_damage| {
                render_fn(buffer, renderer, dt, age, additional_damage, common, seat)
            },
        ) {
            Ok(frame) => frame,
            Err(err) => {
                tracing::warn!(?err, "Failed to render to screencopy buffer");
                None
            }
        },
        BackendData::X11(x11) => match render_session::<_, _>(
            &mut x11.renderer,
            session.user_data().get::<SessionData>().unwrap(),
            frame,
            Transform::Normal,
            |buffer, renderer, dt, age, additional_damage| {
                render_fn(buffer, renderer, dt, age, additional_damage, common, seat)
            },
        ) {
            Ok(frame) => frame,
            Err(err) => {
                tracing::warn!(?err, "Failed to render to screencopy buffer");
                None
            }
        },
        _ => unreachable!(),
    };

    if let Some((frame, damage)) = result {
        frame.success(Transform::Normal, damage, common.clock.now())
    }
}
