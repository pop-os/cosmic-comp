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
            Bind, Blit, BufferType, Color32F, ExportMem, ImportAll, ImportMem, Offscreen, Renderer,
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
        element::{AsGlowRenderer, CosmicElement, DamageElement, FromGlesError},
        render_workspace, CursorMode, ElementFilter, RendererRef,
    },
    shell::{CosmicMappedRenderElement, CosmicSurface, WorkspaceRenderElement},
    state::{Common, KmsNodes, State},
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
    offscreen: Option<&mut R::Framebuffer<'_>>,
    transform: Transform,
    damage: Option<&[Rectangle<i32, Physical>]>,
    sync: SyncPoint,
) -> Result<Option<(Frame, Vec<Rectangle<i32, BufferCoords>>)>, R::Error>
where
    R: ExportMem,
    R::Error: FromGlesError,
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
    if let Some(fb) = offscreen {
        assert!(matches!(buffer_type(&buffer), Some(BufferType::Shm)));
        let buffer_size = buffer_dimensions(&buffer).unwrap();
        if let Err(err) = with_buffer_contents_mut(&buffer, |ptr, len, data| {
            let offset = data.offset;
            let width = data.width;
            let height = data.height;
            let stride = data.stride;
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
            let mapping =
                renderer.copy_framebuffer(fb, Rectangle::from_size(buffer_size), format)?;
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
        .map_err(|err| R::Error::from_gles_error(GlesError::BufferAccessError(err)))
        .and_then(|x| x)
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

pub fn render_session<F, R, T>(
    renderer: &mut R,
    session: &SessionData,
    frame: Frame,
    transform: Transform,
    render_fn: F,
) -> Result<Option<(Frame, Vec<Rectangle<i32, BufferCoords>>)>, DTError<R::Error>>
where
    R: ExportMem + Offscreen<T>,
    R::Error: FromGlesError,
    F: for<'d> FnOnce(
        &WlBuffer,
        &mut R,
        Option<&mut R::Framebuffer<'_>>,
        &'d mut OutputDamageTracker,
        usize,
        Vec<Rectangle<i32, BufferCoords>>,
    ) -> Result<RenderOutputResult<'d>, DTError<R::Error>>,
{
    let mut session_damage_tracking = session.lock().unwrap();

    let buffer = frame.buffer();
    let mut offscreen = matches!(buffer_type(&buffer), Some(BufferType::Shm))
        .then(|| {
            let size = buffer_dimensions(&buffer).ok_or(DTError::OutputNoMode(OutputNoMode))?;
            let format = with_buffer_contents(&buffer, |_, _, data| {
                shm_format_to_fourcc(data.format)
                    .expect("We should be able to convert all hardcoded shm screencopy formats")
            })
            .map_err(|_| DTError::OutputNoMode(OutputNoMode))?;
            renderer
                .create_buffer(format, size)
                .map_err(DTError::Rendering)
        })
        .transpose()?;

    let age = session_damage_tracking.age_for_buffer(&buffer);
    let mut fb = offscreen
        .as_mut()
        .map(|tex| renderer.bind(tex).map_err(DTError::Rendering))
        .transpose()?;
    let res = render_fn(
        &frame.buffer(),
        renderer,
        fb.as_mut(),
        &mut session_damage_tracking.dt,
        age,
        frame.damage(),
    );

    match res {
        Ok(result) => submit_buffer(
            frame,
            renderer,
            fb.as_mut(),
            transform,
            result.damage.map(|x| x.as_slice()),
            result.sync,
        )
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
    let shell = state.common.shell.read().unwrap();
    let Some(workspace) = shell.workspaces.space_for_handle(&handle) else {
        session.stop();
        return;
    };

    let output = workspace.output().clone();
    let idx = shell.workspaces.idx_for_handle(&output, &handle).unwrap();
    std::mem::drop(shell);

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
            *data.lock().unwrap() = SessionUserData::new(OutputDamageTracker::from_output(&output));
        }
        frame.fail(FailureReason::BufferConstraints);
        return;
    }

    fn render_fn<'d, R>(
        buffer: &WlBuffer,
        renderer: &mut R,
        offscreen: Option<&mut R::Framebuffer<'_>>,
        dt: &'d mut OutputDamageTracker,
        mut age: usize,
        additional_damage: Vec<Rectangle<i32, BufferCoords>>,
        draw_cursor: bool,
        common: &mut Common,
        output: &Output,
        handle: (WorkspaceHandle, usize),
    ) -> Result<RenderOutputResult<'d>, DTError<R::Error>>
    where
        R: Renderer + ImportAll + ImportMem + ExportMem + Bind<Dmabuf> + Blit + AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
        R::Error: FromGlesError,
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
            let mut dmabuf = dmabuf.clone();
            let mut fb = renderer.bind(&mut dmabuf).map_err(DTError::Rendering)?;
            render_workspace(
                None,
                renderer,
                &mut fb,
                dt,
                age,
                additional_damage,
                &common.shell,
                None,
                common.clock.now(),
                &output,
                None,
                handle,
                cursor_mode,
                ElementFilter::ExcludeWorkspaceOverview,
            )
            .map(|res| res.0)
        } else {
            let target = offscreen.expect("shm buffers should have an offscreen target");
            age = 0;
            render_workspace(
                None,
                renderer,
                target,
                dt,
                age,
                additional_damage,
                &common.shell,
                None,
                common.clock.now(),
                &output,
                None,
                handle,
                cursor_mode,
                ElementFilter::ExcludeWorkspaceOverview,
            )
            .map(|res| res.0)
        }
    }

    let draw_cursor = session.draw_cursor();
    let transform = output.current_transform();
    let common = &mut state.common;

    let renderer = match state.backend.offscreen_renderer(|kms| {
        let render_node = kms.target_node_for_output(&output).or(kms.primary_node)?;
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

        Some(KmsNodes {
            render_node,
            target_node,
            copy_format: buffer_format.unwrap_or(Fourcc::Abgr8888),
        })
    }) {
        Ok(renderer) => renderer,
        Err(err) => {
            warn!(?err, "Couldn't use node for screencopy");
            frame.fail(FailureReason::Unknown);
            return;
        }
    };
    let result = match renderer {
        RendererRef::Glow(renderer) => {
            match render_session::<_, _, GlesRenderbuffer>(
                renderer,
                session.user_data().get::<SessionData>().unwrap(),
                frame,
                transform,
                |buffer, renderer, offscreen, dt, age, additional_damage| {
                    render_fn(
                        buffer,
                        renderer,
                        offscreen,
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
        RendererRef::GlMulti(mut renderer) => {
            match render_session::<_, _, GlesRenderbuffer>(
                &mut renderer,
                session.user_data().get::<SessionData>().unwrap(),
                frame,
                transform,
                |buffer, renderer, offscreen, dt, age, additional_damage| {
                    render_fn(
                        buffer,
                        renderer,
                        offscreen,
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
            *data.lock().unwrap() =
                SessionUserData::new(OutputDamageTracker::new(size, 1.0, Transform::Normal));
        }
        frame.fail(FailureReason::BufferConstraints);
        return;
    }

    fn render_fn<'d, R>(
        buffer: &WlBuffer,
        renderer: &mut R,
        offscreen: Option<&mut R::Framebuffer<'_>>,
        dt: &'d mut OutputDamageTracker,
        age: usize,
        additional_damage: Vec<Rectangle<i32, BufferCoords>>,
        draw_cursor: bool,
        common: &mut Common,
        window: &CosmicSurface,
        geometry: Rectangle<i32, Logical>,
    ) -> Result<RenderOutputResult<'d>, DTError<R::Error>>
    where
        R: Renderer + ImportAll + ImportMem + ExportMem + Bind<Dmabuf> + Blit + AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
        R::Error: FromGlesError,
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
                    logical_rect.intersection(Rectangle::from_size(geometry.size))
                })
                .map(DamageElement::new)
                .map(Into::<WindowCaptureElement<R>>::into),
        );

        let shell = common.shell.read().unwrap();
        let seat = shell.seats.last_active().clone();
        let location = if let Some(mapped) = shell.element_for_surface(window) {
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
        };
        std::mem::drop(shell);

        if let Some(location) = location {
            if draw_cursor {
                elements.extend(
                    cursor::draw_cursor(
                        renderer,
                        &seat,
                        location,
                        1.0.into(),
                        1.0,
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

            if let Some(dnd_icon) = get_dnd_icon(&seat) {
                elements.extend(
                    cursor::draw_dnd_icon(
                        renderer,
                        &dnd_icon.surface,
                        (location + dnd_icon.offset.to_f64()).to_i32_round(),
                        1.0,
                    )
                    .into_iter()
                    .map(WindowCaptureElement::from),
                );
            }
        }

        if let Ok(dmabuf) = get_dmabuf(buffer) {
            let mut dmabuf_clone = dmabuf.clone();
            let mut fb = renderer
                .bind(&mut dmabuf_clone)
                .map_err(DTError::Rendering)?;
            dt.render_output(renderer, &mut fb, age, &elements, Color32F::TRANSPARENT)
        } else {
            let fb = offscreen.expect("shm buffer should have an offscreen target");
            dt.render_output(renderer, fb, 0, &elements, Color32F::TRANSPARENT)
        }
    }

    let common = &mut state.common;
    let draw_cursor = session.draw_cursor();

    let renderer = match state.backend.offscreen_renderer(|kms| {
        get_dmabuf(&buffer)
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
            .or(kms.primary_node)
    }) {
        Ok(renderer) => renderer,
        Err(err) => {
            warn!(?err, "Couldn't use node for screencopy");
            frame.fail(FailureReason::Unknown);
            return;
        }
    };
    let result = match renderer {
        RendererRef::Glow(renderer) => match render_session::<_, _, GlesRenderbuffer>(
            renderer,
            session.user_data().get::<SessionData>().unwrap(),
            frame,
            Transform::Normal,
            |buffer, renderer, offscreen, dt, age, additional_damage| {
                render_fn(
                    buffer,
                    renderer,
                    offscreen,
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
        RendererRef::GlMulti(mut renderer) => match render_session::<_, _, GlesRenderbuffer>(
            &mut renderer,
            session.user_data().get::<SessionData>().unwrap(),
            frame,
            Transform::Normal,
            |buffer, renderer, offscreen, dt, age, additional_damage| {
                render_fn(
                    buffer,
                    renderer,
                    offscreen,
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
            *data.lock().unwrap() = SessionUserData::new(OutputDamageTracker::new(
                cursor_size.to_logical(1, Transform::Normal).to_physical(1),
                1.0,
                Transform::Normal,
            ));
        }
        frame.fail(FailureReason::BufferConstraints);
        return;
    }

    fn render_fn<'d, R>(
        buffer: &WlBuffer,
        renderer: &mut R,
        offscreen: Option<&mut R::Framebuffer<'_>>,
        dt: &'d mut OutputDamageTracker,
        age: usize,
        additional_damage: Vec<Rectangle<i32, BufferCoords>>,
        common: &mut Common,
        seat: &Seat<State>,
    ) -> Result<RenderOutputResult<'d>, DTError<R::Error>>
    where
        R: Renderer + ImportAll + ImportMem + ExportMem + Bind<Dmabuf> + Blit + AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
        R::Error: FromGlesError,
        CosmicElement<R>: RenderElement<R>,
        CosmicMappedRenderElement<R>: RenderElement<R>,
    {
        let mut elements = cursor::draw_cursor(
            renderer,
            &seat,
            Point::from((0.0, 0.0)),
            1.0.into(),
            1.0,
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
                    logical_rect.intersection(Rectangle::from_size((64, 64).into()))
                })
                .map(DamageElement::new)
                .map(Into::<WindowCaptureElement<R>>::into),
        );

        if let Ok(dmabuf) = get_dmabuf(buffer) {
            let mut dmabuf_clone = dmabuf.clone();
            let mut fb = renderer
                .bind(&mut dmabuf_clone)
                .map_err(DTError::Rendering)?;
            dt.render_output(renderer, &mut fb, age, &elements, [0.0, 0.0, 0.0, 0.0])
        } else {
            let fb = offscreen.expect("shm buffers should have offscreen target");
            dt.render_output(renderer, fb, 0, &elements, [0.0, 0.0, 0.0, 0.0])
        }
    }

    let common = &mut state.common;
    let renderer = match state.backend.offscreen_renderer(|kms| kms.primary_node) {
        Ok(renderer) => renderer,
        Err(err) => {
            warn!(?err, "Couldn't use node for screencopy");
            frame.fail(FailureReason::Unknown);
            return;
        }
    };
    let result = match renderer {
        RendererRef::Glow(renderer) => {
            match render_session::<_, _, GlesRenderbuffer>(
                renderer,
                session.user_data().get::<SessionData>().unwrap(),
                frame,
                Transform::Normal,
                |buffer, renderer, offscreen, dt, age, additional_damage| {
                    render_fn(
                        buffer,
                        renderer,
                        offscreen,
                        dt,
                        age,
                        additional_damage,
                        common,
                        seat,
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
        RendererRef::GlMulti(mut renderer) => {
            match render_session::<_, _, GlesRenderbuffer>(
                &mut renderer,
                session.user_data().get::<SessionData>().unwrap(),
                frame,
                Transform::Normal,
                |buffer, renderer, offscreen, dt, age, additional_damage| {
                    render_fn(
                        buffer,
                        renderer,
                        offscreen,
                        dt,
                        age,
                        additional_damage,
                        common,
                        seat,
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
    };

    if let Some((frame, damage)) = result {
        frame.success(Transform::Normal, damage, common.clock.now())
    }
}
