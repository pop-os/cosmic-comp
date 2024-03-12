use std::{borrow::Borrow, cell::RefCell, collections::HashMap};

use smithay::{
    backend::{
        allocator::{Fourcc, Modifier},
        egl::EGLDevice,
        renderer::{
            damage::OutputDamageTracker,
            gles::{Capability, GlesRenderer},
            glow::GlowRenderer,
            utils::with_renderer_surface_state,
        },
    },
    desktop::space::SpaceElement,
    output::Output,
    reexports::wayland_server::protocol::wl_shm::Format as ShmFormat,
    utils::{Buffer as BufferCoords, Point, Size, Transform},
    wayland::{dmabuf::get_dmabuf, seat::WaylandFocus},
};

use crate::{
    shell::CosmicSurface,
    state::{BackendData, State},
    utils::prelude::{
        OutputExt, PointExt, PointGlobalExt, PointLocalExt, RectExt, RectLocalExt, SeatExt,
    },
    wayland::protocols::{
        image_source::ImageSourceData,
        screencopy::{
            delegate_screencopy, BufferConstraints, CursorSession, DmabufConstraints, Frame,
            ScreencopyHandler, ScreencopyState, Session,
        },
    },
};

mod render;
mod user_data;
pub use self::render::*;
use self::user_data::*;
pub use self::user_data::{FrameHolder, ScreencopySessions, SessionData, SessionHolder};

pub const WORKSPACE_OVERVIEW_NAMESPACE: &str = "cosmic-workspace-overview";

impl ScreencopyHandler for State {
    fn screencopy_state(&mut self) -> &mut ScreencopyState {
        &mut self.common.screencopy_state
    }

    fn capture_source(&mut self, source: &ImageSourceData) -> Option<BufferConstraints> {
        match source {
            ImageSourceData::Output(weak) => weak
                .upgrade()
                .and_then(|output| constraints_for_output(&output, &mut self.backend)),
            ImageSourceData::Workspace(handle) => {
                let workspace = self.common.shell.workspaces.space_for_handle(&handle)?;
                constraints_for_output(workspace.output(), &mut self.backend)
            }
            ImageSourceData::Toplevel(window) => {
                constraints_for_toplevel(window, &mut self.backend)
            }
            _ => None,
        }
    }
    fn capture_cursor_source(&mut self, _source: &ImageSourceData) -> Option<BufferConstraints> {
        let size = if let Some((geometry, _)) = self
            .common
            .last_active_seat()
            .cursor_geometry((0.0, 0.0), self.common.clock.now())
        {
            geometry.size
        } else {
            Size::from((64, 64))
        };

        Some(BufferConstraints {
            size,
            shm: vec![ShmFormat::Argb8888],
            dma: None,
        })
    }

    fn new_session(&mut self, session: Session) {
        match session.source() {
            ImageSourceData::Output(weak) => {
                let Some(mut output) = weak.upgrade() else {
                    session.stop();
                    return;
                };

                session.user_data().insert_if_missing(|| {
                    RefCell::new(SessionUserData::new(OutputDamageTracker::from_output(
                        &output,
                    )))
                });

                output.add_session(session);
            }
            ImageSourceData::Workspace(handle) => {
                let Some(workspace) = self.common.shell.workspaces.space_for_handle_mut(&handle)
                else {
                    session.stop();
                    return;
                };

                session.user_data().insert_if_missing(|| {
                    RefCell::new(SessionUserData::new(OutputDamageTracker::from_output(
                        workspace.output(),
                    )))
                });
                workspace.add_session(session);
            }
            ImageSourceData::Toplevel(mut toplevel) => {
                let size = toplevel.geometry().size.to_physical(1);
                session.user_data().insert_if_missing(|| {
                    RefCell::new(SessionUserData::new(OutputDamageTracker::new(
                        size,
                        1.0,
                        Transform::Normal,
                    )))
                });
                toplevel.add_session(session);
            }
            ImageSourceData::Destroyed => unreachable!(),
        }
    }
    fn new_cursor_session(&mut self, session: CursorSession) {
        let (pointer_loc, pointer_size, hotspot) = {
            let seat = self.common.last_active_seat();

            let pointer = seat.get_pointer().unwrap();
            let pointer_loc = pointer.current_location().to_i32_round().as_global();

            let (pointer_size, hotspot) = if let Some((geometry, hotspot)) =
                seat.cursor_geometry((0.0, 0.0), self.common.clock.now())
            {
                (geometry.size, hotspot)
            } else {
                (Size::from((64, 64)), Point::from((0, 0)))
            };

            (pointer_loc, pointer_size, hotspot)
        };

        session.user_data().insert_if_missing(|| {
            RefCell::new(SessionUserData::new(OutputDamageTracker::new(
                pointer_size.to_logical(1, Transform::Normal).to_physical(1),
                1.0,
                Transform::Normal,
            )))
        });

        match session.source() {
            ImageSourceData::Output(weak) => {
                let Some(mut output) = weak.upgrade() else {
                    session.stop();
                    return;
                };

                if output.geometry().contains(pointer_loc) {
                    let buffer_pos = pointer_loc
                        .to_local(&output)
                        .as_logical()
                        .to_f64()
                        .to_buffer(
                            output.current_scale().fractional_scale(),
                            output.current_transform(),
                            &output
                                .current_mode()
                                .map(|mode| {
                                    mode.size
                                        .to_f64()
                                        .to_logical(output.current_scale().fractional_scale())
                                })
                                .unwrap_or(Size::from((0.0, 0.0))),
                        )
                        .to_i32_round();
                    session.set_cursor_hotspot(hotspot);
                    session.set_cursor_pos(Some(buffer_pos));
                }

                output.add_cursor_session(session);
            }
            ImageSourceData::Workspace(handle) => {
                let Some(workspace) = self.common.shell.workspaces.space_for_handle_mut(&handle)
                else {
                    session.stop();
                    return;
                };

                let output = workspace.output().clone();
                if output.geometry().contains(pointer_loc) {
                    let buffer_pos = pointer_loc
                        .to_local(&output)
                        .as_logical()
                        .to_f64()
                        .to_buffer(
                            output.current_scale().fractional_scale(),
                            output.current_transform(),
                            &output
                                .current_mode()
                                .map(|mode| {
                                    mode.size
                                        .to_f64()
                                        .to_logical(output.current_scale().fractional_scale())
                                })
                                .unwrap_or(Size::from((0.0, 0.0))),
                        )
                        .to_i32_round();
                    session.set_cursor_hotspot(hotspot);
                    session.set_cursor_pos(Some(buffer_pos));
                }

                workspace.add_cursor_session(session);
            }
            ImageSourceData::Toplevel(mut toplevel) => {
                if let Some(element) = self.common.shell.element_for_surface(&toplevel) {
                    if element.has_active_window(&toplevel) {
                        if let Some(workspace) = self.common.shell.space_for(element) {
                            if let Some(geometry) = workspace.element_geometry(element) {
                                let mut surface_geo = element.active_window_geometry().as_local();
                                surface_geo.loc += geometry.loc;
                                let global_geo = surface_geo.to_global(workspace.output());
                                if global_geo.contains(pointer_loc) {
                                    let buffer_pos = (pointer_loc - global_geo.loc)
                                        .as_logical()
                                        .to_buffer(1, Transform::Normal, &toplevel.geometry().size);
                                    session.set_cursor_hotspot(hotspot);
                                    session.set_cursor_pos(Some(buffer_pos));
                                }
                            }
                        }
                    }
                }

                toplevel.add_cursor_session(session);
            }
            ImageSourceData::Destroyed => unreachable!(),
        }
    }

    fn frame(&mut self, session: Session, frame: Frame) {
        match session.source() {
            ImageSourceData::Output(weak) => {
                let Some(mut output) = weak.upgrade() else {
                    session.stop(); // will fail the frame as well
                    return;
                };

                output.add_frame(session, frame);
                self.backend
                    .schedule_render(&self.common.event_loop_handle, &output);
            }
            ImageSourceData::Workspace(handle) => {
                render_workspace_to_buffer(self, session, frame, handle)
            }
            ImageSourceData::Toplevel(toplevel) => {
                render_window_to_buffer(self, session, frame, &toplevel)
            }
            ImageSourceData::Destroyed => unreachable!(),
        }
    }

    fn cursor_frame(&mut self, session: CursorSession, frame: Frame) {
        if !session.has_cursor() {
            frame.success(Transform::Normal, Vec::new(), self.common.clock.now());
            return;
        }

        let seat = self.common.last_active_seat().clone();
        render_cursor_to_buffer(self, &session, frame, &seat);
    }

    fn frame_aborted(&mut self, frame: Frame) {
        for mut output in self.common.shell.outputs().cloned() {
            output.remove_frame(&frame)
        }
    }

    fn session_destroyed(&mut self, session: Session) {
        match session.source() {
            ImageSourceData::Output(weak) => {
                if let Some(mut output) = weak.upgrade() {
                    output.remove_session(session);
                }
            }
            ImageSourceData::Workspace(handle) => {
                if let Some(workspace) = self.common.shell.workspaces.space_for_handle_mut(&handle)
                {
                    workspace.remove_session(session)
                }
            }
            ImageSourceData::Toplevel(mut toplevel) => toplevel.remove_session(session),
            ImageSourceData::Destroyed => unreachable!(),
        }
    }

    fn cursor_session_destroyed(&mut self, session: CursorSession) {
        match session.source() {
            ImageSourceData::Output(weak) => {
                if let Some(mut output) = weak.upgrade() {
                    output.remove_cursor_session(session);
                }
            }
            ImageSourceData::Workspace(handle) => {
                if let Some(workspace) = self.common.shell.workspaces.space_for_handle_mut(&handle)
                {
                    workspace.remove_cursor_session(session)
                }
            }
            ImageSourceData::Toplevel(mut toplevel) => toplevel.remove_cursor_session(session),
            ImageSourceData::Destroyed => unreachable!(),
        }
    }
}

fn constraints_for_output(output: &Output, backend: &mut BackendData) -> Option<BufferConstraints> {
    let mode = match output.current_mode() {
        Some(mode) => mode.size.to_logical(1).to_buffer(1, Transform::Normal),
        None => {
            return None;
        }
    };

    let mut _kms_renderer = None;
    let renderer = match backend {
        BackendData::Kms(ref mut kms) => {
            let node = kms
                .target_node_for_output(&output)
                .unwrap_or(kms.primary_node);
            _kms_renderer = Some(kms.api.single_renderer(&node).unwrap());
            _kms_renderer.as_mut().unwrap().as_mut()
        }
        BackendData::Winit(ref mut winit) => winit.backend.renderer(),
        BackendData::X11(ref mut x11) => &mut x11.renderer,
        _ => unreachable!(),
    };

    Some(constraints_for_renderer(mode, renderer))
}

fn constraints_for_toplevel(
    surface: &CosmicSurface,
    backend: &mut BackendData,
) -> Option<BufferConstraints> {
    let size = surface.geometry().size.to_buffer(1, Transform::Normal);
    let wl_surface = surface.wl_surface()?;

    let mut _kms_renderer = None;
    let renderer = match backend {
        BackendData::Kms(ref mut kms) => {
            let dma_node = with_renderer_surface_state(&wl_surface, |state| {
                let buffer = state.buffer()?;
                let dmabuf = get_dmabuf(&*buffer).ok()?;
                dmabuf.node()
            })
            .flatten();

            let node = dma_node.unwrap_or(kms.primary_node);
            _kms_renderer = Some(kms.api.single_renderer(&node).unwrap());
            _kms_renderer.as_mut().unwrap().as_mut()
        }
        BackendData::Winit(ref mut winit) => winit.backend.renderer(),
        BackendData::X11(ref mut x11) => &mut x11.renderer,
        _ => unreachable!(),
    };

    Some(constraints_for_renderer(size, renderer))
}

fn constraints_for_renderer(
    size: Size<i32, BufferCoords>,
    renderer: &mut GlowRenderer,
) -> BufferConstraints {
    let mut constraints = BufferConstraints {
        size,
        shm: vec![ShmFormat::Abgr8888, ShmFormat::Xbgr8888],
        dma: None,
    };

    if (renderer as &dyn Borrow<GlesRenderer>)
        .borrow()
        .capabilities()
        .contains(&Capability::ColorTransformations)
    {
        constraints
            .shm
            .extend([ShmFormat::Abgr2101010, ShmFormat::Xbgr2101010]);
    }

    if let Some(node) = EGLDevice::device_for_display(renderer.egl_context().display())
        .ok()
        .and_then(|device| device.try_get_render_node().ok().flatten())
    {
        constraints.dma = Some(DmabufConstraints {
            node,
            formats: renderer
                .egl_context()
                .dmabuf_render_formats()
                .iter()
                .fold(
                    HashMap::<Fourcc, Vec<Modifier>>::new(),
                    |mut map, format| {
                        map.entry(format.code).or_default().push(format.modifier);
                        map
                    },
                )
                .into_iter()
                .collect::<Vec<_>>(),
        });
    }

    constraints
}

delegate_screencopy!(State);
