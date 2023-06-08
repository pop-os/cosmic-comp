// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render::{element::AsGlowRenderer, IndicatorShader},
    shell::{
        element::CosmicMappedRenderElement,
        focus::target::{KeyboardFocusTarget, PointerFocusTarget},
        CosmicMapped, CosmicSurface,
    },
    utils::prelude::*,
};

use smithay::{
    backend::renderer::{
        element::{AsRenderElements, RenderElement},
        ImportAll, ImportMem, Renderer,
    },
    desktop::space::SpaceElement,
    input::{
        pointer::{
            AxisFrame, ButtonEvent, GrabStartData as PointerGrabStartData, MotionEvent,
            PointerGrab, PointerInnerHandle, RelativeMotionEvent,
        },
        Seat,
    },
    output::Output,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    utils::{IsAlive, Logical, Point, Rectangle, Serial},
    wayland::compositor::SurfaceData,
};
use std::{cell::RefCell, collections::HashSet, time::Duration};

pub type SeatMoveGrabState = RefCell<Option<MoveGrabState>>;

pub struct MoveGrabState {
    window: CosmicMapped,
    window_offset: Point<i32, Logical>,
    indicator_thickness: u8,
}

impl MoveGrabState {
    pub fn render<I, R>(&self, renderer: &mut R, seat: &Seat<State>, output: &Output) -> Vec<I>
    where
        R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
        <R as Renderer>::TextureId: 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        I: From<CosmicMappedRenderElement<R>>,
    {
        #[cfg(feature = "debug")]
        puffin::profile_function!();

        let cursor_at = seat.get_pointer().unwrap().current_location();

        let mut window_geo = self.window.geometry();
        window_geo.loc += cursor_at.to_i32_round() + self.window_offset;
        if !output.geometry().intersection(window_geo).is_some() {
            return Vec::new();
        }

        let scale = output.current_scale().fractional_scale().into();
        let render_location = cursor_at.to_i32_round() - output.geometry().loc + self.window_offset;

        let mut elements: Vec<I> = Vec::new();
        if self.indicator_thickness > 0 {
            elements.push(
                CosmicMappedRenderElement::from(IndicatorShader::focus_element(
                    renderer,
                    self.window.clone(),
                    Rectangle::from_loc_and_size(render_location, self.window.geometry().size),
                    self.indicator_thickness,
                    1.0,
                ))
                .into(),
            );
        }
        elements.extend(AsRenderElements::<R>::render_elements::<I>(
            &self.window,
            renderer,
            (render_location - self.window.geometry().loc).to_physical_precise_round(scale),
            scale,
            1.0,
        ));
        elements
    }

    pub fn send_frames(
        &self,
        output: &Output,
        time: impl Into<Duration>,
        throttle: Option<Duration>,
        primary_scan_out_output: impl FnMut(&WlSurface, &SurfaceData) -> Option<Output> + Copy,
    ) {
        self.window
            .active_window()
            .send_frame(output, time, throttle, primary_scan_out_output)
    }

    pub fn window(&self) -> CosmicSurface {
        self.window.active_window()
    }
}

pub struct MoveSurfaceGrab {
    window: CosmicMapped,
    start_data: PointerGrabStartData<State>,
    seat: Seat<State>,
    outputs: HashSet<Output>,
}

impl PointerGrab<State> for MoveSurfaceGrab {
    fn motion(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<i32, Logical>)>,
        event: &MotionEvent,
    ) {
        let borrow = self
            .seat
            .user_data()
            .get::<SeatMoveGrabState>()
            .map(|s| s.borrow());
        if let Some(grab_state) = borrow.as_ref().and_then(|s| s.as_ref()) {
            let mut window_geo = self.window.geometry();
            window_geo.loc += event.location.to_i32_round() + grab_state.window_offset;
            for output in state.common.shell.outputs() {
                if let Some(overlap) = output.geometry().intersection(window_geo) {
                    if self.outputs.insert(output.clone()) {
                        self.window.output_enter(output, overlap);
                    }
                } else if self.outputs.remove(&output) {
                    self.window.output_leave(output);
                }
            }
        }
        drop(borrow);

        // While the grab is active, no client has pointer focus
        handle.motion(state, None, event);
        if !self.window.alive() {
            self.ungrab(state, handle, event.serial, event.time);
        }
    }

    fn relative_motion(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<i32, Logical>)>,
        event: &RelativeMotionEvent,
    ) {
        // While the grab is active, no client has pointer focus
        handle.relative_motion(state, None, event);
    }

    fn button(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &ButtonEvent,
    ) {
        handle.button(state, event);
        if handle.current_pressed().is_empty() {
            self.ungrab(state, handle, event.serial, event.time);
        }
    }

    fn axis(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        details: AxisFrame,
    ) {
        handle.axis(state, details);
    }

    fn start_data(&self) -> &PointerGrabStartData<State> {
        &self.start_data
    }
}

impl MoveSurfaceGrab {
    pub fn new(
        start_data: PointerGrabStartData<State>,
        window: CosmicMapped,
        seat: &Seat<State>,
        initial_cursor_location: Point<f64, Logical>,
        initial_window_location: Point<i32, Logical>,
        indicator_thickness: u8,
    ) -> MoveSurfaceGrab {
        let output = seat.active_output();
        let mut outputs = HashSet::new();
        outputs.insert(output.clone());
        window.output_enter(&output, window.geometry()); // not accurate but...

        let grab_state = MoveGrabState {
            window: window.clone(),
            window_offset: dbg!(initial_window_location)
                - dbg!(initial_cursor_location.to_i32_round()),
            indicator_thickness,
        };

        *seat
            .user_data()
            .get::<SeatMoveGrabState>()
            .unwrap()
            .borrow_mut() = Some(grab_state);

        MoveSurfaceGrab {
            window,
            start_data,
            seat: seat.clone(),
            outputs,
        }
    }

    fn ungrab(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        serial: Serial,
        time: u32,
    ) {
        // No more buttons are pressed, release the grab.
        let output = self.seat.active_output();

        let position = if let Some(grab_state) = self
            .seat
            .user_data()
            .get::<SeatMoveGrabState>()
            .and_then(|s| s.borrow_mut().take())
        {
            if grab_state.window.alive() {
                let window_location = handle.current_location().to_i32_round()
                    - output.geometry().loc
                    + grab_state.window_offset;

                let workspace_handle = state.common.shell.active_space(&output).handle;
                for (window, _) in grab_state.window.windows() {
                    state
                        .common
                        .shell
                        .toplevel_info_state
                        .toplevel_enter_workspace(&window, &workspace_handle);
                    state
                        .common
                        .shell
                        .toplevel_info_state
                        .toplevel_enter_output(&window, &output);
                }

                let offset = state
                    .common
                    .shell
                    .active_space(&output)
                    .floating_layer
                    .space
                    .output_geometry(&output)
                    .unwrap()
                    .loc;
                grab_state.window.set_geometry(Rectangle::from_loc_and_size(
                    window_location + offset,
                    grab_state.window.geometry().size,
                ));
                state
                    .common
                    .shell
                    .active_space_mut(&output)
                    .floating_layer
                    .map_internal(grab_state.window, &output, Some(window_location + offset));

                let pointer_pos = handle.current_location();
                let relative_pos = state.common.shell.map_global_to_space(pointer_pos, &output);
                Some(window_location + offset + (pointer_pos - relative_pos).to_i32_round())
            } else {
                None
            }
        } else {
            None
        };

        handle.unset_grab(state, serial, time);
        if self.window.alive() {
            if let Some(position) = position {
                handle.motion(
                    state,
                    Some((
                        PointerFocusTarget::from(self.window.clone()),
                        position - self.window.geometry().loc,
                    )),
                    &MotionEvent {
                        location: handle.current_location(),
                        serial: serial,
                        time: time,
                    },
                );
            }
            Common::set_focus(
                state,
                Some(&KeyboardFocusTarget::from(self.window.clone())),
                &self.seat,
                Some(serial),
            )
        }
    }
}
