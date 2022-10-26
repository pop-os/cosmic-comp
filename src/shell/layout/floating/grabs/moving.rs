// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::{
        element::{CosmicMapped, CosmicMappedRenderElement},
        focus::target::{KeyboardFocusTarget, PointerFocusTarget},
    },
    utils::prelude::*,
};

use smithay::{
    backend::renderer::{element::AsRenderElements, ImportAll, Renderer},
    desktop::space::SpaceElement,
    input::{
        pointer::{
            AxisFrame, ButtonEvent, Focus, GrabStartData as PointerGrabStartData, MotionEvent,
            PointerGrab, PointerInnerHandle,
        },
        Seat,
    },
    output::Output,
    utils::{IsAlive, Logical, Physical, Point, Rectangle, Scale, Serial},
};
use std::cell::RefCell;

pub type SeatMoveGrabState = RefCell<Option<MoveGrabState>>;

pub struct MoveGrabState {
    window: CosmicMapped,
    initial_cursor_location: Point<f64, Logical>,
    initial_window_location: Point<i32, Logical>,
    initial_output_location: Point<i32, Logical>,
}

impl MoveGrabState {
    pub fn render<I, R>(&self, seat: &Seat<State>, output: &Output) -> Vec<I>
    where
        R: Renderer + ImportAll,
        <R as Renderer>::TextureId: 'static,
        I: From<CosmicMappedRenderElement<R>>,
    {
        let cursor_at = seat.get_pointer().unwrap().current_location();
        let delta = cursor_at - self.initial_cursor_location;
        let location =
            self.initial_output_location.to_f64() + self.initial_window_location.to_f64() + delta;

        let mut window_geo = self.window.geometry();
        window_geo.loc += location.to_i32_round();
        if !output.geometry().intersection(window_geo).is_some() {
            return Vec::new();
        }

        let scale = output.current_scale().fractional_scale().into();
        self.window.render_elements::<I>(
            (location.to_i32_round() - output.geometry().loc - self.window.geometry().loc)
                .to_physical_precise_round(scale),
            scale,
        )
    }
}

pub struct MoveSurfaceGrab {
    window: CosmicMapped,
    start_data: PointerGrabStartData<State>,
    seat: Seat<State>,
}

impl PointerGrab<State> for MoveSurfaceGrab {
    fn motion(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<i32, Logical>)>,
        event: &MotionEvent,
    ) {
        // While the grab is active, no client has pointer focus
        handle.motion(state, None, event);
        if !self.window.alive() {
            self.ungrab(state, handle, event.serial, event.time);
        }
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
        initial_output_location: Point<i32, Logical>,
    ) -> MoveSurfaceGrab {
        let grab_state = MoveGrabState {
            window: window.clone(),
            initial_cursor_location,
            initial_window_location,
            initial_output_location,
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

        if let Some(grab_state) = self
            .seat
            .user_data()
            .get::<SeatMoveGrabState>()
            .and_then(|s| s.borrow_mut().take())
        {
            if grab_state.window.alive() {
                let delta = handle.current_location() - grab_state.initial_cursor_location;
                let window_location = (grab_state.initial_window_location.to_f64()
                    + grab_state.initial_output_location.to_f64()
                    - output.geometry().loc.to_f64()
                    + delta)
                    .to_i32_round();

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

                state
                    .common
                    .shell
                    .active_space_mut(&output)
                    .floating_layer
                    .map_internal(grab_state.window, &output, Some(window_location));
            }
        }

        handle.unset_grab(state, serial, time);
        if self.window.alive() {
            Common::set_focus(
                state,
                Some(&KeyboardFocusTarget::from(self.window.clone())),
                &self.seat,
                Some(serial),
            )
        }
    }
}
