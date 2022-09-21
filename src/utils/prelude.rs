use crate::input::{ActiveOutput, SeatId};
use smithay::{
    input::Seat,
    output::Output,
    utils::{Logical, Rectangle, Transform},
};
use std::cell::RefCell;

pub use crate::shell::{Shell, Workspace};
pub use crate::state::{Common, State};
pub use crate::wayland::handlers::xdg_shell::popup::update_reactive_popups;

pub trait OutputExt {
    fn geometry(&self) -> Rectangle<i32, Logical>;
}

impl OutputExt for Output {
    fn geometry(&self) -> Rectangle<i32, Logical> {
        Rectangle::from_loc_and_size(self.current_location(), {
            Transform::from(self.current_transform())
                .transform_size(
                    self.current_mode()
                        .map(|m| m.size)
                        .unwrap_or_else(|| (0, 0).into()),
                )
                .to_f64()
                .to_logical(self.current_scale().fractional_scale())
                .to_i32_round()
        })
    }
}

pub trait SeatExt {
    fn id(&self) -> usize;
}

impl SeatExt for Seat<State> {
    fn id(&self) -> usize {
        self.user_data().get::<SeatId>().unwrap().0
    }
}

pub fn active_output(seat: &Seat<State>, state: &Common) -> Output {
    seat.user_data()
        .get::<ActiveOutput>()
        .map(|x| x.0.borrow().clone())
        .unwrap_or_else(|| {
            state
                .shell
                .outputs()
                .next()
                .cloned()
                .expect("Backend has no outputs?")
        })
}

pub fn set_active_output(seat: &Seat<State>, output: &Output) {
    if !seat
        .user_data()
        .insert_if_missing(|| ActiveOutput(RefCell::new(output.clone())))
    {
        *seat
            .user_data()
            .get::<ActiveOutput>()
            .unwrap()
            .0
            .borrow_mut() = output.clone();
    }
}
