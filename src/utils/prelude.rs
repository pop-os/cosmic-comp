use crate::input::{ActiveOutput, SeatId};
use smithay::{
    input::Seat,
    output::Output,
    utils::{Logical, Rectangle, Transform},
};

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

    fn active_output(&self) -> Output;
    fn set_active_output(&self, output: &Output);
}

impl SeatExt for Seat<State> {
    fn id(&self) -> usize {
        self.user_data().get::<SeatId>().unwrap().0
    }

    fn active_output(&self) -> Output {
        self.user_data()
            .get::<ActiveOutput>()
            .map(|x| x.0.borrow().clone())
            .unwrap()
    }

    fn set_active_output(&self, output: &Output) {
        *self
            .user_data()
            .get::<ActiveOutput>()
            .unwrap()
            .0
            .borrow_mut() = output.clone();
    }
}
