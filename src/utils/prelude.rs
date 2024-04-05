use smithay::{
    output::Output,
    utils::{Rectangle, Transform},
};

pub use super::geometry::*;
pub use crate::shell::{SeatExt, Shell, Workspace};
pub use crate::state::{Common, State};
pub use crate::wayland::handlers::xdg_shell::popup::update_reactive_popups;

pub trait OutputExt {
    fn geometry(&self) -> Rectangle<i32, Global>;
}

impl OutputExt for Output {
    fn geometry(&self) -> Rectangle<i32, Global> {
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
        .as_global()
    }
}
