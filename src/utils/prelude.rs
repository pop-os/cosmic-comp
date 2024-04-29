use smithay::{
    output::Output,
    utils::{Rectangle, Transform},
};

pub use super::geometry::*;
pub use crate::shell::{SeatExt, Shell, Workspace};
pub use crate::state::{Common, State};
pub use crate::wayland::handlers::xdg_shell::popup::update_reactive_popups;

use std::sync::atomic::{AtomicBool, Ordering};

pub trait OutputExt {
    fn geometry(&self) -> Rectangle<i32, Global>;
    fn adaptive_sync(&self) -> bool;
    fn set_adaptive_sync(&self, vrr: bool);
}

struct Vrr(AtomicBool);

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

    fn adaptive_sync(&self) -> bool {
        self.user_data()
            .get::<Vrr>()
            .map(|vrr| vrr.0.load(Ordering::SeqCst))
            .unwrap_or(false)
    }
    fn set_adaptive_sync(&self, vrr: bool) {
        let user_data = self.user_data();
        user_data.insert_if_missing_threadsafe(|| Vrr(AtomicBool::new(false)));
        user_data
            .get::<Vrr>()
            .unwrap()
            .0
            .store(vrr, Ordering::SeqCst);
    }
}
