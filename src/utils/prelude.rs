use smithay::{
    output::{Output, WeakOutput},
    utils::{Rectangle, Transform},
};

pub use super::geometry::*;
use crate::config::{OutputConfig, OutputState};
pub use crate::shell::{SeatExt, Shell, Workspace};
pub use crate::state::{Common, State};
pub use crate::wayland::handlers::xdg_shell::popup::update_reactive_popups;

use std::{
    cell::{Ref, RefCell, RefMut},
    sync::{
        atomic::{AtomicBool, Ordering},
        Mutex,
    },
};

pub trait OutputExt {
    fn geometry(&self) -> Rectangle<i32, Global>;
    fn adaptive_sync(&self) -> bool;
    fn set_adaptive_sync(&self, vrr: bool);
    fn mirroring(&self) -> Option<Output>;
    fn set_mirroring(&self, output: Option<Output>);

    fn is_enabled(&self) -> bool;
    fn config(&self) -> Ref<'_, OutputConfig>;
    fn config_mut(&self) -> RefMut<'_, OutputConfig>;
}

struct Vrr(AtomicBool);

struct Mirroring(Mutex<Option<WeakOutput>>);

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

    fn mirroring(&self) -> Option<Output> {
        self.user_data().get::<Mirroring>().and_then(|mirroring| {
            mirroring
                .0
                .lock()
                .unwrap()
                .clone()
                .and_then(|o| o.upgrade())
        })
    }
    fn set_mirroring(&self, output: Option<Output>) {
        let user_data = self.user_data();
        user_data.insert_if_missing_threadsafe(|| Mirroring(Mutex::new(None)));
        *user_data.get::<Mirroring>().unwrap().0.lock().unwrap() =
            output.map(|output| output.downgrade());
    }

    fn is_enabled(&self) -> bool {
        self.user_data()
            .get::<RefCell<OutputConfig>>()
            .map(|conf| conf.borrow().enabled != OutputState::Disabled)
            .unwrap_or(false)
    }

    fn config(&self) -> Ref<'_, OutputConfig> {
        self.user_data()
            .get::<RefCell<OutputConfig>>()
            .unwrap()
            .borrow()
    }

    fn config_mut(&self) -> RefMut<'_, OutputConfig> {
        self.user_data()
            .get::<RefCell<OutputConfig>>()
            .unwrap()
            .borrow_mut()
    }
}
