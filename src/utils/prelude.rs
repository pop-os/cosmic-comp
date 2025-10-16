use cosmic_comp_config::output::comp::{AdaptiveSync, OutputConfig, OutputState};
use smithay::{
    backend::drm::VrrSupport as Support,
    output::{Output, WeakOutput},
    utils::Rectangle,
};

pub use super::geometry::*;
pub use crate::shell::{SeatExt, Shell, Workspace};
pub use crate::state::{Common, State};
pub use crate::wayland::handlers::xdg_shell::popup::update_reactive_popups;
use crate::{config::EdidProduct, shell::zoom::OutputZoomState};

use std::{
    cell::{Ref, RefCell, RefMut},
    sync::{
        Mutex,
        atomic::{AtomicU8, Ordering},
    },
};

pub trait OutputExt {
    fn is_internal(&self) -> bool;
    fn geometry(&self) -> Rectangle<i32, Global>;
    fn zoomed_geometry(&self) -> Option<Rectangle<i32, Global>>;

    fn adaptive_sync(&self) -> AdaptiveSync;
    fn set_adaptive_sync(&self, vrr: AdaptiveSync);
    fn adaptive_sync_support(&self) -> Option<Support>;
    fn set_adaptive_sync_support(&self, vrr: Option<Support>);
    fn mirroring(&self) -> Option<Output>;
    fn set_mirroring(&self, output: Option<Output>);

    fn is_enabled(&self) -> bool;
    fn config(&self) -> Ref<'_, OutputConfig>;
    fn config_mut(&self) -> RefMut<'_, OutputConfig>;

    fn edid(&self) -> Option<&EdidProduct>;
}

struct Vrr(AtomicU8);
struct VrrSupport(AtomicU8);
struct Mirroring(Mutex<Option<WeakOutput>>);

impl OutputExt for Output {
    fn is_internal(&self) -> bool {
        let name = self.name();
        name.starts_with("eDP-") || name.starts_with("LVDS-") || name.starts_with("DSI-")
    }

    fn geometry(&self) -> Rectangle<i32, Global> {
        Rectangle::new(self.current_location(), {
            self.current_transform()
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

    fn zoomed_geometry(&self) -> Option<Rectangle<i32, Global>> {
        let output_geometry = self.geometry();

        let output_state = self.user_data().get::<Mutex<OutputZoomState>>()?;
        let mut output_state_ref = output_state.lock().unwrap();

        let focal_point = output_state_ref.current_focal_point().to_global(self);
        let mut zoomed_output_geo = output_geometry.to_f64();
        zoomed_output_geo.loc -= focal_point;
        zoomed_output_geo = zoomed_output_geo.downscale(output_state_ref.current_level());
        zoomed_output_geo.loc += focal_point;

        Some(zoomed_output_geo.to_i32_round())
    }

    fn adaptive_sync(&self) -> AdaptiveSync {
        self.user_data()
            .get::<Vrr>()
            .map(|vrr| match vrr.0.load(Ordering::SeqCst) {
                2 => AdaptiveSync::Force,
                1 => AdaptiveSync::Enabled,
                _ => AdaptiveSync::Disabled,
            })
            .unwrap_or(AdaptiveSync::Disabled)
    }
    fn set_adaptive_sync(&self, vrr: AdaptiveSync) {
        let user_data = self.user_data();
        user_data.insert_if_missing_threadsafe(|| Vrr(AtomicU8::new(0)));
        user_data.get::<Vrr>().unwrap().0.store(
            match vrr {
                AdaptiveSync::Disabled => 0,
                AdaptiveSync::Enabled => 1,
                AdaptiveSync::Force => 2,
            },
            Ordering::SeqCst,
        );
    }

    fn adaptive_sync_support(&self) -> Option<Support> {
        self.user_data()
            .get::<VrrSupport>()
            .and_then(|vrr| match vrr.0.load(Ordering::SeqCst) {
                0 => None,
                2 => Some(Support::RequiresModeset),
                3 => Some(Support::Supported),
                _ => Some(Support::NotSupported),
            })
    }

    fn set_adaptive_sync_support(&self, vrr: Option<Support>) {
        let user_data = self.user_data();
        user_data.insert_if_missing_threadsafe(|| VrrSupport(AtomicU8::new(0)));
        user_data.get::<VrrSupport>().unwrap().0.store(
            match vrr {
                None => 0,
                Some(Support::NotSupported) => 1,
                Some(Support::RequiresModeset) => 2,
                Some(Support::Supported) => 3,
            },
            Ordering::SeqCst,
        );
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

    fn edid(&self) -> Option<&EdidProduct> {
        self.user_data().get()
    }
}
