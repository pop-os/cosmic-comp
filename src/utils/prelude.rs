use std::{cell::RefCell, sync::Mutex, time::Duration};

use crate::{
    backend::render::cursor::{CursorShape, CursorState},
    input::{ActiveOutput, SeatId},
};
use smithay::{
    desktop::utils::bbox_from_surface_tree,
    input::{
        pointer::{CursorIcon, CursorImageAttributes, CursorImageStatus},
        Seat,
    },
    output::Output,
    utils::{Buffer, IsAlive, Logical, Monotonic, Point, Rectangle, Time, Transform},
    wayland::compositor::with_states,
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
    fn cursor_geometry(
        &self,
        loc: impl Into<Point<f64, Buffer>>,
        time: Time<Monotonic>,
    ) -> Option<(Rectangle<i32, Buffer>, Point<i32, Buffer>)>;
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

    fn cursor_geometry(
        &self,
        loc: impl Into<Point<f64, Buffer>>,
        time: Time<Monotonic>,
    ) -> Option<(Rectangle<i32, Buffer>, Point<i32, Buffer>)> {
        let location = loc.into().to_i32_round();

        let cursor_status = self
            .user_data()
            .get::<RefCell<CursorImageStatus>>()
            .map(|cell| {
                let mut cursor_status = cell.borrow_mut();
                if let CursorImageStatus::Surface(ref surface) = *cursor_status {
                    if !surface.alive() {
                        *cursor_status = CursorImageStatus::default_named();
                    }
                }
                cursor_status.clone()
            })
            .unwrap_or(CursorImageStatus::default_named());

        match cursor_status {
            CursorImageStatus::Surface(surface) => {
                let hotspot = with_states(&surface, |states| {
                    states
                        .data_map
                        .get::<Mutex<CursorImageAttributes>>()
                        .unwrap()
                        .lock()
                        .unwrap()
                        .hotspot
                });
                let geo = bbox_from_surface_tree(&surface, (location.x, location.y));
                let buffer_geo = Rectangle::from_loc_and_size(
                    (geo.loc.x, geo.loc.y),
                    geo.size.to_buffer(1, Transform::Normal),
                );
                Some((buffer_geo, (hotspot.x, hotspot.y).into()))
            }
            CursorImageStatus::Named(CursorIcon::Default) => {
                let seat_userdata = self.user_data();
                seat_userdata.insert_if_missing(CursorState::default);
                let state = seat_userdata.get::<CursorState>().unwrap();
                let frame = state
                    .cursors
                    .get(&CursorShape::Default)
                    .unwrap()
                    .get_image(1, Into::<Duration>::into(time).as_millis() as u32);

                Some((
                    Rectangle::from_loc_and_size(
                        location,
                        (frame.width as i32, frame.height as i32),
                    ),
                    (frame.xhot as i32, frame.yhot as i32).into(),
                ))
            }
            CursorImageStatus::Named(_) => {
                // TODO: Handle for `cursor_shape_v1` protocol
                None
            }
            CursorImageStatus::Hidden => None,
        }
    }
}
