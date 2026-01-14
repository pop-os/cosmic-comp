// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    state::State,
    wayland::protocols::{
        image_capture_source::{ImageCaptureSourceKind, delegate_cosmic_image_capture_source},
        toplevel_info::window_from_ext,
    },
};
use smithay::{
    output::Output,
    wayland::{
        foreign_toplevel_list::ForeignToplevelHandle,
        image_capture_source::{
            ImageCaptureSource, ImageCaptureSourceHandler, OutputCaptureSourceHandler,
            OutputCaptureSourceState, ToplevelCaptureSourceHandler, ToplevelCaptureSourceState,
        },
    },
};

impl ImageCaptureSourceHandler for State {
    fn source_destroyed(&mut self, _source: ImageCaptureSource) {}
}

impl OutputCaptureSourceHandler for State {
    fn output_capture_source_state(&mut self) -> &mut OutputCaptureSourceState {
        &mut self.common.output_capture_source_state
    }

    fn output_source_created(&mut self, source: ImageCaptureSource, output: &Output) {
        source
            .user_data()
            .insert_if_missing(|| ImageCaptureSourceKind::Output(output.downgrade()));
    }
}

impl ToplevelCaptureSourceHandler for State {
    fn toplevel_capture_source_state(&mut self) -> &mut ToplevelCaptureSourceState {
        &mut self.common.toplevel_capture_source_state
    }

    fn toplevel_source_created(
        &mut self,
        source: ImageCaptureSource,
        toplevel: &ForeignToplevelHandle,
    ) {
        let data = match window_from_ext(self, toplevel) {
            Some(toplevel) => ImageCaptureSourceKind::Toplevel(toplevel.clone()),
            None => ImageCaptureSourceKind::Destroyed,
        };
        source.user_data().insert_if_missing(|| data);
    }
}

smithay::delegate_image_capture_source!(State);
smithay::delegate_output_capture_source!(State);
smithay::delegate_toplevel_capture_source!(State);

delegate_cosmic_image_capture_source!(State);
