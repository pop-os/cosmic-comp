// SPDX-License-Identifier: GPL-3.0-only

use smithay::output::Output;
use tracing::warn;

use crate::{
    backend::kms::Surface,
    state::{BackendData, State},
    utils::prelude::OutputExt,
    wayland::protocols::output_power::{
        OutputPowerHandler, OutputPowerState, delegate_output_power,
    },
};

pub fn set_all_surfaces_dpms_on(state: &mut State) {
    let mut changed = false;
    let mut reinit = false;
    for surface in kms_surfaces(state) {
        if !surface.get_dpms() {
            surface.set_dpms(true);
            changed = true;
            reinit |= !surface.is_active();
        }
    }
    if reinit {
        reinit_inactive_surfaces(state);
    }

    if changed {
        // Powering an output on because of input is activity. Without this the idle
        // notifier stays idled, the idle daemon never sees `resumed`, and nothing
        // powers the output off again until real input arrives.
        let seat = state.common.shell.read().seats.last_active().clone();
        state.common.idle_notifier_state.notify_activity(&seat);
        OutputPowerState::refresh(state);
    }
}

/// A surface that gave up rendering only comes back through an output config apply, so a wake
/// retries it, like the redraw loop it replaced did.
fn reinit_inactive_surfaces(state: &mut State) {
    // Deferred, so every output woken in the same batch is on before the config is applied.
    state.common.event_loop_handle.insert_idle(|state| {
        // Only outputs meant to be on: a client may have turned this one off again meanwhile.
        if kms_surfaces(state).any(|surface| {
            !surface.is_active() && surface.output.is_enabled() && surface.get_dpms()
        }) {
            if let Err(err) = state.refresh_output_config() {
                warn!("Unable to re-initialize outputs after wake: {}", err);
            }
            // Re-initializing powers outputs on, which clients must see.
            OutputPowerState::refresh(state);
        }
    });
}

fn kms_surfaces(state: &mut State) -> impl Iterator<Item = &mut Surface> {
    if let BackendData::Kms(kms_state) = &mut state.backend {
        Some(
            kms_state
                .drm_devices
                .values_mut()
                .flat_map(|device| device.inner.surfaces.values_mut()),
        )
    } else {
        None
    }
    .into_iter()
    .flatten()
}

// Get KMS `Surface` for output, and for all outputs mirroring it
fn kms_surfaces_for_output<'a>(
    state: &'a mut State,
    output: &'a Output,
) -> impl Iterator<Item = &'a mut Surface> + 'a {
    kms_surfaces(state).filter(move |surface| {
        surface.output == *output || surface.output.mirroring().as_ref() == Some(output)
    })
}

// Get KMS `Surface` for output
fn primary_kms_surface_for_output<'a>(
    state: &'a mut State,
    output: &Output,
) -> Option<&'a mut Surface> {
    kms_surfaces(state).find(|surface| surface.output == *output)
}

impl OutputPowerHandler for State {
    fn output_power_state(&mut self) -> &mut OutputPowerState {
        &mut self.common.output_power_state
    }

    fn get_dpms(&mut self, output: &Output) -> Option<bool> {
        let surface = primary_kms_surface_for_output(self, output)?;
        Some(surface.get_dpms())
    }

    fn set_dpms(&mut self, output: &Output, on: bool) {
        let mut reinit = false;
        for surface in kms_surfaces_for_output(self, output) {
            // cosmic-idle repeats `On` for outputs that are already on; only a real wake retries.
            reinit |= on && !surface.get_dpms() && !surface.is_active();
            surface.set_dpms(on);
        }
        if reinit {
            reinit_inactive_surfaces(self);
        }
    }
}

delegate_output_power!(State);
