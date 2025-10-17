// SPDX-License-Identifier: GPL-3.0-only

use cosmic_comp_config::output::comp::{OutputConfig, OutputState, TransformDef};
use smithay::{output::Output, utils::Point};
use tracing::{error, warn};

use crate::{
    state::State,
    utils::prelude::OutputExt,
    wayland::protocols::output_configuration::{
        ModeConfiguration, OutputConfiguration, OutputConfigurationHandler,
        OutputConfigurationState, delegate_output_configuration,
    },
};

use std::cell::RefCell;

impl OutputConfigurationHandler for State {
    fn output_configuration_state(&mut self) -> &mut OutputConfigurationState<Self> {
        &mut self.common.output_configuration_state
    }

    fn test_configuration(&mut self, conf: Vec<(Output, OutputConfiguration)>) -> bool {
        self.output_configuration(true, conf)
    }
    fn apply_configuration(&mut self, conf: Vec<(Output, OutputConfiguration)>) -> bool {
        self.output_configuration(false, conf)
    }

    fn request_xwayland_primary(&mut self, primary_output: Option<Output>) {
        for output in self.common.output_configuration_state.outputs() {
            output.config_mut().xwayland_primary =
                primary_output.as_ref().is_some_and(|o| *o == output);
        }
        self.common.update_xwayland_primary_output();
        self.common
            .config
            .write_outputs(self.common.output_configuration_state.outputs());
    }
}

impl State {
    fn output_configuration(
        &mut self,
        test_only: bool,
        mut conf: Vec<(Output, OutputConfiguration)>,
    ) -> bool {
        if conf
            .iter()
            .all(|(_, conf)| matches!(conf, OutputConfiguration::Disabled))
        {
            return false; // we don't allow the user to accidentally disable all their outputs
        }

        // sanitize negative positions
        {
            let (offset_x, offset_y) = conf.iter().fold((0, 0), |mut offset, (_, conf)| {
                if let OutputConfiguration::Enabled {
                    position: Some(position),
                    ..
                } = conf
                {
                    if position.x.is_negative() {
                        offset.0 = offset.0.max(position.x.abs());
                    }
                    if position.y.is_negative() {
                        offset.1 = offset.1.max(position.y.abs());
                    }
                }
                offset
            });

            if offset_x > 0 || offset_y > 0 {
                for (output, conf) in conf.iter_mut() {
                    if let OutputConfiguration::Enabled { position, .. } = conf {
                        let current_config = output
                            .user_data()
                            .get::<RefCell<OutputConfig>>()
                            .unwrap()
                            .borrow();

                        *position = Some(
                            position.unwrap_or(Point::from((
                                current_config.position.0 as i32,
                                current_config.position.1 as i32,
                            ))) + Point::from((offset_x, offset_y)),
                        );
                    }
                }
            }
        }

        let mut backups = Vec::new();
        for (output, conf) in &conf {
            {
                let mut current_config = output
                    .user_data()
                    .get::<RefCell<OutputConfig>>()
                    .unwrap()
                    .borrow_mut();
                backups.push((output, current_config.clone()));

                if let OutputConfiguration::Enabled {
                    mirroring,
                    mode,
                    scale,
                    transform,
                    position,
                    adaptive_sync,
                } = conf
                {
                    match mode {
                        Some(ModeConfiguration::Mode(mode)) => {
                            current_config.mode =
                                ((mode.size.w, mode.size.h), Some(mode.refresh as u32));
                        }
                        Some(ModeConfiguration::Custom { size, refresh }) => {
                            current_config.mode = ((size.w, size.h), refresh.map(|x| x as u32));
                        }
                        _ => {}
                    }
                    if let Some(scale) = scale {
                        current_config.scale = *scale;
                    }
                    if let Some(transform) = transform {
                        current_config.transform = match transform {
                            smithay::utils::Transform::Normal => TransformDef::Normal,
                            smithay::utils::Transform::_90 => TransformDef::_90,
                            smithay::utils::Transform::_180 => TransformDef::_180,
                            smithay::utils::Transform::_270 => TransformDef::_270,
                            smithay::utils::Transform::Flipped => TransformDef::Flipped,
                            smithay::utils::Transform::Flipped90 => TransformDef::Flipped90,
                            smithay::utils::Transform::Flipped180 => TransformDef::Flipped180,
                            smithay::utils::Transform::Flipped270 => TransformDef::Flipped270,
                        }
                    }
                    if let Some(position) = position {
                        current_config.position = (position.x as u32, position.y as u32);
                    }
                    if let Some(vrr) = adaptive_sync {
                        current_config.vrr = *vrr;
                    }
                    if let Some(mirror) = mirroring {
                        current_config.enabled = OutputState::Mirroring(mirror.name());
                    } else {
                        current_config.enabled = OutputState::Enabled;
                    }
                } else {
                    current_config.enabled = OutputState::Disabled;
                }
            }
        }

        let mut backend = self.backend.lock();
        let res = backend.apply_config_for_outputs(
            test_only,
            &self.common.event_loop_handle,
            self.common.config.dynamic_conf.screen_filter(),
            self.common.shell.clone(),
            &mut self.common.workspace_state.update(),
            &self.common.xdg_activation_state,
            self.common.startup_done.clone(),
            &self.common.clock,
        );
        if let Err(err) = res {
            warn!("Failed to apply config. Resetting: {:?}", err);
            for (output, backup) in backups {
                {
                    let mut current_config = output
                        .user_data()
                        .get::<RefCell<OutputConfig>>()
                        .unwrap()
                        .borrow_mut();
                    *current_config = backup;
                }
            }
            if !test_only {
                if let Err(err) = backend.apply_config_for_outputs(
                    false,
                    &self.common.event_loop_handle,
                    self.common.config.dynamic_conf.screen_filter(),
                    self.common.shell.clone(),
                    &mut self.common.workspace_state.update(),
                    &self.common.xdg_activation_state,
                    self.common.startup_done.clone(),
                    &self.common.clock,
                ) {
                    error!("Failed to reset output config: {:?}", err);
                }
            }
            return false;
        }
        self.common.refresh();

        for output in conf
            .iter()
            .filter(|(_, c)| {
                matches!(
                    c,
                    OutputConfiguration::Enabled {
                        mirroring: None,
                        ..
                    }
                )
            })
            .map(|(o, _)| o)
        {
            self.common.output_configuration_state.enable_head(output);
        }
        for output in conf
            .iter()
            .filter(|(_, c)| {
                matches!(
                    c,
                    OutputConfiguration::Disabled
                        | OutputConfiguration::Enabled {
                            mirroring: Some(_),
                            ..
                        }
                )
            })
            .map(|(o, _)| o)
        {
            self.common.output_configuration_state.disable_head(output);
        }
        self.common
            .config
            .write_outputs(self.common.output_configuration_state.outputs());
        self.common.event_loop_handle.insert_idle(move |state| {
            state.common.output_configuration_state.update();
        });

        true
    }
}

delegate_output_configuration!(State);
