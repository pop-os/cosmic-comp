use std::path::Path;

use cosmic_randr_shell::{AdaptiveSyncState, List};

use crate::output::comp::OutputState;

pub struct CompList {
    infos: Vec<super::comp::OutputInfo>,
    outputs: Vec<super::comp::OutputConfig>,
}

impl From<CompList> for cosmic_randr_shell::List {
    fn from(CompList { infos, outputs }: CompList) -> cosmic_randr_shell::List {
        let mut list = cosmic_randr_shell::List::default();
        for (info, output) in infos.into_iter().zip(outputs.into_iter()) {
            let current = list.modes.insert(cosmic_randr_shell::Mode {
                size: (output.mode.0 .0 as u32, output.mode.0 .1 as u32),
                refresh_rate: output.mode.1.unwrap_or_default(),
                // XXX not in config as far as i can tell
                preferred: false,
            });
            let modes = vec![current];

            // for mode in output. {}
            list.outputs.insert(cosmic_randr_shell::Output {
                name: info.connector,
                enabled: !matches!(output.enabled, OutputState::Disabled),
                mirroring: match output.enabled {
                    OutputState::Mirroring(m) => Some(m),
                    _ => None,
                },
                make: Some(info.make).filter(|make| make != "Unknown"),
                model: if info.model.as_str() == "Unknown" {
                    String::new()
                } else {
                    info.model
                },
                position: (output.position.0 as i32, output.position.1 as i32),
                scale: output.scale,
                transform: Some(match output.transform {
                    crate::output::comp::TransformDef::Normal => {
                        cosmic_randr_shell::Transform::Normal
                    }
                    crate::output::comp::TransformDef::_90 => {
                        cosmic_randr_shell::Transform::Rotate90
                    }
                    crate::output::comp::TransformDef::_180 => {
                        cosmic_randr_shell::Transform::Rotate180
                    }
                    crate::output::comp::TransformDef::_270 => {
                        cosmic_randr_shell::Transform::Rotate270
                    }
                    crate::output::comp::TransformDef::Flipped => {
                        cosmic_randr_shell::Transform::Flipped
                    }
                    crate::output::comp::TransformDef::Flipped90 => {
                        cosmic_randr_shell::Transform::Flipped90
                    }
                    crate::output::comp::TransformDef::Flipped180 => {
                        cosmic_randr_shell::Transform::Flipped180
                    }
                    crate::output::comp::TransformDef::Flipped270 => {
                        cosmic_randr_shell::Transform::Flipped270
                    }
                }),
                modes,
                current: Some(current),
                adaptive_sync: Some(match output.vrr {
                    crate::output::comp::AdaptiveSync::Enabled => AdaptiveSyncState::Auto,
                    crate::output::comp::AdaptiveSync::Disabled => AdaptiveSyncState::Disabled,
                    crate::output::comp::AdaptiveSync::Force => AdaptiveSyncState::Always,
                }),
                xwayland_primary: Some(output.xwayland_primary),
                // XXX no physical output size in the config
                physical: (0, 0),
                adaptive_sync_availability: None,
            });
        }

        list
    }
}

pub fn load_outputs(path: Option<impl AsRef<Path>>) -> Vec<List> {
    let output_config = crate::output::comp::load_outputs(path);
    output_config
        .config
        .into_iter()
        .map(|(infos, outputs)| {
            let comp_config = CompList { infos, outputs };
            List::from(comp_config)
        })
        .collect()
}
