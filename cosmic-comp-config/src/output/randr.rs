use std::path::Path;

use cosmic_randr_shell::{AdaptiveSyncState, List};

use crate::EdidProduct;
use crate::output::comp::OutputState;

/// Represents a currently connected output from the Wayland compositor
#[derive(Debug, Clone)]
pub struct CurrentOutput {
    /// Connector name (e.g., "DP-5", "eDP-1")
    pub connector: String,
    pub make: String,
    pub model: String,
    /// EDID product info for precise matching
    pub edid: Option<EdidProduct>,
}

pub struct CompList {
    infos: Vec<super::comp::OutputInfo>,
    outputs: Vec<super::comp::OutputConfig>,
}

/// Convert a saved OutputConfig to a randr Output, using the provided connector name
fn config_to_randr_output(
    info: &super::comp::OutputInfo,
    output: &super::comp::OutputConfig,
    connector: String,
    modes: &mut slotmap::SlotMap<cosmic_randr_shell::ModeKey, cosmic_randr_shell::Mode>,
) -> cosmic_randr_shell::Output {
    let current = modes.insert(cosmic_randr_shell::Mode {
        size: (output.mode.0.0 as u32, output.mode.0.1 as u32),
        refresh_rate: output.mode.1.unwrap_or_default(),
        preferred: false,
    });

    cosmic_randr_shell::Output {
        serial_number: info
            .edid
            .as_ref()
            .and_then(|edid| edid.serial.map(|s| s.to_string()))
            .unwrap_or_default(),
        name: connector,
        enabled: !matches!(output.enabled, OutputState::Disabled),
        mirroring: match &output.enabled {
            OutputState::Mirroring(m) => Some(m.clone()),
            _ => None,
        },
        make: Some(info.make.clone()).filter(|make| make != "Unknown"),
        model: if info.model.as_str() == "Unknown" {
            String::new()
        } else {
            info.model.clone()
        },
        position: (output.position.0 as i32, output.position.1 as i32),
        scale: output.scale,
        transform: Some(match output.transform {
            crate::output::comp::TransformDef::Normal => cosmic_randr_shell::Transform::Normal,
            crate::output::comp::TransformDef::_90 => cosmic_randr_shell::Transform::Rotate90,
            crate::output::comp::TransformDef::_180 => cosmic_randr_shell::Transform::Rotate180,
            crate::output::comp::TransformDef::_270 => cosmic_randr_shell::Transform::Rotate270,
            crate::output::comp::TransformDef::Flipped => cosmic_randr_shell::Transform::Flipped,
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
        modes: vec![current],
        current: Some(current),
        adaptive_sync: Some(match output.vrr {
            crate::output::comp::AdaptiveSync::Enabled => AdaptiveSyncState::Auto,
            crate::output::comp::AdaptiveSync::Disabled => AdaptiveSyncState::Disabled,
            crate::output::comp::AdaptiveSync::Force => AdaptiveSyncState::Always,
        }),
        xwayland_primary: Some(output.xwayland_primary),
        physical: (0, 0),
        adaptive_sync_availability: None,
    }
}

/// Check if a saved OutputInfo matches a current output by EDID or make/model
fn info_matches_output(info: &super::comp::OutputInfo, current: &CurrentOutput) -> bool {
    // First try to match by EDID (most precise)
    if let (Some(saved_edid), Some(current_edid)) = (&info.edid, &current.edid) {
        return saved_edid == current_edid;
    }

    // Fall back to make/model matching
    info.make == current.make && info.model == current.model
}

impl From<CompList> for cosmic_randr_shell::List {
    fn from(CompList { infos, outputs }: CompList) -> cosmic_randr_shell::List {
        let mut list = cosmic_randr_shell::List::default();
        for (info, output) in infos.into_iter().zip(outputs.into_iter()) {
            // Use connector if available, otherwise use make/model as fallback for display
            let connector = info
                .connector
                .clone()
                .unwrap_or_else(|| format!("{} {}", info.make, info.model));
            let randr_output = config_to_randr_output(&info, &output, connector, &mut list.modes);
            list.outputs.insert(randr_output);
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

/// Given currently connected outputs, find the best matching saved config
/// and return it with correct connector names filled in from the current outputs.
///
/// This is the preferred way to get output config when you have access to
/// live output information (e.g., in cosmic-greeter).
pub fn get_matching_config(
    path: Option<impl AsRef<Path>>,
    current_outputs: &[CurrentOutput],
) -> Option<List> {
    let output_config = crate::output::comp::load_outputs(path);

    // Find the best matching saved config
    let mut best_match: Option<(
        &Vec<super::comp::OutputInfo>,
        &Vec<super::comp::OutputConfig>,
    )> = None;

    for (saved_infos, saved_configs) in output_config.config.iter() {
        // Must have same number of outputs
        if saved_infos.len() != current_outputs.len() {
            continue;
        }

        // Check if all saved infos match a current output
        let all_match = saved_infos.iter().all(|saved_info| {
            current_outputs
                .iter()
                .any(|current| info_matches_output(saved_info, current))
        });

        if all_match {
            // Prefer configs with more outputs (more specific match)
            if best_match.is_none_or(|(infos, _)| infos.len() < saved_infos.len()) {
                best_match = Some((saved_infos, saved_configs));
            }
        }
    }

    // Convert the matched config to a List with correct connector names
    let (saved_infos, saved_configs) = best_match?;

    let mut list = cosmic_randr_shell::List::default();

    for (saved_info, saved_config) in saved_infos.iter().zip(saved_configs.iter()) {
        // Find the matching current output to get the actual connector name
        let current = current_outputs
            .iter()
            .find(|c| info_matches_output(saved_info, c))?;

        let randr_output = config_to_randr_output(
            saved_info,
            saved_config,
            current.connector.clone(),
            &mut list.modes,
        );
        list.outputs.insert(randr_output);
    }

    Some(list)
}
