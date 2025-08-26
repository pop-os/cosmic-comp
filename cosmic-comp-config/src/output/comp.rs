// SPDX-License-Identifier: GPL-3.0-only

use serde::{Deserialize, Serialize};
use std::{collections::HashMap, fs::OpenOptions, path::Path};
use tracing::{error, warn};

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum OutputState {
    #[serde(rename = "true")]
    Enabled,
    #[serde(rename = "false")]
    Disabled,
    Mirroring(String),
}

fn default_state() -> OutputState {
    OutputState::Enabled
}

#[derive(Debug, Deserialize, Serialize, Clone, Copy, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum AdaptiveSync {
    #[serde(rename = "true")]
    Enabled,
    #[serde(rename = "false")]
    Disabled,
    Force,
}

fn default_sync() -> AdaptiveSync {
    AdaptiveSync::Enabled
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct OutputsConfig {
    pub config: HashMap<Vec<OutputInfo>, Vec<OutputConfig>>,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct OutputConfig {
    pub mode: ((i32, i32), Option<u32>),
    #[serde(default = "default_sync")]
    pub vrr: AdaptiveSync,
    pub scale: f64,
    pub transform: TransformDef,
    pub position: (u32, u32),
    #[serde(default = "default_state")]
    pub enabled: OutputState,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max_bpc: Option<u32>,
    #[serde(default)]
    pub xwayland_primary: bool,
}

impl Default for OutputConfig {
    fn default() -> OutputConfig {
        OutputConfig {
            mode: ((0, 0), None),
            vrr: AdaptiveSync::Enabled,
            scale: 1.0,
            transform: TransformDef::Normal,
            position: (0, 0),
            enabled: OutputState::Enabled,
            max_bpc: None,
            xwayland_primary: false,
        }
    }
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OutputInfo {
    pub connector: String,
    pub make: String,
    pub model: String,
}

pub fn load_outputs(path: Option<impl AsRef<Path>>) -> OutputsConfig {
    if let Some(path) = path.as_ref() {
        let path: &Path = path.as_ref();
        if path.exists() {
            match ron::de::from_reader::<_, OutputsConfig>(
                OpenOptions::new().read(true).open(path).unwrap(),
            ) {
                Ok(mut config) => {
                    for (info, config) in config.config.iter_mut() {
                        let config_clone = config.clone();
                        for conf in config.iter_mut() {
                            if let OutputState::Mirroring(conn) = &conf.enabled {
                                if let Some((j, _)) = info
                                    .iter()
                                    .enumerate()
                                    .find(|(_, info)| &info.connector == conn)
                                {
                                    if config_clone[j].enabled != OutputState::Enabled {
                                        warn!("Invalid Mirroring tag, overriding with `Enabled` instead");
                                        conf.enabled = OutputState::Enabled;
                                    }
                                } else {
                                    warn!(
                                        "Invalid Mirroring tag, overriding with `Enabled` instead"
                                    );
                                    conf.enabled = OutputState::Enabled;
                                }
                            }
                        }
                    }
                    return config;
                }
                Err(err) => {
                    warn!(?err, "Failed to read output_config, resetting..");
                    if let Err(err) = std::fs::remove_file(path) {
                        error!(?err, "Failed to remove output_config.");
                    }
                }
            };
        }
    }

    OutputsConfig {
        config: HashMap::new(),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TransformDef {
    Normal,
    _90,
    _180,
    _270,
    Flipped,
    Flipped90,
    Flipped180,
    Flipped270,
}
