// SPDX-License-Identifier: GPL-3.0-only

use serde::{Deserialize, Serialize};

pub mod input;
pub mod workspace;

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct XkbConfig {
    pub rules: String,
    pub model: String,
    pub layout: String,
    pub variant: String,
    pub options: Option<String>,
}

impl Default for XkbConfig {
    fn default() -> XkbConfig {
        XkbConfig {
            rules: String::new(),
            model: String::new(),
            layout: String::new(),
            variant: String::new(),
            options: None,
        }
    }
}
