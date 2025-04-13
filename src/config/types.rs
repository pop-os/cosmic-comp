// SPDX-License-Identifier: GPL-3.0-only
#![allow(non_snake_case)]

use serde::{Deserialize, Serialize};
pub use smithay::{input::keyboard::XkbConfig as WlXkbConfig, utils::Transform};

#[derive(Serialize, Deserialize)]
#[serde(remote = "Transform")]
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
