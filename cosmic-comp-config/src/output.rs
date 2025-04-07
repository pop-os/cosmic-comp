// SPDX-License-Identifier: GPL-3.0-only

use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EdidProduct {
    pub manufacturer: [char; 3],
    pub product: u16,
    pub serial: Option<u32>,
    pub manufacture_week: i32,
    pub manufacture_year: i32,
    pub model_year: Option<i32>,
}

#[cfg(feature = "libdisplay-info")]
impl From<libdisplay_info::edid::VendorProduct> for EdidProduct {
    fn from(vp: libdisplay_info::edid::VendorProduct) -> Self {
        Self {
            manufacturer: vp.manufacturer,
            product: vp.product,
            serial: vp.serial,
            manufacture_week: vp.manufacture_week,
            manufacture_year: vp.manufacture_year,
            model_year: vp.model_year,
        }
    }
}
