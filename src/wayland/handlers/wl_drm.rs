// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    state::State,
    wayland::protocols::drm::delegate_wl_drm,
};

delegate_wl_drm!(State);
