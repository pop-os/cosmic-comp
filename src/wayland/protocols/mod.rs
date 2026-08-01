// SPDX-License-Identifier: GPL-3.0-only

pub mod a11y;
pub mod animated_resize;
pub mod backdrop_color;
// Fork-only client-facing KDE blur protocol (org_kde_kwin_blur). Upstream's
// `ext_background_effect_v1` is bound too, from smithay, but it is a staging
// protocol that carries only a region -- strength, corner rounding, saturation,
// tint and border have nowhere to go in it. Our clients speak this one instead;
// both write the same per-surface state.
pub mod blur;
pub mod corner_radius;
pub mod drm;
pub mod exclusive_mode;
pub mod home_visibility;
pub mod image_capture_source;
pub mod keyboard_layout;
pub mod layer_auto_hide;
pub mod layer_corner_radius;
pub mod layer_edge_resize;
pub mod layer_shadow;
pub mod layer_surface_dismiss;
pub mod layer_surface_placement;
pub mod layer_surface_visibility;
pub mod layer_usable_area;
pub mod output_configuration;
pub mod output_power;
pub mod overlap_notify;
pub mod surface_embed;
pub mod tooltip;
pub mod toplevel_info;
pub mod toplevel_management;
pub mod voice_mode;
pub mod workspace;
