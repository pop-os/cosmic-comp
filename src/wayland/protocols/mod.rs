// SPDX-License-Identifier: GPL-3.0-only

pub mod a11y;
pub mod animated_resize;
pub mod backdrop_color;
pub mod background_effect;
pub mod blur;
// MERGE: fork-only client-facing KDE blur protocol (org_kde_kwin_blur). Kept because it is
// self-contained plumbing, but upstream's ext background-effect now drives rendering — review
// whether our layer-shell clients should migrate and this module be dropped.
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
