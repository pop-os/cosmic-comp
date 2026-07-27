// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render::{
        CachedLayerSurface, LayerBlurSurfaceInfo, set_cached_layer_surfaces,
        set_layer_blur_surfaces,
    },
    shell::PendingLayer,
    utils::prelude::*,
    wayland::protocols::blur::{get_blur_radius, has_blur as surface_has_blur},
};
use smithay::{
    desktop::{
        LayerSurface, PopupKind, PopupManager, WindowSurfaceType, layer_map_for_output,
        utils::bbox_from_surface_tree,
    },
    output::Output,
    reexports::wayland_server::{Resource, protocol::wl_output::WlOutput},
    utils::{IsAlive, Rectangle},
    wayland::shell::{
        wlr_layer::{
            Layer, LayerSurface as WlrLayerSurface, WlrLayerShellHandler, WlrLayerShellState,
        },
        xdg::PopupSurface,
    },
};

/// All layer types that need to be cached for render thread access
const CACHED_LAYERS: [Layer; 4] = [Layer::Background, Layer::Bottom, Layer::Top, Layer::Overlay];

/// Update the layer surface caches for an output.
/// Called from the main thread when layer surfaces change.
/// Populates both the blur cache and the general layer surface cache for render thread.
///
/// `hidden_surfaces` should be the set of surface ObjectIds that are currently
/// hidden via the layer_surface_visibility protocol. Hidden surfaces are
/// excluded from blur processing to avoid wasting GPU time.
pub fn update_layer_blur_state(
    output: &Output,
    hidden_surfaces: &std::collections::HashSet<wayland_backend::server::ObjectId>,
) {
    let layer_map = layer_map_for_output(output);

    // Update blur surfaces cache
    let mut blur_surfaces: Vec<LayerBlurSurfaceInfo> = layer_map
        .layers()
        .filter(|layer| {
            let surface = layer.wl_surface();
            let alive = surface.alive();
            let has_blur = surface_has_blur(surface);
            let is_hidden = hidden_surfaces.contains(&surface.id());
            tracing::trace!(
                surface_id = surface.id().protocol_id(),
                alive,
                has_blur,
                is_hidden,
                layer = ?layer.layer(),
                "Checking layer surface for blur"
            );
            alive && has_blur && !is_hidden
        })
        .filter_map(|layer| {
            let geometry = layer_map.layer_geometry(layer)?;
            let layer_type = layer.layer();
            let surface = layer.wl_surface();
            let surface_id = surface.id();
            let blur_radius = get_blur_radius(surface);

            // Skip surfaces with trivially small geometry (e.g., 0x0 or 1x1 placeholder
            // surfaces that request blur but are not visually meaningful)
            let area = geometry.size.w.max(0) as i64 * geometry.size.h.max(0) as i64;
            if area < 4 {
                tracing::trace!(
                    ns = %layer.namespace(),
                    w = geometry.size.w,
                    h = geometry.size.h,
                    "Skipping layer blur for trivially small surface"
                );
                return None;
            }

            // Keep a handle so the cached texture can be reclaimed once this
            // surface dies (see reap_dead_blur_textures).
            crate::backend::render::blur::record_layer_blur_surface(surface);

            Some(LayerBlurSurfaceInfo {
                surface_id,
                geometry,
                layer: layer_type,
                blur_radius,
            })
        })
        .collect();

    // Layer-shell POPUPS that request blur are NOT in `layer_map.layers()`, so
    // enumerate them here too. `Stage::LayerPopup` looks the blur-capture texture
    // up by the popup's OWN surface id, so without an entry here a popup never
    // gets a texture and renders no backdrop. Geometry mirrors `layer_popups`'
    // standard placement (parent layer loc + popup offset).
    for layer in layer_map.layers() {
        let Some(layer_geo) = layer_map.layer_geometry(layer) else {
            continue;
        };
        for (popup, popup_offset) in PopupManager::popups_for_surface(layer.wl_surface()) {
            let surface = popup.wl_surface();
            if !surface.alive()
                || !surface_has_blur(surface)
                || hidden_surfaces.contains(&surface.id())
            {
                continue;
            }
            let popup_geo = popup.geometry();
            // `popup.geometry()` is (0,0,0,0) until the client sets a window
            // geometry; fall back to the surface-tree bounding box.
            let size = if popup_geo.size.w > 0 && popup_geo.size.h > 0 {
                popup_geo.size
            } else {
                bbox_from_surface_tree(surface, (0, 0)).size
            };
            if (size.w.max(0) as i64) * (size.h.max(0) as i64) < 4 {
                continue;
            }
            let loc = layer_geo.loc + (popup_offset - popup_geo.loc);
            // Popups never reach `layer_destroyed`, so this handle is the only
            // way their texture is ever reclaimed.
            crate::backend::render::blur::record_layer_blur_surface(surface);
            blur_surfaces.push(LayerBlurSurfaceInfo {
                surface_id: surface.id(),
                geometry: Rectangle::new(loc, size),
                layer: layer.layer(),
                blur_radius: get_blur_radius(surface),
            });
        }
    }

    if !blur_surfaces.is_empty() {
        tracing::trace!(
            output = output.name(),
            blur_surface_count = blur_surfaces.len(),
            "[BLUR-TIMING] 4/5 update_layer_blur_state() populated cache with blur surfaces"
        );
    } else {
        tracing::trace!(
            output = output.name(),
            blur_surface_count = blur_surfaces.len(),
            "Updated layer blur surfaces"
        );
    }

    set_layer_blur_surfaces(&output.name(), blur_surfaces);

    // Update general layer surface cache for each layer type
    for layer_type in CACHED_LAYERS {
        let surfaces: Vec<CachedLayerSurface> = layer_map
            .layers_on(layer_type)
            .rev()
            .filter_map(|layer| {
                let geometry = layer_map.layer_geometry(layer)?;
                Some(CachedLayerSurface {
                    surface: layer.clone(),
                    location: geometry.loc,
                })
            })
            .collect();

        set_cached_layer_surfaces(&output.name(), layer_type, surfaces);
    }
}

impl WlrLayerShellHandler for State {
    fn shell_state(&mut self) -> &mut WlrLayerShellState {
        &mut self.common.layer_shell_state
    }

    fn new_layer_surface(
        &mut self,
        surface: WlrLayerSurface,
        wl_output: Option<WlOutput>,
        _layer: Layer,
        namespace: String,
    ) {
        let mut shell = self.common.shell.write();
        let seat = shell.seats.last_active().clone();
        let no_output = wl_output.is_none();
        let output = wl_output
            .as_ref()
            .and_then(Output::from_resource)
            .unwrap_or_else(|| seat.active_output());
        let layer_surface = LayerSurface::new(surface, namespace);
        if no_output {
            shell
                .output_agnostic_layers
                .insert(layer_surface.wl_surface().id());
        }
        shell.pending_layers.push(PendingLayer {
            surface: layer_surface,
            output,
            seat,
        });
    }

    fn new_popup(&mut self, _parent: WlrLayerSurface, popup: PopupSurface) {
        self.common
            .shell
            .read()
            .unconstrain_popup(&PopupKind::from(popup.clone()));

        if let Err(err) = popup.send_configure() {
            tracing::warn!("Unable to configure popup. {err:?}",);
        } else {
            self.common
                .popups
                .track_popup(PopupKind::from(popup))
                .unwrap();
        }
    }

    fn layer_destroyed(&mut self, surface: WlrLayerSurface) {
        let surface_id = surface.wl_surface().id();
        let mut shell = self.common.shell.write();

        // Clean up visibility tracking for this surface
        shell.remove_surface_visibility(surface_id.clone());
        shell.remove_hidden_surface(&surface_id);
        shell.remove_client_exclusive_zone(&surface_id);
        shell.remove_layer_fade_in(&surface_id);
        shell.remove_layer_fade_out(&surface_id);
        shell.remove_layer_open(&surface_id);
        shell.remove_layer_close(&surface_id);
        shell.layer_slides.retain(|s| s.surface_id != surface_id);
        shell.output_agnostic_layers.remove(&surface_id);
        shell.exclusive_focus_granted.remove(&surface_id);

        // Release this surface's blurred backdrop. It is a full-output-sized GPU
        // texture, and the cache is otherwise only pruned when a whole output
        // goes away, so skipping this leaks one texture per destroyed layer.
        crate::backend::render::blur::clear_layer_blur_texture_for_surface(&surface_id);

        // Clean up any edge-resize state for this surface: a panel destroyed mid
        // drag/animation must not leave a stuck ghost, grab target, spring or settle.
        // (A stuck settle in particular would keep the dispatch loop re-evaluating
        // edge hover every iteration forever.)
        if shell
            .edge_drag_ghost
            .as_ref()
            .is_some_and(|g| g.surface_id == surface_id)
        {
            shell.edge_drag_ghost = None;
        }
        if shell
            .edge_hover
            .as_ref()
            .is_some_and(|h| h.surface_id == surface_id)
        {
            shell.edge_hover = None;
        }
        if shell
            .active_layer_resize
            .as_ref()
            .is_some_and(|r| r.surface_id == surface_id)
        {
            shell.active_layer_resize = None;
        }
        if shell
            .layer_resize_settle
            .as_ref()
            .is_some_and(|r| r.surface_id == surface_id)
        {
            shell.layer_resize_settle = None;
        }
        if shell
            .active_layer_resize_anim
            .as_ref()
            .is_some_and(|a| a.surface_id == surface_id)
        {
            shell.active_layer_resize_anim = None;
        }
        if shell
            .layer_maximize
            .as_ref()
            .is_some_and(|m| m.surface_id == surface_id)
        {
            shell.layer_maximize = None;
        }

        let maybe_output = shell
            .outputs()
            .find(|o| {
                let map = layer_map_for_output(o);
                map.layer_for_surface(surface.wl_surface(), WindowSurfaceType::TOPLEVEL)
                    .is_some()
            })
            .cloned();

        if let Some(output) = maybe_output {
            {
                let mut map = layer_map_for_output(&output);
                let layer = map
                    .layer_for_surface(surface.wl_surface(), WindowSurfaceType::TOPLEVEL)
                    .unwrap()
                    .clone();
                map.unmap_layer(&layer);
            }

            // Update layer blur cache after unmapping
            update_layer_blur_state(&output, shell.hidden_surfaces());

            shell.workspaces.recalculate();

            self.backend.schedule_render(&output);
        }
    }
}
