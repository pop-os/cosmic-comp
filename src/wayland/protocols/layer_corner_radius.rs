// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the layer corner radius protocol (layer_corner_radius_manager_v1)
//!
//! This protocol allows clients to specify corner radius for layer shell surfaces.
//! The corner radius can be used for proper blur masking.

// Re-export only the actual code
pub use generated::{layer_corner_radius_manager_v1, layer_corner_radius_surface_v1};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/corner-radius.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/corner-radius.xml");
}

use crate::wayland::protocols::corner_radius::{CacheableCorners, Corners};
use smithay::utils::HookId;
use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
        backend::GlobalId, protocol::wl_surface::WlSurface,
    },
    wayland::compositor::{add_pre_commit_hook, with_states},
};
use std::sync::Mutex;

type SurfaceHookId = Mutex<
    Option<(
        HookId,
        Weak<layer_corner_radius_surface_v1::LayerCornerRadiusSurfaceV1>,
    )>,
>;

/// State for the layer corner radius manager protocol
#[derive(Debug)]
pub struct LayerCornerRadiusState {
    global: GlobalId,
}

impl LayerCornerRadiusState {
    /// Create a new layer corner radius manager global
    pub fn new<D>(dh: &DisplayHandle) -> LayerCornerRadiusState
    where
        D: GlobalDispatch<layer_corner_radius_manager_v1::LayerCornerRadiusManagerV1, ()>
            + Dispatch<layer_corner_radius_manager_v1::LayerCornerRadiusManagerV1, ()>
            + Dispatch<
                layer_corner_radius_surface_v1::LayerCornerRadiusSurfaceV1,
                LayerCornerRadiusData,
            > + LayerCornerRadiusHandler
            + 'static,
    {
        let global = dh
            .create_global::<D, layer_corner_radius_manager_v1::LayerCornerRadiusManagerV1, _>(
                1,
                (),
            );
        LayerCornerRadiusState { global }
    }

    /// Get the global ID of the layer corner radius manager
    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

/// Handler trait for layer corner radius events
pub trait LayerCornerRadiusHandler {
    /// Get the layer corner radius state
    fn layer_corner_radius_state(&mut self) -> &mut LayerCornerRadiusState;
}

impl<D> GlobalDispatch<layer_corner_radius_manager_v1::LayerCornerRadiusManagerV1, (), D>
    for LayerCornerRadiusState
where
    D: GlobalDispatch<layer_corner_radius_manager_v1::LayerCornerRadiusManagerV1, ()>
        + Dispatch<layer_corner_radius_manager_v1::LayerCornerRadiusManagerV1, ()>
        + Dispatch<layer_corner_radius_surface_v1::LayerCornerRadiusSurfaceV1, LayerCornerRadiusData>
        + LayerCornerRadiusHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<layer_corner_radius_manager_v1::LayerCornerRadiusManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<layer_corner_radius_manager_v1::LayerCornerRadiusManagerV1, (), D>
    for LayerCornerRadiusState
where
    D: GlobalDispatch<layer_corner_radius_manager_v1::LayerCornerRadiusManagerV1, ()>
        + Dispatch<layer_corner_radius_manager_v1::LayerCornerRadiusManagerV1, ()>
        + Dispatch<layer_corner_radius_surface_v1::LayerCornerRadiusSurfaceV1, LayerCornerRadiusData>
        + LayerCornerRadiusHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &layer_corner_radius_manager_v1::LayerCornerRadiusManagerV1,
        request: layer_corner_radius_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            layer_corner_radius_manager_v1::Request::Destroy => {
                // Global destroyed, nothing to do
            }
            layer_corner_radius_manager_v1::Request::GetCornerRadius { id, surface } => {
                // Check if surface already has a corner radius object
                let radius_exists = with_states(&surface, |surface_data| {
                    let hook_id = surface_data
                        .data_map
                        .get_or_insert_threadsafe(|| SurfaceHookId::new(None));
                    let guard = hook_id.lock().unwrap();
                    guard.as_ref().map(|(_, t)| t.upgrade().is_ok())
                });

                if radius_exists.unwrap_or_default() {
                    resource.post_error(
                        layer_corner_radius_manager_v1::Error::CornerRadiusExists as u32,
                        format!(
                            "{resource:?} LayerCornerRadiusSurfaceV1 object already exists for the surface"
                        ),
                    );
                    return;
                }

                let data = Mutex::new(LayerCornerRadiusInternal {
                    surface: surface.downgrade(),
                    corners: None,
                });
                let obj = data_init.init(id, data);
                let obj_downgrade = obj.downgrade();

                let needs_hook = radius_exists.is_none();
                if needs_hook {
                    let hook_id = add_pre_commit_hook::<D, _>(&surface, move |_, _dh, _surface| {
                        // Pre-commit hook - corner radius is validated when set
                        // No additional validation needed here
                    });

                    with_states(&surface, |surface_data| {
                        let hook_ids = surface_data
                            .data_map
                            .get_or_insert_threadsafe(|| SurfaceHookId::new(None));
                        let mut guard = hook_ids.lock().unwrap();
                        *guard = Some((hook_id, obj_downgrade));
                    });
                }
            }
        }
    }
}

impl<D>
    Dispatch<layer_corner_radius_surface_v1::LayerCornerRadiusSurfaceV1, LayerCornerRadiusData, D>
    for LayerCornerRadiusState
where
    D: GlobalDispatch<layer_corner_radius_manager_v1::LayerCornerRadiusManagerV1, ()>
        + Dispatch<layer_corner_radius_manager_v1::LayerCornerRadiusManagerV1, ()>
        + Dispatch<layer_corner_radius_surface_v1::LayerCornerRadiusSurfaceV1, LayerCornerRadiusData>
        + LayerCornerRadiusHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        resource: &layer_corner_radius_surface_v1::LayerCornerRadiusSurfaceV1,
        request: layer_corner_radius_surface_v1::Request,
        data: &LayerCornerRadiusData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            layer_corner_radius_surface_v1::Request::Destroy => {
                let mut guard = data.lock().unwrap();
                guard.corners = None;

                let Ok(surface) = guard.surface.upgrade() else {
                    return;
                };

                // Clear hook
                with_states(&surface, |surface_data| {
                    if let Some(hook_ids_mutex) = surface_data.data_map.get::<SurfaceHookId>() {
                        let mut hook_id = hook_ids_mutex.lock().unwrap();
                        *hook_id = None;
                    }
                });

                // Clear cached corners
                with_states(&surface, |s| {
                    let mut cached = s.cached_state.get::<CacheableCorners>();
                    let pending = cached.pending();
                    *pending = CacheableCorners(None);
                });
            }
            layer_corner_radius_surface_v1::Request::SetRadius {
                top_left,
                top_right,
                bottom_right,
                bottom_left,
            } => {
                let mut guard = data.lock().unwrap();
                guard.set_corner_radius(top_left, top_right, bottom_right, bottom_left);

                let Ok(surface) = guard.surface.upgrade() else {
                    resource.post_error(
                        layer_corner_radius_surface_v1::Error::SurfaceDestroyed as u32,
                        format!("{:?} No surface found", resource),
                    );
                    return;
                };

                with_states(&surface, |s| {
                    let mut cached = s.cached_state.get::<CacheableCorners>();
                    let pending = cached.pending();
                    *pending = CacheableCorners(guard.corners);
                });
            }
            layer_corner_radius_surface_v1::Request::UnsetRadius => {
                let mut guard = data.lock().unwrap();
                guard.corners = None;

                let Ok(surface) = guard.surface.upgrade() else {
                    resource.post_error(
                        layer_corner_radius_surface_v1::Error::SurfaceDestroyed as u32,
                        format!("{:?} No surface found", resource),
                    );
                    return;
                };

                with_states(&surface, |s| {
                    let mut cached = s.cached_state.get::<CacheableCorners>();
                    let pending = cached.pending();
                    *pending = CacheableCorners(None);
                });
            }
        }
    }
}

pub type LayerCornerRadiusData = Mutex<LayerCornerRadiusInternal>;

#[derive(Debug)]
pub struct LayerCornerRadiusInternal {
    pub surface: Weak<WlSurface>,
    pub corners: Option<Corners>,
}

impl LayerCornerRadiusInternal {
    fn set_corner_radius(
        &mut self,
        top_left: u32,
        top_right: u32,
        bottom_right: u32,
        bottom_left: u32,
    ) {
        let corners = Corners {
            top_left,
            top_right,
            bottom_right,
            bottom_left,
        };
        self.corners = Some(corners);
    }
}

macro_rules! delegate_layer_corner_radius {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_corner_radius::layer_corner_radius_manager_v1::LayerCornerRadiusManagerV1: ()
        ] => $crate::wayland::protocols::layer_corner_radius::LayerCornerRadiusState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_corner_radius::layer_corner_radius_manager_v1::LayerCornerRadiusManagerV1: ()
        ] => $crate::wayland::protocols::layer_corner_radius::LayerCornerRadiusState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::layer_corner_radius::layer_corner_radius_surface_v1::LayerCornerRadiusSurfaceV1: $crate::wayland::protocols::layer_corner_radius::LayerCornerRadiusData
        ] => $crate::wayland::protocols::layer_corner_radius::LayerCornerRadiusState);
    };
}
pub(crate) use delegate_layer_corner_radius;
