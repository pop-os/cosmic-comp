use cosmic_protocols::corner_radius::v1::server::cosmic_corner_radius_layer_v1::{
    self, CosmicCornerRadiusLayerV1,
};
use cosmic_protocols::corner_radius::v1::server::cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1;
use cosmic_protocols::corner_radius::v1::server::{
    cosmic_corner_radius_manager_v1, cosmic_corner_radius_toplevel_v1,
};
use smithay::desktop::utils::bbox_from_surface_tree;
use smithay::reexports::wayland_protocols::xdg::shell::server::xdg_popup::XdgPopup;
use smithay::reexports::wayland_protocols_wlr::layer_shell::v1::server::zwlr_layer_surface_v1::ZwlrLayerSurfaceV1;
use smithay::reexports::wayland_server::New;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::utils::{HookId, Logical, Point, Rectangle};
use smithay::wayland::compositor::Cacheable;
use smithay::wayland::compositor::add_pre_commit_hook;
use smithay::wayland::compositor::with_states;
use smithay::wayland::shell::wlr_layer::WlrLayerShellHandler;
use smithay::wayland::shell::xdg::{SurfaceCachedState, XdgShellSurfaceUserData};
use smithay::{
    reexports::{
        wayland_protocols::xdg::shell::server::xdg_toplevel::XdgToplevel,
        wayland_server::{Client, Dispatch, DisplayHandle, GlobalDispatch, Resource, Weak},
    },
    wayland::shell::xdg::XdgShellHandler,
};
use std::sync::Mutex;
use wayland_backend::server::GlobalId;

type ToplevelHookId = Mutex<Option<(HookId, Weak<CosmicCornerRadiusToplevelV1>)>>;
type LayerHookId = Mutex<Option<(HookId, Weak<CosmicCornerRadiusLayerV1>)>>;

#[derive(Debug)]
pub struct CornerRadiusState {
    instances: Vec<cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1>,
    global: GlobalId,
}

impl CornerRadiusState {
    pub fn new<D>(dh: &DisplayHandle) -> CornerRadiusState
    where
        D: GlobalDispatch<cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1, ()>
            + Dispatch<cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1, ()>
            + Dispatch<
                cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1,
                CornerRadiusData,
            > + Dispatch<cosmic_corner_radius_layer_v1::CosmicCornerRadiusLayerV1, CornerRadiusData>
            + CornerRadiusHandler
            + 'static,
    {
        let global = dh
            .create_global::<D, cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1, _>(
                2,
                (),
            );
        CornerRadiusState {
            instances: Vec::new(),
            global,
        }
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

pub trait CornerRadiusHandler: XdgShellHandler + WlrLayerShellHandler {
    fn corner_radius_state(&mut self) -> &mut CornerRadiusState;
    fn set_corner_radius(&mut self, data: &CornerRadiusData);
    fn unset_corner_radius(&mut self, data: &CornerRadiusData);
    fn set_padding(&mut self, data: &CornerRadiusData);
    fn unset_padding(&mut self, data: &CornerRadiusData);
}

impl<D> GlobalDispatch<cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1, (), D>
    for CornerRadiusState
where
    D: GlobalDispatch<cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1, ()>
        + Dispatch<cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1, ()>
        + Dispatch<cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1, CornerRadiusData>
        + Dispatch<cosmic_corner_radius_layer_v1::CosmicCornerRadiusLayerV1, CornerRadiusData>
        + CornerRadiusHandler
        + 'static,
{
    fn bind(
        state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: smithay::reexports::wayland_server::New<
            cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1,
        >,
        _global_data: &(),
        data_init: &mut smithay::reexports::wayland_server::DataInit<'_, D>,
    ) {
        let instance = data_init.init(resource, ());
        state.corner_radius_state().instances.push(instance);
    }
}

impl<D> Dispatch<cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1, (), D>
    for CornerRadiusState
where
    D: GlobalDispatch<cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1, ()>
        + Dispatch<cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1, ()>
        + Dispatch<cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1, CornerRadiusData>
        + Dispatch<cosmic_corner_radius_layer_v1::CosmicCornerRadiusLayerV1, CornerRadiusData>
        + CornerRadiusHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1,
        request: <cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1 as smithay::reexports::wayland_server::Resource>::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut smithay::reexports::wayland_server::DataInit<'_, D>,
    ) {
        match request {
            cosmic_corner_radius_manager_v1::Request::Destroy => {
                let corner_radius_state = state.corner_radius_state();
                corner_radius_state.instances.retain(|i| i != resource);
            }
            cosmic_corner_radius_manager_v1::Request::GetCornerRadius { id, toplevel } => {
                if let Some(surface) = state.xdg_shell_state().get_toplevel(&toplevel) {
                    new_xdg(
                        surface.wl_surface(),
                        CornerRadiusSurface::Toplevel(surface.xdg_toplevel().downgrade()),
                        resource,
                        data_init,
                        id,
                    )
                } // TODO: can this fail?
            }
            cosmic_corner_radius_manager_v1::Request::GetCornerRadiusSurface { id, surface } => {
                if let Some(toplevel) =
                    state
                        .xdg_shell_state()
                        .toplevel_surfaces()
                        .iter()
                        .find(|toplevel| {
                            toplevel
                                .xdg_toplevel()
                                .data::<XdgShellSurfaceUserData>()
                                .unwrap()
                                .xdg_surface()
                                == &surface
                        })
                {
                    new_xdg(
                        toplevel.wl_surface(),
                        CornerRadiusSurface::Toplevel(toplevel.xdg_toplevel().downgrade()),
                        resource,
                        data_init,
                        id,
                    )
                } else if let Some(popup) =
                    state
                        .xdg_shell_state()
                        .popup_surfaces()
                        .iter()
                        .find(|popup| {
                            popup
                                .xdg_popup()
                                .data::<XdgShellSurfaceUserData>()
                                .unwrap()
                                .xdg_surface()
                                == &surface
                        })
                {
                    new_xdg(
                        popup.wl_surface(),
                        CornerRadiusSurface::Popup(popup.xdg_popup().downgrade()),
                        resource,
                        data_init,
                        id,
                    )
                } else {
                    resource.post_error(
                        cosmic_corner_radius_manager_v1::Error::NoRole as u32,
                        "xdg_surface has no known role object".to_string(),
                    );
                }
            }
            cosmic_corner_radius_manager_v1::Request::GetCornerRadiusLayer { id, layer } => {
                if let Some(surface) = state
                    .shell_state()
                    .layer_surfaces()
                    .find(|surface| surface.shell_surface() == &layer)
                {
                    let radius_exists = with_states(surface.wl_surface(), |surface_data| {
                        let hook_id = surface_data
                            .data_map
                            .get_or_insert_threadsafe(|| ToplevelHookId::new(None));
                        let guard = hook_id.lock().unwrap();
                        guard.as_ref().map(|(_, t)| t.upgrade().is_ok())
                    });
                    if radius_exists.unwrap_or_default() {
                        resource.post_error(
                                cosmic_corner_radius_manager_v1::Error::CornerRadiusExists as u32,
                                format!(
                                    "{resource:?} CosmicCornerRadiusToplevelV1 object already exists for the surface"
                                ),
                            );
                    }
                    let data = Mutex::new(CornerRadiusInternal {
                        surface: CornerRadiusSurface::Layer(layer.downgrade()),
                        corners: None,
                        padding: None,
                    });
                    let obj = data_init.init(id, data);
                    let obj_downgrade = obj.downgrade();

                    let needs_hook = radius_exists.is_none();
                    if needs_hook {
                        let hook_id =
                            add_pre_commit_hook::<D, _>(surface.wl_surface(), layer_radius_hook);
                        with_states(surface.wl_surface(), |surface_data| {
                            let hook_ids = surface_data
                                .data_map
                                .get_or_insert_threadsafe(|| LayerHookId::new(None));
                            let mut guard = hook_ids.lock().unwrap();
                            *guard = Some((hook_id, obj_downgrade));
                        });
                    }
                } // TODO: can this fail?
            }
            _ => unimplemented!(),
        }
    }

    fn destroyed(
        state: &mut D,
        _client: wayland_backend::server::ClientId,
        resource: &cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1,
        _data: &(),
    ) {
        let corner_radius_state = state.corner_radius_state();
        corner_radius_state.instances.retain(|i| i != resource);
    }
}

fn new_xdg<D>(
    wl_surface: &WlSurface,
    surface: CornerRadiusSurface,
    resource: &cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1,
    data_init: &mut smithay::reexports::wayland_server::DataInit<'_, D>,
    id: New<cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1>,
) where
    D: GlobalDispatch<cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1, ()>
        + Dispatch<cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1, ()>
        + Dispatch<cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1, CornerRadiusData>
        + CornerRadiusHandler
        + 'static,
{
    let radius_exists = with_states(wl_surface, |surface_data| {
        let hook_id = surface_data
            .data_map
            .get_or_insert_threadsafe(|| ToplevelHookId::new(None));
        let guard = hook_id.lock().unwrap();
        guard.as_ref().map(|(_, t)| t.upgrade().is_ok())
    });
    if radius_exists.unwrap_or_default() {
        resource.post_error(
            cosmic_corner_radius_manager_v1::Error::CornerRadiusExists as u32,
            format!(
                "{resource:?} CosmicCornerRadiusToplevelV1 object already exists for the surface"
            ),
        );
    }
    let data = Mutex::new(CornerRadiusInternal {
        surface,
        corners: None,
        padding: None,
    });
    let obj = data_init.init(id, data);
    let obj_downgrade = obj.downgrade();

    let needs_hook = radius_exists.is_none();
    if needs_hook {
        let hook_id = add_pre_commit_hook::<D, _>(wl_surface, xdg_radius_hook);
        with_states(wl_surface, |surface_data| {
            let hook_ids = surface_data
                .data_map
                .get_or_insert_threadsafe(|| ToplevelHookId::new(None));
            let mut guard = hook_ids.lock().unwrap();
            *guard = Some((hook_id, obj_downgrade));
        });
    }
}

impl<D>
    Dispatch<cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1, CornerRadiusData, D>
    for CornerRadiusState
where
    D: GlobalDispatch<cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1, ()>
        + Dispatch<cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1, ()>
        + Dispatch<cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1, CornerRadiusData>
        + CornerRadiusHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1,
        request: <cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1 as Resource>::Request,
        data: &CornerRadiusData,
        _dhandle: &DisplayHandle,
        _data_init: &mut smithay::reexports::wayland_server::DataInit<'_, D>,
    ) {
        match request {
            cosmic_corner_radius_toplevel_v1::Request::Destroy => {
                let mut guard = data.lock().unwrap();
                guard.corners = None;

                let Some(wl_surface) = (match &guard.surface {
                    CornerRadiusSurface::Toplevel(toplevel) => toplevel
                        .upgrade()
                        .ok()
                        .and_then(|toplevel| state.xdg_shell_state().get_toplevel(&toplevel))
                        .map(|toplevel| toplevel.wl_surface().clone()),
                    CornerRadiusSurface::Popup(popup) => popup
                        .upgrade()
                        .ok()
                        .and_then(|popup| state.xdg_shell_state().get_popup(&popup))
                        .map(|popup| popup.wl_surface().clone()),
                    CornerRadiusSurface::Layer(_) => unreachable!(),
                }) else {
                    return;
                };

                with_states(&wl_surface, |surface_data| {
                    if let Some(hook_ids_mutex) = surface_data.data_map.get::<ToplevelHookId>() {
                        let mut hook_id = hook_ids_mutex.lock().unwrap();
                        *hook_id = None;
                    }

                    let mut cached = surface_data.cached_state.get::<CacheableCorners>();
                    let pending = cached.pending();
                    *pending = CacheableCorners(None);
                });
                drop(guard);

                state.unset_corner_radius(data);
            }
            cosmic_corner_radius_toplevel_v1::Request::SetRadius {
                top_left,
                top_right,
                bottom_right,
                bottom_left,
            } => {
                let mut guard = data.lock().unwrap();
                guard.set_corner_radius(top_left, top_right, bottom_right, bottom_left);

                let Some(wl_surface) = (match &guard.surface {
                    CornerRadiusSurface::Toplevel(toplevel) => toplevel
                        .upgrade()
                        .ok()
                        .and_then(|toplevel| state.xdg_shell_state().get_toplevel(&toplevel))
                        .map(|toplevel| toplevel.wl_surface().clone()),
                    CornerRadiusSurface::Popup(popup) => popup
                        .upgrade()
                        .ok()
                        .and_then(|popup| state.xdg_shell_state().get_popup(&popup))
                        .map(|popup| popup.wl_surface().clone()),
                    CornerRadiusSurface::Layer(_) => unreachable!(),
                }) else {
                    resource.post_error(
                        cosmic_corner_radius_toplevel_v1::Error::ToplevelDestroyed as u32,
                        format!("{:?} No toplevel found", resource),
                    );
                    return;
                };
                with_states(&wl_surface, |s| {
                    let mut cached = s.cached_state.get::<CacheableCorners>();
                    let pending = cached.pending();
                    *pending = CacheableCorners(guard.corners);
                });
                drop(guard);

                state.set_corner_radius(data);
            }
            cosmic_corner_radius_toplevel_v1::Request::UnsetRadius => {
                let mut guard = data.lock().unwrap();
                guard.corners = None;

                let Some(wl_surface) = (match &guard.surface {
                    CornerRadiusSurface::Toplevel(toplevel) => toplevel
                        .upgrade()
                        .ok()
                        .and_then(|toplevel| state.xdg_shell_state().get_toplevel(&toplevel))
                        .map(|toplevel| toplevel.wl_surface().clone()),
                    CornerRadiusSurface::Popup(popup) => popup
                        .upgrade()
                        .ok()
                        .and_then(|popup| state.xdg_shell_state().get_popup(&popup))
                        .map(|popup| popup.wl_surface().clone()),
                    CornerRadiusSurface::Layer(_) => unreachable!(),
                }) else {
                    resource.post_error(
                        cosmic_corner_radius_toplevel_v1::Error::ToplevelDestroyed as u32,
                        format!("{:?} No toplevel found", resource),
                    );

                    return;
                };

                with_states(&wl_surface, |s| {
                    let mut cached = s.cached_state.get::<CacheableCorners>();
                    let pending = cached.pending();
                    *pending = CacheableCorners(None);
                });
                drop(guard);

                state.unset_corner_radius(data);
            }
            _ => unimplemented!(),
        }
    }
}

impl<D> Dispatch<cosmic_corner_radius_layer_v1::CosmicCornerRadiusLayerV1, CornerRadiusData, D>
    for CornerRadiusState
where
    D: GlobalDispatch<cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1, ()>
        + Dispatch<cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1, ()>
        + Dispatch<cosmic_corner_radius_layer_v1::CosmicCornerRadiusLayerV1, CornerRadiusData>
        + CornerRadiusHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &cosmic_corner_radius_layer_v1::CosmicCornerRadiusLayerV1,
        request: <cosmic_corner_radius_layer_v1::CosmicCornerRadiusLayerV1 as Resource>::Request,
        data: &CornerRadiusData,
        _dhandle: &DisplayHandle,
        _data_init: &mut smithay::reexports::wayland_server::DataInit<'_, D>,
    ) {
        match request {
            cosmic_corner_radius_layer_v1::Request::Destroy => {
                let mut guard = data.lock().unwrap();
                guard.corners = None;

                let CornerRadiusSurface::Layer(layer_surface) = &guard.surface else {
                    unreachable!("corner_radius_layer without layer shell?");
                };
                let Some(layer_surface) = layer_surface.upgrade().ok().and_then(|layer| {
                    state
                        .shell_state()
                        .layer_surfaces()
                        .find(|s| s.shell_surface() == &layer)
                }) else {
                    resource.post_error(
                        cosmic_corner_radius_layer_v1::Error::LayerDestroyed as u32,
                        format!("{:?} No layer found", resource),
                    );
                    return;
                };

                with_states(layer_surface.wl_surface(), |surface_data| {
                    if let Some(hook_ids_mutex) = surface_data.data_map.get::<LayerHookId>() {
                        let mut hook_id = hook_ids_mutex.lock().unwrap();
                        *hook_id = None;
                    }

                    let mut cached = surface_data.cached_state.get::<CacheableCorners>();
                    let pending = cached.pending();
                    *pending = CacheableCorners(None);

                    let mut cached = surface_data.cached_state.get::<CacheablePadding>();
                    let pending = cached.pending();
                    *pending = CacheablePadding(None);
                });
                drop(guard);

                state.unset_corner_radius(data);
                state.unset_padding(data);
            }
            cosmic_corner_radius_layer_v1::Request::SetRadius {
                top_left,
                top_right,
                bottom_right,
                bottom_left,
            } => {
                let mut guard = data.lock().unwrap();
                guard.set_corner_radius(top_left, top_right, bottom_right, bottom_left);

                let CornerRadiusSurface::Layer(layer_surface) = &guard.surface else {
                    unreachable!("corner_radius_layer without layer shell?");
                };
                let Some(layer_surface) = layer_surface.upgrade().ok().and_then(|layer| {
                    state
                        .shell_state()
                        .layer_surfaces()
                        .find(|s| s.shell_surface() == &layer)
                }) else {
                    resource.post_error(
                        cosmic_corner_radius_layer_v1::Error::LayerDestroyed as u32,
                        format!("{:?} No layer found", resource),
                    );
                    return;
                };

                with_states(layer_surface.wl_surface(), |s| {
                    let mut cached = s.cached_state.get::<CacheableCorners>();
                    let pending = cached.pending();
                    *pending = CacheableCorners(guard.corners);
                });
                drop(guard);

                state.set_corner_radius(data);
            }
            cosmic_corner_radius_layer_v1::Request::UnsetRadius => {
                let mut guard = data.lock().unwrap();
                guard.corners = None;

                let CornerRadiusSurface::Layer(layer_surface) = &guard.surface else {
                    unreachable!("corner_radius_layer without layer shell?");
                };
                let Some(layer_surface) = layer_surface.upgrade().ok().and_then(|layer| {
                    state
                        .shell_state()
                        .layer_surfaces()
                        .find(|s| s.shell_surface() == &layer)
                }) else {
                    resource.post_error(
                        cosmic_corner_radius_layer_v1::Error::LayerDestroyed as u32,
                        format!("{:?} No layer found", resource),
                    );
                    return;
                };

                with_states(layer_surface.wl_surface(), |s| {
                    let mut cached = s.cached_state.get::<CacheableCorners>();
                    let pending = cached.pending();
                    *pending = CacheableCorners(None);
                });
                drop(guard);

                state.unset_corner_radius(data);
            }
            cosmic_corner_radius_layer_v1::Request::SetPadding {
                top,
                right,
                bottom,
                left,
            } => {
                let mut guard = data.lock().unwrap();
                guard.set_padding(top, right, bottom, left);

                let CornerRadiusSurface::Layer(layer_surface) = &guard.surface else {
                    unreachable!("corner_radius_layer without layer shell?");
                };
                let Some(layer_surface) = layer_surface.upgrade().ok().and_then(|layer| {
                    state
                        .shell_state()
                        .layer_surfaces()
                        .find(|s| s.shell_surface() == &layer)
                }) else {
                    resource.post_error(
                        cosmic_corner_radius_layer_v1::Error::LayerDestroyed as u32,
                        format!("{:?} No layer found", resource),
                    );
                    return;
                };

                with_states(layer_surface.wl_surface(), |s| {
                    let mut cached = s.cached_state.get::<CacheablePadding>();
                    let pending = cached.pending();
                    *pending = CacheablePadding(guard.padding);
                });
                drop(guard);

                state.set_padding(data);
            }
            cosmic_corner_radius_layer_v1::Request::UnsetPadding => {
                let mut guard = data.lock().unwrap();
                guard.corners = None;

                let CornerRadiusSurface::Layer(layer_surface) = &guard.surface else {
                    unreachable!("corner_radius_layer without layer shell?");
                };
                let Some(layer_surface) = layer_surface.upgrade().ok().and_then(|layer| {
                    state
                        .shell_state()
                        .layer_surfaces()
                        .find(|s| s.shell_surface() == &layer)
                }) else {
                    resource.post_error(
                        cosmic_corner_radius_layer_v1::Error::LayerDestroyed as u32,
                        format!("{:?} No layer found", resource),
                    );
                    return;
                };

                with_states(layer_surface.wl_surface(), |s| {
                    let mut cached = s.cached_state.get::<CacheablePadding>();
                    let pending = cached.pending();
                    *pending = CacheablePadding(None);
                });
                drop(guard);

                state.unset_padding(data);
            }
            _ => unimplemented!(),
        }
    }
}

fn xdg_radius_hook<D: 'static>(_state: &mut D, _dh: &DisplayHandle, surface: &WlSurface) {
    with_states(surface, |surface_data| {
        let corners = *surface_data
            .cached_state
            .get::<CacheableCorners>()
            .pending();
        if surface_data
            .cached_state
            .get::<SurfaceCachedState>()
            .pending()
            .geometry
            .zip(corners.0.as_ref())
            .is_some_and(|(geo, corners)| {
                let half_min_dim = (geo.size.w.min(geo.size.h) / 2) as u32;
                corners.top_right > half_min_dim
                    || corners.top_left > half_min_dim
                    || corners.bottom_right > half_min_dim
                    || corners.bottom_left > half_min_dim
            })
            && let Some(hook) = surface_data.data_map.get::<ToplevelHookId>()
        {
            let hook_ref = hook.lock().unwrap();
            if let Some((_, obj)) = hook_ref.as_ref()
                && let Ok(obj) = obj.upgrade()
            {
                obj.post_error(
                    cosmic_corner_radius_toplevel_v1::Error::RadiusTooLarge as u32,
                    format!("{obj:?} corner radius too large"),
                );
            }
        }
    });
}

fn pad_rect(
    mut rect: Rectangle<i32, Logical>,
    padding: &Padding,
) -> Option<Rectangle<i32, Logical>> {
    rect.size.h = rect.size.h.checked_sub(padding.top)?;
    rect.loc.x += padding.top;
    rect.size.w = rect.size.w.checked_sub(padding.left)?;
    rect.loc.y += padding.left;
    rect.size.h = rect.size.h.checked_sub(padding.bottom)?;
    rect.size.w = rect.size.w.checked_sub(padding.right)?;
    Some(rect)
}

fn layer_radius_hook<D: 'static>(_state: &mut D, _dh: &DisplayHandle, surface: &WlSurface) {
    let bbox = bbox_from_surface_tree(surface, Point::default());
    with_states(surface, |surface_data| {
        let corners = *surface_data
            .cached_state
            .get::<CacheableCorners>()
            .pending();
        let padding = *surface_data
            .cached_state
            .get::<CacheablePadding>()
            .pending();
        let empty = Padding::default();
        let Some(padded_box) = pad_rect(bbox, padding.0.as_ref().unwrap_or(&empty)) else {
            if let Some(hook) = surface_data.data_map.get::<LayerHookId>() {
                let hook_ref = hook.lock().unwrap();
                if let Some((_, obj)) = hook_ref.as_ref()
                    && let Ok(obj) = obj.upgrade()
                {
                    obj.post_error(
                        cosmic_corner_radius_layer_v1::Error::PaddingTooLarge as u32,
                        format!("{obj:?} padding too large"),
                    );
                }
            }
            return;
        };

        if corners.0.as_ref().is_some_and(|corners| {
            let half_min_dim = (padded_box.size.w.min(padded_box.size.h) / 2) as u32;
            corners.top_right > half_min_dim
                || corners.top_left > half_min_dim
                || corners.bottom_right > half_min_dim
                || corners.bottom_left > half_min_dim
        }) && let Some(hook) = surface_data.data_map.get::<LayerHookId>()
        {
            let hook_ref = hook.lock().unwrap();
            if let Some((_, obj)) = hook_ref.as_ref()
                && let Ok(obj) = obj.upgrade()
            {
                obj.post_error(
                    cosmic_corner_radius_layer_v1::Error::RadiusTooLarge as u32,
                    format!("{obj:?} corner radius too large"),
                );
            }
        }
    });
}

pub type CornerRadiusData = Mutex<CornerRadiusInternal>;

#[derive(Debug)]
pub enum CornerRadiusSurface {
    Toplevel(Weak<XdgToplevel>),
    Popup(Weak<XdgPopup>),
    Layer(Weak<ZwlrLayerSurfaceV1>),
}

#[derive(Debug)]
pub struct CornerRadiusInternal {
    pub surface: CornerRadiusSurface,
    pub corners: Option<Corners>,
    pub padding: Option<Padding>,
}

#[derive(Debug, Copy, Clone)]
pub struct Corners {
    pub top_left: u32,
    pub top_right: u32,
    pub bottom_right: u32,
    pub bottom_left: u32,
}

#[derive(Debug, Copy, Clone, Default)]
pub struct Padding {
    pub top: i32,
    pub right: i32,
    pub bottom: i32,
    pub left: i32,
}

#[derive(Default, Debug, Copy, Clone)]
pub struct CacheableCorners(pub Option<Corners>);

impl Cacheable for CacheableCorners {
    fn commit(&mut self, _dh: &DisplayHandle) -> Self {
        *self
    }
    fn merge_into(self, into: &mut Self, _dh: &DisplayHandle) {
        *into = self;
    }
}

#[derive(Default, Debug, Copy, Clone)]
pub struct CacheablePadding(pub Option<Padding>);

impl Cacheable for CacheablePadding {
    fn commit(&mut self, _dh: &DisplayHandle) -> Self {
        *self
    }
    fn merge_into(self, into: &mut Self, _dh: &DisplayHandle) {
        *into = self;
    }
}

impl CornerRadiusInternal {
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

    fn set_padding(&mut self, top: i32, right: i32, bottom: i32, left: i32) {
        let padding = Padding {
            top,
            right,
            bottom,
            left,
        };
        self.padding = Some(padding);
    }
}

macro_rules! delegate_corner_radius {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::corner_radius::v1::server::cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1: ()
        ] => $crate::wayland::protocols::corner_radius::CornerRadiusState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::corner_radius::v1::server::cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1: ()
        ] => $crate::wayland::protocols::corner_radius::CornerRadiusState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::corner_radius::v1::server::cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1: CornerRadiusData
        ] => $crate::wayland::protocols::corner_radius::CornerRadiusState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::corner_radius::v1::server::cosmic_corner_radius_layer_v1::CosmicCornerRadiusLayerV1: CornerRadiusData
        ] => $crate::wayland::protocols::corner_radius::CornerRadiusState);
    };
}
pub(crate) use delegate_corner_radius;
