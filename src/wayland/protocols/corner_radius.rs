use cosmic_protocols::corner_radius::v1::server::cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1;
use cosmic_protocols::corner_radius::v1::server::{
    cosmic_corner_radius_manager_v1, cosmic_corner_radius_toplevel_v1,
};
use smithay::utils::HookId;
use smithay::wayland::compositor::Cacheable;
use smithay::wayland::compositor::add_pre_commit_hook;
use smithay::wayland::compositor::with_states;
use smithay::wayland::shell::xdg::SurfaceCachedState;
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
            > + CornerRadiusHandler
            + 'static,
    {
        let global = dh
            .create_global::<D, cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1, _>(
                1,
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

pub trait CornerRadiusHandler: XdgShellHandler {
    fn corner_radius_state(&mut self) -> &mut CornerRadiusState;
    fn set_corner_radius(
        &mut self,
        toplevel: &cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1,
        data: &CornerRadiusData,
    );
    fn unset_corner_radius(
        &mut self,
        toplevel: &cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1,
        data: &CornerRadiusData,
    );
}

impl<D> GlobalDispatch<cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1, (), D>
    for CornerRadiusState
where
    D: GlobalDispatch<cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1, ()>
        + Dispatch<cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1, ()>
        + Dispatch<cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1, CornerRadiusData>
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
                            format!("{resource:?} CosmicCornerRadiusToplevelV1 object already exists for the surface"),
                        );
                    }
                    let data = Mutex::new(CornerRadiusInternal {
                        toplevel: toplevel.downgrade(),
                        corners: None,
                    });
                    let obj = data_init.init(id, data);
                    let obj_downgrade = obj.downgrade();

                    let needs_hook = radius_exists.is_none();
                    if needs_hook {
                        let hook_id = add_pre_commit_hook::<D, _>(
                            surface.wl_surface(),
                            move |_, _dh, surface| {
                                let corner_radii_too_big = with_states(surface, |surface_data| {
                                    let corners = *surface_data
                                        .cached_state
                                        .get::<CacheableCorners>()
                                        .pending();
                                    surface_data
                                        .cached_state
                                        .get::<SurfaceCachedState>()
                                        .pending()
                                        .geometry
                                        .zip(corners.0.as_ref())
                                        .is_some_and(|(geo, corners)| {
                                            let half_min_dim =
                                                u8::try_from(geo.size.w.min(geo.size.h) / 2)
                                                    .unwrap_or(u8::MAX);
                                            corners.top_right > half_min_dim
                                                || corners.top_left > half_min_dim
                                                || corners.bottom_right > half_min_dim
                                                || corners.bottom_left > half_min_dim
                                        })
                                });

                                if corner_radii_too_big {
                                    obj.post_error(
                                        cosmic_corner_radius_toplevel_v1::Error::RadiusTooLarge
                                            as u32,
                                        format!("{obj:?} corner radius too large"),
                                    );
                                }
                            },
                        );

                        with_states(surface.wl_surface(), |surface_data| {
                            let hook_ids = surface_data
                                .data_map
                                .get_or_insert_threadsafe(|| ToplevelHookId::new(None));
                            let mut guard = hook_ids.lock().unwrap();
                            *guard = Some((hook_id, obj_downgrade));
                        });
                    }
                }
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

                let Ok(toplevel) = guard.toplevel.upgrade() else {
                    return;
                };

                if let Some(surface) = state.xdg_shell_state().get_toplevel(&toplevel) {
                    with_states(surface.wl_surface(), |surface_data| {
                        if let Some(hook_ids_mutex) = surface_data.data_map.get::<ToplevelHookId>()
                        {
                            let mut hook_id = hook_ids_mutex.lock().unwrap();
                            *hook_id = None;
                        }
                    });
                }

                if let Some(surface) = state.xdg_shell_state().get_toplevel(&toplevel) {
                    with_states(surface.wl_surface(), |s| {
                        let mut cached = s.cached_state.get::<CacheableCorners>();
                        let pending = cached.pending();
                        *pending = CacheableCorners(None);
                    });
                }
                drop(guard);

                state.unset_corner_radius(resource, data);
            }
            cosmic_corner_radius_toplevel_v1::Request::SetRadius {
                top_left,
                top_right,
                bottom_right,
                bottom_left,
            } => {
                let mut guard = data.lock().unwrap();
                guard.set_corner_radius(top_left, top_right, bottom_right, bottom_left);
                let Ok(toplevel) = guard.toplevel.upgrade() else {
                    resource.post_error(
                        cosmic_corner_radius_toplevel_v1::Error::ToplevelDestroyed as u32,
                        format!("{:?} No toplevel found", resource),
                    );
                    return;
                };

                if let Some(surface) = state.xdg_shell_state().get_toplevel(&toplevel) {
                    with_states(surface.wl_surface(), |s| {
                        let mut cached = s.cached_state.get::<CacheableCorners>();
                        let pending = cached.pending();
                        *pending = CacheableCorners(guard.corners);
                    });
                }
                drop(guard);

                state.set_corner_radius(resource, data);
            }
            cosmic_corner_radius_toplevel_v1::Request::UnsetRadius => {
                let mut guard = data.lock().unwrap();
                guard.corners = None;
                let Ok(toplevel) = guard.toplevel.upgrade() else {
                    resource.post_error(
                        cosmic_corner_radius_toplevel_v1::Error::ToplevelDestroyed as u32,
                        format!("{:?} No toplevel found", resource),
                    );
                    return;
                };

                if let Some(surface) = state.xdg_shell_state().get_toplevel(&toplevel) {
                    with_states(surface.wl_surface(), |s| {
                        let mut cached = s.cached_state.get::<CacheableCorners>();
                        let pending = cached.pending();
                        *pending = CacheableCorners(None);
                    });
                }
                drop(guard);

                state.unset_corner_radius(resource, data);
            }
            _ => unimplemented!(),
        }
    }
}

pub type CornerRadiusData = Mutex<CornerRadiusInternal>;

#[derive(Debug)]
pub struct CornerRadiusInternal {
    pub toplevel: Weak<XdgToplevel>,
    pub corners: Option<Corners>,
}

#[derive(Debug, Copy, Clone)]
pub struct Corners {
    pub top_left: u8,
    pub top_right: u8,
    pub bottom_right: u8,
    pub bottom_left: u8,
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

impl CornerRadiusInternal {
    fn set_corner_radius(
        &mut self,
        top_left: u32,
        top_right: u32,
        bottom_right: u32,
        bottom_left: u32,
    ) {
        let corners = Corners {
            top_left: top_left.clamp(u8::MIN as u32, u8::MAX as u32) as u8,
            top_right: top_right.clamp(u8::MIN as u32, u8::MAX as u32) as u8,
            bottom_right: bottom_right.clamp(u8::MIN as u32, u8::MAX as u32) as u8,
            bottom_left: bottom_left.clamp(u8::MIN as u32, u8::MAX as u32) as u8,
        };
        self.corners = Some(corners);
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
    };
}
pub(crate) use delegate_corner_radius;
