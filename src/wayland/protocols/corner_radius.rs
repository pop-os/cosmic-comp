use cosmic_protocols::corner_radius::v1::server::{
    cosmic_corner_radius_manager_v1, cosmic_corner_radius_toplevel_v1,
};
use smithay::reexports::{
    wayland_protocols::xdg::shell::server::xdg_toplevel::XdgToplevel,
    wayland_server::{Client, Dispatch, DisplayHandle, GlobalDispatch, Resource, Weak},
};
use smithay::wayland::compositor::with_states;
use smithay::wayland::shell::xdg::ToplevelSurface;
use std::sync::Mutex;
use wayland_backend::server::GlobalId;

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

pub trait CornerRadiusHandler {
    fn corner_radius_state(&mut self) -> &mut CornerRadiusState;
    fn toplevel_from_resource(&mut self, toplevel: &XdgToplevel) -> Option<ToplevelSurface>;
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
        _resource: &cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1,
        request: <cosmic_corner_radius_manager_v1::CosmicCornerRadiusManagerV1 as smithay::reexports::wayland_server::Resource>::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut smithay::reexports::wayland_server::DataInit<'_, D>,
    ) {
        match request {
            cosmic_corner_radius_manager_v1::Request::Destroy => {
                let corner_radius_state = state.corner_radius_state();
                corner_radius_state.instances.retain(|i| i != _resource);
            }
            cosmic_corner_radius_manager_v1::Request::GetCornerRadius { id, toplevel } => {
                let data = Mutex::new(CornerRadiusInternal {
                    toplevel: toplevel.downgrade(),
                    top_left: 0,
                    top_right: 0,
                    bottom_right: 0,
                    bottom_left: 0,
                });
                let instance = data_init.init(id, data);

                if let Some(surface) = state.toplevel_from_resource(&toplevel) {
                    with_states(surface.wl_surface(), |s| {
                        let data = s
                            .data_map
                            .get_or_insert_threadsafe(|| Mutex::new(instance.downgrade()));
                        let mut guard = data.lock().unwrap();
                        *guard = instance.downgrade();
                    });
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
        _state: &mut D,
        _client: &Client,
        resource: &cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1,
        request: <cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1 as Resource>::Request,
        data: &CornerRadiusData,
        _dhandle: &DisplayHandle,
        _data_init: &mut smithay::reexports::wayland_server::DataInit<'_, D>,
    ) {
        match request {
            cosmic_corner_radius_toplevel_v1::Request::Destroy => {
                // TODO do we want to unset it after it is destroyed or just leave it?
                _state.unset_corner_radius(resource, data);
            }
            cosmic_corner_radius_toplevel_v1::Request::SetRadius {
                top_left,
                top_right,
                bottom_right,
                bottom_left,
            } => {
                {
                    let mut guard = data.lock().unwrap();
                    guard.set_corner_radius(
                        top_left as u8,
                        top_right as u8,
                        bottom_right as u8,
                        bottom_left as u8,
                    );
                }

                _state.set_corner_radius(resource, data);
            }
            cosmic_corner_radius_toplevel_v1::Request::UnsetRadius => {
                _state.unset_corner_radius(resource, data);
            }
            _ => unimplemented!(),
        }
    }
}

pub type CornerRadiusData = Mutex<CornerRadiusInternal>;

#[derive(Debug)]
pub struct CornerRadiusInternal {
    pub toplevel: Weak<XdgToplevel>,
    pub top_left: u8,
    pub top_right: u8,
    pub bottom_right: u8,
    pub bottom_left: u8,
}

impl CornerRadiusInternal {
    fn set_corner_radius(
        &mut self,
        top_left: u8,
        top_right: u8,
        bottom_right: u8,
        bottom_left: u8,
    ) {
        self.top_left = top_left;
        self.top_right = top_right;
        self.bottom_right = bottom_right;
        self.bottom_left = bottom_left;
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
