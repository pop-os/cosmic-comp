// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    desktop::Window,
    input::{Seat, SeatHandler},
    reexports::wayland_server::{
        backend::{ClientId, GlobalId, ObjectId},
        protocol::wl_surface::WlSurface,
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource,
    },
    utils::{Logical, Rectangle},
    wayland::output::Output,
};

pub use cosmic_protocols::toplevel_management::v1::server::zcosmic_toplevel_manager_v1::ZcosmicToplelevelManagementCapabilitiesV1 as ManagementCapabilities;
use cosmic_protocols::toplevel_management::v1::server::zcosmic_toplevel_manager_v1::{
    self, ZcosmicToplevelManagerV1,
};

use super::toplevel_info::{window_from_handle, ToplevelInfoHandler, ToplevelState};

pub struct ToplevelManagementState {
    instances: Vec<ZcosmicToplevelManagerV1>,
    capabilities: Vec<ManagementCapabilities>,
    global: GlobalId,
}

#[allow(unused_variables)]
pub trait ToplevelManagementHandler: ToplevelInfoHandler + SeatHandler {
    fn toplevel_management_state(&mut self) -> &mut ToplevelManagementState;

    fn activate(&mut self, dh: &DisplayHandle, window: &Window, seat: Option<Seat<Self>>) {}
    fn close(&mut self, dh: &DisplayHandle, window: &Window) {}

    fn fullscreen(&mut self, dh: &DisplayHandle, window: &Window, output: Option<Output>) {}
    fn unfullscreen(&mut self, dh: &DisplayHandle, window: &Window) {}
    fn maximize(&mut self, dh: &DisplayHandle, window: &Window) {}
    fn unmaximize(&mut self, dh: &DisplayHandle, window: &Window) {}
    fn minimize(&mut self, dh: &DisplayHandle, window: &Window) {}
    fn unminimize(&mut self, dh: &DisplayHandle, window: &Window) {}
}

pub struct ToplevelManagerGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

impl ToplevelManagementState {
    pub fn new<D, F>(
        dh: &DisplayHandle,
        capabilities: Vec<ManagementCapabilities>,
        client_filter: F,
    ) -> ToplevelManagementState
    where
        D: GlobalDispatch<ZcosmicToplevelManagerV1, ToplevelManagerGlobalData>
            + Dispatch<ZcosmicToplevelManagerV1, ()>
            + ToplevelManagementHandler
            + 'static,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        let global = dh.create_global::<D, ZcosmicToplevelManagerV1, _>(
            1,
            ToplevelManagerGlobalData {
                filter: Box::new(client_filter),
            },
        );
        ToplevelManagementState {
            capabilities,
            instances: Vec::new(),
            global,
        }
    }

    pub fn rectangle_for(
        &mut self,
        window: &Window,
        client: &ClientId,
    ) -> Option<(WlSurface, Rectangle<i32, Logical>)> {
        if let Some(state) = window.user_data().get::<ToplevelState>() {
            state.lock().unwrap().rectangles.get(client).cloned()
        } else {
            None
        }
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

impl<D> GlobalDispatch<ZcosmicToplevelManagerV1, ToplevelManagerGlobalData, D>
    for ToplevelManagementState
where
    D: GlobalDispatch<ZcosmicToplevelManagerV1, ToplevelManagerGlobalData>
        + Dispatch<ZcosmicToplevelManagerV1, ()>
        + ToplevelManagementHandler
        + 'static,
{
    fn bind(
        state: &mut D,
        _dh: &DisplayHandle,
        _client: &Client,
        resource: New<ZcosmicToplevelManagerV1>,
        _global_data: &ToplevelManagerGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        let instance = data_init.init(resource, ());
        let capabilities = {
            let mut caps = state.toplevel_management_state().capabilities.clone();
            let ratio = std::mem::size_of::<ManagementCapabilities>() / std::mem::size_of::<u8>();
            let ptr = caps.as_mut_ptr() as *mut u8;
            let len = caps.len() * ratio;
            let cap = caps.capacity() * ratio;
            std::mem::forget(caps);
            unsafe { Vec::from_raw_parts(ptr, len, cap) }
        };
        instance.capabilities(capabilities);
        state.toplevel_management_state().instances.push(instance);
    }

    fn can_view(client: Client, global_data: &ToplevelManagerGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ZcosmicToplevelManagerV1, (), D> for ToplevelManagementState
where
    D: GlobalDispatch<ZcosmicToplevelManagerV1, ToplevelManagerGlobalData>
        + Dispatch<ZcosmicToplevelManagerV1, ()>
        + ToplevelManagementHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _obj: &ZcosmicToplevelManagerV1,
        request: zcosmic_toplevel_manager_v1::Request,
        _data: &(),
        dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_toplevel_manager_v1::Request::Activate { toplevel, seat } => {
                let window = window_from_handle(toplevel).unwrap();
                state.activate(dh, &window, Seat::from_resource(&seat));
            }
            zcosmic_toplevel_manager_v1::Request::Close { toplevel } => {
                let window = window_from_handle(toplevel).unwrap();
                state.close(dh, &window);
            }
            zcosmic_toplevel_manager_v1::Request::SetFullscreen { toplevel, output } => {
                let window = window_from_handle(toplevel).unwrap();
                state.fullscreen(dh, &window, output.as_ref().and_then(Output::from_resource))
            }
            zcosmic_toplevel_manager_v1::Request::UnsetFullscreen { toplevel } => {
                let window = window_from_handle(toplevel).unwrap();
                state.unfullscreen(dh, &window);
            }
            zcosmic_toplevel_manager_v1::Request::SetMaximized { toplevel } => {
                let window = window_from_handle(toplevel).unwrap();
                state.maximize(dh, &window);
            }
            zcosmic_toplevel_manager_v1::Request::UnsetMaximized { toplevel } => {
                let window = window_from_handle(toplevel).unwrap();
                state.unmaximize(dh, &window);
            }
            zcosmic_toplevel_manager_v1::Request::SetMinimized { toplevel } => {
                let window = window_from_handle(toplevel).unwrap();
                state.minimize(dh, &window);
            }
            zcosmic_toplevel_manager_v1::Request::UnsetMinimized { toplevel } => {
                let window = window_from_handle(toplevel).unwrap();
                state.unminimize(dh, &window);
            }
            zcosmic_toplevel_manager_v1::Request::SetRectangle {
                toplevel,
                surface,
                x,
                y,
                width,
                height,
            } => {
                let window = window_from_handle(toplevel).unwrap();
                if let Some(toplevel_state) = window.user_data().get::<ToplevelState>() {
                    let mut toplevel_state = toplevel_state.lock().unwrap();
                    if let Some(client) = surface.client_id() {
                        if width == 0 && height == 0 {
                            toplevel_state.rectangles.remove(&client);
                        } else {
                            toplevel_state.rectangles.insert(
                                client,
                                (
                                    surface,
                                    Rectangle::from_loc_and_size((x, y), (width, height)),
                                ),
                            );
                        }
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    fn destroyed(state: &mut D, client: ClientId, resource: ObjectId, _data: &()) {
        let mng_state = state.toplevel_management_state();
        mng_state.instances.retain(|i| i.id() != resource);
        if !mng_state
            .instances
            .iter()
            .any(|i| i.client_id().map(|c| c == client).unwrap_or(false))
        {
            for toplevel in state.toplevel_info_state_mut().toplevels.iter() {
                if let Some(toplevel_state) = toplevel.user_data().get::<ToplevelState>() {
                    toplevel_state.lock().unwrap().rectangles.remove(&client);
                }
            }
        }
    }
}

macro_rules! delegate_toplevel_management {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::toplevel_management::v1::server::zcosmic_toplevel_manager_v1::ZcosmicToplevelManagerV1: $crate::wayland::protocols::toplevel_management::ToplevelManagerGlobalData
        ] => $crate::wayland::protocols::toplevel_management::ToplevelManagementState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::toplevel_management::v1::server::zcosmic_toplevel_manager_v1::ZcosmicToplevelManagerV1: ()
        ] => $crate::wayland::protocols::toplevel_management::ToplevelManagementState);
    };
}
pub(crate) use delegate_toplevel_management;
