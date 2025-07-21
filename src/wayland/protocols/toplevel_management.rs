// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    input::{Seat, SeatHandler},
    output::Output,
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource,
        backend::{ClientId, GlobalId},
        protocol::wl_surface::WlSurface,
    },
    utils::{Logical, Rectangle},
};

pub use cosmic_protocols::toplevel_management::v1::server::zcosmic_toplevel_manager_v1::ZcosmicToplelevelManagementCapabilitiesV1 as ManagementCapabilities;
use cosmic_protocols::toplevel_management::v1::server::zcosmic_toplevel_manager_v1::{
    self, ZcosmicToplevelManagerV1,
};

use super::{
    toplevel_info::{ToplevelInfoHandler, ToplevelState, Window, window_from_handle},
    workspace::WorkspaceHandle,
};

#[derive(Debug)]
pub struct ToplevelManagementState {
    instances: Vec<ZcosmicToplevelManagerV1>,
    capabilities: Vec<ManagementCapabilities>,
    global: GlobalId,
}

pub trait ManagementWindow: Window {
    fn close(&self);
}

#[allow(unused_variables)]
pub trait ToplevelManagementHandler: ToplevelInfoHandler + SeatHandler
where
    <Self as ToplevelInfoHandler>::Window: ManagementWindow,
{
    fn toplevel_management_state(&mut self) -> &mut ToplevelManagementState;

    fn activate(
        &mut self,
        dh: &DisplayHandle,
        window: &<Self as ToplevelInfoHandler>::Window,
        seat: Option<Seat<Self>>,
    ) {
    }
    fn close(&mut self, dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {}

    fn fullscreen(
        &mut self,
        dh: &DisplayHandle,
        window: &<Self as ToplevelInfoHandler>::Window,
        output: Option<Output>,
    ) {
    }
    fn unfullscreen(&mut self, dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
    }
    fn maximize(&mut self, dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {}
    fn unmaximize(&mut self, dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {}
    fn minimize(&mut self, dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {}
    fn unminimize(&mut self, dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {}
    fn set_sticky(&mut self, dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {}
    fn unset_sticky(&mut self, dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
    }
    fn move_to_workspace(
        &mut self,
        dh: &DisplayHandle,
        window: &<Self as ToplevelInfoHandler>::Window,
        workspace: WorkspaceHandle,
        output: Output,
    ) {
    }
}

pub fn toplevel_rectangle_for(
    window: &impl ManagementWindow,
) -> impl Iterator<Item = (WlSurface, Rectangle<i32, Logical>)> {
    if let Some(state) = window.user_data().get::<ToplevelState>() {
        let mut state = state.lock().unwrap();
        state
            .rectangles
            .retain(|(surface, _)| surface.upgrade().is_ok());
        Some(
            state
                .rectangles
                .iter()
                .map(|(surface, rect)| (surface.upgrade().unwrap(), *rect))
                .collect::<Vec<_>>()
                .into_iter(),
        )
        .into_iter()
        .flatten()
    } else {
        None.into_iter().flatten()
    }
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
        <D as ToplevelInfoHandler>::Window: ManagementWindow,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        let global = dh.create_global::<D, ZcosmicToplevelManagerV1, _>(
            4,
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
    <D as ToplevelInfoHandler>::Window: ManagementWindow,
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
        let capabilities = state
            .toplevel_management_state()
            .capabilities
            .iter()
            .flat_map(|cap| (*cap as u32).to_ne_bytes())
            .collect::<Vec<u8>>();

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
    <D as ToplevelInfoHandler>::Window: ManagementWindow,
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
            zcosmic_toplevel_manager_v1::Request::SetSticky { toplevel } => {
                let window = window_from_handle(toplevel).unwrap();
                state.set_sticky(dh, &window);
            }
            zcosmic_toplevel_manager_v1::Request::UnsetSticky { toplevel } => {
                let window = window_from_handle(toplevel).unwrap();
                state.unset_sticky(dh, &window);
            }
            zcosmic_toplevel_manager_v1::Request::SetRectangle {
                toplevel,
                surface,
                x,
                y,
                width,
                height,
            } => {
                let window =
                    window_from_handle::<<D as ToplevelInfoHandler>::Window>(toplevel).unwrap();
                if let Some(toplevel_state) = window.user_data().get::<ToplevelState>() {
                    let mut toplevel_state = toplevel_state.lock().unwrap();
                    if width == 0 && height == 0 {
                        toplevel_state
                            .rectangles
                            .retain(|(s, _)| s.id() != surface.id());
                    } else {
                        toplevel_state.rectangles.push((
                            surface.downgrade(),
                            Rectangle::new((x, y).into(), (width, height).into()),
                        ));
                    }
                }
            }
            zcosmic_toplevel_manager_v1::Request::MoveToWorkspace { .. } => {}
            zcosmic_toplevel_manager_v1::Request::MoveToExtWorkspace {
                toplevel,
                workspace,
                output,
            } => {
                let window = window_from_handle(toplevel).unwrap();
                if let Some(workspace_handle) =
                    state.workspace_state().get_ext_workspace_handle(&workspace)
                {
                    if let Some(output) = Output::from_resource(&output) {
                        state.move_to_workspace(dh, &window, workspace_handle, output);
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    fn destroyed(state: &mut D, client: ClientId, resource: &ZcosmicToplevelManagerV1, _data: &()) {
        let mng_state = state.toplevel_management_state();
        mng_state.instances.retain(|i| i != resource);
        if !mng_state
            .instances
            .iter()
            .any(|i| i.client().map(|c| c.id() == client).unwrap_or(false))
        {
            for toplevel in state.toplevel_info_state_mut().toplevels.iter() {
                if let Some(toplevel_state) = toplevel.user_data().get::<ToplevelState>() {
                    toplevel_state.lock().unwrap().rectangles.retain(|(s, _)| {
                        s.upgrade()
                            .ok()
                            .and_then(|s| s.client())
                            .map(|c| c.id() != client)
                            .unwrap_or(false)
                    });
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
