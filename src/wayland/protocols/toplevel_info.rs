// SPDX-License-Identifier: GPL-3.0-only

use std::{
    collections::HashMap,
    sync::Mutex,
};

use smithay::{
    desktop::Window,
    reexports::{
        wayland_protocols::xdg::shell::server::xdg_toplevel,
        wayland_server::{
            backend::{ClientId, GlobalId, ObjectId},
            protocol::wl_surface::WlSurface,
            Client, DataInit, Dispatch, DisplayHandle,
            GlobalDispatch, New, Resource,
        },
    },
    utils::{IsAlive, Rectangle, Logical},
    wayland::{
        compositor::with_states, output::Output, shell::xdg::XdgToplevelSurfaceRoleAttributes,
    },
};

use super::workspace::{WorkspaceHandle, WorkspaceHandler, WorkspaceState};

use cosmic_protocols::toplevel_info::v1::server::{
    zcosmic_toplevel_handle_v1::{self, State as States, ZcosmicToplevelHandleV1},
    zcosmic_toplevel_info_v1::{self, ZcosmicToplevelInfoV1},
};

pub struct ToplevelInfoState<D> {
    dh: DisplayHandle,
    pub(super) toplevels: Vec<Window>,
    instances: Vec<ZcosmicToplevelInfoV1>,
    global: GlobalId,
    _dispatch_data: std::marker::PhantomData<D>,
}

pub trait ToplevelInfoHandler: WorkspaceHandler + Sized {
    fn toplevel_info_state(&self) -> &ToplevelInfoState<Self>;
    fn toplevel_info_state_mut(&mut self) -> &mut ToplevelInfoState<Self>;
}

pub struct ToplevelInfoGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

#[derive(Default)]
pub(super) struct ToplevelStateInner {
    instances: Vec<ZcosmicToplevelHandleV1>,
    outputs: Vec<Output>,
    workspaces: Vec<WorkspaceHandle>,
    minimized: bool,
    pub(super) rectangles: HashMap<ClientId, (WlSurface, Rectangle<i32, Logical>)>,
}
pub(super) type ToplevelState = Mutex<ToplevelStateInner>;

pub struct ToplevelHandleStateInner {
    outputs: Vec<Output>,
    workspaces: Vec<WorkspaceHandle>,
    title: String,
    app_id: String,
    states: Vec<States>,
    pub(super) window: Window,
}
pub type ToplevelHandleState = Mutex<ToplevelHandleStateInner>;

impl ToplevelHandleStateInner {
    fn from_window(window: &Window) -> ToplevelHandleState {
        ToplevelHandleState::new(
            ToplevelHandleStateInner {
                outputs: Vec::new(),
                workspaces: Vec::new(),
                title: String::new(),
                app_id: String::new(),
                states: Vec::new(),
                window: window.clone(),
            }
        )
    }
}

impl<D> GlobalDispatch<ZcosmicToplevelInfoV1, ToplevelInfoGlobalData, D>
    for ToplevelInfoState<D>
where
    D: GlobalDispatch<ZcosmicToplevelInfoV1, ToplevelInfoGlobalData>
        + Dispatch<ZcosmicToplevelInfoV1, ()>
        + Dispatch<ZcosmicToplevelHandleV1, ToplevelHandleState>
        + ToplevelInfoHandler
        + 'static,
{
    fn bind(
        state: &mut D,
        dh: &DisplayHandle,
        _client: &Client,
        resource: New<ZcosmicToplevelInfoV1>,
        _global_data: &ToplevelInfoGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        let instance = data_init.init(resource, ());
        for window in &state.toplevel_info_state().toplevels {
            send_toplevel_to_client::<D>(dh, Some(state.workspace_state()), &instance, window);
        }
        state.toplevel_info_state_mut().instances.push(instance);
    }

    fn can_view(client: Client, global_data: &ToplevelInfoGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ZcosmicToplevelInfoV1, (), D> for ToplevelInfoState<D>
where
    D: GlobalDispatch<ZcosmicToplevelInfoV1, ToplevelInfoGlobalData>
        + Dispatch<ZcosmicToplevelInfoV1, ()>
        + Dispatch<ZcosmicToplevelHandleV1, ToplevelHandleState>
        + ToplevelInfoHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        obj: &ZcosmicToplevelInfoV1,
        request: zcosmic_toplevel_info_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_toplevel_info_v1::Request::Stop => {
                state
                    .toplevel_info_state_mut()
                    .instances
                    .retain(|i| i != obj);
            }
            _ => {}
        }
    }

    fn destroyed(state: &mut D, _client: ClientId, resource: ObjectId, _data: &()) {
        state
            .toplevel_info_state_mut()
            .instances
            .retain(|i| i.id() != resource);
    }
}

impl<D> Dispatch<ZcosmicToplevelHandleV1, ToplevelHandleState, D> for ToplevelInfoState<D>
where
    D: GlobalDispatch<ZcosmicToplevelInfoV1, ToplevelInfoGlobalData>
        + Dispatch<ZcosmicToplevelInfoV1, ()>
        + Dispatch<ZcosmicToplevelHandleV1, ToplevelHandleState>
        + ToplevelInfoHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _obj: &ZcosmicToplevelHandleV1,
        request: zcosmic_toplevel_handle_v1::Request,
        _data: &ToplevelHandleState,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_toplevel_handle_v1::Request::Destroy => {}
            _ => {}
        }
    }

    fn destroyed(
        state: &mut D,
        _client: ClientId,
        resource: ObjectId,
        _data: &ToplevelHandleState,
    ) {
        for toplevel in &state.toplevel_info_state_mut().toplevels {
            if let Some(state) = toplevel.user_data().get::<ToplevelState>() {
                state
                    .lock()
                    .unwrap()
                    .instances
                    .retain(|i| i.id() != resource);
            }
        }
    }
}

impl<D> ToplevelInfoState<D>
where
    D: GlobalDispatch<ZcosmicToplevelInfoV1, ToplevelInfoGlobalData>
        + Dispatch<ZcosmicToplevelInfoV1, ()>
        + Dispatch<ZcosmicToplevelHandleV1, ToplevelHandleState>
        + ToplevelInfoHandler
        + 'static,
{
    pub fn new<F>(dh: &DisplayHandle, client_filter: F) -> ToplevelInfoState<D>
    where
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        let global = dh.create_global::<D, ZcosmicToplevelInfoV1, _>(
            1,
            ToplevelInfoGlobalData {
                filter: Box::new(client_filter),
            },
        );
        ToplevelInfoState {
            dh: dh.clone(),
            toplevels: Vec::new(),
            instances: Vec::new(),
            global,
            _dispatch_data: std::marker::PhantomData,
        }
    }

    pub fn new_toplevel(&mut self, toplevel: &Window) {
        toplevel
            .user_data()
            .insert_if_missing(ToplevelState::default);
        for instance in &self.instances {
            send_toplevel_to_client::<D>(&self.dh, None, instance, toplevel);
        }
        self.toplevels.push(toplevel.clone());
    }

    pub fn toplevel_enter_output(&mut self, toplevel: &Window, output: &Output) {
        if let Some(state) = toplevel.user_data().get::<ToplevelState>() {
            state.lock().unwrap().outputs.push(output.clone());
        }
    }

    pub fn toplevel_leave_output(&mut self, toplevel: &Window, output: &Output) {
        if let Some(state) = toplevel.user_data().get::<ToplevelState>() {
            state.lock().unwrap().outputs.retain(|o| o != output);
        }
    }

    pub fn toplevel_enter_workspace(&mut self, toplevel: &Window, workspace: &WorkspaceHandle) {
        if let Some(state) = toplevel.user_data().get::<ToplevelState>() {
            state.lock().unwrap().workspaces.push(workspace.clone());
        }
    }

    pub fn toplevel_leave_workspace(&mut self, toplevel: &Window, workspace: &WorkspaceHandle) {
        if let Some(state) = toplevel.user_data().get::<ToplevelState>() {
            state.lock().unwrap().workspaces.retain(|w| w != workspace);
        }
    }

    pub fn refresh(&mut self, workspace_state: Option<&WorkspaceState<D>>) {
        self.toplevels.retain(|window| {
            let mut state = window
                .user_data()
                .get::<ToplevelState>()
                .unwrap()
                .lock()
                .unwrap();
            state.rectangles
                .retain(|_, (surface, _)| surface.alive());
            if window.alive() {
                std::mem::drop(state);
                for instance in &self.instances {
                    send_toplevel_to_client::<D>(&self.dh, workspace_state, instance, window);
                }
                true
            } else {
                for handle in &state.instances {
                    // don't send events to stopped instances
                    if self
                        .instances
                        .iter()
                        .any(|i| i.id().same_client_as(&handle.id()))
                    {
                        handle.closed();
                    }
                }
                false
            }
        });
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

fn send_toplevel_to_client<D>(
    dh: &DisplayHandle,
    workspace_state: Option<&WorkspaceState<D>>,
    info: &ZcosmicToplevelInfoV1,
    window: &Window,
) where
    D: GlobalDispatch<ZcosmicToplevelInfoV1, ToplevelInfoGlobalData>
        + Dispatch<ZcosmicToplevelInfoV1, ()>
        + Dispatch<ZcosmicToplevelHandleV1, ToplevelHandleState>
        + ToplevelInfoHandler
        + 'static,
{
    let mut state = window
        .user_data()
        .get::<ToplevelState>()
        .unwrap()
        .lock()
        .unwrap();
    let instance = match state
        .instances
        .iter()
        .find(|i| i.id().same_client_as(&info.id()))
    {
        Some(i) => i,
        None => {
            if let Ok(client) = dh.get_client(info.id()) {
                if let Ok(toplevel_handle) = client
                    .create_resource::<ZcosmicToplevelHandleV1, _, D>(
                        dh,
                        info.version(),
                        ToplevelHandleStateInner::from_window(window),
                    )
                {
                    state.instances.push(toplevel_handle);
                    state.instances.last().unwrap()
                } else {
                    return;
                }
            } else {
                return;
            }
        }
    };

    let mut handle_state = instance
        .data::<ToplevelHandleState>()
        .unwrap()
        .lock()
        .unwrap();
    let mut changed = false;
    with_states(window.toplevel().wl_surface(), |states| {
        let attributes = states
            .data_map
            .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
            .unwrap()
            .lock()
            .unwrap();

        if handle_state.title != attributes.title.as_deref().unwrap_or(&"") {
            handle_state.title = attributes.title.clone().unwrap_or_else(String::new);
            instance.title(handle_state.title.clone());
            changed = true;
        }
        if handle_state.app_id != attributes.app_id.as_deref().unwrap_or(&"") {
            handle_state.title = attributes.app_id.clone().unwrap_or_else(String::new);
            instance.app_id(handle_state.app_id.clone());
            changed = true;
        }

        if (handle_state.states.contains(&States::Maximized)
            != attributes
                .current
                .states
                .contains(xdg_toplevel::State::Maximized))
            || (handle_state.states.contains(&States::Fullscreen)
                != attributes
                    .current
                    .states
                    .contains(xdg_toplevel::State::Fullscreen))
            || (handle_state.states.contains(&States::Activated)
                != attributes
                    .current
                    .states
                    .contains(xdg_toplevel::State::Activated))
            || (handle_state.states.contains(&States::Minimized) != state.minimized)
        {
            let mut states = Vec::new();
            if attributes
                .current
                .states
                .contains(xdg_toplevel::State::Maximized)
            {
                states.push(States::Maximized);
            }
            if attributes
                .current
                .states
                .contains(xdg_toplevel::State::Fullscreen)
            {
                states.push(States::Fullscreen);
            }
            if attributes
                .current
                .states
                .contains(xdg_toplevel::State::Activated)
            {
                states.push(States::Activated);
            }
            if attributes
                .current
                .states
                .contains(xdg_toplevel::State::Maximized)
            {
                states.push(States::Maximized);
            }
            handle_state.states = states.clone();

            let states: Vec<u8> = {
                let ratio = std::mem::size_of::<States>() / std::mem::size_of::<u8>();
                let ptr = states.as_mut_ptr() as *mut u8;
                let len = states.len() * ratio;
                let cap = states.capacity() * ratio;
                std::mem::forget(states);
                unsafe { Vec::from_raw_parts(ptr, len, cap) }
            };
            instance.state(states);
            changed = true;
        }
    });

    if let Ok(client) = dh.get_client(instance.id()) {
        for new_output in state
            .outputs
            .iter()
            .filter(|o| !handle_state.outputs.contains(o))
        {
            new_output.with_client_outputs(dh, &client, |_dh, wl_output| {
                instance.output_enter(wl_output);
            });
            changed = true;
        }
        for old_output in handle_state
            .outputs
            .iter()
            .filter(|o| !state.outputs.contains(o))
        {
            old_output.with_client_outputs(dh, &client, |_dh, wl_output| {
                instance.output_leave(wl_output);
            });
            changed = true;
        }
        handle_state.outputs = state.outputs.clone();
    }

    if let Some(workspace_state) = workspace_state {
        for new_workspace in state
            .workspaces
            .iter()
            .filter(|w| !handle_state.workspaces.contains(w))
        {
            if let Some(handle) =
                workspace_state.raw_workspace_handle(&new_workspace, &instance.id())
            {
                instance.workspace_enter(&handle);
                changed = true;
            }
        }
        for old_workspace in handle_state
            .workspaces
            .iter()
            .filter(|w| !state.workspaces.contains(w))
        {
            if let Some(handle) =
                workspace_state.raw_workspace_handle(&old_workspace, &instance.id())
            {
                instance.workspace_leave(&handle);
                changed = true;
            }
        }
        handle_state.workspaces = state.workspaces.clone();
    }

    if changed {
        instance.done();
    }
}

pub(super) fn window_from_handle(handle: ZcosmicToplevelHandleV1) -> Window {
    handle
        .data::<ToplevelHandleState>()
        .unwrap()
        .lock()
        .unwrap()
        .window
        .clone()
}

macro_rules! delegate_toplevel_info {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::toplevel_info::v1::server::zcosmic_toplevel_info_v1::ZcosmicToplevelInfoV1: $crate::wayland::protocols::toplevel_info::ToplevelInfoGlobalData
        ] => $crate::wayland::protocols::toplevel_info::ToplevelInfoState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::toplevel_info::v1::server::zcosmic_toplevel_info_v1::ZcosmicToplevelInfoV1: ()
        ] => $crate::wayland::protocols::toplevel_info::ToplevelInfoState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::toplevel_info::v1::server::zcosmic_toplevel_handle_v1::ZcosmicToplevelHandleV1: $crate::wayland::protocols::toplevel_info::ToplevelHandleState
        ] => $crate::wayland::protocols::toplevel_info::ToplevelInfoState<Self>);
    };
}
pub(crate) use delegate_toplevel_info;
