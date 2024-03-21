// SPDX-License-Identifier: GPL-3.0-only

use std::sync::Mutex;

use smithay::{
    output::Output,
    reexports::wayland_server::{
        backend::{ClientId, GlobalId},
        protocol::wl_surface::WlSurface,
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
    },
    utils::{user_data::UserDataMap, IsAlive, Logical, Rectangle},
};

use super::workspace::{WorkspaceHandle, WorkspaceHandler, WorkspaceState};

use cosmic_protocols::toplevel_info::v1::server::{
    zcosmic_toplevel_handle_v1::{self, State as States, ZcosmicToplevelHandleV1},
    zcosmic_toplevel_info_v1::{self, ZcosmicToplevelInfoV1},
};

pub trait Window: IsAlive + Clone + PartialEq + Send {
    fn title(&self) -> String;
    fn app_id(&self) -> String;
    fn is_activated(&self) -> bool;
    fn is_maximized(&self) -> bool;
    fn is_fullscreen(&self) -> bool;
    fn is_minimized(&self) -> bool;
    fn user_data(&self) -> &UserDataMap;
}

#[derive(Debug)]
pub struct ToplevelInfoState<D, W: Window> {
    dh: DisplayHandle,
    pub(super) toplevels: Vec<W>,
    instances: Vec<ZcosmicToplevelInfoV1>,
    global: GlobalId,
    _dispatch_data: std::marker::PhantomData<D>,
}

pub trait ToplevelInfoHandler: WorkspaceHandler + Sized {
    type Window: Window;
    fn toplevel_info_state(&self) -> &ToplevelInfoState<Self, Self::Window>;
    fn toplevel_info_state_mut(&mut self) -> &mut ToplevelInfoState<Self, Self::Window>;
}

pub struct ToplevelInfoGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

#[derive(Default)]
pub(super) struct ToplevelStateInner {
    instances: Vec<ZcosmicToplevelHandleV1>,
    outputs: Vec<Output>,
    workspaces: Vec<WorkspaceHandle>,
    pub(super) rectangles: Vec<(Weak<WlSurface>, Rectangle<i32, Logical>)>,
}
pub(super) type ToplevelState = Mutex<ToplevelStateInner>;

pub struct ToplevelHandleStateInner<W: Window> {
    outputs: Vec<Output>,
    workspaces: Vec<WorkspaceHandle>,
    title: String,
    app_id: String,
    states: Vec<States>,
    pub(super) window: W,
}
pub type ToplevelHandleState<W> = Mutex<ToplevelHandleStateInner<W>>;

impl<W: Window> ToplevelHandleStateInner<W> {
    fn from_window(window: &W) -> ToplevelHandleState<W> {
        ToplevelHandleState::new(ToplevelHandleStateInner {
            outputs: Vec::new(),
            workspaces: Vec::new(),
            title: String::new(),
            app_id: String::new(),
            states: Vec::new(),
            window: window.clone(),
        })
    }
}

impl<D, W> GlobalDispatch<ZcosmicToplevelInfoV1, ToplevelInfoGlobalData, D>
    for ToplevelInfoState<D, W>
where
    D: GlobalDispatch<ZcosmicToplevelInfoV1, ToplevelInfoGlobalData>
        + Dispatch<ZcosmicToplevelInfoV1, ()>
        + Dispatch<ZcosmicToplevelHandleV1, ToplevelHandleState<W>>
        + ToplevelInfoHandler<Window = W>
        + 'static,
    W: Window + 'static,
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
            send_toplevel_to_client::<D, W>(dh, Some(state.workspace_state()), &instance, window);
        }
        state.toplevel_info_state_mut().instances.push(instance);
    }

    fn can_view(client: Client, global_data: &ToplevelInfoGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D, W> Dispatch<ZcosmicToplevelInfoV1, (), D> for ToplevelInfoState<D, W>
where
    D: GlobalDispatch<ZcosmicToplevelInfoV1, ToplevelInfoGlobalData>
        + Dispatch<ZcosmicToplevelInfoV1, ()>
        + Dispatch<ZcosmicToplevelHandleV1, ToplevelHandleState<W>>
        + ToplevelInfoHandler<Window = W>
        + 'static,
    W: Window,
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

    fn destroyed(state: &mut D, _client: ClientId, resource: &ZcosmicToplevelInfoV1, _data: &()) {
        state
            .toplevel_info_state_mut()
            .instances
            .retain(|i| i != resource);
    }
}

impl<D, W> Dispatch<ZcosmicToplevelHandleV1, ToplevelHandleState<W>, D> for ToplevelInfoState<D, W>
where
    D: GlobalDispatch<ZcosmicToplevelInfoV1, ToplevelInfoGlobalData>
        + Dispatch<ZcosmicToplevelInfoV1, ()>
        + Dispatch<ZcosmicToplevelHandleV1, ToplevelHandleState<W>>
        + ToplevelInfoHandler<Window = W>
        + 'static,
    W: Window,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _obj: &ZcosmicToplevelHandleV1,
        request: zcosmic_toplevel_handle_v1::Request,
        _data: &ToplevelHandleState<W>,
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
        resource: &ZcosmicToplevelHandleV1,
        _data: &ToplevelHandleState<W>,
    ) {
        for toplevel in &state.toplevel_info_state_mut().toplevels {
            if let Some(state) = toplevel.user_data().get::<ToplevelState>() {
                state.lock().unwrap().instances.retain(|i| i != resource);
            }
        }
    }
}

impl<D, W> ToplevelInfoState<D, W>
where
    D: GlobalDispatch<ZcosmicToplevelInfoV1, ToplevelInfoGlobalData>
        + Dispatch<ZcosmicToplevelInfoV1, ()>
        + Dispatch<ZcosmicToplevelHandleV1, ToplevelHandleState<W>>
        + ToplevelInfoHandler<Window = W>
        + 'static,
    W: Window + 'static,
{
    pub fn new<F>(dh: &DisplayHandle, client_filter: F) -> ToplevelInfoState<D, W>
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

    pub fn new_toplevel(&mut self, toplevel: &W) {
        toplevel
            .user_data()
            .insert_if_missing(ToplevelState::default);
        for instance in &self.instances {
            send_toplevel_to_client::<D, W>(&self.dh, None, instance, toplevel);
        }
        self.toplevels.push(toplevel.clone());
    }

    pub fn toplevel_enter_output(&mut self, toplevel: &W, output: &Output) {
        if let Some(state) = toplevel.user_data().get::<ToplevelState>() {
            state.lock().unwrap().outputs.push(output.clone());
        }
    }

    pub fn toplevel_leave_output(&mut self, toplevel: &W, output: &Output) {
        if let Some(state) = toplevel.user_data().get::<ToplevelState>() {
            state.lock().unwrap().outputs.retain(|o| o != output);
        }
    }

    pub fn toplevel_enter_workspace(&mut self, toplevel: &W, workspace: &WorkspaceHandle) {
        if let Some(state) = toplevel.user_data().get::<ToplevelState>() {
            state.lock().unwrap().workspaces.push(workspace.clone());
        }
    }

    pub fn toplevel_leave_workspace(&mut self, toplevel: &W, workspace: &WorkspaceHandle) {
        if let Some(state) = toplevel.user_data().get::<ToplevelState>() {
            state.lock().unwrap().workspaces.retain(|w| w != workspace);
        }
    }

    pub fn remove_toplevel(&mut self, toplevel: &W) {
        if let Some(state) = toplevel.user_data().get::<ToplevelState>() {
            let mut state_inner = state.lock().unwrap();
            for handle in &state_inner.instances {
                // don't send events to stopped instances
                if self
                    .instances
                    .iter()
                    .any(|i| i.id().same_client_as(&handle.id()))
                {
                    handle.closed();
                }
            }
            *state_inner = Default::default();
        }
        self.toplevels.retain(|w| w != toplevel);
    }

    pub fn refresh(&mut self, workspace_state: Option<&WorkspaceState<D>>) {
        self.toplevels.retain(|window| {
            let mut state = window
                .user_data()
                .get::<ToplevelState>()
                .unwrap()
                .lock()
                .unwrap();
            state
                .rectangles
                .retain(|(surface, _)| surface.upgrade().is_ok());
            if window.alive() {
                std::mem::drop(state);
                for instance in &self.instances {
                    send_toplevel_to_client::<D, W>(&self.dh, workspace_state, instance, window);
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

fn send_toplevel_to_client<D, W: 'static>(
    dh: &DisplayHandle,
    workspace_state: Option<&WorkspaceState<D>>,
    info: &ZcosmicToplevelInfoV1,
    window: &W,
) where
    D: GlobalDispatch<ZcosmicToplevelInfoV1, ToplevelInfoGlobalData>
        + Dispatch<ZcosmicToplevelInfoV1, ()>
        + Dispatch<ZcosmicToplevelHandleV1, ToplevelHandleState<W>>
        + ToplevelInfoHandler<Window = W>
        + 'static,
    W: Window,
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
                    info.toplevel(&toplevel_handle);
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
        .data::<ToplevelHandleState<W>>()
        .unwrap()
        .lock()
        .unwrap();
    let mut changed = false;
    if handle_state.title != window.title() {
        handle_state.title = window.title();
        instance.title(handle_state.title.clone());
        changed = true;
    }
    if handle_state.app_id != window.app_id() {
        handle_state.app_id = window.app_id();
        instance.app_id(handle_state.app_id.clone());
        changed = true;
    }

    if (handle_state.states.contains(&States::Maximized) != window.is_maximized())
        || (handle_state.states.contains(&States::Fullscreen) != window.is_fullscreen())
        || (handle_state.states.contains(&States::Activated) != window.is_activated())
        || (handle_state.states.contains(&States::Minimized) != window.is_minimized())
    {
        let mut states = Vec::new();
        if window.is_maximized() {
            states.push(States::Maximized);
        }
        if window.is_fullscreen() {
            states.push(States::Fullscreen);
        }
        if window.is_activated() {
            states.push(States::Activated);
        }
        if window.is_minimized() {
            states.push(States::Minimized);
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

    if let Ok(client) = dh.get_client(instance.id()) {
        for new_output in state
            .outputs
            .iter()
            .filter(|o| !handle_state.outputs.contains(o))
        {
            for wl_output in new_output.client_outputs(&client) {
                instance.output_enter(&wl_output);
            }
            changed = true;
        }
        for old_output in handle_state
            .outputs
            .iter()
            .filter(|o| !state.outputs.contains(o))
        {
            for wl_output in old_output.client_outputs(&client) {
                instance.output_leave(&wl_output);
            }
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

pub fn window_from_handle<W: Window + 'static>(handle: ZcosmicToplevelHandleV1) -> Option<W> {
    handle
        .data::<ToplevelHandleState<W>>()
        .map(|state| state.lock().unwrap().window.clone())
}

macro_rules! delegate_toplevel_info {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty, $window: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::toplevel_info::v1::server::zcosmic_toplevel_info_v1::ZcosmicToplevelInfoV1: $crate::wayland::protocols::toplevel_info::ToplevelInfoGlobalData
        ] => $crate::wayland::protocols::toplevel_info::ToplevelInfoState<Self, $window>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::toplevel_info::v1::server::zcosmic_toplevel_info_v1::ZcosmicToplevelInfoV1: ()
        ] => $crate::wayland::protocols::toplevel_info::ToplevelInfoState<Self, $window>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::toplevel_info::v1::server::zcosmic_toplevel_handle_v1::ZcosmicToplevelHandleV1: $crate::wayland::protocols::toplevel_info::ToplevelHandleState<$window>
        ] => $crate::wayland::protocols::toplevel_info::ToplevelInfoState<Self, $window>);
    };
}
pub(crate) use delegate_toplevel_info;
