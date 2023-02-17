// SPDX-License-Identifier: GPL-3.0-only

use std::{collections::HashMap, sync::Mutex};

// Re-export only the actual code, and then only use this re-export
// The `generated` module below is just some boilerplate to properly isolate stuff
// and avoid exposing internal details.
//
// You can use all the types from my_protocol as if they went from `wayland_client::protocol`.
pub use generated::{ext_foreign_toplevel_handle_v1, ext_foreign_toplevel_list_v1};

mod generated {
    use smithay::reexports::wayland_server;

    pub mod __interfaces {
        use wayland_backend;
        wayland_scanner::generate_interfaces!(
            "resources/protocols/ext-foreign-toplevel-list-v1.xml"
        );
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/ext-foreign-toplevel-list-v1.xml");
}

use rand::distributions::{Alphanumeric, DistString};
use smithay::{
    output::Output,
    reexports::wayland_server::{
        protocol::wl_surface::WlSurface, Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch,
        New, Resource,
    },
    utils::{user_data::UserDataMap, IsAlive, Logical, Rectangle},
};

use wayland_backend::server::{ClientId, GlobalId, ObjectId};

use self::{
    ext_foreign_toplevel_list_v1::ExtForeignToplevelListV1,
    generated::ext_foreign_toplevel_handle_v1::ExtForeignToplevelHandleV1,
};

use super::workspace::{WorkspaceHandle, WorkspaceHandler, WorkspaceState};

pub trait Window: IsAlive + Clone + Send {
    fn title(&self) -> String;
    fn app_id(&self) -> String;
    fn is_activated(&self) -> bool;
    fn is_maximized(&self) -> bool;
    fn is_fullscreen(&self) -> bool;
    fn is_minimized(&self) -> bool;
    fn user_data(&self) -> &UserDataMap;
}

pub struct ToplevelInfoState<D, W: Window> {
    dh: DisplayHandle,
    pub(super) toplevels: Vec<W>,
    instances: Vec<ExtForeignToplevelListV1>,
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
    instances: Vec<ExtForeignToplevelHandleV1>,
    outputs: Vec<Output>,
    workspaces: Vec<WorkspaceHandle>,
    pub(super) rectangles: HashMap<ClientId, (WlSurface, Rectangle<i32, Logical>)>,
}
pub(super) type ToplevelState = Mutex<ToplevelStateInner>;

pub struct ToplevelHandleStateInner<W: Window> {
    _info: ExtForeignToplevelListV1,
    title: String,
    app_id: String,
    identifier: String,
    pub(super) window: W,
}
pub type ToplevelHandleState<W> = Mutex<ToplevelHandleStateInner<W>>;

impl<W: Window> ToplevelHandleStateInner<W> {
    fn from_window(info: ExtForeignToplevelListV1, window: &W) -> ToplevelHandleState<W> {
        ToplevelHandleState::new(ToplevelHandleStateInner {
            _info: info,
            title: String::new(),
            app_id: String::new(),
            // The protocol allows other allocation schemes for the identifier
            identifier: Alphanumeric.sample_string(&mut rand::thread_rng(), 32),
            window: window.clone(),
        })
    }
}

impl<D, W> GlobalDispatch<ExtForeignToplevelListV1, ToplevelInfoGlobalData, D>
    for ToplevelInfoState<D, W>
where
    D: GlobalDispatch<ExtForeignToplevelListV1, ToplevelInfoGlobalData>
        + Dispatch<ExtForeignToplevelListV1, ()>
        + Dispatch<ExtForeignToplevelHandleV1, ToplevelHandleState<W>>
        + ToplevelInfoHandler<Window = W>
        + 'static,
    W: Window + 'static,
{
    fn bind(
        state: &mut D,
        dh: &DisplayHandle,
        _client: &Client,
        resource: New<ExtForeignToplevelListV1>,
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

impl<D, W> Dispatch<ExtForeignToplevelListV1, (), D> for ToplevelInfoState<D, W>
where
    D: GlobalDispatch<ExtForeignToplevelListV1, ToplevelInfoGlobalData>
        + Dispatch<ExtForeignToplevelListV1, ()>
        + Dispatch<ExtForeignToplevelHandleV1, ToplevelHandleState<W>>
        + ToplevelInfoHandler<Window = W>
        + 'static,
    W: Window + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        obj: &ExtForeignToplevelListV1,
        request: ext_foreign_toplevel_list_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            ext_foreign_toplevel_list_v1::Request::Stop => {
                state
                    .toplevel_info_state_mut()
                    .instances
                    .retain(|i| i != obj);

                // cosmic has already removed the instance, this ensures that no further events are sent.
                obj.finished();
            }
            ext_foreign_toplevel_list_v1::Request::Destroy => {
                // Destructor handles cleanup
            }
            // Out of tree this will become non-exhaustive
            #[allow(unreachable_patterns)]
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

impl<D, W> Dispatch<ExtForeignToplevelHandleV1, ToplevelHandleState<W>, D>
    for ToplevelInfoState<D, W>
where
    D: GlobalDispatch<ExtForeignToplevelListV1, ToplevelInfoGlobalData>
        + Dispatch<ExtForeignToplevelListV1, ()>
        + Dispatch<ExtForeignToplevelHandleV1, ToplevelHandleState<W>>
        + ToplevelInfoHandler<Window = W>
        + 'static,
    W: Window,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _obj: &ExtForeignToplevelHandleV1,
        request: ext_foreign_toplevel_handle_v1::Request,
        _data: &ToplevelHandleState<W>,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            ext_foreign_toplevel_handle_v1::Request::Destroy => {
                // Destructor handles cleanup
            }

            // Out of tree this will become non-exhaustive
            #[allow(unreachable_patterns)]
            _ => {}
        }
    }

    fn destroyed(
        state: &mut D,
        _client: ClientId,
        resource: ObjectId,
        _data: &ToplevelHandleState<W>,
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

impl<D, W> ToplevelInfoState<D, W>
where
    D: GlobalDispatch<ExtForeignToplevelListV1, ToplevelInfoGlobalData>
        + Dispatch<ExtForeignToplevelListV1, ()>
        + Dispatch<ExtForeignToplevelHandleV1, ToplevelHandleState<W>>
        + ToplevelInfoHandler<Window = W>
        + 'static,
    W: Window + 'static,
{
    pub fn new<F>(dh: &DisplayHandle, client_filter: F) -> ToplevelInfoState<D, W>
    where
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        let global = dh.create_global::<D, ExtForeignToplevelListV1, _>(
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

    pub fn refresh(&mut self, workspace_state: Option<&WorkspaceState<D>>) {
        self.toplevels.retain(|window| {
            let mut state = window
                .user_data()
                .get::<ToplevelState>()
                .unwrap()
                .lock()
                .unwrap();
            state.rectangles.retain(|_, (surface, _)| surface.alive());
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
    _workspace_state: Option<&WorkspaceState<D>>,
    info: &ExtForeignToplevelListV1,
    window: &W,
) where
    D: GlobalDispatch<ExtForeignToplevelListV1, ToplevelInfoGlobalData>
        + Dispatch<ExtForeignToplevelListV1, ()>
        + Dispatch<ExtForeignToplevelHandleV1, ToplevelHandleState<W>>
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
                // The protocol implicitly mandates a new handle objects is created per instance because of the
                // new_id argument.
                if let Ok(toplevel_handle) = client
                    .create_resource::<ExtForeignToplevelHandleV1, _, D>(
                        dh,
                        info.version(),
                        ToplevelHandleStateInner::from_window(info.clone(), window),
                    )
                {
                    info.toplevel(&toplevel_handle);

                    let data = toplevel_handle
                        .data::<ToplevelHandleState<W>>()
                        .unwrap()
                        .lock()
                        .unwrap();

                    // The protocol states that the identifier should only be sent when creating a new handle.
                    toplevel_handle.identifier(data.identifier.clone());
                    drop(data); // double borrow occurs unless we explicitly drop the data.

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

    if changed {
        instance.done();
    }
}

pub fn window_from_handle<W: Window + 'static>(handle: ExtForeignToplevelHandleV1) -> Option<W> {
    handle
        .data::<ToplevelHandleState<W>>()
        .map(|state| state.lock().unwrap().window.clone())
}

macro_rules! delegate_foreign_toplevel_info {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty, $window: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::ext_foreign_toplevel_list::ext_foreign_toplevel_list_v1::ExtForeignToplevelListV1: $crate::wayland::protocols::ext_foreign_toplevel_list::ToplevelInfoGlobalData
        ] => $crate::wayland::protocols::ext_foreign_toplevel_list::ToplevelInfoState<Self, $window>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::ext_foreign_toplevel_list::ext_foreign_toplevel_list_v1::ExtForeignToplevelListV1: ()
        ] => $crate::wayland::protocols::ext_foreign_toplevel_list::ToplevelInfoState<Self, $window>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::ext_foreign_toplevel_list::ext_foreign_toplevel_handle_v1::ExtForeignToplevelHandleV1: $crate::wayland::protocols::ext_foreign_toplevel_list::ToplevelHandleState<$window>
        ] => $crate::wayland::protocols::ext_foreign_toplevel_list::ToplevelInfoState<Self, $window>);
    };
}
pub(crate) use delegate_foreign_toplevel_info;
