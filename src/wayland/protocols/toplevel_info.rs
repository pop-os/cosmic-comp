// SPDX-License-Identifier: GPL-3.0-only

use std::{collections::HashSet, sync::Mutex};

use smithay::{
    output::Output,
    reexports::{
        wayland_protocols::ext::foreign_toplevel_list::v1::server::ext_foreign_toplevel_handle_v1::ExtForeignToplevelHandleV1,
        wayland_server::{
            Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
            backend::{ClientId, GlobalId},
            protocol::{wl_output::WlOutput, wl_surface::WlSurface},
        },
    },
    utils::{IsAlive, Logical, Rectangle, user_data::UserDataMap},
    wayland::foreign_toplevel_list::{
        ForeignToplevelHandle, ForeignToplevelListHandler, ForeignToplevelListState,
    },
};

use crate::utils::prelude::{Global, OutputExt, RectGlobalExt};

use super::workspace::{WorkspaceHandle, WorkspaceHandler, WorkspaceState};

use cosmic_protocols::toplevel_info::v1::server::{
    zcosmic_toplevel_handle_v1::{self, State as States, ZcosmicToplevelHandleV1},
    zcosmic_toplevel_info_v1::{self, ZcosmicToplevelInfoV1},
};
use tracing::error;

pub trait Window: IsAlive + Clone + PartialEq + Send {
    fn title(&self) -> String;
    fn app_id(&self) -> String;
    fn is_activated(&self) -> bool;
    fn is_maximized(&self) -> bool;
    fn is_fullscreen(&self) -> bool;
    fn is_minimized(&self) -> bool;
    fn is_sticky(&self) -> bool;
    fn is_resizing(&self) -> bool;
    fn global_geometry(&self) -> Option<Rectangle<i32, Global>>;
    fn user_data(&self) -> &UserDataMap;
}

#[derive(Debug)]
pub struct ToplevelInfoState<D, W: Window> {
    dh: DisplayHandle,
    pub(super) toplevels: Vec<W>,
    instances: Vec<ZcosmicToplevelInfoV1>,
    dirty: bool,
    last_dirty: bool,
    pub(in crate::wayland) foreign_toplevel_list: ForeignToplevelListState,
    global: GlobalId,
    _dispatch_data: std::marker::PhantomData<D>,
}

pub trait ToplevelInfoHandler: ForeignToplevelListHandler + WorkspaceHandler + Sized {
    type Window: Window;
    fn toplevel_info_state(&self) -> &ToplevelInfoState<Self, Self::Window>;
    fn toplevel_info_state_mut(&mut self) -> &mut ToplevelInfoState<Self, Self::Window>;
}

pub struct ToplevelInfoGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

#[derive(Default)]
pub(super) struct ToplevelStateInner {
    foreign_handle: Option<ForeignToplevelHandle>,
    instances: Vec<(Weak<ZcosmicToplevelInfoV1>, ZcosmicToplevelHandleV1)>,
    outputs: Vec<Output>,
    workspaces: Vec<WorkspaceHandle>,
    pub(super) rectangles: Vec<(Weak<WlSurface>, Rectangle<i32, Logical>)>,
}
pub(super) type ToplevelState = Mutex<ToplevelStateInner>;

impl ToplevelStateInner {
    fn from_foreign(foreign_handle: ForeignToplevelHandle) -> Mutex<Self> {
        Mutex::new(ToplevelStateInner {
            foreign_handle: Some(foreign_handle),
            ..Default::default()
        })
    }

    pub fn foreign_handle(&self) -> Option<&ForeignToplevelHandle> {
        self.foreign_handle.as_ref()
    }

    pub fn in_workspace(&self, handle: &WorkspaceHandle) -> bool {
        self.workspaces.contains(handle)
    }
}

pub struct ToplevelHandleStateInner<W: Window> {
    outputs: Vec<Output>,
    geometry: Option<Rectangle<i32, Global>>,
    wl_outputs: HashSet<WlOutput>,
    workspaces: Vec<WorkspaceHandle>,
    title: String,
    app_id: String,
    states: Option<Vec<States>>,
    pub(super) window: Option<W>,
}
pub type ToplevelHandleState<W> = Mutex<ToplevelHandleStateInner<W>>;

impl<W: Window> ToplevelHandleStateInner<W> {
    fn from_window(window: &W) -> ToplevelHandleState<W> {
        ToplevelHandleState::new(ToplevelHandleStateInner {
            outputs: Vec::new(),
            geometry: None,
            wl_outputs: HashSet::new(),
            workspaces: Vec::new(),
            title: String::new(),
            app_id: String::new(),
            states: None,
            window: Some(window.clone()),
        })
    }

    fn empty() -> ToplevelHandleState<W> {
        ToplevelHandleState::new(ToplevelHandleStateInner {
            outputs: Vec::new(),
            geometry: None,
            wl_outputs: HashSet::new(),
            workspaces: Vec::new(),
            title: String::new(),
            app_id: String::new(),
            states: None,
            window: None,
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
            send_toplevel_to_client::<D, W>(dh, state.workspace_state(), &instance, window);
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
    W: Window + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        obj: &ZcosmicToplevelInfoV1,
        request: zcosmic_toplevel_info_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_toplevel_info_v1::Request::GetCosmicToplevel {
                cosmic_toplevel,
                foreign_toplevel,
            } => {
                let toplevel_state = state.toplevel_info_state();
                if let Some(window) = toplevel_state.toplevels.iter().find(|w| {
                    w.user_data().get::<ToplevelState>().and_then(|inner| {
                        inner
                            .lock()
                            .unwrap()
                            .foreign_handle
                            .as_ref()
                            .map(|handle| handle.identifier())
                    }) == ForeignToplevelHandle::from_resource(&foreign_toplevel)
                        .map(|handle| handle.identifier())
                }) {
                    let instance = data_init.init(
                        cosmic_toplevel,
                        ToplevelHandleStateInner::from_window(window),
                    );
                    window
                        .user_data()
                        .get::<ToplevelState>()
                        .unwrap()
                        .lock()
                        .unwrap()
                        .instances
                        .push((obj.downgrade(), instance));
                } else {
                    let _ = data_init.init(cosmic_toplevel, ToplevelHandleStateInner::empty());
                    error!(
                        ?foreign_toplevel,
                        "Toplevel for foreign-toplevel-list not registered for cosmic-toplevel-info."
                    );
                }
            }
            zcosmic_toplevel_info_v1::Request::Stop => {
                state
                    .toplevel_info_state_mut()
                    .instances
                    .retain(|i| i != obj);
                obj.finished();
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
        if let zcosmic_toplevel_handle_v1::Request::Destroy = request {}
    }

    fn destroyed(
        state: &mut D,
        _client: ClientId,
        resource: &ZcosmicToplevelHandleV1,
        _data: &ToplevelHandleState<W>,
    ) {
        for toplevel in &state.toplevel_info_state_mut().toplevels {
            if let Some(state) = toplevel.user_data().get::<ToplevelState>() {
                state
                    .lock()
                    .unwrap()
                    .instances
                    .retain(|(_, i)| i != resource);
            }
        }
    }
}

pub fn toplevel_enter_output(toplevel: &impl Window, output: &Output) {
    if let Some(state) = toplevel.user_data().get::<ToplevelState>() {
        state.lock().unwrap().outputs.push(output.clone());
    }
}

pub fn toplevel_leave_output(toplevel: &impl Window, output: &Output) {
    if let Some(state) = toplevel.user_data().get::<ToplevelState>() {
        state.lock().unwrap().outputs.retain(|o| o != output);
    }
}

pub fn toplevel_enter_workspace(toplevel: &impl Window, workspace: &WorkspaceHandle) {
    if let Some(state) = toplevel.user_data().get::<ToplevelState>() {
        state.lock().unwrap().workspaces.push(*workspace);
    }
}

pub fn toplevel_leave_workspace(toplevel: &impl Window, workspace: &WorkspaceHandle) {
    if let Some(state) = toplevel.user_data().get::<ToplevelState>() {
        state.lock().unwrap().workspaces.retain(|w| w != workspace);
    }
}

impl<D, W> ToplevelInfoState<D, W>
where
    D: GlobalDispatch<ZcosmicToplevelInfoV1, ToplevelInfoGlobalData>
        + Dispatch<ZcosmicToplevelInfoV1, ()>
        + Dispatch<ZcosmicToplevelHandleV1, ToplevelHandleState<W>>
        + ForeignToplevelListHandler
        + ToplevelInfoHandler<Window = W>
        + 'static,
    W: Window + 'static,
{
    pub fn new<F>(dh: &DisplayHandle, client_filter: F) -> ToplevelInfoState<D, W>
    where
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + Clone + 'static,
    {
        let global = dh.create_global::<D, ZcosmicToplevelInfoV1, _>(
            3,
            ToplevelInfoGlobalData {
                filter: Box::new(client_filter.clone()),
            },
        );
        let foreign_toplevel_list =
            ForeignToplevelListState::new_with_filter::<D>(dh, client_filter);
        ToplevelInfoState {
            dh: dh.clone(),
            toplevels: Vec::new(),
            instances: Vec::new(),
            dirty: false,
            last_dirty: false,
            foreign_toplevel_list,
            global,
            _dispatch_data: std::marker::PhantomData,
        }
    }

    pub fn new_toplevel(&mut self, toplevel: &W, workspace_state: &WorkspaceState<D>) {
        let toplevel_handle = self
            .foreign_toplevel_list
            .new_toplevel::<D>(toplevel.title(), toplevel.app_id());

        if let Some(toplevel_state) = toplevel.user_data().get::<ToplevelState>() {
            let mut toplevel_state = toplevel_state.lock().unwrap();
            toplevel_state.foreign_handle = Some(toplevel_handle);
        } else {
            toplevel
                .user_data()
                .insert_if_missing(move || ToplevelStateInner::from_foreign(toplevel_handle));
        }

        for instance in &self.instances {
            send_toplevel_to_client::<D, W>(&self.dh, workspace_state, instance, toplevel);
        }
        self.toplevels.push(toplevel.clone());
        self.dirty = true;
    }

    pub fn remove_toplevel(&mut self, toplevel: &W) {
        if let Some(state) = toplevel.user_data().get::<ToplevelState>() {
            let mut state_inner = state.lock().unwrap();
            for (_info, handle) in &state_inner.instances {
                // don't send events to stopped instances
                if handle.version() < zcosmic_toplevel_info_v1::REQ_GET_COSMIC_TOPLEVEL_SINCE
                    && self
                        .instances
                        .iter()
                        .any(|i| i.id().same_client_as(&handle.id()))
                {
                    handle.closed();
                }
            }
            if let Some(handle) = state_inner.foreign_handle.take() {
                self.foreign_toplevel_list.remove_toplevel(&handle);
            }
            *state_inner = Default::default();
            self.dirty = true;
        }
        self.toplevels.retain(|w| w != toplevel);
    }

    pub fn refresh(&mut self, workspace_state: &WorkspaceState<D>) {
        let mut dirty = std::mem::replace(&mut self.dirty, false);

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
                    let changed = send_toplevel_to_client::<D, W>(
                        &self.dh,
                        workspace_state,
                        instance,
                        window,
                    );
                    dirty = dirty || changed;
                }
                true
            } else {
                for (_info, handle) in &state.instances {
                    // don't send events to stopped instances
                    if handle.version() < zcosmic_toplevel_info_v1::REQ_GET_COSMIC_TOPLEVEL_SINCE
                        && self
                            .instances
                            .iter()
                            .any(|i| i.id().same_client_as(&handle.id()))
                    {
                        handle.closed();
                    }
                }
                dirty = true;
                false
            }
        });

        if !dirty && self.last_dirty {
            for instance in &self.instances {
                if instance.version() >= zcosmic_toplevel_info_v1::EVT_DONE_SINCE {
                    instance.done();
                }
            }
        }

        self.last_dirty = dirty;
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }

    pub fn registered_toplevels(&self) -> impl Iterator<Item = &W> {
        self.toplevels.iter()
    }
}

fn send_toplevel_to_client<D, W>(
    dh: &DisplayHandle,
    workspace_state: &WorkspaceState<D>,
    info: &ZcosmicToplevelInfoV1,
    window: &W,
) -> bool
where
    D: GlobalDispatch<ZcosmicToplevelInfoV1, ToplevelInfoGlobalData>
        + Dispatch<ZcosmicToplevelInfoV1, ()>
        + Dispatch<ZcosmicToplevelHandleV1, ToplevelHandleState<W>>
        + ToplevelInfoHandler<Window = W>
        + 'static,
    W: Window + 'static,
{
    let mut state = window
        .user_data()
        .get::<ToplevelState>()
        .unwrap()
        .lock()
        .unwrap();
    let (_info, instance) = match state.instances.iter().find(|(i, _)| i == info) {
        Some(i) => i,
        None => {
            if info.version() < zcosmic_toplevel_info_v1::REQ_GET_COSMIC_TOPLEVEL_SINCE {
                if let Ok(client) = dh.get_client(info.id()) {
                    if let Ok(toplevel_handle) = client
                        .create_resource::<ZcosmicToplevelHandleV1, _, D>(
                            dh,
                            info.version(),
                            ToplevelHandleStateInner::from_window(window),
                        )
                    {
                        info.toplevel(&toplevel_handle);
                        state.instances.push((info.downgrade(), toplevel_handle));
                        state.instances.last().unwrap()
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }
    };

    let mut handle_state = instance
        .data::<ToplevelHandleState<W>>()
        .unwrap()
        .lock()
        .unwrap();
    let foreign_toplevel_handle = state.foreign_handle.as_ref();
    let mut changed = false;

    if handle_state.title != window.title() {
        handle_state.title = window.title();
        if instance.version() < zcosmic_toplevel_info_v1::REQ_GET_COSMIC_TOPLEVEL_SINCE {
            instance.title(handle_state.title.clone());
        }
        if let Some(handle) = foreign_toplevel_handle {
            handle.send_title(&handle_state.title);
        }
        changed = true;
    }
    if handle_state.app_id != window.app_id() {
        handle_state.app_id = window.app_id();
        if instance.version() < zcosmic_toplevel_info_v1::REQ_GET_COSMIC_TOPLEVEL_SINCE {
            instance.app_id(handle_state.app_id.clone());
        }
        if let Some(handle) = foreign_toplevel_handle {
            handle.send_app_id(&handle_state.app_id);
        }
        changed = true;
    }

    if handle_state.states.as_ref().is_none_or(|states| {
        (states.contains(&States::Maximized) != window.is_maximized())
            || (states.contains(&States::Fullscreen) != window.is_fullscreen())
            || (states.contains(&States::Activated) != window.is_activated())
            || (states.contains(&States::Minimized) != window.is_minimized())
    }) {
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
        if instance.version() >= zcosmic_toplevel_info_v1::REQ_GET_COSMIC_TOPLEVEL_SINCE
            && window.is_sticky()
        {
            states.push(States::Sticky);
        }
        handle_state.states = Some(states.clone());

        let states = states
            .iter()
            .flat_map(|state| (*state as u32).to_ne_bytes())
            .collect::<Vec<u8>>();
        instance.state(states);
        changed = true;
    }

    let mut geometry_changed = false;
    if !window.is_resizing() {
        let geometry = window.global_geometry();
        if handle_state.geometry != geometry {
            handle_state.geometry = geometry;
            changed = true;
            geometry_changed = true;
        }
    }

    if let Ok(client) = dh.get_client(instance.id()) {
        handle_state.outputs = state.outputs.clone();

        let handle_state = &mut *handle_state;
        for output in &handle_state.outputs {
            let geometry = handle_state
                .geometry
                .filter(|_| instance.version() >= zcosmic_toplevel_handle_v1::EVT_GEOMETRY_SINCE)
                .filter(|geo| output.geometry().intersection(*geo).is_some())
                .map(|geo| geo.to_local(output));
            for wl_output in output.client_outputs(&client) {
                if handle_state.wl_outputs.insert(wl_output.clone()) {
                    instance.output_enter(&wl_output);
                    if let Some(geo) = geometry {
                        instance.geometry(&wl_output, geo.loc.x, geo.loc.y, geo.size.w, geo.size.h);
                    }
                    changed = true;
                } else if geometry_changed {
                    if let Some(geo) = geometry {
                        instance.geometry(&wl_output, geo.loc.x, geo.loc.y, geo.size.w, geo.size.h);
                    }
                }
            }
        }
        handle_state.wl_outputs.retain(|wl_output| {
            let retain = wl_output.is_alive()
                && handle_state
                    .outputs
                    .iter()
                    .any(|output| output.owns(wl_output));
            if !retain {
                instance.output_leave(wl_output);
                changed = true;
            }
            retain
        });
    }

    for new_workspace in state
        .workspaces
        .iter()
        .filter(|w| !handle_state.workspaces.contains(w))
    {
        for handle in workspace_state.raw_ext_workspace_handles(new_workspace, &instance.id()) {
            instance.ext_workspace_enter(handle);
            changed = true;
        }
    }
    for old_workspace in handle_state
        .workspaces
        .iter()
        .filter(|w| !state.workspaces.contains(w))
    {
        for handle in workspace_state.raw_ext_workspace_handles(old_workspace, &instance.id()) {
            instance.ext_workspace_leave(handle);
            changed = true;
        }
    }
    handle_state.workspaces = state.workspaces.clone();

    if changed {
        if instance.version() < zcosmic_toplevel_info_v1::REQ_GET_COSMIC_TOPLEVEL_SINCE {
            instance.done();
        }
        if let Some(handle) = foreign_toplevel_handle {
            handle.send_done();
        }
    }

    changed
}

pub fn window_from_handle<W: Window + 'static>(handle: ZcosmicToplevelHandleV1) -> Option<W> {
    handle
        .data::<ToplevelHandleState<W>>()
        .and_then(|state| state.lock().unwrap().window.clone())
}

pub fn window_from_ext_handle<'a, W: Window + 'static, D>(
    state: &'a D,
    foreign_toplevel: &ExtForeignToplevelHandleV1,
) -> Option<&'a W>
where
    D: ToplevelInfoHandler<Window = W>,
{
    let handle = ForeignToplevelHandle::from_resource(foreign_toplevel)?;
    state.toplevel_info_state().toplevels.iter().find(|w| {
        w.user_data().get::<ToplevelState>().and_then(|inner| {
            inner
                .lock()
                .unwrap()
                .foreign_handle
                .as_ref()
                .map(|handle| handle.identifier())
        }) == Some(handle.identifier())
    })
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
