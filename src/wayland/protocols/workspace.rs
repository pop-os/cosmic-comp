// SPDX-License-Identifier: GPL-3.0-only

use std::{collections::HashSet, sync::Mutex};

use smithay::{
    output::Output,
    reexports::wayland_server::{
        backend::{ClientData, ClientId, GlobalId, ObjectId},
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource,
    },
};
use wayland_backend::protocol::WEnum;

use cosmic_protocols::workspace::v1::server::{
    zcosmic_workspace_group_handle_v1::{self, ZcosmicWorkspaceGroupHandleV1},
    zcosmic_workspace_handle_v1::{self, ZcosmicWorkspaceHandleV1},
    zcosmic_workspace_manager_v1::{self, ZcosmicWorkspaceManagerV1},
};

pub use cosmic_protocols::workspace::v1::server::{
    zcosmic_workspace_group_handle_v1::ZcosmicWorkspaceGroupCapabilitiesV1 as GroupCapabilities,
    zcosmic_workspace_handle_v1::ZcosmicWorkspaceCapabilitiesV1 as WorkspaceCapabilities,
};

#[derive(Debug)]
pub struct WorkspaceState<D>
where
    D: GlobalDispatch<ZcosmicWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV1, ()>
        + Dispatch<ZcosmicWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ZcosmicWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    dh: DisplayHandle,
    global: GlobalId,
    instances: Vec<ZcosmicWorkspaceManagerV1>,
    groups: Vec<WorkspaceGroup>,
    _marker: std::marker::PhantomData<D>,
}
pub struct WorkspaceUpdateGuard<'a, D>(&'a mut WorkspaceState<D>)
where
    D: GlobalDispatch<ZcosmicWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV1, ()>
        + Dispatch<ZcosmicWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ZcosmicWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static;

crate::utils::id_gen!(next_group_id, GROUP_ID, GROUP_IDS);
crate::utils::id_gen!(next_workspace_id, WORKSPACE_ID, WORKSPACE_IDS);

#[derive(Debug, Default)]
pub struct WorkspaceGroup {
    id: usize,
    instances: Vec<ZcosmicWorkspaceGroupHandleV1>,
    workspaces: Vec<Workspace>,

    outputs: Vec<Output>,
    capabilities: Vec<GroupCapabilities>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WorkspaceGroupHandle {
    id: usize,
}

#[derive(Default)]
pub struct WorkspaceGroupDataInner {
    outputs: Vec<Output>,
    capabilities: Vec<GroupCapabilities>,
    workspace_count: usize,
}
pub type WorkspaceGroupData = Mutex<WorkspaceGroupDataInner>;

#[derive(Debug)]
pub struct Workspace {
    id: usize,
    instances: Vec<ZcosmicWorkspaceHandleV1>,

    name: String,
    capabilities: Vec<WorkspaceCapabilities>,
    coordinates: Vec<u32>,
    states: HashSet<zcosmic_workspace_handle_v1::State>,
    tiling: zcosmic_workspace_handle_v1::TilingState,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WorkspaceHandle {
    id: usize,
}

#[derive(Default)]
pub struct WorkspaceDataInner {
    name: String,
    capabilities: Vec<WorkspaceCapabilities>,
    coordinates: Vec<u32>,
    states: HashSet<zcosmic_workspace_handle_v1::State>,
    tiling: Option<zcosmic_workspace_handle_v1::TilingState>,
}
pub type WorkspaceData = Mutex<WorkspaceDataInner>;

pub trait WorkspaceHandler
where
    Self: GlobalDispatch<ZcosmicWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV1, ()>
        + Dispatch<ZcosmicWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ZcosmicWorkspaceHandleV1, WorkspaceData>
        + Sized
        + 'static,
{
    type Client: ClientData + WorkspaceClientHandler + 'static;

    fn workspace_state(&self) -> &WorkspaceState<Self>;
    fn workspace_state_mut(&mut self) -> &mut WorkspaceState<Self>;
    fn commit_requests(&mut self, dh: &DisplayHandle, requests: Vec<Request>);
}

pub struct WorkspaceGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

#[derive(Debug)]
pub enum Request {
    Activate(WorkspaceHandle),
    Deactivate(WorkspaceHandle),
    Remove(WorkspaceHandle),
    Rename {
        workspace: WorkspaceHandle,
        name: String,
    },
    SetTilingState {
        workspace: WorkspaceHandle,
        state: WEnum<zcosmic_workspace_handle_v1::TilingState>,
    },
    Create {
        in_group: WorkspaceGroupHandle,
        name: String,
    },
}

#[derive(Debug, Default)]
pub struct WorkspaceClientStateInner {
    requests: Vec<Request>,
}
pub type WorkspaceClientState = Mutex<WorkspaceClientStateInner>;

pub trait WorkspaceClientHandler {
    fn workspace_state(&self) -> &WorkspaceClientState;
}

impl<D> GlobalDispatch<ZcosmicWorkspaceManagerV1, WorkspaceGlobalData, D> for WorkspaceState<D>
where
    D: GlobalDispatch<ZcosmicWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV1, ()>
        + Dispatch<ZcosmicWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ZcosmicWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    fn bind(
        state: &mut D,
        dh: &DisplayHandle,
        _client: &Client,
        resource: New<ZcosmicWorkspaceManagerV1>,
        _global_data: &WorkspaceGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        let state = state.workspace_state_mut();
        let instance = data_init.init(resource, ());
        for group in &mut state.groups {
            send_group_to_client::<D>(dh, &instance, group);
        }
        instance.done();
        state.instances.push(instance);
    }

    fn can_view(client: Client, global_data: &WorkspaceGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ZcosmicWorkspaceManagerV1, (), D> for WorkspaceState<D>
where
    D: GlobalDispatch<ZcosmicWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV1, ()>
        + Dispatch<ZcosmicWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ZcosmicWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    fn request(
        state: &mut D,
        client: &Client,
        obj: &ZcosmicWorkspaceManagerV1,
        request: zcosmic_workspace_manager_v1::Request,
        _data: &(),
        dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_workspace_manager_v1::Request::Commit => {
                if state.workspace_state().instances.contains(obj) {
                    let mut client_state = client
                        .get_data::<<D as WorkspaceHandler>::Client>()
                        .unwrap()
                        .workspace_state()
                        .lock()
                        .unwrap();
                    state.commit_requests(dh, std::mem::take(&mut client_state.requests));
                }
            }
            zcosmic_workspace_manager_v1::Request::Stop => {
                state.workspace_state_mut().instances.retain(|i| i != obj);
                // without an instance, the whole send_group_to_client machinery doesn't work
                // so there is no way for the whole clients hierachy to get any new events
            }
            _ => {}
        }
    }

    fn destroyed(
        state: &mut D,
        _client: ClientId,
        resource: &ZcosmicWorkspaceManagerV1,
        _data: &(),
    ) {
        state
            .workspace_state_mut()
            .instances
            .retain(|i| i != resource);
    }
}

impl<D> Dispatch<ZcosmicWorkspaceGroupHandleV1, WorkspaceGroupData, D> for WorkspaceState<D>
where
    D: GlobalDispatch<ZcosmicWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV1, ()>
        + Dispatch<ZcosmicWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ZcosmicWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    fn request(
        state: &mut D,
        client: &Client,
        obj: &ZcosmicWorkspaceGroupHandleV1,
        request: zcosmic_workspace_group_handle_v1::Request,
        _data: &WorkspaceGroupData,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_workspace_group_handle_v1::Request::CreateWorkspace { workspace } => {
                if let Some(id) = state
                    .workspace_state()
                    .groups
                    .iter()
                    .find(|g| g.instances.contains(obj))
                    .map(|g| g.id)
                {
                    let mut state = client
                        .get_data::<<D as WorkspaceHandler>::Client>()
                        .unwrap()
                        .workspace_state()
                        .lock()
                        .unwrap();
                    state.requests.push(Request::Create {
                        in_group: WorkspaceGroupHandle { id },
                        name: workspace,
                    });
                }
            }
            zcosmic_workspace_group_handle_v1::Request::Destroy => {
                for group in &mut state.workspace_state_mut().groups {
                    group.instances.retain(|i| i != obj)
                }
            }
            _ => {}
        }
    }

    fn destroyed(
        state: &mut D,
        _client: ClientId,
        resource: &ZcosmicWorkspaceGroupHandleV1,
        _data: &WorkspaceGroupData,
    ) {
        for group in &mut state.workspace_state_mut().groups {
            group.instances.retain(|i| i != resource)
        }
    }
}

impl<D> Dispatch<ZcosmicWorkspaceHandleV1, WorkspaceData, D> for WorkspaceState<D>
where
    D: GlobalDispatch<ZcosmicWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV1, ()>
        + Dispatch<ZcosmicWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ZcosmicWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    fn request(
        state: &mut D,
        client: &Client,
        obj: &ZcosmicWorkspaceHandleV1,
        request: zcosmic_workspace_handle_v1::Request,
        _data: &WorkspaceData,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_workspace_handle_v1::Request::Activate => {
                if let Some(workspace_handle) = state.workspace_state().get_workspace_handle(obj) {
                    let mut state = client
                        .get_data::<<D as WorkspaceHandler>::Client>()
                        .unwrap()
                        .workspace_state()
                        .lock()
                        .unwrap();
                    state.requests.push(Request::Activate(workspace_handle));
                }
            }
            zcosmic_workspace_handle_v1::Request::Deactivate => {
                if let Some(workspace_handle) = state.workspace_state().get_workspace_handle(obj) {
                    let mut state = client
                        .get_data::<<D as WorkspaceHandler>::Client>()
                        .unwrap()
                        .workspace_state()
                        .lock()
                        .unwrap();
                    state.requests.push(Request::Deactivate(workspace_handle));
                }
            }
            zcosmic_workspace_handle_v1::Request::Remove => {
                if let Some(workspace_handle) = state.workspace_state().get_workspace_handle(obj) {
                    let mut state = client
                        .get_data::<<D as WorkspaceHandler>::Client>()
                        .unwrap()
                        .workspace_state()
                        .lock()
                        .unwrap();
                    state.requests.push(Request::Remove(workspace_handle));
                }
            }
            zcosmic_workspace_handle_v1::Request::Rename { name } => {
                if let Some(workspace_handle) = state.workspace_state().get_workspace_handle(obj) {
                    let mut state = client
                        .get_data::<<D as WorkspaceHandler>::Client>()
                        .unwrap()
                        .workspace_state()
                        .lock()
                        .unwrap();
                    state.requests.push(Request::Rename {
                        workspace: workspace_handle,
                        name,
                    });
                }
            }
            zcosmic_workspace_handle_v1::Request::SetTilingState {
                state: tiling_state,
            } => {
                if let Some(workspace_handle) = state.workspace_state().get_workspace_handle(obj) {
                    let mut state = client
                        .get_data::<<D as WorkspaceHandler>::Client>()
                        .unwrap()
                        .workspace_state()
                        .lock()
                        .unwrap();
                    state.requests.push(Request::SetTilingState {
                        workspace: workspace_handle,
                        state: tiling_state,
                    });
                }
            }
            zcosmic_workspace_handle_v1::Request::Destroy => {
                for group in &mut state.workspace_state_mut().groups {
                    for workspace in &mut group.workspaces {
                        workspace.instances.retain(|i| i != obj)
                    }
                }
            }
            _ => {}
        }
    }

    fn destroyed(
        state: &mut D,
        _client: ClientId,
        resource: &ZcosmicWorkspaceHandleV1,
        _data: &WorkspaceData,
    ) {
        for group in &mut state.workspace_state_mut().groups {
            for workspace in &mut group.workspaces {
                workspace.instances.retain(|i| i != resource)
            }
        }
    }
}

impl<D> WorkspaceState<D>
where
    D: GlobalDispatch<ZcosmicWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV1, ()>
        + Dispatch<ZcosmicWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ZcosmicWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    pub fn new<F>(dh: &DisplayHandle, client_filter: F) -> WorkspaceState<D>
    where
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        let global = dh.create_global::<D, ZcosmicWorkspaceManagerV1, _>(
            1,
            WorkspaceGlobalData {
                filter: Box::new(client_filter),
            },
        );

        WorkspaceState {
            dh: dh.clone(),
            global,
            instances: Vec::new(),
            groups: Vec::new(),
            _marker: std::marker::PhantomData,
        }
    }

    pub fn workspace_belongs_to_group(
        &self,
        group: &WorkspaceGroupHandle,
        workspace: &WorkspaceHandle,
    ) -> bool {
        if let Some(group) = self.groups.iter().find(|g| g.id == group.id) {
            group.workspaces.iter().any(|w| w.id == workspace.id)
        } else {
            false
        }
    }

    pub fn group_capabilities(
        &self,
        group: &WorkspaceGroupHandle,
    ) -> Option<impl Iterator<Item = &GroupCapabilities>> {
        self.groups
            .iter()
            .find(|g| g.id == group.id)
            .map(|g| g.capabilities.iter())
    }

    pub fn group_outputs(
        &self,
        group: &WorkspaceGroupHandle,
    ) -> Option<impl Iterator<Item = &Output>> {
        self.groups
            .iter()
            .find(|g| g.id == group.id)
            .map(|g| g.outputs.iter())
    }

    pub fn workspace_capabilities(
        &self,
        workspace: &WorkspaceHandle,
    ) -> Option<impl Iterator<Item = &WorkspaceCapabilities>> {
        self.groups.iter().find_map(|g| {
            g.workspaces
                .iter()
                .find(|w| w.id == workspace.id)
                .map(|w| w.capabilities.iter())
        })
    }

    pub fn workspace_name(&self, workspace: &WorkspaceHandle) -> Option<&str> {
        self.groups.iter().find_map(|g| {
            g.workspaces
                .iter()
                .find(|w| w.id == workspace.id)
                .map(|w| &*w.name)
        })
    }

    pub fn workspace_coordinates(&self, workspace: &WorkspaceHandle) -> Option<&[u32]> {
        self.groups.iter().find_map(|g| {
            g.workspaces
                .iter()
                .find(|w| w.id == workspace.id)
                .map(|w| &*w.coordinates)
        })
    }

    pub fn workspace_states(
        &self,
        workspace: &WorkspaceHandle,
    ) -> Option<impl Iterator<Item = &zcosmic_workspace_handle_v1::State>> {
        self.groups.iter().find_map(|g| {
            g.workspaces
                .iter()
                .find(|w| w.id == workspace.id)
                .map(|w| w.states.iter())
        })
    }

    pub fn workspace_tiling_state(
        &self,
        workspace: &WorkspaceHandle,
    ) -> Option<zcosmic_workspace_handle_v1::TilingState> {
        self.groups.iter().find_map(|g| {
            g.workspaces
                .iter()
                .find(|w| w.id == workspace.id)
                .map(|w| w.tiling)
        })
    }

    pub fn group_handle(
        &self,
        group: &ZcosmicWorkspaceGroupHandleV1,
    ) -> Option<WorkspaceGroupHandle> {
        self.groups
            .iter()
            .find(|g| g.instances.contains(group))
            .map(|g| WorkspaceGroupHandle { id: g.id })
    }
    pub fn workspace_handle(
        &self,
        workspace: &ZcosmicWorkspaceHandleV1,
    ) -> Option<WorkspaceHandle> {
        self.groups
            .iter()
            .find_map(|g| {
                g.workspaces
                    .iter()
                    .find(|w| w.instances.contains(workspace))
            })
            .map(|w| WorkspaceHandle { id: w.id })
    }

    pub fn raw_group_handle(
        &self,
        group: &WorkspaceGroupHandle,
        client: &ObjectId,
    ) -> Option<ZcosmicWorkspaceGroupHandleV1> {
        self.groups
            .iter()
            .find(|g| g.id == group.id)
            .and_then(|g| g.instances.iter().find(|i| i.id().same_client_as(client)))
            .cloned()
    }
    pub fn raw_workspace_handle(
        &self,
        workspace: &WorkspaceHandle,
        client: &ObjectId,
    ) -> Option<ZcosmicWorkspaceHandleV1> {
        self.groups
            .iter()
            .find_map(|g| g.workspaces.iter().find(|w| w.id == workspace.id))
            .and_then(|w| w.instances.iter().find(|i| i.id().same_client_as(client)))
            .cloned()
    }

    pub fn update<'a>(&'a mut self) -> WorkspaceUpdateGuard<'a, D> {
        WorkspaceUpdateGuard(self)
    }

    fn done(&mut self) {
        let mut changed = false;
        for instance in &self.instances {
            for mut group in &mut self.groups {
                if send_group_to_client::<D>(&self.dh, instance, &mut group) {
                    changed = true;
                }
            }
        }
        if changed {
            for instance in &self.instances {
                instance.done();
            }
        }
    }

    pub fn get_workspace_handle(
        &self,
        handle: &ZcosmicWorkspaceHandleV1,
    ) -> Option<WorkspaceHandle> {
        self.groups
            .iter()
            .find_map(|g| g.workspaces.iter().find(|w| w.instances.contains(handle)))
            .map(|w| WorkspaceHandle { id: w.id })
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

impl<'a, D> WorkspaceUpdateGuard<'a, D>
where
    D: GlobalDispatch<ZcosmicWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV1, ()>
        + Dispatch<ZcosmicWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ZcosmicWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    pub fn create_workspace_group(&mut self) -> WorkspaceGroupHandle {
        let id = next_group_id();
        let group = WorkspaceGroup {
            id,
            ..Default::default()
        };
        self.0.groups.push(group);
        WorkspaceGroupHandle { id }
    }

    pub fn create_workspace(
        &mut self,
        group: &WorkspaceGroupHandle,
        tiling: zcosmic_workspace_handle_v1::TilingState,
    ) -> Option<WorkspaceHandle> {
        if let Some(group) = self.0.groups.iter_mut().find(|g| g.id == group.id) {
            let id = next_workspace_id();
            let workspace = Workspace {
                id,
                tiling,
                instances: Default::default(),
                name: Default::default(),
                capabilities: Default::default(),
                coordinates: Default::default(),
                states: Default::default(),
            };
            group.workspaces.push(workspace);
            Some(WorkspaceHandle { id })
        } else {
            None
        }
    }

    pub fn remove_workspace_group(&mut self, group: WorkspaceGroupHandle) {
        // "The compositor must remove all workspaces belonging to a workspace group before removing the workspace group."
        for workspace in self
            .0
            .groups
            .iter()
            .filter(|g| g.id == group.id)
            .flat_map(|g| g.workspaces.iter().map(|w| WorkspaceHandle { id: w.id }))
            .collect::<Vec<_>>()
            .into_iter()
        {
            self.remove_workspace(workspace);
        }

        if let Some(group) = self.0.groups.iter().find(|g| g.id == group.id) {
            for instance in &group.instances {
                instance.remove()
            }
        }
        self.0.groups.retain(|g| g.id != group.id);
        GROUP_IDS.lock().unwrap().remove(&group.id);
    }

    pub fn remove_workspace(&mut self, workspace: WorkspaceHandle) {
        for group in &mut self.0.groups {
            if let Some(workspace) = group.workspaces.iter().find(|w| w.id == workspace.id) {
                for instance in &workspace.instances {
                    instance.remove();
                }
            }
            group.workspaces.retain(|w| w.id != workspace.id);
        }
        WORKSPACE_IDS.lock().unwrap().remove(&workspace.id);
    }

    pub fn workspace_belongs_to_group(
        &self,
        group: &WorkspaceGroupHandle,
        workspace: &WorkspaceHandle,
    ) -> bool {
        self.0.workspace_belongs_to_group(group, workspace)
    }

    pub fn group_capabilities(
        &mut self,
        group: &WorkspaceGroupHandle,
    ) -> Option<impl Iterator<Item = &GroupCapabilities>> {
        self.0.group_capabilities(group)
    }

    pub fn set_group_capabilities(
        &mut self,
        group: &WorkspaceGroupHandle,
        capabilities: impl Iterator<Item = GroupCapabilities>,
    ) {
        if let Some(group) = self.0.groups.iter_mut().find(|g| g.id == group.id) {
            group.capabilities = capabilities.collect();
        }
    }

    pub fn group_outputs(
        &self,
        group: &WorkspaceGroupHandle,
    ) -> Option<impl Iterator<Item = &Output>> {
        self.0.group_outputs(group)
    }

    pub fn add_group_output(&mut self, group: &WorkspaceGroupHandle, output: &Output) {
        if let Some(group) = self.0.groups.iter_mut().find(|g| g.id == group.id) {
            group.outputs.push(output.clone())
        }
    }

    pub fn remove_group_output(&mut self, group: &WorkspaceGroupHandle, output: &Output) {
        if let Some(group) = self.0.groups.iter_mut().find(|g| g.id == group.id) {
            group.outputs.retain(|o| o != output)
        }
    }

    pub fn workspace_capabilities(
        &self,
        workspace: &WorkspaceHandle,
    ) -> Option<impl Iterator<Item = &WorkspaceCapabilities>> {
        self.0.workspace_capabilities(workspace)
    }

    pub fn set_workspace_capabilities(
        &mut self,
        workspace: &WorkspaceHandle,
        capabilities: impl Iterator<Item = WorkspaceCapabilities>,
    ) {
        if let Some(workspace) = self
            .0
            .groups
            .iter_mut()
            .find_map(|g| g.workspaces.iter_mut().find(|w| w.id == workspace.id))
        {
            workspace.capabilities = capabilities.collect();
        }
    }

    pub fn workspace_name(&self, workspace: &WorkspaceHandle) -> Option<&str> {
        self.0.workspace_name(workspace)
    }

    pub fn set_workspace_name(&mut self, workspace: &WorkspaceHandle, name: impl Into<String>) {
        if let Some(workspace) = self
            .0
            .groups
            .iter_mut()
            .find_map(|g| g.workspaces.iter_mut().find(|w| w.id == workspace.id))
        {
            workspace.name = name.into();
        }
    }

    pub fn workspace_coordinates(&self, workspace: &WorkspaceHandle) -> Option<&[u32]> {
        self.0.workspace_coordinates(workspace)
    }

    pub fn set_workspace_coordinates(
        &mut self,
        workspace: &WorkspaceHandle,
        coords: [Option<u32>; 3],
    ) {
        if let Some(workspace) = self
            .0
            .groups
            .iter_mut()
            .find_map(|g| g.workspaces.iter_mut().find(|w| w.id == workspace.id))
        {
            workspace.coordinates = coords
                .iter()
                .flat_map(std::convert::identity)
                .copied()
                .collect();
        }
    }

    pub fn workspace_states(
        &self,
        workspace: &WorkspaceHandle,
    ) -> Option<impl Iterator<Item = &zcosmic_workspace_handle_v1::State>> {
        self.0.workspace_states(workspace)
    }

    pub fn add_workspace_state(
        &mut self,
        workspace: &WorkspaceHandle,
        state: zcosmic_workspace_handle_v1::State,
    ) {
        if let Some(workspace) = self
            .0
            .groups
            .iter_mut()
            .find_map(|g| g.workspaces.iter_mut().find(|w| w.id == workspace.id))
        {
            workspace.states.insert(state);
        }
    }

    pub fn remove_workspace_state(
        &mut self,
        workspace: &WorkspaceHandle,
        state: zcosmic_workspace_handle_v1::State,
    ) {
        if let Some(workspace) = self
            .0
            .groups
            .iter_mut()
            .find_map(|g| g.workspaces.iter_mut().find(|w| w.id == workspace.id))
        {
            workspace.states.remove(&state);
        }
    }

    pub fn workspace_tiling_state(
        &self,
        workspace: &WorkspaceHandle,
    ) -> Option<zcosmic_workspace_handle_v1::TilingState> {
        self.0.workspace_tiling_state(workspace)
    }

    pub fn set_workspace_tiling_state(
        &mut self,
        workspace: &WorkspaceHandle,
        state: zcosmic_workspace_handle_v1::TilingState,
    ) {
        if let Some(workspace) = self
            .0
            .groups
            .iter_mut()
            .find_map(|g| g.workspaces.iter_mut().find(|w| w.id == workspace.id))
        {
            workspace.tiling = state;
        }
    }
}

impl<'a, D> Drop for WorkspaceUpdateGuard<'a, D>
where
    D: GlobalDispatch<ZcosmicWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV1, ()>
        + Dispatch<ZcosmicWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ZcosmicWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    fn drop(&mut self) {
        self.0.done();
    }
}

fn send_group_to_client<D>(
    dh: &DisplayHandle,
    mngr: &ZcosmicWorkspaceManagerV1,
    group: &mut WorkspaceGroup,
) -> bool
where
    D: GlobalDispatch<ZcosmicWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV1, ()>
        + Dispatch<ZcosmicWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ZcosmicWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    let instance = match group
        .instances
        .iter_mut()
        .find(|i| i.id().same_client_as(&mngr.id()))
    {
        Some(i) => i,
        None => {
            if let Ok(client) = dh.get_client(mngr.id()) {
                if let Ok(handle) = client.create_resource::<ZcosmicWorkspaceGroupHandleV1, _, D>(
                    dh,
                    mngr.version(),
                    WorkspaceGroupData::default(),
                ) {
                    mngr.workspace_group(&handle);
                    group.instances.push(handle);
                    group.instances.last_mut().unwrap()
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }
    };

    let mut handle_state = instance
        .data::<WorkspaceGroupData>()
        .unwrap()
        .lock()
        .unwrap();
    let mut changed = false;
    if let Ok(client) = dh.get_client(instance.id()) {
        for new_output in group
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
            .filter(|o| !group.outputs.contains(o))
        {
            for wl_output in old_output.client_outputs(&client) {
                instance.output_leave(&wl_output);
            }
            changed = true;
        }
        handle_state.outputs = group.outputs.clone();
    }

    if handle_state.capabilities != group.capabilities {
        let caps: Vec<u8> = {
            let mut caps = group.capabilities.clone();
            let ratio = std::mem::size_of::<GroupCapabilities>() / std::mem::size_of::<u8>();
            let ptr = caps.as_mut_ptr() as *mut u8;
            let len = caps.len() * ratio;
            let cap = caps.capacity() * ratio;
            std::mem::forget(caps);
            unsafe { Vec::from_raw_parts(ptr, len, cap) }
        };
        instance.capabilities(caps);
        handle_state.capabilities = group.capabilities.clone();
        changed = true;
    }

    if handle_state.workspace_count != group.workspaces.len() {
        changed = true;
    }
    handle_state.workspace_count = group.workspaces.len();

    for workspace in &mut group.workspaces {
        if send_workspace_to_client::<D>(dh, instance, workspace) {
            changed = true;
        }
    }

    changed
}

fn send_workspace_to_client<D>(
    dh: &DisplayHandle,
    group: &ZcosmicWorkspaceGroupHandleV1,
    workspace: &mut Workspace,
) -> bool
where
    D: GlobalDispatch<ZcosmicWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV1, ()>
        + Dispatch<ZcosmicWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ZcosmicWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    let instance = match workspace
        .instances
        .iter_mut()
        .find(|i| i.id().same_client_as(&group.id()))
    {
        Some(i) => i,
        None => {
            if let Ok(client) = dh.get_client(group.id()) {
                if let Ok(handle) = client.create_resource::<ZcosmicWorkspaceHandleV1, _, D>(
                    dh,
                    group.version(),
                    WorkspaceData::default(),
                ) {
                    group.workspace(&handle);
                    workspace.instances.push(handle);
                    workspace.instances.last_mut().unwrap()
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }
    };

    let mut handle_state = instance.data::<WorkspaceData>().unwrap().lock().unwrap();
    let mut changed = false;

    if handle_state.name != workspace.name {
        instance.name(workspace.name.clone());
        handle_state.name = workspace.name.clone();
        changed = true;
    }
    if handle_state.coordinates != workspace.coordinates {
        let coords: Vec<u8> = {
            let mut coords = workspace.coordinates.clone();
            let ratio = std::mem::size_of::<u32>() / std::mem::size_of::<u8>();
            let ptr = coords.as_mut_ptr() as *mut u8;
            let len = coords.len() * ratio;
            let cap = coords.capacity() * ratio;
            std::mem::forget(coords);
            unsafe { Vec::from_raw_parts(ptr, len, cap) }
        };
        instance.coordinates(coords);
        handle_state.coordinates = workspace.coordinates.clone();
        changed = true;
    }
    if handle_state.capabilities != workspace.capabilities {
        let caps: Vec<u8> = {
            let mut caps = workspace.capabilities.clone();
            let ratio = std::mem::size_of::<WorkspaceCapabilities>() / std::mem::size_of::<u8>();
            let ptr = caps.as_mut_ptr() as *mut u8;
            let len = caps.len() * ratio;
            let cap = caps.capacity() * ratio;
            std::mem::forget(caps);
            unsafe { Vec::from_raw_parts(ptr, len, cap) }
        };
        instance.capabilities(caps);
        handle_state.capabilities = workspace.capabilities.clone();
        changed = true;
    }
    if handle_state.states != workspace.states {
        let states: Vec<u8> = {
            let mut states = workspace.states.iter().cloned().collect::<Vec<_>>();
            let ratio = std::mem::size_of::<zcosmic_workspace_handle_v1::State>()
                / std::mem::size_of::<u8>();
            let ptr = states.as_mut_ptr() as *mut u8;
            let len = states.len() * ratio;
            let cap = states.capacity() * ratio;
            std::mem::forget(states);
            unsafe { Vec::from_raw_parts(ptr, len, cap) }
        };
        instance.state(states);
        handle_state.states = workspace.states.clone();
        changed = true;
    }
    if instance.version() >= zcosmic_workspace_handle_v1::EVT_TILING_STATE_SINCE {
        if handle_state
            .tiling
            .map(|state| state != workspace.tiling)
            .unwrap_or(true)
        {
            instance.tiling_state(workspace.tiling);
            handle_state.tiling = Some(workspace.tiling);
            changed = true;
        }
    }

    changed
}

macro_rules! delegate_workspace {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::workspace::v1::server::zcosmic_workspace_manager_v1::ZcosmicWorkspaceManagerV1: $crate::wayland::protocols::workspace::WorkspaceGlobalData
        ] => $crate::wayland::protocols::workspace::WorkspaceState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::workspace::v1::server::zcosmic_workspace_manager_v1::ZcosmicWorkspaceManagerV1: ()
        ] => $crate::wayland::protocols::workspace::WorkspaceState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::workspace::v1::server::zcosmic_workspace_group_handle_v1::ZcosmicWorkspaceGroupHandleV1: $crate::wayland::protocols::workspace::WorkspaceGroupData
        ] => $crate::wayland::protocols::workspace::WorkspaceState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::workspace::v1::server::zcosmic_workspace_handle_v1::ZcosmicWorkspaceHandleV1: $crate::wayland::protocols::workspace::WorkspaceData
        ] => $crate::wayland::protocols::workspace::WorkspaceState<Self>);
    };
}
pub(crate) use delegate_workspace;
