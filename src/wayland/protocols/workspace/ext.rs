// SPDX-License-Identifier: GPL-3.0-only

use cosmic_protocols::workspace::v2::server::zcosmic_workspace_handle_v2::ZcosmicWorkspaceHandleV2;

use smithay::{
    output::Output,
    reexports::{
        wayland_protocols::ext::workspace::v1::server::{
            ext_workspace_group_handle_v1::{self, ExtWorkspaceGroupHandleV1, GroupCapabilities},
            ext_workspace_handle_v1::{self, ExtWorkspaceHandleV1},
            ext_workspace_manager_v1::{self, ExtWorkspaceManagerV1},
        },
        wayland_server::{
            backend::ClientId, protocol::wl_output::WlOutput, Client, DataInit, Dispatch,
            DisplayHandle, GlobalDispatch, New, Resource, Weak,
        },
    },
};

use std::{collections::HashSet, sync::Mutex};

use super::{
    Request, State, Workspace, WorkspaceCapabilities, WorkspaceGlobalData, WorkspaceGroup,
    WorkspaceGroupHandle, WorkspaceHandler, WorkspaceState,
};

#[derive(Debug, Default)]
pub struct WorkspaceManagerDataInner {
    pub(super) requests: Vec<Request>,
}

pub type WorkspaceManagerData = Mutex<WorkspaceManagerDataInner>;

#[derive(Default)]
pub struct WorkspaceGroupDataInner {
    outputs: Vec<Output>,
    wl_outputs: HashSet<WlOutput>,
    capabilities: Option<GroupCapabilities>,
    workspace_count: usize,
}

pub struct WorkspaceGroupData {
    inner: Mutex<WorkspaceGroupDataInner>,
    pub(super) manager: Weak<ExtWorkspaceManagerV1>,
}

#[derive(Default)]
pub struct WorkspaceDataInner {
    pub(super) group: Option<ExtWorkspaceGroupHandleV1>,
    name: String,
    capabilities: Option<ext_workspace_handle_v1::WorkspaceCapabilities>,
    coordinates: Vec<u32>,
    states: Option<ext_workspace_handle_v1::State>,
    ext_id: Option<String>,
    pub(super) cosmic_v2_handle: Option<Weak<ZcosmicWorkspaceHandleV2>>,
}

pub struct WorkspaceData {
    pub(super) inner: Mutex<WorkspaceDataInner>,
    pub(super) manager: Weak<ExtWorkspaceManagerV1>,
}

impl<D> GlobalDispatch<ExtWorkspaceManagerV1, WorkspaceGlobalData, D> for WorkspaceState<D>
where
    D: WorkspaceHandler,
{
    fn bind(
        state: &mut D,
        dh: &DisplayHandle,
        _client: &Client,
        resource: New<ExtWorkspaceManagerV1>,
        _global_data: &WorkspaceGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        let state = state.workspace_state_mut();
        let instance = data_init.init(resource, WorkspaceManagerData::default());
        for group in &mut state.groups {
            send_group_to_client::<D>(dh, &instance, group);
        }
        instance.done();
        state.ext_instances.push(instance);
    }

    fn can_view(client: Client, global_data: &WorkspaceGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ExtWorkspaceManagerV1, WorkspaceManagerData, D> for WorkspaceState<D>
where
    D: WorkspaceHandler,
{
    fn request(
        state: &mut D,
        _client: &Client,
        obj: &ExtWorkspaceManagerV1,
        request: ext_workspace_manager_v1::Request,
        data: &WorkspaceManagerData,
        dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            ext_workspace_manager_v1::Request::Commit => {
                if state.workspace_state().ext_instances.contains(obj) {
                    let mut data = data.lock().unwrap();
                    state.commit_requests(dh, std::mem::take(&mut data.requests));
                }
            }
            ext_workspace_manager_v1::Request::Stop => {
                state
                    .workspace_state_mut()
                    .ext_instances
                    .retain(|i| i != obj);
                // without an instance, the whole send_group_to_client machinery doesn't work
                // so there is no way for the whole clients hierachy to get any new events
            }
            _ => {}
        }
    }

    fn destroyed(
        state: &mut D,
        _client: ClientId,
        resource: &ExtWorkspaceManagerV1,
        _data: &WorkspaceManagerData,
    ) {
        state
            .workspace_state_mut()
            .ext_instances
            .retain(|i| i != resource);
    }
}

impl<D> Dispatch<ExtWorkspaceGroupHandleV1, WorkspaceGroupData, D> for WorkspaceState<D>
where
    D: WorkspaceHandler,
{
    fn request(
        state: &mut D,
        _client: &Client,
        obj: &ExtWorkspaceGroupHandleV1,
        request: ext_workspace_group_handle_v1::Request,
        data: &WorkspaceGroupData,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            ext_workspace_group_handle_v1::Request::CreateWorkspace { workspace } => {
                if let Some(id) = state
                    .workspace_state()
                    .groups
                    .iter()
                    .find(|g| g.ext_instances.contains(obj))
                    .map(|g| g.id)
                {
                    if let Ok(manager) = data.manager.upgrade() {
                        let mut state = manager
                            .data::<WorkspaceManagerData>()
                            .unwrap()
                            .lock()
                            .unwrap();
                        state.requests.push(Request::Create {
                            in_group: WorkspaceGroupHandle { id },
                            name: workspace,
                        });
                    }
                }
            }
            ext_workspace_group_handle_v1::Request::Destroy => {
                for group in &mut state.workspace_state_mut().groups {
                    group.ext_instances.retain(|i| i != obj)
                }
            }
            _ => {}
        }
    }

    fn destroyed(
        state: &mut D,
        _client: ClientId,
        resource: &ExtWorkspaceGroupHandleV1,
        _data: &WorkspaceGroupData,
    ) {
        for group in &mut state.workspace_state_mut().groups {
            group.ext_instances.retain(|i| i != resource)
        }
    }
}

impl<D> Dispatch<ExtWorkspaceHandleV1, WorkspaceData, D> for WorkspaceState<D>
where
    D: WorkspaceHandler,
{
    fn request(
        state: &mut D,
        _client: &Client,
        obj: &ExtWorkspaceHandleV1,
        request: ext_workspace_handle_v1::Request,
        data: &WorkspaceData,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            ext_workspace_handle_v1::Request::Activate => {
                if let Some(workspace_handle) =
                    state.workspace_state().get_ext_workspace_handle(obj)
                {
                    if let Ok(manager) = data.manager.upgrade() {
                        let mut state = manager
                            .data::<WorkspaceManagerData>()
                            .unwrap()
                            .lock()
                            .unwrap();
                        state.requests.push(Request::Activate(workspace_handle));
                    }
                }
            }
            ext_workspace_handle_v1::Request::Deactivate => {
                if let Some(workspace_handle) =
                    state.workspace_state().get_ext_workspace_handle(obj)
                {
                    if let Ok(manager) = data.manager.upgrade() {
                        let mut state = manager
                            .data::<WorkspaceManagerData>()
                            .unwrap()
                            .lock()
                            .unwrap();
                        state.requests.push(Request::Deactivate(workspace_handle));
                    }
                }
            }
            ext_workspace_handle_v1::Request::Remove => {
                if let Some(workspace_handle) =
                    state.workspace_state().get_ext_workspace_handle(obj)
                {
                    if let Ok(manager) = data.manager.upgrade() {
                        let mut state = manager
                            .data::<WorkspaceManagerData>()
                            .unwrap()
                            .lock()
                            .unwrap();
                        state.requests.push(Request::Remove(workspace_handle));
                    }
                }
            }
            ext_workspace_handle_v1::Request::Assign { workspace_group } => {
                if let Some(workspace_handle) =
                    state.workspace_state().get_ext_workspace_handle(obj)
                {
                    if let Some(group_id) = state
                        .workspace_state()
                        .groups
                        .iter()
                        .find(|g| g.ext_instances.contains(&workspace_group))
                        .map(|g| g.id)
                    {
                        if let Ok(manager) = data.manager.upgrade() {
                            let mut state = manager
                                .data::<WorkspaceManagerData>()
                                .unwrap()
                                .lock()
                                .unwrap();
                            state.requests.push(Request::Assign {
                                workspace: workspace_handle,
                                group: WorkspaceGroupHandle { id: group_id },
                            });
                        }
                    }
                }
            }
            ext_workspace_handle_v1::Request::Destroy => {
                for group in &mut state.workspace_state_mut().groups {
                    for workspace in &mut group.workspaces {
                        workspace.ext_instances.retain(|i| i != obj)
                    }
                }
            }
            _ => {}
        }
    }

    fn destroyed(
        state: &mut D,
        _client: ClientId,
        resource: &ExtWorkspaceHandleV1,
        _data: &WorkspaceData,
    ) {
        for group in &mut state.workspace_state_mut().groups {
            for workspace in &mut group.workspaces {
                workspace.ext_instances.retain(|i| i != resource)
            }
        }
    }
}

pub(super) fn send_group_to_client<D>(
    dh: &DisplayHandle,
    mngr: &ExtWorkspaceManagerV1,
    group: &mut WorkspaceGroup,
) -> bool
where
    D: WorkspaceHandler,
{
    let instance = match group
        .ext_instances
        .iter_mut()
        .find(|i| i.data::<WorkspaceGroupData>().unwrap().manager == *mngr)
    {
        Some(i) => i,
        None => {
            if let Ok(client) = dh.get_client(mngr.id()) {
                if let Ok(handle) = client.create_resource::<ExtWorkspaceGroupHandleV1, _, D>(
                    dh,
                    mngr.version(),
                    WorkspaceGroupData {
                        inner: Mutex::new(WorkspaceGroupDataInner::default()),
                        manager: mngr.downgrade(),
                    },
                ) {
                    mngr.workspace_group(&handle);
                    group.ext_instances.push(handle);
                    group.ext_instances.last_mut().unwrap()
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
        .inner
        .lock()
        .unwrap();
    let mut changed = false;
    if let Ok(client) = dh.get_client(instance.id()) {
        for output in &group.outputs {
            for wl_output in output.client_outputs(&client) {
                if handle_state.wl_outputs.insert(wl_output.clone()) {
                    instance.output_enter(&wl_output);
                    changed = true;
                }
            }
        }

        handle_state.wl_outputs.retain(|wl_output| {
            let retain =
                wl_output.is_alive() && group.outputs.iter().any(|output| output.owns(wl_output));
            if !retain {
                instance.output_leave(&wl_output);
                changed = true;
            }
            retain
        });

        handle_state.outputs = group.outputs.clone();
    }

    if handle_state.capabilities != Some(group.capabilities) {
        instance.capabilities(group.capabilities);
        handle_state.capabilities = Some(group.capabilities.clone());
        changed = true;
    }

    if handle_state.workspace_count != group.workspaces.len() {
        changed = true;
    }
    handle_state.workspace_count = group.workspaces.len();

    for workspace in &mut group.workspaces {
        if send_workspace_to_client::<D>(dh, mngr, instance, workspace) {
            changed = true;
        }
    }

    changed
}

fn send_workspace_to_client<D>(
    dh: &DisplayHandle,
    mngr: &ExtWorkspaceManagerV1,
    group: &ExtWorkspaceGroupHandleV1,
    workspace: &mut Workspace,
) -> bool
where
    D: WorkspaceHandler,
{
    let instance = match workspace
        .ext_instances
        .iter_mut()
        .find(|i| i.data::<WorkspaceData>().unwrap().manager == *mngr)
    {
        Some(i) => i,
        None => {
            if let Ok(client) = dh.get_client(mngr.id()) {
                if let Ok(handle) = client.create_resource::<ExtWorkspaceHandleV1, _, D>(
                    dh,
                    mngr.version(),
                    WorkspaceData {
                        inner: Mutex::new(WorkspaceDataInner::default()),
                        manager: mngr.downgrade(),
                    },
                ) {
                    mngr.workspace(&handle);
                    workspace.ext_instances.push(handle);
                    workspace.ext_instances.last_mut().unwrap()
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }
    };
    let instance = instance.clone();

    let mut handle_state = instance
        .data::<WorkspaceData>()
        .unwrap()
        .inner
        .lock()
        .unwrap();
    let mut changed = false;

    if handle_state.group.as_ref() != Some(group) {
        if let Some(old_group) = &handle_state.group {
            old_group.workspace_leave(&instance);
        }
        group.workspace_enter(&instance);
        handle_state.group = Some(group.clone());
    }

    if handle_state.name != workspace.name {
        instance.name(workspace.name.clone());
        handle_state.name = workspace.name.clone();
        changed = true;
    }

    if handle_state.coordinates != workspace.coordinates {
        let coords = workspace
            .coordinates
            .iter()
            .flat_map(|coord| coord.to_ne_bytes())
            .collect::<Vec<u8>>();
        instance.coordinates(coords);
        handle_state.coordinates = workspace.coordinates.clone();
        changed = true;
    }

    let capabilities = workspace
        .capabilities
        .iter()
        .filter_map(|cap| match cap {
            WorkspaceCapabilities::Activate => {
                Some(ext_workspace_handle_v1::WorkspaceCapabilities::Activate)
            }
            WorkspaceCapabilities::Deactivate => {
                Some(ext_workspace_handle_v1::WorkspaceCapabilities::Deactivate)
            }
            WorkspaceCapabilities::Remove => {
                Some(ext_workspace_handle_v1::WorkspaceCapabilities::Remove)
            }
            WorkspaceCapabilities::Assign => {
                Some(ext_workspace_handle_v1::WorkspaceCapabilities::Assign)
            }
            _ => None,
        })
        .collect::<ext_workspace_handle_v1::WorkspaceCapabilities>();
    if handle_state.capabilities != Some(capabilities) {
        instance.capabilities(capabilities);
        handle_state.capabilities = Some(capabilities);
        changed = true;
    }

    let states = workspace
        .states
        .iter()
        .filter_map(|state| match state {
            State::Active => Some(ext_workspace_handle_v1::State::Active),
            State::Urgent => Some(ext_workspace_handle_v1::State::Urgent),
            State::Hidden => Some(ext_workspace_handle_v1::State::Hidden),
            _ => None,
        })
        .collect();
    if handle_state.states != Some(states) {
        instance.state(states);
        handle_state.states = Some(states);
        changed = true;
    }

    if handle_state.ext_id.is_none() {
        if let Some(id) = workspace.ext_id.clone() {
            instance.id(id);
        }
    }

    if let Some(cosmic_v2_handle) = handle_state
        .cosmic_v2_handle
        .as_ref()
        .and_then(|x| x.upgrade().ok())
    {
        changed |= super::cosmic_v2::send_workspace_to_client(&cosmic_v2_handle, workspace);
    }

    changed
}
