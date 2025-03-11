// SPDX-License-Identifier: GPL-3.0-only

use cosmic_protocols::workspace::v2::server::zcosmic_workspace_handle_v2::ZcosmicWorkspaceHandleV2;

use smithay::reexports::wayland_server::{
    backend::{ClientData, ClientId},
    Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
};

use std::sync::Mutex;

use super::{
    Request, Workspace, WorkspaceCapabilities, WorkspaceClientHandler, WorkspaceGlobalData,
    WorkspaceGroup, WorkspaceGroupData, WorkspaceGroupHandle, WorkspaceHandler, WorkspaceState,
};

use smithay::reexports::wayland_protocols::ext::workspace::v1::server::{
    ext_workspace_group_handle_v1::{self, ExtWorkspaceGroupHandleV1},
    ext_workspace_handle_v1::{self, ExtWorkspaceHandleV1},
    ext_workspace_manager_v1::{self, ExtWorkspaceManagerV1},
};

#[derive(Default)]
pub struct WorkspaceDataInner {
    name: String,
    capabilities: Option<ext_workspace_handle_v1::WorkspaceCapabilities>,
    coordinates: Vec<u32>,
    states: Option<ext_workspace_handle_v1::State>,
    pub(super) cosmic_v2_handle: Option<Weak<ZcosmicWorkspaceHandleV2>>,
}

pub type WorkspaceData = Mutex<WorkspaceDataInner>;

impl<D> GlobalDispatch<ExtWorkspaceManagerV1, WorkspaceGlobalData, D> for WorkspaceState<D>
where
    D: GlobalDispatch<ExtWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ExtWorkspaceManagerV1, ()>
        + Dispatch<ExtWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ExtWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
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
        let instance = data_init.init(resource, ());
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

impl<D> Dispatch<ExtWorkspaceManagerV1, (), D> for WorkspaceState<D>
where
    D: GlobalDispatch<ExtWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ExtWorkspaceManagerV1, ()>
        + Dispatch<ExtWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ExtWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    fn request(
        state: &mut D,
        client: &Client,
        obj: &ExtWorkspaceManagerV1,
        request: ext_workspace_manager_v1::Request,
        _data: &(),
        dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            ext_workspace_manager_v1::Request::Commit => {
                if state.workspace_state().ext_instances.contains(obj) {
                    let mut client_state = client
                        .get_data::<<D as WorkspaceHandler>::Client>()
                        .unwrap()
                        .workspace_state()
                        .lock()
                        .unwrap();
                    state.commit_requests(dh, std::mem::take(&mut client_state.requests));
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

    fn destroyed(state: &mut D, _client: ClientId, resource: &ExtWorkspaceManagerV1, _data: &()) {
        state
            .workspace_state_mut()
            .ext_instances
            .retain(|i| i != resource);
    }
}

impl<D> Dispatch<ExtWorkspaceGroupHandleV1, WorkspaceGroupData, D> for WorkspaceState<D>
where
    D: GlobalDispatch<ExtWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ExtWorkspaceManagerV1, ()>
        + Dispatch<ExtWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ExtWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    fn request(
        state: &mut D,
        client: &Client,
        obj: &ExtWorkspaceGroupHandleV1,
        request: ext_workspace_group_handle_v1::Request,
        _data: &WorkspaceGroupData,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            ext_workspace_group_handle_v1::Request::CreateWorkspace { workspace } => {
                if let Some(id) = state
                    .workspace_state()
                    .groups
                    .iter()
                    .find(|g| g.ext_instances.iter().any(|(_, i)| i == obj))
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
            ext_workspace_group_handle_v1::Request::Destroy => {
                for group in &mut state.workspace_state_mut().groups {
                    group.ext_instances.retain(|(_, i)| i != obj)
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
            group.ext_instances.retain(|(_, i)| i != resource)
        }
    }
}

impl<D> Dispatch<ExtWorkspaceHandleV1, WorkspaceData, D> for WorkspaceState<D>
where
    D: GlobalDispatch<ExtWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ExtWorkspaceManagerV1, ()>
        + Dispatch<ExtWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ExtWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    fn request(
        state: &mut D,
        client: &Client,
        obj: &ExtWorkspaceHandleV1,
        request: ext_workspace_handle_v1::Request,
        _data: &WorkspaceData,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            ext_workspace_handle_v1::Request::Activate => {
                if let Some(workspace_handle) =
                    state.workspace_state().get_ext_workspace_handle(obj)
                {
                    let mut state = client
                        .get_data::<<D as WorkspaceHandler>::Client>()
                        .unwrap()
                        .workspace_state()
                        .lock()
                        .unwrap();
                    state.requests.push(Request::Activate(workspace_handle));
                }
            }
            ext_workspace_handle_v1::Request::Deactivate => {
                if let Some(workspace_handle) =
                    state.workspace_state().get_ext_workspace_handle(obj)
                {
                    let mut state = client
                        .get_data::<<D as WorkspaceHandler>::Client>()
                        .unwrap()
                        .workspace_state()
                        .lock()
                        .unwrap();
                    state.requests.push(Request::Deactivate(workspace_handle));
                }
            }
            ext_workspace_handle_v1::Request::Remove => {
                if let Some(workspace_handle) =
                    state.workspace_state().get_ext_workspace_handle(obj)
                {
                    let mut state = client
                        .get_data::<<D as WorkspaceHandler>::Client>()
                        .unwrap()
                        .workspace_state()
                        .lock()
                        .unwrap();
                    state.requests.push(Request::Remove(workspace_handle));
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
                        .find(|g| g.ext_instances.iter().any(|(_, i)| *i == workspace_group))
                        .map(|g| g.id)
                    {
                        let mut state = client
                            .get_data::<<D as WorkspaceHandler>::Client>()
                            .unwrap()
                            .workspace_state()
                            .lock()
                            .unwrap();
                        state.requests.push(Request::Assign {
                            workspace: workspace_handle,
                            group: WorkspaceGroupHandle { id: group_id },
                        });
                    }
                }
            }
            ext_workspace_handle_v1::Request::Destroy => {
                for group in &mut state.workspace_state_mut().groups {
                    for workspace in &mut group.workspaces {
                        workspace.ext_instances.retain(|(_, i)| i != obj)
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
                workspace.ext_instances.retain(|(_, i)| i != resource)
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
    D: GlobalDispatch<ExtWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ExtWorkspaceManagerV1, ()>
        + Dispatch<ExtWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ExtWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    let (_, instance) = match group.ext_instances.iter_mut().find(|(m, _)| m == mngr) {
        Some(i) => i,
        None => {
            if let Ok(client) = dh.get_client(mngr.id()) {
                if let Ok(handle) = client.create_resource::<ExtWorkspaceGroupHandleV1, _, D>(
                    dh,
                    mngr.version(),
                    WorkspaceGroupData::default(),
                ) {
                    mngr.workspace_group(&handle);
                    group.ext_instances.push((mngr.downgrade(), handle));
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
    D: GlobalDispatch<ExtWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ExtWorkspaceManagerV1, ()>
        + Dispatch<ExtWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ExtWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    let (_, instance) = match workspace.ext_instances.iter_mut().find(|(m, _)| m == mngr) {
        Some(i) => i,
        None => {
            if let Ok(client) = dh.get_client(mngr.id()) {
                if let Ok(handle) = client.create_resource::<ExtWorkspaceHandleV1, _, D>(
                    dh,
                    mngr.version(),
                    WorkspaceData::default(),
                ) {
                    mngr.workspace(&handle);
                    group.workspace_enter(&handle);
                    if let Some(id) = workspace.ext_id.clone() {
                        handle.id(id);
                    }
                    workspace.ext_instances.push((mngr.downgrade(), handle));
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

    let mut handle_state = instance.data::<WorkspaceData>().unwrap().lock().unwrap();
    let mut changed = false;

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

    if handle_state.states != Some(workspace.states) {
        instance.state(workspace.states);
        handle_state.states = Some(workspace.states.clone());
        changed = true;
    }
    // TODO ext_workspace_handle_v1::id

    if let Some(cosmic_v2_handle) = handle_state
        .cosmic_v2_handle
        .as_ref()
        .and_then(|x| x.upgrade().ok())
    {
        changed |= super::cosmic_v2::send_workspace_to_client(&cosmic_v2_handle, workspace);
    }

    changed
}
