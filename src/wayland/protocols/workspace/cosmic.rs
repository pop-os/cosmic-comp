// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    reexports::wayland_protocols::ext::workspace::v1::server::ext_workspace_handle_v1::{self},
    reexports::wayland_server::{
        backend::{ClientData, ClientId},
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource,
    },
};

use super::{
    GroupCapabilities, Request, Workspace, WorkspaceCapabilities, WorkspaceClientHandler,
    WorkspaceData, WorkspaceGlobalData, WorkspaceGroup, WorkspaceGroupData, WorkspaceGroupHandle,
    WorkspaceHandler, WorkspaceState,
};

use cosmic_protocols::workspace::v1::server::{
    zcosmic_workspace_group_handle_v1::{self, ZcosmicWorkspaceGroupHandleV1},
    zcosmic_workspace_handle_v1::{self, ZcosmicWorkspaceHandleV1},
    zcosmic_workspace_manager_v1::{self, ZcosmicWorkspaceManagerV1},
};

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
                    .find(|g| g.instances.iter().any(|(_, i)| i == obj))
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
                    group.instances.retain(|(_, i)| i != obj)
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
            group.instances.retain(|(_, i)| i != resource)
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
                        workspace.instances.retain(|(_, i)| i != obj)
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
                workspace.instances.retain(|(_, i)| i != resource)
            }
        }
    }
}

pub(super) fn send_group_to_client<D>(
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
    let (_, instance) = match group.instances.iter_mut().find(|(m, _)| m == mngr) {
        Some(i) => i,
        None => {
            if let Ok(client) = dh.get_client(mngr.id()) {
                if let Ok(handle) = client.create_resource::<ZcosmicWorkspaceGroupHandleV1, _, D>(
                    dh,
                    mngr.version(),
                    WorkspaceGroupData::default(),
                ) {
                    mngr.workspace_group(&handle);
                    group.instances.push((mngr.downgrade(), handle));
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
        let caps = group
            .capabilities
            .iter()
            .filter_map(|cap| match cap {
                GroupCapabilities::CreateWorkspace => Some(zcosmic_workspace_group_handle_v1::ZcosmicWorkspaceGroupCapabilitiesV1::CreateWorkspace),
                _ => None,
            })
            .flat_map(|cap| (cap as u32).to_ne_bytes())
            .collect::<Vec<u8>>();
        instance.capabilities(caps);
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
    mngr: &ZcosmicWorkspaceManagerV1,
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
    let (_, instance) = match workspace.instances.iter_mut().find(|(m, _)| m == mngr) {
        Some(i) => i,
        None => {
            if let Ok(client) = dh.get_client(group.id()) {
                if let Ok(handle) = client.create_resource::<ZcosmicWorkspaceHandleV1, _, D>(
                    dh,
                    group.version(),
                    WorkspaceData::default(),
                ) {
                    group.workspace(&handle);
                    workspace.instances.push((mngr.downgrade(), handle));
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
        let coords = workspace
            .coordinates
            .iter()
            .flat_map(|coord| coord.to_ne_bytes())
            .collect::<Vec<u8>>();
        instance.coordinates(coords);
        handle_state.coordinates = workspace.coordinates.clone();
        changed = true;
    }
    if handle_state.capabilities != Some(workspace.capabilities) {
        let caps = workspace
            .capabilities
            .iter()
            .filter_map(|cap| match cap {
                WorkspaceCapabilities::Activate => {
                    Some(zcosmic_workspace_handle_v1::ZcosmicWorkspaceCapabilitiesV1::Activate)
                }
                WorkspaceCapabilities::Deactivate => {
                    Some(zcosmic_workspace_handle_v1::ZcosmicWorkspaceCapabilitiesV1::Deactivate)
                }
                WorkspaceCapabilities::Remove => {
                    Some(zcosmic_workspace_handle_v1::ZcosmicWorkspaceCapabilitiesV1::Remove)
                }
                WorkspaceCapabilities::Rename => {
                    Some(zcosmic_workspace_handle_v1::ZcosmicWorkspaceCapabilitiesV1::Rename)
                }
                WorkspaceCapabilities::SetTilingState => Some(
                    zcosmic_workspace_handle_v1::ZcosmicWorkspaceCapabilitiesV1::SetTilingState,
                ),
                _ => None,
            })
            .flat_map(|cap| (cap as u32).to_ne_bytes())
            .collect::<Vec<u8>>();
        instance.capabilities(caps);
        handle_state.capabilities = Some(workspace.capabilities.clone());
        changed = true;
    }
    if handle_state.states != Some(workspace.states) {
        let states = workspace
            .states
            .iter()
            .filter_map(|state| match state {
                ext_workspace_handle_v1::State::Active => {
                    Some(zcosmic_workspace_handle_v1::State::Active)
                }
                ext_workspace_handle_v1::State::Urgent => {
                    Some(zcosmic_workspace_handle_v1::State::Urgent)
                }
                ext_workspace_handle_v1::State::Hidden => {
                    Some(zcosmic_workspace_handle_v1::State::Hidden)
                }
                _ => None,
            })
            .flat_map(|state| (state as u32).to_ne_bytes())
            .collect::<Vec<u8>>();
        instance.state(states);
        handle_state.states = Some(workspace.states.clone());
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
