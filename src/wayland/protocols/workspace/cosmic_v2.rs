// SPDX-License-Identifier: GPL-3.0-only

use cosmic_protocols::workspace::v2::server::{
    zcosmic_workspace_handle_v2::{self, ZcosmicWorkspaceHandleV2},
    zcosmic_workspace_manager_v2::{self, ZcosmicWorkspaceManagerV2},
};
use smithay::reexports::{
    wayland_protocols::ext::workspace::v1::server::ext_workspace_handle_v1::ExtWorkspaceHandleV1,
    wayland_server::{
        backend::ClientData, Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New,
        Resource, Weak,
    },
};
use std::sync::Mutex;

use super::{
    Request, Workspace, WorkspaceCapabilities, WorkspaceClientHandler, WorkspaceData,
    WorkspaceGlobalData, WorkspaceHandler, WorkspaceState,
};

#[derive(Default)]
pub struct CosmicWorkspaceDataInner {
    capabilities: Option<WorkspaceCapabilities>,
    tiling: Option<zcosmic_workspace_handle_v2::TilingState>,
}

pub struct CosmicWorkspaceData {
    workspace: Weak<ExtWorkspaceHandleV1>,
    inner: Mutex<CosmicWorkspaceDataInner>,
}

impl<D> GlobalDispatch<ZcosmicWorkspaceManagerV2, WorkspaceGlobalData, D> for WorkspaceState<D>
where
    D: GlobalDispatch<ZcosmicWorkspaceManagerV2, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV2, ()>
        + Dispatch<ZcosmicWorkspaceHandleV2, CosmicWorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    fn bind(
        _state: &mut D,
        _dh: &DisplayHandle,
        _client: &Client,
        resource: New<ZcosmicWorkspaceManagerV2>,
        _global_data: &WorkspaceGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }

    fn can_view(client: Client, global_data: &WorkspaceGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ZcosmicWorkspaceManagerV2, (), D> for WorkspaceState<D>
where
    D: GlobalDispatch<ZcosmicWorkspaceManagerV2, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV2, ()>
        + Dispatch<ZcosmicWorkspaceHandleV2, CosmicWorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        obj: &ZcosmicWorkspaceManagerV2,
        request: zcosmic_workspace_manager_v2::Request,
        _data: &(),
        _dh: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_workspace_manager_v2::Request::GetCosmicWorkspace {
                cosmic_workspace,
                workspace,
            } => {
                let cosmic_workspace = data_init.init(
                    cosmic_workspace,
                    CosmicWorkspaceData {
                        workspace: workspace.downgrade(),
                        inner: Mutex::new(CosmicWorkspaceDataInner::default()),
                    },
                );
                if let Some(data) = workspace.data::<WorkspaceData>() {
                    let mut data = data.lock().unwrap();
                    if data.cosmic_v2_handle.as_ref().is_some_and(|x| x.is_alive()) {
                        obj.post_error(
                            zcosmic_workspace_manager_v2::Error::WorkspaceExists,
                            "zcosmic_workspace_handle_v2 already exists for ext_workspace_handle_v1",
                        );
                        return;
                    }
                    data.cosmic_v2_handle = Some(cosmic_workspace.downgrade());
                    if let Some((workspace, ext_mngr, _)) = state
                        .workspace_state()
                        .groups
                        .iter()
                        .flat_map(|g| &g.workspaces)
                        .flat_map(|w| w.ext_instances.iter().map(move |(mngr, i)| (w, mngr, i)))
                        .find(|(_, _, i)| **i == workspace)
                    {
                        if let Ok(ext_mngr) = ext_mngr.upgrade() {
                            send_workspace_to_client(&cosmic_workspace, workspace);
                            ext_mngr.done();
                        }
                    }
                }
            }
            _ => unreachable!(),
        }
    }
}

impl<D> Dispatch<ZcosmicWorkspaceHandleV2, CosmicWorkspaceData, D> for WorkspaceState<D>
where
    D: GlobalDispatch<ZcosmicWorkspaceManagerV2, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV2, ()>
        + Dispatch<ZcosmicWorkspaceHandleV2, CosmicWorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    fn request(
        state: &mut D,
        client: &Client,
        _obj: &ZcosmicWorkspaceHandleV2,
        request: zcosmic_workspace_handle_v2::Request,
        data: &CosmicWorkspaceData,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        let Ok(workspace) = data.workspace.upgrade() else {
            return;
        };
        match request {
            zcosmic_workspace_handle_v2::Request::Rename { name } => {
                if let Some(workspace_handle) =
                    state.workspace_state().get_ext_workspace_handle(&workspace)
                {
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
            zcosmic_workspace_handle_v2::Request::SetTilingState {
                state: tiling_state,
            } => {
                if let Some(workspace_handle) =
                    state.workspace_state().get_ext_workspace_handle(&workspace)
                {
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
            _ => unreachable!(),
        }
    }
}

pub fn send_workspace_to_client(
    instance: &ZcosmicWorkspaceHandleV2,
    workspace: &Workspace,
) -> bool {
    let mut changed = false;

    let mut handle_state = instance
        .data::<CosmicWorkspaceData>()
        .unwrap()
        .inner
        .lock()
        .unwrap();

    if handle_state.capabilities != Some(workspace.capabilities) {
        let caps = workspace
            .capabilities
            .iter()
            .filter_map(|cap| match cap {
                WorkspaceCapabilities::Rename => {
                    Some(zcosmic_workspace_handle_v2::WorkspaceCapabilities::Rename)
                }
                WorkspaceCapabilities::SetTilingState => {
                    Some(zcosmic_workspace_handle_v2::WorkspaceCapabilities::SetTilingState)
                }
                _ => None,
            })
            .collect::<zcosmic_workspace_handle_v2::WorkspaceCapabilities>();
        instance.capabilities(caps);
        handle_state.capabilities = Some(workspace.capabilities);
        changed = true;
    }

    if handle_state
        .tiling
        .map(|state| state != workspace.tiling)
        .unwrap_or(true)
    {
        instance.tiling_state(workspace.tiling);
        handle_state.tiling = Some(workspace.tiling);
        changed = true;
    }

    changed
}
