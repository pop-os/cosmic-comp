// SPDX-License-Identifier: GPL-3.0-only

use std::{collections::HashSet, sync::Mutex};

use smithay::{
    output::Output,
    reexports::{
        wayland_protocols::ext::workspace::v1::server::{
            ext_workspace_group_handle_v1::ExtWorkspaceGroupHandleV1,
            ext_workspace_handle_v1::{self, ExtWorkspaceHandleV1},
            ext_workspace_manager_v1::ExtWorkspaceManagerV1,
        },
        wayland_server::{
            backend::{ClientData, GlobalId, ObjectId},
            protocol::wl_output::WlOutput,
            Client, Dispatch, DisplayHandle, GlobalDispatch, Resource, Weak,
        },
    },
};
use wayland_backend::protocol::WEnum;

use cosmic_protocols::workspace::v2::server::{
    zcosmic_workspace_handle_v2::{self, ZcosmicWorkspaceHandleV2},
    zcosmic_workspace_manager_v2::ZcosmicWorkspaceManagerV2,
};

mod cosmic_v2;
pub use cosmic_v2::CosmicWorkspaceV2Data;
mod ext;
pub use ext::WorkspaceData;

pub use smithay::reexports::wayland_protocols::ext::workspace::v1::server::ext_workspace_group_handle_v1::GroupCapabilities;

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
    pub struct WorkspaceCapabilities: u32 {
        const Activate = 1;
        const Deactivate = 2;
        const Remove = 4;
        /// not in legacy cosmic protocol
        const Assign = 8;
        /// cosmic specific
        const Rename = 16;
        /// cosmic specific
        const SetTilingState = 32;
    }
}

#[derive(Debug)]
pub struct WorkspaceState<D>
where
    D: WorkspaceHandler,
{
    dh: DisplayHandle,
    ext_global: GlobalId,
    cosmic_v2_global: GlobalId,
    ext_instances: Vec<ExtWorkspaceManagerV1>,
    groups: Vec<WorkspaceGroup>,
    _marker: std::marker::PhantomData<D>,
}
pub struct WorkspaceUpdateGuard<'a, D>(&'a mut WorkspaceState<D>)
where
    D: WorkspaceHandler;

crate::utils::id_gen!(next_group_id, GROUP_ID, GROUP_IDS);
crate::utils::id_gen!(next_workspace_id, WORKSPACE_ID, WORKSPACE_IDS);

#[derive(Debug)]
pub struct WorkspaceGroup {
    id: usize,
    ext_instances: Vec<(Weak<ExtWorkspaceManagerV1>, ExtWorkspaceGroupHandleV1)>,
    workspaces: Vec<Workspace>,

    outputs: Vec<Output>,
    capabilities: GroupCapabilities,
}

impl Default for WorkspaceGroup {
    fn default() -> Self {
        Self {
            id: 0,
            ext_instances: Vec::new(),
            workspaces: Vec::new(),

            outputs: Vec::new(),
            capabilities: GroupCapabilities::empty(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WorkspaceGroupHandle {
    id: usize,
}

#[derive(Default)]
pub struct WorkspaceGroupDataInner {
    outputs: Vec<Output>,
    wl_outputs: HashSet<WlOutput>,
    capabilities: Option<GroupCapabilities>,
    workspace_count: usize,
}

pub type WorkspaceGroupData = Mutex<WorkspaceGroupDataInner>;

#[derive(Debug)]
pub struct Workspace {
    id: usize,
    ext_instances: Vec<(Weak<ExtWorkspaceManagerV1>, ExtWorkspaceHandleV1)>,

    name: String,
    capabilities: WorkspaceCapabilities,
    coordinates: Vec<u32>,
    states: ext_workspace_handle_v1::State,
    tiling: zcosmic_workspace_handle_v2::TilingState,
    ext_id: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WorkspaceHandle {
    id: usize,
}

pub trait WorkspaceHandler
where
    Self: GlobalDispatch<ExtWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ExtWorkspaceManagerV1, ()>
        + Dispatch<ExtWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ExtWorkspaceHandleV1, WorkspaceData>
        + GlobalDispatch<ZcosmicWorkspaceManagerV2, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV2, ()>
        + Dispatch<ZcosmicWorkspaceHandleV2, CosmicWorkspaceV2Data>
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
        state: WEnum<zcosmic_workspace_handle_v2::TilingState>,
    },
    Create {
        in_group: WorkspaceGroupHandle,
        name: String,
    },
    Assign {
        workspace: WorkspaceHandle,
        group: WorkspaceGroupHandle,
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

impl<D> WorkspaceState<D>
where
    D: WorkspaceHandler,
{
    pub fn new<F>(dh: &DisplayHandle, client_filter: F) -> WorkspaceState<D>
    where
        F: for<'a> Fn(&'a Client) -> bool + Clone + Send + Sync + 'static,
    {
        let ext_global = dh.create_global::<D, ExtWorkspaceManagerV1, _>(
            1,
            WorkspaceGlobalData {
                filter: Box::new(client_filter.clone()),
            },
        );

        let cosmic_v2_global = dh.create_global::<D, ZcosmicWorkspaceManagerV2, _>(
            1,
            WorkspaceGlobalData {
                filter: Box::new(client_filter.clone()),
            },
        );

        WorkspaceState {
            dh: dh.clone(),
            ext_global,
            cosmic_v2_global,
            ext_instances: Vec::new(),
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

    pub fn group_capabilities(&self, group: &WorkspaceGroupHandle) -> Option<GroupCapabilities> {
        Some(self.groups.iter().find(|g| g.id == group.id)?.capabilities)
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
    ) -> Option<WorkspaceCapabilities> {
        self.groups.iter().find_map(|g| {
            Some(
                g.workspaces
                    .iter()
                    .find(|w| w.id == workspace.id)?
                    .capabilities,
            )
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
    ) -> Option<ext_workspace_handle_v1::State> {
        self.groups
            .iter()
            .find_map(|g| Some(g.workspaces.iter().find(|w| w.id == workspace.id)?.states))
    }

    pub fn workspace_tiling_state(
        &self,
        workspace: &WorkspaceHandle,
    ) -> Option<zcosmic_workspace_handle_v2::TilingState> {
        self.groups.iter().find_map(|g| {
            g.workspaces
                .iter()
                .find(|w| w.id == workspace.id)
                .map(|w| w.tiling)
        })
    }

    pub fn raw_ext_workspace_handles<'a>(
        &'a self,
        workspace: &'a WorkspaceHandle,
        client: &'a ObjectId,
    ) -> impl Iterator<Item = &ExtWorkspaceHandleV1> + 'a {
        self.groups
            .iter()
            .find_map(|g| g.workspaces.iter().find(|w| w.id == workspace.id))
            .into_iter()
            .flat_map(|w| &w.ext_instances)
            .map(|(_, i)| i)
            .filter(|i| Resource::id(*i).same_client_as(client))
    }

    pub fn update(&mut self) -> WorkspaceUpdateGuard<'_, D> {
        WorkspaceUpdateGuard(self)
    }

    fn done(&mut self) {
        let mut changed = false;
        for instance in &self.ext_instances {
            for mut group in &mut self.groups {
                if ext::send_group_to_client::<D>(&self.dh, instance, &mut group) {
                    changed = true;
                }
            }
        }
        if changed {
            for instance in &self.ext_instances {
                instance.done();
            }
        }
    }

    pub fn get_ext_workspace_handle(
        &self,
        handle: &ExtWorkspaceHandleV1,
    ) -> Option<WorkspaceHandle> {
        self.groups
            .iter()
            .find_map(|g| {
                g.workspaces
                    .iter()
                    .find(|w| w.ext_instances.iter().any(|(_, i)| i == handle))
            })
            .map(|w| WorkspaceHandle { id: w.id })
    }

    pub fn ext_global_id(&self) -> GlobalId {
        self.ext_global.clone()
    }

    pub fn cosmic_v2_global_id(&self) -> GlobalId {
        self.cosmic_v2_global.clone()
    }
}

impl<'a, D> WorkspaceUpdateGuard<'a, D>
where
    D: WorkspaceHandler,
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
        tiling: zcosmic_workspace_handle_v2::TilingState,
        ext_id: Option<String>,
    ) -> Option<WorkspaceHandle> {
        if let Some(group) = self.0.groups.iter_mut().find(|g| g.id == group.id) {
            let id = next_workspace_id();
            let workspace = Workspace {
                id,
                tiling,
                ext_instances: Default::default(),
                name: Default::default(),
                capabilities: WorkspaceCapabilities::empty(),
                coordinates: Default::default(),
                states: ext_workspace_handle_v1::State::empty(),
                ext_id,
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
            for (_, instance) in &group.ext_instances {
                instance.removed();
            }
        }
        self.0.groups.retain(|g| g.id != group.id);
        GROUP_IDS.lock().unwrap().remove(&group.id);
    }

    pub fn remove_workspace(&mut self, workspace: WorkspaceHandle) {
        for group in &mut self.0.groups {
            if let Some(workspace) = group.workspaces.iter().find(|w| w.id == workspace.id) {
                for (manager, instance) in &workspace.ext_instances {
                    for (group_manager, group_instance) in &group.ext_instances {
                        if manager == group_manager {
                            group_instance.workspace_leave(instance);
                        }
                    }
                    instance.removed();
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
    ) -> Option<GroupCapabilities> {
        self.0.group_capabilities(group)
    }

    pub fn set_group_capabilities(
        &mut self,
        group: &WorkspaceGroupHandle,
        capabilities: GroupCapabilities,
    ) {
        if let Some(group) = self.0.groups.iter_mut().find(|g| g.id == group.id) {
            group.capabilities = capabilities;
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
    ) -> Option<WorkspaceCapabilities> {
        self.0.workspace_capabilities(workspace)
    }

    pub fn set_workspace_capabilities(
        &mut self,
        workspace: &WorkspaceHandle,
        capabilities: WorkspaceCapabilities,
    ) {
        if let Some(workspace) = self
            .0
            .groups
            .iter_mut()
            .find_map(|g| g.workspaces.iter_mut().find(|w| w.id == workspace.id))
        {
            workspace.capabilities = capabilities;
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

    pub fn set_workspace_coordinates(&mut self, workspace: &WorkspaceHandle, coords: &[u32]) {
        if let Some(workspace) = self
            .0
            .groups
            .iter_mut()
            .find_map(|g| g.workspaces.iter_mut().find(|w| w.id == workspace.id))
        {
            workspace.coordinates = coords.to_vec();
        }
    }

    pub fn workspace_states(
        &self,
        workspace: &WorkspaceHandle,
    ) -> Option<ext_workspace_handle_v1::State> {
        self.0.workspace_states(workspace)
    }

    pub fn add_workspace_state(
        &mut self,
        workspace: &WorkspaceHandle,
        state: ext_workspace_handle_v1::State,
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
        state: ext_workspace_handle_v1::State,
    ) {
        if let Some(workspace) = self
            .0
            .groups
            .iter_mut()
            .find_map(|g| g.workspaces.iter_mut().find(|w| w.id == workspace.id))
        {
            workspace.states.remove(state);
        }
    }

    pub fn workspace_tiling_state(
        &self,
        workspace: &WorkspaceHandle,
    ) -> Option<zcosmic_workspace_handle_v2::TilingState> {
        self.0.workspace_tiling_state(workspace)
    }

    pub fn set_workspace_tiling_state(
        &mut self,
        workspace: &WorkspaceHandle,
        state: zcosmic_workspace_handle_v2::TilingState,
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
    D: WorkspaceHandler,
{
    fn drop(&mut self) {
        self.0.done();
    }
}

macro_rules! delegate_workspace {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::workspace::v1::server::ext_workspace_manager_v1::ExtWorkspaceManagerV1: $crate::wayland::protocols::workspace::WorkspaceGlobalData
        ] => $crate::wayland::protocols::workspace::WorkspaceState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::workspace::v1::server::ext_workspace_manager_v1::ExtWorkspaceManagerV1: ()
        ] => $crate::wayland::protocols::workspace::WorkspaceState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::workspace::v1::server::ext_workspace_group_handle_v1::ExtWorkspaceGroupHandleV1: $crate::wayland::protocols::workspace::WorkspaceGroupData
        ] => $crate::wayland::protocols::workspace::WorkspaceState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::workspace::v1::server::ext_workspace_handle_v1::ExtWorkspaceHandleV1: $crate::wayland::protocols::workspace::WorkspaceData
        ] => $crate::wayland::protocols::workspace::WorkspaceState<Self>);

        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
        cosmic_protocols::workspace::v2::server::zcosmic_workspace_manager_v2::ZcosmicWorkspaceManagerV2: $crate::wayland::protocols::workspace::WorkspaceGlobalData
        ] => $crate::wayland::protocols::workspace::WorkspaceState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::workspace::v2::server::zcosmic_workspace_manager_v2::ZcosmicWorkspaceManagerV2: ()
        ] => $crate::wayland::protocols::workspace::WorkspaceState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::workspace::v2::server::zcosmic_workspace_handle_v2::ZcosmicWorkspaceHandleV2: $crate::wayland::protocols::workspace::CosmicWorkspaceV2Data
        ] => $crate::wayland::protocols::workspace::WorkspaceState<Self>);
    };
}
pub(crate) use delegate_workspace;
