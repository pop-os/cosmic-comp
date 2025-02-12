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
            Client, Dispatch, DisplayHandle, GlobalDispatch, Resource,
        },
    },
};
use wayland_backend::protocol::WEnum;

use cosmic_protocols::workspace::v1::server::{
    zcosmic_workspace_group_handle_v1::ZcosmicWorkspaceGroupHandleV1,
    zcosmic_workspace_handle_v1::{self, ZcosmicWorkspaceHandleV1},
    zcosmic_workspace_manager_v1::ZcosmicWorkspaceManagerV1,
};

mod cosmic;
mod ext;

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
    D: GlobalDispatch<ZcosmicWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV1, ()>
        + Dispatch<ZcosmicWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ZcosmicWorkspaceHandleV1, WorkspaceData>
        + GlobalDispatch<ExtWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ExtWorkspaceManagerV1, ()>
        + Dispatch<ExtWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ExtWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    dh: DisplayHandle,
    cosmic_global: GlobalId,
    ext_global: GlobalId,
    instances: Vec<ZcosmicWorkspaceManagerV1>,
    ext_instances: Vec<ExtWorkspaceManagerV1>,
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

#[derive(Debug)]
pub struct WorkspaceGroup {
    id: usize,
    instances: Vec<ZcosmicWorkspaceGroupHandleV1>,
    ext_instances: Vec<ExtWorkspaceGroupHandleV1>,
    workspaces: Vec<Workspace>,

    outputs: Vec<Output>,
    capabilities: GroupCapabilities,
}

impl Default for WorkspaceGroup {
    fn default() -> Self {
        Self {
            id: 0,
            instances: Vec::new(),
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

pub struct WorkspaceGroupDataInner {
    outputs: Vec<Output>,
    wl_outputs: HashSet<WlOutput>,
    capabilities: GroupCapabilities,
    workspace_count: usize,
}

impl Default for WorkspaceGroupDataInner {
    fn default() -> Self {
        Self {
            outputs: Vec::new(),
            wl_outputs: HashSet::new(),
            capabilities: GroupCapabilities::empty(),
            workspace_count: 0,
        }
    }
}

pub type WorkspaceGroupData = Mutex<WorkspaceGroupDataInner>;

#[derive(Debug)]
pub struct Workspace {
    id: usize,
    instances: Vec<ZcosmicWorkspaceHandleV1>,
    ext_instances: Vec<ExtWorkspaceHandleV1>,

    name: String,
    capabilities: WorkspaceCapabilities,
    coordinates: Vec<u32>,
    states: ext_workspace_handle_v1::State,
    tiling: zcosmic_workspace_handle_v1::TilingState,
    ext_id: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WorkspaceHandle {
    id: usize,
}

pub struct WorkspaceDataInner {
    name: String,
    capabilities: WorkspaceCapabilities,
    coordinates: Vec<u32>,
    states: ext_workspace_handle_v1::State,
    tiling: Option<zcosmic_workspace_handle_v1::TilingState>,
}

impl Default for WorkspaceDataInner {
    fn default() -> Self {
        Self {
            name: String::new(),
            capabilities: WorkspaceCapabilities::empty(),
            coordinates: Vec::new(),
            states: ext_workspace_handle_v1::State::empty(),
            tiling: None,
        }
    }
}

pub type WorkspaceData = Mutex<WorkspaceDataInner>;

pub trait WorkspaceHandler
where
    Self: GlobalDispatch<ZcosmicWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV1, ()>
        + Dispatch<ZcosmicWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ZcosmicWorkspaceHandleV1, WorkspaceData>
        + GlobalDispatch<ExtWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ExtWorkspaceManagerV1, ()>
        + Dispatch<ExtWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ExtWorkspaceHandleV1, WorkspaceData>
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
    D: GlobalDispatch<ZcosmicWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV1, ()>
        + Dispatch<ZcosmicWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ZcosmicWorkspaceHandleV1, WorkspaceData>
        + GlobalDispatch<ExtWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ExtWorkspaceManagerV1, ()>
        + Dispatch<ExtWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ExtWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    pub fn new<F>(dh: &DisplayHandle, client_filter: F) -> WorkspaceState<D>
    where
        F: for<'a> Fn(&'a Client) -> bool + Clone + Send + Sync + 'static,
    {
        let cosmic_global = dh.create_global::<D, ZcosmicWorkspaceManagerV1, _>(
            2,
            WorkspaceGlobalData {
                filter: Box::new(client_filter.clone()),
            },
        );

        let ext_global = dh.create_global::<D, ExtWorkspaceManagerV1, _>(
            1,
            WorkspaceGlobalData {
                filter: Box::new(client_filter),
            },
        );

        WorkspaceState {
            dh: dh.clone(),
            cosmic_global,
            ext_global,
            instances: Vec::new(),
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

    pub fn update(&mut self) -> WorkspaceUpdateGuard<'_, D> {
        WorkspaceUpdateGuard(self)
    }

    fn done(&mut self) {
        let mut changed = false;
        for instance in &self.instances {
            for mut group in &mut self.groups {
                if cosmic::send_group_to_client::<D>(&self.dh, instance, &mut group) {
                    changed = true;
                }
            }
        }
        for instance in &self.ext_instances {
            for mut group in &mut self.groups {
                if ext::send_group_to_client::<D>(&self.dh, instance, &mut group) {
                    changed = true;
                }
            }
        }
        if changed {
            for instance in &self.instances {
                instance.done();
            }
            for instance in &self.ext_instances {
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

    pub fn get_ext_workspace_handle(
        &self,
        handle: &ExtWorkspaceHandleV1,
    ) -> Option<WorkspaceHandle> {
        self.groups
            .iter()
            .find_map(|g| {
                g.workspaces
                    .iter()
                    .find(|w| w.ext_instances.contains(handle))
            })
            .map(|w| WorkspaceHandle { id: w.id })
    }

    pub fn cosmic_global_id(&self) -> GlobalId {
        self.cosmic_global.clone()
    }

    pub fn ext_global_id(&self) -> GlobalId {
        self.ext_global.clone()
    }
}

impl<'a, D> WorkspaceUpdateGuard<'a, D>
where
    D: GlobalDispatch<ZcosmicWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ZcosmicWorkspaceManagerV1, ()>
        + Dispatch<ZcosmicWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ZcosmicWorkspaceHandleV1, WorkspaceData>
        + Dispatch<ExtWorkspaceManagerV1, ()>
        + Dispatch<ExtWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ExtWorkspaceHandleV1, WorkspaceData>
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
        ext_id: Option<String>,
    ) -> Option<WorkspaceHandle> {
        if let Some(group) = self.0.groups.iter_mut().find(|g| g.id == group.id) {
            let id = next_workspace_id();
            let workspace = Workspace {
                id,
                tiling,
                instances: Default::default(),
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
            for instance in &group.instances {
                instance.remove();
            }
            for instance in &group.ext_instances {
                instance.removed();
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
                for instance in &workspace.ext_instances {
                    // TODO remove only if it matches the group
                    for group_instance in &group.ext_instances {
                        group_instance.workspace_leave(instance);
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
        + GlobalDispatch<ExtWorkspaceManagerV1, WorkspaceGlobalData>
        + Dispatch<ExtWorkspaceManagerV1, ()>
        + Dispatch<ExtWorkspaceGroupHandleV1, WorkspaceGroupData>
        + Dispatch<ExtWorkspaceHandleV1, WorkspaceData>
        + WorkspaceHandler
        + 'static,
    <D as WorkspaceHandler>::Client: ClientData + WorkspaceClientHandler + 'static,
{
    fn drop(&mut self) {
        self.0.done();
    }
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
    };
}
pub(crate) use delegate_workspace;
