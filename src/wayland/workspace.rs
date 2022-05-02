// SPDX-License-Identifier: GPL-3.0-only

// Re-export only the actual code, and then only use this re-export
// The `generated` module below is just some boilerplate to properly isolate stuff
// and avoid exposing internal details.
//
// You can use all the types from my_protocol as if they went from `wayland_client::protocol`.
pub use generated::server::{
    zext_workspace_group_handle_v1, zext_workspace_handle_v1, zext_workspace_manager_v1,
};

mod generated {
    // The generated code tends to trigger a lot of warnings
    // so we isolate it into a very permissive module
    #![allow(dead_code, non_camel_case_types, unused_unsafe, unused_variables)]
    #![allow(non_upper_case_globals, non_snake_case, unused_imports)]

    pub mod server {
        use smithay::reexports::{wayland_commons, wayland_server};

        // These imports are used by the generated code
        pub(crate) use wayland_commons::map::{Object, ObjectMetadata};
        pub(crate) use wayland_commons::smallvec;
        pub(crate) use wayland_commons::wire::{Argument, ArgumentType, Message, MessageDesc};
        pub(crate) use wayland_commons::{Interface, MessageGroup};
        pub(crate) use wayland_server::protocol::wl_output;
        pub(crate) use wayland_server::sys;
        pub(crate) use wayland_server::{AnonymousObject, Main, Resource, ResourceMap};
        include!(concat!(env!("OUT_DIR"), "/ext_workspace.rs"));
    }
}

pub use self::generated::server::zext_workspace_handle_v1::State;
use self::generated::server::{
    zext_workspace_group_handle_v1::ZextWorkspaceGroupHandleV1,
    zext_workspace_handle_v1::ZextWorkspaceHandleV1,
    zext_workspace_manager_v1::ZextWorkspaceManagerV1,
};
use smithay::{
    reexports::wayland_server::{Client, DispatchData, Display, Filter, Global, Main},
    wayland::output::Output,
};
use std::{
    cell::RefCell,
    collections::HashSet,
    fmt,
    sync::{Arc, Mutex, Weak},
};

#[derive(Debug, Clone)]
pub struct WorkspaceManager {
    inner: Arc<Mutex<WorkspaceManagerInner>>,
}

struct WorkspaceManagerInner {
    instances: Vec<Main<ZextWorkspaceManagerV1>>,
    groups: Vec<Arc<Mutex<WorkspaceGroupInner>>>,
    commit: Box<
        dyn FnMut(&WorkspaceGroup, Vec<(&Workspace, Vec<PendingOperation>)>, DispatchData)
            + 'static,
    >,
}

impl fmt::Debug for WorkspaceManagerInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("WorkspaceManagerInner")
            .field("instances", &self.instances)
            .field("groups", &self.groups)
            .finish_non_exhaustive()
    }
}

pub fn init_ext_workspace<C, F>(
    display: &mut Display,
    commit: C,
    client_filter: F,
) -> (WorkspaceManager, Global<ZextWorkspaceManagerV1>)
where
    C: FnMut(&WorkspaceGroup, Vec<(&Workspace, Vec<PendingOperation>)>, DispatchData) + 'static,
    F: FnMut(Client) -> bool + 'static,
{
    let inner = Arc::new(Mutex::new(WorkspaceManagerInner {
        instances: Vec::new(),
        groups: Vec::new(),
        commit: Box::new(commit),
    }));

    let inner_clone = inner.clone();
    let filter = Filter::new(
        move |(main, _version): (Main<ZextWorkspaceManagerV1>, u32), _, _| {
            let inner = inner_clone.clone();
            main.quick_assign(move |main, request, mut ddata| match request {
                zext_workspace_manager_v1::Request::Commit => {
                    if let Some(client) = main.as_ref().client() {
                        let mut inner_guard = inner.lock().unwrap();
                        let inner = &mut *inner_guard;
                        for group in inner.groups.iter() {
                            let group_inner = group.lock().unwrap();
                            let mut changes = group_inner
                                .workspaces
                                .iter()
                                .flat_map(|x| x.upgrade())
                                .flat_map(|w| {
                                    let workspace_inner = w.lock().unwrap();
                                    let operations = workspace_inner
                                        .instances
                                        .iter()
                                        .find(|w_instance| {
                                            w_instance
                                                .as_ref()
                                                .client()
                                                .map(|c| c == client)
                                                .unwrap_or(false)
                                        })
                                        .and_then(|w_instance| {
                                            w_instance
                                                .as_ref()
                                                .user_data()
                                                .get::<WorkspaceUserdata>()
                                        })
                                        .and_then(|user_data| {
                                            let operations =
                                                std::mem::take(&mut *user_data.borrow_mut());
                                            if !operations.is_empty() {
                                                Some(operations)
                                            } else {
                                                None
                                            }
                                        });
                                    if let Some(ops) = operations {
                                        std::mem::drop(workspace_inner);
                                        Some((Workspace { inner: w }, ops))
                                    } else {
                                        None
                                    }
                                })
                                .collect::<Vec<(Workspace, Vec<PendingOperation>)>>();

                            std::mem::drop(group_inner);
                            let group = WorkspaceGroup {
                                inner: group.clone(),
                            };
                            let borrowed_changes = changes
                                .iter_mut()
                                .map(|(w, p)| (&*w, std::mem::take(p)))
                                .collect();
                            (inner.commit)(&group, borrowed_changes, ddata.reborrow());
                        }
                    }
                }
                zext_workspace_manager_v1::Request::Stop => {
                    let mut inner = inner.lock().unwrap();
                    inner.instances.retain(|m| **m != *main);
                }
            });

            let mut inner_guard = inner_clone.lock().unwrap();
            if let Some(client) = main.as_ref().client() {
                for group in inner_guard.groups.iter() {
                    if let Some(new_instance) =
                        WorkspaceGroup::create_instance(&client, group.clone())
                    {
                        main.workspace_group(&*new_instance);

                        let mut group_inner_guard = group.lock().unwrap();
                        for output in group_inner_guard.outputs.iter() {
                            output.with_client_outputs(client.clone(), |output| {
                                new_instance.output_enter(output)
                            });
                        }
                        for workspace in group_inner_guard.workspaces.iter() {
                            if let Some(workspace) = workspace.upgrade() {
                                let workspace_clone = workspace.clone();
                                if let Some(new_ws_instance) =
                                    Workspace::create_instance(&client, workspace_clone)
                                {
                                    new_instance.workspace(&*new_ws_instance);
                                    let mut inner = workspace.lock().unwrap();
                                    inner.instances.push(new_ws_instance);
                                    inner.send(&client);
                                }
                            }
                        }
                        group_inner_guard.instances.push(new_instance);
                    }
                }
                main.done();
            }

            inner_guard.instances.push(main);
        },
    );
    let global = display.create_global_with_filter(1, filter, client_filter);

    (WorkspaceManager { inner }, global)
}

impl WorkspaceManager {
    pub fn new_group<F>(
        &self,
        create_new_workspace: F,
        outputs: impl Iterator<Item = Output>,
    ) -> WorkspaceGroup
    where
        F: Fn(&WorkspaceGroup, String, DispatchData) + 'static,
    {
        let mut inner = self.inner.lock().unwrap();
        let create_new_workspace = Arc::new(Box::new(create_new_workspace)
            as Box<dyn Fn(&WorkspaceGroup, String, DispatchData) + 'static>);
        let group_inner = Arc::new(Mutex::new(WorkspaceGroupInner {
            instances: Vec::new(),
            outputs: outputs.collect(),
            workspaces: Vec::new(),
            create_new_workspace,
        }));

        for instance in inner.instances.iter() {
            if let Some(client) = instance.as_ref().client() {
                if let Some(group) = WorkspaceGroup::create_instance(&client, group_inner.clone()) {
                    instance.workspace_group(&*group);
                    let mut group_inner_guard = group_inner.lock().unwrap();
                    for output in group_inner_guard.outputs.iter() {
                        output.with_client_outputs(client.clone(), |output| {
                            group.output_enter(output)
                        });
                    }
                    group_inner_guard.instances.push(group);
                }
            }
            instance.done();
        }

        inner.groups.push(group_inner.clone());
        WorkspaceGroup { inner: group_inner }
    }

    pub fn update_outputs<F>(&self, mut update: F)
    where
        F: FnMut(&WorkspaceGroup, &mut HashSet<Output>),
    {
        let inner = self.inner.lock().unwrap();
        for group in inner.groups.iter() {
            let mut group_inner = group.lock().unwrap();
            let group = WorkspaceGroup {
                inner: group.clone(),
            };

            let previous_outputs = group_inner.outputs.clone();
            update(&group, &mut group_inner.outputs);

            for output in previous_outputs.difference(&group_inner.outputs) {
                for instance in group_inner.instances.iter() {
                    if let Some(client) = instance.as_ref().client() {
                        output.with_client_outputs(client.clone(), |output| {
                            instance.output_leave(output)
                        });
                    }
                }
            }
            for output in group_inner.outputs.difference(&previous_outputs) {
                for instance in group_inner.instances.iter() {
                    if let Some(client) = instance.as_ref().client() {
                        output.with_client_outputs(client.clone(), |output| {
                            instance.output_enter(output)
                        });
                    }
                }
            }
        }
        for instance in inner.instances.iter() {
            instance.done();
        }
    }

    pub fn remove_group(&self, group: &WorkspaceGroup) {
        let mut inner = self.inner.lock().unwrap();
        // grr I want drain_filter
        if let Some(pos) = inner
            .groups
            .iter()
            .position(|x| Arc::ptr_eq(x, &group.inner))
        {
            let group = inner.groups.remove(pos);
            let inner = group.lock().unwrap();
            for instance in inner.instances.iter() {
                instance.remove();
            }
        }
    }

    pub fn done(&self) {
        for instance in self.inner.lock().unwrap().instances.iter() {
            instance.done();
        }
    }
}

#[derive(Debug)]
pub struct WorkspaceGroup {
    inner: Arc<Mutex<WorkspaceGroupInner>>,
}

impl PartialEq for WorkspaceGroup {
    fn eq(&self, other: &WorkspaceGroup) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }
}

struct WorkspaceGroupInner {
    instances: Vec<Main<ZextWorkspaceGroupHandleV1>>,
    outputs: HashSet<Output>,
    workspaces: Vec<Weak<Mutex<WorkspaceInner>>>,
    create_new_workspace: Arc<Box<dyn Fn(&WorkspaceGroup, String, DispatchData) + 'static>>,
}

impl fmt::Debug for WorkspaceGroupInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("WorkspaceGroupInner")
            .field("instances", &self.instances)
            .field("outputs", &self.outputs)
            .field("workspaces", &self.workspaces)
            .finish_non_exhaustive()
    }
}

impl WorkspaceGroup {
    pub fn create_workspace(&self, name: String) -> Workspace {
        let mut inner = self.inner.lock().unwrap();
        let workspace_inner = Arc::new(Mutex::new(WorkspaceInner {
            instances: Vec::new(),
            name: name.clone(),
            states: Vec::new(),
            coordinates: Vec::new(),
        }));
        inner.workspaces.retain(|w| w.upgrade().is_some());
        inner.workspaces.push(Arc::downgrade(&workspace_inner));

        for instance in inner.instances.iter() {
            if let Some(client) = instance.as_ref().client() {
                let workspace_inner_clone = workspace_inner.clone();
                if let Some(workspace) = Workspace::create_instance(&client, workspace_inner_clone)
                {
                    instance.workspace(&*workspace);
                    let mut workspace_inner_guard = workspace_inner.lock().unwrap();
                    workspace_inner_guard.instances.push(workspace);
                    workspace_inner_guard.send(&client);
                }
            }
        }

        Workspace {
            inner: workspace_inner,
        }
    }

    pub fn belongs(&self, workspace: &Workspace) -> bool {
        self.inner
            .lock()
            .unwrap()
            .workspaces
            .iter()
            .flat_map(|x| x.upgrade())
            .any(|w| Arc::ptr_eq(&w, &workspace.inner))
    }

    fn create_instance(
        client: &Client,
        group_inner: Arc<Mutex<WorkspaceGroupInner>>,
    ) -> Option<Main<ZextWorkspaceGroupHandleV1>> {
        if let Some(group) = client.create_resource::<ZextWorkspaceGroupHandleV1>(1) {
            let group_inner_clone = group_inner.clone();
            group.quick_assign(move |group, request, ddata| match request {
                zext_workspace_group_handle_v1::Request::CreateWorkspace { workspace } => {
                    let callback = group_inner_clone
                        .lock()
                        .unwrap()
                        .create_new_workspace
                        .clone();
                    let group = WorkspaceGroup {
                        inner: group_inner_clone.clone(),
                    };
                    callback(&group, workspace, ddata);
                }
                zext_workspace_group_handle_v1::Request::Destroy => {
                    group_inner_clone
                        .lock()
                        .unwrap()
                        .instances
                        .retain(|g| *g != group);
                }
            });
            Some(group)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct Workspace {
    inner: Arc<Mutex<WorkspaceInner>>,
}

impl PartialEq for Workspace {
    fn eq(&self, other: &Workspace) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }
}

#[derive(Debug)]
struct WorkspaceInner {
    instances: Vec<Main<ZextWorkspaceHandleV1>>,
    name: String,
    states: Vec<State>,
    coordinates: Vec<u32>,
}

type WorkspaceUserdata = RefCell<Vec<PendingOperation>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PendingOperation {
    Activate,
    Deactivate,
    Remove,
}

impl Workspace {
    pub fn name(&self) -> String {
        self.inner.lock().unwrap().name.clone()
    }

    pub fn set_name(&self, name: String) {
        let mut inner = self.inner.lock().unwrap();
        for instance in inner.instances.iter() {
            instance.name(name.clone());
        }
        inner.name = name;
    }

    pub fn states(&self) -> Vec<State> {
        self.inner.lock().unwrap().states.clone()
    }

    pub fn add_state(&self, state: State) {
        let mut inner = self.inner.lock().unwrap();
        inner.states.push(state);
        for instance in inner.instances.iter() {
            let states = {
                let mut states = inner.states.clone();
                let ptr = states.as_mut_ptr();
                let len = states.len();
                let cap = states.capacity();
                ::std::mem::forget(states);
                unsafe { Vec::from_raw_parts(ptr as *mut u8, len * 4, cap * 4) }
            };
            instance.state(states);
        }
    }

    pub fn remove_state(&self, state: State) {
        let mut inner = self.inner.lock().unwrap();
        inner.states.retain(|s| *s != state);
        for instance in inner.instances.iter() {
            let states = {
                let mut states = inner.states.clone();
                let ptr = states.as_mut_ptr();
                let len = states.len();
                let cap = states.capacity();
                ::std::mem::forget(states);
                unsafe { Vec::from_raw_parts(ptr as *mut u8, len * 4, cap * 4) }
            };
            instance.state(states);
        }
    }

    pub fn set_states(&self, states: impl Iterator<Item = State>) {
        let mut inner = self.inner.lock().unwrap();
        inner.states = states.collect();
        for instance in inner.instances.iter() {
            let states = {
                let mut states = inner.states.clone();
                let ptr = states.as_mut_ptr();
                let len = states.len();
                let cap = states.capacity();
                ::std::mem::forget(states);
                unsafe { Vec::from_raw_parts(ptr as *mut u8, len * 4, cap * 4) }
            };
            instance.state(states);
        }
    }

    pub fn coordinates(&self) -> Vec<u32> {
        self.inner.lock().unwrap().coordinates.clone()
    }

    pub fn set_coordinates(&self, coordinates: impl Iterator<Item = u32>) {
        let mut inner = self.inner.lock().unwrap();
        inner.coordinates = coordinates.collect();
        for instances in inner.instances.iter() {
            let coords = {
                let mut coords = inner.coordinates.clone();
                let ptr = coords.as_mut_ptr();
                let len = coords.len();
                let cap = coords.capacity();
                ::std::mem::forget(coords);
                unsafe { Vec::from_raw_parts(ptr as *mut u8, len * 4, cap * 4) }
            };
            instances.coordinates(coords);
        }
    }

    pub fn remove(self) {
        for instance in self.inner.lock().unwrap().instances.drain(..) {
            instance.remove();
        }
    }

    fn create_instance(
        client: &Client,
        workspace_inner_clone: Arc<Mutex<WorkspaceInner>>,
    ) -> Option<Main<ZextWorkspaceHandleV1>> {
        if let Some(workspace) = client.create_resource::<ZextWorkspaceHandleV1>(1) {
            workspace.quick_assign(move |workspace, request, _| match request {
                zext_workspace_handle_v1::Request::Activate => {
                    let user_data = workspace.as_ref().user_data();
                    user_data.set(|| -> WorkspaceUserdata { RefCell::new(Vec::new()) });
                    user_data
                        .get::<WorkspaceUserdata>()
                        .unwrap()
                        .borrow_mut()
                        .push(PendingOperation::Activate);
                }
                zext_workspace_handle_v1::Request::Deactivate => {
                    let user_data = workspace.as_ref().user_data();
                    user_data.set(|| -> WorkspaceUserdata { RefCell::new(Vec::new()) });
                    user_data
                        .get::<WorkspaceUserdata>()
                        .unwrap()
                        .borrow_mut()
                        .push(PendingOperation::Deactivate);
                }
                zext_workspace_handle_v1::Request::Remove => {
                    let user_data = workspace.as_ref().user_data();
                    user_data.set(|| -> WorkspaceUserdata { RefCell::new(Vec::new()) });
                    user_data
                        .get::<WorkspaceUserdata>()
                        .unwrap()
                        .borrow_mut()
                        .push(PendingOperation::Remove);
                }
                zext_workspace_handle_v1::Request::Destroy => {
                    workspace_inner_clone
                        .lock()
                        .unwrap()
                        .instances
                        .retain(|w| **w != *workspace);
                }
            });
            Some(workspace)
        } else {
            None
        }
    }
}

impl WorkspaceInner {
    fn send(&self, client: &Client) {
        if let Some(instance) = self
            .instances
            .iter()
            .find(|w| w.as_ref().client().map(|c| &c == client).unwrap_or(false))
        {
            instance.name(self.name.clone());
            let states = {
                let mut states = self.states.clone();
                let ptr = states.as_mut_ptr();
                let len = states.len();
                let cap = states.capacity();
                ::std::mem::forget(states);
                unsafe { Vec::from_raw_parts(ptr as *mut u8, len * 4, cap * 4) }
            };
            instance.state(states);
            let coords = {
                let mut coords = self.coordinates.clone();
                let ptr = coords.as_mut_ptr();
                let len = coords.len();
                let cap = coords.capacity();
                ::std::mem::forget(coords);
                unsafe { Vec::from_raw_parts(ptr as *mut u8, len * 4, cap * 4) }
            };
            instance.coordinates(coords);
        }
    }
}
