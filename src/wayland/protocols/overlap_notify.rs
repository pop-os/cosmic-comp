// SPDX-License-Identifier: GPL-3.0-only

use std::{collections::HashMap, sync::Mutex};

use cosmic_protocols::{
    overlap_notify::v1::server::{
        zcosmic_overlap_notification_v1::ZcosmicOverlapNotificationV1,
        zcosmic_overlap_notify_v1::{self, ZcosmicOverlapNotifyV1},
    },
    toplevel_info::v1::server::{
        zcosmic_toplevel_handle_v1::ZcosmicToplevelHandleV1,
        zcosmic_toplevel_info_v1::ZcosmicToplevelInfoV1,
    },
};
use smithay::{
    desktop::{LayerSurface, layer_map_for_output},
    output::Output,
    reexports::{
        wayland_protocols::ext::foreign_toplevel_list::v1::server::ext_foreign_toplevel_handle_v1::ExtForeignToplevelHandleV1,
        wayland_protocols_wlr::layer_shell::v1::server::{
            zwlr_layer_shell_v1::Layer as WlrLayer, zwlr_layer_surface_v1::ZwlrLayerSurfaceV1,
        },
        wayland_server::{Client, Dispatch, DisplayHandle, GlobalDispatch, Resource, Weak},
    },
    utils::{Logical, Rectangle},
    wayland::{
        foreign_toplevel_list::ForeignToplevelListHandler,
        shell::wlr_layer::{ExclusiveZone, Layer},
    },
};
use wayland_backend::server::{GlobalId, ObjectId};

use crate::utils::prelude::{RectExt, RectGlobalExt, RectLocalExt};

use super::{
    toplevel_info::{
        ToplevelHandleState, ToplevelInfoGlobalData, ToplevelInfoHandler, ToplevelState, Window,
    },
    workspace::WorkspaceHandle,
};

#[derive(Debug)]
pub struct OverlapNotifyState {
    instances: Vec<ZcosmicOverlapNotifyV1>,
    global: GlobalId,
}

impl OverlapNotifyState {
    pub fn new<D, F>(dh: &DisplayHandle, client_filter: F) -> OverlapNotifyState
    where
        D: GlobalDispatch<ZcosmicOverlapNotifyV1, OverlapNotifyGlobalData>
            + Dispatch<ZcosmicOverlapNotifyV1, ()>
            + Dispatch<ZcosmicOverlapNotificationV1, ()>
            + OverlapNotifyHandler
            + 'static,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        let global = dh.create_global::<D, ZcosmicOverlapNotifyV1, _>(
            1,
            OverlapNotifyGlobalData {
                filter: Box::new(client_filter),
            },
        );
        OverlapNotifyState {
            instances: Vec::new(),
            global,
        }
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }

    #[profiling::function]
    pub fn refresh<D, W>(state: &mut D)
    where
        D: GlobalDispatch<ZcosmicOverlapNotifyV1, OverlapNotifyGlobalData>
            + Dispatch<ZcosmicOverlapNotifyV1, ()>
            + Dispatch<ZcosmicOverlapNotificationV1, ()>
            + OverlapNotifyHandler
            + GlobalDispatch<ZcosmicToplevelInfoV1, ToplevelInfoGlobalData>
            + Dispatch<ZcosmicToplevelInfoV1, ()>
            + Dispatch<ZcosmicToplevelHandleV1, ToplevelHandleState<W>>
            + ForeignToplevelListHandler
            + ToplevelInfoHandler<Window = W>
            + 'static,
        W: Window + 'static,
    {
        let active_workspaces: Vec<_> = state.active_workspaces().collect();
        for output in state.outputs() {
            let map = layer_map_for_output(&output);

            for layer_surface in map.layers() {
                if let Some(data) = layer_surface
                    .user_data()
                    .get::<LayerOverlapNotificationData>()
                {
                    let mut inner = data.lock().unwrap();

                    if inner.has_active_notifications() {
                        let mut new_snapshot = OverlapSnapshot::default();

                        let layer_geo = map
                            .layer_geometry(layer_surface)
                            .unwrap_or_default()
                            .as_local()
                            .to_global(&output);

                        for window in
                            state
                                .toplevel_info_state()
                                .registered_toplevels()
                                .filter(|w| {
                                    let state = w
                                        .user_data()
                                        .get::<ToplevelState>()
                                        .unwrap()
                                        .lock()
                                        .unwrap();

                                    w.is_sticky()
                                        || active_workspaces.iter().any(|active_workspace| {
                                            state.in_workspace(active_workspace)
                                        })
                                })
                        {
                            if let Some(window_geo) = window.global_geometry() {
                                if let Some(intersection) = layer_geo.intersection(window_geo) {
                                    // relative to layer location
                                    let region = Rectangle::new(
                                        intersection.loc - layer_geo.loc,
                                        intersection.size,
                                    )
                                    .as_logical();
                                    new_snapshot.add_toplevel(window, region);
                                }
                            }
                        }

                        for other_surface in map.layers().filter(|s| *s != layer_surface) {
                            if other_surface.wl_surface().id() == layer_surface.wl_surface().id() {
                                continue;
                            }
                            let other_geo = map
                                .layer_geometry(other_surface)
                                .unwrap_or_default()
                                .as_local()
                                .to_global(&output);
                            if let Some(intersection) = layer_geo.intersection(other_geo) {
                                // relative to layer location
                                let region = Rectangle::new(
                                    intersection.loc - layer_geo.loc,
                                    intersection.size,
                                )
                                .as_logical();
                                new_snapshot.add_layer(other_surface, region);
                            }
                        }

                        inner.update_snapshot(new_snapshot);
                    }
                }
            }
        }
    }
}

pub trait OverlapNotifyHandler: ToplevelInfoHandler {
    fn overlap_notify_state(&mut self) -> &mut OverlapNotifyState;
    fn layer_surface_from_resource(&self, resource: ZwlrLayerSurfaceV1) -> Option<LayerSurface>;
    fn outputs(&self) -> impl Iterator<Item = Output>;
    fn active_workspaces(&self) -> impl Iterator<Item = WorkspaceHandle>;
}

pub struct OverlapNotifyGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

type LayerOverlapNotificationData = Mutex<LayerOverlapNotificationDataInternal>;

#[derive(Debug, Default)]
struct LayerOverlapNotificationDataInternal {
    active_notifications: Vec<Weak<ZcosmicOverlapNotificationV1>>,
    last_snapshot: OverlapSnapshot,
}

impl LayerOverlapNotificationDataInternal {
    pub fn has_active_notifications(&mut self) -> bool {
        self.active_notifications.retain(|w| w.upgrade().is_ok());
        !self.active_notifications.is_empty()
    }

    pub fn add_notification(&mut self, new_notification: ZcosmicOverlapNotificationV1) {
        if let Some(client) = new_notification.client() {
            for (toplevel, overlap) in &self.last_snapshot.toplevel_overlaps {
                if let Some(toplevel) = toplevel
                    .upgrade()
                    .ok()
                    .filter(|handle| handle.client().is_some_and(|c| c == client))
                {
                    new_notification.toplevel_enter(
                        &toplevel,
                        overlap.loc.x,
                        overlap.loc.y,
                        overlap.size.w,
                        overlap.size.h,
                    );
                }
            }
        }
        for (identifier, namespace, exclusive, layer, overlap) in
            self.last_snapshot.layer_overlaps.values()
        {
            new_notification.layer_enter(
                identifier.clone(),
                namespace.clone(),
                if *exclusive { 1 } else { 0 },
                match layer {
                    Layer::Background => WlrLayer::Background,
                    Layer::Bottom => WlrLayer::Bottom,
                    Layer::Top => WlrLayer::Top,
                    Layer::Overlay => WlrLayer::Overlay,
                },
                overlap.loc.x,
                overlap.loc.y,
                overlap.size.w,
                overlap.size.h,
            );
        }
        self.active_notifications.push(new_notification.downgrade());
    }

    pub fn update_snapshot(&mut self, new_snapshot: OverlapSnapshot) {
        let notifications = self
            .active_notifications
            .iter()
            .flat_map(|w| w.upgrade().ok())
            .collect::<Vec<_>>();

        for toplevel in self.last_snapshot.toplevel_overlaps.keys() {
            if !new_snapshot.toplevel_overlaps.contains_key(toplevel) {
                if let Ok(toplevel) = toplevel.upgrade() {
                    if let Some(client) = toplevel.client() {
                        for notification in notifications
                            .iter()
                            .filter(|n| n.client().is_some_and(|c| c == client))
                        {
                            notification.toplevel_leave(&toplevel);
                        }
                    }
                }
            }
        }
        for (toplevel, overlap) in &new_snapshot.toplevel_overlaps {
            if !self
                .last_snapshot
                .toplevel_overlaps
                .get(toplevel)
                .is_some_and(|old_overlap| old_overlap == overlap)
            {
                if let Ok(toplevel) = toplevel.upgrade() {
                    if let Some(client) = toplevel.client() {
                        for notification in notifications
                            .iter()
                            .filter(|n| n.client().is_some_and(|c| c == client))
                        {
                            notification.toplevel_enter(
                                &toplevel,
                                overlap.loc.x,
                                overlap.loc.y,
                                overlap.size.w,
                                overlap.size.h,
                            );
                        }
                    }
                }
            }
        }

        for (layer_surface, (identifier, ..)) in &self.last_snapshot.layer_overlaps {
            if !new_snapshot.layer_overlaps.contains_key(layer_surface) {
                for notification in &notifications {
                    notification.layer_leave(identifier.clone());
                }
            }
        }
        for (layer_surface, (identifier, namespace, exclusive, layer, overlap)) in
            &new_snapshot.layer_overlaps
        {
            if !self
                .last_snapshot
                .layer_overlaps
                .get(layer_surface)
                .is_some_and(|(_, _, _, _, old_overlap)| old_overlap == overlap)
            {
                for notification in &notifications {
                    notification.layer_enter(
                        identifier.clone(),
                        namespace.clone(),
                        if *exclusive { 1 } else { 0 },
                        match layer {
                            Layer::Background => WlrLayer::Background,
                            Layer::Bottom => WlrLayer::Bottom,
                            Layer::Top => WlrLayer::Top,
                            Layer::Overlay => WlrLayer::Overlay,
                        },
                        overlap.loc.x,
                        overlap.loc.y,
                        overlap.size.w,
                        overlap.size.h,
                    );
                }
            }
        }

        self.last_snapshot = new_snapshot;
    }
}

#[derive(Debug, Default, Clone)]
struct OverlapSnapshot {
    toplevel_overlaps: HashMap<Weak<ExtForeignToplevelHandleV1>, Rectangle<i32, Logical>>,
    layer_overlaps: HashMap<ObjectId, (String, String, bool, Layer, Rectangle<i32, Logical>)>,
}

impl OverlapSnapshot {
    pub fn add_toplevel(&mut self, window: &impl Window, overlap: Rectangle<i32, Logical>) {
        if let Some(handles) = window
            .user_data()
            .get::<ToplevelState>()
            .unwrap()
            .lock()
            .unwrap()
            .foreign_handle()
            .map(|handle| handle.resources())
        {
            for handle in handles.into_iter().map(|h| h.downgrade()) {
                self.toplevel_overlaps.insert(handle, overlap);
            }
        }
    }

    pub fn add_layer(&mut self, layer_surface: &LayerSurface, overlap: Rectangle<i32, Logical>) {
        let exclusive = matches!(
            layer_surface.cached_state().exclusive_zone,
            ExclusiveZone::Exclusive(_)
        );
        let layer = layer_surface.layer();
        let id = layer_surface.wl_surface().id();
        let identifier = layer_surface
            .user_data()
            .get_or_insert(|| Identifier::from(id.clone()));

        self.layer_overlaps.insert(
            id,
            (
                identifier.0.clone(),
                layer_surface.namespace().to_string(),
                exclusive,
                layer,
                overlap,
            ),
        );
    }
}

struct Identifier(String);

impl From<ObjectId> for Identifier {
    fn from(value: ObjectId) -> Self {
        Identifier(value.to_string())
    }
}

impl<D> GlobalDispatch<ZcosmicOverlapNotifyV1, OverlapNotifyGlobalData, D> for OverlapNotifyState
where
    D: GlobalDispatch<ZcosmicOverlapNotifyV1, OverlapNotifyGlobalData>
        + Dispatch<ZcosmicOverlapNotifyV1, ()>
        + Dispatch<ZcosmicOverlapNotificationV1, ()>
        + OverlapNotifyHandler
        + 'static,
{
    fn bind(
        state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: smithay::reexports::wayland_server::New<ZcosmicOverlapNotifyV1>,
        _global_data: &OverlapNotifyGlobalData,
        data_init: &mut smithay::reexports::wayland_server::DataInit<'_, D>,
    ) {
        let instance = data_init.init(resource, ());
        state.overlap_notify_state().instances.push(instance);
    }

    fn can_view(client: Client, global_data: &OverlapNotifyGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ZcosmicOverlapNotifyV1, (), D> for OverlapNotifyState
where
    D: GlobalDispatch<ZcosmicOverlapNotifyV1, OverlapNotifyGlobalData>
        + Dispatch<ZcosmicOverlapNotifyV1, ()>
        + Dispatch<ZcosmicOverlapNotificationV1, ()>
        + OverlapNotifyHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &ZcosmicOverlapNotifyV1,
        request: <ZcosmicOverlapNotifyV1 as smithay::reexports::wayland_server::Resource>::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut smithay::reexports::wayland_server::DataInit<'_, D>,
    ) {
        if let zcosmic_overlap_notify_v1::Request::NotifyOnOverlap {
            overlap_notification,
            layer_surface,
        } = request
        {
            let notification = data_init.init(overlap_notification, ());
            if let Some(surface) = state.layer_surface_from_resource(layer_surface) {
                let mut data = surface
                    .user_data()
                    .get_or_insert_threadsafe(LayerOverlapNotificationData::default)
                    .lock()
                    .unwrap();
                data.add_notification(notification);
            }
        }
    }

    fn destroyed(
        state: &mut D,
        _client: wayland_backend::server::ClientId,
        resource: &ZcosmicOverlapNotifyV1,
        _data: &(),
    ) {
        let overlap_state = state.overlap_notify_state();
        overlap_state.instances.retain(|i| i != resource);
    }
}

impl<D> Dispatch<ZcosmicOverlapNotificationV1, (), D> for OverlapNotifyState
where
    D: GlobalDispatch<ZcosmicOverlapNotifyV1, OverlapNotifyGlobalData>
        + Dispatch<ZcosmicOverlapNotifyV1, ()>
        + Dispatch<ZcosmicOverlapNotificationV1, ()>
        + OverlapNotifyHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _resource: &ZcosmicOverlapNotificationV1,
        _request: <ZcosmicOverlapNotificationV1 as Resource>::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        _data_init: &mut smithay::reexports::wayland_server::DataInit<'_, D>,
    ) {
        {}
    }
}

macro_rules! delegate_overlap_notify {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::overlap_notify::v1::server::zcosmic_overlap_notify_v1::ZcosmicOverlapNotifyV1: $crate::wayland::protocols::overlap_notify::OverlapNotifyGlobalData
        ] => $crate::wayland::protocols::overlap_notify::OverlapNotifyState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::overlap_notify::v1::server::zcosmic_overlap_notify_v1::ZcosmicOverlapNotifyV1: ()
        ] => $crate::wayland::protocols::overlap_notify::OverlapNotifyState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::overlap_notify::v1::server::zcosmic_overlap_notification_v1::ZcosmicOverlapNotificationV1: ()
        ] => $crate::wayland::protocols::overlap_notify::OverlapNotifyState);
    };
}
pub(crate) use delegate_overlap_notify;
