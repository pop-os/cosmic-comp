use cosmic_protocols::session_lock_layer::v1::server::cosmic_session_lock_layer_v1;
use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, backend::GlobalId,
        protocol::wl_surface,
    },
    wayland::{
        Dispatch2, GlobalDispatch2, compositor::with_states, shell::wlr_layer::WlrLayerShellHandler,
    },
};
use std::sync::atomic::{AtomicBool, Ordering};

struct SessionLockLayerData;

// state type

// global dispatch

// TODO generic over D

pub fn layer_show_on_lock(wl_surface: &wl_surface::WlSurface) -> bool {
    with_states(wl_surface, |states| {
        if let Some(data) = states.data_map.get::<SessionLockLayerSurfaceData>() {
            data.show_on_lock.load(Ordering::SeqCst)
        } else {
            false
        }
    })
}

#[derive(Debug)]
pub struct SessionLockLayerState {
    global: GlobalId,
}

impl SessionLockLayerState {
    pub fn new<D>(dh: &DisplayHandle) -> Self
    where
        D: GlobalDispatch<
                cosmic_session_lock_layer_v1::CosmicSessionLockLayerV1,
                SessionLockLayerGlobalData,
            > + 'static,
    {
        let global = dh
            .create_global::<D, cosmic_session_lock_layer_v1::CosmicSessionLockLayerV1, _>(
                1,
                SessionLockLayerGlobalData,
            );
        Self { global }
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

struct SessionLockLayerGlobalData;

impl<D> GlobalDispatch2<cosmic_session_lock_layer_v1::CosmicSessionLockLayerV1, D>
    for SessionLockLayerGlobalData
where
    D: Dispatch<cosmic_session_lock_layer_v1::CosmicSessionLockLayerV1, SessionLockLayerData>,
{
    fn bind(
        &self,
        state: &mut D,
        handle: &DisplayHandle,
        client: &Client,
        resource: New<cosmic_session_lock_layer_v1::CosmicSessionLockLayerV1>,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, SessionLockLayerData);
    }

    // TODO can_view
}

#[derive(Default)]
struct SessionLockLayerSurfaceData {
    show_on_lock: AtomicBool,
}

impl<D> Dispatch2<cosmic_session_lock_layer_v1::CosmicSessionLockLayerV1, D>
    for SessionLockLayerData
where
    D: WlrLayerShellHandler,
{
    fn request(
        &self,
        state: &mut D,
        _: &Client,
        _: &cosmic_session_lock_layer_v1::CosmicSessionLockLayerV1,
        request: cosmic_session_lock_layer_v1::Request,
        _: &DisplayHandle,
        _: &mut DataInit<'_, D>,
    ) {
        let (layer, value) = match request {
            cosmic_session_lock_layer_v1::Request::SetShowOnLock { layer } => (layer, true),
            cosmic_session_lock_layer_v1::Request::UnsetShowOnLock { layer } => (layer, false),
            _ => unreachable!(),
        };

        if let Some(layer) = state
            .shell_state()
            .layer_surfaces()
            .find(|surface| surface.shell_surface() == &layer)
        {
            with_states(layer.wl_surface(), |states| {
                let data = states
                    .data_map
                    .get_or_insert_threadsafe(SessionLockLayerSurfaceData::default);
                data.show_on_lock.store(value, Ordering::SeqCst);
            });
        }
    }
}
