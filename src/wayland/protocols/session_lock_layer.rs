use cosmic_protocols::session_lock_layer::v1::server::cosmic_session_lock_layer_manager_v1;
use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, backend::GlobalId,
        protocol::wl_surface,
    },
    wayland::{
        Dispatch2, GlobalDispatch2,
        compositor::{Cacheable, with_states},
        shell::wlr_layer::WlrLayerShellHandler,
    },
};

struct SessionLockLayerData;

#[derive(Debug)]
pub struct SessionLockLayerState {
    global: GlobalId,
}

impl SessionLockLayerState {
    pub fn new<D, F>(dh: &DisplayHandle, client_filter: F) -> Self
    where
        F: for<'a> Fn(&'a Client) -> bool + Clone + Send + Sync + 'static,
        D: GlobalDispatch<
                cosmic_session_lock_layer_manager_v1::CosmicSessionLockLayerManagerV1,
                SessionLockLayerGlobalData,
            > + 'static,
    {
        let global = dh
            .create_global::<D, cosmic_session_lock_layer_manager_v1::CosmicSessionLockLayerManagerV1, _>(
                1,
                SessionLockLayerGlobalData {
                    filter: Box::new(client_filter.clone()),
                },
            );
        Self { global }
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

#[doc(hidden)]
pub struct SessionLockLayerGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

impl<D> GlobalDispatch2<cosmic_session_lock_layer_manager_v1::CosmicSessionLockLayerManagerV1, D>
    for SessionLockLayerGlobalData
where
    D: Dispatch<
            cosmic_session_lock_layer_manager_v1::CosmicSessionLockLayerManagerV1,
            SessionLockLayerData,
        >,
{
    fn bind(
        &self,
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<cosmic_session_lock_layer_manager_v1::CosmicSessionLockLayerManagerV1>,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, SessionLockLayerData);
    }

    fn can_view(&self, client: &Client) -> bool {
        (self.filter)(client)
    }
}

#[derive(Clone, Copy, Default)]
struct SessionLockLayerSurfaceData {
    show_on_lock: bool,
}

impl Cacheable for SessionLockLayerSurfaceData {
    fn commit(&mut self, _dh: &DisplayHandle) -> Self {
        *self
    }

    fn merge_into(self, into: &mut Self, _dh: &DisplayHandle) {
        *into = self;
    }
}

pub fn layer_show_on_lock(wl_surface: &wl_surface::WlSurface) -> bool {
    with_states(wl_surface, |states| {
        if states.cached_state.has::<SessionLockLayerSurfaceData>() {
            let mut state = states.cached_state.get::<SessionLockLayerSurfaceData>();
            state.current().show_on_lock
        } else {
            false
        }
    })
}

impl<D> Dispatch2<cosmic_session_lock_layer_manager_v1::CosmicSessionLockLayerManagerV1, D>
    for SessionLockLayerData
where
    D: WlrLayerShellHandler,
{
    fn request(
        &self,
        state: &mut D,
        _: &Client,
        _: &cosmic_session_lock_layer_manager_v1::CosmicSessionLockLayerManagerV1,
        request: cosmic_session_lock_layer_manager_v1::Request,
        _: &DisplayHandle,
        _: &mut DataInit<'_, D>,
    ) {
        let (layer, value) = match request {
            cosmic_session_lock_layer_manager_v1::Request::SetShowOnLock { layer } => (layer, true),
            cosmic_session_lock_layer_manager_v1::Request::UnsetShowOnLock { layer } => {
                (layer, false)
            }
            cosmic_session_lock_layer_manager_v1::Request::Destroy => {
                return;
            }
            _ => unreachable!(),
        };

        if let Some(layer) = state
            .shell_state()
            .layer_surfaces()
            .find(|surface| surface.shell_surface() == &layer)
        {
            with_states(layer.wl_surface(), |states| {
                let mut state = states.cached_state.get::<SessionLockLayerSurfaceData>();
                state.pending().show_on_lock = value;
            });
        }
    }
}
