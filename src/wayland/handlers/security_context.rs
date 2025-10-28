use crate::state::{ClientState, State};
use smithay::{
    backend::drm::DrmNode,
    delegate_security_context,
    wayland::security_context::{
        SecurityContext, SecurityContextHandler, SecurityContextListenerSource,
    },
    xwayland::XWaylandClientData,
};
use std::sync::Arc;
use tracing::warn;

impl SecurityContextHandler for State {
    fn context_created(
        &mut self,
        source: SecurityContextListenerSource,
        security_context: SecurityContext,
    ) {
        self.common
            .event_loop_handle
            .insert_source(source, move |client_stream, _, state| {
                let client_data = state
                    .common
                    .display_handle
                    .backend_handle()
                    .get_client_data(security_context.creator_client_id.clone())
                    .ok();

                let new_state = state.new_client_state();

                let drm_node = client_data
                    .as_ref()
                    .and_then(|data| data.downcast_ref::<ClientState>())
                    .and_then(|data| data.advertised_drm_node)
                    .or_else(|| {
                        client_data
                            .as_ref()
                            .and_then(|data| data.downcast_ref::<XWaylandClientData>())
                            .and_then(|data| data.user_data().get::<DrmNode>().cloned())
                    })
                    .or(new_state.advertised_drm_node);

                if let Err(err) = state.common.display_handle.insert_client(
                    client_stream,
                    Arc::new(ClientState {
                        security_context: Some(security_context.clone()),
                        advertised_drm_node: drm_node,
                        ..new_state
                    }),
                ) {
                    warn!(?err, "Error adding wayland client");
                };
            })
            .expect("Failed to init the wayland socket source.");
    }
}
delegate_security_context!(State);
