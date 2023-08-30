use crate::state::{ClientState, State};
use smithay::{
    delegate_security_context,
    wayland::security_context::{
        SecurityContext, SecurityContextHandler, SecurityContextListenerSource,
    },
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
            .insert_source(source, move |client_stream, _, data| {
                if let Err(err) = data.display.handle().insert_client(
                    client_stream,
                    Arc::new(ClientState {
                        security_context: Some(security_context.clone()),
                        ..data.state.new_client_state()
                    }),
                ) {
                    warn!(?err, "Error adding wayland client");
                };
            })
            .expect("Failed to init the wayland socket source.");
    }
}
delegate_security_context!(State);
