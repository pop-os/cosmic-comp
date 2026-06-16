use std::os::unix::net::UnixStream;

use reis::eis;
use smithay::reexports::reis;

use smithay::backend::libei::{EiInput, EiInputEvent};
use smithay::reexports::calloop;

use crate::config::xkb_config_to_wl;
use crate::input::InputBackendId;
use crate::state::State;

pub fn setup_ei(
    handle: &calloop::LoopHandle<'static, State>,
) -> calloop::channel::Sender<UnixStream> {
    let (sender, channel) = calloop::channel::channel::<UnixStream>();
    let handle_clone = handle.clone();
    handle
        .insert_source(channel, move |event, _, _| {
            let calloop::channel::Event::Msg(stream) = event else {
                return;
            };
            let context = match eis::Context::new(stream) {
                Ok(context) => context,
                Err(err) => {
                    tracing::error!("Failed to create EI context: {}", err);
                    return;
                }
            };
            let source = EiInput::new(context);
            if let Err(err) =
                handle_clone.insert_source(source, |event, connection, data| match event {
                    EiInputEvent::Connected => {
                        let seat = connection.add_seat("default");
                        let conf = data.common.config.xkb_config();
                        let _ = seat.add_keyboard("virtual keyboard", xkb_config_to_wl(&conf));
                        seat.add_pointer("virtual pointer");
                        seat.add_pointer_absolute("virtual absolute pointer");
                        seat.add_touch("virtual touch");
                    }
                    EiInputEvent::Disconnected => {}
                    EiInputEvent::Event(event) => {
                        let backend_id = InputBackendId::Ei(connection.eis_connection().clone());
                        data.process_input_event(event, backend_id);
                    }
                })
            {
                tracing::error!("Failed to insert EI input source: {}", err);
            }
        })
        .expect("Failed to insert EI channel source into the event loop");

    sender
}
