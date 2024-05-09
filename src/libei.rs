use reis::calloop::EisListenerSource;
use reis::eis;
use smithay::reexports::reis;

use smithay::backend::libei::{EiInput, EiInputEvent};
use smithay::input::keyboard::XkbConfig;
use smithay::reexports::calloop;

use crate::state::State;

pub fn listen_eis(handle: &calloop::LoopHandle<'static, State>) {
    let listener = match eis::Listener::bind_auto() {
        Ok(listener) => listener,
        Err(err) => {
            tracing::error!("Failed to bind EI listener socket: {}", err);
            return;
        }
    };

    unsafe { std::env::set_var("LIBEI_SOCKET", listener.path()) };

    let listener_source = EisListenerSource::new(listener);
    let handle_clone = handle.clone();
    handle
        .insert_source(listener_source, move |context, _, _| {
            let source = EiInput::new(context);
            handle_clone
                .insert_source(source, |event, connection, data| match event {
                    EiInputEvent::Connected => {
                        let seat = connection.add_seat("default");
                        // TODO config
                        let _ = seat.add_keyboard("virtual keyboard", XkbConfig::default());
                        seat.add_pointer("virtual pointer");
                        seat.add_pointer_absolute("virtual absoulte pointer");
                        seat.add_touch("virtual touch");
                    }
                    EiInputEvent::Disconnected => {}
                    EiInputEvent::Event(event) => {
                        data.process_input_event(event);
                    }
                })
                .unwrap();
            Ok(calloop::PostAction::Continue)
        })
        .unwrap();
}
