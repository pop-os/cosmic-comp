use reis::calloop::EisListenerSource;
use reis::eis;
use smithay::reexports::reis;

use smithay::backend::libei::EiInput;
use smithay::reexports::calloop;

use crate::state::State;

pub fn listen_eis(handle: &calloop::LoopHandle<'static, State>) {
    let path = reis::default_socket_path().unwrap();
    std::fs::remove_file(&path); // XXX in use?
    let listener = eis::Listener::bind(&path).unwrap();
    let listener_source = EisListenerSource::new(listener);

    std::env::set_var("LIBEI_SOCKET", path);

    let handle_clone = handle.clone();
    handle
        .insert_source(listener_source, move |context, _, _| {
            let source = EiInput::new(context);
            handle_clone
                .insert_source(source, |event, _, data| {
                    data.process_input_event(event, true);
                })
                .unwrap();
            Ok(calloop::PostAction::Continue)
        })
        .unwrap();
}
