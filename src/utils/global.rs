use std::time::Duration;

use calloop::{
    LoopHandle,
    timer::{TimeoutAction, Timer},
};
use smithay::reexports::wayland_server::DisplayHandle;
use wayland_backend::server::GlobalId;

pub fn remove_global_with_timer<D: 'static>(
    dh: &DisplayHandle,
    event_loop_handle: &LoopHandle<D>,
    id: GlobalId,
) {
    dh.disable_global::<D>(id.clone());
    let source = Timer::from_duration(Duration::from_secs(5));
    let dh = dh.clone();
    let res = event_loop_handle.insert_source(source, move |_, _, _state| {
        dh.remove_global::<D>(id.clone());
        TimeoutAction::Drop
    });
    if let Err(err) = res {
        tracing::error!(
            "failed to insert timer source to destroy output global: {}",
            err
        );
    }
}
