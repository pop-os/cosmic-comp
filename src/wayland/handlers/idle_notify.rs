use smithay::{delegate_idle_notify, wayland::idle_notify::IdleNotifierHandler};

use crate::state::State;

impl IdleNotifierHandler for State {
    fn idle_notifier_state(
        &mut self,
    ) -> &mut smithay::wayland::idle_notify::IdleNotifierState<Self> {
        &mut self.common.idle_notifier_state
    }
}
delegate_idle_notify!(State);
