use crate::state::State;
use smithay::{
    delegate_ext_data_control,
    wayland::selection::ext_data_control::{DataControlHandler, DataControlState},
};

impl DataControlHandler for State {
    fn data_control_state(&mut self) -> &mut DataControlState {
        &mut self.common.ext_data_control_state
    }
}

delegate_ext_data_control!(State);
