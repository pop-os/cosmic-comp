// SPDX-License-Identifier: GPL-3.0-only

use smithay::reexports::{
    wayland_protocols_wlr::output_power_management::v1::server::{
        zwlr_output_power_manager_v1::{self, ZwlrOutputPowerManagerV1},
        zwlr_output_power_v1::{self, ZwlrOutputPowerV1},
    },
    wayland_server::{
        backend::GlobalId, protocol::wl_output::WlOutput, Client, DataInit, Dispatch,
        DisplayHandle, GlobalDispatch, New, Resource, Weak,
    },
};
use wayland_backend::protocol::WEnum;

pub trait OutputPowerHandler {
    fn set_dpms(&mut self, output: &WlOutput, on: bool);
}

pub struct OutputPowerState {
    global: GlobalId,
}

impl OutputPowerState {
    pub fn new<D, F>(dh: &DisplayHandle, client_filter: F) -> OutputPowerState
    where
        D: GlobalDispatch<ZwlrOutputPowerManagerV1, OutputPowerManagerGlobalData> + 'static,
        F: for<'a> Fn(&'a Client) -> bool + Clone + Send + Sync + 'static,
    {
        let global = dh.create_global::<D, ZwlrOutputPowerManagerV1, _>(
            1,
            OutputPowerManagerGlobalData {
                filter: Box::new(client_filter.clone()),
            },
        );

        OutputPowerState { global }
    }

    pub fn send_dpms(&self, _on: bool) {
        // TODO
    }
}

pub struct OutputPowerManagerGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

pub struct OutputPowerData {
    output: Weak<WlOutput>,
}

impl<D> GlobalDispatch<ZwlrOutputPowerManagerV1, OutputPowerManagerGlobalData, D>
    for OutputPowerState
where
    D: GlobalDispatch<ZwlrOutputPowerManagerV1, OutputPowerManagerGlobalData>
        + Dispatch<ZwlrOutputPowerManagerV1, ()>
        + 'static,
{
    fn bind(
        _state: &mut D,
        _dh: &DisplayHandle,
        _client: &Client,
        resource: New<ZwlrOutputPowerManagerV1>,
        _global_data: &OutputPowerManagerGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }

    fn can_view(client: Client, global_data: &OutputPowerManagerGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ZwlrOutputPowerManagerV1, (), D> for OutputPowerState
where
    D: GlobalDispatch<ZwlrOutputPowerManagerV1, OutputPowerManagerGlobalData>
        + Dispatch<ZwlrOutputPowerManagerV1, ()>
        + Dispatch<ZwlrOutputPowerV1, OutputPowerData>
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _obj: &ZwlrOutputPowerManagerV1,
        request: zwlr_output_power_manager_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zwlr_output_power_manager_v1::Request::GetOutputPower { id, output } => {
                data_init.init(
                    id,
                    OutputPowerData {
                        output: output.downgrade(),
                    },
                );
            }
            zwlr_output_power_manager_v1::Request::Destroy => {}
            _ => unreachable!(),
        }
    }
}

impl<D> Dispatch<ZwlrOutputPowerV1, OutputPowerData, D> for OutputPowerState
where
    D: Dispatch<ZwlrOutputPowerV1, OutputPowerData> + OutputPowerHandler + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _obj: &ZwlrOutputPowerV1,
        request: zwlr_output_power_v1::Request,
        data: &OutputPowerData,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zwlr_output_power_v1::Request::SetMode { mode } => {
                if let Ok(output) = data.output.upgrade() {
                    let on = match mode {
                        WEnum::Value(zwlr_output_power_v1::Mode::On) => true,
                        WEnum::Value(zwlr_output_power_v1::Mode::Off) => false,
                        _ => {
                            return;
                        }
                    };
                    state.set_dpms(&output, on);
                }
            }
            zwlr_output_power_v1::Request::Destroy => {}
            _ => unreachable!(),
        }
    }
}

macro_rules! delegate_output_power {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::output_power_management::v1::server::zwlr_output_power_manager_v1::ZwlrOutputPowerManagerV1: $crate::wayland::protocols::output_power::OutputPowerManagerGlobalData
        ] => $crate::wayland::protocols::output_power::OutputPowerState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::output_power_management::v1::server::zwlr_output_power_manager_v1::ZwlrOutputPowerManagerV1: ()
        ] => $crate::wayland::protocols::output_power::OutputPowerState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::output_power_management::v1::server::zwlr_output_power_v1::ZwlrOutputPowerV1: $crate::wayland::protocols::output_power::OutputPowerData
        ] => $crate::wayland::protocols::output_power::OutputPowerState);
    };
}
pub(crate) use delegate_output_power;
