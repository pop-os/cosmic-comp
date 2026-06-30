// SPDX-License-Identifier: GPL-3.0-only

use cosmic_protocols::a11y::v1::server::cosmic_a11y_manager_v1;
use smithay::reexports::wayland_server::{
    Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource,
};
use wayland_backend::{protocol::WEnum, server::GlobalId};

pub trait A11yHandler {
    fn a11y_state(&mut self) -> &mut A11yState;

    fn request_screen_magnifier(&mut self, enabled: bool);
    fn request_screen_invert(&mut self, inverted: bool);
    fn request_screen_filter_select(&mut self, filter: ColorFilter);
    fn request_screen_filter_state(&mut self, enabled: bool);
}

#[derive(Debug)]
pub struct A11yState {
    global: GlobalId,
    instances: Vec<cosmic_a11y_manager_v1::CosmicA11yManagerV1>,

    magnifier_state: bool,
    screen_inverted: bool,
    screen_filter: ColorFilter,
    screen_filter_state: bool,
}

struct Unknown;

fn protocol_to_color_filter(
    protocol: WEnum<cosmic_a11y_manager_v1::Filter>,
) -> Result<ColorFilter, Unknown> {
    match protocol {
        WEnum::Value(cosmic_a11y_manager_v1::Filter::Greyscale) => Ok(ColorFilter::Greyscale),
        WEnum::Value(cosmic_a11y_manager_v1::Filter::DaltonizeProtanopia) => {
            Ok(ColorFilter::Protanopia)
        }
        WEnum::Value(cosmic_a11y_manager_v1::Filter::DaltonizeDeuteranopia) => {
            Ok(ColorFilter::Deuteranopia)
        }
        WEnum::Value(cosmic_a11y_manager_v1::Filter::DaltonizeTritanopia) => {
            Ok(ColorFilter::Tritanopia)
        }
        WEnum::Unknown(_) | WEnum::Value(_) => Err(Unknown),
    }
}

fn color_filter_to_protocol(filter: ColorFilter) -> cosmic_a11y_manager_v1::Filter {
    match filter {
        ColorFilter::Greyscale => cosmic_a11y_manager_v1::Filter::Greyscale,
        ColorFilter::Protanopia => cosmic_a11y_manager_v1::Filter::DaltonizeProtanopia,
        ColorFilter::Deuteranopia => cosmic_a11y_manager_v1::Filter::DaltonizeDeuteranopia,
        ColorFilter::Tritanopia => cosmic_a11y_manager_v1::Filter::DaltonizeTritanopia,
    }
}

impl A11yState {
    pub fn new<D, F>(dh: &DisplayHandle, client_filter: F) -> A11yState
    where
        D: GlobalDispatch<cosmic_a11y_manager_v1::CosmicA11yManagerV1, A11yGlobalData> + 'static,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        let global = dh.create_global::<D, cosmic_a11y_manager_v1::CosmicA11yManagerV1, _>(
            3,
            A11yGlobalData {
                filter: Box::new(client_filter),
            },
        );
        A11yState {
            global,
            instances: Vec::new(),

            magnifier_state: false,
            screen_inverted: false,
            screen_filter: ColorFilter::Greyscale,
            screen_filter_state: false,
        }
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }

    pub fn screen_inverted(&self) -> bool {
        self.screen_inverted
    }

    pub fn set_screen_magnifier(&mut self, enabled: bool) {
        self.magnifier_state = enabled;

        for instance in &self.instances {
            instance.magnifier(if enabled {
                cosmic_a11y_manager_v1::ActiveState::Enabled
            } else {
                cosmic_a11y_manager_v1::ActiveState::Disabled
            });
        }
    }

    pub fn set_screen_inverted(&mut self, enabled: bool) {
        self.screen_inverted = enabled;
        self.send_screen_filter();
    }

    pub fn set_screen_filter(&mut self, state: ColorFilter) {
        self.screen_filter = state;
        self.send_screen_filter();
    }

    pub fn set_screen_filter_state(&mut self, enabled: bool) {
        self.screen_filter_state = enabled;
        self.send_screen_filter();
    }

    fn send_screen_filter(&self) {
        for instance in &self.instances {
            if instance.version() >= cosmic_a11y_manager_v1::EVT_SCREEN_FILTER2_SINCE {
                instance.screen_filter2(
                    if self.screen_inverted {
                        cosmic_a11y_manager_v1::ActiveState::Enabled
                    } else {
                        cosmic_a11y_manager_v1::ActiveState::Disabled
                    },
                    color_filter_to_protocol(self.screen_filter),
                    if self.screen_filter_state {
                        cosmic_a11y_manager_v1::ActiveState::Enabled
                    } else {
                        cosmic_a11y_manager_v1::ActiveState::Disabled
                    },
                );
            }
        }
    }

    fn send_deprecated(&self, client_id: wayland_backend::server::ClientId, message: String) {
        if let Some(instance) = &self
            .instances
            .iter()
            .find(|i| i.client().map_or(false, |client| client.id() == client_id))
        {
            instance.post_error(cosmic_a11y_manager_v1::Error::Deprecated, message);
        }
    }
}

pub struct A11yGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

impl<D> GlobalDispatch<cosmic_a11y_manager_v1::CosmicA11yManagerV1, A11yGlobalData, D> for A11yState
where
    D: GlobalDispatch<cosmic_a11y_manager_v1::CosmicA11yManagerV1, A11yGlobalData>
        + Dispatch<cosmic_a11y_manager_v1::CosmicA11yManagerV1, ()>
        + A11yHandler
        + 'static,
{
    fn bind(
        state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<cosmic_a11y_manager_v1::CosmicA11yManagerV1>,
        _global_data: &A11yGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        let instance = data_init.init(resource, ());
        let state = state.a11y_state();

        instance.magnifier(if state.magnifier_state {
            cosmic_a11y_manager_v1::ActiveState::Enabled
        } else {
            cosmic_a11y_manager_v1::ActiveState::Disabled
        });

        if instance.version() >= cosmic_a11y_manager_v1::EVT_SCREEN_FILTER2_SINCE {
            instance.screen_filter2(
                if state.screen_inverted {
                    cosmic_a11y_manager_v1::ActiveState::Enabled
                } else {
                    cosmic_a11y_manager_v1::ActiveState::Disabled
                },
                color_filter_to_protocol(state.screen_filter),
                if state.screen_filter_state {
                    cosmic_a11y_manager_v1::ActiveState::Enabled
                } else {
                    cosmic_a11y_manager_v1::ActiveState::Disabled
                },
            );
        }

        state.instances.push(instance);
    }

    fn can_view(client: Client, global_data: &A11yGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<cosmic_a11y_manager_v1::CosmicA11yManagerV1, (), D> for A11yState
where
    D: Dispatch<cosmic_a11y_manager_v1::CosmicA11yManagerV1, ()> + A11yHandler + 'static,
{
    fn request(
        state: &mut D,
        client: &Client,
        _resource: &cosmic_a11y_manager_v1::CosmicA11yManagerV1,
        request: <cosmic_a11y_manager_v1::CosmicA11yManagerV1 as smithay::reexports::wayland_server::Resource>::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            cosmic_a11y_manager_v1::Request::SetMagnifier { active } => {
                let enabled = active
                    .into_result()
                    .unwrap_or(cosmic_a11y_manager_v1::ActiveState::Disabled)
                    == cosmic_a11y_manager_v1::ActiveState::Enabled;
                if enabled != state.a11y_state().magnifier_state {
                    state.request_screen_magnifier(enabled);
                }
            }
            cosmic_a11y_manager_v1::Request::SetScreenFilter { .. } => {
                state.a11y_state().send_deprecated(
                    client.id(),
                    "Version 3 or higher was bound. Use `set_screen_filter2` instead.".to_string(),
                );
            }
            cosmic_a11y_manager_v1::Request::SetScreenFilter2 {
                inverted,
                filter,
                filter_state,
            } => {
                let inverted = inverted
                    .into_result()
                    .unwrap_or(cosmic_a11y_manager_v1::ActiveState::Disabled)
                    == cosmic_a11y_manager_v1::ActiveState::Enabled;
                let filter = protocol_to_color_filter(filter);
                let filter_state = filter_state
                    .into_result()
                    .unwrap_or(cosmic_a11y_manager_v1::ActiveState::Disabled)
                    == cosmic_a11y_manager_v1::ActiveState::Enabled;
                let mut filter_changed = false;

                if inverted != state.a11y_state().screen_inverted {
                    state.request_screen_invert(inverted);
                }

                if let Ok(filter) = filter
                    && filter != state.a11y_state().screen_filter
                {
                    filter_changed = true;
                    state.request_screen_filter_select(filter);
                }

                if filter_state != state.a11y_state().screen_filter_state
                    || (filter_state == true && filter_changed)
                {
                    state.request_screen_filter_state(filter_state);
                }
            }
            _ => unreachable!(),
        }
    }

    fn destroyed(
        state: &mut D,
        _client: wayland_backend::server::ClientId,
        resource: &cosmic_a11y_manager_v1::CosmicA11yManagerV1,
        _data: &(),
    ) {
        state.a11y_state().instances.retain(|i| i != resource);
    }
}

macro_rules! delegate_a11y {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::a11y::v1::server::cosmic_a11y_manager_v1::CosmicA11yManagerV1: $crate::wayland::protocols::a11y::A11yGlobalData
        ] => $crate::wayland::protocols::a11y::A11yState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::a11y::v1::server::cosmic_a11y_manager_v1::CosmicA11yManagerV1: ()
        ] => $crate::wayland::protocols::a11y::A11yState);
    };
}
pub(crate) use delegate_a11y;

use crate::config::ColorFilter;
