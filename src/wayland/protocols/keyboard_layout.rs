// SPDX-License-Identifier: GPL-3.0-only

use cosmic_protocols::keyboard_layout::v1::server::{
    zcosmic_keyboard_layout_manager_v1::{self, ZcosmicKeyboardLayoutManagerV1},
    zcosmic_keyboard_layout_v1::{self, ZcosmicKeyboardLayoutV1},
};
use smithay::{
    input::{
        SeatHandler,
        keyboard::{KeyboardHandle, Layout},
    },
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource,
    },
};
use std::mem;
use wayland_backend::server::{ClientId, GlobalId};

pub trait KeyboardLayoutHandler {
    fn keyboard_layout_state(&mut self) -> &mut KeyboardLayoutState;
}

#[derive(Debug)]
pub struct KeyboardLayoutState {
    pub global: GlobalId,
    keyboard_layouts: Vec<(ZcosmicKeyboardLayoutV1, Option<Layout>)>,
}

impl KeyboardLayoutState {
    pub fn new<D, F>(dh: &DisplayHandle, client_filter: F) -> Self
    where
        D: GlobalDispatch<ZcosmicKeyboardLayoutManagerV1, KeymapGlobalData> + 'static,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        let global = dh.create_global::<D, ZcosmicKeyboardLayoutManagerV1, _>(
            1,
            KeymapGlobalData {
                filter: Box::new(client_filter),
            },
        );
        KeyboardLayoutState {
            global,
            keyboard_layouts: Vec::new(),
        }
    }

    pub fn refresh<D>(state: &mut D)
    where
        D: SeatHandler + KeyboardLayoutHandler + 'static,
    {
        let mut keyboard_layouts = mem::take(&mut state.keyboard_layout_state().keyboard_layouts);
        for (keyboard_layout, last_layout) in &mut keyboard_layouts {
            if let Some(data) = keyboard_layout.data::<KeymapUserData<D>>() {
                if let Some(handle) = &data.handle {
                    let active_layout = handle.with_xkb_state(state, |context| {
                        context.xkb().lock().unwrap().active_layout()
                    });
                    if *last_layout != Some(active_layout) {
                        keyboard_layout.group(active_layout.0);
                        *last_layout = Some(active_layout);
                    }
                }
            }
        }
        state.keyboard_layout_state().keyboard_layouts = keyboard_layouts;
    }
}

pub struct KeymapGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

impl<D> GlobalDispatch<ZcosmicKeyboardLayoutManagerV1, KeymapGlobalData, D> for KeyboardLayoutState
where
    D: GlobalDispatch<ZcosmicKeyboardLayoutManagerV1, KeymapGlobalData>
        + Dispatch<ZcosmicKeyboardLayoutManagerV1, ()>
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<ZcosmicKeyboardLayoutManagerV1>,
        _global_data: &KeymapGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }

    fn can_view(client: Client, global_data: &KeymapGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ZcosmicKeyboardLayoutManagerV1, (), D> for KeyboardLayoutState
where
    D: Dispatch<ZcosmicKeyboardLayoutManagerV1, ()>,
    D: Dispatch<ZcosmicKeyboardLayoutV1, KeymapUserData<D>>,
    D: 'static,
    D: SeatHandler,
    D: KeyboardLayoutHandler,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &ZcosmicKeyboardLayoutManagerV1,
        request: zcosmic_keyboard_layout_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_keyboard_layout_manager_v1::Request::GetKeyboardLayout {
                keyboard_layout,
                keyboard,
            } => {
                let handle = KeyboardHandle::<D>::from_resource(&keyboard);
                let active_layout = handle.as_ref().map(|handle| {
                    handle.with_xkb_state(state, |context| {
                        context.xkb().lock().unwrap().active_layout()
                    })
                });
                let keyboard_layout = data_init.init(keyboard_layout, KeymapUserData { handle });
                if let Some(layout) = active_layout {
                    keyboard_layout.group(layout.0);
                }
                state
                    .keyboard_layout_state()
                    .keyboard_layouts
                    .push((keyboard_layout, active_layout));
            }
            zcosmic_keyboard_layout_manager_v1::Request::Destroy => {}
            _ => unreachable!(),
        }
    }
}

#[doc(hidden)]
pub struct KeymapUserData<D: SeatHandler> {
    handle: Option<KeyboardHandle<D>>,
}

impl<D> Dispatch<ZcosmicKeyboardLayoutV1, KeymapUserData<D>, D> for KeyboardLayoutState
where
    D: Dispatch<ZcosmicKeyboardLayoutV1, KeymapUserData<D>>,
    D: 'static,
    D: SeatHandler,
    D: KeyboardLayoutHandler,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &ZcosmicKeyboardLayoutV1,
        request: zcosmic_keyboard_layout_v1::Request,
        data: &KeymapUserData<D>,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_keyboard_layout_v1::Request::SetGroup { group } => {
                if let Some(handle) = data.handle.as_ref() {
                    handle.with_xkb_state(state, |mut context| {
                        context.set_layout(Layout(group));
                    });
                }
            }
            zcosmic_keyboard_layout_v1::Request::Destroy => {}
            _ => unreachable!(),
        }
    }

    fn destroyed(
        state: &mut D,
        _client: ClientId,
        keyboard_layout: &ZcosmicKeyboardLayoutV1,
        _data: &KeymapUserData<D>,
    ) {
        let keyboard_layouts = &mut state.keyboard_layout_state().keyboard_layouts;
        if let Some(idx) = keyboard_layouts
            .iter()
            .position(|(x, _)| x == keyboard_layout)
        {
            keyboard_layouts.remove(idx);
        }
    }
}

macro_rules! delegate_keyboard_layout {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::keyboard_layout::v1::server::zcosmic_keyboard_layout_manager_v1::ZcosmicKeyboardLayoutManagerV1: $crate::wayland::protocols::keyboard_layout::KeymapGlobalData
        ] => $crate::wayland::protocols::keyboard_layout::KeyboardLayoutState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::keyboard_layout::v1::server::zcosmic_keyboard_layout_manager_v1::ZcosmicKeyboardLayoutManagerV1: ()
        ] => $crate::wayland::protocols::keyboard_layout::KeyboardLayoutState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::keyboard_layout::v1::server::zcosmic_keyboard_layout_v1::ZcosmicKeyboardLayoutV1: $crate::wayland::protocols::keyboard_layout::KeymapUserData<$ty>
        ] => $crate::wayland::protocols::keyboard_layout::KeyboardLayoutState);
    };
}
pub(crate) use delegate_keyboard_layout;
