// SPDX-License-Identifier: GPL-3.0-only

use cosmic_protocols::keymap::v1::server::{
    zcosmic_keymap_manager_v1::{self, ZcosmicKeymapManagerV1},
    zcosmic_keymap_v1::{self, ZcosmicKeymapV1},
};
use smithay::{
    input::{
        keyboard::{KeyboardHandle, Layout},
        SeatHandler,
    },
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource,
    },
};
use std::mem;
use wayland_backend::server::{ClientId, GlobalId};

pub trait KeymapHandler {
    fn keymap_state(&mut self) -> &mut KeymapState;
}

#[derive(Debug)]
pub struct KeymapState {
    pub global: GlobalId,
    keymaps: Vec<(ZcosmicKeymapV1, Option<Layout>)>,
}

impl KeymapState {
    pub fn new<D, F>(dh: &DisplayHandle, client_filter: F) -> Self
    where
        D: GlobalDispatch<ZcosmicKeymapManagerV1, KeymapGlobalData> + 'static,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        let global = dh.create_global::<D, ZcosmicKeymapManagerV1, _>(
            1,
            KeymapGlobalData {
                filter: Box::new(client_filter),
            },
        );
        KeymapState {
            global,
            keymaps: Vec::new(),
        }
    }

    pub fn refresh<D>(state: &mut D)
    where
        D: SeatHandler + KeymapHandler + 'static,
    {
        let mut keymaps = mem::take(&mut state.keymap_state().keymaps);
        for (keymap, last_layout) in &mut keymaps {
            if let Some(data) = keymap.data::<KeymapUserData<D>>() {
                if let Some(handle) = &data.handle {
                    let active_layout = handle.with_xkb_state(state, |context| {
                        context.xkb().lock().unwrap().active_layout()
                    });
                    if *last_layout != Some(active_layout) {
                        keymap.group(active_layout.0);
                        *last_layout = Some(active_layout);
                    }
                }
            }
        }
        state.keymap_state().keymaps = keymaps;
    }
}

pub struct KeymapGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

impl<D> GlobalDispatch<ZcosmicKeymapManagerV1, KeymapGlobalData, D> for KeymapState
where
    D: GlobalDispatch<ZcosmicKeymapManagerV1, KeymapGlobalData>
        + Dispatch<ZcosmicKeymapManagerV1, ()>
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<ZcosmicKeymapManagerV1>,
        _global_data: &KeymapGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }

    fn can_view(client: Client, global_data: &KeymapGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ZcosmicKeymapManagerV1, (), D> for KeymapState
where
    D: Dispatch<ZcosmicKeymapManagerV1, ()>,
    D: Dispatch<ZcosmicKeymapV1, KeymapUserData<D>>,
    D: 'static,
    D: SeatHandler,
    D: KeymapHandler,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &ZcosmicKeymapManagerV1,
        request: zcosmic_keymap_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_keymap_manager_v1::Request::GetKeymap { keymap, keyboard } => {
                let handle = KeyboardHandle::<D>::from_resource(&keyboard);
                let active_layout = handle.as_ref().map(|handle| {
                    handle.with_xkb_state(state, |context| {
                        context.xkb().lock().unwrap().active_layout()
                    })
                });
                let keymap = data_init.init(keymap, KeymapUserData { handle });
                if let Some(layout) = active_layout {
                    keymap.group(layout.0);
                }
                state.keymap_state().keymaps.push((keymap, active_layout));
            }
            zcosmic_keymap_manager_v1::Request::Destroy => {}
            _ => unreachable!(),
        }
    }
}

#[doc(hidden)]
pub struct KeymapUserData<D: SeatHandler> {
    handle: Option<KeyboardHandle<D>>,
}

impl<D> Dispatch<ZcosmicKeymapV1, KeymapUserData<D>, D> for KeymapState
where
    D: Dispatch<ZcosmicKeymapV1, KeymapUserData<D>>,
    D: 'static,
    D: SeatHandler,
    D: KeymapHandler,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &ZcosmicKeymapV1,
        request: zcosmic_keymap_v1::Request,
        data: &KeymapUserData<D>,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_keymap_v1::Request::SetGroup { group } => {
                if let Some(handle) = data.handle.as_ref() {
                    handle.with_xkb_state(state, |mut context| {
                        context.set_layout(Layout(group));
                    });
                }
            }
            zcosmic_keymap_v1::Request::Destroy => {}
            _ => unreachable!(),
        }
    }

    fn destroyed(
        state: &mut D,
        _client: ClientId,
        keymap: &ZcosmicKeymapV1,
        _data: &KeymapUserData<D>,
    ) {
        let keymaps = &mut state.keymap_state().keymaps;
        if let Some(idx) = keymaps.iter().position(|(x, _)| x == keymap) {
            keymaps.remove(idx);
        }
    }
}

macro_rules! delegate_keymap {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::keymap::v1::server::zcosmic_keymap_manager_v1::ZcosmicKeymapManagerV1: $crate::wayland::protocols::keymap::KeymapGlobalData
        ] => $crate::wayland::protocols::keymap::KeymapState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::keymap::v1::server::zcosmic_keymap_manager_v1::ZcosmicKeymapManagerV1: ()
        ] => $crate::wayland::protocols::keymap::KeymapState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::keymap::v1::server::zcosmic_keymap_v1::ZcosmicKeymapV1: $crate::wayland::protocols::keymap::KeymapUserData<$ty>
        ] => $crate::wayland::protocols::keymap::KeymapState);
    };
}
pub(crate) use delegate_keymap;
