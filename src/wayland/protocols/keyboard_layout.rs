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
    wayland::{Dispatch2, GlobalDispatch2},
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
        D: GlobalDispatch<ZcosmicKeyboardLayoutManagerV1, LayoutGlobalData> + 'static,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        let global = dh.create_global::<D, ZcosmicKeyboardLayoutManagerV1, _>(
            1,
            LayoutGlobalData {
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
            if let Some(data) = keyboard_layout.data::<LayoutUserData<D>>() {
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

pub struct LayoutGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

struct LayoutManagerData;

impl<D> GlobalDispatch2<ZcosmicKeyboardLayoutManagerV1, D> for LayoutGlobalData
where
    D: Dispatch<ZcosmicKeyboardLayoutManagerV1, LayoutManagerData> + 'static,
{
    fn bind(
        &self,
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<ZcosmicKeyboardLayoutManagerV1>,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, LayoutManagerData);
    }

    fn can_view(&self, client: &Client) -> bool {
        (self.filter)(client)
    }
}

impl<D> Dispatch2<ZcosmicKeyboardLayoutManagerV1, D> for LayoutManagerData
where
    D: Dispatch<ZcosmicKeyboardLayoutV1, LayoutUserData<D>>,
    D: 'static,
    D: SeatHandler,
    D: KeyboardLayoutHandler,
{
    fn request(
        &self,
        state: &mut D,
        _client: &Client,
        _resource: &ZcosmicKeyboardLayoutManagerV1,
        request: zcosmic_keyboard_layout_manager_v1::Request,
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
                let keyboard_layout = data_init.init(keyboard_layout, LayoutUserData { handle });
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
struct LayoutUserData<D: SeatHandler> {
    handle: Option<KeyboardHandle<D>>,
}

impl<D> Dispatch2<ZcosmicKeyboardLayoutV1, D> for LayoutUserData<D>
where
    D: 'static,
    D: SeatHandler,
    D: KeyboardLayoutHandler,
{
    fn request(
        &self,
        state: &mut D,
        _client: &Client,
        _resource: &ZcosmicKeyboardLayoutV1,
        request: zcosmic_keyboard_layout_v1::Request,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_keyboard_layout_v1::Request::SetGroup { group } => {
                if let Some(handle) = self.handle.as_ref() {
                    handle.with_xkb_state(state, |mut context| {
                        context.set_layout(Layout(group));
                    });
                    KeyboardLayoutState::refresh(state);
                }
            }
            zcosmic_keyboard_layout_v1::Request::Destroy => {}
            _ => unreachable!(),
        }
    }

    fn destroyed(
        &self,
        state: &mut D,
        _client: ClientId,
        keyboard_layout: &ZcosmicKeyboardLayoutV1,
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
