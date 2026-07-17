// SPDX-License-Identifier: GPL-3.0-only

//! `wp_tearing_control_v1` support.
//!
//! Advertises the `wp_tearing_control_manager_v1` global, letting clients
//! attach a `wp_tearing_control_v1` object to a `wl_surface` to hint whether
//! its content is suitable for presentation with tearing (async page flips).
//!
//! The presentation hint is double-buffered per the protocol: requests only
//! update the *pending* value, which is promoted to *current* on the next
//! `wl_surface.commit` via [`on_commit`]. Other subsystems read the active
//! hint through [`presentation_hint`].

use std::sync::Mutex;

use smithay::{
    reexports::{
        wayland_protocols::wp::tearing_control::v1::server::{
            wp_tearing_control_manager_v1::{self, WpTearingControlManagerV1},
            wp_tearing_control_v1::{self, WpTearingControlV1},
        },
        wayland_server::{
            Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
            protocol::wl_surface::WlSurface,
        },
    },
    wayland::compositor::with_states,
};
use wayland_backend::{protocol::WEnum, server::GlobalId};

use crate::state::State;

/// Presentation hint requested by a client for a surface.
///
/// This mirrors the protocol's `wp_tearing_control_v1.presentation_hint`
/// enum. `Vsync` is the default and the state used whenever a surface has no
/// (live or ever committed) `wp_tearing_control_v1` object.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum PresentationHint {
    #[default]
    Vsync,
    Async,
}

impl From<wp_tearing_control_v1::PresentationHint> for PresentationHint {
    fn from(hint: wp_tearing_control_v1::PresentationHint) -> Self {
        match hint {
            wp_tearing_control_v1::PresentationHint::Async => PresentationHint::Async,
            _ => PresentationHint::Vsync,
        }
    }
}

type TearingSurfaceData = Mutex<TearingSurfaceState>;

#[derive(Debug, Default)]
struct TearingSurfaceState {
    /// Set by `set_presentation_hint`, applied on the next commit.
    pending: PresentationHint,
    /// The value in effect since the last commit.
    current: PresentationHint,
    /// The currently live `wp_tearing_control_v1` object for this surface, if any.
    /// Used to enforce the "one object per surface" protocol rule; a client is
    /// free to destroy its object and create a new one later.
    object: Option<Weak<WpTearingControlV1>>,
}

/// Returns the current (post-commit) presentation hint for `surface`.
///
/// Defaults to [`PresentationHint::Vsync`] if the surface never had a
/// `wp_tearing_control_v1` object or hasn't committed since one was created.
pub fn presentation_hint(surface: &WlSurface) -> PresentationHint {
    with_states(surface, |states| {
        states
            .data_map
            .get::<TearingSurfaceData>()
            .map(|state| state.lock().unwrap().current)
    })
    .unwrap_or_default()
}

/// Promotes the pending presentation hint to current for `surface`.
///
/// Must be called from the compositor's surface commit handling, since
/// `set_presentation_hint` is double-buffered per the protocol.
pub fn on_commit(surface: &WlSurface) {
    with_states(surface, |states| {
        if let Some(state) = states.data_map.get::<TearingSurfaceData>() {
            let mut state = state.lock().unwrap();
            state.current = state.pending;
        }
    });
}

#[derive(Debug)]
pub struct TearingControlManagerState {
    global: GlobalId,
}

impl TearingControlManagerState {
    pub fn new<D>(dh: &DisplayHandle) -> TearingControlManagerState
    where
        D: GlobalDispatch<WpTearingControlManagerV1, ()>
            + Dispatch<WpTearingControlManagerV1, ()>
            + Dispatch<WpTearingControlV1, TearingControlUserData>
            + 'static,
    {
        let global = dh.create_global::<D, WpTearingControlManagerV1, _>(1, ());
        TearingControlManagerState { global }
    }

    #[allow(dead_code)]
    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

#[derive(Debug)]
pub struct TearingControlUserData {
    surface: Weak<WlSurface>,
}

impl<D> GlobalDispatch<WpTearingControlManagerV1, (), D> for TearingControlManagerState
where
    D: GlobalDispatch<WpTearingControlManagerV1, ()>
        + Dispatch<WpTearingControlManagerV1, ()>
        + Dispatch<WpTearingControlV1, TearingControlUserData>
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<WpTearingControlManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<WpTearingControlManagerV1, (), D> for TearingControlManagerState
where
    D: Dispatch<WpTearingControlManagerV1, ()>
        + Dispatch<WpTearingControlV1, TearingControlUserData>
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        manager: &WpTearingControlManagerV1,
        request: wp_tearing_control_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            wp_tearing_control_manager_v1::Request::GetTearingControl { id, surface } => {
                let exists = with_states(&surface, |states| {
                    states
                        .data_map
                        .get::<TearingSurfaceData>()
                        .is_some_and(|state| {
                            state
                                .lock()
                                .unwrap()
                                .object
                                .as_ref()
                                .is_some_and(|obj| obj.upgrade().is_ok())
                        })
                });

                if exists {
                    manager.post_error(
                        wp_tearing_control_manager_v1::Error::TearingControlExists as u32,
                        format!(
                            "{manager:?}: a wp_tearing_control_v1 object already exists for {surface:?}"
                        ),
                    );
                }

                // A `new_id` argument must always be initialized, even when we
                // raise a protocol error above (the client will be disconnected).
                let control = data_init.init(
                    id,
                    TearingControlUserData {
                        surface: surface.downgrade(),
                    },
                );

                with_states(&surface, |states| {
                    let state = states
                        .data_map
                        .get_or_insert_threadsafe(TearingSurfaceData::default);
                    state.lock().unwrap().object = Some(control.downgrade());
                });

                tracing::warn!(surface = ?surface.id(), "tearing_control: client created control object");
            }
            wp_tearing_control_manager_v1::Request::Destroy => {}
            _ => {}
        }
    }
}

impl<D> Dispatch<WpTearingControlV1, TearingControlUserData, D> for TearingControlManagerState
where
    D: Dispatch<WpTearingControlV1, TearingControlUserData> + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _resource: &WpTearingControlV1,
        request: wp_tearing_control_v1::Request,
        data: &TearingControlUserData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        let Ok(surface) = data.surface.upgrade() else {
            return;
        };

        match request {
            wp_tearing_control_v1::Request::SetPresentationHint { hint } => {
                if let WEnum::Value(hint) = hint {
                    let parsed_hint: PresentationHint = hint.into();
                    tracing::warn!(surface = ?surface.id(), hint = ?parsed_hint, "tearing_control: hint set (pending until commit)");
                    with_states(&surface, |states| {
                        if let Some(state) = states.data_map.get::<TearingSurfaceData>() {
                            state.lock().unwrap().pending = parsed_hint;
                        }
                    });
                }
            }
            wp_tearing_control_v1::Request::Destroy => {
                // Per spec: destroying the object reverts the presentation hint
                // to vsync; being double-buffered, this only takes effect on
                // the next commit.
                with_states(&surface, |states| {
                    if let Some(state) = states.data_map.get::<TearingSurfaceData>() {
                        state.lock().unwrap().pending = PresentationHint::Vsync;
                    }
                });

                tracing::warn!("tearing_control: control destroyed (pending reset to vsync)");
            }
            _ => {}
        }
    }
}

smithay::reexports::wayland_server::delegate_global_dispatch!(State: [
    WpTearingControlManagerV1: ()
] => TearingControlManagerState);
smithay::reexports::wayland_server::delegate_dispatch!(State: [
    WpTearingControlManagerV1: ()
] => TearingControlManagerState);
smithay::reexports::wayland_server::delegate_dispatch!(State: [
    WpTearingControlV1: TearingControlUserData
] => TearingControlManagerState);
