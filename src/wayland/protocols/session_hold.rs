// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the session-hold protocol (cosmic_session_hold_manager_v1).
//!
//! When a session starts, the compositor keeps the previous session's last
//! frame on screen and cross-fades out of it once there is something to show
//! (see `AdoptFrame` in the KMS surface thread). A client that means to cover
//! the session before the user sees it — a first-run experience, say — can
//! hold that frame up until it genuinely has pixels.
//!
//! The compositor never learns which client that is. Presence of a live
//! `cosmic_session_hold_v1` is the whole signal, so an application that decides
//! it has nothing to show simply never claims one and nothing waits for it.
//! That is the point: no configuration names an application, and no compositor
//! code reads another program's state.
//!
//! A hold dies with the connection that owns it, so a client that crashes or is
//! killed releases it without anything having to notice.

// Re-export generated types
pub use generated::{cosmic_session_hold_manager_v1, cosmic_session_hold_v1};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self};

    pub mod __interfaces {
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/session-hold.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/session-hold.xml");
}

use smithay::reexports::wayland_server::{
    Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, backend::GlobalId,
};

/// State for the session-hold manager protocol (holds the global alive).
#[derive(Debug)]
pub struct SessionHoldState {
    global: GlobalId,
}

impl SessionHoldState {
    pub fn new<D>(dh: &DisplayHandle) -> SessionHoldState
    where
        D: GlobalDispatch<cosmic_session_hold_manager_v1::CosmicSessionHoldManagerV1, ()>
            + Dispatch<cosmic_session_hold_manager_v1::CosmicSessionHoldManagerV1, ()>
            + Dispatch<cosmic_session_hold_v1::CosmicSessionHoldV1, ()>
            + SessionHoldHandler
            + 'static,
    {
        let global = dh
            .create_global::<D, cosmic_session_hold_manager_v1::CosmicSessionHoldManagerV1, _>(
                1,
                (),
            );
        SessionHoldState { global }
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

/// Handler trait for the session-hold protocol.
pub trait SessionHoldHandler {
    fn session_hold_state(&mut self) -> &mut SessionHoldState;

    /// A client asked the session not to be revealed yet.
    ///
    /// `timeout_ms` is the client's own estimate of how long it needs. It is a
    /// hint: the compositor caps how long it will wait regardless.
    fn session_hold_taken(&mut self, timeout_ms: u32);

    /// A hold went away, either released or lost with its client.
    fn session_hold_released(&mut self);
}

impl<D> GlobalDispatch<cosmic_session_hold_manager_v1::CosmicSessionHoldManagerV1, (), D>
    for SessionHoldState
where
    D: GlobalDispatch<cosmic_session_hold_manager_v1::CosmicSessionHoldManagerV1, ()>
        + Dispatch<cosmic_session_hold_manager_v1::CosmicSessionHoldManagerV1, ()>
        + Dispatch<cosmic_session_hold_v1::CosmicSessionHoldV1, ()>
        + SessionHoldHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _dh: &DisplayHandle,
        _client: &Client,
        resource: New<cosmic_session_hold_manager_v1::CosmicSessionHoldManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<cosmic_session_hold_manager_v1::CosmicSessionHoldManagerV1, (), D>
    for SessionHoldState
where
    D: Dispatch<cosmic_session_hold_manager_v1::CosmicSessionHoldManagerV1, ()>
        + Dispatch<cosmic_session_hold_v1::CosmicSessionHoldV1, ()>
        + SessionHoldHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &cosmic_session_hold_manager_v1::CosmicSessionHoldManagerV1,
        request: cosmic_session_hold_manager_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            cosmic_session_hold_manager_v1::Request::Hold { id, timeout_ms } => {
                data_init.init(id, ());
                state.session_hold_taken(timeout_ms);
            }
            cosmic_session_hold_manager_v1::Request::Destroy => {}
        }
    }
}

impl<D> Dispatch<cosmic_session_hold_v1::CosmicSessionHoldV1, (), D> for SessionHoldState
where
    D: Dispatch<cosmic_session_hold_v1::CosmicSessionHoldV1, ()> + SessionHoldHandler + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _resource: &cosmic_session_hold_v1::CosmicSessionHoldV1,
        request: cosmic_session_hold_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            // The hold is dropped in `destroyed` below, which also covers a
            // client that disconnects without releasing.
            cosmic_session_hold_v1::Request::Release => {}
        }
    }

    fn destroyed(
        state: &mut D,
        _client: smithay::reexports::wayland_server::backend::ClientId,
        _resource: &cosmic_session_hold_v1::CosmicSessionHoldV1,
        _data: &(),
    ) {
        state.session_hold_released();
    }
}

macro_rules! delegate_session_hold {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::session_hold::cosmic_session_hold_manager_v1::CosmicSessionHoldManagerV1: ()
        ] => $crate::wayland::protocols::session_hold::SessionHoldState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::session_hold::cosmic_session_hold_manager_v1::CosmicSessionHoldManagerV1: ()
        ] => $crate::wayland::protocols::session_hold::SessionHoldState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::session_hold::cosmic_session_hold_v1::CosmicSessionHoldV1: ()
        ] => $crate::wayland::protocols::session_hold::SessionHoldState);
    };
}
pub(crate) use delegate_session_hold;
