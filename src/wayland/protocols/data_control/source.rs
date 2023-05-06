pub use super::server::zwlr_data_control_source_v1::{
    Error, Request, ZwlrDataControlSourceV1 as Addr,
};

use smithay::reexports::wayland_server::{
    self,
    backend::{ClientId, ObjectId},
    Dispatch, DisplayHandle, Resource,
};
use std::os::fd::AsRawFd;
use tracing::{debug, error};
use wayland_backend::io_lifetimes::OwnedFd;

use super::{
    device::{self, Device},
    offer, Handler, Selection, SelectionType,
};

pub struct Source {
    addr: Addr,
    state: State,
}

impl Source {
    pub fn new(addr: Addr) -> Self {
        Self {
            addr,
            state: State::Offering {
                mime_types: Vec::new(),
            },
        }
    }

    pub fn addr(&self) -> &Addr {
        &self.addr
    }

    pub fn consume(
        &mut self,
        by: &device::Addr,
        selection_type: SelectionType,
        dh: &DisplayHandle,
    ) -> Option<Vec<String>> {
        match &mut self.state {
            State::Offering { mime_types } => {
                let mime_types = std::mem::take(mime_types);
                self.state = State::Consumed {
                    by: by.clone(),
                    selection_type,
                    display_handle: dh.clone(),
                };
                Some(mime_types)
            }
            State::Consumed { .. } => None,
        }
    }
}

enum State {
    Offering {
        mime_types: Vec<String>,
    },
    Consumed {
        by: device::Addr,
        selection_type: SelectionType,
        display_handle: DisplayHandle,
    },
}

pub fn send(addr: &Addr, mime_type: String, fd: OwnedFd) {
    debug!(id = %addr.id(), mime_type, ?fd, "send");
    addr.send(mime_type, fd.as_raw_fd());
}

pub fn cancelled(addr: &Addr) {
    debug!(id = %addr.id(), "cancelled");
    addr.cancelled();
}

fn error_invalid_offer(addr: &Addr) {
    error!(id = %addr.id(), "invalid_offer");
    addr.post_error(
        Error::InvalidOffer,
        "offer sent after wlr_data_control_device.set_selection",
    );
}

impl<D> Dispatch<Addr, (), D> for super::State<D>
where
    D: Handler + Dispatch<offer::Addr, ()> + 'static,
{
    fn request(
        handler: &mut D,
        _client: &wayland_server::Client,
        addr: &Addr,
        request: Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        _data_init: &mut wayland_server::DataInit<'_, D>,
    ) {
        match request {
            Request::Offer { mime_type } => {
                debug!(id = %addr.id(), mime_type, "offer");

                match handler.state().sources.get_mut(&addr.id()) {
                    Some(Source {
                        state: State::Offering { mime_types },
                        ..
                    }) => {
                        mime_types.push(mime_type);
                    }
                    Some(_) => error_invalid_offer(addr),
                    None => error!(id = %addr.id(), "Missing source"),
                };
            }
            Request::Destroy => debug!(id = %addr.id(), "destroy"),
            _ => unreachable!(),
        }
    }

    fn destroyed(handler: &mut D, _client: ClientId, id: ObjectId, _: &()) {
        // Set empty selection if this Source is currently being used.

        let Some(source) = handler.state().sources.remove(&id) else { return };
        let State::Consumed { by: device_addr, selection_type, display_handle: dh } = source.state else { return };
        let Some(seat) = handler.state().devices.get(&device_addr.id()).map(Device::seat).cloned() else { return };

        match handler
            .state()
            .seat_selections
            .get_selections_mut(&seat)
            .get_mut(selection_type)
        {
            Selection::Client(Some(s)) if s.source_addr == source.addr => {}
            _ => return,
        };

        super::set_selection(handler, &seat, &dh, selection_type, None);
    }
}
