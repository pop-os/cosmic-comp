pub use super::server::zwlr_data_control_offer_v1::{Request, ZwlrDataControlOfferV1 as Addr};

use super::{source, Handler, Selection, SelectionType, State};
use smithay::{
    input::{Seat, SeatHandler},
    reexports::wayland_server::{self, Dispatch, DisplayHandle, Resource},
};
use tracing::{debug, error};
use wayland_backend::server::{ClientId, ObjectId};

pub struct Offer<D: SeatHandler> {
    addr: Addr,
    selection_type: SelectionType,
    seat: Seat<D>,
}

impl<D: SeatHandler> Offer<D> {
    pub fn new(addr: Addr, selection_type: SelectionType, seat: Seat<D>) -> Self {
        Self {
            addr,
            selection_type,
            seat,
        }
    }

    pub fn addr(&self) -> &Addr {
        &self.addr
    }
}

pub fn offer(addr: &Addr, mime_type: String) {
    debug!(id = %addr.id(), mime_type, "offer");
    addr.offer(mime_type);
}

impl<D: Handler> Dispatch<Addr, (), D> for State<D> {
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
            Request::Receive { mime_type, fd } => {
                debug!(id = %addr.id(), mime_type, ?fd, "receive");

                let selection_type = {
                    let state = handler.state();

                    let Some(offer) = state.offers.get(&addr.id()) else {
                         return error!(id = %addr.id(), "Missing Offer!");   
                    };

                    let mut selections = state.seat_selections.get_selections_mut(&offer.seat);
                    if let Selection::Client(Some(s)) = selections.get_mut(offer.selection_type) {
                        return source::send(&s.source_addr, mime_type, fd);
                    };

                    offer.selection_type
                };

                handler.send_selection(selection_type, mime_type, fd);
            }
            Request::Destroy => debug!(id = %addr.id(), "destroy"),
            _ => unreachable!(),
        }
    }

    fn destroyed(handler: &mut D, _client: ClientId, id: ObjectId, _data: &()) {
        debug!(%id, "destroyed");
        let _ = handler.state().offers.remove(&id);
    }
}
