pub use super::server::zwlr_data_control_device_v1::{
    Error, Request, ZwlrDataControlDeviceV1 as Addr,
};

use super::{offer, source, ClientSelection, Handler, Selection, SelectionType, State};
use smithay::{
    input::{Seat, SeatHandler},
    reexports::wayland_server::{Client, DataInit, Dispatch, DisplayHandle, Resource},
};
use tracing::{debug, error};
use wayland_backend::{server::ClientId, server::ObjectId};

pub struct Device<D: SeatHandler> {
    addr: Addr,
    seat: Seat<D>,
}

impl<D: SeatHandler> Device<D> {
    pub fn new(addr: Addr, seat: Seat<D>) -> Self {
        Self { addr, seat }
    }

    pub fn addr(&self) -> &Addr {
        &self.addr
    }

    pub fn seat(&self) -> &Seat<D> {
        &self.seat
    }
}

pub fn data_offer(addr: &Addr, offer_addr: &offer::Addr) {
    debug!(id = %addr.id(), offer_id = %offer_addr.id(), "data_offer");
    addr.data_offer(offer_addr);
}

pub fn selection(addr: &Addr, offer_addr: Option<&offer::Addr>) {
    debug!(offer_id = ?offer_addr.map(|addr| addr.id()), "selection");
    addr.selection(offer_addr);
}

fn finished(addr: &Addr) {
    debug!(id = %addr.id(), "finished");
    addr.finished();
}

pub fn primary_selection(addr: &Addr, offer_addr: Option<&offer::Addr>) {
    debug!(offer_id = ?offer_addr.map(|addr| addr.id()), "primary_selection");
    addr.primary_selection(offer_addr);
}

fn error_used_source(addr: &Addr) {
    const MSG: &str =
        "Source given to set_selection or set_primary_selection was already used before";

    error!(id = %addr.id(), MSG, "user_source");
    addr.post_error(Error::UsedSource, MSG);
}

impl<D> Dispatch<Addr, (), D> for State<D>
where
    D: SeatHandler + Handler + Dispatch<offer::Addr, ()> + 'static,
{
    fn request(
        handler: &mut D,
        _client: &Client,
        addr: &Addr,
        request: Request,
        _data: &(),
        dh: &DisplayHandle,
        _di: &mut DataInit<'_, D>,
    ) {
        match request {
            Request::SetSelection { source } => {
                debug!(source_id = ?source.as_ref().map(|addr| addr.id()), "set_selection");
                set_selection((handler, dh), addr, source, SelectionType::Clipboard);
            }
            Request::SetPrimarySelection { source } => {
                debug!(source_id = ?source.as_ref().map(|addr| addr.id()), "set_primary_selection");
                set_selection((handler, dh), addr, source, SelectionType::Primary);
            }
            Request::Destroy => debug!(id = %addr.id(), "destroy"),

            _ => unreachable!(),
        }
    }

    fn destroyed(handler: &mut D, _: ClientId, id: ObjectId, _: &()) {
        let Some(device) = handler.state().devices.remove(&id) else { return };
        finished(&device.addr);
    }
}

fn set_selection<D>(
    (handler, dh): (&mut D, &DisplayHandle),
    device_addr: &Addr,
    source_addr: Option<source::Addr>,
    ty: SelectionType,
) where
    D: Handler + Dispatch<offer::Addr, ()> + 'static,
{
    let (mime_types, selection) = if let Some(source_addr) = source_addr {
        let source = handler.state().sources.get_mut(&source_addr.id());
        let Some(mime_types) = source.and_then(|s| s.consume(device_addr, ty, dh)) else {
            return error_used_source(device_addr);
        };

        let selection = ClientSelection {
            mime_types,
            source_addr,
        };

        (Some(selection.mime_types.clone()), Some(selection))
    } else {
        (None, None)
    };

    handler.new_selection(ty, mime_types);

    let state = handler.state();
    let Some(seat) = state.devices.get(&device_addr.id()).map(Device::seat) else {
        return error!(id = %device_addr.id(), "Missing device!")
    };

    super::set_selection_and_broadcast(
        (seat, dh),
        &state.devices,
        &mut state.offers,
        &mut state.seat_selections,
        Selection::Client(selection),
        ty,
    )
}
