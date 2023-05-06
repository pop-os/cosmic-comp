use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::os::unix::io::OwnedFd;

use smithay::input::{Seat, SeatHandler};
use smithay::reexports::wayland_protocols_wlr::data_control::v1::server;
use smithay::reexports::wayland_server::{
    backend::ObjectId, Client, Dispatch, DisplayHandle, GlobalDispatch, Resource,
};
use tracing::error;

use self::{device::Device, manager::Manager, offer::Offer, source::Source};

mod device;
mod manager;
mod offer;
mod source;

/// Events that are generated by interactions of the clients with the data control protocol.
pub trait Handler: SeatHandler + Sized {
    fn state(&mut self) -> &mut State<Self>;

    /// A client has set the clipboard selection.
    fn new_selection(&mut self, _ty: SelectionType, _mime_types: Option<Vec<String>>) {}

    /// A client requested to read the server-set clipboard selection.
    ///
    /// * `mime_type` - the requested mime type
    /// * `fd` - the fd to write into
    fn send_selection(&mut self, _ty: SelectionType, _mime_type: String, _fd: OwnedFd) {}
}

pub struct State<D: SeatHandler> {
    _manager: Manager,

    devices: HashMap<ObjectId, Device<D>>,
    sources: HashMap<ObjectId, Source>,
    offers: HashMap<ObjectId, Offer<D>>,

    seat_selections: SeatSelections,
}

struct SeatSelections;

impl SeatSelections {
    fn get_selections_mut<'a, 'b: 'a, D: SeatHandler + 'static>(
        &'a mut self,
        seat: &'b Seat<D>,
    ) -> RefMut<'a, Selections> {
        seat.user_data().insert_if_missing(|| {
            RefCell::new(self::Selections {
                clipboard: Selection::Client(None),
                primary_selection: Selection::Compositor(None),
            })
        });

        seat.user_data()
            .get::<RefCell<Selections>>()
            .unwrap()
            .borrow_mut()
    }
}

pub struct Selections {
    clipboard: Selection,
    primary_selection: Selection,
}

impl Selections {
    fn get_mut(&mut self, ty: SelectionType) -> &mut Selection {
        match ty {
            SelectionType::Clipboard => &mut self.clipboard,
            SelectionType::Primary => &mut self.primary_selection,
        }
    }
}

impl<D: SeatHandler> State<D> {
    pub fn new(dh: &DisplayHandle) -> Self
    where
        D: GlobalDispatch<manager::Addr, ()> + 'static,
    {
        Self {
            _manager: manager::new::<D>(dh),
            devices: HashMap::new(),
            sources: HashMap::new(),
            offers: HashMap::new(),
            seat_selections: SeatSelections,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Selection {
    Client(Option<ClientSelection>),
    Compositor(Option<CompositorSelection>),
}

impl Drop for Selection {
    fn drop(&mut self) {
        if let Self::Client(Some(s)) = self {
            source::cancelled(&s.source_addr);
        }
    }
}

impl Selection {
    fn mime_types(&self) -> Option<Vec<String>> {
        match self {
            Selection::Client(Some(s)) => Some(s.mime_types.clone()),
            Selection::Compositor(Some(s)) => Some(s.mime_types.clone()),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ClientSelection {
    mime_types: Vec<String>,
    source_addr: source::Addr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct CompositorSelection {
    mime_types: Vec<String>,
}

#[derive(Clone, Copy, Debug)]
pub enum SelectionType {
    Clipboard,
    Primary,
}

fn set_selection_and_broadcast<D>(
    (seat, dh): (&Seat<D>, &DisplayHandle),
    devices: &HashMap<ObjectId, Device<D>>,
    offers: &mut HashMap<ObjectId, Offer<D>>,
    seat_selections: &mut SeatSelections,
    new_selection: Selection,
    ty: SelectionType,
) where
    D: Handler + Dispatch<offer::Addr, ()> + 'static,
{
    match seat_selections.get_selections_mut(seat).get_mut(ty) {
        s if s == &new_selection => return,
        s => *s = new_selection,
    }

    for dev in devices.values() {
        if dev.seat() != seat {
            continue;
        }

        if let Ok(client) = dh.get_client(dev.addr().id()) {
            offer_selection::<D>((seat, dh, &client), offers, seat_selections, dev.addr(), ty);
        };
    }
}

fn offer_selection<D>(
    (seat, dh, client): (&Seat<D>, &DisplayHandle, &Client),
    offers: &mut HashMap<ObjectId, Offer<D>>,
    seat_selections: &mut SeatSelections,
    dev_addr: &device::Addr,
    ty: SelectionType,
) where
    D: Handler + Dispatch<offer::Addr, ()> + 'static,
{
    let mut sels = seat_selections.get_selections_mut(seat);
    let offer_addr = sels.get_mut(ty).mime_types().and_then(|mime_types| {
        let offer = client
            .create_resource::<_, _, D>(dh, dev_addr.version(), ())
            .map(|addr| Offer::new(addr, ty, seat.clone()))
            .map(|offer| offers.entry(offer.addr().id()).or_insert(offer))
            .map_err(|e| error!("Failed to create Offer: {e:?}"))
            .ok()?;

        device::data_offer(dev_addr, offer.addr());
        for mime_type in mime_types {
            offer::offer(offer.addr(), mime_type);
        }

        Some(offer.addr())
    });

    match ty {
        SelectionType::Clipboard => device::selection(dev_addr, offer_addr),
        SelectionType::Primary => device::primary_selection(dev_addr, offer_addr),
    }
}

/// Set a compositor-provided primary selection
///
/// You need to provide the available mime types for this selection.
///
/// Whenever a client requests to read the selection, your callback will
/// receive a [`Handler::send_selection`] event.
pub fn set_selection<D>(
    handler: &mut D,
    seat: &Seat<D>,
    dh: &DisplayHandle,
    ty: SelectionType,
    mime_types: Option<Vec<String>>,
) where
    D: Handler + Dispatch<offer::Addr, ()> + 'static,
{
    let state = handler.state();
    set_selection_and_broadcast(
        (seat, dh),
        &mut state.devices,
        &mut state.offers,
        &mut state.seat_selections,
        Selection::Compositor(mime_types.map(|mt| CompositorSelection { mime_types: mt })),
        ty,
    )
}

/// Request the current data_control selection of the given type
/// to be written to the provided file descriptor in the given mime type.
pub fn request_selection<D: Handler + 'static>(
    handler: &mut D,
    seat: &Seat<D>,
    ty: SelectionType,
    mime_type: String,
    fd: OwnedFd,
) -> Result<(), SelectionRequestError> {
    let state = handler.state();
    match state.seat_selections.get_selections_mut(seat).get_mut(ty) {
        Selection::Client(Some(s)) if s.mime_types.contains(&mime_type) => {
            Ok(source::send(&s.source_addr, mime_type, fd))
        }
        Selection::Client(Some(_)) => Err(SelectionRequestError::InvalidMimetype),
        Selection::Client(None) => Err(SelectionRequestError::NoSelection),
        Selection::Compositor(_) => Err(SelectionRequestError::ServerSideSelection),
    }
}

/// Errors happening when requesting selection contents
#[derive(Debug, thiserror::Error)]
pub enum SelectionRequestError {
    /// Requested mime type is not available
    #[error("Requested mime type is not available")]
    InvalidMimetype,
    /// Requesting server side selection contents is not supported
    #[error("Current selection is server-side")]
    ServerSideSelection,
    /// There is no active selection
    #[error("No active selection to query")]
    NoSelection,
}

#[allow(missing_docs)]
#[macro_export]
macro_rules! delegate {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::data_control::v1::server::zwlr_data_control_manager_v1::ZwlrDataControlManagerV1: ()
        ] => $crate::wayland::protocols::data_control::State<Self>);

        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::data_control::v1::server::zwlr_data_control_manager_v1::ZwlrDataControlManagerV1: ()
        ] => $crate::wayland::protocols::data_control::State<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::data_control::v1::server::zwlr_data_control_device_v1::ZwlrDataControlDeviceV1: ()
        ] => $crate::wayland::protocols::data_control::State<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::data_control::v1::server::zwlr_data_control_source_v1::ZwlrDataControlSourceV1: ()
        ] => $crate::wayland::protocols::data_control::State<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::data_control::v1::server::zwlr_data_control_offer_v1::ZwlrDataControlOfferV1: ()
        ] => $crate::wayland::protocols::data_control::State<Self>);
    };
}
pub(crate) use delegate;
