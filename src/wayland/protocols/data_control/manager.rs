pub use super::server::zwlr_data_control_manager_v1::{Request, ZwlrDataControlManagerV1 as Addr};

use smithay::{
    input::{Seat, SeatHandler},
    reexports::wayland_server::{self, Dispatch, DisplayHandle, GlobalDispatch, Resource},
};

use tracing::{debug, error};

use wayland_backend::server::GlobalId;

use super::{device, offer, source, Device, Handler, SelectionType, Source, State};

pub struct Manager {
    _id: GlobalId,
}

pub fn new<D>(dh: &DisplayHandle) -> Manager
where
    D: GlobalDispatch<Addr, ()> + 'static,
{
    Manager {
        _id: dh.create_global::<D, Addr, _>(1, ()),
    }
}

impl<D> GlobalDispatch<Addr, (), D> for State<D>
where
    D: SeatHandler + Dispatch<Addr, ()>,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &wayland_server::Client,
        addr: wayland_server::New<Addr>,
        _global_data: &(),
        data_init: &mut wayland_server::DataInit<'_, D>,
    ) {
        data_init.init(addr, ());
    }
}

impl<D> Dispatch<Addr, (), D> for State<D>
where
    D: SeatHandler
        + Handler
        + Dispatch<source::Addr, ()>
        + Dispatch<device::Addr, ()>
        + Dispatch<offer::Addr, ()>
        + 'static,
{
    fn request(
        handler: &mut D,
        client: &wayland_server::Client,
        _addr: &Addr,
        request: Request,
        _data: &(),
        dh: &DisplayHandle,
        di: &mut wayland_server::DataInit<'_, D>,
    ) {
        match request {
            Request::CreateDataSource { id } => {
                debug!("create_data_source");

                let source = Source::new(di.init(id, ()));
                let _ = handler.state().sources.insert(source.addr().id(), source);
            }
            Request::GetDataDevice { id, seat } => {
                debug!(?seat, "get_data_device");

                let Some(seat) = Seat::<D>::from_resource(&seat) else {
                    error!(?seat, "Failed to retrieve `Seat` from `WlSeat`");
                    return;
                };

                let state = handler.state();

                let device = Device::new(di.init(id, ()), seat);
                let device = state.devices.entry(device.addr().id()).or_insert(device);

                for ty in [SelectionType::Clipboard, SelectionType::Primary] {
                    super::offer_selection::<D>(
                        (device.seat(), dh, client),
                        &mut state.offers,
                        &mut state.seat_selections,
                        device.addr(),
                        ty,
                    )
                }
            }
            Request::Destroy => debug!("destroy"),
            _ => unreachable!(),
        }
    }
}
