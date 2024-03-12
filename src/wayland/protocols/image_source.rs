use super::{
    toplevel_info::window_from_handle,
    workspace::{WorkspaceHandle, WorkspaceHandler},
};
use crate::shell::CosmicSurface;
use cosmic_protocols::image_source::v1::server::{
    zcosmic_image_source_v1::ZcosmicImageSourceV1,
    zcosmic_output_image_source_manager_v1::{
        Request as OutputSourceRequest, ZcosmicOutputImageSourceManagerV1,
    },
    zcosmic_toplevel_image_source_manager_v1::{
        Request as ToplevelSourceRequest, ZcosmicToplevelImageSourceManagerV1,
    },
    zcosmic_workspace_image_source_manager_v1::{
        Request as WorkspaceSourceRequest, ZcosmicWorkspaceImageSourceManagerV1,
    },
};
use smithay::{
    output::{Output, WeakOutput},
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource,
    },
};
use wayland_backend::server::GlobalId;

#[derive(Debug)]
pub struct ImageSourceState {
    output_source_global: GlobalId,
    workspace_source_global: GlobalId,
    toplevel_source_global: GlobalId,
}

pub struct OutputImageSourceManagerGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}
pub struct WorkspaceImageSourceManagerGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}
pub struct ToplevelImageSourceManagerGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImageSourceData {
    Output(WeakOutput),
    Workspace(WorkspaceHandle),
    Toplevel(CosmicSurface),
    Destroyed,
}

impl ImageSourceState {
    pub fn new<D, F>(display: &DisplayHandle, client_filter: F) -> ImageSourceState
    where
        D: GlobalDispatch<ZcosmicOutputImageSourceManagerV1, OutputImageSourceManagerGlobalData>
            + Dispatch<ZcosmicOutputImageSourceManagerV1, ()>
            + GlobalDispatch<
                ZcosmicWorkspaceImageSourceManagerV1,
                WorkspaceImageSourceManagerGlobalData,
            > + Dispatch<ZcosmicWorkspaceImageSourceManagerV1, ()>
            + GlobalDispatch<
                ZcosmicToplevelImageSourceManagerV1,
                ToplevelImageSourceManagerGlobalData,
            > + Dispatch<ZcosmicToplevelImageSourceManagerV1, ()>
            + Dispatch<ZcosmicImageSourceV1, ImageSourceData>
            + WorkspaceHandler
            + 'static,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + Clone + 'static,
    {
        ImageSourceState {
            output_source_global: display.create_global::<D, ZcosmicOutputImageSourceManagerV1, _>(
                1,
                OutputImageSourceManagerGlobalData {
                    filter: Box::new(client_filter.clone()),
                },
            ),
            workspace_source_global: display
                .create_global::<D, ZcosmicWorkspaceImageSourceManagerV1, _>(
                    1,
                    WorkspaceImageSourceManagerGlobalData {
                        filter: Box::new(client_filter.clone()),
                    },
                ),
            toplevel_source_global: display
                .create_global::<D, ZcosmicToplevelImageSourceManagerV1, _>(
                    1,
                    ToplevelImageSourceManagerGlobalData {
                        filter: Box::new(client_filter),
                    },
                ),
        }
    }

    pub fn output_source_id(&self) -> &GlobalId {
        &self.output_source_global
    }

    pub fn workspace_source_id(&self) -> &GlobalId {
        &self.workspace_source_global
    }

    pub fn toplevel_source_id(&self) -> &GlobalId {
        &self.toplevel_source_global
    }
}

impl<D> GlobalDispatch<ZcosmicOutputImageSourceManagerV1, OutputImageSourceManagerGlobalData, D>
    for ImageSourceState
where
    D: GlobalDispatch<ZcosmicOutputImageSourceManagerV1, OutputImageSourceManagerGlobalData>
        + Dispatch<ZcosmicOutputImageSourceManagerV1, ()>
        + Dispatch<ZcosmicImageSourceV1, ImageSourceData>
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<ZcosmicOutputImageSourceManagerV1>,
        _global_data: &OutputImageSourceManagerGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }

    fn can_view(client: Client, global_data: &OutputImageSourceManagerGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D>
    GlobalDispatch<ZcosmicWorkspaceImageSourceManagerV1, WorkspaceImageSourceManagerGlobalData, D>
    for ImageSourceState
where
    D: GlobalDispatch<ZcosmicWorkspaceImageSourceManagerV1, WorkspaceImageSourceManagerGlobalData>
        + Dispatch<ZcosmicWorkspaceImageSourceManagerV1, ()>
        + Dispatch<ZcosmicImageSourceV1, ImageSourceData>
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<ZcosmicWorkspaceImageSourceManagerV1>,
        _global_data: &WorkspaceImageSourceManagerGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }

    fn can_view(client: Client, global_data: &WorkspaceImageSourceManagerGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> GlobalDispatch<ZcosmicToplevelImageSourceManagerV1, ToplevelImageSourceManagerGlobalData, D>
    for ImageSourceState
where
    D: GlobalDispatch<ZcosmicToplevelImageSourceManagerV1, ToplevelImageSourceManagerGlobalData>
        + Dispatch<ZcosmicToplevelImageSourceManagerV1, ()>
        + Dispatch<ZcosmicImageSourceV1, ImageSourceData>
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<ZcosmicToplevelImageSourceManagerV1>,
        _global_data: &ToplevelImageSourceManagerGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }

    fn can_view(client: Client, global_data: &ToplevelImageSourceManagerGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ZcosmicOutputImageSourceManagerV1, (), D> for ImageSourceState
where
    D: Dispatch<ZcosmicOutputImageSourceManagerV1, ()>
        + Dispatch<ZcosmicImageSourceV1, ImageSourceData>
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _resource: &ZcosmicOutputImageSourceManagerV1,
        request: <ZcosmicOutputImageSourceManagerV1 as Resource>::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            OutputSourceRequest::CreateSource { source, output } => {
                let data = match Output::from_resource(&output) {
                    Some(output) => ImageSourceData::Output(output.downgrade()),
                    None => ImageSourceData::Destroyed,
                };
                data_init.init(source, data);
            }
            _ => {}
        }
    }

    fn destroyed(
        _state: &mut D,
        _client: wayland_backend::server::ClientId,
        _resource: &ZcosmicOutputImageSourceManagerV1,
        _data: &(),
    ) {
    }
}

impl<D> Dispatch<ZcosmicWorkspaceImageSourceManagerV1, (), D> for ImageSourceState
where
    D: Dispatch<ZcosmicWorkspaceImageSourceManagerV1, ()>
        + Dispatch<ZcosmicImageSourceV1, ImageSourceData>
        + WorkspaceHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &ZcosmicWorkspaceImageSourceManagerV1,
        request: <ZcosmicWorkspaceImageSourceManagerV1 as Resource>::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            WorkspaceSourceRequest::CreateSource { source, output } => {
                let data = match state.workspace_state().workspace_handle(&output) {
                    Some(workspace) => ImageSourceData::Workspace(workspace),
                    None => ImageSourceData::Destroyed,
                };
                data_init.init(source, data);
            }
            _ => {}
        }
    }

    fn destroyed(
        _state: &mut D,
        _client: wayland_backend::server::ClientId,
        _resource: &ZcosmicWorkspaceImageSourceManagerV1,
        _data: &(),
    ) {
    }
}

impl<D> Dispatch<ZcosmicToplevelImageSourceManagerV1, (), D> for ImageSourceState
where
    D: Dispatch<ZcosmicToplevelImageSourceManagerV1, ()>
        + Dispatch<ZcosmicImageSourceV1, ImageSourceData>
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _resource: &ZcosmicToplevelImageSourceManagerV1,
        request: <ZcosmicToplevelImageSourceManagerV1 as Resource>::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            ToplevelSourceRequest::CreateSource {
                source,
                toplevel_handle,
            } => {
                let data = match window_from_handle(toplevel_handle) {
                    Some(toplevel) => ImageSourceData::Toplevel(toplevel),
                    None => ImageSourceData::Destroyed,
                };
                data_init.init(source, data);
            }
            _ => {}
        }
    }

    fn destroyed(
        _state: &mut D,
        _client: wayland_backend::server::ClientId,
        _resource: &ZcosmicToplevelImageSourceManagerV1,
        _data: &(),
    ) {
    }
}

impl<D> Dispatch<ZcosmicImageSourceV1, ImageSourceData, D> for ImageSourceState
where
    D: Dispatch<ZcosmicImageSourceV1, ImageSourceData> + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _resource: &ZcosmicImageSourceV1,
        request: <ZcosmicImageSourceV1 as Resource>::Request,
        _data: &ImageSourceData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            _ => {}
        }
    }

    fn destroyed(
        _state: &mut D,
        _client: wayland_backend::server::ClientId,
        _resource: &ZcosmicImageSourceV1,
        _data: &ImageSourceData,
    ) {
    }
}

macro_rules! delegate_image_source {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::image_source::v1::server::zcosmic_output_image_source_manager_v1::ZcosmicOutputImageSourceManagerV1: $crate::wayland::protocols::image_source::OutputImageSourceManagerGlobalData
        ] => $crate::wayland::protocols::image_source::ImageSourceState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::image_source::v1::server::zcosmic_output_image_source_manager_v1::ZcosmicOutputImageSourceManagerV1: ()
        ] => $crate::wayland::protocols::image_source::ImageSourceState);
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::image_source::v1::server::zcosmic_workspace_image_source_manager_v1::ZcosmicWorkspaceImageSourceManagerV1: $crate::wayland::protocols::image_source::WorkspaceImageSourceManagerGlobalData
        ] => $crate::wayland::protocols::image_source::ImageSourceState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::image_source::v1::server::zcosmic_workspace_image_source_manager_v1::ZcosmicWorkspaceImageSourceManagerV1: ()
        ] => $crate::wayland::protocols::image_source::ImageSourceState);
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::image_source::v1::server::zcosmic_toplevel_image_source_manager_v1::ZcosmicToplevelImageSourceManagerV1: $crate::wayland::protocols::image_source::ToplevelImageSourceManagerGlobalData
        ] => $crate::wayland::protocols::image_source::ImageSourceState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::image_source::v1::server::zcosmic_toplevel_image_source_manager_v1::ZcosmicToplevelImageSourceManagerV1: ()
        ] => $crate::wayland::protocols::image_source::ImageSourceState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::image_source::v1::server::zcosmic_image_source_v1::ZcosmicImageSourceV1: $crate::wayland::protocols::image_source::ImageSourceData
        ] => $crate::wayland::protocols::image_source::ImageSourceState);
    };
}
pub(crate) use delegate_image_source;
