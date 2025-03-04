use super::{
    toplevel_info::window_from_ext_handle,
    workspace::{WorkspaceHandle, WorkspaceHandler},
};
use crate::{
    shell::CosmicSurface,
    wayland::protocols::toplevel_info::ToplevelInfoHandler,
};
use cosmic_protocols::image_capture_source::v1::server::{
    zcosmic_workspace_image_capture_source_manager_v1::{
        Request as CosmicWorkspaceSourceRequest, ZcosmicWorkspaceImageCaptureSourceManagerV1,
    },
};
use smithay::reexports::wayland_protocols::ext::image_capture_source::v1::server::{
    ext_foreign_toplevel_image_capture_source_manager_v1::{
        Request as ToplevelSourceRequest, ExtForeignToplevelImageCaptureSourceManagerV1,
    },
    ext_image_capture_source_v1::ExtImageCaptureSourceV1,
    ext_output_image_capture_source_manager_v1::{
        Request as OutputSourceRequest, ExtOutputImageCaptureSourceManagerV1,
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
pub struct ImageCaptureSourceState {
    output_source_global: GlobalId,
    workspace_source_global: GlobalId,
    toplevel_source_global: GlobalId,
}

pub struct OutputImageCaptureSourceManagerGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}
pub struct WorkspaceImageCaptureSourceManagerGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}
pub struct ToplevelImageCaptureSourceManagerGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImageCaptureSourceData {
    Output(WeakOutput),
    Workspace(WorkspaceHandle),
    Toplevel(CosmicSurface),
    Destroyed,
}

impl ImageCaptureSourceState {
    pub fn new<D, F>(display: &DisplayHandle, client_filter: F) -> ImageCaptureSourceState
    where
        D: GlobalDispatch<
                ExtOutputImageCaptureSourceManagerV1,
                OutputImageCaptureSourceManagerGlobalData,
            > + Dispatch<ExtOutputImageCaptureSourceManagerV1, ()>
            + GlobalDispatch<
                ZcosmicWorkspaceImageCaptureSourceManagerV1,
                WorkspaceImageCaptureSourceManagerGlobalData,
            > + Dispatch<ZcosmicWorkspaceImageCaptureSourceManagerV1, ()>
            + GlobalDispatch<
                ExtForeignToplevelImageCaptureSourceManagerV1,
                ToplevelImageCaptureSourceManagerGlobalData,
            > + Dispatch<ExtForeignToplevelImageCaptureSourceManagerV1, ()>
            + Dispatch<ExtImageCaptureSourceV1, ImageCaptureSourceData>
            + WorkspaceHandler
            + 'static,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + Clone + 'static,
    {
        ImageCaptureSourceState {
            output_source_global: display
                .create_global::<D, ExtOutputImageCaptureSourceManagerV1, _>(
                    1,
                    OutputImageCaptureSourceManagerGlobalData {
                        filter: Box::new(client_filter.clone()),
                    },
                ),
            workspace_source_global: display
                .create_global::<D, ZcosmicWorkspaceImageCaptureSourceManagerV1, _>(
                    1,
                    WorkspaceImageCaptureSourceManagerGlobalData {
                        filter: Box::new(client_filter.clone()),
                    },
                ),
            toplevel_source_global: display
                .create_global::<D, ExtForeignToplevelImageCaptureSourceManagerV1, _>(
                    1,
                    ToplevelImageCaptureSourceManagerGlobalData {
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

impl<D>
    GlobalDispatch<
        ExtOutputImageCaptureSourceManagerV1,
        OutputImageCaptureSourceManagerGlobalData,
        D,
    > for ImageCaptureSourceState
where
    D: GlobalDispatch<
            ExtOutputImageCaptureSourceManagerV1,
            OutputImageCaptureSourceManagerGlobalData,
        > + Dispatch<ExtOutputImageCaptureSourceManagerV1, ()>
        + Dispatch<ExtImageCaptureSourceV1, ImageCaptureSourceData>
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<ExtOutputImageCaptureSourceManagerV1>,
        _global_data: &OutputImageCaptureSourceManagerGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }

    fn can_view(client: Client, global_data: &OutputImageCaptureSourceManagerGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D>
    GlobalDispatch<
        ZcosmicWorkspaceImageCaptureSourceManagerV1,
        WorkspaceImageCaptureSourceManagerGlobalData,
        D,
    > for ImageCaptureSourceState
where
    D: GlobalDispatch<
            ZcosmicWorkspaceImageCaptureSourceManagerV1,
            WorkspaceImageCaptureSourceManagerGlobalData,
        > + Dispatch<ZcosmicWorkspaceImageCaptureSourceManagerV1, ()>
        + Dispatch<ExtImageCaptureSourceV1, ImageCaptureSourceData>
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<ZcosmicWorkspaceImageCaptureSourceManagerV1>,
        _global_data: &WorkspaceImageCaptureSourceManagerGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }

    fn can_view(
        client: Client,
        global_data: &WorkspaceImageCaptureSourceManagerGlobalData,
    ) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D>
    GlobalDispatch<
        ExtForeignToplevelImageCaptureSourceManagerV1,
        ToplevelImageCaptureSourceManagerGlobalData,
        D,
    > for ImageCaptureSourceState
where
    D: GlobalDispatch<
            ExtForeignToplevelImageCaptureSourceManagerV1,
            ToplevelImageCaptureSourceManagerGlobalData,
        > + Dispatch<ExtForeignToplevelImageCaptureSourceManagerV1, ()>
        + Dispatch<ExtImageCaptureSourceV1, ImageCaptureSourceData>
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<ExtForeignToplevelImageCaptureSourceManagerV1>,
        _global_data: &ToplevelImageCaptureSourceManagerGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }

    fn can_view(client: Client, global_data: &ToplevelImageCaptureSourceManagerGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ExtOutputImageCaptureSourceManagerV1, (), D> for ImageCaptureSourceState
where
    D: Dispatch<ExtOutputImageCaptureSourceManagerV1, ()>
        + Dispatch<ExtImageCaptureSourceV1, ImageCaptureSourceData>
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _resource: &ExtOutputImageCaptureSourceManagerV1,
        request: <ExtOutputImageCaptureSourceManagerV1 as Resource>::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            OutputSourceRequest::CreateSource { source, output } => {
                let data = match Output::from_resource(&output) {
                    Some(output) => ImageCaptureSourceData::Output(output.downgrade()),
                    None => ImageCaptureSourceData::Destroyed,
                };
                data_init.init(source, data);
            }
            _ => {}
        }
    }

    fn destroyed(
        _state: &mut D,
        _client: wayland_backend::server::ClientId,
        _resource: &ExtOutputImageCaptureSourceManagerV1,
        _data: &(),
    ) {
    }
}

impl<D> Dispatch<ZcosmicWorkspaceImageCaptureSourceManagerV1, (), D> for ImageCaptureSourceState
where
    D: Dispatch<ZcosmicWorkspaceImageCaptureSourceManagerV1, ()>
        + Dispatch<ExtImageCaptureSourceV1, ImageCaptureSourceData>
        + WorkspaceHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &ZcosmicWorkspaceImageCaptureSourceManagerV1,
        request: <ZcosmicWorkspaceImageCaptureSourceManagerV1 as Resource>::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            CosmicWorkspaceSourceRequest::CreateSource { source, output } => {
                let data = match state.workspace_state().get_ext_workspace_handle(&output) {
                    Some(workspace) => ImageCaptureSourceData::Workspace(workspace),
                    None => ImageCaptureSourceData::Destroyed,
                };
                data_init.init(source, data);
            }
            _ => {}
        }
    }

    fn destroyed(
        _state: &mut D,
        _client: wayland_backend::server::ClientId,
        _resource: &ZcosmicWorkspaceImageCaptureSourceManagerV1,
        _data: &(),
    ) {
    }
}

impl<D> Dispatch<ExtForeignToplevelImageCaptureSourceManagerV1, (), D> for ImageCaptureSourceState
where
    D: Dispatch<ExtForeignToplevelImageCaptureSourceManagerV1, ()>
        + Dispatch<ExtImageCaptureSourceV1, ImageCaptureSourceData>
        + ToplevelInfoHandler<Window = CosmicSurface>
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &ExtForeignToplevelImageCaptureSourceManagerV1,
        request: <ExtForeignToplevelImageCaptureSourceManagerV1 as Resource>::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            ToplevelSourceRequest::CreateSource {
                source,
                toplevel_handle,
            } => {
                let data = match window_from_ext_handle(state, &toplevel_handle) {
                    Some(toplevel) => ImageCaptureSourceData::Toplevel(toplevel.clone()),
                    None => ImageCaptureSourceData::Destroyed,
                };
                data_init.init(source, data);
            }
            _ => {}
        }
    }

    fn destroyed(
        _state: &mut D,
        _client: wayland_backend::server::ClientId,
        _resource: &ExtForeignToplevelImageCaptureSourceManagerV1,
        _data: &(),
    ) {
    }
}

impl<D> Dispatch<ExtImageCaptureSourceV1, ImageCaptureSourceData, D> for ImageCaptureSourceState
where
    D: Dispatch<ExtImageCaptureSourceV1, ImageCaptureSourceData> + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _resource: &ExtImageCaptureSourceV1,
        request: <ExtImageCaptureSourceV1 as Resource>::Request,
        _data: &ImageCaptureSourceData,
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
        _resource: &ExtImageCaptureSourceV1,
        _data: &ImageCaptureSourceData,
    ) {
    }
}

macro_rules! delegate_image_capture_source {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::image_capture_source::v1::server::ext_output_image_capture_source_manager_v1::ExtOutputImageCaptureSourceManagerV1: $crate::wayland::protocols::image_capture_source::OutputImageCaptureSourceManagerGlobalData
        ] => $crate::wayland::protocols::image_capture_source::ImageCaptureSourceState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::image_capture_source::v1::server::ext_output_image_capture_source_manager_v1::ExtOutputImageCaptureSourceManagerV1: ()
        ] => $crate::wayland::protocols::image_capture_source::ImageCaptureSourceState);
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::image_capture_source::v1::server::zcosmic_workspace_image_capture_source_manager_v1::ZcosmicWorkspaceImageCaptureSourceManagerV1: $crate::wayland::protocols::image_capture_source::WorkspaceImageCaptureSourceManagerGlobalData
        ] => $crate::wayland::protocols::image_capture_source::ImageCaptureSourceState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::image_capture_source::v1::server::zcosmic_workspace_image_capture_source_manager_v1::ZcosmicWorkspaceImageCaptureSourceManagerV1: ()
        ] => $crate::wayland::protocols::image_capture_source::ImageCaptureSourceState);
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::image_capture_source::v1::server::ext_foreign_toplevel_image_capture_source_manager_v1::ExtForeignToplevelImageCaptureSourceManagerV1: $crate::wayland::protocols::image_capture_source::ToplevelImageCaptureSourceManagerGlobalData
        ] => $crate::wayland::protocols::image_capture_source::ImageCaptureSourceState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::image_capture_source::v1::server::ext_foreign_toplevel_image_capture_source_manager_v1::ExtForeignToplevelImageCaptureSourceManagerV1: ()
        ] => $crate::wayland::protocols::image_capture_source::ImageCaptureSourceState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::ext::image_capture_source::v1::server::ext_image_capture_source_v1::ExtImageCaptureSourceV1: $crate::wayland::protocols::image_capture_source::ImageCaptureSourceData
        ] => $crate::wayland::protocols::image_capture_source::ImageCaptureSourceState);
    };
}
pub(crate) use delegate_image_capture_source;
