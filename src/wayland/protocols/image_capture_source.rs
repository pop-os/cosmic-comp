use super::{
    workspace::{WorkspaceHandle, WorkspaceHandler},
};
use crate::{
    shell::CosmicSurface,
};
use cosmic_protocols::image_capture_source::v1::server::{
    zcosmic_workspace_image_capture_source_manager_v1::{
        Request as CosmicWorkspaceSourceRequest, ZcosmicWorkspaceImageCaptureSourceManagerV1,
    },
};
use smithay::reexports::wayland_protocols::ext::image_capture_source::v1::server::{
    ext_image_capture_source_v1::ExtImageCaptureSourceV1,
};
use smithay::{
    output::WeakOutput,
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource,
    },
    wayland::image_capture_source::{ImageCaptureSource, ImageCaptureSourceData},
};
use wayland_backend::server::GlobalId;

#[derive(Debug)]
pub struct CosmicImageCaptureSourceState {
    workspace_source_global: GlobalId,
}

pub struct WorkspaceImageCaptureSourceManagerGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImageCaptureSourceKind {
    Output(WeakOutput),
    Workspace(WorkspaceHandle),
    Toplevel(CosmicSurface),
    Destroyed,
}

impl ImageCaptureSourceKind {
    pub fn from_resource(resource: &ExtImageCaptureSourceV1) -> Option<Self> {
        let source = ImageCaptureSource::from_resource(resource)?;
        source.user_data().get::<ImageCaptureSourceKind>().cloned()
    }
}

impl CosmicImageCaptureSourceState {
    pub fn new<D, F>(display: &DisplayHandle, client_filter: F) -> CosmicImageCaptureSourceState
    where
        D: GlobalDispatch<
                ZcosmicWorkspaceImageCaptureSourceManagerV1,
                WorkspaceImageCaptureSourceManagerGlobalData,
            > + Dispatch<ZcosmicWorkspaceImageCaptureSourceManagerV1, ()>
            + Dispatch<ExtImageCaptureSourceV1, ImageCaptureSourceData>
            + WorkspaceHandler
            + 'static,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + Clone + 'static,
    {
        CosmicImageCaptureSourceState {
            workspace_source_global: display
                .create_global::<D, ZcosmicWorkspaceImageCaptureSourceManagerV1, _>(
                    1,
                    WorkspaceImageCaptureSourceManagerGlobalData {
                        filter: Box::new(client_filter.clone()),
                    },
                ),
        }
    }

    pub fn workspace_source_id(&self) -> &GlobalId {
        &self.workspace_source_global
    }
}

impl<D>
    GlobalDispatch<
        ZcosmicWorkspaceImageCaptureSourceManagerV1,
        WorkspaceImageCaptureSourceManagerGlobalData,
        D,
    > for CosmicImageCaptureSourceState
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

impl<D> Dispatch<ZcosmicWorkspaceImageCaptureSourceManagerV1, (), D>
    for CosmicImageCaptureSourceState
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
        if let CosmicWorkspaceSourceRequest::CreateSource {
            source: source_handle,
            output,
        } = request
        {
            let data = match state.workspace_state().get_ext_workspace_handle(&output) {
                Some(workspace) => ImageCaptureSourceKind::Workspace(workspace),
                None => ImageCaptureSourceKind::Destroyed,
            };
            let source = ImageCaptureSource::new();
            source.user_data().insert_if_missing(|| data);
            let instance = data_init.init(
                source_handle,
                ImageCaptureSourceData {
                    source: source.clone(),
                },
            );
            source.add_instance(&instance);
        }
    }
}

macro_rules! delegate_cosmic_image_capture_source {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::image_capture_source::v1::server::zcosmic_workspace_image_capture_source_manager_v1::ZcosmicWorkspaceImageCaptureSourceManagerV1: $crate::wayland::protocols::image_capture_source::WorkspaceImageCaptureSourceManagerGlobalData
        ] => $crate::wayland::protocols::image_capture_source::CosmicImageCaptureSourceState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::image_capture_source::v1::server::zcosmic_workspace_image_capture_source_manager_v1::ZcosmicWorkspaceImageCaptureSourceManagerV1: ()
        ] => $crate::wayland::protocols::image_capture_source::CosmicImageCaptureSourceState);
    };
}
pub(crate) use delegate_cosmic_image_capture_source;
