use super::{
    workspace::{WorkspaceHandle, WorkspaceHandler},
};
use crate::shell::element::surface::WeakCosmicSurface;
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
    wayland::{image_capture_source::{ImageCaptureSource, ImageCaptureSourceData}, Dispatch2, GlobalDispatch2},
};
use wayland_backend::server::GlobalId;

#[derive(Debug)]
pub struct CosmicImageCaptureSourceState {
    workspace_source_global: GlobalId,
}

pub struct WorkspaceImageCaptureSourceManagerGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

#[derive(Debug, Clone)]
pub enum ImageCaptureSourceKind {
    Output(WeakOutput),
    Workspace(WorkspaceHandle),
    Toplevel(WeakCosmicSurface),
    Destroyed,
}

impl ImageCaptureSourceKind {
    pub fn from_source(source: &ImageCaptureSource) -> Self {
        // If no user-data, assume source was created for a destroyed output, etc.
        source
            .user_data()
            .get::<ImageCaptureSourceKind>()
            .cloned()
            .unwrap_or(Self::Destroyed)
    }
}

impl CosmicImageCaptureSourceState {
    pub fn new<D, F>(display: &DisplayHandle, client_filter: F) -> CosmicImageCaptureSourceState
    where
        D: GlobalDispatch<
                ZcosmicWorkspaceImageCaptureSourceManagerV1,
                WorkspaceImageCaptureSourceManagerGlobalData,
            > + Dispatch<ZcosmicWorkspaceImageCaptureSourceManagerV1, CaptureSourceManagerData>
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

pub struct CaptureSourceManagerData;

impl<D> GlobalDispatch2<ZcosmicWorkspaceImageCaptureSourceManagerV1, D>
    for WorkspaceImageCaptureSourceManagerGlobalData
where
    D: Dispatch<ZcosmicWorkspaceImageCaptureSourceManagerV1, CaptureSourceManagerData>
        + Dispatch<ExtImageCaptureSourceV1, ImageCaptureSourceData>
        + 'static,
{
    fn bind(
        &self,
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<ZcosmicWorkspaceImageCaptureSourceManagerV1>,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, CaptureSourceManagerData);
    }

    fn can_view(&self, client: &Client) -> bool {
        (self.filter)(client)
    }
}

impl<D> Dispatch2<ZcosmicWorkspaceImageCaptureSourceManagerV1, D> for CaptureSourceManagerData
where
    D: Dispatch<ExtImageCaptureSourceV1, ImageCaptureSourceData> + WorkspaceHandler + 'static,
{
    fn request(
        &self,
        state: &mut D,
        _client: &Client,
        _resource: &ZcosmicWorkspaceImageCaptureSourceManagerV1,
        request: <ZcosmicWorkspaceImageCaptureSourceManagerV1 as Resource>::Request,
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
