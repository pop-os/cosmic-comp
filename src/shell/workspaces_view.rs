use crate::backend::render::CursorMode;
use crate::backend::render::ElementFilter;
use crate::backend::render::element::CosmicElement;
use crate::backend::render::{element::AsGlowRenderer, workspace_elements};
use crate::shell::CosmicMappedRenderElement;
use crate::shell::Shell;
use crate::shell::WorkspaceRenderElement;
use crate::wayland::protocols::workspace::WorkspaceHandle;
use smithay::backend::drm::DrmNode;
use smithay::backend::renderer::element::utils::{
    CropRenderElement, RelocateRenderElement, RescaleRenderElement, constrain_render_elements,
};
use smithay::backend::renderer::{
    Renderer,
    element::{AsRenderElements, RenderElement, surface::WaylandSurfaceRenderElement},
};
use smithay::output::Output;
use smithay::utils::{Monotonic, Physical, Point, Scale, Time};
use std::sync::Arc;

enum WorkspacesViewRenderElement<R>
where
    R: AsGlowRenderer,
    R::TextureId: Send + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    Workspace(CropRenderElement<RelocateRenderElement<RescaleRenderElement<CosmicElement<R>>>>),
    // TODO show windows or stacks?
    Mapped(
        CropRenderElement<
            RelocateRenderElement<RescaleRenderElement<CosmicMappedRenderElement<R>>>,
        >,
    ),
}

#[derive(Debug)]
pub struct WorkspacesViewState {}

impl WorkspacesViewState {
    pub fn new() -> Self {
        Self {}
    }

    pub fn render<R>(
        &self,
        gpu: Option<&DrmNode>,
        renderer: &mut R,
        shell: &Arc<parking_lot::RwLock<Shell>>,
        now: Time<Monotonic>,
        output: &Output,
        scanout_node: Option<DrmNode>,
    ) -> Vec<WaylandSurfaceRenderElement<R>>
    where
        R: AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
        CosmicElement<R>: RenderElement<R>,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        WorkspaceRenderElement<R>: RenderElement<R>,
    {
        let shell_read = shell.read();
        let Some(workspace_set) = shell_read.workspaces.sets.get(output) else {
            return Vec::new();
        };
        for (i, workspace) in workspace_set.workspaces.iter().enumerate() {
            workspace_elements(
                gpu,
                renderer,
                shell,
                None,
                now,
                output,
                None,
                (workspace.handle, i),
                CursorMode::None,
                ElementFilter::ExcludeWorkspaceOverview,
                scanout_node,
            );
        }
        // TODO
        // list of workspaces
        // list of toplevels
        Vec::new()
    }
}

// TODO keyboard target
// TODO touch target
// TODO tablet tartget
