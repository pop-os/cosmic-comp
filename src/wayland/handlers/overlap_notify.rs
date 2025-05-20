use smithay::{
    desktop::{layer_map_for_output, LayerSurface, WindowSurfaceType},
    output::Output,
    reexports::wayland_protocols_wlr::layer_shell::v1::server::zwlr_layer_surface_v1::ZwlrLayerSurfaceV1,
};

use crate::{
    state::State,
    wayland::protocols::overlap_notify::{
        delegate_overlap_notify, OverlapNotifyHandler, OverlapNotifyState,
    },
};

impl OverlapNotifyHandler for State {
    fn overlap_notify_state(&mut self) -> &mut OverlapNotifyState {
        &mut self.common.overlap_notify_state
    }

    fn layer_surface_from_resource(&self, resource: ZwlrLayerSurfaceV1) -> Option<LayerSurface> {
        self.common
            .layer_shell_state
            .layer_surfaces()
            .find(|l| l.shell_surface() == &resource)
            .and_then(|l| {
                let shell = self.common.shell.read();
                let outputs = shell.outputs();
                let ret = outputs.map(|o| layer_map_for_output(o)).find_map(|s| {
                    s.layer_for_surface(l.wl_surface(), WindowSurfaceType::ALL)
                        .cloned()
                });
                drop(shell);
                ret
            })
    }

    fn outputs(&self) -> impl Iterator<Item = Output> {
        let shell = self.common.shell.read();
        shell.outputs().cloned().collect::<Vec<_>>().into_iter()
    }

    fn active_workspaces(
        &self,
    ) -> impl Iterator<Item = crate::wayland::protocols::workspace::WorkspaceHandle> {
        let shell = self.common.shell.read();
        shell
            .workspaces
            .sets
            .iter()
            .map(|(_, set)| set.workspaces[set.active].handle)
            .collect::<Vec<_>>()
            .into_iter()
    }
}

delegate_overlap_notify!(State);
