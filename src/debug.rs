// SPDX-License-Identifier: GPL-3.0-only

use crate::state::{Common, Fps};
use smithay::{
    backend::{
        drm::DrmNode,
        renderer::{
            element::texture::TextureRenderElement,
            gles2::{Gles2Error, Gles2Texture},
            glow::GlowRenderer,
        },
    },
    desktop::layer_map_for_output,
    reexports::wayland_server::Resource,
    utils::{IsAlive, Logical, Rectangle},
};

pub fn fps_ui(
    gpu: Option<&DrmNode>,
    state: &Common,
    renderer: &mut GlowRenderer,
    fps: &mut Fps,
    area: Rectangle<i32, Logical>,
    scale: f64,
) -> Result<TextureRenderElement<Gles2Texture>, Gles2Error> {
    use egui::widgets::plot::{Bar, BarChart, HLine, Legend, Plot};

    let (max, min, avg, avg_fps) = (
        fps.max_frametime().as_secs_f64(),
        fps.min_frametime().as_secs_f64(),
        fps.avg_frametime().as_secs_f64(),
        fps.avg_fps(),
    );
    let amount = dbg!(avg_fps.round() as usize * 2);
    let bars = fps
        .frames
        .iter()
        .rev()
        .take(amount)
        .rev()
        .enumerate()
        .map(|(i, (_, d))| {
            let value = d.as_secs_f64();
            let transformed = ((value - min) / (max - min) * 255.0).round() as u8;

            Bar::new(i as f64, transformed as f64).fill(egui::Color32::from_rgb(
                transformed,
                255 - transformed,
                0,
            ))
        })
        .collect::<Vec<_>>();

    fps.state.render(
        |ctx| {
            egui::Area::new("main")
                .anchor(egui::Align2::LEFT_TOP, (10.0, 10.0))
                .show(ctx, |ui| {
                    let label_res = ui.label(format!(
                        "cosmic-comp version {}",
                        std::env!("CARGO_PKG_VERSION")
                    ));
                    if let Some(hash) = std::option_env!("GIT_HASH").and_then(|x| x.get(0..10)) {
                        ui.label(hash);
                    }

                    if !state.egui.active {
                        ui.label("Press Super+Escape for debug menu");
                    } else {
                        ui.set_max_width(400.0);
                        ui.separator();

                        if let Some(gpu) = gpu {
                            ui.label(egui::RichText::new(format!("renderD{}", gpu.minor())).code());
                        }
                        ui.label(egui::RichText::new(format!("FPS: {:>7.3}", avg_fps)).heading());
                        ui.label("Frame Times:");
                        ui.label(egui::RichText::new(format!("avg: {:>7.6}", avg)).code());
                        ui.label(egui::RichText::new(format!("min: {:>7.6}", min)).code());
                        ui.label(egui::RichText::new(format!("max: {:>7.6}", max)).code());
                        let fps_chart = BarChart::new(bars).vertical();

                        Plot::new("FPS")
                            .legend(Legend::default())
                            .view_aspect(50.0)
                            .include_x(0.0)
                            .include_x(amount as f64)
                            .include_y(0.0)
                            .include_y(300)
                            .show_x(false)
                            .show(ui, |plot_ui| {
                                plot_ui.bar_chart(fps_chart);
                                /*
                                plot_ui.hline(
                                    HLine::new(avg)
                                        .highlight(true)
                                        .color(egui::Color32::LIGHT_BLUE),
                                );
                                */
                            });
                    }
                });
        },
        renderer,
        area,
        scale,
        0.8,
        state.clock.now().into(),
    )
}

/*
pub fn debug_ui(
    state: &mut Common,
    area: Rectangle<f64, Physical>,
    scale: f64,
) -> Option<EguiFrame> {
    if !state.egui.active {
        return None;
    }

    Some(state.egui.debug_state.run(
        |ctx| {
            use crate::utils::prelude::*;

            egui::Window::new("Workspaces")
                .default_pos([0.0, 300.0])
                .vscroll(true)
                .collapsible(true)
                .show(ctx, |ui| {
                    use crate::{
                        config::WorkspaceMode as ConfigMode,
                        shell::{OutputBoundState, WorkspaceMode, MAX_WORKSPACES},
                    };

                    ui.set_min_width(250.0);

                    // Mode

                    ui.label(egui::RichText::new("Mode").heading());
                    let mut mode = match &state.shell.workspace_mode {
                        WorkspaceMode::Global { .. } => ConfigMode::Global,
                        WorkspaceMode::OutputBound => ConfigMode::OutputBound,
                    };
                    ui.radio_value(&mut mode, ConfigMode::OutputBound, "Output bound");
                    ui.radio_value(&mut mode, ConfigMode::Global, "Global");
                    state.shell.set_mode(mode);

                    let mode = match &state.shell.workspace_mode {
                        WorkspaceMode::OutputBound => (ConfigMode::OutputBound, None),
                        WorkspaceMode::Global { ref active, .. } => {
                            (ConfigMode::Global, Some(*active))
                        }
                    };
                    match mode {
                        (ConfigMode::OutputBound, _) => {
                            ui.label("Workspaces:");
                            for output in state.shell.outputs().cloned().collect::<Vec<_>>() {
                                ui.horizontal(|ui| {
                                    let active = output
                                        .user_data()
                                        .get::<OutputBoundState>()
                                        .unwrap()
                                        .active
                                        .get();
                                    let mut active_val = active as f64;
                                    ui.label(output.name());
                                    ui.add(
                                        egui::DragValue::new(&mut active_val)
                                            .clamp_range(0..=(MAX_WORKSPACES - 1))
                                            .speed(1.0),
                                    );
                                    if active != active_val as usize {
                                        state.shell.activate(
                                            &state.seats[0],
                                            &output,
                                            active_val as usize,
                                        );
                                    }
                                });
                            }
                        }
                        (ConfigMode::Global, Some(active)) => {
                            ui.horizontal(|ui| {
                                let mut active_val = active as f64;
                                ui.label("Workspace:");
                                ui.add(
                                    egui::DragValue::new(&mut active_val)
                                        .clamp_range(0..=(MAX_WORKSPACES - 1))
                                        .speed(1.0),
                                );
                                if active != active_val as usize {
                                    let output = state.shell.outputs().next().cloned().unwrap();
                                    state.shell.activate(
                                        &state.seats[0],
                                        &output,
                                        active_val as usize,
                                    );
                                }
                            });
                        }
                        _ => unreachable!(),
                    }

                    // Spaces
                    for (i, workspace) in state.shell.spaces.iter().enumerate() {
                        ui.collapsing(format!("Space: {}", i), |ui| {
                            ui.collapsing(format!("Windows"), |ui| {
                                for window in workspace.space.windows() {
                                    ui.collapsing(format!("{:?}", window.toplevel()), |ui| {
                                        ui.label(format!("Rect:         {:?}", {
                                            let mut geo = window.geometry();
                                            geo.loc += workspace
                                                .space
                                                .window_location(window)
                                                .unwrap_or((0, 0).into());
                                            geo
                                        }));
                                        ui.label(format!(
                                            "Bounding box: {:?}",
                                            workspace.space.window_bbox(window)
                                        ));
                                    });
                                }
                            })
                        });
                    }
                });

            egui::Window::new("Outputs")
                .collapsible(true)
                .hscroll(true)
                .default_pos([300.0, 300.0])
                .show(ctx, |ui| {
                    ui.label(format!("Global Space: {:?}", state.shell.global_space()));
                    for output in state
                        .shell
                        .outputs()
                        .cloned()
                        .collect::<Vec<_>>()
                        .into_iter()
                    {
                        ui.separator();
                        ui.collapsing(output.name(), |ui| {
                            ui.label(format!("Mode: {:#?}", output.current_mode()));
                            ui.label(format!("Scale: {:#?}", output.current_scale()));
                            ui.label(format!("Transform: {:#?}", output.current_transform()));
                            ui.label(format!("Geometry: {:?}", output.geometry()));
                            ui.label(format!(
                                "Local Geometry: {:?}",
                                state
                                    .shell
                                    .active_space(&output)
                                    .space
                                    .output_geometry(&output)
                            ));
                            ui.label(format!(
                                "Relative Geometry: {:?}",
                                state
                                    .shell
                                    .space_relative_output_geometry((0i32, 0i32), &output)
                            ));
                            ui.separator();
                            ui.collapsing("Layers:", |ui| {
                                let map = layer_map_for_output(&output);
                                for layer in map.layers() {
                                    ui.collapsing(
                                        format!(
                                            "{}/{:?}",
                                            layer.wl_surface().id(),
                                            layer.wl_surface().client_id()
                                        ),
                                        |ui| {
                                            ui.label(format!(
                                                "Alive: {:?} {:?} {:?}",
                                                layer.alive(),
                                                layer.layer_surface().alive(),
                                                layer.wl_surface().alive()
                                            ));
                                            ui.label(format!("Layer: {:?}", layer.layer()));
                                            ui.label(format!("Namespace: {:?}", layer.namespace()));
                                            ui.label(format!("Geometry: {:?}", layer.bbox()));
                                            ui.label(format!(
                                                "Anchor: {:?}",
                                                layer.cached_state().anchor
                                            ));
                                            ui.label(format!(
                                                "Margin: {:?}",
                                                layer.cached_state().margin
                                            ));
                                            ui.label(format!(
                                                "Exclusive: {:?}",
                                                layer.cached_state().exclusive_zone
                                            ));
                                        },
                                    );
                                }
                                ui.label(format!("{:?}", map));
                            });
                        });
                    }
                });
        },
        area,
        scale,
        state.egui.alpha,
        &state.start_time,
        state.egui.modifiers.clone(),
    ))
}
*/
