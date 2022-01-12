// SPDX-License-Identifier: GPL-3.0-only

use crate::state::{Common, Fps};
use smithay_egui::EguiFrame;
use smithay::utils::{Rectangle, Logical};

pub fn debug_ui(state: &mut Common, fps: &Fps, area: Rectangle<i32, Logical>, scale: f64, primary: bool) -> EguiFrame {
    let size = area.size;
    let alpha = state.egui.alpha;

    state.egui.state.run(
        |ctx| {
            egui::Area::new("main")
                .anchor(egui::Align2::LEFT_TOP, (10.0, 10.0))
                .show(ctx, |ui| {
                    use egui::widgets::plot::{Bar, BarChart, HLine, Legend, Plot};

                    // Basics

                    let label_res = ui.label(format!(
                        "cosmic-comp version {}",
                        std::env!("CARGO_PKG_VERSION")
                    ));
                    if let Some(hash) = std::option_env!("GIT_HASH").and_then(|x| x.get(0..8)) {
                        ui.label(hash);
                    }

                    if !state.egui.active {
                        ui.label("Press Mod+Escape for debug menu");
                    } else {
                        ui.set_max_width(label_res.rect.width());
                        ui.separator();

                        // FPS
                        
                        let (max, min, avg, avg_fps) = (
                            fps.max_frametime().as_secs_f64(),
                            fps.min_frametime().as_secs_f64(),
                            fps.avg_frametime().as_secs_f64(),
                            fps.avg_fps(),
                        );

                        ui.label(egui::RichText::new(format!("FPS: {:>7.3}", avg_fps)).heading());
                        ui.label("Frame Times:");
                        ui.label(egui::RichText::new(format!("avg: {:>7.6}", avg)).code());
                        ui.label(egui::RichText::new(format!("min: {:>7.6}", min)).code());
                        ui.label(egui::RichText::new(format!("max: {:>7.6}", max)).code());
                        let fps_chart = BarChart::new(
                            fps.frames
                                .iter()
                                .rev().take(30).rev()
                                .enumerate()
                                .map(|(i, d)| {
                                    let value = d.as_secs_f64();
                                    let transformed = ((value - min) / (max - min) * 255.0).round() as u8;
                                    Bar::new(i as f64, value).fill(egui::Color32::from_rgb(
                                        transformed,
                                        255 - transformed,
                                        0,
                                    ))
                                })
                                .collect(),
                        )
                        .vertical();

                        Plot::new("FPS")
                            .legend(Legend::default())
                            .view_aspect(33.0)
                            .include_x(0.0)
                            .include_x(30.0)
                            .include_y(0.0)
                            .include_y(max * 2.0)
                            .show_x(false)
                            .show(ui, |plot_ui| {
                                plot_ui.bar_chart(fps_chart);
                                plot_ui.hline(
                                    HLine::new(avg)
                                        .highlight()
                                        .color(egui::Color32::LIGHT_BLUE),
                                );
                            });
                        
                        ui.separator();

                        // Toggles and stuff
                        ui.add(egui::Slider::new(&mut state.egui.alpha, 0.1..=1.0).clamp_to_range(true).text("Opacity"));
                        ui.checkbox(&mut state.egui.spaces, "Workspace UI");
                        //TODO: ui.checkbox(&mut state.egui.outputs, "Outputs UI");
                    }
                });
                        
            // don't show these one others then the primary monitor
            if primary {
                egui::Window::new("Workspaces")
                    .open(&mut state.egui.spaces)
                    .vscroll(true)
                    .show(ctx, |ui| {
                        use crate::shell::workspaces::{ActiveWorkspace, Mode, MAX_WORKSPACES};

                        ui.set_min_width(250.0);

                        // Mode 

                        ui.label(egui::RichText::new("Mode").heading());
                        let mut mode = *state.spaces.mode();
                        let active = if let Mode::Global { active } = mode { active } else { 0 };
                        ui.radio_value(&mut mode, Mode::OutputBound, "Output bound");
                        ui.radio_value(&mut mode, Mode::Global { active }, "Global");
                        state.spaces.set_mode(mode);

                        match *state.spaces.mode() {
                            Mode::OutputBound => {
                                ui.label("Workspaces:");
                                for output in state.spaces.outputs().cloned().collect::<Vec<_>>() {
                                    ui.horizontal(|ui| {
                                        let active = output
                                            .user_data()
                                            .get::<ActiveWorkspace>()
                                            .unwrap()
                                            .get()
                                            .unwrap();
                                        let mut active_val = active as f64;
                                        ui.label(output.name());
                                        ui.add(egui::DragValue::new(&mut active_val).clamp_range(0..=(MAX_WORKSPACES-1)).speed(1.0));
                                        if active != active_val as usize {
                                            state.spaces.activate(&output, active_val as usize);
                                        }
                                    });
                                }
                            },
                            Mode::Global { active } => {
                                ui.horizontal(|ui| {
                                    let mut active_val = active as f64;
                                    ui.label("Workspace:");
                                    ui.add(egui::DragValue::new(&mut active_val).clamp_range(0..=(MAX_WORKSPACES-1)).speed(1.0));
                                    if active != active_val as usize {
                                        let output = state.spaces.outputs().next().cloned().unwrap();
                                        state.spaces.activate(&output, active_val as usize);
                                    }
                                });
                            }
                        }

                        // Spaces
                        for (i, space) in state.spaces.spaces.iter().enumerate() {
                            ui.collapsing(format!("Space: {}", i), |ui| {
                                ui.collapsing(format!("Windows"), |ui| {
                                    for window in space.windows() {
                                        ui.collapsing(format!("{:?}", window.toplevel()), |ui| {
                                            ui.label(format!("Rect:         {:?}", space.window_geometry(window)));
                                            ui.label(format!("Bounding box: {:?}", space.window_bbox(window)));
                                        });
                                    }
                                })
                            });
                        }
                    });
                
                egui::Window::new("Outputs")
                    .open(&mut state.egui.outputs)
                    .hscroll(true)
                    .show(ctx, |ui| {

                    });
            }
        },
        area,
        size.to_f64().to_physical(scale).to_i32_round(),
        scale,
        alpha,
        &state.start_time,
        state.egui.modifiers.clone(),
    )
}
