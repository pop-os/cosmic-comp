// SPDX-License-Identifier: GPL-3.0-only

use crate::state::{Common, Fps};
use egui::CtxRef;

pub fn debug_ui(ctx: &CtxRef, fps: &Fps, primary: bool) {
    egui::Area::new("main")
        .anchor(egui::Align2::LEFT_TOP, (10.0, 10.0))
        .show(ctx, |ui| {
            use egui::widgets::plot::{Bar, BarChart, HLine, Legend, Plot};

            // Basics

            let label_res = ui.label(format!(
                "cosmic-comp version {}",
                std::env!("CARGO_PKG_VERSION")
            ));
            ui.set_max_width(label_res.rect.width());
            if let Some(hash) = std::option_env!("GIT_HASH").and_then(|x| x.get(0..8)) {
                ui.label(hash);
            }
            ui.separator();

            // FPS

            let (max, min, avg) = (
                fps.max().as_secs_f64(),
                fps.min().as_secs_f64(),
                fps.avg_fps(),
            );
            let fps_chart = BarChart::new(
                fps.frames
                    .iter()
                    .enumerate()
                    .map(|(i, duration)| {
                        let value = duration.as_secs_f64();
                        let transformed = ((value - min) / (max - min) * 255.0).round() as u8;
                        Bar::new(i as f64, value + 1.0).fill(egui::Color32::from_rgb(
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
                .include_x(100.0)
                .include_y(0.0)
                .include_y(max + 1.0)
                .show_x(false)
                .show(ui, |plot_ui| {
                    plot_ui.bar_chart(fps_chart);
                    plot_ui.hline(
                        HLine::new(avg)
                            .highlight()
                            .name(format!("AVG {}", avg.round() as i32))
                            .color(egui::Color32::DARK_BLUE),
                    );
                });
        });
}
