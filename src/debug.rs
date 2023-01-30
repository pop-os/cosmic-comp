// SPDX-License-Identifier: GPL-3.0-only

use std::collections::HashMap;

use crate::state::{Common, Fps};
use egui::{Color32, Vec2};
use smithay::{
    backend::{
        drm::DrmNode,
        renderer::{
            element::texture::TextureRenderElement,
            gles2::{Gles2Error, Gles2Texture},
            glow::GlowRenderer,
        },
    },
    utils::{Logical, Rectangle},
};

pub const ELEMENTS_COLOR: Color32 = Color32::from_rgb(70, 198, 115);
pub const RENDER_COLOR: Color32 = Color32::from_rgb(29, 114, 58);
pub const SCREENCOPY_COLOR: Color32 = Color32::from_rgb(253, 178, 39);
pub const DISPLAY_COLOR: Color32 = Color32::from_rgb(41, 184, 209);

pub fn fps_ui(
    gpu: Option<&DrmNode>,
    state: &Common,
    renderer: &mut GlowRenderer,
    fps: &mut Fps,
    area: Rectangle<i32, Logical>,
    scale: f64,
) -> Result<TextureRenderElement<Gles2Texture>, Gles2Error> {
    use egui::widgets::plot::{Bar, BarChart, Legend, Plot};

    let (max, min, avg, avg_fps) = (
        fps.max_frametime().as_secs_f64(),
        fps.min_frametime().as_secs_f64(),
        fps.avg_frametime().as_secs_f64(),
        fps.avg_fps(),
    );
    let (max_disp, min_disp) = (
        fps.max_time_to_display().as_secs_f64(),
        fps.min_time_to_display().as_secs_f64(),
    );

    let amount = avg_fps.round() as usize * 2;
    let ((bars_elements, bars_render), (bars_screencopy, bars_displayed)): (
        (Vec<Bar>, Vec<Bar>),
        (Vec<Bar>, Vec<Bar>),
    ) = fps
        .frames
        .iter()
        .rev()
        .take(amount)
        .rev()
        .enumerate()
        .map(|(i, frame)| {
            let elements_val = frame.duration_elements.as_secs_f64();
            let render_val = frame.duration_render.as_secs_f64();
            let screencopy_val = frame
                .duration_screencopy
                .as_ref()
                .map(|val| val.as_secs_f64())
                .unwrap_or(0.0);
            let displayed_val = frame.duration_displayed.as_secs_f64();

            let transformed_elements =
                ((elements_val - min_disp) / (max_disp - min_disp) * 255.0).round() as u8;
            let transformed_render =
                ((render_val - min_disp) / (max_disp - min_disp) * 255.0).round() as u8;
            let transformed_screencopy =
                ((screencopy_val - min_disp) / (max_disp - min_disp) * 255.0).round() as u8;
            let transformed_displayed =
                ((displayed_val - min_disp) / (max_disp - min_disp) * 255.0).round() as u8;
            (
                (
                    Bar::new(i as f64, transformed_elements as f64).fill(ELEMENTS_COLOR),
                    Bar::new(i as f64, transformed_render as f64).fill(RENDER_COLOR),
                ),
                (
                    Bar::new(i as f64, transformed_screencopy as f64).fill(SCREENCOPY_COLOR),
                    Bar::new(i as f64, transformed_displayed as f64).fill(DISPLAY_COLOR),
                ),
            )
        })
        .unzip();

    let vendors = HashMap::from([
        (
            "0x10de",
            fps.state
                .with_image(renderer, "nvidia", |image, ctx| {
                    (image.texture_id(ctx), image.size_vec2())
                })
                .expect("Logo images not loaded?"),
        ),
        (
            "0x1002",
            fps.state
                .with_image(renderer, "amd", |image, ctx| {
                    (image.texture_id(ctx), image.size_vec2())
                })
                .expect("Logo images not loaded?"),
        ),
        (
            "0x8086",
            fps.state
                .with_image(renderer, "intel", |image, ctx| {
                    (image.texture_id(ctx), image.size_vec2())
                })
                .expect("Logo images not loaded?"),
        ),
    ]);

    fps.state.render(
        |ctx| {
            egui::Area::new("main")
                .anchor(egui::Align2::LEFT_TOP, (10.0, 10.0))
                .show(ctx, |ui| {
                    ui.label(format!(
                        "cosmic-comp version {}",
                        std::env!("CARGO_PKG_VERSION")
                    ));
                    if let Some(hash) = std::option_env!("GIT_HASH").and_then(|x| x.get(0..10)) {
                        ui.label(format!(": {hash}"));
                    }

                    if !state.egui.active {
                        ui.label("Press Super+Escape for debug overlay");
                    } else {
                        ui.set_max_width(300.0);
                        ui.separator();

                        if let Some(gpu) = gpu {
                            ui.horizontal(|ui| {
                                let resp = ui.label(
                                    egui::RichText::new(format!("renderD{}", gpu.minor())).code(),
                                );
                                if let Ok(vendor) = std::fs::read_to_string(format!(
                                    "/sys/class/drm/renderD{}/device/vendor",
                                    gpu.minor()
                                )) {
                                    if let Some((texture_id, mut size)) = vendors.get(vendor.trim())
                                    {
                                        let factor = resp.rect.height() / size.y;
                                        size = Vec2::from([size.x * factor, resp.rect.height()]);
                                        ui.image(*texture_id, size);
                                    }
                                }
                            });
                        }
                        ui.label(egui::RichText::new(format!("FPS: {:>7.3}", avg_fps)).heading());
                        ui.label("Frame Times:");
                        ui.label(egui::RichText::new(format!("avg: {:>7.6}", avg)).code());
                        ui.label(egui::RichText::new(format!("min: {:>7.6}", min)).code());
                        ui.label(egui::RichText::new(format!("max: {:>7.6}", max)).code());
                        let elements_chart = BarChart::new(bars_elements).vertical();
                        let render_chart = BarChart::new(bars_render)
                            .stack_on(&[&elements_chart])
                            .vertical();
                        let screencopy_chart = BarChart::new(bars_screencopy)
                            .stack_on(&[&elements_chart, &render_chart])
                            .vertical();
                        let display_chart = BarChart::new(bars_displayed)
                            .stack_on(&[&elements_chart, &render_chart, &screencopy_chart])
                            .vertical();

                        Plot::new("FPS")
                            .legend(Legend::default())
                            .view_aspect(50.0)
                            .include_x(0.0)
                            .include_x(amount as f64)
                            .include_y(0.0)
                            .include_y(300)
                            .show_x(false)
                            .show(ui, |plot_ui| {
                                plot_ui.bar_chart(elements_chart);
                                plot_ui.bar_chart(render_chart);
                                plot_ui.bar_chart(screencopy_chart);
                                plot_ui.bar_chart(display_chart);
                            });

                        ui.separator();
                        ui.label(egui::RichText::new("Input States").heading());
                        for (num, seat) in state.seats().enumerate() {
                            ui.label(egui::RichText::new(format!("\tseat-{}", num)).strong());
                            if let Some(ptr) = seat.get_pointer() {
                                ui.label(egui::RichText::new(format!("{:#?}", ptr)).code());
                            }
                            if let Some(kbd) = seat.get_keyboard() {
                                ui.label(egui::RichText::new(format!("{:#?}", kbd)).code());
                            }
                        }
                    }
                });
        },
        renderer,
        area,
        scale,
        0.8,
    )
}
