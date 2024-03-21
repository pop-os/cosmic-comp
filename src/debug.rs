// SPDX-License-Identifier: GPL-3.0-only

use std::collections::HashMap;

use crate::{
    shell::focus::target::{KeyboardFocusTarget, PointerFocusTarget},
    state::{Common, Fps},
};
use egui::{load::SizedTexture, Color32, Vec2};
use smithay::{
    backend::{
        drm::DrmNode,
        renderer::{
            element::texture::TextureRenderElement,
            gles::{GlesError, GlesTexture},
            glow::GlowRenderer,
        },
    },
    desktop::WindowSurface,
    input::keyboard::xkb,
    reexports::wayland_server::Resource,
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
) -> Result<TextureRenderElement<GlesTexture>, GlesError> {
    use egui_plot::{Bar, BarChart, Legend, Plot};

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
                                        ui.image(SizedTexture::new(*texture_id, size));
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
                        for seat in state.seats() {
                            ui.label(egui::RichText::new(format!("\t{}", seat.name())).strong());
                            if let Some(ptr) = seat.get_pointer() {
                                egui::Frame::none()
                                    .fill(egui::Color32::DARK_GRAY)
                                    .rounding(5.)
                                    .inner_margin(10.)
                                    .show(ui, |ui| {
                                        ui.label(
                                            egui::RichText::new(format!(
                                                "Pos: {:?}",
                                                ptr.current_location()
                                            ))
                                            .code(),
                                        );
                                        ui.label(
                                            egui::RichText::new(format!(
                                                "Focus: {}",
                                                format_pointer_focus(ptr.current_focus())
                                            ))
                                            .code(),
                                        );
                                        ui.label(
                                            egui::RichText::new(format!(
                                                "Grabbed: {:?}",
                                                ptr.is_grabbed()
                                            ))
                                            .code(),
                                        );
                                    });
                            }
                            if let Some(kbd) = seat.get_keyboard() {
                                egui::Frame::none()
                                    .fill(egui::Color32::DARK_GRAY)
                                    .rounding(5.)
                                    .inner_margin(10.)
                                    .show(ui, |ui| {
                                        let mut keysyms = format!(
                                            "Keys: {:?}",
                                            kbd.with_pressed_keysyms(|syms| syms
                                                .into_iter()
                                                .map(|k| xkb::keysym_get_name(k.modified_sym()))
                                                .fold(String::new(), |mut list, val| {
                                                    list.push_str(&format!("{}, ", val));
                                                    list
                                                }))
                                        );
                                        keysyms.truncate(keysyms.len().saturating_sub(2));
                                        ui.label(egui::RichText::new(keysyms).code());

                                        let mods = kbd.modifier_state();
                                        ui.label(
                                            egui::RichText::new(format!(
                                                "Mods: Ctrl {} / Alt {} / Logo {} / Shift {}",
                                                mods.ctrl, mods.alt, mods.logo, mods.shift,
                                            ))
                                            .code(),
                                        );

                                        ui.label(
                                            egui::RichText::new(format!(
                                                "Focus: {}",
                                                format_keyboard_focus(kbd.current_focus())
                                            ))
                                            .code(),
                                        );
                                        ui.label(
                                            egui::RichText::new(format!(
                                                "Grabbed: {:?}",
                                                kbd.is_grabbed()
                                            ))
                                            .code(),
                                        );
                                    });
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

fn format_pointer_focus(focus: Option<PointerFocusTarget>) -> String {
    use PointerFocusTarget::*;

    match focus {
        Some(Element(x)) => match x {
            x if x.is_stack() => format!(
                "Stacked Window {} ({})",
                match x.active_window().0.underlying_surface() {
                    WindowSurface::Wayland(t) => t.wl_surface().id().protocol_id(),
                    WindowSurface::X11(x) => x.window_id(),
                },
                x.active_window().title()
            ),
            x if x.is_window() => format!(
                "Window {} ({})",
                match x.active_window().0.underlying_surface() {
                    WindowSurface::Wayland(t) => t.wl_surface().id().protocol_id(),
                    WindowSurface::X11(x) => x.window_id(),
                },
                x.active_window().title()
            ),
            _ => unreachable!(),
        },
        Some(Fullscreen(x)) => format!(
            "Fullscreen {} ({})",
            match x.0.underlying_surface() {
                WindowSurface::Wayland(t) => t.wl_surface().id().protocol_id(),
                WindowSurface::X11(x) => x.window_id(),
            },
            x.title()
        ),
        Some(LayerSurface(x)) => format!("LayerSurface {}", x.wl_surface().id().protocol_id()),
        Some(Popup(x)) => format!("Popup {}", x.wl_surface().id().protocol_id()),
        Some(OverrideRedirect(x)) => format!("Override Redirect {}", x.window_id()),
        Some(PointerFocusTarget::ResizeFork(x)) => format!("Resize Fork {:?}", x.node),
        Some(LockSurface(x)) => format!("LockSurface {}", x.wl_surface().id().protocol_id()),
        None => format!("None"),
    }
}

fn format_keyboard_focus(focus: Option<KeyboardFocusTarget>) -> String {
    use KeyboardFocusTarget::*;

    match focus {
        Some(Element(x)) => match x {
            x if x.is_stack() => format!(
                "Stacked Window {} ({})",
                match x.active_window().0.underlying_surface() {
                    WindowSurface::Wayland(t) => t.wl_surface().id().protocol_id(),
                    WindowSurface::X11(x) => x.window_id(),
                },
                x.active_window().title()
            ),
            x if x.is_window() => format!(
                "Window {} ({})",
                match x.active_window().0.underlying_surface() {
                    WindowSurface::Wayland(t) => t.wl_surface().id().protocol_id(),
                    WindowSurface::X11(x) => x.window_id(),
                },
                x.active_window().title()
            ),
            _ => unreachable!(),
        },
        Some(Fullscreen(x)) => format!(
            "Fullscreen {} ({})",
            match x.0.underlying_surface() {
                WindowSurface::Wayland(t) => t.wl_surface().id().protocol_id(),
                WindowSurface::X11(x) => x.window_id(),
            },
            x.title()
        ),
        Some(LayerSurface(x)) => format!("LayerSurface {}", x.wl_surface().id().protocol_id()),
        Some(Popup(x)) => format!("Popup {}", x.wl_surface().id().protocol_id()),
        Some(Group(_)) => format!("Window Group"),
        Some(LockSurface(x)) => format!("LockSurface {}", x.wl_surface().id().protocol_id()),
        None => format!("None"),
    }
}
