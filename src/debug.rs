// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    State,
    backend::kms::Timings,
    shell::focus::target::{KeyboardFocusTarget, PointerFocusTarget, PointerFocusToplevel},
};
use egui::Color32;
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
    input::{Seat, keyboard::xkb},
    reexports::wayland_server::Resource,
    utils::{Logical, Rectangle, Time},
};
use smithay_egui::EguiState;

pub const ELEMENTS_COLOR: Color32 = Color32::from_rgb(70, 198, 115);
pub const RENDER_COLOR: Color32 = Color32::from_rgb(29, 114, 58);
pub const SUBMITTED_COLOR: Color32 = Color32::from_rgb(253, 178, 39);
pub const DISPLAY_COLOR: Color32 = Color32::from_rgb(41, 184, 209);

pub fn fps_ui<'a>(
    gpu: Option<&DrmNode>,
    debug_active: bool,
    seats: &[Seat<State>],
    renderer: &mut GlowRenderer,
    state: &EguiState,
    timings: &Timings,
    area: Rectangle<i32, Logical>,
    scale: f64,
) -> Result<TextureRenderElement<GlesTexture>, GlesError> {
    use egui_plot::{Bar, BarChart, Legend, Plot};

    let (max, min, avg, avg_fps) = (
        timings.max_rendertime().as_secs_f64(),
        timings.min_rendertime().as_secs_f64(),
        timings.avg_rendertime().as_secs_f64(),
        timings.avg_fps(),
    );

    let amount = avg_fps.round() as usize * 2;
    let (max_disp, min_disp, avg_disp) = (
        timings.max_frametime(amount).as_secs_f64(),
        timings.min_frametime(amount).as_secs_f64(),
        timings
            .avg_frametime(amount)
            .unwrap_or_default()
            .as_secs_f64(),
    );

    let ((bars_elements, bars_render), (bars_submitted, bars_displayed)): (
        (Vec<Bar>, Vec<Bar>),
        (Vec<Bar>, Vec<Bar>),
    ) = timings
        .previous_frames
        .iter()
        .rev()
        .take(amount)
        .rev()
        .enumerate()
        .map(|(i, frame)| {
            let elements_val = frame.render_duration_elements.as_secs_f64();
            let render_val = frame.render_duration_draw.as_secs_f64();
            let submitted_val =
                Time::elapsed(&frame.render_start, frame.presentation_submitted).as_secs_f64();
            let displayed_val =
                Time::elapsed(&frame.render_start, frame.presentation_presented).as_secs_f64();

            let transformed_elements =
                ((elements_val - min_disp) / (max_disp - min_disp) * 255.0).round() as u8;
            let transformed_render =
                ((render_val - min_disp) / (max_disp - min_disp) * 255.0).round() as u8;
            let transformed_submitted =
                ((submitted_val - min_disp) / (max_disp - min_disp) * 255.0).round() as u8;
            let transformed_displayed =
                ((displayed_val - min_disp) / (max_disp - min_disp) * 255.0).round() as u8;
            (
                (
                    Bar::new(i as f64, transformed_elements as f64).fill(ELEMENTS_COLOR),
                    Bar::new(i as f64, transformed_render as f64).fill(RENDER_COLOR),
                ),
                (
                    Bar::new(i as f64, transformed_submitted as f64).fill(SUBMITTED_COLOR),
                    Bar::new(i as f64, transformed_displayed as f64).fill(DISPLAY_COLOR),
                ),
            )
        })
        .unzip();

    state.render(
        |ctx| {
            egui::Area::new("main".into())
                .anchor(egui::Align2::LEFT_TOP, (10.0, 10.0))
                .show(ctx, |ui| {
                    ui.label(format!(
                        "cosmic-comp version {}",
                        std::env!("CARGO_PKG_VERSION")
                    ));
                    if let Some(hash) = std::option_env!("GIT_HASH").and_then(|x| x.get(0..10)) {
                        ui.label(format!(": {hash}"));
                    }

                    if !debug_active {
                        ui.label("Press Super+Ctrl+Escape for debug overlay");
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
                                    if let Some(img) = match vendor.trim() {
                                        "0x10de" => Some(egui::include_image!(
                                            "../resources/icons/nvidia.svg"
                                        )),
                                        "0x1002" => {
                                            Some(egui::include_image!("../resources/icons/amd.svg"))
                                        }
                                        "0x8086" => Some(egui::include_image!(
                                            "../resources/icons/intel.svg"
                                        )),
                                        _ => None,
                                    } {
                                        ui.add(
                                            egui::Image::new(img).max_height(resp.rect.height()),
                                        );
                                    }
                                }
                            });
                        }
                        ui.label(egui::RichText::new(format!("VRR: {}", timings.vrr())).code());
                        ui.label(egui::RichText::new(format!("FPS: {:>7.3}", avg_fps)).heading());
                        ui.label("Render Times:");
                        ui.label(egui::RichText::new(format!("avg: {:>7.6}", avg)).code());
                        ui.label(egui::RichText::new(format!("min: {:>7.6}", min)).code());
                        ui.label(egui::RichText::new(format!("max: {:>7.6}", max)).code());
                        ui.label("Frame Times:");
                        ui.label(egui::RichText::new(format!("avg: {:>7.6}", avg_disp)).code());
                        ui.label(egui::RichText::new(format!("min: {:>7.6}", min_disp)).code());
                        ui.label(egui::RichText::new(format!("max: {:>7.6}", max_disp)).code());

                        let elements_chart = BarChart::new(bars_elements.clone()).vertical();
                        let render_chart = BarChart::new(bars_render.clone())
                            .stack_on(&[&elements_chart])
                            .vertical();
                        let submitted_chart = BarChart::new(bars_submitted.clone())
                            .stack_on(&[&elements_chart, &render_chart])
                            .vertical();
                        let display_chart = BarChart::new(bars_displayed.clone())
                            .stack_on(&[&elements_chart, &render_chart, &submitted_chart])
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
                                plot_ui.bar_chart(submitted_chart);
                                plot_ui.bar_chart(display_chart);
                            });

                        ui.separator();
                        ui.label(egui::RichText::new("Input States").heading());
                        for seat in seats {
                            ui.label(egui::RichText::new(format!("\t{}", seat.name())).strong());
                            if let Some(ptr) = seat.get_pointer() {
                                egui::Frame::NONE
                                    .fill(egui::Color32::DARK_GRAY)
                                    .corner_radius(5.)
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
                                egui::Frame::NONE
                                    .fill(egui::Color32::DARK_GRAY)
                                    .corner_radius(5.)
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
        Some(WlSurface { surface, toplevel }) => match toplevel {
            Some(PointerFocusToplevel::Surface(window)) => {
                format!(
                    "Window {} ({})",
                    match window.0.underlying_surface() {
                        WindowSurface::Wayland(_) => surface.id().protocol_id(),
                        WindowSurface::X11(x) => x.window_id(),
                    },
                    window.title()
                )
            }
            Some(PointerFocusToplevel::Popup(_)) => {
                format!("Popup {}", surface.id().protocol_id())
            }
            _ => format!("Surface {}", surface.id().protocol_id()),
        },
        Some(X11Surface { surface, toplevel }) => match toplevel {
            Some(window) => {
                format!("Window {} ({})", surface.window_id(), window.title())
            }
            _ => format!("X11Surface {}", surface.window_id()),
        },
        Some(StackUI(stack)) => format!(
            "Stack SSD {} ({})",
            match stack.active().0.underlying_surface() {
                WindowSurface::Wayland(t) => t.wl_surface().id().protocol_id(),
                WindowSurface::X11(x) => x.window_id(),
            },
            stack.active().title()
        ),
        Some(WindowUI(window)) => format!(
            "Window SSD {} ({})",
            match window.surface().0.underlying_surface() {
                WindowSurface::Wayland(t) => t.wl_surface().id().protocol_id(),
                WindowSurface::X11(x) => x.window_id(),
            },
            window.surface().title()
        ),
        Some(ResizeFork(_)) => String::from("Resize UI"),
        Some(ZoomUI(_)) => String::from("Zoom UI"),
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
