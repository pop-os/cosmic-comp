// SPDX-License-Identifier: GPL-3.0-only

//! On-screen "capturing performance" badge.
//!
//! Shown in the top-left of each output while an F12 capture window is running
//! (`crate::perf::is_capturing()`). A themed rounded card with a soft shadow, a
//! red record dot, and a label — rendered through the compositor's iced path
//! ([`IcedElement`]). Created on the main thread when a capture is armed (stored
//! in `Shell`), rendered on the surface thread, dropped when the capture ends, so
//! it costs nothing otherwise.

use calloop::LoopHandle;
use iced_core::{Alignment, Background, Border, Color, Length, Shadow, Vector};
use iced_widget::{Space, container, row};
use icetron_p::prelude::styled_text;
use smithay::{
    backend::renderer::ImportMem,
    output::Output,
    utils::{Logical, Point, Size},
};

use crate::backend::render::AsGlowRenderer;
use crate::utils::iced::IcedRenderElement;
use crate::{
    comp_theme::CompTheme,
    state::State,
    utils::{
        apply::Apply,
        iced::{CompElement, IcedElement, Program},
    },
};

pub type PerfBadge = IcedElement<PerfBadgeProgram>;

pub struct PerfBadgeProgram;

impl Program for PerfBadgeProgram {
    type Message = ();

    fn program_name() -> &'static str {
        "PerfCaptureBadge"
    }

    fn view<'a>(&'a self, theme: &'a CompTheme) -> CompElement<'a, Self::Message> {
        let text_color = theme.on_surface_color();
        let surface = theme.surface_color();
        let divider = theme.divider_color();
        let record = Color::from_rgb(0.93, 0.22, 0.22);
        // Cold-start (F11) shows its own label; otherwise phase 1 asks the user to
        // move the mouse (input latency) and phase 2 is the forced max-fps test.
        let label = if crate::perf::is_coldstart() {
            "Cold-start test…"
        } else if crate::perf::is_stressing() {
            "Max-fps test…"
        } else {
            "Capturing — move your mouse"
        };

        // Red record dot.
        let dot = Space::new()
            .width(10.0)
            .height(10.0)
            .apply(container)
            .class(Box::new(move |_: &iced_core::Theme| container::Style {
                background: Some(Background::Color(record)),
                border: Border {
                    radius: 5.0.into(),
                    ..Default::default()
                },
                ..Default::default()
            })
                as Box<dyn Fn(&iced_core::Theme) -> container::Style>);

        row![
            dot,
            Space::new().width(10.0).height(Length::Shrink),
            styled_text(label, theme.text_styles().body(), text_color),
        ]
        .align_y(Alignment::Center)
        .apply(container)
        .padding(10)
        .class(Box::new(move |_: &iced_core::Theme| container::Style {
            text_color: Some(text_color),
            background: Some(Background::Color(surface)),
            border: Border {
                radius: 18.0.into(),
                width: 1.0,
                color: divider,
                ..Default::default()
            },
            shadow: Shadow {
                offset: Vector::new(0.0, 3.0),
                blur_radius: 9.0,
                color: Color {
                    r: 0.0,
                    g: 0.0,
                    b: 0.0,
                    a: 0.18,
                },
                ..Default::default()
            },
            snap: false,
            border_only: false,
        })
            as Box<dyn Fn(&iced_core::Theme) -> container::Style>)
        // Transparent outer padding so the card's drop shadow isn't clipped by
        // the element's buffer (which is otherwise sized to the card content).
        .apply(container)
        .padding(12)
        .into()
    }
}

/// Create a ready-to-render badge (sized to its content). Call on the main thread
/// — needs the event loop handle and theme — then `output_enter` each output.
pub fn badge(evlh: LoopHandle<'static, State>, theme: CompTheme) -> PerfBadge {
    let element = IcedElement::new(PerfBadgeProgram, Size::default(), evlh, theme);
    let mut size = element.minimum_size();
    // iced tends to under-measure; pad so the label isn't clipped.
    size.w += 16;
    element.resize(size);
    element
}

/// Render the badge for `output`, positioned top-left (clearing a top panel).
/// `C` is the compositor element type the buffer element converts into.
pub fn render<R>(
    badge: &PerfBadge,
    renderer: &mut R,
    output: &Output,
    push: &mut dyn FnMut(IcedRenderElement<R>),
) where
    R: AsGlowRenderer + ImportMem,
    R::TextureId: Send + Clone + 'static,
{
    let scale = output.current_scale().fractional_scale();
    // Offset by the 12px transparent padding (shadow room) so the visible card
    // still lands ~(16, 52) from the top-left.
    let location = Point::<f64, Logical>::from((4.0, 40.0))
        .to_physical(scale)
        .to_i32_round();
    // The badge is an overlay: everything it draws goes above, and it has no
    // drop shadow to order underneath.
    badge.push_render_elements(renderer, location, scale.into(), 1.0, [0; 4], push, None)
}
