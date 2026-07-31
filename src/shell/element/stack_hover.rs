use crate::{
    backend::render::element::AsGlowRenderer,
    comp_theme::CompTheme,
    fl,
    utils::{
        apply::Apply,
        iced::{CompElement, IcedElement, IcedRenderElement, Program},
        xdg_icon::named_icon,
    },
};

use calloop::LoopHandle;
use iced_core::{Alignment, Length};
use iced_widget::{Space, container, row};
use icetron_p::prelude::styled_text;
use smithay::{
    backend::renderer::ImportMem,
    desktop::space::SpaceElement,
    output::Output,
    utils::{Logical, Physical, Point, Rectangle, Scale, Size},
};

/// Corner radius of the stack-hover pill, matching `CompTheme::accent_container_style`.
const PILL_RADIUS: f32 = 18.0;

#[derive(Debug)]
pub struct StackHover {
    location: Point<i32, Logical>,
    elem: IcedElement<StackHoverInternal>,
}

impl StackHover {
    pub fn new(
        evlh: LoopHandle<'static, crate::state::State>,
        size: Size<i32, Logical>,
        theme: CompTheme,
    ) -> StackHover {
        // MERGE: upstream additionally sets `theme.transparent = frosted_system_interface` here to
        // opt the indicator into the frosted/blurred variant. `CompTheme` has no frosted flag, so
        // the indicator stays fully opaque on this fork.
        let elem = IcedElement::new(StackHoverInternal, size, evlh, theme);
        let minimum = elem.minimum_size();
        let new_size = Size::<i32, Logical>::new(size.w.min(minimum.w), size.h.min(minimum.h));
        let location = Point::new(
            size.w.saturating_sub(new_size.w) / 2,
            size.h.saturating_sub(new_size.h) / 2,
        );
        elem.resize(new_size);

        StackHover { location, elem }
    }

    pub fn push_render_elements<R>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
        alpha: f32,
        push_above: &mut dyn FnMut(IcedRenderElement<R>),
        push_below: Option<&mut dyn FnMut(IcedRenderElement<R>)>,
    ) where
        R: AsGlowRenderer + ImportMem,
        R::TextureId: Send + Clone + 'static,
    {
        self.elem.push_render_elements(
            renderer,
            location + self.location.to_physical_precise_round(scale),
            scale,
            alpha,
            [PILL_RADIUS.round() as u8; 4],
            push_above,
            push_below,
        );
    }

    pub fn output_enter(&self, output: &Output) {
        self.elem
            .output_enter(output, Rectangle::default() /*unused*/);
    }

    pub fn output_leave(&self, output: &Output) {
        self.elem.output_leave(output);
    }
}

pub struct StackHoverInternal;

impl Program for StackHoverInternal {
    type Message = ();

    fn program_name() -> &'static str {
        "StackHover"
    }

    fn view<'a>(&'a self, theme: &'a CompTheme) -> CompElement<'a, Self::Message> {
        let on_accent = theme.on_accent_color();
        let accent = theme.accent_color();

        row![
            named_icon("window-stack-symbolic", 32.0),
            Space::new().width(16.0).height(Length::Shrink),
            styled_text(
                fl!("stack-windows"),
                theme.text_styles().title2(),
                on_accent,
            ),
        ]
        .align_y(Alignment::Center)
        .apply(container)
        .align_x(Alignment::Center)
        .align_y(Alignment::Center)
        .padding(16)
        .apply(container)
        .class(Box::new(move |_: &iced_core::Theme| container::Style {
            text_color: Some(on_accent),
            background: Some(iced_core::Background::Color(accent)),
            border: iced_core::Border {
                radius: PILL_RADIUS.into(),
                width: 0.0,
                color: iced_core::Color::TRANSPARENT,
                ..Default::default()
            },
            shadow: Default::default(),
            snap: false,
            border_only: false,
        })
            as Box<dyn Fn(&iced_core::Theme) -> container::Style>)
        .width(Length::Shrink)
        .height(Length::Shrink)
        .into()
    }
}
