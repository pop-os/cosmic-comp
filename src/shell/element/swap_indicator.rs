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

/// Corner radius of the indicator container. Also handed to the renderer so the
/// backdrop captured behind the element is rounded to the same shape.
const INDICATOR_RADIUS: f32 = 18.0;

#[derive(Debug, Clone)]
pub struct SwapIndicator {
    location: Point<i32, Logical>,
    elem: IcedElement<SwapIndicatorInternal>,
}

impl SwapIndicator {
    pub fn new(evlh: LoopHandle<'static, crate::state::State>, theme: CompTheme) -> SwapIndicator {
        // MERGE: upstream also sets `theme.transparent = frosted_system_interface` here to opt the
        // indicator into its frosted-glass backdrop. `CompTheme` has no frosted/alpha-map concept,
        // so the indicator stays opaque until that lands in `comp_theme`.
        SwapIndicator {
            location: Point::default(),
            elem: IcedElement::new(SwapIndicatorInternal, Size::from((1, 1)), evlh, theme),
        }
    }

    pub fn resize(&mut self, size: Size<i32, Logical>) {
        let minimum = self.elem.minimum_size();
        let new_size = Size::<i32, Logical>::new(size.w.min(minimum.w), size.h.min(minimum.h));
        let location = Point::new(
            size.w.saturating_sub(new_size.w) / 2,
            size.h.saturating_sub(new_size.h) / 2,
        );
        self.elem.resize(new_size);
        self.location = location;
    }

    pub fn set_theme(&self, theme: CompTheme) {
        self.elem.set_theme(theme);
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
            [INDICATOR_RADIUS.round() as u8; 4],
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

pub struct SwapIndicatorInternal;

impl Program for SwapIndicatorInternal {
    type Message = ();

    fn program_name() -> &'static str {
        "SwapIndicator"
    }

    fn view<'a>(&'a self, theme: &'a CompTheme) -> CompElement<'a, Self::Message> {
        let on_accent = theme.on_accent_color();
        let accent = theme.accent_color();

        row![
            named_icon("window-swap-symbolic", 32.0),
            Space::new().width(16.0).height(Length::Shrink),
            styled_text(fl!("swap-windows"), theme.text_styles().title2(), on_accent,),
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
                radius: INDICATOR_RADIUS.into(),
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
