use crate::{
    backend::render::element::AsGlowRenderer,
    fl,
    utils::iced::{IcedElement, IcedRenderElement, Program},
};

use calloop::LoopHandle;
use cosmic::{
    Apply,
    iced::{
        core::{Alignment, Background, Border, Color, Length},
        widget::{container, row, space},
    },
    theme,
    widget::{icon::from_name, text},
};
use smithay::{
    backend::renderer::ImportMem,
    desktop::space::SpaceElement,
    output::Output,
    utils::{Logical, Physical, Point, Rectangle, Scale, Size},
};

#[derive(Debug, Clone)]
pub struct SwapIndicator {
    location: Point<i32, Logical>,
    elem: IcedElement<SwapIndicatorInternal>,
}

impl SwapIndicator {
    pub fn new(
        evlh: LoopHandle<'static, crate::state::State>,
        mut theme: cosmic::Theme,
    ) -> SwapIndicator {
        theme.transparent = theme.cosmic().frosted_system_interface;
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
            self.elem
                .with_theme(|theme| theme.cosmic().radius_s())
                .map(|x| x.round() as u8),
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

    fn view(&self) -> cosmic::Element<'_, Self::Message> {
        row(vec![
            from_name("window-swap-symbolic")
                .size(32)
                .prefer_svg(true)
                .icon()
                .into(),
            space::horizontal().width(16).into(),
            text::title3(fl!("swap-windows")).into(),
        ])
        .align_y(Alignment::Center)
        .apply(container)
        .align_x(Alignment::Center)
        .align_y(Alignment::Center)
        .padding(16)
        .apply(container)
        .class(theme::Container::custom(|theme| {
            let mut background = theme.cosmic().accent_color();
            if theme.transparent {
                background.alpha = theme.cosmic().frosted.alpha();
            }

            container::Style {
                snap: true,
                icon_color: Some(Color::from(theme.cosmic().accent.on)),
                text_color: Some(Color::from(theme.cosmic().accent.on)),
                background: Some(Background::Color(background.into())),
                border: Border {
                    radius: theme.cosmic().radius_s().into(),
                    width: 0.0,
                    color: Color::TRANSPARENT,
                },
                shadow: Default::default(),
            }
        }))
        .width(Length::Shrink)
        .height(Length::Shrink)
        .into()
    }
}
