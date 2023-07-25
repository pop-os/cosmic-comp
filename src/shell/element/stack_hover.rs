use crate::{
    fl,
    utils::iced::{IcedElement, Program},
};

use apply::Apply;
use calloop::LoopHandle;
use cosmic::{
    iced::widget::{container, row},
    iced_core::{Background, Color, Length},
    theme,
    widget::{icon, text},
};
use smithay::utils::{Logical, Size};

pub type StackHover = IcedElement<StackHoverInternal>;

pub fn stack_hover(
    evlh: LoopHandle<'static, crate::state::Data>,
    size: Size<i32, Logical>,
) -> StackHover {
    StackHover::new(StackHoverInternal, size, evlh)
}

pub struct StackHoverInternal;

impl Program for StackHoverInternal {
    type Message = ();

    fn view(&self) -> crate::utils::iced::Element<'_, Self::Message> {
        row(vec![
            icon("window-stack-symbolic", 24)
                .force_svg(true)
                .style(theme::Svg::Symbolic)
                .apply(container)
                .padding([0, 8, 0, 0])
                .width(Length::Shrink)
                .apply(container)
                .center_y()
                .height(Length::Fill)
                .into(),
            text(fl!("stack-windows"))
                .font(cosmic::font::FONT)
                .size(24)
                .line_height(1.3)
                .apply(container)
                .width(Length::Shrink)
                .apply(container)
                .center_y()
                .height(Length::Fill)
                .into(),
        ])
        .width(Length::Shrink)
        .height(Length::Shrink)
        .apply(container)
        .padding([8, 16])
        .style(theme::Container::custom(|theme| container::Appearance {
            text_color: Some(Color::from(theme.cosmic().palette.neutral_9)),
            background: Some(Background::Color(theme.cosmic().palette.neutral_3.into())),
            border_radius: 24.0.into(),
            border_width: 0.0,
            border_color: Color::TRANSPARENT,
        }))
        .width(Length::Shrink)
        .height(Length::Fixed(48.))
        .apply(container)
        .center_x()
        .center_y()
        .width(Length::Fill)
        .height(Length::Fill)
        .into()
    }
}
