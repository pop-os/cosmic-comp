use crate::{
    fl,
    utils::iced::{IcedElement, Program},
};

use calloop::LoopHandle;
use cosmic::{
    Apply,
    iced::{
        Alignment,
        widget::{container, row},
    },
    iced_core::{Background, Border, Color, Length},
    theme,
    widget::{horizontal_space, icon::from_name, text},
};
use smithay::utils::{Logical, Size};

pub type StackHover = IcedElement<StackHoverInternal>;

pub fn stack_hover(
    evlh: LoopHandle<'static, crate::state::State>,
    size: Size<i32, Logical>,
    theme: cosmic::Theme,
) -> StackHover {
    StackHover::new(StackHoverInternal, size, evlh, theme)
}

pub struct StackHoverInternal;

impl Program for StackHoverInternal {
    type Message = ();

    fn view(&self) -> cosmic::Element<'_, Self::Message> {
        row(vec![
            from_name("window-stack-symbolic")
                .size(32)
                .prefer_svg(true)
                .icon()
                .into(),
            horizontal_space().width(16).into(),
            text::title3(fl!("stack-windows")).into(),
        ])
        .align_y(Alignment::Center)
        .apply(container)
        .align_x(Alignment::Center)
        .align_y(Alignment::Center)
        .padding(16)
        .apply(container)
        .class(theme::Container::custom(|theme| container::Style {
            icon_color: Some(Color::from(theme.cosmic().accent.on)),
            text_color: Some(Color::from(theme.cosmic().accent.on)),
            background: Some(Background::Color(theme.cosmic().accent_color().into())),
            border: Border {
                radius: 18.0.into(),
                width: 0.0,
                color: Color::TRANSPARENT,
            },
            shadow: Default::default(),
        }))
        .width(Length::Shrink)
        .height(Length::Shrink)
        .apply(container)
        .center_x(Length::Fill)
        .center_y(Length::Fill)
        .into()
    }
}
