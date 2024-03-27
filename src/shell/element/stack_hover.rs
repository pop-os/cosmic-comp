use crate::{
    fl,
    utils::iced::{IcedElement, Program},
};

use calloop::LoopHandle;
use cosmic::{
    iced::{
        widget::{container, row},
        Alignment,
    },
    iced_core::{Background, Border, Color, Length},
    theme,
    widget::{horizontal_space, icon::from_name, text},
    Apply,
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
            horizontal_space(16).into(),
            text(fl!("stack-windows"))
                .font(cosmic::font::FONT)
                .size(24)
                .into(),
        ])
        .align_items(Alignment::Center)
        .apply(container)
        .center_x()
        .center_y()
        .padding(16)
        .apply(container)
        .style(theme::Container::custom(|theme| container::Appearance {
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
        .width(Length::Fill)
        .height(Length::Fill)
        .center_x()
        .center_y()
        .into()
    }
}
