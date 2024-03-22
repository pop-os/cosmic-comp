use crate::{
    fl,
    utils::iced::{IcedElement, Program},
};

use calloop::LoopHandle;
use cosmic::{
    iced::widget::{container, row},
    iced_core::{Background, Border, Color, Length},
    theme,
    widget::{icon::from_name, text},
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
                .size(24)
                .prefer_svg(true)
                .icon()
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
            icon_color: Some(Color::from(theme.cosmic().accent.on)),
            text_color: Some(Color::from(theme.cosmic().accent.on)),
            background: Some(Background::Color(theme.cosmic().accent_color().into())),
            border: Border {
                radius: 24.0.into(),
                width: 0.0,
                color: Color::TRANSPARENT,
            },
            shadow: Default::default(),
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
