use crate::{
    fl,
    utils::iced::{IcedElement, Program},
};

use calloop::LoopHandle;
use cosmic::{
    iced::widget::{container, horizontal_space, row},
    iced_core::{Alignment, Background, Border, Color, Length},
    theme,
    widget::{icon::from_name, text},
    Apply,
};
use smithay::utils::Size;

pub type SwapIndicator = IcedElement<SwapIndicatorInternal>;

pub fn swap_indicator(
    evlh: LoopHandle<'static, crate::state::State>,
    theme: cosmic::Theme,
) -> SwapIndicator {
    SwapIndicator::new(SwapIndicatorInternal, Size::from((1, 1)), evlh, theme)
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
            horizontal_space(16).into(),
            text(fl!("swap-windows"))
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
        .height(Length::Fill)
        .width(Length::Fill)
        .center_x()
        .center_y()
        .into()
    }
}
