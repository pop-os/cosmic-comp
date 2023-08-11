use crate::{
    fl,
    utils::iced::{IcedElement, Program},
};

use apply::Apply;
use calloop::LoopHandle;
use cosmic::{
    iced::widget::{container, horizontal_space, row},
    iced_core::{Alignment, Background, Color, Length},
    theme,
    widget::{icon, text},
};
use smithay::utils::Size;

pub type SwapIndicator = IcedElement<SwapIndicatorInternal>;

pub fn swap_indicator(evlh: LoopHandle<'static, crate::state::Data>) -> SwapIndicator {
    SwapIndicator::new(SwapIndicatorInternal, Size::from((1, 1)), evlh)
}

pub struct SwapIndicatorInternal;

impl Program for SwapIndicatorInternal {
    type Message = ();

    fn view(&self) -> crate::utils::iced::Element<'_, Self::Message> {
        row(vec![
            icon("window-swap-symbolic", 32).force_svg(true).into(),
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
            text_color: Some(Color::from(theme.cosmic().accent.on)),
            background: Some(Background::Color(theme.cosmic().accent_color().into())),
            border_radius: 18.0.into(),
            border_width: 0.0,
            border_color: Color::TRANSPARENT,
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
