use crate::{
    fl,
    utils::iced::{IcedElement, Program},
};

use calloop::LoopHandle;
use cosmic::{
    Apply,
    iced::{
        Alignment,
        core::{Background, Border, Color, Length},
        widget::{container, row},
    },
    theme,
    widget::{icon::from_name, space, text},
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
            space::horizontal().width(16).into(),
            text::title3(fl!("stack-windows")).into(),
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
                background.alpha = theme
                    .cosmic()
                    .alpha_map
                    .blurred_alpha(theme.cosmic().frosted);
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
        .apply(container)
        .center_x(Length::Fill)
        .center_y(Length::Fill)
        .into()
    }
}
