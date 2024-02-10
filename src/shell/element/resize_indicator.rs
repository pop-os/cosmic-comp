use std::sync::Mutex;

use crate::{
    config::{Action, Config},
    fl,
    shell::{grabs::ResizeEdge, ResizeDirection},
    utils::iced::{IcedElement, Program},
};

use calloop::LoopHandle;
use cosmic::{
    iced::widget::{column, container, horizontal_space, row, vertical_space},
    iced_core::{Background, Border, Color, Length},
    theme,
    widget::{icon::from_name, text},
    Apply,
};
use smithay::utils::Size;

pub type ResizeIndicator = IcedElement<ResizeIndicatorInternal>;

pub fn resize_indicator(
    direction: ResizeDirection,
    config: &Config,
    evlh: LoopHandle<'static, crate::state::State>,
    theme: cosmic::Theme,
) -> ResizeIndicator {
    ResizeIndicator::new(
        ResizeIndicatorInternal {
            edges: Mutex::new(ResizeEdge::all()),
            direction,
            shortcut1: config
                .static_conf
                .key_bindings
                .iter()
                .find_map(|(pattern, action)| {
                    (*action == Action::Resizing(ResizeDirection::Outwards)).then_some(pattern)
                })
                .map(|pattern| format!("{}: ", pattern.to_string()))
                .unwrap_or_else(|| crate::fl!("unknown-keybinding")),
            shortcut2: config
                .static_conf
                .key_bindings
                .iter()
                .find_map(|(pattern, action)| {
                    (*action == Action::Resizing(ResizeDirection::Inwards)).then_some(pattern)
                })
                .map(|pattern| format!("{}: ", pattern.to_string()))
                .unwrap_or_else(|| crate::fl!("unknown-keybinding")),
        },
        Size::from((1, 1)),
        evlh,
        theme,
    )
}

pub struct ResizeIndicatorInternal {
    pub edges: Mutex<ResizeEdge>,
    pub direction: ResizeDirection,
    pub shortcut1: String,
    pub shortcut2: String,
}

impl Program for ResizeIndicatorInternal {
    type Message = ();

    fn view(&self) -> cosmic::Element<'_, Self::Message> {
        let edges = self.edges.lock().unwrap();
        let icon_container_style = || {
            theme::Container::custom(|theme| container::Appearance {
                icon_color: Some(Color::from(theme.cosmic().accent.on)),
                text_color: Some(Color::from(theme.cosmic().accent.on)),
                background: Some(Background::Color(theme.cosmic().accent_color().into())),
                border: Border {
                    radius: 18.0.into(),
                    width: 0.0,
                    color: Color::TRANSPARENT,
                },
                shadow: Default::default(),
            })
        };

        column(vec![
            if edges.contains(ResizeEdge::TOP) {
                from_name(if self.direction == ResizeDirection::Outwards {
                    "go-up-symbolic"
                } else {
                    "go-down-symbolic"
                })
                .size(32)
                .prefer_svg(true)
                .apply(container)
                .padding(2)
                .style(icon_container_style())
                .width(Length::Shrink)
                .apply(container)
                .center_x()
                .width(Length::Fill)
                .into()
            } else {
                vertical_space(36).into()
            },
            row(vec![
                if edges.contains(ResizeEdge::LEFT) {
                    from_name(if self.direction == ResizeDirection::Outwards {
                        "go-previous-symbolic"
                    } else {
                        "go-next-symbolic"
                    })
                    .size(32)
                    .prefer_svg(true)
                    .apply(container)
                    .padding(4)
                    .style(icon_container_style())
                    .width(Length::Shrink)
                    .apply(container)
                    .center_y()
                    .height(Length::Fill)
                    .into()
                } else {
                    horizontal_space(36).into()
                },
                row(vec![
                    text(&self.shortcut1)
                        .font(cosmic::font::FONT_SEMIBOLD)
                        .size(14)
                        .into(),
                    text(fl!("grow-window"))
                        .font(cosmic::font::FONT)
                        .size(14)
                        .into(),
                    horizontal_space(40).into(),
                    text(&self.shortcut2)
                        .font(cosmic::font::FONT_SEMIBOLD)
                        .size(14)
                        .into(),
                    text(fl!("shrink-window"))
                        .font(cosmic::font::FONT)
                        .size(14)
                        .into(),
                ])
                .apply(container)
                .center_x()
                .center_y()
                .padding(16)
                .apply(container)
                .style(icon_container_style())
                .width(Length::Shrink)
                .height(Length::Shrink)
                .apply(container)
                .height(Length::Fill)
                .width(Length::Fill)
                .center_x()
                .center_y()
                .into(),
                if edges.contains(ResizeEdge::RIGHT) {
                    from_name(if self.direction == ResizeDirection::Outwards {
                        "go-next-symbolic"
                    } else {
                        "go-previous-symbolic"
                    })
                    .size(32)
                    .prefer_svg(true)
                    .apply(container)
                    .padding(4)
                    .style(icon_container_style())
                    .height(Length::Shrink)
                    .apply(container)
                    .center_y()
                    .height(Length::Fill)
                    .into()
                } else {
                    horizontal_space(36).into()
                },
            ])
            .width(Length::Fill)
            .height(Length::Fill)
            .into(),
            if edges.contains(ResizeEdge::BOTTOM) {
                from_name(if self.direction == ResizeDirection::Outwards {
                    "go-down-symbolic"
                } else {
                    "go-up-symbolic"
                })
                .size(32)
                .prefer_svg(true)
                .apply(container)
                .padding(4)
                .style(icon_container_style())
                .width(Length::Shrink)
                .apply(container)
                .center_x()
                .width(Length::Fill)
                .into()
            } else {
                vertical_space(36).into()
            },
        ])
        .into()
    }
}
