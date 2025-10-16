use std::sync::Mutex;

use crate::{
    config::Config,
    fl,
    shell::grabs::ResizeEdge,
    utils::iced::{IcedElement, Program},
};

use calloop::LoopHandle;
use cosmic::{
    Apply,
    iced::{
        Alignment,
        widget::{column, container, horizontal_space, row, vertical_space},
    },
    iced_core::{Background, Border, Color, Length},
    theme,
    widget::{icon::from_name, text},
};
use cosmic_settings_config::shortcuts::action::{Action, ResizeDirection};
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
                .shortcuts
                .iter()
                .find_map(|(pattern, action)| {
                    (*action == Action::Resizing(ResizeDirection::Outwards)).then_some(pattern)
                })
                .map(|pattern| format!("{}: ", pattern.to_string()))
                .unwrap_or_else(|| crate::fl!("unknown-keybinding")),
            shortcut2: config
                .shortcuts
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
            theme::Container::custom(|theme| container::Style {
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
                .size(20)
                .prefer_svg(true)
                .apply(container)
                .padding(8)
                .class(icon_container_style())
                .width(Length::Shrink)
                .apply(container)
                .center_x(Length::Fill)
                .into()
            } else {
                vertical_space().height(36).into()
            },
            row(vec![
                if edges.contains(ResizeEdge::LEFT) {
                    from_name(if self.direction == ResizeDirection::Outwards {
                        "go-previous-symbolic"
                    } else {
                        "go-next-symbolic"
                    })
                    .size(20)
                    .prefer_svg(true)
                    .apply(container)
                    .padding(8)
                    .class(icon_container_style())
                    .width(Length::Shrink)
                    .apply(container)
                    .center_y(Length::Fill)
                    .into()
                } else {
                    horizontal_space().width(36).into()
                },
                row(vec![
                    text::heading(&self.shortcut1).into(),
                    text::body(fl!("grow-window")).into(),
                    horizontal_space().width(40).into(),
                    text::heading(&self.shortcut2).into(),
                    text::body(fl!("shrink-window")).into(),
                ])
                .apply(container)
                .align_x(Alignment::Center)
                .align_y(Alignment::Center)
                .padding(16)
                .apply(container)
                .class(icon_container_style())
                .width(Length::Shrink)
                .height(Length::Shrink)
                .apply(container)
                .center_x(Length::Fill)
                .center_y(Length::Fill)
                .into(),
                if edges.contains(ResizeEdge::RIGHT) {
                    from_name(if self.direction == ResizeDirection::Outwards {
                        "go-next-symbolic"
                    } else {
                        "go-previous-symbolic"
                    })
                    .size(20)
                    .prefer_svg(true)
                    .apply(container)
                    .padding(8)
                    .class(icon_container_style())
                    .height(Length::Shrink)
                    .apply(container)
                    .center_y(Length::Fill)
                    .into()
                } else {
                    horizontal_space().width(36).into()
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
                .size(20)
                .prefer_svg(true)
                .apply(container)
                .padding(8)
                .class(icon_container_style())
                .width(Length::Shrink)
                .apply(container)
                .center_x(Length::Fill)
                .into()
            } else {
                vertical_space().height(36).into()
            },
        ])
        .into()
    }
}
