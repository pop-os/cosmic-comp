use std::sync::{Arc, Mutex};

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
        core::{Background, Border, Color, Length},
        widget::{container, row, space},
    },
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
        row(vec![
            text::heading(&self.shortcut1).into(),
            text::body(fl!("grow-window")).into(),
            space::horizontal().width(40).into(),
            text::heading(&self.shortcut2).into(),
            text::body(fl!("shrink-window")).into(),
        ])
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
                    radius: 18.0.into(),
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

pub struct ResizeIndicatorArrow {
    direction: Arc<Mutex<ResizeDirection>>,
    icon_outwards: &'static str,
    icon_inwards: &'static str,
}

impl Program for ResizeIndicatorArrow {
    type Message = ();

    fn view(&self) -> cosmic::Element<'_, Self::Message> {
        from_name(
            if *self.direction.lock().unwrap() == ResizeDirection::Outwards {
                self.icon_outwards
            } else {
                self.icon_inwards
            },
        )
        .size(20)
        .prefer_svg(true)
        .apply(container)
        .padding(8)
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
        .into()
    }
}
