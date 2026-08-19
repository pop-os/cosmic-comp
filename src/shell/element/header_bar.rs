//! Compositor header bar (SSD title bar) — wraps icetron's `app_header`.
//!
//! Provides a thin adapter between icetron's `app_header` component and the
//! compositor's SSD decoration system. The header uses icetron's theme tokens
//! so SSD windows match CSD visual design.

use iced_core::Alignment;
use iced_core::{Element, Length};
use iced_widget::{Svg, container, row, svg};
use icetron_p::prelude::{animated_opacity, app_header, header_height, styled_text};

use crate::comp_theme::CompTheme;

/// SSD header bar height in logical pixels for the given theme.
/// Adds 1px for the bottom border rendered by app_header's `show_border`.
pub fn ssd_header_height(theme: &CompTheme) -> u32 {
    header_height(&**theme) as u32
}

/// Application icon for the SSD header — leaked static SVG bytes or a raster image handle.
///
/// SVG bytes are leaked once per window to obtain `&'static [u8]` for icetron's
/// `title_icon()` API. This is bounded (one allocation per window lifetime).
#[derive(Clone, Debug)]
pub enum AppIcon {
    /// Leaked SVG bytes — can be passed directly to `title_icon()`.
    ///
    /// `symbolic` marks a single-colour glyph that should take the title
    /// colour. A full-colour app mark must not: iced's `svg::Style::color` is
    /// an RGB *replacement* filter, so tinting one flattens the whole mark into
    /// a solid block of that colour, keeping only its alpha silhouette.
    Svg {
        bytes: &'static [u8],
        symbolic: bool,
    },
    /// Pre-scaled RGBA raster image (not usable with title_icon, ignored for now).
    Image(iced_core::image::Handle),
}

/// Builder for the compositor SSD header bar.
pub struct HeaderBar<'a, Message> {
    title: String,
    on_drag: Option<Message>,
    on_close: Option<Message>,
    on_minimize: Option<Message>,
    on_maximize: Option<Message>,
    on_right_click: Option<Message>,
    focused: bool,
    hovered: bool,
    maximized: bool,
    theme: Option<&'a CompTheme>,
    app_icon: Option<AppIcon>,
}

impl<'a, Message: Clone + 'static> Default for HeaderBar<'a, Message> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, Message: Clone + 'static> HeaderBar<'a, Message> {
    pub fn new() -> Self {
        Self {
            title: String::new(),
            on_drag: None,
            on_close: None,
            on_minimize: None,
            on_maximize: None,
            on_right_click: None,
            focused: false,
            hovered: false,
            maximized: false,
            theme: None,
            app_icon: None,
        }
    }

    pub fn title(mut self, title: impl Into<String>) -> Self {
        self.title = title.into();
        self
    }

    pub fn on_drag(mut self, msg: Message) -> Self {
        self.on_drag = Some(msg);
        self
    }

    pub fn on_close(mut self, msg: Message) -> Self {
        self.on_close = Some(msg);
        self
    }

    pub fn on_minimize(mut self, msg: Message) -> Self {
        self.on_minimize = Some(msg);
        self
    }

    pub fn on_maximize(mut self, msg: Message) -> Self {
        self.on_maximize = Some(msg);
        self
    }

    pub fn on_right_click(mut self, msg: Message) -> Self {
        self.on_right_click = Some(msg);
        self
    }

    pub fn focused(mut self, focused: bool) -> Self {
        self.focused = focused;
        self
    }

    pub fn hovered(mut self, hovered: bool) -> Self {
        self.hovered = hovered;
        self
    }

    pub fn maximized(mut self, maximized: bool) -> Self {
        self.maximized = maximized;
        self
    }

    pub fn app_icon(mut self, icon: AppIcon) -> Self {
        self.app_icon = Some(icon);
        self
    }

    pub fn theme(mut self, theme: &'a CompTheme) -> Self {
        self.theme = Some(theme);
        self
    }

    /// Convert to an iced Element using icetron's app_header.
    pub fn into_element(self) -> Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> {
        let theme = self.theme.expect("HeaderBar requires .theme()");

        let mut header = app_header(&**theme)
            .title(Some(&self.title))
            .focused(self.focused)
            .hovered(self.hovered)
            .is_windowed(!self.maximized)
            .backdrop_blur(false)
            .opaque(true)
            .show_border(true);

        // Pass application icon with native SVG colors via title_content.
        // We wrap in animated_opacity to replicate app_header's title fade behavior
        // (0.8 when unfocused, animates to 1.0 on hover/focus).
        {
            let title_style = theme.header_title_text_style();
            let title_color = theme.header_title_color();
            let title_gap = theme.header_title_gap();

            let text_element: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> =
                styled_text(&self.title, title_style, title_color).into();

            let title_row: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> =
                if let Some(ref icon) = self.app_icon {
                    let icon_size = theme.ui_size_icon_sm();
                    let icon_element: Element<
                        'a,
                        Message,
                        iced_core::Theme,
                        iced_tiny_skia::Renderer,
                    > = match icon {
                        AppIcon::Svg { bytes, symbolic } => {
                            let handle = iced_core::svg::Handle::from_memory(*bytes);
                            // A fully transparent tint is this iced fork's
                            // opt-out from the colour filter. `None` does *not*
                            // mean "leave the artwork alone" here — the fork
                            // reads it as "inherit the parent's text colour",
                            // which is what painted full-colour app marks flat
                            // title-grey. Only a symbolic glyph is recoloured.
                            let icon_tint = if *symbolic {
                                title_color
                            } else {
                                iced_core::Color::TRANSPARENT
                            };
                            Svg::new(handle)
                                .width(icon_size)
                                .height(icon_size)
                                .style(move |_theme, _status| svg::Style {
                                    color: Some(icon_tint),
                                })
                                .into()
                        }
                        AppIcon::Image(handle) => iced_widget::image::Image::new(handle.clone())
                            .width(icon_size)
                            .height(icon_size)
                            .into(),
                    };
                    row![icon_element, text_element]
                        .spacing(title_gap)
                        .align_y(Alignment::Center)
                        .into()
                } else {
                    // No icon yet (async resolution pending) — show title only
                    text_element
                };

            // Replicate app_header's opacity animation logic
            let target_opacity = if self.hovered || self.focused {
                1.0
            } else {
                0.8
            };
            let title_content: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> =
                if target_opacity == 1.0 {
                    title_row
                } else {
                    animated_opacity(title_row, &**theme, target_opacity).into()
                };

            header = header.title_content(title_content);
        }

        if let Some(msg) = self.on_drag {
            header = header.on_drag(msg);
        }
        if let Some(msg) = self.on_close {
            header = header.on_close(msg);
        }
        if let Some(msg) = self.on_minimize {
            header = header.on_minimize(msg);
        }
        // app_header uses on_toggle_window for maximize/unmaximize
        if let Some(msg) = self.on_maximize {
            header = header.on_toggle_window(msg);
        }
        if let Some(msg) = self.on_right_click {
            header = header.on_right_click(msg);
        }

        let header_height = header_height(&**theme);
        // Force header background to fully opaque — the blur backdrop renders
        // behind the header and should not bleed through.
        let header_bg = theme.header_background();
        let top_radius = if self.maximized {
            0.0
        } else {
            theme.radius_window()[0]
        };
        let header_elem: Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> =
            header.into();
        container(header_elem)
            .width(Length::Fill)
            .height(Length::Fixed(header_height))
            .style(move |_theme| container::Style {
                background: Some(iced_core::Background::Color(header_bg)),
                border: iced_core::Border {
                    radius: iced_core::border::Radius {
                        top_left: top_radius,
                        top_right: top_radius,
                        bottom_right: 0.0,
                        bottom_left: 0.0,
                    },
                    ..Default::default()
                },
                ..Default::default()
            })
            .into()
    }
}

impl<'a, Message: Clone + 'static> From<HeaderBar<'a, Message>>
    for Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer>
{
    fn from(header: HeaderBar<'a, Message>) -> Self {
        header.into_element()
    }
}

/// Create a new header bar builder.
pub fn header_bar<'a, Message: Clone + 'static>() -> HeaderBar<'a, Message> {
    HeaderBar::new()
}
