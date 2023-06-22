use apply::Apply;
use cosmic::{
    font::Font,
    iced::{
        widget::{
            self, container::draw_background, rule::FillMode, text::StyleSheet as TextStyleSheet,
        },
        Element,
    },
    iced_core::{
        alignment, event,
        layout::{Layout, Limits, Node},
        mouse, overlay, renderer,
        widget::{
            operation::{Operation, OperationOutputWrapper},
            tree::Tree,
            Widget,
        },
        Clipboard, Color, Length, Point, Rectangle, Shell, Size,
    },
    iced_style::{
        button::StyleSheet as ButtonStyleSheet, container::StyleSheet as ContainerStyleSheet,
        rule::StyleSheet as RuleStyleSheet,
    },
    iced_widget::scrollable::AbsoluteOffset,
    theme,
    widget::{icon, text, Icon},
};

use super::tab_text::tab_text;

pub(super) enum TabRuleTheme {
    ActiveActivated,
    ActiveDeactivated,
    Default,
}

impl Into<theme::Rule> for TabRuleTheme {
    fn into(self) -> theme::Rule {
        match self {
            Self::ActiveActivated => theme::Rule::custom(|theme| widget::rule::Appearance {
                color: theme.cosmic().accent_color().into(),
                width: 4,
                radius: 0.,
                fill_mode: FillMode::Full,
            }),
            Self::ActiveDeactivated => theme::Rule::custom(|theme| widget::rule::Appearance {
                color: theme.cosmic().palette.neutral_5.into(),
                width: 4,
                radius: 0.,
                fill_mode: FillMode::Full,
            }),
            Self::Default => theme::Rule::custom(|theme| widget::rule::Appearance {
                color: theme.cosmic().palette.neutral_5.into(),
                width: 4,
                radius: 8.,
                fill_mode: FillMode::Padded(4),
            }),
        }
    }
}

pub(super) enum TabBackgroundTheme {
    ActiveActivated,
    ActiveDeactivated,
    Default,
}

impl Into<theme::Container> for TabBackgroundTheme {
    fn into(self) -> theme::Container {
        match self {
            Self::ActiveActivated => {
                theme::Container::custom(|theme| widget::container::Appearance {
                    text_color: Some(Color::from(theme.cosmic().accent_text_color())),
                    background: Some(cosmic::iced::Background::Color(Color::from_rgba(
                        1.0, 1.0, 1.0, 0.1,
                    ))),
                    border_radius: 0.0.into(),
                    border_width: 0.0,
                    border_color: Color::TRANSPARENT,
                })
            }
            Self::ActiveDeactivated => {
                theme::Container::custom(|_theme| widget::container::Appearance {
                    text_color: None,
                    background: Some(cosmic::iced::Background::Color(Color::from_rgba(
                        1.0, 1.0, 1.0, 0.1,
                    ))),
                    border_radius: 0.0.into(),
                    border_width: 0.0,
                    border_color: Color::TRANSPARENT,
                })
            }
            Self::Default => theme::Container::Transparent,
        }
    }
}

pub trait TabMessage {
    fn activate(idx: usize) -> Self;
    fn is_activate(&self) -> Option<usize>;

    fn scroll_further() -> Self;
    fn scroll_back() -> Self;
    fn populate_scroll(&mut self, current_offset: AbsoluteOffset) -> Option<AbsoluteOffset>;
    fn scrolled() -> Self;
}

pub struct Tab<'a, Message: TabMessage> {
    app_icon: Icon<'a>,
    title: String,
    font: Font,
    close_message: Option<Message>,
    rule_theme: TabRuleTheme,
    background_theme: TabBackgroundTheme,
    active: bool,
}

impl<'a, Message: TabMessage> Tab<'a, Message> {
    pub fn new(title: impl Into<String>, app_id: impl Into<String>) -> Self {
        Tab {
            app_icon: icon(app_id.into(), 16),
            title: title.into(),
            font: cosmic::font::FONT,
            close_message: None,
            rule_theme: TabRuleTheme::Default,
            background_theme: TabBackgroundTheme::Default,
            active: false,
        }
    }

    pub fn on_close(mut self, message: Message) -> Self {
        self.close_message = Some(message);
        self
    }

    pub(super) fn font(mut self, font: Font) -> Self {
        self.font = font;
        self
    }

    pub(super) fn rule_style(mut self, theme: TabRuleTheme) -> Self {
        self.rule_theme = theme;
        self
    }

    pub(super) fn background_style(mut self, theme: TabBackgroundTheme) -> Self {
        self.background_theme = theme;
        self
    }

    pub(super) fn non_active(mut self) -> Self {
        self.active = false;
        self
    }

    pub(super) fn active(mut self) -> Self {
        self.active = true;
        self
    }

    pub(super) fn internal<Renderer>(self, idx: usize) -> TabInternal<'a, Message, Renderer>
    where
        Renderer: cosmic::iced_core::Renderer + 'a,
        Renderer: cosmic::iced_core::text::Renderer<Font = Font>,
        Renderer::Theme: ButtonStyleSheet<Style = theme::Button>,
        Renderer::Theme: ContainerStyleSheet,
        Renderer::Theme: RuleStyleSheet<Style = theme::Rule>,
        Renderer::Theme: TextStyleSheet,
        Message: 'a,
        widget::Button<'a, Message, Renderer>: Into<Element<'a, Message, Renderer>>,
        widget::Container<'a, Message, Renderer>: Into<Element<'a, Message, Renderer>>,
        Icon<'a>: Into<Element<'a, Message, Renderer>>,
    {
        let mut close_button = icon("window-close-symbolic", 16)
            .force_svg(true)
            .style(theme::Svg::Symbolic)
            .apply(widget::button)
            .padding(0)
            .style(theme::Button::Text);
        if let Some(close_message) = self.close_message {
            close_button = close_button.on_press(close_message);
        }

        let mut items = vec![
            widget::vertical_rule(4).style(self.rule_theme).into(),
            self.app_icon
                .apply(widget::container)
                .height(Length::Fill)
                .width(Length::Shrink)
                .padding([2, 4])
                .center_y()
                .into(),
        ];
        items.push(
            text(self.title)
                .size(14)
                .font(self.font)
                .horizontal_alignment(alignment::Horizontal::Left)
                .vertical_alignment(alignment::Vertical::Center)
                .apply(tab_text)
                .height(Length::Fill)
                .width(Length::Fill)
                .into(),
        );
        items.push(
            close_button
                .apply(widget::container)
                .height(Length::Fill)
                .width(Length::Shrink)
                .padding([2, 4])
                .center_y()
                .align_x(alignment::Horizontal::Right)
                .into(),
        );

        TabInternal {
            idx,
            active: self.active,
            background: self.background_theme.into(),
            elements: items,
        }
    }
}

const TAB_HEIGHT: i32 = 24;
pub const MIN_ACTIVE_TAB_WIDTH: i32 = 140;
const MIN_TAB_WIDTH: i32 = 38;

const TEXT_BREAKPOINT: i32 = 44;
const CLOSE_BREAKPOINT: i32 = 125;

pub(super) struct TabInternal<'a, Message: TabMessage, Renderer> {
    idx: usize,
    active: bool,
    background: theme::Container,
    elements: Vec<Element<'a, Message, Renderer>>,
}

impl<'a, Message, Renderer> Widget<Message, Renderer> for TabInternal<'a, Message, Renderer>
where
    Renderer: cosmic::iced_core::Renderer,
    Renderer::Theme: ContainerStyleSheet<Style = theme::Container>,
    Message: TabMessage,
{
    fn children(&self) -> Vec<Tree> {
        self.elements.iter().map(Tree::new).collect()
    }

    fn diff(&mut self, tree: &mut Tree) {
        tree.diff_children(&mut self.elements)
    }

    fn width(&self) -> Length {
        Length::Fill
    }

    fn height(&self) -> Length {
        Length::Fill
    }

    fn layout(&self, renderer: &Renderer, limits: &Limits) -> Node {
        let min_size = Size {
            height: TAB_HEIGHT as f32,
            width: if self.active {
                MIN_ACTIVE_TAB_WIDTH as f32
            } else {
                MIN_TAB_WIDTH as f32
            },
        };
        let limits = limits
            .min_width(min_size.width)
            .min_height(min_size.height)
            .width(Length::Fill)
            .height(Length::Fill);
        let size = limits.resolve(min_size).max(min_size);

        let limits = Limits::new(size, size)
            .min_width(size.width)
            .min_height(size.height)
            .width(size.width)
            .height(size.height);
        cosmic::iced_core::layout::flex::resolve(
            cosmic::iced_core::layout::flex::Axis::Horizontal,
            renderer,
            &limits,
            0.into(),
            8.,
            cosmic::iced::Alignment::Center,
            if size.width >= CLOSE_BREAKPOINT as f32 {
                &self.elements
            } else if size.width >= TEXT_BREAKPOINT as f32 {
                &self.elements[0..3]
            } else {
                &self.elements[0..2]
            },
        )
    }

    fn operate(
        &self,
        tree: &mut Tree,
        layout: Layout<'_>,
        renderer: &Renderer,
        operation: &mut dyn Operation<OperationOutputWrapper<Message>>,
    ) {
        operation.container(None, &mut |operation| {
            self.elements
                .iter()
                .zip(&mut tree.children)
                .zip(layout.children())
                .for_each(|((child, state), layout)| {
                    child
                        .as_widget()
                        .operate(state, layout, renderer, operation);
                })
        });
    }

    fn on_event(
        &mut self,
        tree: &mut Tree,
        event: event::Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &Renderer,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Message>,
    ) -> event::Status {
        let status = self
            .elements
            .iter_mut()
            .zip(&mut tree.children)
            .zip(layout.children())
            .map(|((child, state), layout)| {
                child.as_widget_mut().on_event(
                    state,
                    event.clone(),
                    layout,
                    cursor_position,
                    renderer,
                    clipboard,
                    shell,
                )
            })
            .fold(event::Status::Ignored, event::Status::merge);

        if status == event::Status::Ignored
            && layout.bounds().contains(cursor_position)
            && matches!(
                event,
                event::Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))
            )
        {
            shell.publish(Message::activate(self.idx));
            return event::Status::Captured;
        }

        status
    }

    fn mouse_interaction(
        &self,
        tree: &Tree,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
        renderer: &Renderer,
    ) -> mouse::Interaction {
        self.elements
            .iter()
            .zip(&tree.children)
            .zip(layout.children())
            .map(|((child, state), layout)| {
                child.as_widget().mouse_interaction(
                    state,
                    layout,
                    cursor_position,
                    viewport,
                    renderer,
                )
            })
            .max()
            .unwrap_or_default()
    }

    fn draw(
        &self,
        tree: &Tree,
        renderer: &mut Renderer,
        theme: &Renderer::Theme,
        renderer_style: &renderer::Style,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
    ) {
        let style = theme.appearance(&self.background);

        draw_background(renderer, &style, layout.bounds());

        for ((child, state), layout) in self
            .elements
            .iter()
            .zip(&tree.children)
            .zip(layout.children())
        {
            child.as_widget().draw(
                state,
                renderer,
                theme,
                &renderer::Style {
                    text_color: style.text_color.unwrap_or(renderer_style.text_color),
                },
                layout,
                cursor_position,
                viewport,
            );
        }
    }

    fn overlay<'b>(
        &'b mut self,
        tree: &'b mut Tree,
        layout: Layout<'_>,
        renderer: &Renderer,
    ) -> Option<overlay::Element<'b, Message, Renderer>> {
        overlay::from_children(&mut self.elements, tree, layout, renderer)
    }
}
