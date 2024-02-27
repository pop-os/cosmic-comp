use cosmic::{
    font::Font,
    iced::widget::{self, container::draw_background, rule::FillMode},
    iced_core::{
        alignment, event,
        layout::{Layout, Limits, Node},
        mouse, overlay, renderer,
        widget::{
            operation::{Operation, OperationOutputWrapper},
            tree::Tree,
            Id, Widget,
        },
        Clipboard, Color, Length, Rectangle, Shell, Size,
    },
    iced_widget::scrollable::AbsoluteOffset,
    theme,
    widget::{icon::from_name, Icon},
    Apply,
};

use super::tab_text::tab_text;

/// The background color of the stack tab header.
pub(super) fn primary_container_color(theme: &cosmic::cosmic_theme::Theme) -> Color {
    const PRIMARY_CONTAINER_DARK: Color = Color::from_rgba(0.149, 0.149, 0.149, 1.0);
    const PRIMARY_CONTAINER_LIGHT: Color = Color::from_rgba(0.894, 0.894, 0.894, 1.0);

    if theme.is_dark {
        PRIMARY_CONTAINER_DARK
    } else {
        PRIMARY_CONTAINER_LIGHT
    }
}

/// The background color for the selected stack tab.
pub(super) fn selected_state_color(theme: &cosmic::cosmic_theme::Theme) -> Color {
    const SELECTED_STATE_DARK: Color = Color::from_rgba(0.302, 0.302, 0.302, 0.3);
    const SELECTED_STATE_LIGHT: Color = Color::from_rgba(0.596, 0.596, 0.596, 0.2);

    if theme.is_dark {
        SELECTED_STATE_DARK
    } else {
        SELECTED_STATE_LIGHT
    }
}

#[derive(Clone, Copy)]
pub(super) enum TabRuleTheme {
    ActiveActivated,
    ActiveDeactivated,
    Default,
}

impl From<TabRuleTheme> for theme::Rule {
    fn from(theme: TabRuleTheme) -> Self {
        match theme {
            TabRuleTheme::ActiveActivated => Self::custom(|theme| widget::rule::Appearance {
                color: theme.cosmic().accent_color().into(),
                width: 4,
                radius: 0.0.into(),
                fill_mode: FillMode::Full,
            }),
            TabRuleTheme::ActiveDeactivated => Self::custom(|theme| widget::rule::Appearance {
                color: theme.cosmic().palette.neutral_5.into(),
                width: 4,
                radius: 0.0.into(),
                fill_mode: FillMode::Full,
            }),
            TabRuleTheme::Default => Self::custom(|theme| widget::rule::Appearance {
                color: theme.cosmic().palette.neutral_5.into(),
                width: 4,
                radius: 8.0.into(),
                fill_mode: FillMode::Padded(4),
            }),
        }
    }
}

#[derive(Clone, Copy)]
pub(super) enum TabBackgroundTheme {
    ActiveActivated,
    ActiveDeactivated,
    Default,
}

impl TabBackgroundTheme {
    /// Select the background color of stack tabs based on dark theme preference.
    fn background_color(self, theme: &theme::Theme) -> Color {
        match self {
            TabBackgroundTheme::ActiveActivated | TabBackgroundTheme::ActiveDeactivated => {
                selected_state_color(theme.cosmic())
            }

            TabBackgroundTheme::Default => primary_container_color(theme.cosmic()),
        }
    }
}

impl From<TabBackgroundTheme> for theme::Container {
    fn from(background_theme: TabBackgroundTheme) -> Self {
        match background_theme {
            TabBackgroundTheme::ActiveActivated => {
                Self::custom(move |theme| widget::container::Appearance {
                    icon_color: Some(Color::from(theme.cosmic().accent_text_color())),
                    text_color: Some(Color::from(theme.cosmic().accent_text_color())),
                    background: Some(background_theme.background_color(theme).into()),
                    border_radius: 0.0.into(),
                    border_width: 0.0,
                    border_color: Color::TRANSPARENT,
                })
            }
            TabBackgroundTheme::ActiveDeactivated => {
                Self::custom(move |theme| widget::container::Appearance {
                    icon_color: None,
                    text_color: None,
                    background: Some(background_theme.background_color(theme).into()),
                    border_radius: 0.0.into(),
                    border_width: 0.0,
                    border_color: Color::TRANSPARENT,
                })
            }
            TabBackgroundTheme::Default => Self::Transparent,
        }
    }
}

pub trait TabMessage: Clone {
    fn activate(idx: usize) -> Self;
    fn is_activate(&self) -> Option<usize>;

    fn scroll_further() -> Self;
    fn scroll_back() -> Self;
    fn populate_scroll(&mut self, current_offset: AbsoluteOffset) -> Option<AbsoluteOffset>;
    fn scrolled() -> Self;
}

pub struct Tab<Message: TabMessage> {
    id: Id,
    app_icon: Icon,
    title: String,
    font: Font,
    close_message: Option<Message>,
    press_message: Option<Message>,
    right_click_message: Option<Message>,
    rule_theme: TabRuleTheme,
    background_theme: TabBackgroundTheme,
    active: bool,
}

impl<Message: TabMessage + 'static> Tab<Message> {
    pub fn new(title: impl Into<String>, app_id: impl Into<String>, id: Id) -> Self {
        Tab {
            id,
            app_icon: from_name(app_id.into()).size(16).icon(),
            title: title.into(),
            font: cosmic::font::FONT,
            close_message: None,
            press_message: None,
            right_click_message: None,
            rule_theme: TabRuleTheme::Default,
            background_theme: TabBackgroundTheme::Default,
            active: false,
        }
    }

    pub fn on_press(mut self, message: Message) -> Self {
        self.press_message = Some(message);
        self
    }

    pub fn on_right_click(mut self, message: Message) -> Self {
        self.right_click_message = Some(message);
        self
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

    pub(super) fn internal(self, idx: usize) -> TabInternal<'static, Message> {
        let mut close_button = from_name("window-close-symbolic")
            .size(16)
            .prefer_svg(true)
            .icon()
            .apply(widget::button)
            .padding(0)
            .style(theme::iced::Button::Text);
        if let Some(close_message) = self.close_message {
            close_button = close_button.on_press(close_message);
        }

        let items = vec![
            widget::vertical_rule(4).style(self.rule_theme).into(),
            self.app_icon
                .clone()
                .apply(widget::container)
                .height(Length::Fill)
                .width(Length::Shrink)
                .padding([2, 4])
                .center_y()
                .into(),
            tab_text(self.title)
                .font(self.font)
                .font_size(14.0)
                .height(Length::Fill)
                .width(Length::Fill)
                .into(),
            close_button
                .apply(widget::container)
                .height(Length::Fill)
                .width(Length::Shrink)
                .padding([2, 4])
                .center_y()
                .align_x(alignment::Horizontal::Right)
                .into(),
        ];

        TabInternal {
            id: self.id,
            idx,
            active: self.active,
            background: self.background_theme.into(),
            elements: items,
            press_message: self.press_message,
            right_click_message: self.right_click_message,
        }
    }
}

const TAB_HEIGHT: i32 = 24;
pub const MIN_ACTIVE_TAB_WIDTH: i32 = 140;
const MIN_TAB_WIDTH: i32 = 38;

const TEXT_BREAKPOINT: i32 = 44;
const CLOSE_BREAKPOINT: i32 = 125;

pub(super) struct TabInternal<'a, Message: TabMessage> {
    id: Id,
    idx: usize,
    active: bool,
    background: theme::Container,
    elements: Vec<cosmic::Element<'a, Message>>,
    press_message: Option<Message>,
    right_click_message: Option<Message>,
}

impl<'a, Message: TabMessage> Widget<Message, cosmic::Renderer> for TabInternal<'a, Message> {
    fn id(&self) -> Option<Id> {
        Some(self.id.clone())
    }

    fn children(&self) -> Vec<Tree> {
        self.elements.iter().map(Tree::new).collect()
    }

    fn diff(&mut self, tree: &mut Tree) {
        tree.diff_children(&mut self.elements);
    }

    fn width(&self) -> Length {
        Length::Fill
    }

    fn height(&self) -> Length {
        Length::Fill
    }

    fn layout(&self, tree: &mut Tree, renderer: &cosmic::Renderer, limits: &Limits) -> Node {
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
            &mut tree.children,
        )
    }

    fn operate(
        &self,
        tree: &mut Tree,
        layout: Layout<'_>,
        renderer: &cosmic::Renderer,
        operation: &mut dyn Operation<OperationOutputWrapper<Message>>,
    ) {
        operation.container(None, layout.bounds(), &mut |operation| {
            self.elements
                .iter()
                .zip(&mut tree.children)
                .zip(layout.children())
                .for_each(|((child, state), layout)| {
                    child
                        .as_widget()
                        .operate(state, layout, renderer, operation);
                });
        });
    }

    fn on_event(
        &mut self,
        tree: &mut Tree,
        event: event::Event,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        renderer: &cosmic::Renderer,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Message>,
        viewport: &Rectangle,
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
                    cursor,
                    renderer,
                    clipboard,
                    shell,
                    viewport,
                )
            })
            .fold(event::Status::Ignored, event::Status::merge);

        if status == event::Status::Ignored && cursor.is_over(layout.bounds()) {
            if matches!(
                event,
                event::Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))
            ) {
                if let Some(message) = self.press_message.clone() {
                    shell.publish(message);
                    return event::Status::Captured;
                }
            }
            if matches!(
                event,
                event::Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Right))
            ) {
                if let Some(message) = self.right_click_message.clone() {
                    shell.publish(message);
                    return event::Status::Captured;
                }
            }
            if matches!(
                event,
                event::Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))
            ) {
                shell.publish(Message::activate(self.idx));
                return event::Status::Captured;
            }
        }

        status
    }

    fn mouse_interaction(
        &self,
        tree: &Tree,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        viewport: &Rectangle,
        renderer: &cosmic::Renderer,
    ) -> mouse::Interaction {
        self.elements
            .iter()
            .zip(&tree.children)
            .zip(layout.children())
            .map(|((child, state), layout)| {
                child
                    .as_widget()
                    .mouse_interaction(state, layout, cursor, viewport, renderer)
            })
            .max()
            .unwrap_or_default()
    }

    fn draw(
        &self,
        tree: &Tree,
        renderer: &mut cosmic::Renderer,
        theme: &cosmic::Theme,
        renderer_style: &renderer::Style,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        viewport: &Rectangle,
    ) {
        use cosmic::widget::container::StyleSheet;
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
                    icon_color: style.text_color.unwrap_or(renderer_style.text_color),
                    text_color: style.text_color.unwrap_or(renderer_style.text_color),
                    scale_factor: renderer_style.scale_factor,
                },
                layout,
                cursor,
                viewport,
            );
        }
    }

    fn overlay<'b>(
        &'b mut self,
        tree: &'b mut Tree,
        layout: Layout<'_>,
        renderer: &cosmic::Renderer,
    ) -> Option<overlay::Element<'b, Message, cosmic::Renderer>> {
        overlay::from_children(&mut self.elements, tree, layout, renderer)
    }
}
