use std::hash::{Hash, Hasher};

use cosmic::{
    font::Font,
    iced::{
        widget::{self, container::draw_background, rule::FillMode},
        Background, Padding,
    },
    iced_core::{
        alignment, event,
        layout::{Layout, Limits, Node},
        mouse, overlay, renderer,
        widget::{
            operation::Operation,
            tree::{self, Tree},
            Id, Widget,
        },
        Border, Clipboard, Color, Length, Rectangle, Shell, Size,
    },
    iced_widget::scrollable::AbsoluteOffset,
    theme,
    widget::icon,
    Apply,
};

use super::tab_text::tab_text;
use crate::shell::CosmicSurface;

#[derive(Clone, Copy)]
pub(super) enum TabRuleTheme {
    ActiveActivated,
    ActiveDeactivated,
    Default,
}

impl From<TabRuleTheme> for theme::Rule {
    fn from(theme: TabRuleTheme) -> Self {
        match theme {
            TabRuleTheme::ActiveActivated => Self::custom(|theme| widget::rule::Style {
                color: theme.cosmic().accent_color().into(),
                width: 4,
                radius: 0.0.into(),
                fill_mode: FillMode::Full,
            }),
            TabRuleTheme::ActiveDeactivated => Self::custom(|theme| widget::rule::Style {
                color: theme.cosmic().palette.neutral_5.into(),
                width: 4,
                radius: 0.0.into(),
                fill_mode: FillMode::Full,
            }),
            TabRuleTheme::Default => Self::custom(|theme| widget::rule::Style {
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
    /// Selected active stack
    ActiveActivated,
    /// Selected inactive stack
    ActiveDeactivated,
    /// Not selected
    Default,
}

impl TabBackgroundTheme {
    pub fn into_container_theme(self, hovered: bool) -> theme::Container<'static> {
        let (selected, active_stack) = match self {
            Self::ActiveActivated => (true, true),
            Self::ActiveDeactivated => (true, false),
            Self::Default => (false, false),
        };

        theme::Container::custom(move |theme| {
            let cosmic_theme = theme.cosmic();

            let text_color = if selected && active_stack {
                Some(Color::from(cosmic_theme.accent_text_color()))
            } else {
                None
            };

            widget::container::Style {
                icon_color: text_color,
                text_color,
                background: Some(Background::Color(
                    if hovered {
                        cosmic_theme.primary.component.hover_state_color()
                    } else if selected {
                        cosmic_theme.primary.component.selected_state_color()
                    } else {
                        cosmic_theme.primary.component.base
                    }
                    .into(),
                )),
                border: Border {
                    radius: 0.0.into(),
                    width: 0.0,
                    color: Color::TRANSPARENT,
                },
                shadow: Default::default(),
            }
        })
    }
}

pub trait TabMessage: Clone {
    fn activate(idx: usize) -> Self;

    fn scroll_further() -> Self;
    fn scroll_back() -> Self;
    fn populate_scroll(&mut self, current_offset: AbsoluteOffset) -> Option<AbsoluteOffset>;
    fn scrolled() -> Self;
}

#[derive(Debug, Clone)]
pub struct Model {
    pub id: Id,
    pub app_icon: cosmic::widget::icon::Handle,
    pub title: String,
    pub title_hash: u64,
}

impl Model {
    pub fn new(id: Id, appid: String, title: String) -> Self {
        // Pre-emptively cache the hash of each title for more efficient diffing.
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        title.hash(&mut hasher);

        Self {
            id,
            app_icon: icon::from_name(appid).size(16).handle(),
            title,
            title_hash: hasher.finish(),
        }
    }
}

impl From<&CosmicSurface> for Model {
    fn from(window: &CosmicSurface) -> Self {
        let user_data = window.user_data();
        user_data.insert_if_missing(Id::unique);
        Self::new(
            user_data.get::<Id>().unwrap().clone(),
            window.app_id(),
            window.title(),
        )
    }
}

struct LocalState {
    hovered: bool,
}

pub struct Tab<'a, Message: TabMessage> {
    model: &'a Model,
    font: Font,
    close_message: Option<Message>,
    press_message: Option<Message>,
    right_click_message: Option<Message>,
    rule_theme: TabRuleTheme,
    background_theme: TabBackgroundTheme,
    active: bool,
}

impl<'a, Message: TabMessage + 'static> Tab<'a, Message> {
    pub fn new(model: &'a Model) -> Self {
        Tab {
            model,
            font: cosmic::font::default(),
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

    pub(super) fn active(mut self) -> Self {
        self.active = true;
        self
    }

    pub(super) fn internal(self, idx: usize) -> TabInternal<'a, Message> {
        let mut close_button = icon::from_name("window-close-symbolic")
            .size(16)
            .prefer_svg(true)
            .icon()
            .apply(widget::button)
            .padding(0)
            .class(theme::iced::Button::Text);
        if let Some(close_message) = self.close_message {
            close_button = close_button.on_press(close_message);
        }

        let items = [
            widget::vertical_rule(4).class(self.rule_theme).into(),
            cosmic::widget::icon(self.model.app_icon.clone())
                .clone()
                .apply(widget::container)
                .width(Length::Shrink)
                .padding([2, 4, 2, 8])
                .center_y(Length::Fill)
                .into(),
            tab_text(&self.model, self.active)
                .font(self.font)
                .font_size(14.0)
                .height(Length::Fill)
                .width(Length::Fill)
                .into(),
            close_button
                .apply(widget::container)
                .width(Length::Shrink)
                .padding([2, 12, 2, 4])
                .center_y(Length::Fill)
                .align_x(alignment::Horizontal::Right)
                .into(),
        ];

        TabInternal {
            id: self.model.id.clone(),
            idx,
            active: self.active,
            background: self.background_theme,
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
    background: TabBackgroundTheme,
    elements: [cosmic::Element<'a, Message>; 4],
    press_message: Option<Message>,
    right_click_message: Option<Message>,
}

impl<'a, Message> Widget<Message, cosmic::Theme, cosmic::Renderer> for TabInternal<'a, Message>
where
    Message: TabMessage,
{
    fn id(&self) -> Option<Id> {
        Some(self.id.clone())
    }

    fn children(&self) -> Vec<Tree> {
        self.elements.iter().map(Tree::new).collect()
    }

    fn set_id(&mut self, id: Id) {
        self.id = id;
    }

    fn diff(&mut self, tree: &mut Tree) {
        tree.diff_children(&mut self.elements);
    }

    fn size(&self) -> Size<Length> {
        Size::new(Length::Fill, Length::Fill)
    }

    fn state(&self) -> tree::State {
        tree::State::new(LocalState { hovered: false })
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
        let size = limits
            .resolve(Length::Shrink, Length::Shrink, min_size)
            .max(min_size);

        let limits = Limits::new(size, size)
            .min_width(size.width)
            .min_height(size.height)
            .width(size.width)
            .height(size.height);

        cosmic::iced_core::layout::flex::resolve(
            cosmic::iced_core::layout::flex::Axis::Horizontal,
            renderer,
            &limits,
            Length::Fill,
            Length::Fill,
            Padding::ZERO,
            0.,
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
        operation: &mut dyn Operation<()>,
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
        let state = tree.state.downcast_mut::<LocalState>();
        state.hovered = cursor.is_over(layout.bounds());

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

        if status == event::Status::Ignored && state.hovered {
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
        use cosmic::widget::container::Catalog;
        let state = tree.state.downcast_ref::<LocalState>();

        let style = theme.style(&self.background.into_container_theme(state.hovered));
        let text_color = style.text_color.unwrap_or(renderer_style.text_color);

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
                    icon_color: text_color,
                    text_color,
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
        translation: cosmic::iced::Vector,
    ) -> Option<overlay::Element<'b, Message, cosmic::Theme, cosmic::Renderer>> {
        overlay::from_children(&mut self.elements, tree, layout, renderer, translation)
    }
}
