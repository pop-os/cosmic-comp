use iced_core::{
    Background, Border, Color, Font, Length, Point, Rectangle, Renderer as _, Shell, Size, Vector,
    event,
    layout::{Layout, Limits, Node},
    mouse, overlay, renderer,
    svg::{self, Renderer as SvgRenderer},
    text::{LineHeight, Paragraph, Renderer as TextRenderer, Shaping},
    widget::{Id, Operation, Widget, tree::Tree},
};
use iced_widget::scrollable::AbsoluteOffset;

use crate::comp_theme::CompTheme;
use crate::utils::iced::CompElement;

pub trait TabMessage: Clone {
    fn activate(idx: usize) -> Self;

    fn scroll_further() -> Self;
    fn scroll_back() -> Self;
    fn populate_scroll(&mut self, current_offset: AbsoluteOffset) -> Option<AbsoluteOffset>;
    fn scrolled() -> Self;
}

/// Internal height of the tab content area (text + icon).
/// This is used for min-height calculations of individual tabs.
const TAB_CONTENT_HEIGHT: f32 = 28.0;

pub const MIN_ACTIVE_TAB_WIDTH: i32 = 140;
const MIN_TAB_WIDTH: i32 = 38;
const ICON_SIZE: f32 = 16.0;
const CLOSE_ICON_SIZE: f32 = 12.0;
const H_PADDING: f32 = 8.0;
const ICON_GAP: f32 = 6.0;
const CLOSE_BREAKPOINT: f32 = 125.0;
const TEXT_BREAKPOINT: f32 = 44.0;

/// Style configuration for a tab, pre-computed from theme tokens.
// MERGE: upstream's blur PR added frosted-window alpha to its libcosmic
// `TabRuleTheme`/`TabBackgroundTheme` styles. Not ported: this fork draws tabs manually
// from `CompTheme` and no longer has those cosmic theme wrappers.
#[derive(Clone, Copy)]
pub(super) struct TabStyle {
    pub background: Option<Color>,
    pub text_color: Color,
    pub radius: f32,
}

impl TabStyle {
    pub fn active(theme: &CompTheme) -> Self {
        TabStyle {
            background: Some(theme.surface_color()),
            text_color: theme.on_bg_color(),
            radius: theme.radius_s()[0],
        }
    }

    pub fn inactive(theme: &CompTheme) -> Self {
        TabStyle {
            background: None,
            text_color: theme.on_surface_color(),
            radius: theme.radius_s()[0],
        }
    }
}

pub struct Tab<Message: TabMessage> {
    id: Id,
    app_id: String,
    title: String,
    close_message: Option<Message>,
    press_message: Option<Message>,
    right_click_message: Option<Message>,
    active: bool,
}

impl<Message: TabMessage + 'static> Tab<Message> {
    pub fn new(title: impl Into<String>, app_id: impl Into<String>, id: Id) -> Self {
        Tab {
            id,
            app_id: app_id.into(),
            title: title.into(),
            close_message: None,
            press_message: None,
            right_click_message: None,
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

    pub(super) fn active(mut self) -> Self {
        self.active = true;
        self
    }

    pub(super) fn non_active(mut self) -> Self {
        self.active = false;
        self
    }

    /// Convert into the internal widget with pre-computed style.
    pub(super) fn internal(self, idx: usize, theme: &CompTheme) -> TabInternal<Message> {
        let style = if self.active {
            TabStyle::active(theme)
        } else {
            TabStyle::inactive(theme)
        };

        TabInternal {
            id: self.id,
            app_id: self.app_id,
            title: self.title,
            idx,
            active: self.active,
            style,
            press_message: self.press_message,
            right_click_message: self.right_click_message,
            close_message: self.close_message,
        }
    }
}

/// Local widget state stored in the tree.
struct TabState {
    /// Pre-measured title paragraph.
    paragraph: <iced_tiny_skia::Renderer as TextRenderer>::Paragraph,
    /// Hash of title+active to detect changes.
    content_hash: u64,
    /// App icon SVG handle (resolved from XDG).
    icon_handle: Option<iced_core::svg::Handle>,
    /// Close icon SVG handle.
    close_handle: iced_core::svg::Handle,
}

impl TabState {
    fn new(title: &str, active: bool, app_id: &str) -> Self {
        let font = if active {
            Font {
                weight: iced_core::font::Weight::Semibold,
                ..Font::DEFAULT
            }
        } else {
            Font::DEFAULT
        };

        let paragraph =
            <iced_tiny_skia::Renderer as TextRenderer>::Paragraph::with_text(iced_core::Text {
                content: title,
                size: iced_core::Pixels(13.0),
                bounds: Size::INFINITE,
                font,
                align_x: iced_core::text::Alignment::Left,
                align_y: iced_core::alignment::Vertical::Center,
                shaping: Shaping::Advanced,
                line_height: LineHeight::default(),
                wrapping: iced_core::text::Wrapping::None,
                letter_spacing: None,
                ellipsis: iced_core::text::Ellipsis::default(),
                hint_factor: None,
            });

        let icon_handle = crate::utils::xdg_icon::icon_handle(app_id);

        let close_handle = iced_core::svg::Handle::from_memory(icetron_themes::icons::X.bytes);

        let content_hash = Self::compute_hash(title, active);

        TabState {
            paragraph,
            content_hash,
            icon_handle,
            close_handle,
        }
    }

    fn compute_hash(title: &str, active: bool) -> u64 {
        use std::hash::{Hash, Hasher};
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        title.hash(&mut hasher);
        active.hash(&mut hasher);
        hasher.finish()
    }
}

/// A single tab rendered as a manual-draw widget (like icetron's chip).
/// No child elements - draws background, icon, text, and close button directly.
pub(super) struct TabInternal<Message: TabMessage> {
    id: Id,
    app_id: String,
    title: String,
    idx: usize,
    active: bool,
    style: TabStyle,
    press_message: Option<Message>,
    right_click_message: Option<Message>,
    close_message: Option<Message>,
}

impl<Message> Widget<Message, iced_core::Theme, iced_tiny_skia::Renderer> for TabInternal<Message>
where
    Message: TabMessage,
{
    fn id(&self) -> Option<Id> {
        Some(self.id.clone())
    }

    fn set_id(&mut self, id: Id) {
        self.id = id;
    }

    fn tag(&self) -> iced_core::widget::tree::Tag {
        iced_core::widget::tree::Tag::of::<TabState>()
    }

    fn state(&self) -> iced_core::widget::tree::State {
        iced_core::widget::tree::State::new(TabState::new(&self.title, self.active, &self.app_id))
    }

    fn diff(&self, tree: &mut Tree) {
        let state = tree.state.downcast_mut::<TabState>();
        let hash = TabState::compute_hash(&self.title, self.active);
        if state.content_hash != hash {
            *state = TabState::new(&self.title, self.active, &self.app_id);
        }
    }

    fn children(&self) -> Vec<Tree> {
        Vec::new()
    }

    fn size(&self) -> Size<Length> {
        Size::new(Length::Fill, Length::Fill)
    }

    fn layout(
        &mut self,
        _tree: &mut Tree,
        _renderer: &iced_tiny_skia::Renderer,
        limits: &Limits,
    ) -> Node {
        let min_width = if self.active {
            MIN_ACTIVE_TAB_WIDTH as f32
        } else {
            MIN_TAB_WIDTH as f32
        };

        let size = limits.resolve(
            Length::Fill,
            Length::Fill,
            Size::new(min_width, TAB_CONTENT_HEIGHT),
        );

        Node::new(Size::new(
            size.width.max(min_width),
            size.height.max(TAB_CONTENT_HEIGHT),
        ))
    }

    fn update(
        &mut self,
        _tree: &mut Tree,
        event: &event::Event,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        _renderer: &iced_tiny_skia::Renderer,
        shell: &mut Shell<'_, Message>,
        _viewport: &Rectangle,
    ) {
        if !cursor.is_over(layout.bounds()) {
            return;
        }

        match event {
            event::Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => {
                if let Some(message) = self.press_message.clone() {
                    shell.publish(message);
                    shell.capture_event();
                }
            }
            event::Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Right)) => {
                if let Some(message) = self.right_click_message.clone() {
                    shell.publish(message);
                    shell.capture_event();
                }
            }
            event::Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)) => {
                // Check if the click is on the close button area
                let bounds = layout.bounds();
                if bounds.width >= CLOSE_BREAKPOINT
                    && let Some(pos) = cursor.position()
                {
                    let close_x = bounds.x + bounds.width - H_PADDING - CLOSE_ICON_SIZE;
                    if pos.x >= close_x
                        && let Some(message) = self.close_message.clone()
                    {
                        shell.publish(message);
                        shell.capture_event();
                        return;
                    }
                }
                shell.publish(Message::activate(self.idx));
                shell.capture_event();
            }
            _ => {}
        }
    }

    fn mouse_interaction(
        &self,
        _tree: &Tree,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        _viewport: &Rectangle,
        _renderer: &iced_tiny_skia::Renderer,
    ) -> mouse::Interaction {
        if cursor.is_over(layout.bounds()) {
            mouse::Interaction::Pointer
        } else {
            mouse::Interaction::default()
        }
    }

    fn draw(
        &self,
        tree: &Tree,
        renderer: &mut iced_tiny_skia::Renderer,
        _theme: &iced_core::Theme,
        _style: &renderer::Style,
        layout: Layout<'_>,
        _cursor: mouse::Cursor,
        _viewport: &Rectangle,
    ) {
        let bounds = layout.bounds();
        let state = tree.state.downcast_ref::<TabState>();

        // 1. Draw background (only for active tab)
        if let Some(bg_color) = self.style.background {
            // Inset the background slightly for visual breathing room
            let bg_bounds = Rectangle {
                x: bounds.x + 2.0,
                y: bounds.y + 4.0,
                width: bounds.width - 4.0,
                height: bounds.height - 8.0,
            };
            renderer.fill_quad(
                renderer::Quad {
                    bounds: bg_bounds,
                    border: Border {
                        radius: self.style.radius.into(),
                        width: 0.0,
                        color: Color::TRANSPARENT,
                        ..Default::default()
                    },
                    shadow: Default::default(),
                    snap: true,
                    border_only: false,
                },
                Background::Color(bg_color),
            );
        }

        // Calculate content area (inset by padding)
        let content_x = bounds.x + H_PADDING;
        let content_width = bounds.width - H_PADDING * 2.0;
        let center_y = bounds.y + bounds.height / 2.0;

        // 2. Draw app icon (if wide enough and handle exists)
        let mut text_x = content_x;
        if bounds.width >= TEXT_BREAKPOINT && state.icon_handle.is_some() {
            let icon_y = center_y - ICON_SIZE / 2.0;
            if let Some(ref handle) = state.icon_handle {
                let icon_bounds = Rectangle {
                    x: content_x,
                    y: icon_y,
                    width: ICON_SIZE,
                    height: ICON_SIZE,
                };
                SvgRenderer::draw_svg(renderer, svg::Svg::new(handle.clone()), icon_bounds, bounds);
            }
            text_x = content_x + ICON_SIZE + ICON_GAP;
        }

        // 3. Draw title text (if wide enough)
        if bounds.width >= TEXT_BREAKPOINT {
            let show_close = bounds.width >= CLOSE_BREAKPOINT && self.close_message.is_some();
            let text_max_width = if show_close {
                content_width - (text_x - content_x) - CLOSE_ICON_SIZE - ICON_GAP
            } else {
                content_width - (text_x - content_x)
            };

            if text_max_width > 0.0 {
                let text_bounds = state.paragraph.min_bounds();
                let text_y = center_y - text_bounds.height / 2.0;

                // Clip to available width
                let clip_rect = Rectangle {
                    x: text_x,
                    y: bounds.y,
                    width: text_max_width,
                    height: bounds.height,
                };

                renderer.with_layer(clip_rect, |renderer| {
                    renderer.fill_paragraph(
                        &state.paragraph,
                        Point::new(text_x, text_y),
                        self.style.text_color,
                        clip_rect,
                    );
                });
            }
        }

        // 4. Draw close button icon (if wide enough)
        if bounds.width >= CLOSE_BREAKPOINT && self.close_message.is_some() {
            let close_x = bounds.x + bounds.width - H_PADDING - CLOSE_ICON_SIZE;
            let close_y = center_y - CLOSE_ICON_SIZE / 2.0;
            let close_color = Color {
                a: 0.6,
                ..self.style.text_color
            };
            let close_bounds = Rectangle {
                x: close_x,
                y: close_y,
                width: CLOSE_ICON_SIZE,
                height: CLOSE_ICON_SIZE,
            };
            let mut close_svg = svg::Svg::new(state.close_handle.clone());
            close_svg.color = Some(close_color);
            SvgRenderer::draw_svg(renderer, close_svg, close_bounds, bounds);
        }
    }

    fn operate(
        &mut self,
        _tree: &mut Tree,
        _layout: Layout<'_>,
        _renderer: &iced_tiny_skia::Renderer,
        _operation: &mut dyn Operation,
    ) {
    }

    fn overlay<'b>(
        &'b mut self,
        _tree: &'b mut Tree,
        _layout: Layout<'b>,
        _renderer: &iced_tiny_skia::Renderer,
        _viewport: &Rectangle,
        _translation: Vector,
    ) -> Option<overlay::Element<'b, Message, iced_core::Theme, iced_tiny_skia::Renderer>> {
        None
    }
}

impl<Message: TabMessage + 'static> From<TabInternal<Message>> for CompElement<'_, Message> {
    fn from(tab: TabInternal<Message>) -> Self {
        Self::new(tab)
    }
}
