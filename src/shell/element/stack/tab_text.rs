use std::hash::{Hash, Hasher};

use cosmic::{
    iced::{alignment, Point},
    iced_core::{
        gradient,
        layout::{Layout, Limits, Node},
        mouse::Cursor,
        renderer::{self, Renderer as IcedRenderer},
        text::{LineHeight, Paragraph, Renderer as TextRenderer, Shaping},
        widget::{tree, Tree, Widget},
        Background, Color, Degrees, Gradient, Length, Rectangle, Size, Text,
    },
};

/// Text in a stack tab with an overflow gradient.
pub fn tab_text(text: String) -> TabText {
    TabText::new(text, Color::TRANSPARENT)
}

struct LocalState {
    text_hash: u64,
    paragraph: <cosmic::Renderer as TextRenderer>::Paragraph,
    overflowed: bool,
}

/// Text in a stack tab with an overflow gradient.
pub struct TabText {
    text: String,
    background: Color,
    font: cosmic::font::Font,
    font_size: f32,
    height: Length,
    width: Length,
}

impl TabText {
    pub fn new(text: String, background: Color) -> Self {
        TabText {
            width: Length::Shrink,
            height: Length::Shrink,
            background,
            font: cosmic::font::DEFAULT,
            font_size: 14.0,
            text,
        }
    }

    pub fn background(mut self, background: Color) -> Self {
        self.background = background;
        self
    }

    pub fn font(mut self, font: cosmic::font::Font) -> Self {
        self.font = font;
        self
    }

    pub fn font_size(mut self, font_size: f32) -> Self {
        self.font_size = font_size;
        self
    }

    pub fn width(mut self, width: impl Into<Length>) -> Self {
        let width = width.into();
        self.width = width;
        self
    }

    pub fn height(mut self, height: impl Into<Length>) -> Self {
        let height = height.into();
        self.height = height;
        self
    }

    fn create_hash(&self) -> u64 {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        self.text.hash(&mut hasher);
        hasher.finish()
    }

    fn create_paragraph(&self) -> <cosmic::Renderer as TextRenderer>::Paragraph {
        <cosmic::Renderer as TextRenderer>::Paragraph::with_text(Text {
            content: &self.text,
            size: cosmic::iced_core::Pixels(self.font_size),
            bounds: Size::INFINITY,
            font: self.font,
            horizontal_alignment: alignment::Horizontal::Left,
            vertical_alignment: alignment::Vertical::Center,
            shaping: Shaping::Advanced,
            line_height: LineHeight::default(),
        })
    }
}

impl<Message> Widget<Message, cosmic::Renderer> for TabText {
    fn tag(&self) -> tree::Tag {
        tree::Tag::of::<LocalState>()
    }

    fn state(&self) -> tree::State {
        tree::State::new(LocalState {
            text_hash: self.create_hash(),
            paragraph: self.create_paragraph(),
            overflowed: false,
        })
    }

    fn width(&self) -> Length {
        self.width
    }
    fn height(&self) -> Length {
        self.height
    }

    fn layout(&self, tree: &mut Tree, _renderer: &cosmic::Renderer, limits: &Limits) -> Node {
        let limits = limits.width(self.width).height(self.height);

        let state = tree.state.downcast_mut::<LocalState>();
        let text_bounds = state.paragraph.min_bounds();
        state.overflowed = limits.max().width < text_bounds.width;
        let actual_size = limits.resolve(text_bounds);

        Node::new(actual_size)
    }

    fn diff(&mut self, tree: &mut Tree) {
        // If the text changes, update the paragraph.
        let state = tree.state.downcast_mut::<LocalState>();
        let text_hash = self.create_hash();
        if state.text_hash != text_hash {
            state.text_hash = text_hash;
            state.paragraph = self.create_paragraph();
        }
    }

    fn draw(
        &self,
        tree: &Tree,
        renderer: &mut cosmic::Renderer,
        _theme: &cosmic::Theme,
        style: &renderer::Style,
        layout: Layout<'_>,
        _cursor: Cursor,
        _viewport: &Rectangle,
    ) {
        let bounds = layout.bounds();
        let state = tree.state.downcast_ref::<LocalState>();

        renderer.with_layer(bounds, |renderer| {
            renderer.fill_paragraph(
                &state.paragraph,
                Point::new(bounds.x, bounds.y + bounds.height / 2.0),
                style.text_color,
                bounds,
            );
        });

        if state.overflowed {
            let gradient_bounds = Rectangle {
                x: (bounds.x + bounds.width - 24.).max(bounds.x),
                width: 24.0_f32.min(bounds.width),
                ..bounds
            };

            let mut transparent_background = self.background;
            transparent_background.a = 0.0;
            renderer.fill_quad(
                renderer::Quad {
                    bounds: gradient_bounds,
                    border_radius: 0.0.into(),
                    border_width: 0.0,
                    border_color: Color::TRANSPARENT,
                },
                Background::Gradient(Gradient::Linear(
                    gradient::Linear::new(Degrees(90.))
                        .add_stop(0.0, transparent_background)
                        .add_stop(1.0, self.background),
                )),
            );
        }
    }
}

impl<Message: 'static> From<TabText> for cosmic::Element<'_, Message> {
    fn from(value: TabText) -> Self {
        Self::new(value)
    }
}
