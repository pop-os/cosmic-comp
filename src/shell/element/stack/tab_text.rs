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
        Background, Border, Color, Degrees, Gradient, Length, Rectangle, Size, Text,
    },
};

/// Text in a stack tab with an overflow gradient.
pub fn tab_text(text: String, selected: bool) -> TabText {
    TabText::new(text, selected)
}

struct LocalState {
    text_hash: u64,
    paragraph: <cosmic::Renderer as TextRenderer>::Paragraph,
    overflowed: bool,
}

/// Text in a stack tab with an overflow gradient.
pub struct TabText {
    text: String,
    font: cosmic::font::Font,
    font_size: f32,
    selected: bool,
    height: Length,
    width: Length,
}

impl TabText {
    pub fn new(text: String, selected: bool) -> Self {
        TabText {
            width: Length::Shrink,
            height: Length::Shrink,
            font: cosmic::font::default(),
            font_size: 14.0,
            selected,
            text,
        }
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
        self.selected.hash(&mut hasher);
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
            wrapping: cosmic::iced::advanced::text::Wrapping::None,
        })
    }
}

impl<Message> Widget<Message, cosmic::Theme, cosmic::Renderer> for TabText {
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

    fn size(&self) -> Size<Length> {
        Size::new(self.width, self.height)
    }

    fn layout(&self, tree: &mut Tree, _renderer: &cosmic::Renderer, limits: &Limits) -> Node {
        let state = tree.state.downcast_mut::<LocalState>();
        let text_bounds = state.paragraph.min_bounds();
        state.overflowed = limits.max().width < text_bounds.width;
        let actual_size = limits.resolve(self.width, self.height, text_bounds);

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
        theme: &cosmic::Theme,
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
                Point::new(bounds.x, bounds.height.mul_add(0.5, bounds.y)),
                style.text_color,
                bounds,
            );

            renderer.with_layer(bounds, |renderer| {
                if state.overflowed {
                    let overlay = match (theme.cosmic().is_dark, self.selected) {
                        (true, false) => Color {
                            a: 1.0,
                            r: 0.149,
                            g: 0.149,
                            b: 0.149,
                        },
                        (true, true) => Color {
                            a: 1.0,
                            r: 0.196,
                            g: 0.196,
                            b: 0.196,
                        },
                        (false, false) => Color {
                            a: 1.0,
                            r: 0.894,
                            g: 0.894,
                            b: 0.894,
                        },
                        (false, true) => Color {
                            a: 1.0,
                            r: 0.831,
                            g: 0.831,
                            b: 0.831,
                        },
                    };

                    let transparent = Color { a: 0.0, ..overlay };

                    renderer.fill_quad(
                        renderer::Quad {
                            bounds: Rectangle {
                                x: (bounds.x + bounds.width - 27.).max(bounds.x),
                                width: 27.0_f32.min(bounds.width),
                                ..bounds
                            },
                            border: Border {
                                radius: 0.0.into(),
                                width: 0.0,
                                color: Color::TRANSPARENT,
                            },
                            shadow: Default::default(),
                        },
                        Background::Gradient(Gradient::Linear(
                            gradient::Linear::new(Degrees(90.))
                                .add_stop(0.0, transparent)
                                .add_stop(1.0, overlay),
                        )),
                    );
                }
            });
        });
    }
}

impl<Message: 'static> From<TabText> for cosmic::Element<'_, Message> {
    fn from(value: TabText) -> Self {
        Self::new(value)
    }
}
