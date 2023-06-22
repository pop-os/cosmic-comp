use cosmic::{
    iced::Element,
    iced_core::{
        gradient,
        layout::{Layout, Limits, Node},
        mouse::Cursor,
        renderer,
        widget::{Tree, Widget},
        Background, Color, Degrees, Gradient, Length, Point, Rectangle, Size,
    },
    iced_widget::text::StyleSheet as TextStyleSheet,
};

pub struct TabText<'a, Message, Renderer>
where
    Renderer: cosmic::iced_core::Renderer,
    Renderer::Theme: TextStyleSheet,
{
    text: Element<'a, Message, Renderer>,
    background: Color,
    height: Length,
    width: Length,
}

pub fn tab_text<'a, Message, Renderer>(
    text: impl Into<Element<'a, Message, Renderer>>,
) -> TabText<'a, Message, Renderer>
where
    Renderer: cosmic::iced_core::Renderer,
    Renderer::Theme: TextStyleSheet,
{
    TabText::new(text, Color::TRANSPARENT)
}

impl<'a, Message, Renderer> TabText<'a, Message, Renderer>
where
    Renderer: cosmic::iced_core::Renderer,
    Renderer::Theme: TextStyleSheet,
{
    pub fn new(text: impl Into<Element<'a, Message, Renderer>>, background: Color) -> Self {
        TabText {
            width: Length::Shrink,
            height: Length::Shrink,
            background,
            text: text.into(),
        }
    }

    pub fn background(mut self, background: Color) -> Self {
        self.background = background;
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
}

impl<'a, Message, Renderer> Widget<Message, Renderer> for TabText<'a, Message, Renderer>
where
    Renderer: cosmic::iced_core::Renderer,
    Renderer::Theme: TextStyleSheet,
{
    fn width(&self) -> Length {
        self.width
    }
    fn height(&self) -> Length {
        self.height
    }

    fn children(&self) -> Vec<Tree> {
        vec![Tree::new(&self.text)]
    }

    fn diff(&mut self, tree: &mut Tree) {
        tree.diff_children(std::slice::from_mut(&mut self.text))
    }

    fn layout(&self, renderer: &Renderer, limits: &Limits) -> Node {
        let limits = limits.width(self.width).height(self.height);
        let child_limits = Limits::new(
            Size::new(limits.min().width, limits.min().height - 4.),
            Size::new(limits.max().width * 2., limits.max().height - 4.),
        );
        let mut content = self.text.as_widget().layout(renderer, &child_limits);
        content.move_to(Point::new(0., 2.));
        let size = limits.resolve(content.size());

        Node::with_children(size, vec![content])
    }

    fn draw(
        &self,
        tree: &Tree,
        renderer: &mut Renderer,
        theme: &Renderer::Theme,
        style: &renderer::Style,
        layout: Layout<'_>,
        cursor: Cursor,
        _viewport: &Rectangle,
    ) {
        let bounds = layout.bounds();
        let content_layout = layout.children().next().unwrap();

        renderer.with_layer(bounds, |renderer| {
            self.text.as_widget().draw(
                &tree.children[0],
                renderer,
                theme,
                style,
                content_layout,
                cursor,
                &bounds,
            );
        });

        let gradient_bounds = Rectangle {
            x: (bounds.x + bounds.width - 24.).max(bounds.x),
            width: 24.0_f32.min(bounds.width),
            ..bounds
        };

        let mut transparent_background = self.background.clone();
        transparent_background.a = 0.0;
        renderer.fill_quad(
            renderer::Quad {
                bounds: gradient_bounds,
                border_radius: 0.0.into(),
                border_width: 0.0,
                border_color: Color::TRANSPARENT,
            },
            Background::Gradient(Gradient::Linear(
                gradient::Linear::new(Degrees(180.))
                    .add_stop(0.0, transparent_background)
                    .add_stop(1.0, self.background),
            )),
        );
    }
}

impl<'a, Message, Renderer> Into<Element<'a, Message, Renderer>> for TabText<'a, Message, Renderer>
where
    Renderer: cosmic::iced_core::Renderer + 'a,
    Renderer::Theme: TextStyleSheet,
    Message: 'a,
{
    fn into(self) -> Element<'a, Message, Renderer> {
        Element::new(self)
    }
}
