use cosmic::{
    iced::Element,
    iced_core::{
        Background, Border, Clipboard, Color, Event, Layout, Length, Rectangle,
        Renderer as IcedRenderer, Shell, Size, event, layout, mouse, overlay,
        renderer::{Quad, Style},
        widget::{Id, Tree, Widget, tree},
    },
    widget::button::Catalog,
};

pub struct SubmenuItem<'a, Message> {
    elem: cosmic::Element<'a, Message>,
    idx: usize,
    styling: <cosmic::Theme as Catalog>::Class,
}

impl<'a, Message> SubmenuItem<'a, Message> {
    pub fn new(elem: impl Into<cosmic::Element<'a, Message>>, idx: usize) -> Self {
        Self {
            elem: elem.into(),
            idx,
            styling: Default::default(),
        }
    }

    pub fn style(mut self, style: <cosmic::Theme as Catalog>::Class) -> Self {
        self.styling = style;
        self
    }
}

pub trait CursorEvents {
    fn cursor_entered(idx: usize, bounds: Rectangle<f32>) -> Self;
    fn cursor_left(idx: usize, bounds: Rectangle<f32>) -> Self;
}

struct State {
    cursor_over: bool,
}

impl<Message> Widget<Message, cosmic::Theme, cosmic::Renderer> for SubmenuItem<'_, Message>
where
    Message: CursorEvents,
{
    fn id(&self) -> Option<Id> {
        None
    }

    fn size(&self) -> Size<Length> {
        self.elem.as_widget().size()
    }

    fn layout(
        &self,
        state: &mut Tree,
        renderer: &cosmic::Renderer,
        limits: &layout::Limits,
    ) -> layout::Node {
        let state = &mut state.children[0];
        let node = self.elem.as_widget().layout(state, renderer, limits);
        layout::Node::with_children(node.size(), vec![node])
    }

    fn draw(
        &self,
        state: &Tree,
        renderer: &mut cosmic::Renderer,
        theme: &cosmic::Theme,
        style: &Style,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        viewport: &Rectangle,
    ) {
        let widget_state = state.state.downcast_ref::<State>();
        let styling = if widget_state.cursor_over {
            theme.hovered(true, false, &self.styling)
        } else {
            theme.active(true, false, &self.styling)
        };

        renderer.fill_quad(
            Quad {
                bounds: layout.bounds(),
                border: Border {
                    radius: styling.border_radius,
                    width: styling.border_width,
                    color: styling.border_color,
                },
                shadow: Default::default(),
            },
            styling
                .background
                .unwrap_or(Background::Color(Color::TRANSPARENT)),
        );

        let state = &state.children[0];
        let layout = layout.children().next().unwrap();
        self.elem.as_widget().draw(
            state,
            renderer,
            theme,
            &Style {
                text_color: styling.text_color.unwrap_or(style.text_color),
                icon_color: styling.icon_color.unwrap_or(style.text_color),
                ..*style
            },
            layout,
            cursor,
            viewport,
        )
    }

    fn tag(&self) -> tree::Tag {
        tree::Tag::of::<State>()
    }

    fn state(&self) -> tree::State {
        tree::State::new(State { cursor_over: false })
    }

    fn children(&self) -> Vec<Tree> {
        vec![Tree::new(&self.elem)]
    }

    fn diff(&mut self, tree: &mut Tree) {
        tree.diff_children(std::slice::from_mut(&mut self.elem))
    }

    fn operate(
        &self,
        state: &mut Tree,
        layout: Layout<'_>,
        renderer: &cosmic::Renderer,
        operation: &mut dyn cosmic::widget::Operation<()>,
    ) {
        let state = &mut state.children[0];
        let layout = layout.children().next().unwrap();
        self.elem
            .as_widget()
            .operate(state, layout, renderer, operation)
    }

    fn on_event(
        &mut self,
        state: &mut Tree,
        event: Event,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        renderer: &cosmic::Renderer,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Message>,
        viewport: &Rectangle,
    ) -> event::Status {
        let mut bounds = layout.bounds();

        // fix padding 1 and event... don't ask.
        bounds.x -= 1.;
        bounds.width += 2.;

        let is_over = cursor.is_over(bounds);
        let widget_state = state.state.downcast_mut::<State>();
        match event {
            Event::Mouse(mouse::Event::CursorEntered)
            | Event::Mouse(mouse::Event::CursorMoved { .. })
                if is_over && !widget_state.cursor_over =>
            {
                shell.publish(Message::cursor_entered(self.idx, bounds));
                widget_state.cursor_over = true;
            }
            Event::Mouse(mouse::Event::CursorMoved { .. })
            | Event::Mouse(mouse::Event::CursorLeft)
                if !is_over && widget_state.cursor_over =>
            {
                shell.publish(Message::cursor_left(self.idx, bounds));
                widget_state.cursor_over = false;
            }
            _ => {}
        };

        let state = &mut state.children[0];
        let layout = layout.children().next().unwrap();
        self.elem.as_widget_mut().on_event(
            state, event, layout, cursor, renderer, clipboard, shell, viewport,
        )
    }

    fn mouse_interaction(
        &self,
        state: &Tree,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        viewport: &Rectangle,
        renderer: &cosmic::Renderer,
    ) -> mouse::Interaction {
        let state = &state.children[0];
        let layout = layout.children().next().unwrap();
        self.elem
            .as_widget()
            .mouse_interaction(state, layout, cursor, viewport, renderer)
    }

    fn overlay<'b>(
        &'b mut self,
        state: &'b mut Tree,
        layout: Layout<'_>,
        renderer: &cosmic::Renderer,
        translation: cosmic::iced::Vector,
    ) -> Option<overlay::Element<'b, Message, cosmic::Theme, cosmic::Renderer>> {
        let state = &mut state.children[0];
        let layout = layout.children().next().unwrap();
        self.elem
            .as_widget_mut()
            .overlay(state, layout, renderer, translation)
    }
}

impl<'a, Message> From<SubmenuItem<'a, Message>> for cosmic::Element<'a, Message>
where
    Message: CursorEvents + 'a,
{
    fn from(val: SubmenuItem<'a, Message>) -> Self {
        Element::new(val)
    }
}
