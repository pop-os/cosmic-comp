use cosmic::{
    iced::Element,
    iced_core::{
        event, layout, mouse, overlay,
        renderer::{Quad, Style},
        widget::{tree, Id, OperationOutputWrapper, Tree, Widget},
        Background, Clipboard, Color, Event, Layout, Length, Rectangle, Renderer as IcedRenderer,
        Shell,
    },
    widget::button::StyleSheet,
};

pub struct SubmenuItem<'a, Message, Renderer>
where
    Renderer: IcedRenderer,
    Renderer::Theme: StyleSheet,
{
    elem: Element<'a, Message, Renderer>,
    idx: usize,
    styling: <Renderer::Theme as StyleSheet>::Style,
}

impl<'a, Message, Renderer> SubmenuItem<'a, Message, Renderer>
where
    Renderer: IcedRenderer,
    Renderer::Theme: StyleSheet,
{
    pub fn new(elem: impl Into<Element<'a, Message, Renderer>>, idx: usize) -> Self {
        Self {
            elem: elem.into(),
            idx,
            styling: Default::default(),
        }
    }

    pub fn style(mut self, style: <Renderer::Theme as StyleSheet>::Style) -> Self {
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

impl<'a, Message, Renderer> Widget<Message, Renderer> for SubmenuItem<'a, Message, Renderer>
where
    Renderer: IcedRenderer,
    Renderer::Theme: StyleSheet,
    Message: CursorEvents,
{
    fn id(&self) -> Option<Id> {
        None
    }

    fn width(&self) -> Length {
        self.elem.as_widget().width()
    }

    fn height(&self) -> Length {
        self.elem.as_widget().height()
    }

    fn layout(
        &self,
        state: &mut Tree,
        renderer: &Renderer,
        limits: &layout::Limits,
    ) -> layout::Node {
        let state = &mut state.children[0];
        let node = self.elem.as_widget().layout(state, renderer, limits);
        layout::Node::with_children(node.size(), vec![node])
    }

    fn draw(
        &self,
        state: &Tree,
        renderer: &mut Renderer,
        theme: &<Renderer as IcedRenderer>::Theme,
        style: &Style,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        viewport: &Rectangle,
    ) {
        let widget_state = state.state.downcast_ref::<State>();
        let styling = if widget_state.cursor_over {
            theme.hovered(true, &self.styling)
        } else {
            theme.active(true, &self.styling)
        };

        renderer.fill_quad(
            Quad {
                bounds: layout.bounds(),
                border_radius: styling.border_radius,
                border_width: styling.border_width,
                border_color: styling.border_color,
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
        renderer: &Renderer,
        operation: &mut dyn cosmic::widget::Operation<OperationOutputWrapper<Message>>,
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
        renderer: &Renderer,
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
        renderer: &Renderer,
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
        renderer: &Renderer,
    ) -> Option<overlay::Element<'b, Message, Renderer>> {
        let state = &mut state.children[0];
        let layout = layout.children().next().unwrap();
        self.elem.as_widget_mut().overlay(state, layout, renderer)
    }
}

impl<'a, Message, Renderer> Into<Element<'a, Message, Renderer>>
    for SubmenuItem<'a, Message, Renderer>
where
    Renderer: IcedRenderer + 'a,
    Renderer::Theme: StyleSheet,
    Message: CursorEvents + 'a,
{
    fn into(self) -> Element<'a, Message, Renderer> {
        Element::new(self)
    }
}
