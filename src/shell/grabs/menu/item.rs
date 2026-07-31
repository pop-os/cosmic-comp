// MERGE: this widget was ported off libcosmic in the fork (iced_core + iced_tiny_skia +
// CompTheme). Upstream's changes here were libcosmic-only (button::Catalog styling and the
// `theme.list_item_position` menu-item radius fixup) and have no equivalent; dropped.
use crate::comp_theme::CompTheme;
use crate::utils::iced::CompElement;
use iced_core::{
    Background, Border, Color, Event, Layout, Length, Rectangle, Renderer as IcedRenderer, Shell,
    Size, Vector, layout, mouse, overlay,
    renderer::{Quad, Style},
    widget::{Id, Tree, Widget, tree},
};

pub struct SubmenuItem<'a, Message> {
    elem: CompElement<'a, Message>,
    idx: usize,
    /// Pre-computed hover highlight color.
    hover_color: Color,
    /// Pre-computed hover border radius.
    hover_radius: iced_core::border::Radius,
}

impl<'a, Message> SubmenuItem<'a, Message> {
    pub fn new(elem: impl Into<CompElement<'a, Message>>, idx: usize, theme: &CompTheme) -> Self {
        let surface = theme.surface_color();
        Self {
            elem: elem.into(),
            idx,
            hover_color: Color::from_rgba(
                surface.r + 0.1,
                surface.g + 0.1,
                surface.b + 0.1,
                surface.a,
            ),
            hover_radius: crate::comp_theme::radius_from_array(theme.radius_s()),
        }
    }
}

pub trait CursorEvents {
    fn cursor_entered(idx: usize, bounds: Rectangle<f32>) -> Self;
    fn cursor_left(idx: usize, bounds: Rectangle<f32>) -> Self;
}

struct State {
    cursor_over: bool,
}

impl<Message> Widget<Message, iced_core::Theme, iced_tiny_skia::Renderer>
    for SubmenuItem<'_, Message>
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
        &mut self,
        state: &mut Tree,
        renderer: &iced_tiny_skia::Renderer,
        limits: &layout::Limits,
    ) -> layout::Node {
        let state = &mut state.children[0];
        let node = self.elem.as_widget_mut().layout(state, renderer, limits);
        layout::Node::with_children(node.size(), vec![node])
    }

    fn draw(
        &self,
        state: &Tree,
        renderer: &mut iced_tiny_skia::Renderer,
        theme: &iced_core::Theme,
        style: &Style,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        viewport: &Rectangle,
    ) {
        let widget_state = state.state.downcast_ref::<State>();

        // Hover highlight
        if widget_state.cursor_over {
            renderer.fill_quad(
                Quad {
                    bounds: layout.bounds(),
                    border: Border {
                        radius: self.hover_radius,
                        ..Default::default()
                    },
                    shadow: Default::default(),
                    snap: true,
                    border_only: false,
                },
                Background::Color(self.hover_color),
            );
        }

        let state = &state.children[0];
        let layout = layout.children().next().unwrap();
        self.elem
            .as_widget()
            .draw(state, renderer, theme, style, layout, cursor, viewport)
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

    fn diff(&self, tree: &mut Tree) {
        tree.diff_children(std::slice::from_ref(&self.elem))
    }

    fn operate(
        &mut self,
        state: &mut Tree,
        layout: Layout<'_>,
        renderer: &iced_tiny_skia::Renderer,
        operation: &mut dyn iced_core::widget::Operation,
    ) {
        let state = &mut state.children[0];
        let layout = layout.children().next().unwrap();
        self.elem
            .as_widget_mut()
            .operate(state, layout, renderer, operation)
    }

    fn update(
        &mut self,
        state: &mut Tree,
        event: &Event,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        renderer: &iced_tiny_skia::Renderer,
        shell: &mut Shell<'_, Message>,
        viewport: &Rectangle,
    ) {
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
        self.elem
            .as_widget_mut()
            .update(state, event, layout, cursor, renderer, shell, viewport);
    }

    fn mouse_interaction(
        &self,
        state: &Tree,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        viewport: &Rectangle,
        renderer: &iced_tiny_skia::Renderer,
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
        layout: Layout<'b>,
        renderer: &iced_tiny_skia::Renderer,
        _viewport: &Rectangle,
        translation: Vector,
    ) -> Option<overlay::Element<'b, Message, iced_core::Theme, iced_tiny_skia::Renderer>> {
        let state = &mut state.children[0];
        let layout = layout.children().next().unwrap();
        self.elem
            .as_widget_mut()
            .overlay(state, layout, renderer, _viewport, translation)
    }
}

impl<'a, Message> From<SubmenuItem<'a, Message>> for CompElement<'a, Message>
where
    Message: CursorEvents + 'a,
{
    fn from(val: SubmenuItem<'a, Message>) -> Self {
        iced_core::Element::new(val)
    }
}
