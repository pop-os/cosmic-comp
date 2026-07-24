use super::tab::{MIN_ACTIVE_TAB_WIDTH, Tab, TabMessage};
use iced_core::{
    Alignment, Element, Length, Point, Rectangle, Renderer as _, Shell, Size, Vector, event,
    layout::{Layout, Limits, Node, flex},
    mouse, overlay, renderer,
    widget::{
        Id, Operation, Widget,
        operation::scrollable::{AbsoluteOffset, RelativeOffset, Scrollable},
        tree::{self, Tree},
    },
};

use crate::comp_theme::CompTheme;
use crate::utils::iced::CompElement;
use icetron_p::prelude::{ButtonIconSize, ButtonIconType, IconButton, Source};

use keyframe::{ease, functions::EaseInOutCubic};
use std::time::{Duration, Instant};

pub struct Tabs<'a, Message> {
    elements: Vec<CompElement<'a, Message>>,
    id: Option<Id>,
    height: Length,
    width: Length,
    scroll_to: Option<usize>,
    /// Tab-strip scroll duration, resolved from the theme at construction (the
    /// `Widget`/`Scrollable` impls have no theme handle of their own).
    scroll_duration: Duration,
}

#[derive(Debug, Clone, Copy)]
struct ScrollAnimationState {
    start_time: Instant,
    start: Offset,
    end: Offset,
    extra: Offset,
}

/// The local state of [`Tabs`].
#[derive(Debug, Clone)]
pub struct State {
    offset_x: Offset,
    scroll_animation: Option<ScrollAnimationState>,
    scroll_to: Option<usize>,
}

impl Scrollable for State {
    fn snap_to(&mut self, offset: RelativeOffset<Option<f32>>) {
        if let Some(x) = offset.x {
            self.offset_x = Offset::Relative(x.clamp(0.0, 1.0));
        }
    }

    fn scroll_to(&mut self, offset: AbsoluteOffset<Option<f32>>) {
        let x = offset.x.unwrap_or(0.0);
        let new_offset = Offset::Absolute(x.max(0.0));
        self.scroll_animation = Some(ScrollAnimationState {
            start_time: Instant::now(),
            start: self.offset_x,
            end: new_offset,
            extra: Offset::Absolute(0.),
        });
        self.offset_x = new_offset;
    }

    fn scroll_by(
        &mut self,
        offset: AbsoluteOffset,
        _bounds: Rectangle,
        _content_bounds: Rectangle,
    ) {
        self.scroll_animation = Some(ScrollAnimationState {
            start_time: Instant::now(),
            start: self.offset_x,
            end: self.offset_x,
            extra: Offset::Absolute(offset.x.max(0.0)),
        });
    }
}

impl Default for State {
    fn default() -> Self {
        State {
            offset_x: Offset::Absolute(0.),
            scroll_animation: None,
            scroll_to: None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Offset {
    Absolute(f32),
    Relative(f32),
}

impl Offset {
    fn absolute(self, viewport: f32, content: f32) -> f32 {
        match self {
            Offset::Absolute(absolute) => absolute.min((content - viewport).max(0.0)),
            Offset::Relative(percentage) => ((content - viewport) * percentage).max(0.0),
        }
    }
}

impl<'a, Message> Tabs<'a, Message>
where
    Message: TabMessage + 'static,
{
    pub fn new(
        tabs: impl ExactSizeIterator<Item = Tab<Message>>,
        active: usize,
        _activated: bool,
        _group_focused: bool,
        theme: &'a CompTheme,
    ) -> Self {
        let tabs = tabs.into_iter().enumerate().map(|(i, tab)| {
            let tab = if i == active {
                tab.active()
            } else {
                tab.non_active()
            };
            Element::new(tab.internal(i, theme))
        });

        let prev_button: CompElement<'_, Message> =
            IconButton::new(Source::named("go-previous-symbolic", 16.0), &**theme)
                .on_press(Message::scroll_back())
                .button_type(ButtonIconType::Ghost)
                .size(ButtonIconSize::Small)
                .into();

        let next_button: CompElement<'_, Message> =
            IconButton::new(Source::named("go-next-symbolic", 16.0), &**theme)
                .on_press(Message::scroll_further())
                .button_type(ButtonIconType::Ghost)
                .size(ButtonIconSize::Small)
                .into();

        let mut elements: Vec<CompElement<'_, Message>> = Vec::with_capacity(tabs.len() + 4);

        // Layout: [space, prev_button, ...tabs..., next_button, space]
        elements.push(
            iced_widget::Space::new()
                .width(4.0)
                .height(Length::Fill)
                .into(),
        );
        elements.push(prev_button);
        elements.extend(tabs);
        elements.push(next_button);
        elements.push(
            iced_widget::Space::new()
                .width(4.0)
                .height(Length::Fill)
                .into(),
        );

        Tabs {
            elements,
            id: None,
            width: Length::Fill,
            height: Length::Fill,
            scroll_to: None,
            scroll_duration: theme.motion.scroll,
        }
    }

    pub fn id(mut self, id: impl Into<Id>) -> Self {
        self.id = Some(id.into());
        self
    }

    pub fn width(mut self, width: impl Into<Length>) -> Self {
        self.width = width.into();
        self
    }

    pub fn height(mut self, height: impl Into<Length>) -> Self {
        self.height = height.into();
        self
    }

    pub fn force_visible(mut self, idx: Option<usize>) -> Self {
        self.scroll_to = idx;
        self
    }
}

impl State {
    pub fn offset(
        &self,
        bounds: Rectangle,
        content_bounds: Size,
        scroll_duration: Duration,
    ) -> Vector {
        if let Some(animation) = self.scroll_animation {
            let percentage = {
                let percentage = Instant::now()
                    .duration_since(animation.start_time)
                    .as_millis() as f32
                    / scroll_duration.as_millis() as f32;
                ease(EaseInOutCubic, 0.0, 1.0, percentage.min(1.0))
            };

            Vector::new(
                animation.start.absolute(bounds.width, content_bounds.width)
                    + animation.extra.absolute(bounds.width, content_bounds.width) * percentage
                    + (animation.end.absolute(bounds.width, content_bounds.width)
                        - animation.start.absolute(bounds.width, content_bounds.width))
                        * percentage,
                0.,
            )
        } else {
            Vector::new(
                self.offset_x.absolute(bounds.width, content_bounds.width),
                0.,
            )
        }
    }

    pub fn cleanup_old_animations(&mut self, scroll_duration: Duration) {
        if let Some(animation) = self.scroll_animation.as_ref()
            && Instant::now().duration_since(animation.start_time) > scroll_duration
        {
            self.scroll_animation.take();
        }
    }
}

impl<Message> Widget<Message, iced_core::Theme, iced_tiny_skia::Renderer> for Tabs<'_, Message>
where
    Message: TabMessage,
{
    fn size(&self) -> Size<Length> {
        Size::new(self.width, self.height)
    }

    fn id(&self) -> Option<Id> {
        self.id.clone()
    }

    fn set_id(&mut self, id: Id) {
        self.id = Some(id);
    }

    fn tag(&self) -> tree::Tag {
        tree::Tag::of::<State>()
    }

    fn state(&self) -> tree::State {
        tree::State::Some(Box::<State>::default())
    }

    fn children(&self) -> Vec<Tree> {
        self.elements.iter().map(Tree::new).collect()
    }

    fn diff(&self, tree: &mut Tree) {
        tree.diff_children(&self.elements);
    }

    fn layout(
        &mut self,
        tree: &mut Tree,
        renderer: &iced_tiny_skia::Renderer,
        limits: &Limits,
    ) -> Node {
        let limits = limits.width(self.width).height(self.height);
        let num_elements = self.elements.len();
        // Elements layout: [space(0), prev_btn(1), tabs(2..n-2), next_btn(n-2), space(n-1)]
        let num_tabs = num_elements - 4; // subtract space, prev, next, space

        // Measure tab min sizes
        let child_limits = Limits::new(
            Size::new(0.0, limits.min().height),
            Size::new(f32::INFINITY, limits.max().height),
        )
        .width(Length::Shrink)
        .height(self.height);

        let mut tab_nodes: Vec<Node> = self.elements[2..num_elements - 2]
            .iter_mut()
            .zip(tree.children.iter_mut().skip(2))
            .map(|(tab, tab_tree)| {
                tab.as_widget_mut()
                    .layout(tab_tree, renderer, &child_limits)
            })
            .collect();

        let min_content_width: f32 = tab_nodes.iter().map(|n| n.size().width).sum();
        let size = limits.resolve(
            self.width,
            self.height,
            Size::new(min_content_width, limits.max().height),
        );
        let available_height = size.height;

        let scrolling = min_content_width > size.width;

        if !scrolling {
            // Distribute tabs evenly across available width
            let tab_width = if num_tabs > 0 {
                (size.width / num_tabs as f32).floor()
            } else {
                0.0
            };

            // Ensure active tab gets at least MIN_ACTIVE_TAB_WIDTH
            let use_equal = tab_width >= MIN_ACTIVE_TAB_WIDTH as f32;

            let children = if use_equal {
                // Equal width distribution using flex
                flex::resolve(
                    flex::Axis::Horizontal,
                    renderer,
                    &limits,
                    self.width,
                    self.height,
                    0.into(),
                    0.,
                    Alignment::Center,
                    &mut self.elements[2..num_elements - 2],
                    &mut tree.children[2..num_elements - 2],
                )
                .children()
                .to_vec()
            } else {
                // Manual width distribution with min-width for active tab
                let remaining = size.width - MIN_ACTIVE_TAB_WIDTH as f32;
                let inactive_width = if num_tabs > 1 {
                    remaining / (num_tabs - 1) as f32
                } else {
                    size.width
                };

                let mut offset = 0.0;
                self.elements[2..num_elements - 2]
                    .iter_mut()
                    .zip(tree.children[2..num_elements - 2].iter_mut())
                    .map(|(tab, tab_tree)| {
                        let w = tab.as_widget().size().width;
                        let actual_width = if matches!(w, Length::Fill) {
                            // Active tabs use Fill
                            MIN_ACTIVE_TAB_WIDTH as f32
                        } else {
                            inactive_width
                        };
                        let tab_limits = Limits::new(
                            Size::new(actual_width, available_height),
                            Size::new(actual_width, available_height),
                        );
                        let mut node = tab.as_widget_mut().layout(tab_tree, renderer, &tab_limits);
                        node = node.move_to(Point::new(offset, 0.));
                        offset += node.bounds().width;
                        node
                    })
                    .collect()
            };

            // Placeholder nodes for hidden scroll buttons
            Node::with_children(
                size,
                vec![
                    Node::new(Size::new(0., 0.)),
                    Node::with_children(Size::new(0., 0.), vec![Node::new(Size::new(0., 0.))]),
                ]
                .into_iter()
                .chain(children)
                .chain(vec![
                    Node::with_children(Size::new(0., 0.), vec![Node::new(Size::new(0., 0.))]),
                    Node::new(Size::new(0., 0.)),
                ])
                .collect(),
            )
        } else {
            // Scrolling mode: layout tabs at natural width, add scroll buttons
            let scroll_btn_area = 30.0;
            let mut offset = scroll_btn_area;

            for node in &mut tab_nodes {
                *node = node.clone().move_to(Point::new(offset, 0.));
                offset += node.bounds().width;
            }

            let last_position = Point::new(size.width - scroll_btn_area - 4., 0.);

            Node::with_children(
                size,
                vec![Node::new(Size::new(4., size.height)), {
                    let mut node = Node::with_children(
                        Size::new(16., 16.),
                        vec![Node::new(Size::new(16., 16.))],
                    );
                    node = node.move_to(Point::new(9., (size.height - 16.) / 2.));
                    node
                }]
                .into_iter()
                .chain(tab_nodes)
                .chain(vec![
                    {
                        let mut node = Node::with_children(
                            Size::new(16., 16.),
                            vec![Node::new(Size::new(16., 16.))],
                        );
                        node =
                            node.move_to(last_position + Vector::new(9., (size.height - 16.) / 2.));
                        node
                    },
                    {
                        let mut node = Node::new(Size::new(4., size.height));
                        node = node.move_to(last_position + Vector::new(30., 0.));
                        node
                    },
                ])
                .collect(),
            )
        }
    }

    fn draw(
        &self,
        tree: &Tree,
        renderer: &mut iced_tiny_skia::Renderer,
        theme: &iced_core::Theme,
        style: &renderer::Style,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        viewport: &Rectangle,
    ) {
        let state = tree.state.downcast_ref::<State>();
        let num_elements = self.elements.len();

        let mut bounds = layout.bounds();
        let content_bounds =
            layout
                .children()
                .skip(2)
                .take(num_elements - 4)
                .fold(Size::new(0., 0.), |a, b| Size {
                    width: a.width + b.bounds().width,
                    height: b.bounds().height,
                });

        let scrolling = content_bounds.width.floor() > bounds.width;

        if scrolling {
            bounds.width -= 64.;
            bounds.x += 30.;

            // Draw prev button
            self.elements[1].as_widget().draw(
                &tree.children[1],
                renderer,
                theme,
                style,
                layout.children().nth(1).unwrap(),
                cursor,
                viewport,
            );
        }

        let offset = state.offset(bounds, content_bounds, self.scroll_duration);

        // Draw tabs in a clipped layer with scroll offset
        renderer.with_layer(bounds, |renderer| {
            renderer.with_translation(Vector::new(-offset.x, -offset.y), |renderer| {
                for ((tab, wstate), tab_layout) in self.elements[2..num_elements - 2]
                    .iter()
                    .zip(tree.children.iter().skip(2))
                    .zip(layout.children().skip(2))
                {
                    let cursor = match cursor {
                        mouse::Cursor::Available(point) => mouse::Cursor::Available(point + offset),
                        mouse::Cursor::Levitating(point) => {
                            mouse::Cursor::Levitating(point + offset)
                        }
                        mouse::Cursor::Unavailable => mouse::Cursor::Unavailable,
                    };

                    tab.as_widget()
                        .draw(wstate, renderer, theme, style, tab_layout, cursor, &bounds);
                }
            });
        });

        if scrolling {
            // Draw next button
            let next_idx = num_elements - 2;
            self.elements[next_idx].as_widget().draw(
                &tree.children[next_idx],
                renderer,
                theme,
                style,
                layout.children().nth(next_idx).unwrap(),
                cursor,
                viewport,
            );
        }
    }

    fn operate(
        &mut self,
        tree: &mut Tree,
        layout: Layout<'_>,
        renderer: &iced_tiny_skia::Renderer,
        operation: &mut dyn Operation<()>,
    ) {
        let num_elements = self.elements.len();
        let state = tree.state.downcast_mut::<State>();
        let bounds = layout.bounds();
        let content_layout = layout.children().next().unwrap();
        let content_bounds = content_layout.bounds();

        state.cleanup_old_animations(self.scroll_duration);

        operation.scrollable(
            self.id.as_ref(),
            bounds,
            content_bounds,
            Vector { x: 0.0, y: 0.0 },
            state,
        );

        operation.container(self.id.as_ref(), bounds);

        self.elements[2..num_elements - 2]
            .iter_mut()
            .zip(tree.children.iter_mut().skip(2))
            .zip(layout.children().skip(2))
            .for_each(|((child, state), layout)| {
                child
                    .as_widget_mut()
                    .operate(state, layout, renderer, operation);
            });
    }

    fn update(
        &mut self,
        tree: &mut Tree,
        event: &event::Event,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        renderer: &iced_tiny_skia::Renderer,
        shell: &mut Shell<'_, Message>,
        viewport: &Rectangle,
    ) {
        let state = tree.state.downcast_mut::<State>();
        state.cleanup_old_animations(self.scroll_duration);
        let num_elements = self.elements.len();

        let mut bounds = layout.bounds();
        let content_bounds =
            layout
                .children()
                .skip(2)
                .take(num_elements - 4)
                .fold(Size::new(0., 0.), |a, b| Size {
                    width: a.width + b.bounds().width,
                    height: b.bounds().height,
                });
        let scrolling = content_bounds.width.floor() > bounds.width;

        if scrolling {
            bounds.x += 30.;
            bounds.width -= 64.;
        }
        let offset = state.offset(bounds, content_bounds, self.scroll_duration);

        // Handle scroll_to
        if let Some(idx) = self.scroll_to {
            state.scroll_to = Some(idx);
        }
        if let Some(idx) = state.scroll_to.take() {
            if scrolling {
                let tab_bounds = layout.children().nth(idx + 2).unwrap().bounds();
                let left_offset = tab_bounds.x - layout.bounds().x - 30.;
                let right_offset = left_offset + tab_bounds.width + 4.;
                let scroll_width = bounds.width;
                let current_start = offset.x;
                let current_end = current_start + scroll_width;

                if (left_offset - current_start).is_sign_negative()
                    || (current_end - right_offset).is_sign_negative()
                {
                    let new_offset = if (left_offset - current_start).abs()
                        < (right_offset - current_end).abs()
                    {
                        AbsoluteOffset {
                            x: left_offset,
                            y: 0.,
                        }
                    } else {
                        AbsoluteOffset {
                            x: right_offset - scroll_width,
                            y: 0.,
                        }
                    };

                    state.scroll_animation = Some(ScrollAnimationState {
                        start_time: Instant::now(),
                        start: Offset::Absolute(offset.x),
                        end: Offset::Absolute(new_offset.x),
                        extra: Offset::Absolute(0.),
                    });
                    state.offset_x = Offset::Absolute(new_offset.x);
                }
            }
            shell.publish(Message::scrolled());
        }

        // Forward events to children
        let mut messages = Vec::new();
        let mut internal_shell = Shell::new(&mut messages);

        if scrolling && cursor.position().is_some_and(|pos| pos.x < bounds.x) {
            // Forward to prev button
            self.elements[0..2]
                .iter_mut()
                .zip(&mut tree.children)
                .zip(layout.children())
                .for_each(|((child, state), layout)| {
                    child.as_widget_mut().update(
                        state,
                        event,
                        layout,
                        cursor,
                        renderer,
                        &mut internal_shell,
                        viewport,
                    );
                });
        } else if scrolling
            && cursor
                .position()
                .is_some_and(|pos| pos.x >= bounds.x + bounds.width)
        {
            // Forward to next button
            self.elements[num_elements - 2..num_elements]
                .iter_mut()
                .zip(tree.children.iter_mut().skip(num_elements - 2))
                .zip(layout.children().skip(num_elements - 2))
                .for_each(|((child, state), layout)| {
                    child.as_widget_mut().update(
                        state,
                        event,
                        layout,
                        cursor,
                        renderer,
                        &mut internal_shell,
                        viewport,
                    );
                });
        } else {
            // Forward to tab elements with scroll offset applied to cursor
            self.elements[2..num_elements - 2]
                .iter_mut()
                .zip(tree.children.iter_mut().skip(2))
                .zip(layout.children().skip(2))
                .for_each(|((child, state), layout)| {
                    let cursor = match cursor {
                        mouse::Cursor::Available(point) => mouse::Cursor::Available(point + offset),
                        mouse::Cursor::Levitating(point) => {
                            mouse::Cursor::Levitating(point + offset)
                        }
                        mouse::Cursor::Unavailable => mouse::Cursor::Unavailable,
                    };

                    child.as_widget_mut().update(
                        state,
                        event,
                        layout,
                        cursor,
                        renderer,
                        &mut internal_shell,
                        viewport,
                    );
                });
        }

        if internal_shell.is_event_captured() {
            shell.capture_event();
        }

        for mut message in messages {
            if let Some(offset) = message.populate_scroll(AbsoluteOffset {
                x: state.offset_x.absolute(bounds.width, content_bounds.width),
                y: 0.,
            }) {
                state.scroll_to(offset.into());
                continue;
            }
            shell.publish(message);
        }
    }

    fn mouse_interaction(
        &self,
        tree: &Tree,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        viewport: &Rectangle,
        renderer: &iced_tiny_skia::Renderer,
    ) -> mouse::Interaction {
        let state = tree.state.downcast_ref::<State>();
        let num_elements = self.elements.len();

        let mut bounds = layout.bounds();
        let content_bounds =
            layout
                .children()
                .skip(2)
                .take(num_elements - 4)
                .fold(Size::new(0., 0.), |a, b| Size {
                    width: a.width + b.bounds().width,
                    height: b.bounds().height,
                });
        let scrolling = content_bounds.width.floor() > bounds.width;

        if scrolling {
            bounds.width -= 64.;
            bounds.x += 30.;
        }
        let offset = state.offset(bounds, content_bounds, self.scroll_duration);

        self.elements[2..num_elements - 2]
            .iter()
            .zip(tree.children.iter().skip(2))
            .zip(layout.children().skip(2))
            .map(|((child, state), layout)| {
                let cursor = match cursor {
                    mouse::Cursor::Available(point) => mouse::Cursor::Available(point + offset),
                    mouse::Cursor::Levitating(point) => mouse::Cursor::Levitating(point + offset),
                    mouse::Cursor::Unavailable => mouse::Cursor::Unavailable,
                };
                child
                    .as_widget()
                    .mouse_interaction(state, layout, cursor, viewport, renderer)
            })
            .max()
            .unwrap_or_default()
    }

    fn overlay<'b>(
        &'b mut self,
        tree: &'b mut Tree,
        layout: Layout<'b>,
        renderer: &iced_tiny_skia::Renderer,
        _viewport: &Rectangle,
        translation: Vector,
    ) -> Option<overlay::Element<'b, Message, iced_core::Theme, iced_tiny_skia::Renderer>> {
        overlay::from_children(
            &mut self.elements,
            tree,
            layout,
            renderer,
            _viewport,
            translation,
        )
    }
}
