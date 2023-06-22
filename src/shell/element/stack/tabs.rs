use super::tab::{Tab, TabBackgroundTheme, TabMessage, TabRuleTheme, MIN_ACTIVE_TAB_WIDTH};
use apply::Apply;
use cosmic::{
    font::Font,
    iced::{id::Id, widget, Element},
    iced_core::{
        event,
        layout::{Layout, Limits, Node},
        mouse, overlay, renderer,
        widget::{
            operation::{
                scrollable::{AbsoluteOffset, RelativeOffset},
                Operation, OperationOutputWrapper, Scrollable,
            },
            text::StyleSheet as TextStyleSheet,
            tree::{self, Tree},
            Widget,
        },
        Background, Clipboard, Color, Length, Point, Rectangle, Shell, Size, Vector,
    },
    iced_style::{
        button::StyleSheet as ButtonStyleSheet, container::StyleSheet as ContainerStyleSheet,
        rule::StyleSheet as RuleStyleSheet,
    },
    iced_widget::container::draw_background,
    theme,
    widget::{icon, Icon},
};

pub struct Tabs<'a, Message, Renderer>
where
    Renderer: cosmic::iced_core::Renderer,
    Renderer::Theme: RuleStyleSheet,
{
    elements: Vec<Element<'a, Message, Renderer>>,
    id: Option<Id>,
    height: Length,
    width: Length,
    group_focused: bool,
    scroll_to: Option<usize>,
}

/// The local state of [`Tabs`].
#[derive(Debug, Clone, Copy)]
pub struct State {
    offset_x: Offset,
    scroll_to: Option<usize>,
}

impl Scrollable for State {
    fn snap_to(&mut self, offset: RelativeOffset) {
        self.offset_x = Offset::Relative(offset.x.clamp(0.0, 1.0));
    }

    fn scroll_to(&mut self, offset: AbsoluteOffset) {
        self.offset_x = Offset::Absolute(offset.x.max(0.0));
    }
}

impl Default for State {
    fn default() -> Self {
        State {
            offset_x: Offset::Absolute(0.),
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

impl<'a, Message, Renderer> Tabs<'a, Message, Renderer>
where
    Renderer: cosmic::iced_core::Renderer + 'a,
    Renderer: cosmic::iced_core::text::Renderer<Font = Font>,
    Renderer::Theme: ButtonStyleSheet<Style = theme::Button>,
    Renderer::Theme: ContainerStyleSheet<Style = theme::Container>,
    Renderer::Theme: RuleStyleSheet<Style = theme::Rule>,
    Renderer::Theme: TextStyleSheet,
    Message: TabMessage + 'a,
    widget::Button<'a, Message, Renderer>: Into<Element<'a, Message, Renderer>>,
    widget::Container<'a, Message, Renderer>: Into<Element<'a, Message, Renderer>>,
    Icon<'a>: Into<Element<'a, Message, Renderer>>,
{
    pub fn new(
        tabs: impl IntoIterator<Item = Tab<'a, Message>>,
        active: usize,
        activated: bool,
        group_focused: bool,
    ) -> Self {
        let mut tabs = tabs
            .into_iter()
            .enumerate()
            .map(|(i, tab)| {
                let rule = if activated {
                    TabRuleTheme::ActiveActivated
                } else {
                    TabRuleTheme::ActiveDeactivated
                };

                let tab = if i == active {
                    tab.rule_style(rule)
                        .background_style(if activated {
                            TabBackgroundTheme::ActiveActivated
                        } else {
                            TabBackgroundTheme::ActiveDeactivated
                        })
                        .font(cosmic::font::FONT_SEMIBOLD)
                        .active()
                } else if i.checked_sub(1) == Some(active) {
                    tab.rule_style(rule).non_active()
                } else {
                    tab.non_active()
                };

                Element::new(tab.internal(i))
            })
            .collect::<Vec<_>>();

        tabs.push(
            widget::vertical_rule(4)
                .style(if tabs.len() - 1 == active {
                    if activated {
                        TabRuleTheme::ActiveActivated
                    } else {
                        TabRuleTheme::ActiveDeactivated
                    }
                } else {
                    TabRuleTheme::Default
                })
                .into(),
        );

        Tabs {
            elements: vec![
                widget::vertical_rule(4)
                    .style(if group_focused {
                        TabRuleTheme::ActiveActivated
                    } else {
                        TabRuleTheme::Default
                    })
                    .into(),
                icon("go-previous-symbolic", 16)
                    .force_svg(true)
                    .style(theme::Svg::Symbolic)
                    .apply(widget::button)
                    .style(theme::Button::Text)
                    .on_press(Message::scroll_back())
                    .into(),
            ]
            .into_iter()
            .chain(tabs)
            .chain(vec![
                icon("go-next-symbolic", 16)
                    .force_svg(true)
                    .style(theme::Svg::Symbolic)
                    .apply(widget::button)
                    .style(theme::Button::Text)
                    .on_press(Message::scroll_further())
                    .into(),
                widget::vertical_rule(4)
                    .style(if group_focused {
                        TabRuleTheme::ActiveActivated
                    } else {
                        TabRuleTheme::Default
                    })
                    .into(),
            ])
            .collect(),
            id: None,
            width: Length::Fill,
            height: Length::Shrink,
            group_focused,
            scroll_to: None,
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
    pub fn offset(&self, bounds: Rectangle, content_bounds: Size) -> Vector {
        Vector::new(
            self.offset_x.absolute(bounds.width, content_bounds.width),
            0.,
        )
    }
}

impl<'a, Message, Renderer> Widget<Message, Renderer> for Tabs<'a, Message, Renderer>
where
    Renderer: cosmic::iced_core::Renderer,
    Renderer::Theme: ContainerStyleSheet<Style = theme::Container> + RuleStyleSheet,
    Message: TabMessage,
{
    fn width(&self) -> Length {
        self.width
    }
    fn height(&self) -> Length {
        self.height
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
        tree::State::Some(Box::new(State::default()))
    }

    fn children(&self) -> Vec<Tree> {
        self.elements.iter().map(|elem| Tree::new(elem)).collect()
    }

    fn diff(&mut self, tree: &mut Tree) {
        tree.diff_children(&mut self.elements)
    }

    fn layout(&self, renderer: &Renderer, limits: &Limits) -> Node {
        let limits = limits.width(self.width).height(self.height);

        // calculate the smallest possible size
        let child_limits = Limits::new(
            Size::new(0.0, limits.min().height),
            Size::new(f32::INFINITY, limits.max().height),
        )
        .width(Length::Shrink)
        .height(Length::Shrink);

        let mut nodes = self.elements[2..self.elements.len() - 2]
            .iter()
            .map(|tab| tab.as_widget().layout(renderer, &child_limits))
            .collect::<Vec<_>>();

        // sum up
        let min_size = nodes
            .iter()
            .map(|node| node.size())
            .fold(Size::new(0., 0.), |a, b| Size {
                width: a.width + b.width,
                height: a.height.max(b.height),
            });
        let size = limits.resolve(min_size);

        if min_size.width <= size.width {
            // we don't need to scroll

            // can we make every tab equal weight and keep the active large enough?
            let children = if (size.width / (self.elements.len() as f32 - 5.)).ceil() as i32
                >= MIN_ACTIVE_TAB_WIDTH
            {
                // just use a flex layout
                cosmic::iced_core::layout::flex::resolve(
                    cosmic::iced_core::layout::flex::Axis::Horizontal,
                    renderer,
                    &limits,
                    0.into(),
                    0.,
                    cosmic::iced::Alignment::Center,
                    &self.elements[2..self.elements.len() - 2],
                )
                .children()
                .to_vec()
            } else {
                // otherwise we need a more manual approach
                let min_width = (size.width - MIN_ACTIVE_TAB_WIDTH as f32 - 4.)
                    / (self.elements.len() as f32 - 6.);
                let mut offset = 0.;

                let mut nodes = self.elements[2..self.elements.len() - 3]
                    .iter()
                    .map(|tab| {
                        let child_limits = Limits::new(
                            Size::new(min_width, limits.min().height),
                            Size::new(f32::INFINITY, limits.max().height),
                        )
                        .width(Length::Shrink)
                        .height(Length::Shrink);

                        let mut node = tab.as_widget().layout(renderer, &child_limits);
                        node.move_to(Point::new(offset, 0.));
                        offset += node.bounds().width;
                        node
                    })
                    .collect::<Vec<_>>();
                nodes.push({
                    let mut node = Node::new(Size::new(4., limits.max().height));
                    node.move_to(Point::new(offset, 0.));
                    node
                });
                nodes
            };

            // and add placeholder nodes for the not rendered scroll-buttons/rules
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
                .collect::<Vec<_>>(),
            )
        } else {
            // we scroll, so use the computed min size, but add scroll buttons.
            let mut offset = 30.;
            for node in &mut nodes {
                node.move_to(Point::new(offset, 0.));
                offset += node.bounds().width;
            }
            let last_position = Point::new(size.width - 34., 0.);
            nodes.remove(nodes.len() - 1);

            Node::with_children(
                size,
                vec![Node::new(Size::new(4., size.height)), {
                    let mut node = Node::with_children(
                        Size::new(16., 16.),
                        vec![Node::new(Size::new(16., 16.))],
                    );
                    node.move_to(Point::new(9., (size.height - 16.) / 2.));
                    node
                }]
                .into_iter()
                .chain(nodes)
                .chain(vec![
                    {
                        let mut node = Node::new(Size::new(4., size.height));
                        node.move_to(last_position);
                        node
                    },
                    {
                        let mut node = Node::with_children(
                            Size::new(16., 16.),
                            vec![Node::new(Size::new(16., 16.))],
                        );
                        node.move_to(last_position + Vector::new(9., (size.height - 16.) / 2.));
                        node
                    },
                    {
                        let mut node = Node::new(Size::new(4., size.height));
                        node.move_to(last_position + Vector::new(30., 0.));
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
        renderer: &mut Renderer,
        theme: &<Renderer as cosmic::iced_core::Renderer>::Theme,
        style: &renderer::Style,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
    ) {
        let state = tree.state.downcast_ref::<State>();

        let mut bounds = layout.bounds();
        let content_bounds = layout
            .children()
            .skip(2)
            .take(self.elements.len() - 5)
            .fold(Size::new(0., 0.), |a, b| Size {
                width: a.width + b.bounds().width,
                height: b.bounds().height,
            });

        let background_style = ContainerStyleSheet::appearance(
            theme,
            &theme::Container::custom(|theme| widget::container::Appearance {
                text_color: None,
                background: Some(Background::Color(Color::from(
                    theme.cosmic().palette.neutral_3,
                ))),
                border_radius: 0.0.into(),
                border_width: 0.0,
                border_color: Color::TRANSPARENT,
            }),
        );
        draw_background(renderer, &background_style, bounds);

        let scrolling = content_bounds.width.floor() > bounds.width;
        if scrolling {
            bounds.width -= 64.;
            bounds.x += 30.;
        }
        let offset = state.offset(bounds, content_bounds);
        let offset_viewport = Rectangle {
            x: bounds.x + offset.x,
            y: bounds.y + offset.y,
            ..bounds
        };

        if scrolling {
            // we have scroll buttons
            for ((scroll, state), layout) in self
                .elements
                .iter()
                .take(2)
                .zip(&tree.children)
                .zip(layout.children())
            {
                scroll.as_widget().draw(
                    state,
                    renderer,
                    theme,
                    style,
                    layout,
                    cursor_position,
                    viewport,
                );
            }
        }

        renderer.with_layer(bounds, |renderer| {
            renderer.with_translation(Vector::new(-offset.x, -offset.y), |renderer| {
                for ((tab, state), layout) in self.elements[2..self.elements.len() - 3]
                    .iter()
                    .zip(tree.children.iter().skip(2))
                    .zip(layout.children().skip(2))
                {
                    tab.as_widget().draw(
                        state,
                        renderer,
                        theme,
                        style,
                        layout,
                        cursor_position + offset,
                        &offset_viewport,
                    );
                }
            });
        });
        self.elements[self.elements.len() - 3].as_widget().draw(
            &tree.children[self.elements.len() - 3],
            renderer,
            theme,
            style,
            layout.children().nth(self.elements.len() - 3).unwrap(),
            cursor_position,
            viewport,
        );

        if !scrolling && self.group_focused {
            // HACK, overdraw our rule at the edges
            self.elements[0].as_widget().draw(
                &tree.children[2].children[0],
                renderer,
                theme,
                style,
                layout.children().nth(2).unwrap().children().nth(0).unwrap(),
                cursor_position,
                viewport,
            );
            self.elements[self.elements.len() - 1].as_widget().draw(
                &tree.children[self.elements.len() - 3],
                renderer,
                theme,
                style,
                layout.children().nth(self.elements.len() - 3).unwrap(),
                cursor_position,
                viewport,
            );
        }

        if scrolling {
            // we have scroll buttons
            for ((scroll, state), layout) in self.elements
                [self.elements.len() - 2..self.elements.len()]
                .iter()
                .zip(tree.children.iter().skip(self.elements.len() - 2))
                .zip(layout.children().skip(self.elements.len() - 2))
            {
                scroll.as_widget().draw(
                    state,
                    renderer,
                    theme,
                    style,
                    layout,
                    cursor_position,
                    viewport,
                );
            }
        }
    }

    fn operate(
        &self,
        tree: &mut Tree,
        layout: Layout<'_>,
        renderer: &Renderer,
        operation: &mut dyn Operation<OperationOutputWrapper<Message>>,
    ) {
        let state = tree.state.downcast_mut::<State>();

        operation.scrollable(state, self.id.as_ref());

        operation.container(self.id.as_ref(), &mut |operation| {
            self.elements[2..self.elements.len() - 3]
                .iter()
                .zip(tree.children.iter_mut().skip(2))
                .zip(layout.children().skip(2))
                .for_each(|((child, state), layout)| {
                    child
                        .as_widget()
                        .operate(state, layout, renderer, operation);
                })
        });
    }

    fn on_event(
        &mut self,
        tree: &mut Tree,
        event: event::Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &Renderer,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Message>,
    ) -> event::Status {
        let state = tree.state.downcast_mut::<State>();

        let mut bounds = layout.bounds();
        let content_bounds = layout.children().fold(Size::new(0., 0.), |a, b| Size {
            width: a.width + b.bounds().width,
            height: b.bounds().height,
        });
        let scrolling = content_bounds.width.floor() > bounds.width;

        if scrolling {
            bounds.x += 30.;
            bounds.width -= 64.;
        }
        let offset = state.offset(bounds, content_bounds);

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

                assert!((right_offset - left_offset) <= (current_end - current_start));
                if (left_offset - current_start).is_sign_negative()
                    || (current_end - right_offset).is_sign_negative()
                {
                    let offset = if (left_offset - current_start).abs()
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

                    state.offset_x = Offset::Absolute(offset.x);
                }
            }
            shell.publish(Message::scrolled());
        }

        let mut messages = Vec::new();
        let mut internal_shell = Shell::new(&mut messages);

        let len = self.elements.len();
        let result = if scrolling && cursor_position.x < bounds.x {
            self.elements[0..2]
                .iter_mut()
                .zip(&mut tree.children)
                .zip(layout.children())
                .map(|((child, state), layout)| {
                    child.as_widget_mut().on_event(
                        state,
                        event.clone(),
                        layout,
                        cursor_position,
                        renderer,
                        clipboard,
                        &mut internal_shell,
                    )
                })
                .fold(event::Status::Ignored, event::Status::merge)
        } else if scrolling && cursor_position.x >= bounds.x + bounds.width {
            self.elements[len - 3..len]
                .iter_mut()
                .zip(tree.children.iter_mut().skip(len - 3))
                .zip(layout.children().skip(len - 3))
                .map(|((child, state), layout)| {
                    child.as_widget_mut().on_event(
                        state,
                        event.clone(),
                        layout,
                        cursor_position,
                        renderer,
                        clipboard,
                        &mut internal_shell,
                    )
                })
                .fold(event::Status::Ignored, event::Status::merge)
        } else {
            self.elements[2..len - 3]
                .iter_mut()
                .zip(tree.children.iter_mut().skip(2))
                .zip(layout.children().skip(2))
                .map(|((child, state), layout)| {
                    child.as_widget_mut().on_event(
                        state,
                        event.clone(),
                        layout,
                        cursor_position + offset,
                        renderer,
                        clipboard,
                        &mut internal_shell,
                    )
                })
                .fold(event::Status::Ignored, event::Status::merge)
        };

        std::mem::drop(internal_shell);
        for mut message in messages {
            if let Some(offset) = message.populate_scroll(AbsoluteOffset {
                x: state.offset_x.absolute(bounds.width, content_bounds.width),
                y: 0.,
            }) {
                state.scroll_to(offset);
                continue;
            }

            shell.publish(message);
        }

        result
    }

    fn mouse_interaction(
        &self,
        tree: &Tree,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
        renderer: &Renderer,
    ) -> mouse::Interaction {
        let state = tree.state.downcast_ref::<State>();

        let mut bounds = layout.bounds();
        let content_bounds = layout.children().fold(Size::new(0., 0.), |a, b| Size {
            width: a.width + b.bounds().width,
            height: b.bounds().height,
        });
        let scrolling = content_bounds.width.floor() > bounds.width;

        if scrolling {
            bounds.width -= 64.;
            bounds.x += 30.;
        }
        let offset = state.offset(bounds, content_bounds);
        let offset_viewport = &Rectangle {
            y: bounds.y + offset.y,
            x: bounds.x + offset.x,
            ..bounds
        };

        if scrolling && cursor_position.x < bounds.x {
            self.elements[0..2]
                .iter()
                .zip(&tree.children)
                .zip(layout.children())
                .map(|((child, state), layout)| {
                    child.as_widget().mouse_interaction(
                        state,
                        layout,
                        cursor_position,
                        viewport,
                        renderer,
                    )
                })
                .max()
        } else if scrolling && cursor_position.x >= bounds.x + bounds.width {
            self.elements[self.elements.len() - 3..self.elements.len()]
                .iter()
                .zip(tree.children.iter().skip(self.elements.len() - 3))
                .zip(layout.children().skip(self.elements.len() - 3))
                .map(|((child, state), layout)| {
                    child.as_widget().mouse_interaction(
                        state,
                        layout,
                        cursor_position,
                        viewport,
                        renderer,
                    )
                })
                .max()
        } else {
            self.elements[2..self.elements.len() - 3]
                .iter()
                .zip(tree.children.iter().skip(2))
                .zip(layout.children().skip(2))
                .map(|((child, state), layout)| {
                    child.as_widget().mouse_interaction(
                        state,
                        layout,
                        cursor_position + offset,
                        offset_viewport,
                        renderer,
                    )
                })
                .max()
        }
        .unwrap_or_default()
    }

    fn overlay<'b>(
        &'b mut self,
        tree: &'b mut Tree,
        layout: Layout<'_>,
        renderer: &Renderer,
    ) -> Option<overlay::Element<'b, Message, Renderer>> {
        overlay::from_children(&mut self.elements, tree, layout, renderer)
    }
}
