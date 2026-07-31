use std::sync::{Arc, Mutex};

use crate::{
    backend::render::element::AsGlowRenderer,
    comp_theme::CompTheme,
    config::Config,
    fl,
    shell::grabs::ResizeEdge,
    utils::{
        apply::Apply,
        iced::{CompElement, IcedElement, IcedRenderElement, Program},
        xdg_icon::named_icon,
    },
};

use calloop::LoopHandle;
use cosmic_settings_config::shortcuts::action::{Action, ResizeDirection};
use iced_core::{Alignment, Background, Border, Color, Element, Length};
use iced_widget::container::Container;
use iced_widget::{Space, container, row};
use icetron_p::prelude::styled_text;
use smithay::{
    backend::renderer::ImportMem,
    desktop::space::SpaceElement,
    output::Output,
    utils::{Logical, Physical, Point, Rectangle, Scale, Size},
};

/// Helper to create a container with concrete types (avoids type inference issues).
fn comp_container<'a, Message: 'a>(
    content: impl Into<Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer>>,
) -> Container<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer> {
    Container::new(content)
}

/// Corner radius of the accent badges. The arrow badges are [`ARROW_SIZE`]
/// square, so this makes them circular and the shortcut badge a pill.
const BADGE_RADIUS: f32 = 18.0;

const ARROW_SIZE: i32 = 36;

#[derive(Debug, Clone)]
pub struct ResizeIndicator {
    edges: ResizeEdge,
    direction: Arc<Mutex<ResizeDirection>>,
    size: Size<i32, Logical>,

    center_elem: IcedElement<ResizeIndicatorInternal>,
    left_elem: IcedElement<ResizeIndicatorArrow>,
    top_elem: IcedElement<ResizeIndicatorArrow>,
    right_elem: IcedElement<ResizeIndicatorArrow>,
    down_elem: IcedElement<ResizeIndicatorArrow>,
}

impl ResizeIndicator {
    pub fn new(
        direction: ResizeDirection,
        config: &Config,
        evlh: LoopHandle<'static, crate::state::State>,
        theme: CompTheme,
    ) -> ResizeIndicator {
        // MERGE: upstream sets `theme.transparent = theme.cosmic().frosted_system_interface`
        // here. `CompTheme` has no frosted/transparency flag, so the accent badges stay
        // opaque in this fork.
        let direction = Arc::new(Mutex::new(direction));

        ResizeIndicator {
            edges: ResizeEdge::all(),
            direction: direction.clone(),
            size: Size::default(),
            center_elem: IcedElement::new(
                ResizeIndicatorInternal {
                    shortcut1: config
                        .shortcuts
                        .iter()
                        .find_map(|(pattern, action)| {
                            (*action == Action::Resizing(ResizeDirection::Outwards))
                                .then_some(pattern)
                        })
                        .map(|pattern| format!("{}: ", pattern.to_string()))
                        .unwrap_or_else(|| crate::fl!("unknown-keybinding")),
                    shortcut2: config
                        .shortcuts
                        .iter()
                        .find_map(|(pattern, action)| {
                            (*action == Action::Resizing(ResizeDirection::Inwards))
                                .then_some(pattern)
                        })
                        .map(|pattern| format!("{}: ", pattern.to_string()))
                        .unwrap_or_else(|| crate::fl!("unknown-keybinding")),
                },
                Size::from((1, 1)),
                evlh.clone(),
                theme.clone(),
            ),
            left_elem: IcedElement::new(
                ResizeIndicatorArrow {
                    direction: direction.clone(),
                    icon_outwards: "go-previous-symbolic",
                    icon_inwards: "go-next-symbolic",
                },
                Size::from((ARROW_SIZE, ARROW_SIZE)),
                evlh.clone(),
                theme.clone(),
            ),
            top_elem: IcedElement::new(
                ResizeIndicatorArrow {
                    direction: direction.clone(),
                    icon_outwards: "go-up-symbolic",
                    icon_inwards: "go-down-symbolic",
                },
                Size::from((ARROW_SIZE, ARROW_SIZE)),
                evlh.clone(),
                theme.clone(),
            ),
            right_elem: IcedElement::new(
                ResizeIndicatorArrow {
                    direction: direction.clone(),
                    icon_outwards: "go-next-symbolic",
                    icon_inwards: "go-previous-symbolic",
                },
                Size::from((ARROW_SIZE, ARROW_SIZE)),
                evlh.clone(),
                theme.clone(),
            ),
            down_elem: IcedElement::new(
                ResizeIndicatorArrow {
                    direction,
                    icon_outwards: "go-down-symbolic",
                    icon_inwards: "go-up-symbolic",
                },
                Size::from((ARROW_SIZE, ARROW_SIZE)),
                evlh.clone(),
                theme.clone(),
            ),
        }
    }

    pub fn resize(&mut self, size: Size<i32, Logical>) {
        let minimum = self.center_elem.minimum_size();
        let new_size = Size::<i32, Logical>::new(size.w.min(minimum.w), size.h.min(minimum.h));
        self.center_elem.resize(new_size);
        self.size = size;
    }

    pub fn push_render_elements<R>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
        alpha: f32,
        push_above: &mut dyn FnMut(IcedRenderElement<R>),
        mut push_below: Option<&mut dyn FnMut(IcedRenderElement<R>)>,
    ) where
        R: AsGlowRenderer + ImportMem,
        R::TextureId: Send + Clone + 'static,
    {
        let elem_size = self.center_elem.current_size();
        let center_location = Point::new(
            self.size.w.saturating_sub(elem_size.w) / 2,
            self.size.h.saturating_sub(elem_size.h) / 2,
        );
        let left_location = Point::new(0, self.size.h.saturating_sub(ARROW_SIZE) / 2);
        let top_location = Point::new(self.size.w.saturating_sub(ARROW_SIZE) / 2, 0);
        let right_location = Point::new(
            self.size.w.saturating_sub(ARROW_SIZE),
            self.size.h.saturating_sub(ARROW_SIZE) / 2,
        );
        let down_location = Point::new(
            self.size.w.saturating_sub(ARROW_SIZE) / 2,
            self.size.h.saturating_sub(ARROW_SIZE),
        );
        // MERGE: upstream derives this from `theme.cosmic().radius_s()`. This fork styles
        // the badges with `BADGE_RADIUS`, so the buffer rounding must match that instead.
        let radii = [BADGE_RADIUS.round() as u8; 4];

        self.center_elem.push_render_elements(
            renderer,
            location + center_location.to_physical_precise_round(scale),
            scale,
            alpha,
            radii,
            push_above,
            if let Some(push_below) = push_below.as_mut() {
                Some(&mut **push_below)
            } else {
                None
            },
        );
        let mut render = move |elem: &IcedElement<ResizeIndicatorArrow>,
                               loc: Point<i32, Logical>| {
            elem.push_render_elements(
                renderer,
                location + loc.to_physical_precise_round(scale),
                scale,
                alpha,
                radii,
                push_above,
                if let Some(push_below) = push_below.as_mut() {
                    Some(&mut **push_below)
                } else {
                    None
                },
            );
        };

        if self.edges.contains(ResizeEdge::LEFT) {
            render(&self.left_elem, left_location);
        }
        if self.edges.contains(ResizeEdge::TOP) {
            render(&self.top_elem, top_location);
        }
        if self.edges.contains(ResizeEdge::RIGHT) {
            render(&self.right_elem, right_location);
        }
        if self.edges.contains(ResizeEdge::BOTTOM) {
            render(&self.down_elem, down_location);
        }
    }

    pub fn set_edges(&mut self, edges: ResizeEdge) {
        self.edges = edges;
    }

    pub fn set_direction(&self, direction: ResizeDirection) {
        let mut dir_ref = self.direction.lock().unwrap();
        if *dir_ref != direction {
            *dir_ref = direction;
            self.left_elem.force_redraw();
            self.top_elem.force_redraw();
            self.right_elem.force_redraw();
            self.down_elem.force_redraw();
        }
    }

    /// Fork feature: live theme switching. Propagates a new [`CompTheme`] to every
    /// sub-element so the indicator restyles without being rebuilt.
    pub fn set_theme(&self, theme: CompTheme) {
        self.center_elem.set_theme(theme.clone());
        self.left_elem.set_theme(theme.clone());
        self.top_elem.set_theme(theme.clone());
        self.right_elem.set_theme(theme.clone());
        self.down_elem.set_theme(theme);
    }

    pub fn output_enter(&self, output: &Output) {
        self.center_elem
            .output_enter(output, Rectangle::default() /*unused*/);
        self.left_elem
            .output_enter(output, Rectangle::default() /*unused*/);
        self.top_elem
            .output_enter(output, Rectangle::default() /*unused*/);
        self.right_elem
            .output_enter(output, Rectangle::default() /*unused*/);
        self.down_elem
            .output_enter(output, Rectangle::default() /*unused*/);
    }

    pub fn output_leave(&self, output: &Output) {
        self.center_elem.output_leave(output);
        self.left_elem.output_leave(output);
        self.top_elem.output_leave(output);
        self.right_elem.output_leave(output);
        self.down_elem.output_leave(output);
    }
}

pub struct ResizeIndicatorInternal {
    shortcut1: String,
    shortcut2: String,
}

/// Accent-colored container style for the indicator badges.
fn accent_container_class(theme: &CompTheme) -> Box<dyn Fn(&iced_core::Theme) -> container::Style> {
    let on_accent = theme.on_accent_color();
    let accent = theme.accent_color();
    Box::new(move |_: &iced_core::Theme| container::Style {
        text_color: Some(on_accent),
        background: Some(Background::Color(accent)),
        border: Border {
            radius: BADGE_RADIUS.into(),
            width: 0.0,
            color: Color::TRANSPARENT,
            ..Default::default()
        },
        shadow: Default::default(),
        snap: false,
        border_only: false,
    })
}

impl Program for ResizeIndicatorInternal {
    type Message = ();

    fn program_name() -> &'static str {
        "ResizeIndicator"
    }

    fn view<'a>(&'a self, theme: &'a CompTheme) -> CompElement<'a, Self::Message> {
        let on_accent = theme.on_accent_color();

        row![
            styled_text(
                &self.shortcut1,
                theme.text_styles().label_default_strong(),
                on_accent,
            ),
            styled_text(fl!("grow-window"), theme.text_styles().body(), on_accent),
            Space::new().width(40.0).height(Length::Shrink),
            styled_text(
                &self.shortcut2,
                theme.text_styles().label_default_strong(),
                on_accent,
            ),
            styled_text(fl!("shrink-window"), theme.text_styles().body(), on_accent),
        ]
        .apply(comp_container)
        .align_x(Alignment::Center)
        .align_y(Alignment::Center)
        .padding(16)
        .apply(comp_container)
        .class(accent_container_class(theme))
        .width(Length::Shrink)
        .height(Length::Shrink)
        .into()
    }
}

pub struct ResizeIndicatorArrow {
    direction: Arc<Mutex<ResizeDirection>>,
    icon_outwards: &'static str,
    icon_inwards: &'static str,
}

impl Program for ResizeIndicatorArrow {
    type Message = ();

    fn program_name() -> &'static str {
        "ResizeIndicatorArrow"
    }

    fn view<'a>(&'a self, theme: &'a CompTheme) -> CompElement<'a, Self::Message> {
        named_icon(
            if *self.direction.lock().unwrap() == ResizeDirection::Outwards {
                self.icon_outwards
            } else {
                self.icon_inwards
            },
            20.0,
        )
        .apply(comp_container)
        .padding(8)
        .class(accent_container_class(theme))
        .width(Length::Shrink)
        .into()
    }
}
