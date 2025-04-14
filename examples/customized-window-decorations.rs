use cosmic::iced::{Color, Rectangle, Size};
use cosmic::widget;
use cosmic::widget::canvas;
use cosmic_comp::hooks::{Decorations, Hooks};
use cosmic_comp::shell::element::stack::{
    DefaultDecorations as DefaultStackDecorations, TAB_HEIGHT,
};
use cosmic_comp::shell::element::window::{
    DefaultDecorations as DefaultWindowDecorations, SSD_HEIGHT,
};
use std::sync::Arc;

/// The struct implementing Decorations hook
#[derive(Debug)]
struct AddIndicator<Lower> {
    lower: Lower,
    height: i32,
}

/// An iced program drawing a circle, which we'll add to the window decorations
struct Circle {
    radius: f32,
    color: Color,
}

impl<Message, Theme, Renderer: cosmic::iced_renderer::geometry::Renderer>
    canvas::Program<Message, Theme, Renderer> for Circle
{
    type State = ();

    fn draw(
        &self,
        _state: &(),
        renderer: &Renderer,
        _theme: &Theme,
        bounds: Rectangle,
        _cursor: cosmic::iced::mouse::Cursor,
    ) -> Vec<Renderer::Geometry> {
        let bounds = bounds.size();
        let min = bounds.height.min(bounds.width);
        let mut frame = canvas::Frame::new(renderer, Size::new(min, min));
        let circle = canvas::Path::circle(frame.center(), self.radius);
        frame.fill(&circle, self.color);
        vec![frame.into_geometry()]
    }
}

impl<Internal, Message: std::clone::Clone + 'static, Lower: Decorations<Internal, Message>>
    Decorations<Internal, Message> for AddIndicator<Lower>
{
    fn view(&self, window: &Internal) -> cosmic::Element<'_, Message> {
        let orig = self.lower.view(window);
        widget::row()
            .push(
                widget::column()
                    .push(canvas(Circle {
                        radius: (self.height as f32 / 2.) * 0.8,
                        color: Color::new(1.0, 0.0, 0.0, 1.0),
                    }))
                    .width(self.height as f32),
            )
            .push(orig)
            .into()
    }
}

/// The customized cosmic-comp entrypoint
fn main() -> Result<(), Box<dyn std::error::Error>> {
    cosmic_comp::run(Hooks {
        window_decorations: Some(Arc::new(AddIndicator {
            height: SSD_HEIGHT,
            lower: DefaultWindowDecorations,
        })),
        stack_decorations: Some(Arc::new(AddIndicator {
            height: TAB_HEIGHT,
            lower: DefaultStackDecorations,
        })),
    })
}
