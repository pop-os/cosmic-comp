use cosmic_comp::comp_theme::CompTheme;
use cosmic_comp::hooks::{Decorations, Hooks};
use cosmic_comp::shell::element::stack::{
    CosmicStackInternal, DefaultDecorations as DefaultStackDecorations, Message as StackMessage,
};
use cosmic_comp::shell::element::window::{
    CosmicWindowInternal, DefaultDecorations as DefaultWindowDecorations, Message as WindowMessage,
};
use cosmic_comp::utils::iced::CompElement;
use iced_core::{Color, Length};
use std::sync::Arc;

/// The struct implementing Decorations hook — prepends a colored square to the
/// default decorations to demonstrate the customization API.
#[derive(Debug)]
struct AddIndicator<Lower> {
    lower: Lower,
}

// MERGE: dropped upstream's `Circle` canvas demo + blanket `Decorations` impl. The fork's
// `Decorations` trait takes `&CompTheme` and returns `CompElement`, and this example is built on
// `iced_widget` rather than `cosmic::widget`, so upstream's version cannot apply here.
impl Decorations<CosmicWindowInternal, WindowMessage> for AddIndicator<DefaultWindowDecorations> {
    fn view<'a>(
        &'a self,
        window: &'a CosmicWindowInternal,
        theme: &'a CompTheme,
    ) -> CompElement<'a, WindowMessage> {
        let orig = self.lower.view(window, theme);
        iced_widget::row![indicator_square(), orig]
            .width(Length::Fill)
            .into()
    }
}

impl Decorations<CosmicStackInternal, StackMessage> for AddIndicator<DefaultStackDecorations> {
    fn view<'a>(
        &'a self,
        stack: &'a CosmicStackInternal,
        theme: &'a CompTheme,
    ) -> CompElement<'a, StackMessage> {
        let orig = self.lower.view(stack, theme);
        iced_widget::row![indicator_square(), orig]
            .width(Length::Fill)
            .into()
    }
}

/// A simple red square used as a visual indicator.
fn indicator_square<'a, Message: 'a>() -> CompElement<'a, Message> {
    iced_widget::container(iced_widget::Space::new().width(16).height(16))
        .style(|_theme: &iced_core::Theme| iced_widget::container::Style {
            background: Some(Color::new(1.0, 0.0, 0.0, 1.0).into()),
            ..Default::default()
        })
        .width(Length::Shrink)
        .height(Length::Fill)
        .into()
}

/// The customized cosmic-comp entrypoint
fn main() -> Result<(), Box<dyn std::error::Error>> {
    cosmic_comp::run(Hooks {
        window_decorations: Some(Arc::new(AddIndicator {
            lower: DefaultWindowDecorations,
        })),
        stack_decorations: Some(Arc::new(AddIndicator {
            lower: DefaultStackDecorations,
        })),
    })
}
