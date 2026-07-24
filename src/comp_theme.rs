//! Compositor theme backed by icetron's `ThemeInterface`.
//!
//! `CompTheme` wraps an `Arc<dyn ThemeInterface>` to provide design tokens.
//! It is NOT the iced widget theme type — `iced_core::Theme` is used for that.
//! `CompTheme` is passed explicitly to view functions so they can extract
//! design tokens and pre-bake them into widget style closures.
//!
//! The theme is loaded at runtime from a `.ron` file installed by the
//! active theme package (e.g., `icetron-theme-playtron`).

use iced_core::{Background, Border, Color};
use icetron_themes::ThemeInterface;
use icetron_themes::dynamic::{DEFAULT_THEME_PAIR, DynamicTheme};
use std::ops::Deref;
use std::sync::Arc;
use tracing::info;

/// Compositor theme replacing `cosmic::Theme`.
/// Wraps a `ThemeInterface` for design tokens and carries compositor-specific settings.
#[derive(Clone)]
pub struct CompTheme {
    /// The underlying design token provider.
    theme: Arc<dyn ThemeInterface>,
    /// Whether this is a dark color scheme.
    pub is_dark: bool,
    /// Active window highlight border thickness (pixels).
    pub active_hint: u32,
    /// Gap sizes: (outer_gap, inner_gap) in logical pixels.
    pub gaps: (u32, u32),
    /// Motion tokens (durations, easing curves, springs) resolved from `theme`.
    /// Read by every animation instead of hardcoded constants.
    pub motion: crate::backend::render::animations::motion::Motion,
}

impl std::fmt::Debug for CompTheme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CompTheme")
            .field("is_dark", &self.is_dark)
            .field("active_hint", &self.active_hint)
            .field("gaps", &self.gaps)
            .finish_non_exhaustive()
    }
}

impl Deref for CompTheme {
    type Target = dyn ThemeInterface;
    fn deref(&self) -> &Self::Target {
        &*self.theme
    }
}

impl Default for CompTheme {
    fn default() -> Self {
        let is_dark = true;
        let theme: Arc<dyn ThemeInterface> = Arc::new(DEFAULT_THEME_PAIR.load(is_dark));
        Self {
            motion: crate::backend::render::animations::motion::Motion::from_theme(&*theme),
            theme,
            is_dark,
            active_hint: 3,
            gaps: (4, 4),
        }
    }
}

impl CompTheme {
    pub fn new(theme: Arc<dyn ThemeInterface>, is_dark: bool) -> Self {
        Self {
            motion: crate::backend::render::animations::motion::Motion::from_theme(&*theme),
            theme,
            is_dark,
            active_hint: 3,
            gaps: (4, 4),
        }
    }

    /// Load the current system theme (brand + color mode from disk).
    /// Falls back to the embedded playtron dark theme if nothing is configured.
    pub fn from_current() -> Self {
        use icetron_themes::color_mode::{Mode, get_color_mode};

        let mode = get_color_mode();
        let is_dark = matches!(mode, Mode::Dark);
        let theme_name = DynamicTheme::current_theme_name();

        info!(
            ?mode,
            is_dark,
            ?theme_name,
            "Loading compositor theme from current system config"
        );

        match theme_name {
            Some(ref name) => Self::from_file(name, is_dark),
            None => {
                // No symlink configured — use first available installed theme
                // (matches `icetron-theme current` fallback behavior).
                let fallback_name = DynamicTheme::list_available()
                    .first()
                    .map(|e| e.name.clone())
                    .unwrap_or_else(|| "playtron".to_string());
                info!(
                    fallback_name,
                    "No active theme symlink, using first available"
                );
                Self::from_file(&fallback_name, is_dark)
            }
        }
    }

    /// Load a theme by name from the system search paths.
    pub fn from_file(theme_name: &str, is_dark: bool) -> Self {
        let loaded = DynamicTheme::load_by_name(theme_name, is_dark);
        let using_fallback = loaded.is_none();
        let theme: Arc<dyn ThemeInterface> = Arc::new(loaded.unwrap_or_else(|| {
            let fallback = if is_dark {
                DEFAULT_THEME_PAIR.dark_fallback
            } else {
                DEFAULT_THEME_PAIR.light_fallback
            };
            ron::from_str(fallback).expect("embedded fallback theme RON is invalid")
        }));

        info!(
            theme_name,
            is_dark,
            using_fallback,
            radius_window = theme.radius_window(),
            bg_color = ?theme.fill_default(),
            text_color = ?theme.text_primary(),
            "CompTheme loaded, window radius: {}", theme.radius_window()
        );

        Self {
            motion: crate::backend::render::animations::motion::Motion::from_theme(&*theme),
            theme,
            is_dark,
            active_hint: 3,
            gaps: (4, 4),
        }
    }

    /// Access the underlying theme interface.
    pub fn theme(&self) -> &dyn ThemeInterface {
        &*self.theme
    }

    /// Neutral mid-gray color for backdrop/indicator effects.
    pub fn neutral_color(&self) -> WindowHintColor {
        let c = self.theme.text_tertiary();
        WindowHintColor {
            red: c.r,
            green: c.g,
            blue: c.b,
        }
    }

    pub fn accent_color(&self) -> Color {
        self.theme.primary()
    }

    pub fn on_accent_color(&self) -> Color {
        self.theme.primary_foreground()
    }

    pub fn on_bg_color(&self) -> Color {
        self.theme.text_primary()
    }

    pub fn bg_color(&self) -> Color {
        self.theme.fill_default()
    }

    pub fn surface_color(&self) -> Color {
        self.theme.surface_1()
    }

    pub fn on_surface_color(&self) -> Color {
        self.theme.text_primary()
    }

    pub fn divider_color(&self) -> Color {
        self.theme.stroke_subtle()
    }

    pub fn radius_s(&self) -> [f32; 4] {
        let r = self.theme.radius_sm();
        [r; 4]
    }

    pub fn radius_m(&self) -> [f32; 4] {
        let r = self.theme.radius_md();
        [r; 4]
    }

    /// Window corner radius for compositor decorations.
    /// Delegates to the theme's `radius_window()` token.
    pub fn radius_window(&self) -> [f32; 4] {
        [self.theme.radius_window(); 4]
    }

    /// Active window hint color (used for focus indication borders).
    pub fn active_window_hint(&self) -> WindowHintColor {
        let c = self.theme.primary();
        WindowHintColor {
            red: c.r,
            green: c.g,
            blue: c.b,
        }
    }

    /// Container style for accent-colored indicator boxes.
    pub fn accent_container_style(&self) -> ContainerStyle {
        ContainerStyle {
            text_color: Some(self.theme.primary_foreground()),
            background: Some(Background::Color(self.theme.primary())),
            border: Border {
                radius: 18.0.into(),
                width: 0.0,
                color: Color::TRANSPARENT,
                ..Default::default()
            },
        }
    }

    /// Container style for surface-colored floating panels (zoom bar, menus).
    pub fn surface_container_style(&self) -> ContainerStyle {
        ContainerStyle {
            text_color: Some(self.theme.text_primary()),
            background: Some(Background::Color(self.theme.surface_1())),
            border: Border {
                radius: radius_from_array(self.radius_s()),
                width: 1.0,
                color: self.theme.stroke_subtle(),
                ..Default::default()
            },
        }
    }

    /// Create an `iced_core::Theme` with a palette mapped from this theme's tokens.
    /// Used as the iced widget theme type for `UserInterface::draw()`.
    pub fn to_iced_theme(&self) -> iced_core::Theme {
        iced_core::Theme::custom(
            "compositor",
            iced_core::theme::Palette {
                background: self.bg_color(),
                text: self.on_bg_color(),
                primary: self.accent_color(),
                success: Color::from_rgb(0.0, 0.8, 0.0),
                warning: Color::from_rgb(0.8, 0.6, 0.0),
                danger: Color::from_rgb(0.8, 0.0, 0.0),
            },
        )
    }
}

/// Convert a `[f32; 4]` corner radius array to `iced_core::border::Radius`.
pub fn radius_from_array(r: [f32; 4]) -> iced_core::border::Radius {
    iced_core::border::Radius {
        top_left: r[0],
        top_right: r[1],
        bottom_right: r[2],
        bottom_left: r[3],
    }
}

/// Simple container style (replaces cosmic's theme::Container::custom closures).
#[derive(Debug, Clone)]
pub struct ContainerStyle {
    pub text_color: Option<Color>,
    pub background: Option<Background>,
    pub border: Border,
}

impl From<ContainerStyle> for iced_widget::container::Style {
    fn from(s: ContainerStyle) -> Self {
        iced_widget::container::Style {
            text_color: s.text_color,
            background: s.background,
            border: s.border,
            shadow: Default::default(),
            snap: false,
            border_only: false,
        }
    }
}

/// Color struct for active window hint (replaces `palette::Srgba`).
#[derive(Debug, Clone, Copy)]
pub struct WindowHintColor {
    pub red: f32,
    pub green: f32,
    pub blue: f32,
}
