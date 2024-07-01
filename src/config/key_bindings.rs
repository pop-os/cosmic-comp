use cosmic_comp_config::workspace::WorkspaceLayout;
use cosmic_settings_config::shortcuts::State as KeyState;
use cosmic_settings_config::shortcuts::{self, Modifiers, Shortcuts};
use smithay::input::keyboard::ModifiersState;
use xkbcommon::xkb;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Action {
    /// Behaviors managed internally by cosmic-comp.
    Private(PrivateAction),
    /// Behaviors managed via cosmic-settings.
    Shortcut(shortcuts::Action),
}

#[derive(Clone, Debug, Eq, PartialEq)]
// Behaviors which are internally defined and emitted.
pub enum PrivateAction {
    Escape,
    Resizing(
        shortcuts::action::ResizeDirection,
        shortcuts::action::ResizeEdge,
        shortcuts::State,
    ),
}

pub fn add_default_bindings(shortcuts: &mut Shortcuts, workspace_layout: WorkspaceLayout) {
    let (
        workspace_previous,
        workspace_next,
        (output_previous, output_previous_dir),
        (output_next, output_next_dir),
    ) = match workspace_layout {
        WorkspaceLayout::Horizontal => (
            [xkb::Keysym::Left, xkb::Keysym::h],
            [xkb::Keysym::Right, xkb::Keysym::l],
            (
                [xkb::Keysym::Up, xkb::Keysym::k],
                shortcuts::action::Direction::Up,
            ),
            (
                [xkb::Keysym::Down, xkb::Keysym::j],
                shortcuts::action::Direction::Down,
            ),
        ),
        WorkspaceLayout::Vertical => (
            [xkb::Keysym::Up, xkb::Keysym::k],
            [xkb::Keysym::Down, xkb::Keysym::j],
            (
                [xkb::Keysym::Left, xkb::Keysym::h],
                shortcuts::action::Direction::Left,
            ),
            (
                [xkb::Keysym::Right, xkb::Keysym::l],
                shortcuts::action::Direction::Right,
            ),
        ),
    };

    shortcuts.insert_default_binding(
        Modifiers::new().logo().ctrl(),
        workspace_previous.iter().copied(),
        shortcuts::Action::PreviousWorkspace,
    );

    shortcuts.insert_default_binding(
        Modifiers::new().logo().ctrl(),
        workspace_next.iter().copied(),
        shortcuts::Action::NextWorkspace,
    );

    shortcuts.insert_default_binding(
        Modifiers::new().logo().ctrl().shift(),
        workspace_previous.iter().copied(),
        shortcuts::Action::MoveToPreviousWorkspace,
    );

    shortcuts.insert_default_binding(
        Modifiers::new().logo().ctrl().shift(),
        workspace_next.iter().copied(),
        shortcuts::Action::MoveToNextWorkspace,
    );

    shortcuts.insert_default_binding(
        Modifiers::new().logo().ctrl(),
        output_previous.iter().copied(),
        shortcuts::Action::SwitchOutput(output_previous_dir),
    );

    shortcuts.insert_default_binding(
        Modifiers::new().logo().ctrl(),
        output_next.iter().copied(),
        shortcuts::Action::SwitchOutput(output_next_dir),
    );

    shortcuts.insert_default_binding(
        Modifiers::new().logo().ctrl().shift(),
        output_previous.iter().copied(),
        shortcuts::Action::MoveToOutput(output_previous_dir),
    );

    shortcuts.insert_default_binding(
        Modifiers::new().logo().ctrl().shift(),
        output_next.iter().copied(),
        shortcuts::Action::MoveToOutput(output_next_dir),
    );
}

/// Convert `cosmic_settings_config::shortcuts::State` to `smithay::backend::input::KeyState`.
pub fn cosmic_keystate_to_smithay(value: KeyState) -> smithay::backend::input::KeyState {
    match value {
        KeyState::Pressed => smithay::backend::input::KeyState::Pressed,
        KeyState::Released => smithay::backend::input::KeyState::Released,
    }
}

/// Convert `smithay::backend::input::KeyState` to `cosmic_settings_config::shortcuts::State`.
pub fn cosmic_keystate_from_smithay(value: smithay::backend::input::KeyState) -> KeyState {
    match value {
        smithay::backend::input::KeyState::Pressed => KeyState::Pressed,
        smithay::backend::input::KeyState::Released => KeyState::Released,
    }
}

/// Compare `cosmic_settings_config::shortcuts::Modifiers` to `smithay::input::keyboard::ModifiersState`.
pub fn cosmic_modifiers_eq_smithay(this: &Modifiers, other: &ModifiersState) -> bool {
    this.ctrl == other.ctrl
        && this.alt == other.alt
        && this.shift == other.shift
        && this.logo == other.logo
}

/// Convert `smithay::input::keyboard::ModifiersState` to `cosmic_settings_config::shortcuts::Modifiers`
pub fn cosmic_modifiers_from_smithay(value: ModifiersState) -> Modifiers {
    Modifiers {
        ctrl: value.ctrl,
        alt: value.alt,
        shift: value.shift,
        logo: value.logo,
    }
}
