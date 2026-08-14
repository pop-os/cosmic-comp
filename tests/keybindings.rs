// SPDX-License-Identifier: GPL-3.0-only

//! `data/keybindings.ron` ships as the shortcut `defaults` and gets no
//! compile-time checking. Worse, an action the config crate cannot parse is
//! silently swapped for `Disable`, so a stale variant name reads at runtime as
//! "that key does nothing". Parse the shipped file here instead.

use cosmic_settings_config::shortcuts::{
    Action, Binding, Shortcuts,
    action::{FocusDirection, System},
};

fn defaults() -> Shortcuts {
    ron::from_str(include_str!("../data/keybindings.ron"))
        .expect("data/keybindings.ron is not valid RON")
}

fn action_for(shortcuts: &Shortcuts, binding: &str) -> Action {
    let key = Binding::from_str_partial(binding).expect("invalid binding in test");
    shortcuts
        .0
        .get(&key)
        .unwrap_or_else(|| panic!("{binding} is unbound"))
        .clone()
}

#[test]
fn every_action_deserializes() {
    let shortcuts = defaults();

    assert!(!shortcuts.0.is_empty());
    assert!(
        !shortcuts
            .0
            .values()
            .any(|action| *action == Action::Disable),
        "an action failed to deserialize and was replaced with Disable"
    );
}

/// The laptop function row emits the Windows chords rather than dedicated
/// keysyms, so F7/F9/F10 are reachable only through these bindings.
#[test]
fn laptop_function_row() {
    let shortcuts = defaults();

    assert_eq!(
        action_for(&shortcuts, "Super+p"),
        Action::System(System::DisplayToggle)
    );
    assert_eq!(
        action_for(&shortcuts, "Super+i"),
        Action::System(System::Settings)
    );
    assert_eq!(
        action_for(&shortcuts, "Super+l"),
        Action::System(System::LockScreen)
    );
    assert_eq!(
        action_for(&shortcuts, "XF86Calculator"),
        Action::System(System::Calculator)
    );
}

/// Taking Super+i/Super+l for the function row must not cost a focus direction.
#[test]
fn every_focus_direction_is_reachable() {
    let shortcuts = defaults();

    for (binding, direction) in [
        ("Super+Left", FocusDirection::Left),
        ("Super+Right", FocusDirection::Right),
        ("Super+Up", FocusDirection::Up),
        ("Super+Down", FocusDirection::Down),
        ("Super+u", FocusDirection::Out),
        ("Super+Shift+u", FocusDirection::In),
    ] {
        assert_eq!(action_for(&shortcuts, binding), Action::Focus(direction));
    }
}
