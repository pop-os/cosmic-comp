// SPDX-License-Identifier: GPL-3.0-only

use crate::shell::{focus::FocusDirection, grabs::ResizeEdge, Direction, ResizeDirection};
use cosmic_comp_config::workspace::WorkspaceLayout;
use serde::Deserialize;
use smithay::{
    backend::input::KeyState,
    input::keyboard::{xkb::keysym_get_name, ModifiersState},
};
use std::collections::HashMap;

use super::types::*;

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub enum KeyModifier {
    Ctrl,
    Alt,
    Shift,
    Super,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct KeyModifiers {
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
    pub logo: bool,
}

impl PartialEq<ModifiersState> for KeyModifiers {
    fn eq(&self, other: &ModifiersState) -> bool {
        self.ctrl == other.ctrl
            && self.alt == other.alt
            && self.shift == other.shift
            && self.logo == other.logo
    }
}

impl Into<KeyModifiers> for ModifiersState {
    fn into(self) -> KeyModifiers {
        KeyModifiers {
            ctrl: self.ctrl,
            alt: self.alt,
            shift: self.shift,
            logo: self.logo,
        }
    }
}

impl std::ops::AddAssign<KeyModifier> for KeyModifiers {
    fn add_assign(&mut self, rhs: KeyModifier) {
        match rhs {
            KeyModifier::Ctrl => self.ctrl = true,
            KeyModifier::Alt => self.alt = true,
            KeyModifier::Shift => self.shift = true,
            KeyModifier::Super => self.logo = true,
        };
    }
}

impl std::ops::BitOr for KeyModifier {
    type Output = KeyModifiers;

    fn bitor(self, rhs: KeyModifier) -> Self::Output {
        let mut modifiers = self.into();
        modifiers += rhs;
        modifiers
    }
}

impl Into<KeyModifiers> for KeyModifier {
    fn into(self) -> KeyModifiers {
        let mut modifiers = KeyModifiers {
            ctrl: false,
            alt: false,
            shift: false,
            logo: false,
        };
        modifiers += self;
        modifiers
    }
}

/// Describtion of a key combination that might be
/// handled by the compositor.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Hash)]
#[serde(deny_unknown_fields)]
pub struct KeyPattern {
    /// What modifiers are expected to be pressed alongside the key
    #[serde(deserialize_with = "deserialize_KeyModifiers")]
    pub modifiers: KeyModifiers,
    /// The actual key, that was pressed
    #[serde(deserialize_with = "deserialize_Keysym", default)]
    pub key: Option<Keysym>,
}

impl KeyPattern {
    pub fn new(modifiers: impl Into<KeyModifiers>, key: Option<Keysym>) -> KeyPattern {
        KeyPattern {
            modifiers: modifiers.into(),
            key,
        }
    }

    pub fn inferred_direction(&self) -> Option<Direction> {
        match self.key? {
            Keysym::Left | Keysym::h | Keysym::H => Some(Direction::Left),
            Keysym::Down | Keysym::j | Keysym::J => Some(Direction::Down),
            Keysym::Up | Keysym::k | Keysym::K => Some(Direction::Up),
            Keysym::Right | Keysym::l | Keysym::L => Some(Direction::Right),
            _ => None,
        }
    }
}

impl ToString for KeyPattern {
    fn to_string(&self) -> String {
        let mut result = String::new();
        if self.modifiers.logo {
            result += "Super+";
        }
        if self.modifiers.ctrl {
            result += "Ctrl+";
        }
        if self.modifiers.alt {
            result += "Alt+";
        }
        if self.modifiers.shift {
            result += "Shift+";
        }

        if let Some(key) = self.key {
            result += &keysym_get_name(key);
        } else {
            result.remove(result.len() - 1);
        }
        result
    }
}

#[derive(Debug, Deserialize, Clone, PartialEq, Eq)]
pub enum Action {
    Terminate,
    Debug,
    Close,
    #[serde(skip)]
    Escape,

    Workspace(u8),
    NextWorkspace,
    PreviousWorkspace,
    LastWorkspace,
    MoveToWorkspace(u8),
    MoveToNextWorkspace,
    MoveToPreviousWorkspace,
    MoveToLastWorkspace,
    SendToWorkspace(u8),
    SendToNextWorkspace,
    SendToPreviousWorkspace,
    SendToLastWorkspace,

    NextOutput,
    PreviousOutput,
    MoveToNextOutput,
    MoveToPreviousOutput,
    SendToNextOutput,
    SendToPreviousOutput,
    SwitchOutput(Direction),
    MoveToOutput(Direction),
    SendToOutput(Direction),

    MigrateWorkspaceToNextOutput,
    MigrateWorkspaceToPreviousOutput,
    MigrateWorkspaceToOutput(Direction),

    Focus(FocusDirection),
    Move(Direction),

    ToggleOrientation,
    Orientation(crate::shell::layout::Orientation),

    ToggleStacking,
    ToggleTiling,
    ToggleWindowFloating,
    SwapWindow,

    Resizing(ResizeDirection),
    #[serde(skip)]
    _ResizingInternal(ResizeDirection, ResizeEdge, KeyState),
    Maximize,
    Spawn(String),
}

fn insert_binding(
    key_bindings: &mut HashMap<KeyPattern, Action>,
    modifiers: KeyModifiers,
    keys: impl Iterator<Item = Keysym>,
    action: Action,
) {
    if !key_bindings.values().any(|a| a == &action) {
        for key in keys {
            let pattern = KeyPattern {
                modifiers: modifiers.clone(),
                key: Some(key),
            };
            if !key_bindings.contains_key(&pattern) {
                key_bindings.insert(pattern, action.clone());
            }
        }
    }
}

pub fn add_default_bindings(
    key_bindings: &mut HashMap<KeyPattern, Action>,
    workspace_layout: WorkspaceLayout,
) {
    let (
        workspace_previous,
        workspace_next,
        (output_previous, output_previous_dir),
        (output_next, output_next_dir),
    ) = match workspace_layout {
        WorkspaceLayout::Horizontal => (
            [Keysym::Left, Keysym::h],
            [Keysym::Right, Keysym::l],
            ([Keysym::Up, Keysym::k], Direction::Up),
            ([Keysym::Down, Keysym::j], Direction::Down),
        ),
        WorkspaceLayout::Vertical => (
            [Keysym::Up, Keysym::k],
            [Keysym::Down, Keysym::j],
            ([Keysym::Left, Keysym::h], Direction::Left),
            ([Keysym::Right, Keysym::l], Direction::Right),
        ),
    };

    insert_binding(
        key_bindings,
        KeyModifiers {
            logo: true,
            ctrl: true,
            ..Default::default()
        },
        workspace_previous.iter().copied(),
        Action::PreviousWorkspace,
    );
    insert_binding(
        key_bindings,
        KeyModifiers {
            logo: true,
            ctrl: true,
            ..Default::default()
        },
        workspace_next.iter().copied(),
        Action::NextWorkspace,
    );
    insert_binding(
        key_bindings,
        KeyModifiers {
            logo: true,
            ctrl: true,
            shift: true,
            ..Default::default()
        },
        workspace_previous.iter().copied(),
        Action::MoveToPreviousWorkspace,
    );
    insert_binding(
        key_bindings,
        KeyModifiers {
            logo: true,
            ctrl: true,
            shift: true,
            ..Default::default()
        },
        workspace_next.iter().copied(),
        Action::MoveToNextWorkspace,
    );

    insert_binding(
        key_bindings,
        KeyModifiers {
            logo: true,
            ctrl: true,
            ..Default::default()
        },
        output_previous.iter().copied(),
        Action::SwitchOutput(output_previous_dir),
    );
    insert_binding(
        key_bindings,
        KeyModifiers {
            logo: true,
            ctrl: true,
            ..Default::default()
        },
        output_next.iter().copied(),
        Action::SwitchOutput(output_next_dir),
    );
    insert_binding(
        key_bindings,
        KeyModifiers {
            logo: true,
            ctrl: true,
            shift: true,
            ..Default::default()
        },
        output_previous.iter().copied(),
        Action::MoveToOutput(output_previous_dir),
    );
    insert_binding(
        key_bindings,
        KeyModifiers {
            logo: true,
            ctrl: true,
            shift: true,
            ..Default::default()
        },
        output_next.iter().copied(),
        Action::MoveToOutput(output_next_dir),
    );
}
