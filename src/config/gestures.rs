use cosmic_comp_config::workspace::WorkspaceLayout;
use cosmic_settings_config::shortcuts::{self, action::Direction, Gestures};

pub fn add_default_gestures(gestures: &mut Gestures, workspace_layout: WorkspaceLayout) {
    // TODO: Do we take into account natural scroll?
    let (workspace_forward, workspace_backward) = match workspace_layout {
        WorkspaceLayout::Vertical => (Direction::Up, Direction::Down),
        WorkspaceLayout::Horizontal => (Direction::Left, Direction::Right),
    };

    gestures.insert_default_gesture(
        4 as u32,
        workspace_forward.clone(),
        shortcuts::Action::NextWorkspace,
    );

    gestures.insert_default_gesture(
        4 as u32,
        workspace_backward.clone(),
        shortcuts::Action::PreviousWorkspace,
    );

    // TODO: More default gestures?
}
