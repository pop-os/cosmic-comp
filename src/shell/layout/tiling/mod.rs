// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render::{
        element::AsGlowRenderer, BackdropShader, IndicatorShader, Key, ACTIVE_GROUP_COLOR,
        GROUP_COLOR,
    },
    shell::{
        element::{
            resize_indicator::ResizeIndicator,
            stack::{CosmicStackRenderElement, MoveResult as StackMoveResult},
            window::CosmicWindowRenderElement,
            CosmicMapped, CosmicMappedRenderElement, CosmicStack, CosmicWindow,
        },
        focus::{
            target::{KeyboardFocusTarget, PointerFocusTarget, WindowGroup},
            FocusDirection, FocusStackMut,
        },
        grabs::ResizeEdge,
        layout::Orientation,
        CosmicSurface, OutputNotMapped, OverviewMode, ResizeDirection, ResizeMode, Trigger,
    },
    utils::prelude::*,
    wayland::{
        handlers::xdg_shell::popup::get_popup_toplevel, protocols::toplevel_info::ToplevelInfoState,
    },
};

use id_tree::{InsertBehavior, MoveBehavior, Node, NodeId, NodeIdError, RemoveBehavior, Tree};
use keyframe::{ease, functions::EaseInOutCubic};
use smithay::{
    backend::renderer::{
        element::{
            utils::{CropRenderElement, Relocate, RelocateRenderElement, RescaleRenderElement},
            AsRenderElements, Id, RenderElement,
        },
        glow::GlowRenderer,
        ImportAll, ImportMem, Renderer,
    },
    desktop::{layer_map_for_output, space::SpaceElement, PopupKind},
    input::Seat,
    output::Output,
    reexports::wayland_server::Client,
    utils::{IsAlive, Logical, Point, Rectangle, Scale},
    wayland::{compositor::add_blocker, seat::WaylandFocus},
};
use std::{
    borrow::Borrow,
    collections::{HashMap, VecDeque},
    hash::Hash,
    sync::{Arc, Weak},
    time::{Duration, Instant},
};
use tracing::trace;
use wayland_backend::server::ClientId;

mod blocker;
mod grabs;
pub use self::blocker::*;
pub use self::grabs::*;

pub const ANIMATION_DURATION: Duration = Duration::from_millis(200);
pub const MOUSE_ANIMATION_DELAY: Duration = Duration::from_millis(150);
pub const INITIAL_MOUSE_ANIMATION_DELAY: Duration = Duration::from_millis(500);

#[derive(Debug, Clone)]
struct OutputData {
    output: Output,
    location: Point<i32, Logical>,
}

impl Borrow<Output> for OutputData {
    fn borrow(&self) -> &Output {
        &self.output
    }
}

impl PartialEq for OutputData {
    fn eq(&self, other: &Self) -> bool {
        self.output == other.output
    }
}

impl Eq for OutputData {}

impl PartialEq<Output> for OutputData {
    fn eq(&self, other: &Output) -> bool {
        &self.output == other
    }
}

impl Hash for OutputData {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.output.hash(state)
    }
}

#[derive(Debug, serde::Deserialize, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Left,
    Right,
    Up,
    Down,
}

impl std::ops::Not for Direction {
    type Output = Self;
    fn not(self) -> Self::Output {
        match self {
            Direction::Left => Direction::Right,
            Direction::Right => Direction::Left,
            Direction::Up => Direction::Down,
            Direction::Down => Direction::Up,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FocusResult {
    None,
    Handled,
    Some(KeyboardFocusTarget),
}

#[derive(Debug, Clone, PartialEq)]
pub enum MoveResult {
    Done,
    MoveFurther(KeyboardFocusTarget),
    ShiftFocus(KeyboardFocusTarget),
}

#[derive(Debug, Clone, PartialEq)]
enum TargetZone {
    Initial,
    InitialPlaceholder(NodeId),
    WindowStack(NodeId, Rectangle<i32, Logical>),
    WindowSplit(NodeId, Direction),
    GroupEdge(NodeId, Direction),
    GroupInterior(NodeId, usize),
}

impl TargetZone {
    fn is_window_zone(&self) -> bool {
        matches!(
            self,
            TargetZone::WindowStack(..) | TargetZone::WindowSplit(_, _)
        )
    }
}

#[derive(Debug, Clone, Default)]
struct TreeQueue {
    trees: VecDeque<(Tree<Data>, Duration, Option<TilingBlocker>)>,
    animation_start: Option<Instant>,
}

impl TreeQueue {
    pub fn push_tree(
        &mut self,
        tree: Tree<Data>,
        duration: impl Into<Option<Duration>>,
        blocker: Option<TilingBlocker>,
    ) {
        self.trees
            .push_back((tree, duration.into().unwrap_or(Duration::ZERO), blocker))
    }
}

#[derive(Debug, Clone)]
pub struct TilingLayout {
    gaps: (i32, i32),
    queues: HashMap<OutputData, TreeQueue>,
    standby_tree: Option<Tree<Data>>,
    pending_blockers: Vec<TilingBlocker>,
    placeholder_id: Id,
    last_overview_hover: Option<(Option<Instant>, TargetZone)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PillIndicator {
    Outer(Direction),
    Inner(usize),
}

#[derive(Debug, Clone)]
pub enum Data {
    Group {
        orientation: Orientation,
        sizes: Vec<i32>,
        last_geometry: Rectangle<i32, Logical>,
        alive: Arc<()>,
        pill_indicator: Option<PillIndicator>,
    },
    Mapped {
        mapped: CosmicMapped,
        last_geometry: Rectangle<i32, Logical>,
    },
    Placeholder {
        last_geometry: Rectangle<i32, Logical>,
        initial_placeholder: bool,
    },
}

impl Data {
    fn new_group(orientation: Orientation, geo: Rectangle<i32, Logical>) -> Data {
        Data::Group {
            orientation,
            sizes: vec![
                match orientation {
                    Orientation::Vertical => geo.size.w / 2,
                    Orientation::Horizontal => geo.size.h / 2,
                };
                2
            ],
            last_geometry: geo,
            alive: Arc::new(()),
            pill_indicator: None,
        }
    }

    fn is_group(&self) -> bool {
        matches!(self, Data::Group { .. })
    }
    fn is_mapped(&self, mapped: Option<&CosmicMapped>) -> bool {
        match mapped {
            Some(m) => matches!(self, Data::Mapped { mapped, .. } if m == mapped),
            None => matches!(self, Data::Mapped { .. }),
        }
    }
    fn is_stack(&self) -> bool {
        match self {
            Data::Mapped { mapped, .. } => mapped.is_stack(),
            _ => false,
        }
    }
    fn is_placeholder(&self) -> bool {
        match self {
            Data::Placeholder { .. } => true,
            _ => false,
        }
    }

    fn orientation(&self) -> Orientation {
        match self {
            Data::Group { orientation, .. } => *orientation,
            _ => panic!("Not a group"),
        }
    }

    fn add_window(&mut self, idx: usize) {
        match self {
            Data::Group {
                sizes,
                last_geometry,
                orientation,
                ..
            } => {
                let last_length = match orientation {
                    Orientation::Horizontal => last_geometry.size.h,
                    Orientation::Vertical => last_geometry.size.w,
                };
                let equal_sizing = last_length / (sizes.len() as i32 + 1); // new window size
                let remainder = last_length - equal_sizing; // size for the rest of the windowns

                for size in sizes.iter_mut() {
                    *size = ((*size as f64 / last_length as f64) * remainder as f64).round() as i32;
                }
                let used_size: i32 = sizes.iter().sum();
                let new_size = last_length - used_size;

                sizes.insert(idx, new_size);
            }
            _ => panic!("Adding window to leaf?"),
        }
    }

    fn swap_windows(&mut self, i: usize, j: usize) {
        match self {
            Data::Group { sizes, .. } => {
                sizes.swap(i, j);
            }
            _ => panic!("Swapping windows to a leaf?"),
        }
    }

    fn remove_window(&mut self, idx: usize) {
        match self {
            Data::Group {
                sizes,
                last_geometry,
                orientation,
                ..
            } => {
                let last_length = match orientation {
                    Orientation::Horizontal => last_geometry.size.h,
                    Orientation::Vertical => last_geometry.size.w,
                };
                let old_size = sizes.remove(idx);
                let remaining_size: i32 = sizes.iter().sum();

                for size in sizes.iter_mut() {
                    *size +=
                        ((*size as f64 / remaining_size as f64) * old_size as f64).round() as i32;
                }
                let used_size: i32 = sizes.iter().sum();
                let overflow = last_length - used_size;
                if overflow != 0 {
                    *sizes.last_mut().unwrap() += overflow;
                }
            }
            _ => panic!("Added window to leaf?"),
        }
    }

    fn geometry(&self) -> &Rectangle<i32, Logical> {
        match self {
            Data::Group { last_geometry, .. } => last_geometry,
            Data::Mapped { last_geometry, .. } => last_geometry,
            Data::Placeholder { last_geometry, .. } => last_geometry,
        }
    }

    fn update_geometry(&mut self, geo: Rectangle<i32, Logical>) {
        match self {
            Data::Group {
                orientation,
                sizes,
                last_geometry,
                ..
            } => {
                let previous_length = match orientation {
                    Orientation::Horizontal => last_geometry.size.h,
                    Orientation::Vertical => last_geometry.size.w,
                };
                let new_length = match orientation {
                    Orientation::Horizontal => geo.size.h,
                    Orientation::Vertical => geo.size.w,
                };

                sizes.iter_mut().for_each(|len| {
                    *len = (((*len as f64) / (previous_length as f64)) * (new_length as f64))
                        .round() as i32;
                });
                let sum: i32 = sizes.iter().sum();
                if sum < new_length {
                    *sizes.last_mut().unwrap() += new_length - sum;
                }
                *last_geometry = geo;
            }
            Data::Mapped { last_geometry, .. } | Data::Placeholder { last_geometry, .. } => {
                *last_geometry = geo;
            }
        }
    }

    fn len(&self) -> usize {
        match self {
            Data::Group { sizes, .. } => sizes.len(),
            _ => 1,
        }
    }
}

#[derive(Debug, Clone)]
enum FocusedNodeData {
    Group(Vec<NodeId>, Weak<()>),
    Window(CosmicMapped),
}

impl TilingLayout {
    pub fn new(gaps: (u8, u8)) -> TilingLayout {
        TilingLayout {
            gaps: (gaps.0 as i32, gaps.1 as i32),
            queues: HashMap::new(),
            standby_tree: None,
            pending_blockers: Vec::new(),
            placeholder_id: Id::new(),
            last_overview_hover: None,
        }
    }

    pub fn map_output(&mut self, output: &Output, location: Point<i32, Logical>) {
        if !self.queues.contains_key(output) {
            self.queues.insert(
                OutputData {
                    output: output.clone(),
                    location,
                },
                TreeQueue {
                    trees: {
                        let mut queue = VecDeque::new();
                        queue.push_back((
                            self.standby_tree.take().unwrap_or_else(Tree::new),
                            Duration::ZERO,
                            None,
                        ));
                        queue
                    },
                    animation_start: None,
                },
            );
        } else {
            let tree = self.queues.remove(output).unwrap();
            self.queues.insert(
                OutputData {
                    output: output.clone(),
                    location,
                },
                tree,
            );
        }
    }

    pub fn unmap_output(
        &mut self,
        output: &Output,
        toplevel_info: &mut ToplevelInfoState<State, CosmicSurface>,
    ) {
        if let Some(mut src) = self.queues.remove(output) {
            // Operate on last pending tree & unblock queue
            for blocker in src
                .trees
                .iter_mut()
                .flat_map(|(_, _, blocker)| blocker.take())
            {
                self.pending_blockers.push(blocker);
            }
            let (src, _, _) = src.trees.pop_back().expect("No tree in queue");

            let Some((new_output, dst_queue)) = self.queues.iter_mut().next() else {
                self.standby_tree = Some(src);
                return;
            };

            let mut dst = dst_queue.trees.back().unwrap().0.copy_clone();
            let orientation = match new_output.output.geometry().size {
                x if x.w >= x.h => Orientation::Vertical,
                _ => Orientation::Horizontal,
            };
            for node in src
                .root_node_id()
                .and_then(|root_id| src.traverse_pre_order(root_id).ok())
                .into_iter()
                .flatten()
            {
                if let Data::Mapped {
                    mapped,
                    last_geometry: _,
                } = node.data()
                {
                    for (toplevel, _) in mapped.windows() {
                        toplevel_info.toplevel_leave_output(&toplevel, output);
                        toplevel_info.toplevel_enter_output(&toplevel, &new_output.output);
                    }
                    mapped.output_leave(output);
                    mapped.output_enter(&new_output.output, mapped.bbox());
                }
            }
            TilingLayout::merge_trees(src, &mut dst, orientation);

            let blocker = TilingLayout::update_positions(output, &mut dst, self.gaps);
            dst_queue.push_tree(dst, ANIMATION_DURATION, blocker);
        }
    }

    pub fn map<'a>(
        &mut self,
        window: CosmicMapped,
        seat: &Seat<State>,
        focus_stack: impl Iterator<Item = &'a CosmicMapped> + 'a,
        direction: Option<Direction>,
    ) {
        let output = seat.active_output();
        window.output_enter(&output, window.bbox());
        window.set_bounds(output.geometry().size);
        self.map_internal(window, &output, Some(focus_stack), direction);
    }

    fn map_internal<'a>(
        &mut self,
        window: impl Into<CosmicMapped>,
        output: &Output,
        focus_stack: Option<impl Iterator<Item = &'a CosmicMapped> + 'a>,
        direction: Option<Direction>,
    ) {
        let queue = self.queues.get_mut(output).expect("Output not mapped?");
        let mut tree = queue.trees.back().unwrap().0.copy_clone();

        TilingLayout::map_to_tree(&mut tree, window, output, focus_stack, direction);

        let blocker = TilingLayout::update_positions(output, &mut tree, self.gaps);
        queue.push_tree(tree, ANIMATION_DURATION, blocker);
    }

    fn map_to_tree<'a>(
        mut tree: &mut Tree<Data>,
        window: impl Into<CosmicMapped>,
        output: &Output,
        focus_stack: Option<impl Iterator<Item = &'a CosmicMapped> + 'a>,
        direction: Option<Direction>,
    ) {
        let window = window.into();
        let new_window = Node::new(Data::Mapped {
            mapped: window.clone(),
            last_geometry: Rectangle::from_loc_and_size((0, 0), (100, 100)),
        });

        let window_id = if let Some(direction) = direction {
            if let Some(root_id) = tree.root_node_id().cloned() {
                let orientation = match direction {
                    Direction::Left | Direction::Right => Orientation::Vertical,
                    Direction::Up | Direction::Down => Orientation::Horizontal,
                };

                let new_id = tree.insert(new_window, InsertBehavior::AsRoot).unwrap();
                TilingLayout::new_group(&mut tree, &root_id, &new_id, orientation).unwrap();
                tree.make_nth_sibling(
                    &new_id,
                    match direction {
                        Direction::Left | Direction::Up => 1,
                        Direction::Right | Direction::Down => 0,
                    },
                )
                .unwrap();
                new_id
            } else {
                tree.insert(new_window, InsertBehavior::AsRoot).unwrap()
            }
        } else {
            let last_active = focus_stack
                .and_then(|focus_stack| TilingLayout::last_active_window(&mut tree, focus_stack));

            if let Some((ref node_id, mut last_active_window)) = last_active {
                if window.is_window() && last_active_window.is_stack() {
                    let surface = window.active_window();
                    last_active_window
                        .stack_ref_mut()
                        .unwrap()
                        .add_window(surface, None);
                    return;
                }

                let orientation = {
                    let window_size = tree.get(node_id).unwrap().data().geometry().size;
                    if window_size.w > window_size.h {
                        Orientation::Vertical
                    } else {
                        Orientation::Horizontal
                    }
                };
                let new_id = tree.insert(new_window, InsertBehavior::AsRoot).unwrap();
                TilingLayout::new_group(&mut tree, &node_id, &new_id, orientation).unwrap();
                new_id
            } else {
                // nothing? then we add to the root
                if let Some(root_id) = tree.root_node_id().cloned() {
                    let orientation = {
                        let output_size = output.geometry().size;
                        if output_size.w > output_size.h {
                            Orientation::Vertical
                        } else {
                            Orientation::Horizontal
                        }
                    };
                    let new_id = tree.insert(new_window, InsertBehavior::AsRoot).unwrap();
                    TilingLayout::new_group(&mut tree, &root_id, &new_id, orientation).unwrap();
                    new_id
                } else {
                    tree.insert(new_window, InsertBehavior::AsRoot).unwrap()
                }
            }
        };

        *window.tiling_node_id.lock().unwrap() = Some(window_id);
    }

    pub fn unmap(&mut self, window: &CosmicMapped) -> Option<Output> {
        let output = {
            let node_id = window.tiling_node_id.lock().unwrap().clone()?;
            self.queues
                .iter()
                .find(|(_, queue)| {
                    queue
                        .trees
                        .back()
                        .unwrap()
                        .0
                        .get(&node_id)
                        .map(|node| node.data().is_mapped(Some(window)))
                        .unwrap_or(false)
                })
                .map(|(o, _)| o.output.clone())?
        };

        self.unmap_window_internal(window);

        window.output_leave(&output);
        window.set_tiled(false);
        Some(output)
    }

    pub fn unmap_as_placeholder(&mut self, window: &CosmicMapped) -> Option<(Output, NodeId)> {
        let node_id = window.tiling_node_id.lock().unwrap().clone()?;
        let output = {
            self.queues
                .iter()
                .find(|(_, queue)| {
                    queue
                        .trees
                        .back()
                        .unwrap()
                        .0
                        .get(&node_id)
                        .map(|node| node.data().is_mapped(Some(window)))
                        .unwrap_or(false)
                })
                .map(|(o, _)| o.output.clone())?
        };

        let data = self
            .queues
            .get_mut(&output)
            .unwrap()
            .trees
            .back_mut()
            .unwrap()
            .0
            .get_mut(&node_id)
            .unwrap()
            .data_mut();
        *data = Data::Placeholder {
            last_geometry: data.geometry().clone(),
            initial_placeholder: true,
        };

        window.output_leave(&output);
        window.set_tiled(false);
        Some((output, node_id))
    }

    fn unmap_window_internal(&mut self, mapped: &CosmicMapped) {
        let tiling_node_id = mapped.tiling_node_id.lock().unwrap().as_ref().cloned();
        if let Some(node_id) = tiling_node_id {
            if let Some((output, queue)) = self.queues.iter_mut().find(|(_, queue)| {
                let tree = &queue.trees.back().unwrap().0;
                tree.get(&node_id)
                    .map(|node| node.data().is_mapped(Some(mapped)))
                    .unwrap_or(false)
            }) {
                let mut tree = queue.trees.back().unwrap().0.copy_clone();

                TilingLayout::unmap_internal(&mut tree, &node_id);

                let blocker = TilingLayout::update_positions(&output.output, &mut tree, self.gaps);
                queue.push_tree(tree, ANIMATION_DURATION, blocker);
            }
        }
    }

    fn unmap_internal(tree: &mut Tree<Data>, node: &NodeId) {
        let parent_id = tree.get(node).ok().and_then(|node| node.parent()).cloned();
        let position = parent_id.as_ref().and_then(|parent_id| {
            tree.children_ids(&parent_id)
                .unwrap()
                .position(|id| id == node)
        });
        let parent_parent_id = parent_id.as_ref().and_then(|parent_id| {
            tree.get(parent_id)
                .ok()
                .and_then(|node| node.parent())
                .cloned()
        });

        // remove self
        trace!(?node, "Removing node.");
        let _ = tree.remove_node(node.clone(), RemoveBehavior::DropChildren);

        // fixup parent node
        match parent_id {
            Some(id) => {
                let position = position.unwrap();
                let group = tree.get_mut(&id).unwrap().data_mut();
                assert!(group.is_group());

                if group.len() > 2 {
                    group.remove_window(position);
                } else {
                    trace!("Removing Group");
                    let other_child = tree.children_ids(&id).unwrap().cloned().next().unwrap();
                    let fork_pos = parent_parent_id.as_ref().and_then(|parent_id| {
                        tree.children_ids(parent_id).unwrap().position(|i| i == &id)
                    });
                    let _ = tree.remove_node(id.clone(), RemoveBehavior::OrphanChildren);
                    tree.move_node(
                        &other_child,
                        parent_parent_id
                            .as_ref()
                            .map(|parent_id| MoveBehavior::ToParent(parent_id))
                            .unwrap_or(MoveBehavior::ToRoot),
                    )
                    .unwrap();
                    if let Some(old_pos) = fork_pos {
                        tree.make_nth_sibling(&other_child, old_pos).unwrap();
                    }
                }
            }
            None => {} // root
        }
    }

    pub fn output_for_element(&self, elem: &CosmicMapped) -> Option<&Output> {
        self.mapped().find_map(|(o, m, _)| (m == elem).then_some(o))
    }

    // TODO: Move would needs this to be accurate during animations
    pub fn element_geometry(&self, elem: &CosmicMapped) -> Option<Rectangle<i32, Logical>> {
        if let Some(id) = elem.tiling_node_id.lock().unwrap().as_ref() {
            if let Some(output) = self.output_for_element(elem) {
                let (output_data, queue) = self.queues.get_key_value(output).unwrap();
                let node = queue.trees.back().unwrap().0.get(id).ok()?;
                let data = node.data();
                assert!(data.is_mapped(Some(elem)));
                let mut geo = *data.geometry();
                geo.loc += output_data.location;
                return Some(geo);
            }
        }
        None
    }

    pub fn move_current_node<'a>(
        &mut self,
        direction: Direction,
        seat: &Seat<State>,
    ) -> MoveResult {
        let output = seat.active_output();
        let queue = self.queues.get_mut(&output).unwrap();
        let mut tree = queue.trees.back().unwrap().0.copy_clone();

        let Some(target) = seat.get_keyboard().unwrap().current_focus() else {
            return MoveResult::Done;
        };
        let Some((node_id, data)) =
            TilingLayout::currently_focused_node(&mut tree, &seat.active_output(), target)
        else {
            return MoveResult::Done;
        };

        // stacks may handle movement internally
        if let FocusedNodeData::Window(window) = data.clone() {
            match window.handle_move(direction) {
                StackMoveResult::Handled => return MoveResult::Done,
                StackMoveResult::MoveOut(surface, loop_handle) => {
                    let mapped: CosmicMapped = CosmicWindow::new(surface, loop_handle).into();
                    mapped.output_enter(&output, mapped.bbox());
                    let orientation = match direction {
                        Direction::Left | Direction::Right => Orientation::Vertical,
                        Direction::Up | Direction::Down => Orientation::Horizontal,
                    };

                    let new_node = Node::new(Data::Mapped {
                        mapped: mapped.clone(),
                        last_geometry: Rectangle::from_loc_and_size((0, 0), (100, 100)),
                    });
                    let new_id = tree.insert(new_node, InsertBehavior::AsRoot).unwrap();
                    TilingLayout::new_group(&mut tree, &node_id, &new_id, orientation).unwrap();
                    tree.make_nth_sibling(
                        &new_id,
                        match direction {
                            Direction::Left | Direction::Up => 0,
                            Direction::Right | Direction::Down => 1,
                        },
                    )
                    .unwrap();
                    *mapped.tiling_node_id.lock().unwrap() = Some(new_id);

                    let blocker = TilingLayout::update_positions(&output, &mut tree, self.gaps);
                    queue.push_tree(tree, ANIMATION_DURATION, blocker);
                    return MoveResult::ShiftFocus(mapped.into());
                }
                StackMoveResult::Default => {} // continue normally
            }
        }

        let mut child_id = node_id.clone();
        // Without a parent to start with, just return
        let Some(og_parent) = tree.get(&node_id).unwrap().parent().cloned() else {
            return match data {
                FocusedNodeData::Window(window) => MoveResult::MoveFurther(window.into()),
                FocusedNodeData::Group(focus_stack, alive) => MoveResult::MoveFurther(
                    WindowGroup {
                        node: node_id,
                        output: output.downgrade(),
                        alive,
                        focus_stack,
                    }
                    .into(),
                ),
            };
        };
        let og_idx = tree
            .children_ids(&og_parent)
            .unwrap()
            .position(|id| id == &child_id)
            .unwrap();
        let mut maybe_parent = Some(og_parent.clone());

        while let Some(parent) = maybe_parent {
            let parent_data = tree.get(&parent).unwrap().data();
            let orientation = parent_data.orientation();
            let len = parent_data.len();

            // which child are we?
            let idx = tree
                .children_ids(&parent)
                .unwrap()
                .position(|id| id == &child_id)
                .unwrap();

            // if the orientation does not match..
            if matches!(
                (orientation, direction),
                (Orientation::Horizontal, Direction::Right)
                    | (Orientation::Horizontal, Direction::Left)
                    | (Orientation::Vertical, Direction::Up)
                    | (Orientation::Vertical, Direction::Down)
            ) {
                // ...create a new group with our parent (cleanup will remove any one-child-groups afterwards)
                TilingLayout::new_group(
                    &mut tree,
                    &parent,
                    &node_id,
                    match direction {
                        Direction::Left | Direction::Right => Orientation::Vertical,
                        Direction::Up | Direction::Down => Orientation::Horizontal,
                    },
                )
                .unwrap();
                tree.make_nth_sibling(
                    &node_id,
                    if direction == Direction::Left || direction == Direction::Up {
                        0
                    } else {
                        1
                    },
                )
                .unwrap();

                tree.get_mut(&og_parent)
                    .unwrap()
                    .data_mut()
                    .remove_window(og_idx);

                let blocker = TilingLayout::update_positions(&output, &mut tree, self.gaps);
                queue.push_tree(tree, ANIMATION_DURATION, blocker);
                return MoveResult::Done;
            }

            // now if the orientation matches

            // if we are not already in this group, we just move into it (up)
            if child_id != node_id {
                tree.move_node(&node_id, MoveBehavior::ToParent(&parent))
                    .unwrap();
                tree.make_nth_sibling(
                    &node_id,
                    if direction == Direction::Left || direction == Direction::Up {
                        idx
                    } else {
                        idx + 1
                    },
                )
                .unwrap();
                tree.get_mut(&parent).unwrap().data_mut().add_window(idx);
                tree.get_mut(&og_parent)
                    .unwrap()
                    .data_mut()
                    .remove_window(og_idx);

                let blocker = TilingLayout::update_positions(&output, &mut tree, self.gaps);
                queue.push_tree(tree, ANIMATION_DURATION, blocker);
                return MoveResult::Done;
            }

            // we can maybe move inside the group, if we don't run out of elements
            if let Some(next_idx) = match (orientation, direction) {
                (Orientation::Horizontal, Direction::Down)
                | (Orientation::Vertical, Direction::Right)
                    if idx < (len - 1) =>
                {
                    Some(idx + 1)
                }
                (Orientation::Horizontal, Direction::Up)
                | (Orientation::Vertical, Direction::Left)
                    if idx > 0 =>
                {
                    Some(idx - 1)
                }
                _ => None,
            } {
                // if we can, we need to check the next element and move "into" it
                let next_child_id = tree
                    .children_ids(&parent)
                    .unwrap()
                    .nth(next_idx)
                    .unwrap()
                    .clone();

                let result = if tree.get(&next_child_id).unwrap().data().is_stack()
                    && tree.get(&node_id).unwrap().data().is_mapped(None)
                    && !tree.get(&node_id).unwrap().data().is_stack()
                    && len == 2
                {
                    let node = tree
                        .remove_node(node_id, RemoveBehavior::DropChildren)
                        .unwrap();

                    let stack_data = tree.get_mut(&next_child_id).unwrap().data_mut();
                    let mut mapped = match stack_data {
                        Data::Mapped { mapped, .. } => mapped.clone(),
                        _ => unreachable!(),
                    };
                    let stack = mapped.stack_ref_mut().unwrap();

                    let surface = match node.data() {
                        Data::Mapped { mapped, .. } => mapped.active_window(),
                        _ => unreachable!(),
                    };
                    stack.add_window(
                        surface,
                        match direction {
                            Direction::Right => Some(0),
                            _ => None,
                        },
                    );
                    tree.get_mut(&og_parent)
                        .unwrap()
                        .data_mut()
                        .remove_window(og_idx);

                    MoveResult::ShiftFocus(mapped.into())
                } else if tree.get(&next_child_id).unwrap().data().is_group() && len == 2 {
                    // if it is a group, we want to move into the group
                    tree.move_node(&node_id, MoveBehavior::ToParent(&next_child_id))
                        .unwrap();
                    let group_orientation = tree.get(&next_child_id).unwrap().data().orientation();
                    match (group_orientation, direction) {
                        (Orientation::Horizontal, Direction::Down)
                        | (Orientation::Vertical, Direction::Right) => {
                            tree.make_first_sibling(&node_id).unwrap();
                            tree.get_mut(&next_child_id)
                                .unwrap()
                                .data_mut()
                                .add_window(0);
                        }
                        (Orientation::Horizontal, Direction::Up)
                        | (Orientation::Vertical, Direction::Left) => {
                            tree.make_last_sibling(&node_id).unwrap();
                            let group = tree.get_mut(&next_child_id).unwrap().data_mut();
                            group.add_window(group.len());
                        }
                        _ => {
                            // we want the middle
                            let group_len = tree.get(&next_child_id).unwrap().data().len();
                            if group_len % 2 == 0 {
                                tree.make_nth_sibling(&node_id, group_len / 2).unwrap();
                                tree.get_mut(&next_child_id)
                                    .unwrap()
                                    .data_mut()
                                    .add_window(group_len / 2);
                            } else {
                                // we move again by making a new fork
                                let old_id = tree
                                    .children_ids(&next_child_id)
                                    .unwrap()
                                    .skip(group_len / 2)
                                    .next()
                                    .unwrap()
                                    .clone();
                                TilingLayout::new_group(
                                    &mut tree,
                                    &old_id,
                                    &node_id,
                                    !group_orientation,
                                )
                                .unwrap();
                                tree.make_nth_sibling(
                                    &node_id,
                                    if direction == Direction::Left || direction == Direction::Up {
                                        1
                                    } else {
                                        0
                                    },
                                )
                                .unwrap();
                            }
                        }
                    };
                    tree.get_mut(&og_parent)
                        .unwrap()
                        .data_mut()
                        .remove_window(og_idx);

                    MoveResult::Done
                } else if len == 2 && child_id == node_id {
                    // if we are just us two in the group, lets swap
                    tree.make_nth_sibling(&node_id, next_idx).unwrap();
                    // also swap sizes
                    tree.get_mut(&og_parent)
                        .unwrap()
                        .data_mut()
                        .swap_windows(idx, next_idx);

                    MoveResult::Done
                } else {
                    // else we make a new fork
                    TilingLayout::new_group(&mut tree, &next_child_id, &node_id, orientation)
                        .unwrap();
                    tree.make_nth_sibling(
                        &node_id,
                        if direction == Direction::Left || direction == Direction::Up {
                            1
                        } else {
                            0
                        },
                    )
                    .unwrap();
                    tree.get_mut(&og_parent)
                        .unwrap()
                        .data_mut()
                        .remove_window(og_idx);

                    MoveResult::Done
                };

                let blocker = TilingLayout::update_positions(&output, &mut tree, self.gaps);
                queue.push_tree(tree, ANIMATION_DURATION, blocker);
                return result;
            }

            // We have reached the end of our parent group, try to move out even higher.
            maybe_parent = tree.get(&parent).unwrap().parent().cloned();
            child_id = parent.clone();
        }

        match data {
            FocusedNodeData::Window(window) => MoveResult::MoveFurther(window.into()),
            FocusedNodeData::Group(focus_stack, alive) => MoveResult::MoveFurther(
                WindowGroup {
                    node: node_id,
                    output: output.downgrade(),
                    alive,
                    focus_stack,
                }
                .into(),
            ),
        }
    }

    pub fn next_focus<'a>(
        &mut self,
        direction: FocusDirection,
        seat: &Seat<State>,
        focus_stack: impl Iterator<Item = &'a CosmicMapped> + 'a,
    ) -> FocusResult {
        let output = seat.active_output();
        let tree = &self.queues.get(&output).unwrap().trees.back().unwrap().0;

        let Some(target) = seat.get_keyboard().unwrap().current_focus() else {
            return FocusResult::None;
        };
        let Some(focused) =
            TilingLayout::currently_focused_node(tree, &seat.active_output(), target).or_else(
                || {
                    TilingLayout::last_active_window(tree, focus_stack)
                        .map(|(id, mapped)| (id, FocusedNodeData::Window(mapped)))
                },
            )
        else {
            return FocusResult::None;
        };

        let (last_node_id, data) = focused;

        // stacks may handle focus internally
        if let FocusedNodeData::Window(window) = data.clone() {
            if window.handle_focus(direction) {
                return FocusResult::Handled;
            }
        }

        if direction == FocusDirection::In {
            if let FocusedNodeData::Group(mut stack, _) = data.clone() {
                let maybe_id = stack.pop().unwrap();
                let id = if tree
                    .children_ids(&last_node_id)
                    .unwrap()
                    .any(|id| id == &maybe_id)
                {
                    Some(maybe_id)
                } else {
                    tree.children_ids(&last_node_id).unwrap().next().cloned()
                };

                if let Some(id) = id {
                    return match tree.get(&id).unwrap().data() {
                        Data::Mapped { mapped, .. } => {
                            if mapped.is_stack() {
                                mapped.stack_ref().unwrap().focus_stack();
                            }
                            FocusResult::Some(mapped.clone().into())
                        }
                        Data::Group { alive, .. } => FocusResult::Some(
                            WindowGroup {
                                node: id,
                                output: output.downgrade(),
                                alive: Arc::downgrade(alive),
                                focus_stack: stack,
                            }
                            .into(),
                        ),
                        Data::Placeholder { .. } => return FocusResult::None,
                    };
                }
            }
        }

        let mut node_id = last_node_id.clone();
        while let Some(group) = tree.get(&node_id).unwrap().parent() {
            let child = node_id.clone();
            let group_data = tree.get(&group).unwrap().data();
            let main_orientation = group_data.orientation();
            assert!(group_data.is_group());

            if direction == FocusDirection::Out {
                return FocusResult::Some(
                    WindowGroup {
                        node: group.clone(),
                        output: output.downgrade(),
                        alive: match group_data {
                            &Data::Group { ref alive, .. } => Arc::downgrade(alive),
                            _ => unreachable!(),
                        },
                        focus_stack: match data {
                            FocusedNodeData::Group(mut stack, _) => {
                                stack.push(child);
                                stack
                            }
                            _ => vec![child],
                        },
                    }
                    .into(),
                );
            }

            // which child are we?
            let idx = tree
                .children_ids(&group)
                .unwrap()
                .position(|id| id == &child)
                .unwrap();
            let len = group_data.len();

            let focus_subtree = match (main_orientation, direction) {
                (Orientation::Horizontal, FocusDirection::Down)
                | (Orientation::Vertical, FocusDirection::Right)
                    if idx < (len - 1) =>
                {
                    tree.children_ids(&group).unwrap().skip(idx + 1).next()
                }
                (Orientation::Horizontal, FocusDirection::Up)
                | (Orientation::Vertical, FocusDirection::Left)
                    if idx > 0 =>
                {
                    tree.children_ids(&group).unwrap().skip(idx - 1).next()
                }
                _ => None, // continue iterating
            };

            if focus_subtree.is_some() {
                let mut node_id = focus_subtree;
                while node_id.is_some() {
                    match tree.get(node_id.unwrap()).unwrap().data() {
                        Data::Group { orientation, .. } if orientation == &main_orientation => {
                            // if the group is layed out in the direction we care about,
                            // we can just use the first or last element (depending on the direction)
                            match direction {
                                FocusDirection::Down | FocusDirection::Right => {
                                    node_id = tree
                                        .children_ids(node_id.as_ref().unwrap())
                                        .unwrap()
                                        .next();
                                }
                                FocusDirection::Up | FocusDirection::Left => {
                                    node_id = tree
                                        .children_ids(node_id.as_ref().unwrap())
                                        .unwrap()
                                        .last();
                                }
                                _ => unreachable!(),
                            }
                        }
                        Data::Group { .. } => {
                            let center = {
                                let geo = tree.get(&last_node_id).unwrap().data().geometry();
                                let mut point = geo.loc;
                                match direction {
                                    FocusDirection::Down => {
                                        point += Point::from((geo.size.w / 2 - 1, geo.size.h))
                                    }
                                    FocusDirection::Up => point.x += geo.size.w / 2 - 1,
                                    FocusDirection::Left => point.y += geo.size.h / 2 - 1,
                                    FocusDirection::Right => {
                                        point += Point::from((geo.size.w, geo.size.h / 2 - 1))
                                    }
                                    _ => unreachable!(),
                                };
                                point.to_f64()
                            };

                            let distance = |candidate: &&NodeId| -> f64 {
                                let geo = tree.get(candidate).unwrap().data().geometry();
                                let mut point = geo.loc;
                                match direction {
                                    FocusDirection::Up => {
                                        point += Point::from((geo.size.w / 2, geo.size.h))
                                    }
                                    FocusDirection::Down => point.x += geo.size.w,
                                    FocusDirection::Right => point.y += geo.size.h / 2,
                                    FocusDirection::Left => {
                                        point += Point::from((geo.size.w, geo.size.h / 2))
                                    }
                                    _ => unreachable!(),
                                };
                                let point = point.to_f64();
                                ((point.x - center.x).powi(2) + (point.y - center.y).powi(2)).sqrt()
                            };

                            node_id = tree
                                .children_ids(node_id.as_ref().unwrap())
                                .unwrap()
                                .min_by(|node1, node2| {
                                    distance(node1).abs().total_cmp(&distance(node2).abs())
                                });
                        }
                        Data::Mapped { mapped, .. } => {
                            return FocusResult::Some(mapped.clone().into());
                        }
                        Data::Placeholder { .. } => return FocusResult::None,
                    }
                }
            } else {
                node_id = group.clone();
            }
        }

        FocusResult::None
    }

    pub fn update_orientation<'a>(
        &mut self,
        new_orientation: Option<Orientation>,
        seat: &Seat<State>,
    ) {
        let output = seat.active_output();
        let Some(queue) = self.queues.get_mut(&output) else {
            return;
        };
        let mut tree = queue.trees.back().unwrap().0.copy_clone();

        let Some(target) = seat.get_keyboard().unwrap().current_focus() else {
            return;
        };
        if let Some((last_active, _)) =
            TilingLayout::currently_focused_node(&tree, &seat.active_output(), target)
        {
            if let Some(group) = tree.get(&last_active).unwrap().parent().cloned() {
                if let &mut Data::Group {
                    ref mut orientation,
                    ref mut sizes,
                    ref last_geometry,
                    ..
                } = tree.get_mut(&group).unwrap().data_mut()
                {
                    let previous_length = match orientation {
                        Orientation::Horizontal => last_geometry.size.h,
                        Orientation::Vertical => last_geometry.size.w,
                    };
                    let new_orientation = new_orientation.unwrap_or(!*orientation);
                    let new_length = match new_orientation {
                        Orientation::Horizontal => last_geometry.size.h,
                        Orientation::Vertical => last_geometry.size.w,
                    };

                    sizes.iter_mut().for_each(|len| {
                        *len = (((*len as f64) / (previous_length as f64)) * (new_length as f64))
                            .round() as i32;
                    });
                    let sum: i32 = sizes.iter().sum();
                    if sum < new_length {
                        *sizes.last_mut().unwrap() += new_length - sum;
                    }

                    *orientation = new_orientation;

                    let blocker = TilingLayout::update_positions(&output, &mut tree, self.gaps);
                    queue.push_tree(tree, ANIMATION_DURATION, blocker);
                }
            }
        }
    }

    pub fn toggle_stacking<'a>(&mut self, seat: &Seat<State>, mut focus_stack: FocusStackMut) {
        let output = seat.active_output();
        let Some(queue) = self.queues.get_mut(&output) else {
            return;
        };
        let mut tree = queue.trees.back().unwrap().0.copy_clone();

        let Some(target) = seat.get_keyboard().unwrap().current_focus() else {
            return;
        };
        if let Some((last_active, last_active_data)) =
            TilingLayout::currently_focused_node(&tree, &seat.active_output(), target)
        {
            match last_active_data {
                FocusedNodeData::Window(mapped) => {
                    if mapped.is_window() {
                        // if it is just a window
                        match tree.get_mut(&last_active).unwrap().data_mut() {
                            Data::Mapped { mapped, .. } => {
                                mapped.convert_to_stack(std::iter::once((&output, mapped.bbox())));
                                focus_stack.append(&mapped);
                            }
                            _ => unreachable!(),
                        };
                    } else {
                        // if we have a stack
                        let mut surfaces = mapped.windows().map(|(s, _)| s);
                        let first = surfaces.next().expect("Stack without a window?");

                        let handle = match tree.get_mut(&last_active).unwrap().data_mut() {
                            Data::Mapped { mapped, .. } => {
                                let handle = mapped.loop_handle();
                                mapped.convert_to_surface(
                                    first,
                                    std::iter::once((&output, mapped.bbox())),
                                );
                                focus_stack.append(&mapped);
                                handle
                            }
                            _ => unreachable!(),
                        };

                        // map the rest
                        for other in surfaces {
                            other.try_force_undecorated(false);
                            other.set_tiled(false);
                            let window =
                                CosmicMapped::from(CosmicWindow::new(other, handle.clone()));
                            window.output_enter(&output, window.bbox());
                            window.set_bounds(output.geometry().size);

                            TilingLayout::map_to_tree(
                                &mut tree,
                                window,
                                &output,
                                Some(focus_stack.iter()),
                                None,
                            )
                        }

                        // TODO: Focus the new group
                    }
                }
                FocusedNodeData::Group(_, _) => {
                    let mut handle = None;
                    let surfaces = tree
                        .traverse_pre_order(&last_active)
                        .unwrap()
                        .flat_map(|node| match node.data() {
                            Data::Mapped { mapped, .. } => {
                                if handle.is_none() {
                                    handle = Some(mapped.loop_handle());
                                }
                                Some(mapped.windows().map(|(s, _)| s))
                            }
                            _ => None,
                        })
                        .flatten()
                        .collect::<Vec<_>>();

                    if surfaces.is_empty() {
                        return;
                    }
                    let handle = handle.unwrap();
                    let stack = CosmicStack::new(surfaces.into_iter(), handle);

                    for child in tree
                        .children_ids(&last_active)
                        .unwrap()
                        .cloned()
                        .collect::<Vec<_>>()
                        .into_iter()
                    {
                        tree.remove_node(child, RemoveBehavior::DropChildren)
                            .unwrap();
                    }
                    let data = tree.get_mut(&last_active).unwrap().data_mut();

                    let geo = *data.geometry();
                    stack.set_geometry(geo);
                    stack.output_enter(&output, stack.bbox());
                    stack.set_activate(true);
                    stack.active().send_configure();
                    stack.refresh();

                    let mapped = CosmicMapped::from(stack);
                    *mapped.last_geometry.lock().unwrap() = Some(geo);
                    *mapped.tiling_node_id.lock().unwrap() = Some(last_active);
                    focus_stack.append(&mapped);
                    *data = Data::Mapped {
                        mapped,
                        last_geometry: geo,
                    };
                }
            }

            let blocker = TilingLayout::update_positions(&output, &mut tree, self.gaps);
            queue.push_tree(tree, ANIMATION_DURATION, blocker);
        }
    }

    pub fn recalculate(&mut self, output: &Output) {
        let Some(queue) = self.queues.get_mut(output) else {
            return;
        };
        let mut tree = queue.trees.back().unwrap().0.copy_clone();
        let blocker = TilingLayout::update_positions(&output, &mut tree, self.gaps);
        queue.push_tree(tree, ANIMATION_DURATION, blocker);
    }

    pub fn refresh(&mut self) {
        #[cfg(feature = "debug")]
        puffin::profile_function!();

        let dead_windows = self
            .mapped()
            .map(|(_, w, _)| w.clone())
            .filter(|w| !w.alive())
            .collect::<Vec<_>>();
        for dead_window in dead_windows.iter() {
            self.unmap_window_internal(dead_window);
        }

        for (_, mapped, _) in self.mapped() {
            mapped.refresh();
        }
    }

    pub fn animations_going(&self) -> bool {
        self.queues
            .values()
            .any(|queue| queue.animation_start.is_some())
    }

    pub fn update_animation_state(&mut self) -> HashMap<ClientId, Client> {
        let mut clients = HashMap::new();
        for blocker in self.pending_blockers.drain(..) {
            clients.extend(blocker.signal_ready());
        }

        for queue in self.queues.values_mut() {
            if let Some(start) = queue.animation_start {
                let duration_since_start = Instant::now().duration_since(start);
                if duration_since_start
                    >= queue
                        .trees
                        .get(1)
                        .expect("Animation going without second tree?")
                        .1
                {
                    let _ = queue.animation_start.take();
                    let _ = queue.trees.pop_front();
                    let _ = queue.trees.front_mut().unwrap().2.take();
                } else {
                    continue;
                }
            }

            let ready_trees = queue
                .trees
                .iter()
                .skip(1)
                .take_while(|(_, _, blocker)| {
                    blocker
                        .as_ref()
                        .map(|blocker| blocker.is_ready() && blocker.is_signaled())
                        .unwrap_or(true)
                })
                .count();

            // merge
            let other_duration = if ready_trees > 1 {
                queue
                    .trees
                    .drain(1..ready_trees)
                    .fold(None, |res, (_, duration, blocker)| {
                        if let Some(blocker) = blocker {
                            clients.extend(blocker.signal_ready());
                        }
                        Some(
                            res.map(|old_duration: Duration| old_duration.max(duration))
                                .unwrap_or(duration),
                        )
                    })
            } else {
                None
            };

            // start
            if ready_trees > 0 {
                let (_, duration, blocker) = queue.trees.get_mut(1).unwrap();
                *duration = other_duration
                    .map(|other| other.max(*duration))
                    .unwrap_or(*duration);
                if let Some(blocker) = blocker {
                    clients.extend(blocker.signal_ready());
                }
                queue.animation_start = Some(Instant::now());
            }
        }

        clients
    }

    pub fn possible_resizes(tree: &Tree<Data>, mut node_id: NodeId) -> ResizeEdge {
        let mut edges = ResizeEdge::empty();

        while let Some(group_id) = tree.get(&node_id).unwrap().parent().cloned() {
            let orientation = tree.get(&group_id).unwrap().data().orientation();

            let node_idx = tree
                .children_ids(&group_id)
                .unwrap()
                .position(|id| id == &node_id)
                .unwrap();
            let total = tree.children_ids(&group_id).unwrap().count();
            if orientation == Orientation::Vertical {
                if node_idx > 0 {
                    edges.insert(ResizeEdge::LEFT);
                }
                if node_idx < total - 1 {
                    edges.insert(ResizeEdge::RIGHT);
                }
            } else {
                if node_idx > 0 {
                    edges.insert(ResizeEdge::TOP);
                }
                if node_idx < total - 1 {
                    edges.insert(ResizeEdge::BOTTOM);
                }
            }

            node_id = group_id;
        }

        edges
    }

    pub fn resize(
        &mut self,
        focused: &KeyboardFocusTarget,
        direction: ResizeDirection,
        edges: ResizeEdge,
        amount: i32,
    ) -> bool {
        let Some((output, mut node_id)) = self.queues.iter().find_map(|(output, queue)| {
            let tree = &queue.trees.back().unwrap().0;
            let root_id = tree.root_node_id()?;
            let id =
                match TilingLayout::currently_focused_node(tree, &output.output, focused.clone()) {
                    Some((_id, FocusedNodeData::Window(mapped))) =>
                    // we need to make sure the id belongs to this tree..
                    {
                        tree.traverse_pre_order_ids(root_id)
                            .unwrap()
                            .find(|id| tree.get(id).unwrap().data().is_mapped(Some(&mapped)))
                    }
                    Some((id, FocusedNodeData::Group(_, _))) => Some(id), // in this case the output was already matched, so the id is to be trusted
                    _ => None,
                };
            id.map(|id| (output.output.clone(), id))
        }) else {
            return false;
        };

        let queue = self.queues.get_mut(&output).unwrap();
        let mut tree = queue.trees.back().unwrap().0.copy_clone();

        while let Some(group_id) = tree.get(&node_id).unwrap().parent().cloned() {
            let orientation = tree.get(&group_id).unwrap().data().orientation();
            if !((orientation == Orientation::Vertical
                && (edges.contains(ResizeEdge::LEFT) || edges.contains(ResizeEdge::RIGHT)))
                || (orientation == Orientation::Horizontal
                    && (edges.contains(ResizeEdge::TOP) || edges.contains(ResizeEdge::BOTTOM))))
            {
                node_id = group_id.clone();
                continue;
            }

            let node_idx = tree
                .children_ids(&group_id)
                .unwrap()
                .position(|id| id == &node_id)
                .unwrap();
            let Some(other_idx) = (match edges {
                x if x.intersects(ResizeEdge::TOP_LEFT) => node_idx.checked_sub(1),
                _ => {
                    if tree.children_ids(&group_id).unwrap().count() - 1 > node_idx {
                        Some(node_idx + 1)
                    } else {
                        None
                    }
                }
            }) else {
                node_id = group_id.clone();
                continue;
            };

            let data = tree.get_mut(&group_id).unwrap().data_mut();

            match data {
                Data::Group { sizes, .. } => {
                    let (shrink_idx, grow_idx) = if direction == ResizeDirection::Inwards {
                        (node_idx, other_idx)
                    } else {
                        (other_idx, node_idx)
                    };

                    if sizes[shrink_idx] + sizes[grow_idx]
                        < match orientation {
                            Orientation::Vertical => 720,
                            Orientation::Horizontal => 480,
                        }
                    {
                        return true;
                    };

                    let old_size = sizes[shrink_idx];
                    sizes[shrink_idx] =
                        (old_size - amount).max(if orientation == Orientation::Vertical {
                            360
                        } else {
                            240
                        });
                    let diff = old_size - sizes[shrink_idx];
                    sizes[grow_idx] += diff;
                }
                _ => unreachable!(),
            }
            let blocker = TilingLayout::update_positions(&output, &mut tree, self.gaps);
            queue.push_tree(tree, Duration::ZERO, blocker);

            return true;
        }

        true
    }

    pub fn stacking_indicator(&self) -> Option<Rectangle<i32, Logical>> {
        if let Some(TargetZone::WindowStack(_, geo)) =
            self.last_overview_hover.as_ref().map(|(_, zone)| zone)
        {
            Some(*geo)
        } else {
            None
        }
    }

    pub fn cleanup_drag(&mut self, output: &Output) {
        let mut queue = self.queues.get_mut(output);
        let mut owned_tree = None;
        let mut tree = if let Some(queue) = queue.as_mut() {
            owned_tree = queue.trees.back().map(|x| x.0.copy_clone());
            owned_tree.as_mut()
        } else {
            self.standby_tree.as_mut()
        }
        .unwrap();

        if let Some(root) = tree.root_node_id() {
            for id in tree
                .traverse_pre_order_ids(root)
                .unwrap()
                .collect::<Vec<_>>()
                .into_iter()
            {
                match tree.get_mut(&id).map(|node| node.data_mut()) {
                    Ok(Data::Placeholder { .. }) => TilingLayout::unmap_internal(&mut tree, &id),
                    Ok(Data::Group { pill_indicator, .. }) if pill_indicator.is_some() => {
                        pill_indicator.take();
                    }
                    _ => {}
                }
            }

            if let Some(mut tree) = owned_tree {
                let blocker = TilingLayout::update_positions(output, &mut tree, self.gaps);
                queue.unwrap().push_tree(tree, ANIMATION_DURATION, blocker);
            }
        }
    }

    pub fn drop_window(
        &mut self,
        window: CosmicMapped,
        output: &Output,
        _cursor_pos: Point<f64, Logical>,
    ) -> (CosmicMapped, Point<i32, Logical>) {
        let mut queue = if self.queues.contains_key(output) {
            self.queues.get_mut(output)
        } else {
            self.queues.values_mut().next()
        };
        let mut owned_tree = None;
        let mut tree = if let Some(queue) = queue.as_mut() {
            owned_tree = queue.trees.back().map(|x| x.0.copy_clone());
            owned_tree.as_mut()
        } else {
            self.standby_tree.as_mut()
        }
        .unwrap();

        window.output_enter(&output, window.bbox());
        window.set_bounds(output.geometry().size);

        let mapped = match self.last_overview_hover.as_ref().map(|x| &x.1) {
            Some(TargetZone::GroupEdge(group_id, direction)) if tree.get(&group_id).is_ok() => {
                let new_id = tree
                    .insert(
                        Node::new(Data::Mapped {
                            mapped: window.clone(),
                            last_geometry: Rectangle::from_loc_and_size((0, 0), (100, 100)),
                        }),
                        InsertBehavior::UnderNode(group_id),
                    )
                    .unwrap();

                let orientation = if matches!(direction, Direction::Left | Direction::Right) {
                    Orientation::Vertical
                } else {
                    Orientation::Horizontal
                };
                if tree.get(group_id).unwrap().data().orientation() != orientation {
                    TilingLayout::new_group(&mut tree, &group_id, &new_id, orientation).unwrap();
                } else {
                    let data = tree.get_mut(group_id).unwrap().data_mut();
                    let len = data.len();
                    data.add_window(if matches!(direction, Direction::Left | Direction::Up) {
                        0
                    } else {
                        len
                    });
                }
                if matches!(direction, Direction::Left | Direction::Up) {
                    tree.make_first_sibling(&new_id).unwrap();
                } else {
                    tree.make_last_sibling(&new_id).unwrap();
                }
                *window.tiling_node_id.lock().unwrap() = Some(new_id);
                window
            }
            Some(TargetZone::GroupInterior(group_id, idx)) if tree.get(&group_id).is_ok() => {
                let new_id = tree
                    .insert(
                        Node::new(Data::Mapped {
                            mapped: window.clone(),
                            last_geometry: Rectangle::from_loc_and_size((0, 0), (100, 100)),
                        }),
                        InsertBehavior::UnderNode(group_id),
                    )
                    .unwrap();
                let idx = {
                    let data = tree.get_mut(group_id).unwrap().data_mut();
                    let bound_idx = data.len().min(*idx + 1);
                    data.add_window(bound_idx);
                    bound_idx
                };
                tree.make_nth_sibling(&new_id, dbg!(idx)).unwrap();
                *window.tiling_node_id.lock().unwrap() = Some(new_id);
                window
            }
            Some(TargetZone::InitialPlaceholder(node_id)) if tree.get(&node_id).is_ok() => {
                let data = tree.get_mut(&node_id).unwrap().data_mut();
                let geo = data.geometry().clone();

                *data = Data::Mapped {
                    mapped: window.clone(),
                    last_geometry: geo,
                };
                *window.tiling_node_id.lock().unwrap() = Some(node_id.clone());
                window
            }
            Some(TargetZone::WindowSplit(window_id, direction)) if tree.get(&window_id).is_ok() => {
                let new_id = tree
                    .insert(
                        Node::new(Data::Mapped {
                            mapped: window.clone(),
                            last_geometry: Rectangle::from_loc_and_size((0, 0), (100, 100)),
                        }),
                        InsertBehavior::UnderNode(&window_id),
                    )
                    .unwrap();
                let orientation = if matches!(direction, Direction::Left | Direction::Right) {
                    Orientation::Vertical
                } else {
                    Orientation::Horizontal
                };
                TilingLayout::new_group(&mut tree, &window_id, &new_id, orientation).unwrap();
                if matches!(direction, Direction::Left | Direction::Up) {
                    tree.make_first_sibling(&new_id).unwrap();
                }
                *window.tiling_node_id.lock().unwrap() = Some(new_id.clone());
                window
            }
            Some(TargetZone::WindowStack(window_id, _)) if tree.get(&window_id).is_ok() => {
                match tree.get_mut(window_id).unwrap().data_mut() {
                    Data::Mapped { mapped, .. } => {
                        mapped.convert_to_stack(std::iter::once((output, mapped.bbox())));
                        let Some(stack) = mapped.stack_ref_mut() else {
                            unreachable!()
                        };
                        for surface in window.windows().map(|s| s.0) {
                            stack.add_window(surface, None);
                        }
                        mapped.clone()
                    }
                    _ => unreachable!(),
                }
            }
            _ => {
                TilingLayout::map_to_tree(
                    &mut tree,
                    window.clone(),
                    output,
                    Option::<std::iter::Empty<_>>::None,
                    None,
                );
                window
            }
        };

        if let Some(root) = tree.root_node_id() {
            for id in tree
                .traverse_pre_order_ids(root)
                .unwrap()
                .collect::<Vec<_>>()
                .into_iter()
            {
                match tree.get_mut(&id).map(|node| node.data_mut()) {
                    Ok(Data::Placeholder { .. }) => TilingLayout::unmap_internal(&mut tree, &id),
                    Ok(Data::Group { pill_indicator, .. }) if pill_indicator.is_some() => {
                        pill_indicator.take();
                    }
                    _ => {}
                }
            }
        }

        if let Some(mut tree) = owned_tree {
            let blocker = TilingLayout::update_positions(output, &mut tree, self.gaps);
            queue.unwrap().push_tree(tree, ANIMATION_DURATION, blocker);
        }

        let location = output.geometry().loc + self.element_geometry(&mapped).unwrap().loc;
        (mapped, location)
    }

    fn last_active_window<'a>(
        tree: &Tree<Data>,
        mut focus_stack: impl Iterator<Item = &'a CosmicMapped>,
    ) -> Option<(NodeId, CosmicMapped)> {
        focus_stack
            .find_map(|mapped| tree.root_node_id()
                .and_then(|root| tree.traverse_pre_order_ids(root).unwrap()
                    .find(|id| matches!(tree.get(id).map(|n| n.data()), Ok(Data::Mapped { mapped: m, .. }) if m == mapped))
                ).map(|id| (id, mapped.clone()))
            )
    }

    fn currently_focused_node(
        tree: &Tree<Data>,
        output: &Output,
        mut target: KeyboardFocusTarget,
    ) -> Option<(NodeId, FocusedNodeData)> {
        // if the focus is currently on a popup, treat it's toplevel as the target
        if let KeyboardFocusTarget::Popup(popup) = target {
            let toplevel_surface = match popup {
                PopupKind::Xdg(xdg) => get_popup_toplevel(&xdg),
            }?;
            let root_id = tree.root_node_id()?;
            let node =
                tree.traverse_pre_order(root_id)
                    .unwrap()
                    .find(|node| match node.data() {
                        Data::Mapped { mapped, .. } => mapped
                            .windows()
                            .any(|(w, _)| w.wl_surface().as_ref() == Some(&toplevel_surface)),
                        _ => false,
                    })?;

            target = KeyboardFocusTarget::Element(match node.data() {
                Data::Mapped { mapped, .. } => mapped.clone(),
                _ => unreachable!(),
            });
        }

        match target {
            KeyboardFocusTarget::Element(mapped) => {
                let node_id = mapped.tiling_node_id.lock().unwrap().clone()?;
                let node = tree.get(&node_id).ok()?;
                let data = node.data();
                if data.is_mapped(Some(&mapped)) {
                    return Some((node_id, FocusedNodeData::Window(mapped)));
                }
            }
            KeyboardFocusTarget::Group(window_group) => {
                if window_group.output == *output {
                    let node = tree.get(&window_group.node).ok()?;
                    if node.data().is_group() {
                        return Some((
                            window_group.node,
                            FocusedNodeData::Group(window_group.focus_stack, window_group.alive),
                        ));
                    }
                }
            }
            _ => {}
        };

        None
    }

    fn new_group(
        tree: &mut Tree<Data>,
        old_id: &NodeId,
        new_id: &NodeId,
        orientation: Orientation,
    ) -> Result<NodeId, NodeIdError> {
        let new_group = Node::new(Data::new_group(
            orientation,
            Rectangle::from_loc_and_size((0, 0), (100, 100)),
        ));
        let old = tree.get(old_id)?;
        let parent_id = old.parent().cloned();
        let pos = parent_id.as_ref().and_then(|parent_id| {
            tree.children_ids(parent_id)
                .unwrap()
                .position(|id| id == old_id)
        });

        let group_id = tree
            .insert(
                new_group,
                if let Some(parent) = parent_id.as_ref() {
                    InsertBehavior::UnderNode(parent)
                } else {
                    InsertBehavior::AsRoot
                },
            )
            .unwrap();

        tree.move_node(old_id, MoveBehavior::ToParent(&group_id))
            .unwrap();
        // keep position
        if let Some(old_pos) = pos {
            tree.make_nth_sibling(&group_id, old_pos).unwrap();
        }
        tree.move_node(new_id, MoveBehavior::ToParent(&group_id))
            .unwrap();

        Ok(group_id)
    }

    fn has_adjacent_node(tree: &Tree<Data>, node: &NodeId, direction: Direction) -> bool {
        let mut search_node = node;
        match tree.ancestor_ids(node) {
            Ok(mut iter) => iter.any(|parent_id| {
                let parent = tree.get(parent_id).unwrap();
                let children = parent.children();
                let idx = children.iter().position(|id| id == search_node).unwrap();
                search_node = parent_id;

                match direction {
                    Direction::Up => {
                        parent.data().orientation() == Orientation::Horizontal && idx > 0
                    }
                    Direction::Down => {
                        parent.data().orientation() == Orientation::Horizontal
                            && idx < (children.len() - 1)
                    }
                    Direction::Left => {
                        parent.data().orientation() == Orientation::Vertical && idx > 0
                    }
                    Direction::Right => {
                        parent.data().orientation() == Orientation::Vertical
                            && idx < (children.len() - 1)
                    }
                }
            }),
            Err(_) => false,
        }
    }

    fn has_sibling_node(tree: &Tree<Data>, node: &NodeId, direction: Direction) -> bool {
        match tree.get(node).ok().and_then(|node| node.parent()) {
            Some(parent_id) => {
                let parent = tree.get(parent_id).unwrap();
                let children = parent.children();
                let idx = children.iter().position(|id| id == node).unwrap();

                match direction {
                    Direction::Up => {
                        parent.data().orientation() == Orientation::Horizontal && idx > 0
                    }
                    Direction::Down => {
                        parent.data().orientation() == Orientation::Horizontal
                            && idx < (children.len() - 1)
                    }
                    Direction::Left => {
                        parent.data().orientation() == Orientation::Vertical && idx > 0
                    }
                    Direction::Right => {
                        parent.data().orientation() == Orientation::Vertical
                            && idx < (children.len() - 1)
                    }
                }
            }
            None => false,
        }
    }

    fn update_positions(
        output: &Output,
        tree: &mut Tree<Data>,
        gaps: (i32, i32),
    ) -> Option<TilingBlocker> {
        #[cfg(feature = "debug")]
        puffin::profile_function!();

        if let Some(root_id) = tree.root_node_id() {
            let mut configures = Vec::new();

            let (outer, inner) = gaps;
            let mut geo = layer_map_for_output(&output).non_exclusive_zone();
            geo.loc.x += outer;
            geo.loc.y += outer;
            geo.size.w -= outer * 2;
            geo.size.h -= outer * 2;
            let mut stack = vec![geo];

            for node_id in tree
                .traverse_pre_order_ids(root_id)
                .unwrap()
                .collect::<Vec<_>>()
                .into_iter()
            {
                let node = tree.get_mut(&node_id).unwrap();
                let data = node.data_mut();

                // flatten tree
                if data.is_group() && data.len() == 1 {
                    // RemoveBehavior::LiftChildren sadly does not what we want: lifting them into the same place.
                    // So we need to fix that manually..
                    let idx = node.parent().cloned().map(|parent_id| {
                        tree.children_ids(&parent_id)
                            .unwrap()
                            .position(|id| id == &node_id)
                            .unwrap()
                    });
                    let child_id = tree
                        .children_ids(&node_id)
                        .unwrap()
                        .cloned()
                        .next()
                        .unwrap();
                    tree.remove_node(node_id, RemoveBehavior::LiftChildren)
                        .unwrap();
                    if let Some(idx) = idx {
                        tree.make_nth_sibling(&child_id, idx).unwrap();
                    } else {
                        // additionally `RemoveBehavior::LiftChildren` doesn't work, when removing the root-node,
                        // even with just one child. *sigh*
                        tree.move_node(&child_id, MoveBehavior::ToRoot).unwrap();
                    }

                    continue;
                }

                if let Some(mut geo) = stack.pop() {
                    let node = tree.get(&node_id).unwrap();
                    let data = node.data();
                    if data.is_mapped(None) {
                        let gap = (
                            (
                                if TilingLayout::has_adjacent_node(tree, &node_id, Direction::Left)
                                {
                                    inner / 2
                                } else {
                                    inner
                                },
                                if TilingLayout::has_adjacent_node(tree, &node_id, Direction::Up) {
                                    inner / 2
                                } else {
                                    inner
                                },
                            ),
                            (
                                if TilingLayout::has_adjacent_node(tree, &node_id, Direction::Right)
                                {
                                    inner / 2
                                } else {
                                    inner
                                },
                                if TilingLayout::has_adjacent_node(tree, &node_id, Direction::Down)
                                {
                                    inner / 2
                                } else {
                                    inner
                                },
                            ),
                        );
                        geo.loc += gap.0.into();
                        geo.size -= gap.0.into();
                        geo.size -= gap.1.into();
                    }

                    let node = tree.get_mut(&node_id).unwrap();
                    let data = node.data_mut();
                    data.update_geometry(geo);

                    match data {
                        Data::Group {
                            orientation, sizes, ..
                        } => match orientation {
                            Orientation::Horizontal => {
                                let mut previous: i32 = sizes.iter().sum();
                                for size in sizes.iter().rev() {
                                    previous -= *size;
                                    stack.push(Rectangle::from_loc_and_size(
                                        (geo.loc.x, geo.loc.y + previous),
                                        (geo.size.w, *size),
                                    ));
                                }
                            }
                            Orientation::Vertical => {
                                let mut previous: i32 = sizes.iter().sum();
                                for size in sizes.iter().rev() {
                                    previous -= *size;
                                    stack.push(Rectangle::from_loc_and_size(
                                        (geo.loc.x + previous, geo.loc.y),
                                        (*size, geo.size.h),
                                    ));
                                }
                            }
                        },
                        Data::Mapped { mapped, .. } => {
                            if !(mapped.is_fullscreen(true) || mapped.is_maximized(true)) {
                                mapped.set_tiled(true);
                                let internal_geometry = Rectangle::from_loc_and_size(
                                    geo.loc + output.geometry().loc,
                                    geo.size,
                                );
                                if mapped.geometry() != internal_geometry {
                                    mapped.set_geometry(internal_geometry);
                                    if let Some(serial) = mapped.configure() {
                                        configures.push((mapped.active_window(), serial));
                                    }
                                }
                            }
                        }
                        Data::Placeholder { .. } => {}
                    }
                }
            }

            if !configures.is_empty() {
                let blocker = TilingBlocker::new(configures);
                for (surface, _) in &blocker.necessary_acks {
                    if let Some(surface) = surface.wl_surface() {
                        add_blocker(&surface, blocker.clone());
                    }
                }
                return Some(blocker);
            }
        }

        None
    }

    pub fn element_under(
        &mut self,
        location: Point<f64, Logical>,
        overview: OverviewMode,
    ) -> Option<(PointerFocusTarget, Point<i32, Logical>)> {
        let last_overview_hover = &mut self.last_overview_hover;
        let placeholder_id = &self.placeholder_id;
        let gaps = &self.gaps;
        self.queues.iter_mut().find_map(|(output_data, queue)| {
            let tree = &queue.trees.back().unwrap().0;
            let root = tree.root_node_id()?;
            let location = (location - output_data.location.to_f64()).to_i32_round();

            {
                let output_geo =
                    Rectangle::from_loc_and_size((0, 0), output_data.output.geometry().size);
                if !output_geo.contains(location) {
                    return None;
                }
            }

            if !matches!(overview, OverviewMode::Started(_, _)) {
                last_overview_hover.take();
            }

            if matches!(overview, OverviewMode::None) {
                let mut result = None;
                let mut lookup = Some(root.clone());
                while let Some(node) = lookup {
                    let data = tree.get(&node).unwrap().data();
                    if data.geometry().contains(location) {
                        result = Some(node.clone());
                    }

                    lookup = None;
                    if result.is_some() && data.is_group() {
                        for child_id in tree.children_ids(&node).unwrap() {
                            if tree
                                .get(child_id)
                                .unwrap()
                                .data()
                                .geometry()
                                .contains(location)
                            {
                                lookup = Some(child_id.clone());
                                break;
                            }
                        }
                    }
                }

                match result.map(|id| (id.clone(), tree.get(&id).unwrap().data().clone())) {
                    Some((
                        _,
                        Data::Mapped {
                            mapped,
                            last_geometry,
                        },
                    )) => {
                        let test_point = location.to_f64() - last_geometry.loc.to_f64()
                            + mapped.geometry().loc.to_f64();
                        mapped.is_in_input_region(&test_point).then(|| {
                            (
                                mapped.clone().into(),
                                last_geometry.loc - output_data.location - mapped.geometry().loc,
                            )
                        })
                    }
                    Some((
                        id,
                        Data::Group {
                            orientation,
                            last_geometry,
                            ..
                        },
                    )) => {
                        let idx = tree
                            .children(&id)
                            .unwrap()
                            .position(|node| {
                                let data = node.data();
                                match orientation {
                                    Orientation::Vertical => location.x < data.geometry().loc.x,
                                    Orientation::Horizontal => location.y < data.geometry().loc.y,
                                }
                            })
                            .and_then(|x| x.checked_sub(1))?;
                        Some((
                            ResizeForkTarget {
                                node: id.clone(),
                                output: output_data.output.downgrade(),
                                left_up_idx: idx,
                                orientation,
                            }
                            .into(),
                            last_geometry.loc - output_data.location
                                + tree
                                    .children(&id)
                                    .unwrap()
                                    .skip(idx)
                                    .next()
                                    .map(|node| {
                                        let geo = node.data().geometry();
                                        geo.loc + geo.size
                                    })
                                    .unwrap(),
                        ))
                    }
                    _ => None,
                }
            } else if matches!(overview, OverviewMode::Started(Trigger::Pointer(_), _)) {
                let non_exclusive_zone =
                    layer_map_for_output(&output_data.output).non_exclusive_zone();
                let geometries = geometries_for_groupview(
                    tree,
                    Option::<&mut GlowRenderer>::None,
                    non_exclusive_zone,
                    None,
                    1.0,
                    overview.alpha().unwrap(),
                    1.0,
                    placeholder_id,
                    Some(None),
                )
                .map(|x| x.0)
                .unwrap_or_default();

                let mut result = None;
                let mut lookup = Some(root.clone());
                while let Some(node) = lookup {
                    let data = tree.get(&node).unwrap().data();
                    if geometries
                        .get(&node)
                        .map(|geo| geo.contains(location))
                        .unwrap_or(false)
                    {
                        result = Some(node.clone());
                    }

                    lookup = None;
                    if result.is_some() && data.is_group() {
                        if tree.children(&node).unwrap().any(|child| {
                            matches!(
                                child.data(),
                                Data::Placeholder {
                                    initial_placeholder: false,
                                    ..
                                }
                            )
                        }) {
                            break;
                        }
                        for child_id in tree.children_ids(&node).unwrap() {
                            if geometries
                                .get(child_id)
                                .map(|geo| geo.contains(location))
                                .unwrap_or(false)
                            {
                                lookup = Some(child_id.clone());
                                break;
                            }
                        }
                    }
                }

                if let Some(res_id) = result {
                    let mut last_geometry = *geometries.get(&res_id)?;
                    let node = tree.get(&res_id).unwrap();
                    let data = node.data().clone();

                    let group_zone = if let Data::Group { orientation, .. } = &data {
                        if node.children().iter().any(|child_id| {
                            tree.get(child_id)
                                .ok()
                                .map(|child| {
                                    matches!(
                                        child.data(),
                                        Data::Placeholder {
                                            initial_placeholder: false,
                                            ..
                                        }
                                    )
                                })
                                .unwrap_or(false)
                        }) {
                            None
                        } else {
                            let left_edge = match &*last_overview_hover {
                                Some((_, TargetZone::GroupEdge(id, Direction::Left)))
                                    if *id == res_id =>
                                {
                                    let zone = Rectangle::from_loc_and_size(
                                        last_geometry.loc,
                                        (80, last_geometry.size.h),
                                    );
                                    last_geometry.loc.x += 80;
                                    last_geometry.size.w -= 80;
                                    zone
                                }
                                _ => {
                                    let zone = Rectangle::from_loc_and_size(
                                        last_geometry.loc,
                                        (32, last_geometry.size.h),
                                    );
                                    last_geometry.loc.x += 32;
                                    last_geometry.size.w -= 32;
                                    zone
                                }
                            };
                            let top_edge = match &*last_overview_hover {
                                Some((_, TargetZone::GroupEdge(id, Direction::Up)))
                                    if *id == res_id =>
                                {
                                    let zone = Rectangle::from_loc_and_size(
                                        last_geometry.loc,
                                        (last_geometry.size.w, 80),
                                    );
                                    last_geometry.loc.y += 80;
                                    last_geometry.size.h -= 80;
                                    zone
                                }
                                _ => {
                                    let zone = Rectangle::from_loc_and_size(
                                        last_geometry.loc,
                                        (last_geometry.size.w, 32),
                                    );
                                    last_geometry.loc.y += 32;
                                    last_geometry.size.h -= 32;
                                    zone
                                }
                            };
                            let right_edge = match &*last_overview_hover {
                                Some((_, TargetZone::GroupEdge(id, Direction::Right)))
                                    if *id == res_id =>
                                {
                                    let zone = Rectangle::from_loc_and_size(
                                        (
                                            last_geometry.loc.x + last_geometry.size.w - 80,
                                            last_geometry.loc.y,
                                        ),
                                        (80, last_geometry.size.h),
                                    );
                                    last_geometry.size.w -= 80;
                                    zone
                                }
                                _ => {
                                    let zone = Rectangle::from_loc_and_size(
                                        (
                                            last_geometry.loc.x + last_geometry.size.w - 32,
                                            last_geometry.loc.y,
                                        ),
                                        (32, last_geometry.size.h),
                                    );
                                    last_geometry.size.w -= 32;
                                    zone
                                }
                            };
                            let bottom_edge = match &*last_overview_hover {
                                Some((_, TargetZone::GroupEdge(id, Direction::Down)))
                                    if *id == res_id =>
                                {
                                    let zone = Rectangle::from_loc_and_size(
                                        (
                                            last_geometry.loc.x,
                                            last_geometry.loc.y + last_geometry.size.h - 80,
                                        ),
                                        (last_geometry.size.w, 80),
                                    );
                                    last_geometry.size.h -= 80;
                                    zone
                                }
                                _ => {
                                    let zone = Rectangle::from_loc_and_size(
                                        (
                                            last_geometry.loc.x,
                                            last_geometry.loc.y + last_geometry.size.h - 32,
                                        ),
                                        (last_geometry.size.w, 32),
                                    );
                                    last_geometry.size.h -= 32;
                                    zone
                                }
                            };

                            if left_edge.contains(location) {
                                Some(TargetZone::GroupEdge(res_id.clone(), Direction::Left))
                            } else if right_edge.contains(location) {
                                Some(TargetZone::GroupEdge(res_id.clone(), Direction::Right))
                            } else if top_edge.contains(location) {
                                Some(TargetZone::GroupEdge(res_id.clone(), Direction::Up))
                            } else if bottom_edge.contains(location) {
                                Some(TargetZone::GroupEdge(res_id.clone(), Direction::Down))
                            } else {
                                let idx = tree
                                    .children_ids(&res_id)
                                    .unwrap()
                                    .position(|node| {
                                        let Some(geo) = geometries.get(node) else {
                                            return false;
                                        };
                                        match orientation {
                                            Orientation::Vertical => location.x < geo.loc.x,
                                            Orientation::Horizontal => location.y < geo.loc.y,
                                        }
                                    })
                                    .and_then(|x| x.checked_sub(1))
                                    .unwrap_or(0);
                                Some(TargetZone::GroupInterior(res_id.clone(), idx))
                            }
                        }
                    } else {
                        None
                    };

                    let target_zone = group_zone.unwrap_or_else(|| match &data {
                        Data::Placeholder { .. } => TargetZone::InitialPlaceholder(res_id),
                        Data::Group { .. } | Data::Mapped { .. } => {
                            let id = if data.is_group() {
                                tree.get(&res_id)
                                    .unwrap()
                                    .children()
                                    .iter()
                                    .find(|child_id| {
                                        tree.get(child_id).unwrap().data().is_mapped(None)
                                    })
                                    .expect("Placeholder group without real window?")
                                    .clone()
                            } else {
                                res_id
                            };

                            let third_width = (last_geometry.size.w as f64 / 3.0).round() as i32;
                            let third_height = (last_geometry.size.h as f64 / 3.0).round() as i32;
                            let stack_region = Rectangle::from_extemities(
                                (
                                    last_geometry.loc.x + third_width,
                                    last_geometry.loc.y + third_height,
                                ),
                                (
                                    last_geometry.loc.x + 2 * third_width,
                                    last_geometry.loc.y + 2 * third_height,
                                ),
                            );

                            if stack_region.contains(location) {
                                TargetZone::WindowStack(id, last_geometry)
                            } else {
                                let left_right = {
                                    let relative_loc = (location.x - last_geometry.loc.x) as f64;
                                    if relative_loc < last_geometry.size.w as f64 / 2.0 {
                                        (
                                            Direction::Left,
                                            relative_loc / last_geometry.size.w as f64,
                                        )
                                    } else {
                                        (
                                            Direction::Right,
                                            1.0 - (relative_loc / last_geometry.size.w as f64),
                                        )
                                    }
                                };
                                let up_down = {
                                    let relative_loc = (location.y - last_geometry.loc.y) as f64;
                                    if relative_loc < last_geometry.size.h as f64 / 2.0 {
                                        (Direction::Up, relative_loc / last_geometry.size.h as f64)
                                    } else {
                                        (
                                            Direction::Down,
                                            1.0 - (relative_loc / last_geometry.size.h as f64),
                                        )
                                    }
                                };

                                let direction = if left_right.1 < up_down.1 {
                                    left_right.0
                                } else {
                                    up_down.0
                                };

                                TargetZone::WindowSplit(id, direction)
                            }
                        }
                    });

                    match &mut *last_overview_hover {
                        last_overview_hover @ None => {
                            *last_overview_hover = Some((
                                None,
                                tree.traverse_pre_order_ids(root)
                                    .unwrap()
                                    .find(|id| match tree.get(id).unwrap().data() {
                                        Data::Placeholder {
                                            initial_placeholder: true,
                                            ..
                                        } => true,
                                        _ => false,
                                    })
                                    .map(|node_id| TargetZone::InitialPlaceholder(node_id))
                                    .unwrap_or(TargetZone::Initial),
                            ));
                        }
                        Some((instant, old_target_zone)) => {
                            if *old_target_zone != target_zone {
                                let overdue = if let Some(instant) = instant {
                                    match old_target_zone {
                                        TargetZone::InitialPlaceholder(_) => {
                                            Instant::now().duration_since(*instant)
                                                > INITIAL_MOUSE_ANIMATION_DELAY
                                        }
                                        _ => {
                                            Instant::now().duration_since(*instant)
                                                > MOUSE_ANIMATION_DELAY
                                        }
                                    }
                                } else {
                                    *instant = Some(Instant::now());
                                    false
                                };

                                if overdue {
                                    let duration = if target_zone.is_window_zone()
                                        && !old_target_zone.is_window_zone()
                                    {
                                        ANIMATION_DURATION * 2
                                    } else {
                                        ANIMATION_DURATION
                                    };

                                    let mut tree = tree.copy_clone();

                                    // remove old placeholders
                                    let removed = if let TargetZone::InitialPlaceholder(node_id) =
                                        old_target_zone
                                    {
                                        if tree.get(&node_id).is_ok() {
                                            TilingLayout::unmap_internal(&mut tree, &node_id);
                                        }
                                        true
                                    } else if let TargetZone::WindowSplit(node_id, _) =
                                        old_target_zone
                                    {
                                        if let Some(children) = tree
                                            .get(&node_id)
                                            .ok()
                                            .and_then(|node| node.parent())
                                            .and_then(|parent_id| tree.get(parent_id).ok())
                                            .map(|node| node.children().clone())
                                        {
                                            for id in children {
                                                let matches = matches!(
                                                    tree.get(&id).unwrap().data(),
                                                    Data::Placeholder {
                                                        initial_placeholder: false,
                                                        ..
                                                    }
                                                );

                                                if matches {
                                                    TilingLayout::unmap_internal(&mut tree, &id);
                                                    break;
                                                }
                                            }
                                        }
                                        true
                                    } else if let TargetZone::GroupEdge(node_id, _) =
                                        old_target_zone
                                    {
                                        if let Ok(node) = tree.get_mut(&node_id) {
                                            match node.data_mut() {
                                                Data::Group { pill_indicator, .. } => {
                                                    *pill_indicator = None;
                                                }
                                                _ => unreachable!(),
                                            }
                                        }
                                        true
                                    } else if let TargetZone::GroupInterior(node_id, _) =
                                        old_target_zone
                                    {
                                        if let Ok(node) = tree.get_mut(&node_id) {
                                            match node.data_mut() {
                                                Data::Group { pill_indicator, .. } => {
                                                    *pill_indicator = None;
                                                }
                                                _ => unreachable!(),
                                            }
                                        }
                                        true
                                    } else {
                                        false
                                    };

                                    // add placeholders
                                    let added = if let TargetZone::WindowSplit(node_id, dir) =
                                        &target_zone
                                    {
                                        let id = tree
                                            .insert(
                                                Node::new(Data::Placeholder {
                                                    last_geometry: Rectangle::from_loc_and_size(
                                                        (0, 0),
                                                        (100, 100),
                                                    ),
                                                    initial_placeholder: false,
                                                }),
                                                InsertBehavior::UnderNode(node_id),
                                            )
                                            .unwrap();
                                        let orientation =
                                            if matches!(dir, Direction::Left | Direction::Right) {
                                                Orientation::Vertical
                                            } else {
                                                Orientation::Horizontal
                                            };
                                        TilingLayout::new_group(
                                            &mut tree,
                                            &node_id,
                                            &id,
                                            orientation,
                                        )
                                        .unwrap();
                                        if matches!(dir, Direction::Left | Direction::Up) {
                                            tree.make_first_sibling(&id).unwrap();
                                        }

                                        true
                                    } else if let TargetZone::GroupEdge(node_id, direction) =
                                        &target_zone
                                    {
                                        if let Ok(node) = tree.get_mut(&node_id) {
                                            match node.data_mut() {
                                                Data::Group { pill_indicator, .. } => {
                                                    *pill_indicator =
                                                        Some(PillIndicator::Outer(*direction));
                                                }
                                                _ => unreachable!(),
                                            }
                                            true
                                        } else {
                                            false
                                        }
                                    } else if let TargetZone::GroupInterior(node_id, idx) =
                                        &target_zone
                                    {
                                        if let Ok(node) = tree.get_mut(&node_id) {
                                            match node.data_mut() {
                                                Data::Group { pill_indicator, .. } => {
                                                    *pill_indicator =
                                                        Some(PillIndicator::Inner(*idx));
                                                }
                                                _ => unreachable!(),
                                            }
                                            true
                                        } else {
                                            false
                                        }
                                    } else {
                                        false
                                    };

                                    if removed || added {
                                        let blocker = TilingLayout::update_positions(
                                            &output_data.output,
                                            &mut tree,
                                            *gaps,
                                        );
                                        queue.push_tree(tree, duration, blocker);
                                    }

                                    *instant = None;
                                    *old_target_zone = target_zone;
                                }
                            } else {
                                *instant = None;
                            }
                        }
                    }
                }

                None
            } else {
                None
            }
        })
    }

    pub fn mapped(
        &self,
    ) -> impl Iterator<Item = (&Output, &CosmicMapped, Rectangle<i32, Logical>)> {
        self.queues
            .iter()
            .flat_map(|(output_data, queue)| {
                let tree = &queue.trees.back().unwrap().0;
                if let Some(root) = tree.root_node_id() {
                    Some(
                        tree.traverse_pre_order(root)
                            .unwrap()
                            .filter(|node| node.data().is_mapped(None))
                            .filter(|node| match node.data() {
                                Data::Mapped { mapped, .. } => mapped.is_activated(false),
                                _ => unreachable!(),
                            })
                            .map(|node| match node.data() {
                                Data::Mapped {
                                    mapped,
                                    last_geometry,
                                    ..
                                } => (&output_data.output, mapped, {
                                    let mut geo = last_geometry.clone();
                                    geo.loc += output_data.location;
                                    geo
                                }),
                                _ => unreachable!(),
                            })
                            .chain(
                                tree.traverse_pre_order(root)
                                    .unwrap()
                                    .filter(|node| node.data().is_mapped(None))
                                    .filter(|node| match node.data() {
                                        Data::Mapped { mapped, .. } => !mapped.is_activated(false),
                                        _ => unreachable!(),
                                    })
                                    .map(|node| match node.data() {
                                        Data::Mapped {
                                            mapped,
                                            last_geometry,
                                            ..
                                        } => (&output_data.output, mapped, {
                                            let mut geo = last_geometry.clone();
                                            geo.loc += output_data.location;
                                            geo
                                        }),
                                        _ => unreachable!(),
                                    }),
                            ),
                    )
                } else {
                    None
                }
            })
            .flatten()
    }

    pub fn windows(
        &self,
    ) -> impl Iterator<Item = (Output, CosmicSurface, Rectangle<i32, Logical>)> + '_ {
        self.mapped().flat_map(|(output, mapped, geo)| {
            mapped.windows().map(move |(w, p)| {
                (output.clone(), w, {
                    let mut geo = geo.clone();
                    geo.loc += p;
                    geo.size -= p.to_size();
                    geo
                })
            })
        })
    }

    pub fn merge(&mut self, other: TilingLayout) {
        for (output_data, mut src_queue) in other.queues {
            let src = src_queue.trees.pop_back().unwrap().0;
            let dst_queue = self.queues.entry(output_data.clone()).or_default();
            let mut dst = dst_queue.trees.back().unwrap().0.copy_clone();

            let orientation = match output_data.output.geometry().size {
                x if x.w >= x.h => Orientation::Vertical,
                _ => Orientation::Horizontal,
            };
            TilingLayout::merge_trees(src, &mut dst, orientation);

            let blocker = TilingLayout::update_positions(&output_data.output, &mut dst, self.gaps);
            dst_queue.push_tree(dst, ANIMATION_DURATION, blocker);
        }
    }

    fn merge_trees(src: Tree<Data>, dst: &mut Tree<Data>, orientation: Orientation) {
        if let Some(dst_root_id) = dst.root_node_id().cloned() {
            let mut stack = Vec::new();

            if let Some(src_root_id) = src.root_node_id() {
                let root_node = src.get(src_root_id).unwrap();
                let new_node = Node::new(root_node.data().clone());
                let new_id = dst
                    .insert(new_node, InsertBehavior::UnderNode(&dst_root_id))
                    .unwrap();
                if let &mut Data::Mapped { ref mut mapped, .. } =
                    dst.get_mut(&new_id).unwrap().data_mut()
                {
                    *mapped.tiling_node_id.lock().unwrap() = Some(new_id.clone());
                }
                TilingLayout::new_group(dst, &dst_root_id, &new_id, orientation).unwrap();
                stack.push((src_root_id.clone(), new_id));
            }

            while let Some((src_id, dst_id)) = stack.pop() {
                for child_id in src.children_ids(&src_id).unwrap() {
                    let src_node = src.get(&child_id).unwrap();
                    let new_node = Node::new(src_node.data().clone());
                    let new_child_id = dst
                        .insert(new_node, InsertBehavior::UnderNode(&dst_id))
                        .unwrap();
                    if let &mut Data::Mapped { ref mut mapped, .. } =
                        dst.get_mut(&new_child_id).unwrap().data_mut()
                    {
                        *mapped.tiling_node_id.lock().unwrap() = Some(new_child_id.clone());
                    }
                    stack.push((child_id.clone(), new_child_id));
                }
            }
        } else {
            *dst = src;
        }
    }

    pub fn render_output<R>(
        &self,
        renderer: &mut R,
        output: &Output,
        seat: Option<&Seat<State>>,
        non_exclusive_zone: Rectangle<i32, Logical>,
        overview: OverviewMode,
        resize_indicator: Option<(ResizeMode, ResizeIndicator)>,
        indicator_thickness: u8,
    ) -> Result<
        (
            Vec<CosmicMappedRenderElement<R>>,
            Vec<CosmicMappedRenderElement<R>>,
        ),
        OutputNotMapped,
    >
    where
        R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
        <R as Renderer>::TextureId: 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        CosmicWindowRenderElement<R>: RenderElement<R>,
        CosmicStackRenderElement<R>: RenderElement<R>,
    {
        #[cfg(feature = "debug")]
        puffin::profile_function!();

        let output_scale = output.current_scale().fractional_scale();

        if !self.queues.contains_key(output) {
            return Err(OutputNotMapped);
        }

        let queue = self.queues.get(output).unwrap();
        let (target_tree, duration, _) = if queue.animation_start.is_some() {
            queue
                .trees
                .get(1)
                .expect("Animation ongoing, should have two trees")
        } else {
            queue.trees.front().unwrap()
        };
        let reference_tree = queue
            .animation_start
            .is_some()
            .then(|| &queue.trees.front().unwrap().0);

        let percentage = if let Some(animation_start) = queue.animation_start {
            let percentage = Instant::now().duration_since(animation_start).as_millis() as f32
                / duration.as_millis() as f32;
            ease(EaseInOutCubic, 0.0, 1.0, percentage)
        } else {
            1.0
        };
        let draw_groups = overview.alpha();

        let mut window_elements = Vec::new();
        let mut popup_elements = Vec::new();

        let is_mouse_tiling = (matches!(overview, OverviewMode::Started(Trigger::Pointer(_), _)))
            .then(|| self.last_overview_hover.as_ref().map(|x| &x.1));

        // all gone windows and fade them out
        let old_geometries = if let Some(reference_tree) = reference_tree.as_ref() {
            let (geometries, _) = if let Some(transition) = draw_groups {
                geometries_for_groupview(
                    reference_tree,
                    &mut *renderer,
                    non_exclusive_zone,
                    seat, // TODO: Would be better to be an old focus,
                    // but for that we have to associate focus with a tree (and animate focus changes properly)
                    1.0 - transition,
                    transition,
                    output_scale,
                    &self.placeholder_id,
                    is_mouse_tiling,
                )
            } else {
                None
            }
            .unzip();

            // all old windows we want to fade out
            let (w_elements, p_elements) = render_old_tree(
                reference_tree,
                target_tree,
                renderer,
                geometries.clone(),
                output_scale,
                percentage,
            );
            window_elements.extend(w_elements);
            popup_elements.extend(p_elements);

            geometries
        } else {
            None
        };

        let (geometries, group_elements) = if let Some(transition) = draw_groups {
            geometries_for_groupview(
                target_tree,
                &mut *renderer,
                non_exclusive_zone,
                seat,
                transition,
                transition,
                output_scale,
                &self.placeholder_id,
                is_mouse_tiling,
            )
        } else {
            None
        }
        .unzip();

        // all alive windows
        let (w_elements, p_elements) = render_new_tree(
            target_tree,
            reference_tree,
            renderer,
            geometries,
            old_geometries,
            seat,
            output,
            percentage,
            if let Some(transition) = draw_groups {
                let diff = (4u8.abs_diff(indicator_thickness) as f32 * transition).round() as u8;
                if 3 > indicator_thickness {
                    indicator_thickness + diff
                } else {
                    indicator_thickness - diff
                }
            } else {
                indicator_thickness
            },
            resize_indicator,
        );
        window_elements.extend(w_elements);
        popup_elements.extend(p_elements);

        // tiling hints
        if let Some(group_elements) = group_elements {
            window_elements.extend(group_elements);
        }

        Ok((window_elements, popup_elements))
    }
}

const GAP_KEYBOARD: i32 = 8;
const GAP_MOUSE: i32 = 32;
const PLACEHOLDER_GAP_MOUSE: i32 = 8;
const WINDOW_BACKDROP_BORDER: i32 = 4;
const WINDOW_BACKDROP_GAP: i32 = 12;

fn geometries_for_groupview<'a, R>(
    tree: &Tree<Data>,
    renderer: impl Into<Option<&'a mut R>>,
    non_exclusive_zone: Rectangle<i32, Logical>,
    seat: Option<&Seat<State>>,
    alpha: f32,
    transition: f32,
    output_scale: f64,
    placeholder_id: &Id,
    mouse_tiling: Option<Option<&TargetZone>>,
) -> Option<(
    HashMap<NodeId, Rectangle<i32, Logical>>,
    Vec<CosmicMappedRenderElement<R>>,
)>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer + 'a,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    CosmicWindowRenderElement<R>: RenderElement<R>,
{
    // we need to recalculate geometry for all elements, if we are drawing groups
    if let Some(root) = tree.root_node_id() {
        let gap: i32 = (if mouse_tiling.is_some() {
            GAP_MOUSE
        } else {
            GAP_KEYBOARD
        } as f32
            * transition)
            .round() as i32;
        let mut renderer = renderer.into();

        let mut stack = vec![(non_exclusive_zone, 0)];
        let mut elements = Vec::new();
        let mut geometries = HashMap::new();
        let alpha = alpha * transition;

        let focused = seat
            .and_then(|seat| {
                seat.get_keyboard()
                    .unwrap()
                    .current_focus()
                    .and_then(|target| {
                        TilingLayout::currently_focused_node(&tree, &seat.active_output(), target)
                    })
            })
            .map(|(id, _)| id);

        let has_potential_groups = if let Some(focused_id) = focused.as_ref() {
            let focused_node = tree.get(focused_id).unwrap();
            if let Some(parent) = focused_node.parent() {
                let parent_node = tree.get(parent).unwrap();
                parent_node.children().len() > 2
            } else {
                false
            }
        } else {
            false
        };

        for node_id in tree.traverse_pre_order_ids(root).unwrap() {
            if let Some((mut geo, depth)) = stack.pop() {
                let node: &Node<Data> = tree.get(&node_id).unwrap();
                let data = node.data();

                let is_placeholder_sibling = node
                    .parent()
                    .and_then(|parent_id| tree.children_ids(parent_id).ok())
                    .map(|mut siblings| {
                        siblings.any(|child_id| tree.get(child_id).unwrap().data().is_placeholder())
                    })
                    .unwrap_or(false);

                let render_potential_group = has_potential_groups
                    && (if let Some(focused_id) = focused.as_ref() {
                        // `focused` can move into us directly
                        if let Some(parent) = node.parent() {
                            let parent_data = tree.get(parent).unwrap().data();

                            let idx = tree
                                .children_ids(parent)
                                .unwrap()
                                .position(|id| id == &node_id)
                                .unwrap();
                            if let Some(focused_idx) = tree
                                .children_ids(parent)
                                .unwrap()
                                .position(|id| id == focused_id)
                            {
                                // only direct neighbors
                                focused_idx.abs_diff(idx) == 1
                                // skip neighbors, if this is a group of two
                                && parent_data.len() > 2
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    } else {
                        false
                    });

                let (element_gap_left, element_gap_up, element_gap_right, element_gap_down) = {
                    let gap = if is_placeholder_sibling {
                        PLACEHOLDER_GAP_MOUSE
                    } else {
                        gap
                    };
                    (
                        if TilingLayout::has_sibling_node(tree, &node_id, Direction::Left)
                            && (mouse_tiling.is_some() || depth > 0)
                        {
                            gap / 2
                        } else {
                            0
                        },
                        if TilingLayout::has_sibling_node(tree, &node_id, Direction::Up)
                            && (mouse_tiling.is_some() || depth > 0)
                        {
                            gap / 2
                        } else {
                            0
                        },
                        if TilingLayout::has_sibling_node(tree, &node_id, Direction::Right)
                            && (mouse_tiling.is_some() || depth > 0)
                        {
                            gap / 2
                        } else {
                            0
                        },
                        if TilingLayout::has_sibling_node(tree, &node_id, Direction::Down)
                            && (mouse_tiling.is_some() || depth > 0)
                        {
                            gap / 2
                        } else {
                            0
                        },
                    )
                };

                match data {
                    Data::Group {
                        orientation,
                        last_geometry,
                        sizes,
                        alive,
                        pill_indicator,
                    } => {
                        let render_active_child = if let Some(focused_id) = focused.as_ref() {
                            !has_potential_groups
                                && node
                                    .children()
                                    .iter()
                                    .any(|child_id| child_id == focused_id)
                        } else {
                            false
                        };

                        geo.loc += (element_gap_left, element_gap_up).into();
                        geo.size -= (element_gap_left, element_gap_up).into();
                        geo.size -= (element_gap_right, element_gap_down).into();

                        geometries.insert(node_id.clone(), geo);

                        if let Some(renderer) = renderer.as_mut() {
                            if (render_potential_group || render_active_child) && &node_id != root {
                                elements.push(
                                    IndicatorShader::element(
                                        *renderer,
                                        Key::Group(Arc::downgrade(alive)),
                                        geo,
                                        4,
                                        if render_active_child { 16 } else { 8 },
                                        alpha * if render_potential_group { 0.40 } else { 1.0 },
                                        output_scale,
                                        GROUP_COLOR,
                                    )
                                    .into(),
                                );
                            }
                            if mouse_tiling.is_some()
                                && pill_indicator.is_some()
                                && &node_id != root
                            {
                                elements.push(
                                    IndicatorShader::element(
                                        *renderer,
                                        Key::Group(Arc::downgrade(alive)),
                                        geo,
                                        4,
                                        8,
                                        alpha * 0.40,
                                        output_scale,
                                        GROUP_COLOR,
                                    )
                                    .into(),
                                );
                            }

                            if mouse_tiling.is_some()
                                && node
                                    .parent()
                                    .map(|parent_id| {
                                        matches!(
                                            tree.get(&parent_id).unwrap().data(),
                                            Data::Group {
                                                pill_indicator: Some(_),
                                                ..
                                            }
                                        )
                                    })
                                    .unwrap_or(false)
                            {
                                // test if parent pill-indicator is adjacent
                                let parent_id = node.parent().unwrap();
                                let parent = tree.get(parent_id).unwrap();
                                let own_idx = tree
                                    .children_ids(parent_id)
                                    .unwrap()
                                    .position(|child_id| child_id == &node_id)
                                    .unwrap();
                                let draw_outline = match parent.data() {
                                    Data::Group {
                                        pill_indicator,
                                        orientation,
                                        ..
                                    } => match pill_indicator {
                                        Some(PillIndicator::Inner(pill_idx)) => {
                                            *pill_idx == own_idx || pill_idx + 1 == own_idx
                                        }
                                        Some(PillIndicator::Outer(dir)) => match (dir, orientation)
                                        {
                                            (Direction::Left, Orientation::Horizontal)
                                            | (Direction::Right, Orientation::Horizontal)
                                            | (Direction::Up, Orientation::Vertical)
                                            | (Direction::Down, Orientation::Vertical) => true,
                                            (Direction::Left, Orientation::Vertical)
                                            | (Direction::Up, Orientation::Horizontal) => {
                                                own_idx == 0
                                            }
                                            (Direction::Right, Orientation::Vertical)
                                            | (Direction::Down, Orientation::Horizontal) => {
                                                own_idx + 1 == parent.data().len()
                                            }
                                        },
                                        None => unreachable!(),
                                    },
                                    _ => unreachable!(),
                                };

                                if draw_outline {
                                    elements.push(
                                        IndicatorShader::element(
                                            *renderer,
                                            Key::Group(Arc::downgrade(alive)),
                                            geo,
                                            4,
                                            8,
                                            alpha * 0.15,
                                            output_scale,
                                            GROUP_COLOR,
                                        )
                                        .into(),
                                    );
                                }
                            }
                        }

                        geo.loc += (gap, gap).into();
                        geo.size -= (gap * 2, gap * 2).into();

                        if mouse_tiling.is_some() {
                            if let Some(PillIndicator::Outer(direction)) = pill_indicator {
                                let (pill_geo, remaining_geo) = match direction {
                                    Direction::Left => (
                                        Rectangle::from_loc_and_size(
                                            (geo.loc.x, geo.loc.y),
                                            (16, geo.size.h),
                                        ),
                                        Rectangle::from_loc_and_size(
                                            (geo.loc.x + 48, geo.loc.y),
                                            (geo.size.w - 48, geo.size.h),
                                        ),
                                    ),
                                    Direction::Up => (
                                        Rectangle::from_loc_and_size(
                                            (geo.loc.x, geo.loc.y),
                                            (geo.size.w, 16),
                                        ),
                                        Rectangle::from_loc_and_size(
                                            (geo.loc.x, geo.loc.y + 48),
                                            (geo.size.w, geo.size.h - 48),
                                        ),
                                    ),
                                    Direction::Right => (
                                        Rectangle::from_loc_and_size(
                                            (geo.loc.x + geo.size.w - 16, geo.loc.y),
                                            (16, geo.size.h),
                                        ),
                                        Rectangle::from_loc_and_size(
                                            geo.loc,
                                            (geo.size.w - 48, geo.size.h),
                                        ),
                                    ),
                                    Direction::Down => (
                                        Rectangle::from_loc_and_size(
                                            (geo.loc.x, geo.loc.y + geo.size.h - 16),
                                            (geo.size.w, 16),
                                        ),
                                        Rectangle::from_loc_and_size(
                                            geo.loc,
                                            (geo.size.w, geo.size.h - 48),
                                        ),
                                    ),
                                };

                                if let Some(renderer) = renderer.as_mut() {
                                    elements.push(
                                        BackdropShader::element(
                                            *renderer,
                                            placeholder_id.clone(),
                                            pill_geo,
                                            8.,
                                            alpha * 0.4,
                                            GROUP_COLOR,
                                        )
                                        .into(),
                                    );
                                }

                                geo = remaining_geo;
                            };
                        }

                        let previous_length = match orientation {
                            Orientation::Horizontal => last_geometry.size.h,
                            Orientation::Vertical => last_geometry.size.w,
                        };
                        let new_length = match orientation {
                            Orientation::Horizontal => geo.size.h,
                            Orientation::Vertical => geo.size.w,
                        };

                        let mut sizes = sizes
                            .iter()
                            .map(|len| {
                                (((*len as f64) / (previous_length as f64)) * (new_length as f64))
                                    .round() as i32
                            })
                            .collect::<Vec<_>>();
                        let sum: i32 = sizes.iter().sum();
                        if sum < new_length {
                            *sizes.last_mut().unwrap() += new_length - sum;
                        }

                        match orientation {
                            Orientation::Horizontal => {
                                let mut previous: i32 = sizes.iter().sum();
                                for (idx, size) in sizes.iter().enumerate().rev() {
                                    previous -= *size;
                                    let mut geo = Rectangle::from_loc_and_size(
                                        (geo.loc.x, geo.loc.y + previous),
                                        (geo.size.w, *size),
                                    );
                                    if mouse_tiling.is_some() {
                                        if let Some(PillIndicator::Inner(pill_idx)) = pill_indicator
                                        {
                                            if *pill_idx == idx {
                                                geo.size.h -= 32;
                                            }
                                            if idx
                                                .checked_sub(1)
                                                .map(|idx| idx == *pill_idx)
                                                .unwrap_or(false)
                                            {
                                                if let Some(renderer) = renderer.as_mut() {
                                                    elements.push(
                                                        BackdropShader::element(
                                                            *renderer,
                                                            placeholder_id.clone(),
                                                            Rectangle::from_loc_and_size(
                                                                (geo.loc.x, geo.loc.y - 8),
                                                                (geo.size.w, 16),
                                                            ),
                                                            8.,
                                                            alpha * 0.4,
                                                            GROUP_COLOR,
                                                        )
                                                        .into(),
                                                    );
                                                }
                                                geo.loc.y += 32;
                                                geo.size.h -= 32;
                                            }
                                        }
                                    }
                                    stack.push((geo, depth + 1));
                                }
                            }
                            Orientation::Vertical => {
                                let mut previous: i32 = sizes.iter().sum();
                                for (idx, size) in sizes.iter().enumerate().rev() {
                                    previous -= *size;
                                    let mut geo = Rectangle::from_loc_and_size(
                                        (geo.loc.x + previous, geo.loc.y),
                                        (*size, geo.size.h),
                                    );
                                    if mouse_tiling.is_some() {
                                        if let Some(PillIndicator::Inner(pill_idx)) = pill_indicator
                                        {
                                            if *pill_idx == idx {
                                                geo.size.w -= 32;
                                            }
                                            if idx
                                                .checked_sub(1)
                                                .map(|idx| idx == *pill_idx)
                                                .unwrap_or(false)
                                            {
                                                if let Some(renderer) = renderer.as_mut() {
                                                    elements.push(
                                                        BackdropShader::element(
                                                            *renderer,
                                                            placeholder_id.clone(),
                                                            Rectangle::from_loc_and_size(
                                                                (geo.loc.x - 8, geo.loc.y),
                                                                (16, geo.size.h),
                                                            ),
                                                            8.,
                                                            alpha * 0.4,
                                                            GROUP_COLOR,
                                                        )
                                                        .into(),
                                                    );
                                                }
                                                geo.loc.x += 32;
                                                geo.size.w -= 32;
                                            }
                                        }
                                    }
                                    stack.push((geo, depth + 1));
                                }
                            }
                        }
                    }
                    Data::Mapped { mapped, .. } => {
                        geo.loc += (element_gap_left, element_gap_up).into();
                        geo.size -= (element_gap_left, element_gap_up).into();
                        geo.size -= (element_gap_right, element_gap_down).into();

                        if let Some(renderer) = renderer.as_mut() {
                            if render_potential_group {
                                elements.push(
                                    IndicatorShader::element(
                                        *renderer,
                                        mapped.clone(),
                                        geo,
                                        4,
                                        8,
                                        alpha * 0.40,
                                        output_scale,
                                        GROUP_COLOR,
                                    )
                                    .into(),
                                );
                                geo.loc += (gap, gap).into();
                                geo.size -= (gap * 2, gap * 2).into();
                            }

                            if focused
                                .as_ref()
                                .map(|focused_id| {
                                    !tree
                                        .ancestor_ids(&node_id)
                                        .unwrap()
                                        .any(|id| id == focused_id)
                                })
                                .unwrap_or(mouse_tiling.is_some())
                            {
                                let color = match mouse_tiling {
                                    Some(Some(TargetZone::WindowStack(stack_id, _)))
                                        if *stack_id == node_id =>
                                    {
                                        ACTIVE_GROUP_COLOR
                                    }
                                    _ => GROUP_COLOR,
                                };
                                geo.loc += (WINDOW_BACKDROP_BORDER, WINDOW_BACKDROP_BORDER).into();
                                geo.size -=
                                    (WINDOW_BACKDROP_BORDER * 2, WINDOW_BACKDROP_BORDER * 2).into();
                                elements.push(
                                    BackdropShader::element(
                                        *renderer,
                                        mapped.clone(),
                                        geo,
                                        8.,
                                        alpha
                                            * if focused
                                                .as_ref()
                                                .map(|focused_id| focused_id == &node_id)
                                                .unwrap_or(color == ACTIVE_GROUP_COLOR)
                                            {
                                                0.4
                                            } else {
                                                0.15
                                            },
                                        color,
                                    )
                                    .into(),
                                );
                            }

                            geo.loc += (WINDOW_BACKDROP_GAP, WINDOW_BACKDROP_GAP).into();
                            geo.size -= (WINDOW_BACKDROP_GAP * 2, WINDOW_BACKDROP_GAP * 2).into();
                        }

                        geometries.insert(node_id.clone(), geo);
                    }
                    Data::Placeholder { .. } => {
                        geo.loc += (element_gap_left, element_gap_up).into();
                        geo.size -= (element_gap_left, element_gap_up).into();
                        geo.size -= (element_gap_right, element_gap_down).into();

                        if let Some(renderer) = renderer.as_mut() {
                            geo.loc += (WINDOW_BACKDROP_BORDER, WINDOW_BACKDROP_BORDER).into();
                            geo.size -=
                                (WINDOW_BACKDROP_BORDER * 2, WINDOW_BACKDROP_BORDER * 2).into();
                            elements.push(
                                BackdropShader::element(
                                    *renderer,
                                    placeholder_id.clone(),
                                    geo,
                                    8.,
                                    alpha * 0.4,
                                    GROUP_COLOR,
                                )
                                .into(),
                            );
                        }

                        geometries.insert(node_id.clone(), geo);
                    }
                }
            }
        }

        Some((geometries, elements))
    } else {
        None
    }
}

fn render_old_tree<R>(
    reference_tree: &Tree<Data>,
    target_tree: &Tree<Data>,
    renderer: &mut R,
    geometries: Option<HashMap<NodeId, Rectangle<i32, Logical>>>,
    output_scale: f64,
    percentage: f32,
) -> (
    Vec<CosmicMappedRenderElement<R>>,
    Vec<CosmicMappedRenderElement<R>>,
)
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    CosmicWindowRenderElement<R>: RenderElement<R>,
    CosmicStackRenderElement<R>: RenderElement<R>,
{
    let mut window_elements = Vec::new();
    let mut popup_elements = Vec::new();

    if let Some(root) = reference_tree.root_node_id() {
        let geometries = geometries.unwrap_or_default();
        reference_tree
            .traverse_pre_order_ids(root)
            .unwrap()
            .filter(|node_id| reference_tree.get(node_id).unwrap().data().is_mapped(None))
            .map(
                |node_id| match reference_tree.get(&node_id).unwrap().data() {
                    Data::Mapped {
                        mapped,
                        last_geometry,
                        ..
                    } => (mapped, last_geometry, geometries.get(&node_id)),
                    _ => unreachable!(),
                },
            )
            .filter(|(mapped, _, _)| {
                if let Some(root) = target_tree.root_node_id() {
                    !target_tree
                        .traverse_pre_order(root)
                        .unwrap()
                        .any(|node| node.data().is_mapped(Some(mapped)))
                } else {
                    true
                }
            })
            .for_each(|(mapped, original_geo, scaled_geo)| {
                let (scale, offset) = scaled_geo
                    .map(|adapted_geo| scale_to_center(&original_geo, adapted_geo))
                    .unwrap_or_else(|| (1.0.into(), (0, 0).into()));
                let geo = scaled_geo
                    .map(|adapted_geo| {
                        Rectangle::from_loc_and_size(
                            adapted_geo.loc + offset,
                            (
                                (original_geo.size.w as f64 * scale).round() as i32,
                                (original_geo.size.h as f64 * scale).round() as i32,
                            ),
                        )
                    })
                    .unwrap_or(*original_geo);

                let crop_rect = geo.clone();
                let original_location = original_geo.loc.to_physical_precise_round(output_scale)
                    - mapped
                        .geometry()
                        .loc
                        .to_physical_precise_round(output_scale);

                let (w_elements, p_elements) = mapped
                    .split_render_elements::<R, CosmicMappedRenderElement<R>>(
                        renderer,
                        original_location,
                        Scale::from(output_scale),
                        1.0 - percentage,
                    );

                window_elements.extend(w_elements.into_iter().flat_map(|element| match element {
                    CosmicMappedRenderElement::Stack(elem) => {
                        Some(CosmicMappedRenderElement::TiledStack({
                            let cropped = CropRenderElement::from_element(
                                elem,
                                output_scale,
                                crop_rect.to_physical_precise_round(output_scale),
                            )?;
                            let rescaled = RescaleRenderElement::from_element(
                                cropped,
                                original_location,
                                scale,
                            );
                            let relocated = RelocateRenderElement::from_element(
                                rescaled,
                                (geo.loc - original_geo.loc)
                                    .to_physical_precise_round(output_scale),
                                Relocate::Relative,
                            );
                            relocated
                        }))
                    }
                    CosmicMappedRenderElement::Window(elem) => {
                        Some(CosmicMappedRenderElement::TiledWindow({
                            let cropped = CropRenderElement::from_element(
                                elem,
                                output_scale,
                                crop_rect.to_physical_precise_round(output_scale),
                            )?;
                            let rescaled = RescaleRenderElement::from_element(
                                cropped,
                                original_location,
                                scale,
                            );
                            let relocated = RelocateRenderElement::from_element(
                                rescaled,
                                (geo.loc - original_geo.loc)
                                    .to_physical_precise_round(output_scale),
                                Relocate::Relative,
                            );
                            relocated
                        }))
                    }
                    x => Some(x),
                }));
                popup_elements.extend(p_elements);
            });
    }

    (window_elements, popup_elements)
}

fn render_new_tree<R>(
    target_tree: &Tree<Data>,
    reference_tree: Option<&Tree<Data>>,
    renderer: &mut R,
    geometries: Option<HashMap<NodeId, Rectangle<i32, Logical>>>,
    old_geometries: Option<HashMap<NodeId, Rectangle<i32, Logical>>>,
    seat: Option<&Seat<State>>,
    output: &Output,
    percentage: f32,
    indicator_thickness: u8,
    mut resize_indicator: Option<(ResizeMode, ResizeIndicator)>,
) -> (
    Vec<CosmicMappedRenderElement<R>>,
    Vec<CosmicMappedRenderElement<R>>,
)
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    CosmicWindowRenderElement<R>: RenderElement<R>,
    CosmicStackRenderElement<R>: RenderElement<R>,
{
    let focused = seat
        .and_then(|seat| {
            seat.get_keyboard()
                .unwrap()
                .current_focus()
                .and_then(|target| {
                    TilingLayout::currently_focused_node(
                        &target_tree,
                        &seat.active_output(),
                        target,
                    )
                })
        })
        .map(|(id, _)| id);

    let mut window_elements = Vec::new();
    let mut popup_elements = Vec::new();

    let mut group_backdrop = None;
    let mut indicator = None;
    let mut resize_elements = None;

    let output_geo = output.geometry();
    let output_scale = output.current_scale().fractional_scale();

    if let Some(root) = target_tree.root_node_id() {
        let old_geometries = old_geometries.unwrap_or_default();
        let geometries = geometries.unwrap_or_default();
        target_tree
            .traverse_pre_order_ids(root)
            .unwrap()
            .for_each(|node_id| {
                let data = target_tree.get(&node_id).unwrap().data();
                let (original_geo, scaled_geo) = (data.geometry(), geometries.get(&node_id));

                let (old_original_geo, old_scaled_geo) =
                    if let Some(reference_tree) = reference_tree.as_ref() {
                        if let Some(root) = reference_tree.root_node_id() {
                            reference_tree
                                .traverse_pre_order_ids(root)
                                .unwrap()
                                .find(|id| &node_id == id)
                                .map(|node_id| {
                                    (
                                        reference_tree.get(&node_id).unwrap().data().geometry(),
                                        old_geometries.get(&node_id),
                                    )
                                })
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                    .unzip();
                let old_geo = old_original_geo.map(|original_geo| {
                    let (scale, offset) = old_scaled_geo
                        .unwrap()
                        .map(|adapted_geo| scale_to_center(original_geo, adapted_geo))
                        .unwrap_or_else(|| (1.0.into(), (0, 0).into()));
                    old_scaled_geo
                        .unwrap()
                        .map(|adapted_geo| {
                            Rectangle::from_loc_and_size(
                                adapted_geo.loc + offset,
                                (
                                    (original_geo.size.w as f64 * scale).round() as i32,
                                    (original_geo.size.h as f64 * scale).round() as i32,
                                ),
                            )
                        })
                        .unwrap_or(*original_geo)
                });

                let crop_rect = original_geo;
                let (scale, offset) = scaled_geo
                    .map(|adapted_geo| scale_to_center(original_geo, adapted_geo))
                    .unwrap_or_else(|| (1.0.into(), (0, 0).into()));
                let new_geo = scaled_geo
                    .map(|adapted_geo| {
                        Rectangle::from_loc_and_size(
                            adapted_geo.loc + offset,
                            (
                                (original_geo.size.w as f64 * scale).round() as i32,
                                (original_geo.size.h as f64 * scale).round() as i32,
                            ),
                        )
                    })
                    .unwrap_or(*original_geo);

                let (geo, alpha) = if let Some(old_geo) = old_geo {
                    (
                        Rectangle::from_loc_and_size(
                            (
                                old_geo.loc.x
                                    + ((new_geo.loc.x - old_geo.loc.x) as f32 * percentage).round()
                                        as i32,
                                old_geo.loc.y
                                    + ((new_geo.loc.y - old_geo.loc.y) as f32 * percentage).round()
                                        as i32,
                            ),
                            (
                                old_geo.size.w
                                    + ((new_geo.size.w - old_geo.size.w) as f32 * percentage)
                                        .round() as i32,
                                old_geo.size.h
                                    + ((new_geo.size.h - old_geo.size.h) as f32 * percentage)
                                        .round() as i32,
                            ),
                        ),
                        1.0,
                    )
                } else {
                    (new_geo, percentage)
                };

                if focused.as_ref() == Some(&node_id) {
                    if indicator_thickness > 0 || data.is_group() {
                        let mut geo = geo.clone();
                        if data.is_group() {
                            let outer_gap: i32 = (GAP_KEYBOARD as f32 * percentage).round() as i32;
                            geo.loc += (outer_gap, outer_gap).into();
                            geo.size -= (outer_gap * 2, outer_gap * 2).into();

                            group_backdrop = Some(BackdropShader::element(
                                renderer,
                                match data {
                                    Data::Group { alive, .. } => Key::Group(Arc::downgrade(alive)),
                                    _ => unreachable!(),
                                },
                                geo,
                                8.,
                                0.4,
                                GROUP_COLOR,
                            ));
                        }

                        indicator = Some(IndicatorShader::focus_element(
                            renderer,
                            match data {
                                Data::Mapped { mapped, .. } => mapped.clone().into(),
                                Data::Group { alive, .. } => Key::Group(Arc::downgrade(alive)),
                                _ => unreachable!(),
                            },
                            geo,
                            if data.is_group() {
                                4
                            } else {
                                indicator_thickness
                            },
                            output_scale,
                            1.0,
                        ));
                    }

                    if let Some((mode, resize)) = resize_indicator.as_mut() {
                        let mut geo = geo.clone();
                        geo.loc -= (18, 18).into();
                        geo.size += (36, 36).into();

                        resize.resize(geo.size);
                        resize.output_enter(output, output_geo);
                        let possible_edges = TilingLayout::possible_resizes(target_tree, node_id);
                        if !possible_edges.is_empty() {
                            if resize.with_program(|internal| {
                                let mut edges = internal.edges.lock().unwrap();
                                if *edges != possible_edges {
                                    *edges = possible_edges;
                                    true
                                } else {
                                    false
                                }
                            }) {
                                resize.force_update();
                            }
                            resize_elements = Some(
                                resize
                                    .render_elements::<CosmicWindowRenderElement<R>>(
                                        renderer,
                                        geo.loc.to_physical_precise_round(output_scale),
                                        output_scale.into(),
                                        alpha * mode.alpha().unwrap_or(1.0),
                                    )
                                    .into_iter()
                                    .map(CosmicMappedRenderElement::from)
                                    .collect::<Vec<_>>(),
                            );
                        }
                    }
                }

                if let Data::Mapped { mapped, .. } = data {
                    let original_location = (original_geo.loc - mapped.geometry().loc)
                        .to_physical_precise_round(output_scale);

                    let (w_elements, p_elements) = mapped
                        .split_render_elements::<R, CosmicMappedRenderElement<R>>(
                            renderer,
                            original_location,
                            Scale::from(output_scale),
                            alpha,
                        );

                    window_elements.extend(w_elements.into_iter().flat_map(
                        |element| match element {
                            CosmicMappedRenderElement::Stack(elem) => {
                                Some(CosmicMappedRenderElement::TiledStack({
                                    let cropped = CropRenderElement::from_element(
                                        elem,
                                        output_scale,
                                        crop_rect.to_physical_precise_round(output_scale),
                                    )?;
                                    let rescaled = RescaleRenderElement::from_element(
                                        cropped,
                                        original_geo.loc.to_physical_precise_round(output_scale),
                                        scale,
                                    );
                                    let relocated = RelocateRenderElement::from_element(
                                        rescaled,
                                        (geo.loc - original_geo.loc)
                                            .to_physical_precise_round(output_scale),
                                        Relocate::Relative,
                                    );
                                    relocated
                                }))
                            }
                            CosmicMappedRenderElement::Window(elem) => {
                                Some(CosmicMappedRenderElement::TiledWindow({
                                    let cropped = CropRenderElement::from_element(
                                        elem,
                                        output_scale,
                                        crop_rect.to_physical_precise_round(output_scale),
                                    )?;
                                    let rescaled = RescaleRenderElement::from_element(
                                        cropped,
                                        original_geo.loc.to_physical_precise_round(output_scale),
                                        scale,
                                    );
                                    let relocated = RelocateRenderElement::from_element(
                                        rescaled,
                                        (geo.loc - original_geo.loc)
                                            .to_physical_precise_round(output_scale),
                                        Relocate::Relative,
                                    );
                                    relocated
                                }))
                            }
                            x => Some(x),
                        },
                    ));
                    popup_elements.extend(p_elements)
                }
            });

        window_elements = resize_elements
            .into_iter()
            .flatten()
            .chain(indicator.into_iter().map(Into::into))
            .chain(window_elements)
            .chain(group_backdrop.into_iter().map(Into::into))
            .collect();
    }

    (window_elements, popup_elements)
}

fn scale_to_center(
    old_geo: &Rectangle<i32, Logical>,
    new_geo: &Rectangle<i32, Logical>,
) -> (f64, Point<i32, Logical>) {
    let scale_w = new_geo.size.w as f64 / old_geo.size.w as f64;
    let scale_h = new_geo.size.h as f64 / old_geo.size.h as f64;

    if scale_w > scale_h {
        (
            scale_h,
            (
                ((new_geo.size.w as f64 - old_geo.size.w as f64 * scale_h) / 2.0).round() as i32,
                0,
            )
                .into(),
        )
    } else {
        (
            scale_w,
            (
                0,
                ((new_geo.size.h as f64 - old_geo.size.h as f64 * scale_w) / 2.0).round() as i32,
            )
                .into(),
        )
    }
}
