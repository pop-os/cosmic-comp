// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render::{element::AsGlowRenderer, BackdropShader, IndicatorShader, Key, GROUP_COLOR},
    shell::{
        element::{
            stack::{CosmicStackRenderElement, MoveResult as StackMoveResult},
            window::CosmicWindowRenderElement,
            CosmicMapped, CosmicMappedRenderElement, CosmicStack, CosmicWindow,
        },
        focus::{
            target::{KeyboardFocusTarget, WindowGroup},
            FocusDirection, FocusStackMut,
        },
        grabs::ResizeEdge,
        layout::Orientation,
        CosmicSurface, OutputNotMapped, OverviewMode,
    },
    utils::prelude::*,
    wayland::{
        handlers::xdg_shell::popup::get_popup_toplevel, protocols::toplevel_info::ToplevelInfoState,
    },
};

use calloop::LoopHandle;
use cosmic_time::{Cubic, Ease, Tween};
use id_tree::{InsertBehavior, MoveBehavior, Node, NodeId, NodeIdError, RemoveBehavior, Tree};
use smithay::{
    backend::renderer::{
        element::{
            utils::{CropRenderElement, Relocate, RelocateRenderElement, RescaleRenderElement},
            AsRenderElements, RenderElement,
        },
        ImportAll, ImportMem, Renderer,
    },
    desktop::{layer_map_for_output, space::SpaceElement, PopupKind},
    input::{pointer::GrabStartData as PointerGrabStartData, Seat},
    output::Output,
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

mod blocker;
mod grabs;
pub use self::blocker::*;
pub use self::grabs::*;

pub const ANIMATION_DURATION: Duration = Duration::from_millis(200);

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

#[derive(Debug, Clone, Default)]
struct TreeQueue {
    trees: VecDeque<(Tree<Data>, Option<TilingBlocker>)>,
    animation_start: Option<Instant>,
}

impl TreeQueue {
    pub fn push_tree(&mut self, tree: Tree<Data>, blocker: Option<TilingBlocker>) {
        self.trees.push_back((tree, blocker))
    }
}

#[derive(Debug, Clone)]
pub struct TilingLayout {
    gaps: (i32, i32),
    queues: HashMap<OutputData, TreeQueue>,
    standby_tree: Option<Tree<Data>>,
    pending_blockers: Vec<TilingBlocker>,
}

#[derive(Debug, Clone)]
pub enum Data {
    Group {
        orientation: Orientation,
        sizes: Vec<i32>,
        last_geometry: Rectangle<i32, Logical>,
        alive: Arc<()>,
    },
    Mapped {
        mapped: CosmicMapped,
        last_geometry: Rectangle<i32, Logical>,
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
            Data::Mapped { .. } => panic!("Adding window to leaf?"),
        }
    }

    fn swap_windows(&mut self, i: usize, j: usize) {
        match self {
            Data::Group { sizes, .. } => {
                sizes.swap(i, j);
            }
            Data::Mapped { .. } => panic!("Swapping windows to a leaf?"),
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
            Data::Mapped { .. } => panic!("Added window to leaf?"),
        }
    }

    fn geometry(&self) -> &Rectangle<i32, Logical> {
        match self {
            Data::Group { last_geometry, .. } => last_geometry,
            Data::Mapped { last_geometry, .. } => last_geometry,
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
            Data::Mapped { last_geometry, .. } => {
                *last_geometry = geo;
            }
        }
    }

    fn len(&self) -> usize {
        match self {
            Data::Group { sizes, .. } => sizes.len(),
            Data::Mapped { .. } => 1,
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
                        queue.push_back((self.standby_tree.take().unwrap_or_else(Tree::new), None));
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
            for blocker in src.trees.iter_mut().flat_map(|(_, blocker)| blocker.take()) {
                self.pending_blockers.push(blocker);
            }
            let (src, _) = src.trees.pop_back().expect("No tree in queue");

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
            dst_queue.push_tree(dst, blocker);
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
        queue.push_tree(tree, blocker);
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

            if let Some((ref node_id, _last_active_window)) = last_active {
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

                let parent_id = tree
                    .get(&node_id)
                    .ok()
                    .and_then(|node| node.parent())
                    .cloned();
                let position = parent_id.as_ref().and_then(|parent_id| {
                    tree.children_ids(&parent_id)
                        .unwrap()
                        .position(|id| id == &node_id)
                });
                let parent_parent_id = parent_id.as_ref().and_then(|parent_id| {
                    tree.get(parent_id)
                        .ok()
                        .and_then(|node| node.parent())
                        .cloned()
                });

                // remove self
                trace!(?mapped, "Remove window.");
                let _ = tree.remove_node(node_id, RemoveBehavior::DropChildren);

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
                            let other_child =
                                tree.children_ids(&id).unwrap().cloned().next().unwrap();
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

                let blocker = TilingLayout::update_positions(&output.output, &mut tree, self.gaps);
                queue.push_tree(tree, blocker);
            }
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

        let Some((node_id, data)) = TilingLayout::currently_focused_node(&mut tree, seat) else {
            return MoveResult::Done
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
                    queue.push_tree(tree, blocker);
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
                FocusedNodeData::Group(focus_stack, alive) => MoveResult::MoveFurther(WindowGroup {
                    node: node_id,
                    output: output.downgrade(),
                    alive,
                    focus_stack,
                }.into()),
            }
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
                queue.push_tree(tree, blocker);
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
                queue.push_tree(tree, blocker);
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
                queue.push_tree(tree, blocker);
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

        if let Some(focused) = TilingLayout::currently_focused_node(tree, seat).or_else(|| {
            TilingLayout::last_active_window(tree, focus_stack)
                .map(|(id, mapped)| (id, FocusedNodeData::Window(mapped)))
        }) {
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
                            Data::Mapped { mapped, .. } => FocusResult::Some(mapped.clone().into()),
                            Data::Group { alive, .. } => FocusResult::Some(
                                WindowGroup {
                                    node: id,
                                    output: output.downgrade(),
                                    alive: Arc::downgrade(alive),
                                    focus_stack: stack,
                                }
                                .into(),
                            ),
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
                                    ((point.x - center.x).powi(2) + (point.y - center.y).powi(2))
                                        .sqrt()
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
                        }
                    }
                } else {
                    node_id = group.clone();
                }
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
        let Some(queue) = self.queues.get_mut(&output) else { return };
        let mut tree = queue.trees.back().unwrap().0.copy_clone();

        if let Some((last_active, _)) = TilingLayout::currently_focused_node(&tree, seat) {
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
                    queue.push_tree(tree, blocker);
                }
            }
        }
    }

    pub fn toggle_stacking<'a>(&mut self, seat: &Seat<State>, mut focus_stack: FocusStackMut) {
        let output = seat.active_output();
        let Some(queue) = self.queues.get_mut(&output) else { return };
        let mut tree = queue.trees.back().unwrap().0.copy_clone();

        if let Some((last_active, last_active_data)) =
            TilingLayout::currently_focused_node(&tree, seat)
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
                            Data::Group { .. } => None,
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
            queue.push_tree(tree, blocker);
        }
    }

    pub fn recalculate(&mut self, output: &Output) {
        let Some(queue) = self.queues.get_mut(output) else { return };
        let mut tree = queue.trees.back().unwrap().0.copy_clone();
        let blocker = TilingLayout::update_positions(&output, &mut tree, self.gaps);
        queue.push_tree(tree, blocker);
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
            self.unmap_window_internal(&dead_window);
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

    pub fn update_animation_state(&mut self, handle: &LoopHandle<'static, crate::state::Data>) {
        for blocker in self.pending_blockers.drain(..) {
            blocker.signal_ready(handle);
        }
        for queue in self.queues.values_mut() {
            if let Some(start) = queue.animation_start {
                let duration_since_start = Instant::now().duration_since(start);
                if duration_since_start > ANIMATION_DURATION {
                    assert!(queue.trees.len() >= 2);
                    let _ = queue.animation_start.take();
                    let _ = queue.trees.pop_front();
                    let _ = queue.trees.front_mut().unwrap().1.take();
                } else {
                    continue;
                }
            }
            if let Some((_, blocker)) = queue.trees.get(1) {
                if blocker
                    .as_ref()
                    .map(TilingBlocker::is_ready)
                    .unwrap_or(true)
                {
                    queue.animation_start = Some(Instant::now());
                    if let Some(blocker) = blocker.as_ref() {
                        blocker.signal_ready(handle)
                    }
                }
            }
        }
    }

    pub fn resize_request(
        &self,
        mapped: &CosmicMapped,
        _seat: &Seat<State>,
        start_data: PointerGrabStartData<State>,
        edges: ResizeEdge,
    ) -> Option<ResizeForkGrab> {
        let (output, mut node_id) = self.queues.iter().find_map(|(output, queue)| {
            let tree = &queue.trees.back().unwrap().0;
            let root_id = tree.root_node_id()?;
            tree.traverse_pre_order_ids(root_id)
                .unwrap()
                .find(|id| tree.get(id).unwrap().data().is_mapped(Some(mapped)))
                .map(|id| (&output.output, id))
        })?;

        let queue = self.queues.get(output).unwrap();
        let tree = &queue.trees.back().unwrap().0;
        while let Some(group_id) = tree.get(&node_id).unwrap().parent() {
            let orientation = tree.get(group_id).unwrap().data().orientation();
            if !((orientation == Orientation::Vertical
                && (edges.contains(ResizeEdge::LEFT) || edges.contains(ResizeEdge::RIGHT)))
                || (orientation == Orientation::Horizontal
                    && (edges.contains(ResizeEdge::TOP) || edges.contains(ResizeEdge::BOTTOM))))
            {
                node_id = group_id.clone();
                continue;
            }

            let node_idx = tree
                .children_ids(group_id)
                .unwrap()
                .position(|id| id == &node_id)
                .unwrap();
            let idx = match edges {
                x if x.intersects(ResizeEdge::TOP_LEFT) => node_idx.checked_sub(1)?,
                _ => node_idx,
            };
            if idx > tree.get(&group_id).unwrap().data().len() {
                return None;
            }

            return Some(ResizeForkGrab::new(
                start_data,
                group_id.clone(),
                output,
                tree.get(&group_id).unwrap().data(),
                idx,
            ));
        }

        None
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
        seat: &Seat<State>,
    ) -> Option<(NodeId, FocusedNodeData)> {
        let mut target = seat.get_keyboard().unwrap().current_focus()?;

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
                if window_group.output == seat.active_output() {
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
                    let node = tree.get_mut(&node_id).unwrap();
                    let data = node.data_mut();
                    if data.is_mapped(None) {
                        geo.loc += (inner, inner).into();
                        geo.size -= (inner * 2, inner * 2).into();
                    }
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
            dst_queue.push_tree(dst, blocker);
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
        indicator_thickness: u8,
    ) -> Result<Vec<CosmicMappedRenderElement<R>>, OutputNotMapped>
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
        let (target_tree, _) = if queue.animation_start.is_some() {
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
                / ANIMATION_DURATION.as_millis() as f32;
            Ease::Cubic(Cubic::Out).tween(percentage)
        } else {
            1.0
        };
        let draw_groups = match overview {
            OverviewMode::Started(_, start) => {
                let percentage = (Instant::now().duration_since(start).as_millis() as f32
                    / ANIMATION_DURATION.as_millis() as f32)
                    .min(1.0);
                Some(Ease::Cubic(Cubic::Out).tween(percentage))
            }
            OverviewMode::Ended(end) => {
                let percentage = (1.0
                    - Instant::now().duration_since(end).as_millis() as f32
                        / ANIMATION_DURATION.as_millis() as f32)
                    .max(0.0);
                if percentage > 0.0 {
                    Some(Ease::Cubic(Cubic::Out).tween(percentage))
                } else {
                    None
                }
            }
            OverviewMode::None => None,
        };

        let mut elements = Vec::new();

        // all gone windows and fade them out
        let old_geometries = if let Some(reference_tree) = reference_tree.as_ref() {
            let (geometries, _) = if let Some(transition) = draw_groups {
                geometries_for_groupview(
                    reference_tree,
                    renderer,
                    non_exclusive_zone,
                    seat, // TODO: Would be better to be an old focus,
                    // but for that we have to associate focus with a tree (and animate focus changes properly)
                    1.0 - transition,
                    transition,
                )
            } else {
                None
            }
            .unzip();

            // all old windows we want to fade out
            elements.extend(render_old_tree(
                reference_tree,
                target_tree,
                renderer,
                geometries.clone(),
                output_scale,
                percentage,
            ));

            geometries
        } else {
            None
        };

        let (geometries, group_elements) = if let Some(transition) = draw_groups {
            geometries_for_groupview(
                target_tree,
                renderer,
                non_exclusive_zone,
                seat,
                transition,
                transition,
            )
        } else {
            None
        }
        .unzip();

        // all alive windows
        elements.extend(render_new_tree(
            target_tree,
            reference_tree,
            renderer,
            geometries,
            old_geometries,
            seat,
            output_scale,
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
        ));

        // tiling hints
        if let Some(group_elements) = group_elements {
            elements.extend(group_elements);
        }

        Ok(elements)
    }
}

const OUTER_GAP: i32 = 8;
const INNER_GAP: i32 = 16;

fn geometries_for_groupview<R>(
    tree: &Tree<Data>,
    renderer: &mut R,
    non_exclusive_zone: Rectangle<i32, Logical>,
    seat: Option<&Seat<State>>,
    alpha: f32,
    transition: f32,
) -> Option<(
    HashMap<NodeId, Rectangle<i32, Logical>>,
    Vec<CosmicMappedRenderElement<R>>,
)>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    CosmicWindowRenderElement<R>: RenderElement<R>,
{
    // we need to recalculate geometry for all elements, if we are drawing groups
    if let Some(root) = tree.root_node_id() {
        let outer_gap: i32 = (OUTER_GAP as f32 * transition).round() as i32;
        let inner_gap: i32 = (INNER_GAP as f32 * transition).round() as i32;

        let mut stack = vec![Rectangle::from_loc_and_size(
            non_exclusive_zone.loc + Point::from((outer_gap, outer_gap)),
            (non_exclusive_zone.size.to_point() - Point::from((outer_gap * 2, outer_gap * 2)))
                .to_size(),
        )];
        let mut elements = Vec::new();
        let mut geometries = HashMap::new();
        let alpha = alpha * transition;

        let focused = seat
            .and_then(|seat| TilingLayout::currently_focused_node(&tree, seat))
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
            if let Some(mut geo) = stack.pop() {
                let node: &Node<Data> = tree.get(&node_id).unwrap();
                let data = node.data();

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

                match data {
                    Data::Group {
                        orientation,
                        last_geometry,
                        sizes,
                        alive,
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

                        if (render_potential_group || render_active_child) && &node_id != root {
                            elements.push(
                                IndicatorShader::element(
                                    renderer,
                                    Key::Group(Arc::downgrade(alive)),
                                    geo,
                                    4,
                                    if render_active_child { 16 } else { 8 },
                                    alpha * if render_potential_group { 0.40 } else { 1.0 },
                                    GROUP_COLOR,
                                )
                                .into(),
                            );
                        }

                        geometries.insert(node_id.clone(), geo);

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
                        }
                    }
                    Data::Mapped { mapped, .. } => {
                        geo.loc += (outer_gap, outer_gap).into();
                        geo.size -= (outer_gap * 2, outer_gap * 2).into();

                        if render_potential_group {
                            elements.push(
                                IndicatorShader::element(
                                    renderer,
                                    mapped.clone(),
                                    geo,
                                    4,
                                    8,
                                    alpha * if render_potential_group { 0.40 } else { 1.0 },
                                    GROUP_COLOR,
                                )
                                .into(),
                            );

                            geo.loc += (outer_gap, outer_gap).into();
                            geo.size -= (outer_gap * 2, outer_gap * 2).into();
                        }

                        if focused
                            .as_ref()
                            .map(|focused_id| {
                                !tree
                                    .ancestor_ids(&node_id)
                                    .unwrap()
                                    .any(|id| id == focused_id)
                            })
                            .unwrap_or(false)
                        {
                            elements.push(
                                BackdropShader::element(
                                    renderer,
                                    mapped.clone(),
                                    geo,
                                    8.,
                                    alpha
                                        * if focused
                                            .as_ref()
                                            .map(|focused_id| focused_id == &node_id)
                                            .unwrap_or(false)
                                        {
                                            0.4
                                        } else {
                                            0.15
                                        },
                                    GROUP_COLOR,
                                )
                                .into(),
                            );
                        }

                        geo.loc += (inner_gap, inner_gap).into();
                        geo.size -= (inner_gap * 2, inner_gap * 2).into();

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
) -> Vec<CosmicMappedRenderElement<R>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    CosmicWindowRenderElement<R>: RenderElement<R>,
    CosmicStackRenderElement<R>: RenderElement<R>,
{
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
            .flat_map(|(mapped, original_geo, scaled_geo)| {
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
                AsRenderElements::<R>::render_elements::<CosmicMappedRenderElement<R>>(
                    mapped,
                    renderer,
                    original_location,
                    Scale::from(output_scale),
                    1.0 - percentage,
                )
                .into_iter()
                .flat_map(|element| match element {
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
                })
                .collect::<Vec<_>>()
            })
            .collect()
    } else {
        Vec::new()
    }
}

fn render_new_tree<R>(
    target_tree: &Tree<Data>,
    reference_tree: Option<&Tree<Data>>,
    renderer: &mut R,
    geometries: Option<HashMap<NodeId, Rectangle<i32, Logical>>>,
    old_geometries: Option<HashMap<NodeId, Rectangle<i32, Logical>>>,
    seat: Option<&Seat<State>>,
    output_scale: f64,
    percentage: f32,
    indicator_thickness: u8,
) -> Vec<CosmicMappedRenderElement<R>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    CosmicWindowRenderElement<R>: RenderElement<R>,
    CosmicStackRenderElement<R>: RenderElement<R>,
{
    let focused = seat
        .and_then(|seat| TilingLayout::currently_focused_node(&target_tree, seat))
        .map(|(id, _)| id);

    let mut group_backdrop = None;

    if let Some(root) = target_tree.root_node_id() {
        let old_geometries = old_geometries.unwrap_or_default();
        let geometries = geometries.unwrap_or_default();
        let mut elements: Vec<CosmicMappedRenderElement<R>> = target_tree
            .traverse_pre_order_ids(root)
            .unwrap()
            .flat_map(|node_id| {
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

                let mut elements = Vec::new();

                if focused == Some(node_id) {
                    if indicator_thickness > 0 || data.is_group() {
                        let mut geo = geo.clone();
                        if data.is_group() {
                            let outer_gap: i32 = (OUTER_GAP as f32 * percentage).round() as i32;
                            geo.loc += (outer_gap, outer_gap).into();
                            geo.size -= (outer_gap * 2, outer_gap * 2).into();

                            group_backdrop = Some(
                                BackdropShader::element(
                                    renderer,
                                    match data {
                                        Data::Group { alive, .. } => {
                                            Key::Group(Arc::downgrade(alive))
                                        }
                                        _ => unreachable!(),
                                    },
                                    geo,
                                    8.,
                                    0.4,
                                    GROUP_COLOR,
                                )
                                .into(),
                            );
                        }

                        let element = IndicatorShader::focus_element(
                            renderer,
                            match data {
                                Data::Mapped { mapped, .. } => mapped.clone().into(),
                                Data::Group { alive, .. } => Key::Group(Arc::downgrade(alive)),
                            },
                            geo,
                            if data.is_group() {
                                4
                            } else {
                                indicator_thickness
                            },
                            1.0,
                        );
                        elements.push(element.into());
                    }
                }

                if let Data::Mapped { mapped, .. } = data {
                    let original_location = (original_geo.loc - mapped.geometry().loc)
                        .to_physical_precise_round(output_scale);

                    elements.extend(
                        AsRenderElements::<R>::render_elements::<CosmicMappedRenderElement<R>>(
                            mapped,
                            renderer,
                            original_location,
                            Scale::from(output_scale),
                            alpha,
                        )
                        .into_iter()
                        .flat_map(|element| match element {
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
                        }),
                    );
                }

                elements
            })
            .collect();
        elements.extend(group_backdrop);
        elements
    } else {
        Vec::new()
    }
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
