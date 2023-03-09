// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render::{element::AsGlowRenderer, IndicatorShader},
    shell::{
        element::{window::CosmicWindowRenderElement, CosmicMapped, CosmicMappedRenderElement},
        focus::{
            target::{KeyboardFocusTarget, WindowGroup},
            FocusDirection,
        },
        grabs::ResizeEdge,
        layout::Orientation,
        CosmicSurface, OutputNotMapped,
    },
    utils::prelude::*,
    wayland::{
        handlers::xdg_shell::popup::get_popup_toplevel, protocols::toplevel_info::ToplevelInfoState,
    },
};

use id_tree::{InsertBehavior, MoveBehavior, Node, NodeId, NodeIdError, RemoveBehavior, Tree};
use smithay::{
    backend::renderer::{
        element::{AsRenderElements, RenderElement},
        ImportAll, ImportMem, Renderer,
    },
    desktop::{layer_map_for_output, space::SpaceElement, PopupKind},
    input::{pointer::GrabStartData as PointerGrabStartData, Seat},
    output::Output,
    utils::{IsAlive, Logical, Point, Rectangle, Scale},
    wayland::seat::WaylandFocus,
};
use std::{borrow::Borrow, collections::HashMap, hash::Hash, sync::Arc};
use tracing::trace;

mod grabs;
pub use self::grabs::*;

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

#[derive(Debug, Clone, PartialEq)]
pub enum FocusResult {
    None,
    Handled,
    Some(KeyboardFocusTarget),
}

#[derive(Debug, Clone)]
pub struct TilingLayout {
    gaps: (i32, i32),
    trees: HashMap<OutputData, Tree<Data>>,
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
                for size in sizes.iter_mut() {
                    *size +=
                        ((old_size as f64 / last_length as f64) * (*size as f64)).round() as i32;
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

impl TilingLayout {
    pub fn new(gaps: (u8, u8)) -> TilingLayout {
        TilingLayout {
            gaps: (gaps.0 as i32, gaps.1 as i32),
            trees: HashMap::new(),
        }
    }
}

impl TilingLayout {
    pub fn map_output(&mut self, output: &Output, location: Point<i32, Logical>) {
        if !self.trees.contains_key(output) {
            self.trees.insert(
                OutputData {
                    output: output.clone(),
                    location,
                },
                Tree::new(),
            );
        } else {
            let tree = self.trees.remove(output).unwrap();
            self.trees.insert(
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
        if let Some(src) = self.trees.remove(output) {
            // TODO: expects last remaining output
            let Some((new_output, dst)) = self.trees.iter_mut().next() else { return; };
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
            TilingLayout::merge_trees(src, dst, orientation);
            self.refresh()
        }
    }

    pub fn map<'a>(
        &mut self,
        window: CosmicMapped,
        seat: &Seat<State>,
        focus_stack: impl Iterator<Item = &'a CosmicMapped> + 'a,
    ) {
        let output = seat.active_output();
        window.output_enter(&output, window.bbox());
        self.map_internal(window, &output, Some(focus_stack));
        self.refresh();
    }

    fn map_internal<'a>(
        &mut self,
        window: impl Into<CosmicMapped>,
        output: &Output,
        focus_stack: Option<impl Iterator<Item = &'a CosmicMapped> + 'a>,
    ) {
        let tree = self.trees.get_mut(output).expect("Output not mapped?");
        let window = window.into();
        let new_window = Node::new(Data::Mapped {
            mapped: window.clone(),
            last_geometry: Rectangle::from_loc_and_size((0, 0), (100, 100)),
        });

        let last_active =
            focus_stack.and_then(|focus_stack| TilingLayout::last_active_window(tree, focus_stack));

        let window_id = if let Some((_last_active_window, ref node_id)) = last_active {
            let orientation = {
                let window_size = tree.get(node_id).unwrap().data().geometry().size;
                if window_size.w > window_size.h {
                    Orientation::Vertical
                } else {
                    Orientation::Horizontal
                }
            };
            let new_id = tree.insert(new_window, InsertBehavior::AsRoot).unwrap();
            TilingLayout::new_group(tree, &node_id, &new_id, orientation).unwrap();
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
                TilingLayout::new_group(tree, &root_id, &new_id, orientation).unwrap();
                new_id
            } else {
                tree.insert(new_window, InsertBehavior::AsRoot).unwrap()
            }
        };

        *window.tiling_node_id.lock().unwrap() = Some(window_id);
    }

    pub fn unmap(&mut self, window: &CosmicMapped) -> Option<Output> {
        let output = {
            let node_id = window.tiling_node_id.lock().unwrap().clone()?;
            self.trees
                .iter()
                .find(|(_, tree)| {
                    tree.get(&node_id)
                        .map(|node| node.data().is_mapped(Some(window)))
                        .unwrap_or(false)
                })
                .map(|(o, _)| o.output.clone())?
        };

        self.unmap_window_internal(window);
        window.output_leave(&output);
        window.set_tiled(false);
        self.refresh();
        Some(output)
    }

    fn unmap_window_internal(&mut self, mapped: &CosmicMapped) {
        if let Some(node_id) = mapped.tiling_node_id.lock().unwrap().as_ref() {
            if let Some(tree) = self.trees.values_mut().find(|tree| {
                tree.get(node_id)
                    .map(|node| node.data().is_mapped(Some(mapped)))
                    .unwrap_or(false)
            }) {
                let parent_id = tree
                    .get(&node_id)
                    .ok()
                    .and_then(|node| node.parent())
                    .cloned();
                let position = parent_id.as_ref().and_then(|parent_id| {
                    tree.children_ids(&parent_id)
                        .unwrap()
                        .position(|id| id == node_id)
                });
                let parent_parent_id = parent_id.as_ref().and_then(|parent_id| {
                    tree.get(parent_id)
                        .ok()
                        .and_then(|node| node.parent())
                        .cloned()
                });

                // remove self
                trace!(?mapped, "Remove window.");
                let _ = tree.remove_node(node_id.clone(), RemoveBehavior::DropChildren);

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
            }
        }
    }

    pub fn output_for_element(&self, elem: &CosmicMapped) -> Option<&Output> {
        self.mapped().find_map(|(o, m, _)| (m == elem).then_some(o))
    }

    pub fn element_geometry(&self, elem: &CosmicMapped) -> Option<Rectangle<i32, Logical>> {
        if let Some(id) = elem.tiling_node_id.lock().unwrap().as_ref() {
            if let Some(output) = self.output_for_element(elem) {
                let (output_data, tree) = self.trees.get_key_value(output).unwrap();
                let node = tree.get(id).ok()?;
                let data = node.data();
                assert!(data.is_mapped(Some(elem)));
                let mut geo = *data.geometry();
                geo.loc += output_data.location;
                return Some(geo);
            }
        }
        None
    }

    pub fn move_current_window<'a>(
        &mut self,
        direction: Direction,
        seat: &Seat<State>,
    ) -> Option<CosmicMapped /*TODO move window groups across screens?*/> {
        let output = seat.active_output();
        let tree = self.trees.get_mut(&output).unwrap();

        let node_id = match TilingLayout::currently_focused_node(tree, seat) {
            Some(node_id) => node_id,
            None => {
                return None;
            }
        };

        let mut child_id = node_id.clone();
        // Without a parent to start with, just return
        let Some(og_parent) = tree.get(&node_id).unwrap().parent().cloned() else {
            let data = tree.get(&node_id).unwrap().data();
            assert!(data.is_mapped(None));
            return match data {
                Data::Mapped { mapped, .. } => Some(mapped.clone()),
                _ => unreachable!(),
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

            // if the orientation does not match, we want to create a new group with our parent.
            if matches!(
                (orientation, direction),
                (Orientation::Horizontal, Direction::Right)
                    | (Orientation::Horizontal, Direction::Left)
                    | (Orientation::Vertical, Direction::Up)
                    | (Orientation::Vertical, Direction::Down)
            ) {
                TilingLayout::new_group(
                    tree,
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
                self.refresh();
                return None;
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
                self.refresh();
                return None;
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
                // if we can, we need to check the next element and move "into" it (down)
                let next_child_id = tree
                    .children_ids(&parent)
                    .unwrap()
                    .nth(next_idx)
                    .unwrap()
                    .clone();
                if tree.get(&next_child_id).unwrap().data().is_group() {
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
                                    tree,
                                    &old_id,
                                    &node_id,
                                    !group_orientation,
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
                            }
                        }
                    };
                    tree.get_mut(&og_parent)
                        .unwrap()
                        .data_mut()
                        .remove_window(og_idx);
                } else if len == 2 && child_id == node_id {
                    // if we are just us two in the group, lets swap
                    tree.make_nth_sibling(&node_id, next_idx).unwrap();
                    // also swap sizes
                    tree.get_mut(&og_parent)
                        .unwrap()
                        .data_mut()
                        .swap_windows(idx, next_idx);
                } else {
                    // else we make a new fork
                    TilingLayout::new_group(tree, &next_child_id, &node_id, orientation).unwrap();
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
                }
                self.refresh();
                return None;
            }

            // We have reached the end of our parent group, try to move out even higher.
            maybe_parent = tree.get(&parent).unwrap().parent().cloned();
            child_id = parent.clone();
        }

        match tree.get(&node_id).unwrap().data() {
            Data::Mapped { mapped, .. } => Some(mapped.clone()),
            Data::Group { .. } => None, // TODO move groups to other screens
        }
    }

    pub fn next_focus<'a>(
        &mut self,
        direction: FocusDirection,
        seat: &Seat<State>,
        focus_stack: impl Iterator<Item = &'a CosmicMapped> + 'a,
    ) -> FocusResult {
        let output = seat.active_output();
        let tree = self.trees.get_mut(&output).unwrap();

        // TODO: Rather use something like seat.current_keyboard_focus
        // TODO https://github.com/Smithay/smithay/pull/777
        if let Some(last_active) = TilingLayout::last_active_window(tree, focus_stack) {
            let (last_window, last_node_id) = last_active;

            // stacks may handle focus internally
            if last_window.handle_focus(direction) {
                return FocusResult::Handled;
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
        focus_stack: impl Iterator<Item = &'a CosmicMapped> + 'a,
    ) {
        let output = seat.active_output();
        let tree = self.trees.get_mut(&output).unwrap();
        if let Some((_, last_active)) = TilingLayout::last_active_window(tree, focus_stack) {
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
                }
            }
        }
        self.refresh();
    }

    pub fn refresh<'a>(&mut self) {
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
        // flatten trees
        for tree in self.trees.values_mut() {
            let root_id = match tree.root_node_id() {
                Some(root) => root,
                None => {
                    continue;
                }
            };
            for node_id in tree
                .traverse_pre_order_ids(root_id)
                .unwrap()
                .collect::<Vec<_>>()
                .into_iter()
            {
                let node = tree.get(&node_id).unwrap();
                let data = node.data();
                if data.is_group() && data.len() == 1 {
                    // RemoveBehavior::LiftChildren sadly does not what we want: lifting them into the same place.
                    // So we need to fix that manually..
                    let child_id = tree
                        .children_ids(&node_id)
                        .unwrap()
                        .cloned()
                        .next()
                        .unwrap();
                    let idx = node.parent().map(|parent_id| {
                        tree.children_ids(&parent_id)
                            .unwrap()
                            .position(|id| id == &node_id)
                            .unwrap()
                    });
                    tree.remove_node(node_id, RemoveBehavior::LiftChildren)
                        .unwrap();
                    if let Some(idx) = idx {
                        tree.make_nth_sibling(&child_id, idx).unwrap();
                    } else {
                        // additionally `RemoveBehavior::LiftChildren` doesn't work, when removing the root-node,
                        // even with just one child. *sigh*
                        tree.move_node(&child_id, MoveBehavior::ToRoot).unwrap();
                    }
                }
            }
        }
        for (_, mapped, _) in self.mapped() {
            mapped.refresh();
        }
        TilingLayout::update_space_positions(&mut self.trees, self.gaps);
    }

    pub fn resize_request(
        &self,
        mapped: &CosmicMapped,
        _seat: &Seat<State>,
        start_data: PointerGrabStartData<State>,
        edges: ResizeEdge,
    ) -> Option<ResizeForkGrab> {
        let (output, mut node_id) = self.trees.iter().find_map(|(output, tree)| {
            let root_id = tree.root_node_id()?;
            tree.traverse_pre_order_ids(root_id)
                .unwrap()
                .find(|id| tree.get(id).unwrap().data().is_mapped(Some(mapped)))
                .map(|id| (&output.output, id))
        })?;

        let tree = self.trees.get(output).unwrap();
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
                x if x.intersects(ResizeEdge::TOP_LEFT) => node_idx - 1,
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
        tree: &mut Tree<Data>,
        mut focus_stack: impl Iterator<Item = &'a CosmicMapped>,
    ) -> Option<(CosmicMapped, NodeId)> {
        focus_stack
            .find_map(|mapped| tree.root_node_id()
                .and_then(|root| tree.traverse_pre_order_ids(root).unwrap()
                    .find(|id| matches!(tree.get(id).map(|n| n.data()), Ok(Data::Mapped { mapped: m, .. }) if m == mapped))
                ).map(|id| (mapped.clone(), id))
            )
    }

    fn currently_focused_node(tree: &mut Tree<Data>, seat: &Seat<State>) -> Option<NodeId> {
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
                    return Some(node_id);
                }
            }
            KeyboardFocusTarget::Group(window_group) => {
                if window_group.output == seat.active_output() {
                    let node = tree.get(&window_group.node).ok()?;
                    if node.data().is_group() {
                        return Some(window_group.node);
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

    fn update_space_positions(trees: &mut HashMap<OutputData, Tree<Data>>, gaps: (i32, i32)) {
        #[cfg(feature = "debug")]
        puffin::profile_function!();

        let (outer, inner) = gaps;
        for (output, tree) in trees
            .iter_mut()
            .map(|(output_data, tree)| (&output_data.output, tree))
        {
            if let Some(root) = tree.root_node_id() {
                let mut geo = layer_map_for_output(&output).non_exclusive_zone();
                geo.loc.x += outer;
                geo.loc.y += outer;
                geo.size.w -= outer * 2;
                geo.size.h -= outer * 2;
                let mut stack = vec![geo];

                for node_id in tree
                    .traverse_pre_order_ids(root)
                    .unwrap()
                    .collect::<Vec<_>>()
                    .into_iter()
                {
                    let node = tree.get_mut(&node_id).unwrap();
                    if let Some(mut geo) = stack.pop() {
                        let data = node.data_mut();
                        match data {
                            Data::Group {
                                orientation, sizes, ..
                            } => {
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
                                data.update_geometry(geo);
                            }
                            Data::Mapped { mapped, .. } => {
                                geo.loc += (inner, inner).into();
                                if !(mapped.is_fullscreen() || mapped.is_maximized()) {
                                    mapped.set_tiled(true);
                                    let size = (geo.size.w - inner * 2, geo.size.h - inner * 2);
                                    let internal_geometry = Rectangle::from_loc_and_size(
                                        geo.loc + output.geometry().loc,
                                        size,
                                    );
                                    if mapped.geometry() != internal_geometry {
                                        mapped.set_geometry(internal_geometry);
                                        mapped.configure();
                                    }
                                }
                                data.update_geometry(geo);
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn mapped(&self) -> impl Iterator<Item = (&Output, &CosmicMapped, Point<i32, Logical>)> {
        self.trees
            .iter()
            .flat_map(|(output_data, tree)| {
                if let Some(root) = tree.root_node_id() {
                    Some(
                        tree.traverse_pre_order(root)
                            .unwrap()
                            .filter(|node| node.data().is_mapped(None))
                            .filter(|node| match node.data() {
                                Data::Mapped { mapped, .. } => mapped.is_activated(),
                                _ => unreachable!(),
                            })
                            .map(|node| match node.data() {
                                Data::Mapped {
                                    mapped,
                                    last_geometry,
                                    ..
                                } => (
                                    &output_data.output,
                                    mapped,
                                    output_data.location + last_geometry.loc,
                                ),
                                _ => unreachable!(),
                            })
                            .chain(
                                tree.traverse_pre_order(root)
                                    .unwrap()
                                    .filter(|node| node.data().is_mapped(None))
                                    .filter(|node| match node.data() {
                                        Data::Mapped { mapped, .. } => !mapped.is_activated(),
                                        _ => unreachable!(),
                                    })
                                    .map(|node| match node.data() {
                                        Data::Mapped {
                                            mapped,
                                            last_geometry,
                                            ..
                                        } => (
                                            &output_data.output,
                                            mapped,
                                            output_data.location + last_geometry.loc,
                                        ),
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
    ) -> impl Iterator<Item = (Output, CosmicSurface, Point<i32, Logical>)> + '_ {
        self.mapped().flat_map(|(output, mapped, loc)| {
            mapped
                .windows()
                .map(move |(w, p)| (output.clone(), w, p + loc))
        })
    }

    pub fn merge(&mut self, other: TilingLayout) {
        for (output_data, src) in other.trees {
            let mut dst = self.trees.entry(output_data.clone()).or_default();
            let orientation = match output_data.output.geometry().size {
                x if x.w >= x.h => Orientation::Vertical,
                _ => Orientation::Horizontal,
            };
            TilingLayout::merge_trees(src, &mut dst, orientation);
        }
        self.refresh();
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
        focused: Option<&CosmicMapped>,
        indicator_thickness: u8,
    ) -> Result<Vec<CosmicMappedRenderElement<R>>, OutputNotMapped>
    where
        R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
        <R as Renderer>::TextureId: 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        CosmicWindowRenderElement<R>: RenderElement<R>,
    {
        #[cfg(feature = "debug")]
        puffin::profile_function!();

        let output_scale = output.current_scale().fractional_scale();

        if !self.trees.contains_key(output) {
            return Err(OutputNotMapped);
        }

        Ok(self
            .trees
            .iter()
            .flat_map(|(output_data, tree)| {
                if &output_data.output != output {
                    return None;
                }

                if let Some(root) = tree.root_node_id() {
                    Some(
                        tree.traverse_pre_order(root)
                            .unwrap()
                            .filter(|node| node.data().is_mapped(None))
                            .filter(|node| match node.data() {
                                Data::Mapped { mapped, .. } => mapped.is_activated(),
                                _ => unreachable!(),
                            })
                            .map(|node| match node.data() {
                                Data::Mapped {
                                    mapped,
                                    last_geometry,
                                    ..
                                } => (mapped, last_geometry.loc),
                                _ => unreachable!(),
                            })
                            .chain(
                                tree.traverse_pre_order(root)
                                    .unwrap()
                                    .filter(|node| node.data().is_mapped(None))
                                    .filter(|node| match node.data() {
                                        Data::Mapped { mapped, .. } => !mapped.is_activated(),
                                        _ => unreachable!(),
                                    })
                                    .map(|node| match node.data() {
                                        Data::Mapped {
                                            mapped,
                                            last_geometry,
                                            ..
                                        } => (mapped, last_geometry.loc),
                                        _ => unreachable!(),
                                    }),
                            ),
                    )
                } else {
                    None
                }
            })
            .flatten()
            .flat_map(|(mapped, loc)| {
                let mut elements =
                    AsRenderElements::<R>::render_elements::<CosmicMappedRenderElement<R>>(
                        mapped,
                        renderer,
                        loc.to_physical_precise_round(output_scale)
                            - mapped
                                .geometry()
                                .loc
                                .to_physical_precise_round(output_scale),
                        Scale::from(output_scale),
                    );
                if focused == Some(mapped) {
                    if indicator_thickness > 0 {
                        let element = IndicatorShader::element(
                            renderer,
                            Rectangle::from_loc_and_size(loc, mapped.geometry().size),
                            indicator_thickness,
                        );
                        elements.insert(0, element.into());
                    }
                }
                elements
            })
            .collect::<Vec<_>>())
    }
}
