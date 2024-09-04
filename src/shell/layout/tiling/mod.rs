// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render::{
        element::AsGlowRenderer, BackdropShader, IndicatorShader, Key, SplitRenderElements, Usage,
        ACTIVE_GROUP_COLOR, GROUP_COLOR,
    },
    shell::{
        element::{
            resize_indicator::ResizeIndicator,
            stack::{
                CosmicStackRenderElement, MoveResult as StackMoveResult,
                TAB_HEIGHT as STACK_TAB_HEIGHT,
            },
            swap_indicator::SwapIndicator,
            window::CosmicWindowRenderElement,
            CosmicMapped, CosmicMappedRenderElement, CosmicStack, CosmicWindow,
        },
        focus::{
            target::{KeyboardFocusTarget, PointerFocusTarget, WindowGroup},
            FocusStackMut,
        },
        grabs::ResizeEdge,
        layout::Orientation,
        CosmicSurface, Direction, FocusResult, MoveResult, OutputNotMapped, OverviewMode,
        ResizeMode, Trigger,
    },
    utils::{prelude::*, tween::EaseRectangle},
    wayland::{
        handlers::xdg_shell::popup::get_popup_toplevel,
        protocols::{
            toplevel_info::{
                toplevel_enter_output, toplevel_enter_workspace, toplevel_leave_output,
                toplevel_leave_workspace,
            },
            workspace::WorkspaceHandle,
        },
    },
};

use cosmic_settings_config::shortcuts::action::{FocusDirection, ResizeDirection};
use id_tree::{InsertBehavior, MoveBehavior, Node, NodeId, NodeIdError, RemoveBehavior, Tree};
use keyframe::{
    ease,
    functions::{EaseInOutCubic, Linear},
};
use smithay::{
    backend::renderer::{
        element::{
            utils::{
                constrain_render_elements, ConstrainAlign, ConstrainScaleBehavior,
                RescaleRenderElement,
            },
            AsRenderElements, Id, RenderElement,
        },
        glow::GlowRenderer,
        ImportAll, ImportMem, Renderer,
    },
    desktop::{layer_map_for_output, space::SpaceElement, PopupKind},
    input::Seat,
    output::Output,
    reexports::wayland_server::Client,
    utils::{IsAlive, Logical, Point, Rectangle, Scale, Size},
    wayland::{compositor::add_blocker, seat::WaylandFocus},
};
use std::{
    collections::{HashMap, VecDeque},
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
pub const MINIMIZE_ANIMATION_DURATION: Duration = Duration::from_millis(320);
pub const MOUSE_ANIMATION_DELAY: Duration = Duration::from_millis(150);
pub const INITIAL_MOUSE_ANIMATION_DELAY: Duration = Duration::from_millis(500);

#[derive(Debug, Clone, PartialEq)]
pub struct NodeDesc {
    pub handle: WorkspaceHandle,
    pub node: NodeId,
    pub stack_window: Option<CosmicSurface>,
}

#[derive(Debug, Clone, PartialEq)]
enum TargetZone {
    Initial,
    InitialPlaceholder(NodeId),
    WindowStack(NodeId, Rectangle<i32, Local>),
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
    output: Output,
    queue: TreeQueue,
    pending_blockers: Vec<TilingBlocker>,
    placeholder_id: Id,
    swapping_stack_surface_id: Id,
    last_overview_hover: Option<(Option<Instant>, TargetZone)>,
    pub theme: cosmic::Theme,
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
        last_geometry: Rectangle<i32, Local>,
        alive: Arc<()>,
        pill_indicator: Option<PillIndicator>,
    },
    Mapped {
        mapped: CosmicMapped,
        last_geometry: Rectangle<i32, Local>,
        minimize_rect: Option<Rectangle<i32, Local>>,
    },
    Placeholder {
        last_geometry: Rectangle<i32, Local>,
        initial_placeholder: bool,
    },
}

impl Data {
    fn new_group(orientation: Orientation, geo: Rectangle<i32, Local>) -> Data {
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

    fn geometry(&self) -> &Rectangle<i32, Local> {
        match self {
            Data::Group { last_geometry, .. } => last_geometry,
            Data::Mapped { last_geometry, .. } => last_geometry,
            Data::Placeholder { last_geometry, .. } => last_geometry,
        }
    }

    fn update_geometry(&mut self, geo: Rectangle<i32, Local>) {
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

                // fix rounding issues
                if sum != new_length {
                    let diff = new_length - sum;
                    *sizes.last_mut().unwrap() += diff;
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

#[derive(Debug)]
pub struct MinimizedTilingState {
    pub parent: Option<id_tree::NodeId>,
    pub sibling: Option<id_tree::NodeId>,
    pub orientation: Orientation,
    pub idx: usize,
    pub sizes: Vec<i32>,
}

impl TilingLayout {
    pub fn new(theme: cosmic::Theme, output: &Output) -> TilingLayout {
        TilingLayout {
            queue: TreeQueue {
                trees: {
                    let mut queue = VecDeque::new();
                    queue.push_back((Tree::new(), Duration::ZERO, None));
                    queue
                },
                animation_start: None,
            },
            output: output.clone(),
            pending_blockers: Vec::new(),
            placeholder_id: Id::new(),
            swapping_stack_surface_id: Id::new(),
            last_overview_hover: None,
            theme,
        }
    }

    pub fn set_output(&mut self, output: &Output) {
        let gaps = self.gaps();
        let mut tree = self.queue.trees.back().unwrap().0.copy_clone();

        for node in tree
            .root_node_id()
            .and_then(|root_id| tree.traverse_pre_order(root_id).ok())
            .into_iter()
            .flatten()
        {
            if let Data::Mapped { mapped, .. } = node.data() {
                mapped.output_leave(&self.output);
                mapped.output_enter(output, mapped.bbox());
            }
        }

        let blocker = TilingLayout::update_positions(output, &mut tree, gaps);
        self.queue.push_tree(tree, None, blocker);
        self.output = output.clone();
    }

    pub fn map<'a>(
        &mut self,
        window: CosmicMapped,
        focus_stack: Option<impl Iterator<Item = &'a CosmicMapped> + 'a>,
        direction: Option<Direction>,
    ) {
        window.output_enter(&self.output, window.bbox());
        window.set_bounds(self.output.geometry().size.as_logical());
        self.map_internal(window, focus_stack, direction, None);
    }

    pub fn map_internal<'a>(
        &mut self,
        window: impl Into<CosmicMapped>,
        focus_stack: Option<impl Iterator<Item = &'a CosmicMapped> + 'a>,
        direction: Option<Direction>,
        minimize_rect: Option<Rectangle<i32, Local>>,
    ) {
        let gaps = self.gaps();

        let mut tree = self.queue.trees.back().unwrap().0.copy_clone();
        let last_active = focus_stack
            .and_then(|focus_stack| TilingLayout::last_active_window(&mut tree, focus_stack))
            .map(|(node_id, _)| node_id);
        let duration = if minimize_rect.is_some() {
            MINIMIZE_ANIMATION_DURATION
        } else {
            ANIMATION_DURATION
        };

        TilingLayout::map_to_tree(
            &mut tree,
            window,
            &self.output,
            last_active,
            direction,
            minimize_rect,
        );
        let blocker = TilingLayout::update_positions(&self.output, &mut tree, gaps);
        self.queue.push_tree(tree, duration, blocker);
    }

    pub fn remap_minimized<'a>(
        &mut self,
        window: CosmicMapped,
        from: Rectangle<i32, Local>,
        tiling_state: Option<MinimizedTilingState>,
        focus_stack: Option<impl Iterator<Item = &'a CosmicMapped> + 'a>,
    ) {
        window.set_minimized(false);
        let gaps = self.gaps();
        let mut tree = self.queue.trees.back().unwrap().0.copy_clone();

        if let Some(MinimizedTilingState {
            parent,
            sibling,
            orientation,
            idx,
            mut sizes,
        }) = tiling_state
        {
            if let Some(node) = parent.as_ref().and_then(|parent| tree.get_mut(parent).ok()) {
                if let Data::Group {
                    orientation: current_orientation,
                    sizes: current_sizes,
                    ..
                } = node.data_mut()
                {
                    let parent_id = parent.unwrap();

                    if *current_orientation == orientation && sizes.len() == current_sizes.len() + 1
                    {
                        let previous_length: i32 = sizes.iter().copied().sum();
                        let new_length: i32 = current_sizes.iter().copied().sum();
                        if previous_length != new_length {
                            // rescale sizes
                            sizes.iter_mut().for_each(|len| {
                                *len = (((*len as f64) / (previous_length as f64))
                                    * (new_length as f64))
                                    .round() as i32;
                            });
                            let sum: i32 = sizes.iter().sum();

                            // fix rounding issues
                            if sum != new_length {
                                let diff = new_length - sum;
                                *sizes.last_mut().unwrap() += diff;
                            }
                        }
                    }

                    *current_sizes = sizes;
                    let new_node = Node::new(Data::Mapped {
                        mapped: window.clone(),
                        last_geometry: Rectangle::from_loc_and_size((0, 0), (100, 100)),
                        minimize_rect: Some(from),
                    });
                    let new_id = tree
                        .insert(new_node, InsertBehavior::UnderNode(&parent_id))
                        .unwrap();
                    tree.make_nth_sibling(&new_id, idx).unwrap();
                    *window.tiling_node_id.lock().unwrap() = Some(new_id);

                    let blocker = TilingLayout::update_positions(&self.output, &mut tree, gaps);
                    self.queue
                        .push_tree(tree, MINIMIZE_ANIMATION_DURATION, blocker);
                    return;
                }
            }

            if sibling
                .as_ref()
                .is_some_and(|sibling| tree.get(&sibling).is_ok())
            {
                let sibling_id = sibling.unwrap();
                let new_node = Node::new(Data::Mapped {
                    mapped: window.clone(),
                    last_geometry: Rectangle::from_loc_and_size((0, 0), (100, 100)),
                    minimize_rect: Some(from),
                });

                let new_id = tree.insert(new_node, InsertBehavior::AsRoot).unwrap();
                let group_id =
                    TilingLayout::new_group(&mut tree, &sibling_id, &new_id, orientation).unwrap();
                tree.make_nth_sibling(&new_id, idx).unwrap();
                if let Data::Group {
                    sizes: default_sizes,
                    last_geometry,
                    ..
                } = tree.get_mut(&group_id).unwrap().data_mut()
                {
                    match orientation {
                        Orientation::Horizontal => {
                            last_geometry.size.h = sizes.iter().copied().sum()
                        }
                        Orientation::Vertical => last_geometry.size.w = sizes.iter().copied().sum(),
                    };
                    *default_sizes = sizes;
                }

                *window.tiling_node_id.lock().unwrap() = Some(new_id);

                let blocker = TilingLayout::update_positions(&self.output, &mut tree, gaps);
                self.queue
                    .push_tree(tree, MINIMIZE_ANIMATION_DURATION, blocker);
                return;
            }
        }

        // else add as new_window
        self.map_internal(window, focus_stack, None, Some(from));
    }

    fn map_to_tree(
        mut tree: &mut Tree<Data>,
        window: impl Into<CosmicMapped>,
        output: &Output,
        node: Option<NodeId>,
        direction: Option<Direction>,
        minimize_rect: Option<Rectangle<i32, Local>>,
    ) {
        let window = window.into();
        let new_window = Node::new(Data::Mapped {
            mapped: window.clone(),
            last_geometry: Rectangle::from_loc_and_size((0, 0), (100, 100)),
            minimize_rect,
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
            if let Some(ref node_id) = node {
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

    pub fn replace_window(&mut self, old: &CosmicMapped, new: &CosmicMapped) {
        let gaps = self.gaps();
        let Some(old_id) = old.tiling_node_id.lock().unwrap().clone() else {
            return;
        };
        let mut tree = self.queue.trees.back().unwrap().0.copy_clone();

        if let Ok(node) = tree.get_mut(&old_id) {
            let data = node.data_mut();
            match data {
                Data::Mapped { mapped, .. } => {
                    assert_eq!(mapped, old);
                    *mapped = new.clone();
                    *new.tiling_node_id.lock().unwrap() = Some(old_id);
                    *old.tiling_node_id.lock().unwrap() = None;
                }
                _ => unreachable!("Tiling id points to group"),
            }

            old.output_leave(&self.output);
            new.output_enter(&self.output, new.bbox());

            let blocker = TilingLayout::update_positions(&self.output, &mut tree, gaps);
            self.queue.push_tree(tree, ANIMATION_DURATION, blocker);
        }
    }

    pub fn move_tree<'a>(
        this: &mut Self,
        other: &mut Self,
        other_handle: &WorkspaceHandle,
        seat: &Seat<State>,
        focus_stack: impl Iterator<Item = &'a CosmicMapped> + 'a,
        desc: NodeDesc,
    ) -> Option<KeyboardFocusTarget> {
        let this_handle = &desc.handle;
        let mut this_tree = this.queue.trees.back().unwrap().0.copy_clone();
        let mut other_tree = other.queue.trees.back().unwrap().0.copy_clone();

        match desc.stack_window {
            Some(stack_surface) => {
                let node = this_tree.get(&desc.node).ok()?;
                let Data::Mapped {
                    mapped: this_mapped,
                    ..
                } = node.data()
                else {
                    return None;
                };
                let this_stack = this_mapped.stack_ref()?;
                this_stack.remove_window(&stack_surface);
                if !this_stack.alive() {
                    this.unmap(&this_mapped);
                }

                let mapped: CosmicMapped =
                    CosmicWindow::new(stack_surface, this_stack.loop_handle(), this.theme.clone())
                        .into();
                if this.output != other.output {
                    mapped.output_leave(&this.output);
                    mapped.output_enter(&other.output, mapped.bbox());
                    for (ref surface, _) in mapped.windows() {
                        toplevel_leave_output(surface, &this.output);
                        toplevel_enter_output(surface, &other.output);
                    }
                }
                for (ref surface, _) in mapped.windows() {
                    toplevel_leave_workspace(surface, this_handle);
                    toplevel_enter_workspace(surface, other_handle);
                }

                mapped.set_tiled(true);
                other.map(mapped.clone(), Some(focus_stack), None);
                return Some(KeyboardFocusTarget::Element(mapped));
            }
            None => {
                let node = this_tree.get(&desc.node).ok()?;
                let mut children = node
                    .children()
                    .into_iter()
                    .map(|child_id| (desc.node.clone(), child_id.clone()))
                    .collect::<Vec<_>>();
                let node = Node::new(node.data().clone());

                let id = match other_tree.root_node_id() {
                    None => other_tree.insert(node, InsertBehavior::AsRoot).unwrap(),
                    Some(_) => {
                        let focused_node = seat
                            .get_keyboard()
                            .unwrap()
                            .current_focus()
                            .and_then(|target| {
                                TilingLayout::currently_focused_node(&other_tree, target)
                            })
                            .map(|(id, _)| id)
                            .unwrap_or(other_tree.root_node_id().unwrap().clone());
                        let orientation = {
                            let window_size = other_tree
                                .get(&focused_node)
                                .unwrap()
                                .data()
                                .geometry()
                                .size;
                            if window_size.w > window_size.h {
                                Orientation::Vertical
                            } else {
                                Orientation::Horizontal
                            }
                        };
                        let id = other_tree.insert(node, InsertBehavior::AsRoot).unwrap();
                        TilingLayout::new_group(&mut other_tree, &focused_node, &id, orientation)
                            .unwrap();
                        id
                    }
                };

                for (ref mut parent_id, _) in children.iter_mut() {
                    *parent_id = id.clone();
                }
                if let Data::Mapped { mapped, .. } = other_tree.get_mut(&id).unwrap().data_mut() {
                    if this.output != other.output {
                        mapped.output_leave(&this.output);
                        mapped.output_enter(&other.output, mapped.bbox());
                        for (ref surface, _) in mapped.windows() {
                            toplevel_leave_output(surface, &this.output);
                            toplevel_enter_output(surface, &other.output);
                        }
                    }
                    for (ref surface, _) in mapped.windows() {
                        toplevel_leave_workspace(surface, this_handle);
                        toplevel_enter_workspace(surface, other_handle);
                    }

                    *mapped.tiling_node_id.lock().unwrap() = Some(id.clone());
                }

                while !children.is_empty() {
                    for (parent_id, child_id) in std::mem::take(&mut children) {
                        let new_children = this_tree
                            .children_ids(&child_id)
                            .unwrap()
                            .cloned()
                            .collect::<Vec<_>>();
                        let child_node = this_tree
                            .remove_node(child_id, RemoveBehavior::OrphanChildren)
                            .unwrap();
                        let maybe_mapped = match child_node.data() {
                            Data::Mapped { mapped, .. } => Some(mapped.clone()),
                            _ => None,
                        };
                        let new_id = other_tree
                            .insert(child_node, InsertBehavior::UnderNode(&parent_id))
                            .unwrap();
                        if let Some(mapped) = maybe_mapped {
                            if this.output != other.output {
                                mapped.output_leave(&this.output);
                                mapped.output_enter(&other.output, mapped.bbox());
                                for (ref surface, _) in mapped.windows() {
                                    toplevel_leave_output(surface, &this.output);
                                    toplevel_enter_output(surface, &other.output);
                                }
                            }
                            for (ref surface, _) in mapped.windows() {
                                toplevel_leave_workspace(surface, this_handle);
                                toplevel_enter_workspace(surface, other_handle);
                            }

                            *mapped.tiling_node_id.lock().unwrap() = Some(new_id.clone());
                        }
                        children.extend(
                            new_children
                                .into_iter()
                                .map(|new_child| (new_id.clone(), new_child)),
                        );
                    }
                }
                let this_gaps = this.gaps();
                let other_gaps = other.gaps();

                TilingLayout::unmap_internal(&mut this_tree, &desc.node);
                let blocker =
                    TilingLayout::update_positions(&this.output, &mut this_tree, this_gaps);
                this.queue.push_tree(this_tree, ANIMATION_DURATION, blocker);

                let blocker =
                    TilingLayout::update_positions(&other.output, &mut other_tree, other_gaps);
                other
                    .queue
                    .push_tree(other_tree, ANIMATION_DURATION, blocker);

                other.node_desc_to_focus(&NodeDesc {
                    handle: other_handle.clone(),
                    node: id,
                    stack_window: None,
                })
            }
        }
    }

    pub fn swap_trees(
        this: &mut Self,
        mut other: Option<&mut Self>,
        this_desc: &NodeDesc,
        other_desc: &NodeDesc,
    ) -> Option<KeyboardFocusTarget> {
        let other_output = other
            .as_ref()
            .map(|other| other.output.clone())
            .unwrap_or(this.output.clone());
        if this.output == other_output
            && this_desc.handle == other_desc.handle
            && this_desc.node == other_desc.node
            && this_desc.stack_window.is_some() != other_desc.stack_window.is_some()
        {
            return None;
        }

        let mut this_tree = this.queue.trees.back().unwrap().0.copy_clone();
        let mut other_tree = match other.as_mut() {
            Some(other) => Some(other.queue.trees.back().unwrap().0.copy_clone()),
            None => {
                if this.output != other_output {
                    Some(this.queue.trees.back().unwrap().0.copy_clone())
                } else {
                    None
                }
            }
        };

        match (&this_desc.stack_window, &other_desc.stack_window) {
            (None, None) if other_tree.is_none() => {
                if this_desc.node != other_desc.node {
                    this_tree
                        .swap_nodes(
                            &this_desc.node,
                            &other_desc.node,
                            id_tree::SwapBehavior::TakeChildren,
                        )
                        .unwrap();
                }
            }
            (None, None) => {
                let Some(other_tree) = other_tree.as_mut() else {
                    unreachable!()
                };
                let this_node = this_tree.get_mut(&this_desc.node).ok()?;
                let other_node = other_tree.get_mut(&other_desc.node).ok()?;

                if let Data::Mapped { mapped, .. } = this_node.data_mut() {
                    if this.output != other_output {
                        mapped.output_leave(&this.output);
                        mapped.output_enter(&other_output, mapped.bbox());
                        for (ref surface, _) in mapped.windows() {
                            toplevel_leave_output(surface, &this.output);
                            toplevel_enter_output(surface, &other_output);
                        }
                    }
                    for (ref surface, _) in mapped.windows() {
                        toplevel_leave_workspace(surface, &this_desc.handle);
                        toplevel_enter_workspace(surface, &other_desc.handle);
                    }
                    *mapped.tiling_node_id.lock().unwrap() = Some(other_desc.node.clone());
                }
                if let Data::Mapped { mapped, .. } = other_node.data_mut() {
                    if this.output != other_output {
                        mapped.output_leave(&other_output);
                        mapped.output_enter(&this.output, mapped.bbox());
                        for (ref surface, _) in mapped.windows() {
                            toplevel_leave_output(surface, &other_output);
                            toplevel_enter_output(surface, &this.output);
                        }
                    }
                    for (ref surface, _) in mapped.windows() {
                        toplevel_leave_workspace(surface, &other_desc.handle);
                        toplevel_enter_workspace(surface, &this_desc.handle);
                    }
                    *mapped.tiling_node_id.lock().unwrap() = Some(this_desc.node.clone());
                }

                // swap data
                let other_data = other_node.data().clone();
                other_node.replace_data(this_node.replace_data(other_data));

                // swap children
                let mut this_children = this_node
                    .children()
                    .into_iter()
                    .map(|child_id| (other_desc.node.clone(), child_id.clone()))
                    .collect::<Vec<_>>();
                let mut other_children = other_node
                    .children()
                    .into_iter()
                    .map(|child_id| (this_desc.node.clone(), child_id.clone()))
                    .collect::<Vec<_>>();

                while !this_children.is_empty() {
                    for (parent_id, child_id) in std::mem::take(&mut this_children) {
                        let new_children = this_tree
                            .children_ids(&child_id)
                            .unwrap()
                            .cloned()
                            .collect::<Vec<_>>();
                        let child_node = this_tree
                            .remove_node(child_id, RemoveBehavior::OrphanChildren)
                            .unwrap();
                        let maybe_mapped = match child_node.data() {
                            Data::Mapped { mapped, .. } => Some(mapped.clone()),
                            _ => None,
                        };
                        let new_id = other_tree
                            .insert(child_node, InsertBehavior::UnderNode(&parent_id))
                            .unwrap();
                        if let Some(mapped) = maybe_mapped {
                            if this.output != other_output {
                                mapped.output_leave(&this.output);
                                mapped.output_enter(&other_output, mapped.bbox());
                                for (ref surface, _) in mapped.windows() {
                                    toplevel_leave_output(surface, &this.output);
                                    toplevel_enter_output(surface, &other_output);
                                }
                            }
                            for (ref surface, _) in mapped.windows() {
                                toplevel_leave_workspace(surface, &this_desc.handle);
                                toplevel_enter_workspace(surface, &other_desc.handle);
                            }
                            *mapped.tiling_node_id.lock().unwrap() = Some(new_id.clone());
                        }
                        this_children.extend(
                            new_children
                                .into_iter()
                                .map(|new_child| (new_id.clone(), new_child)),
                        );
                    }
                }

                while !other_children.is_empty() {
                    for (parent_id, child_id) in std::mem::take(&mut other_children) {
                        let new_children = other_tree
                            .children_ids(&child_id)
                            .unwrap()
                            .cloned()
                            .collect::<Vec<_>>();
                        let child_node = other_tree
                            .remove_node(child_id, RemoveBehavior::OrphanChildren)
                            .unwrap();
                        let maybe_mapped = match child_node.data() {
                            Data::Mapped { mapped, .. } => Some(mapped.clone()),
                            _ => None,
                        };
                        let new_id = this_tree
                            .insert(child_node, InsertBehavior::UnderNode(&parent_id))
                            .unwrap();
                        if let Some(mapped) = maybe_mapped {
                            if this.output != other_output {
                                mapped.output_leave(&other_output);
                                mapped.output_enter(&this.output, mapped.bbox());
                                for (ref surface, _) in mapped.windows() {
                                    toplevel_leave_output(surface, &other_output);
                                    toplevel_enter_output(surface, &this.output);
                                }
                            }
                            for (ref surface, _) in mapped.windows() {
                                toplevel_leave_workspace(surface, &other_desc.handle);
                                toplevel_enter_workspace(surface, &this_desc.handle);
                            }
                            *mapped.tiling_node_id.lock().unwrap() = Some(new_id.clone());
                        }
                        other_children.extend(
                            new_children
                                .into_iter()
                                .map(|new_child| (new_id.clone(), new_child)),
                        );
                    }
                }
            }
            (Some(this_surface), None) => {
                if !this_surface.alive() {
                    return None;
                }
                let this_node = this_tree.get_mut(&this_desc.node).ok()?;
                let Data::Mapped {
                    mapped: this_mapped,
                    ..
                } = this_node.data()
                else {
                    return None;
                };
                let this_mapped = this_mapped.clone();
                let geometry = *this_node.data().geometry();
                assert!(this_mapped.is_stack());

                let other_tree = other_tree.as_mut().unwrap_or(&mut this_tree);
                if other_tree.get(&other_desc.node).is_err() {
                    return None;
                }

                let surfaces = other_tree
                    .traverse_pre_order(&other_desc.node)
                    .unwrap()
                    .flat_map(|node| match node.data() {
                        Data::Mapped { mapped, .. } => Some(mapped.windows().map(|(s, _)| s)),
                        _ => None,
                    })
                    .flatten()
                    .collect::<Vec<_>>();
                let cleanup = other_tree
                    .children_ids(&other_desc.node)
                    .unwrap()
                    .cloned()
                    .collect::<Vec<_>>();
                for node in cleanup {
                    let _ = other_tree.remove_node(node, RemoveBehavior::DropChildren);
                }

                let this_stack = this_mapped.stack_ref()?;
                let this_idx = this_stack
                    .surfaces()
                    .position(|s| &s == this_surface)
                    .unwrap();
                for (i, surface) in surfaces.into_iter().enumerate() {
                    if this.output != other_output {
                        surface.output_leave(&other_output);
                        surface.output_enter(&this.output, surface.bbox());
                        toplevel_leave_output(&surface, &other_output);
                        toplevel_enter_output(&surface, &this.output);
                    }
                    if this_desc.handle != other_desc.handle {
                        toplevel_leave_workspace(&surface, &other_desc.handle);
                        toplevel_enter_workspace(&surface, &this_desc.handle);
                    }
                    this_stack.add_window(surface, Some(this_idx + i));
                }
                if this.output != other_output {
                    this_surface.output_leave(&this.output);
                    toplevel_leave_output(this_surface, &this.output);
                    toplevel_enter_output(this_surface, &other_output);
                }
                if this_desc.handle != other_desc.handle {
                    toplevel_leave_workspace(this_surface, &this_desc.handle);
                    toplevel_enter_workspace(this_surface, &other_desc.handle);
                }
                this_stack.remove_window(&this_surface);

                let mapped: CosmicMapped = CosmicWindow::new(
                    this_surface.clone(),
                    this_stack.loop_handle(),
                    this.theme.clone(),
                )
                .into();
                mapped.set_tiled(true);
                mapped.refresh();
                if this.output != other_output {
                    mapped.output_enter(&other_output, mapped.bbox());
                }

                *mapped.tiling_node_id.lock().unwrap() = Some(other_desc.node.clone());
                other_tree
                    .get_mut(&other_desc.node)
                    .unwrap()
                    .replace_data(Data::Mapped {
                        mapped,
                        last_geometry: geometry,
                        minimize_rect: None,
                    });
            }
            (None, Some(other_surface)) => {
                if !other_surface.alive() {
                    return None;
                }

                let other_tree = other_tree.as_mut().unwrap_or(&mut this_tree);
                let other_node = other_tree.get_mut(&other_desc.node).ok()?;
                let Data::Mapped {
                    mapped: other_mapped,
                    ..
                } = other_node.data()
                else {
                    return None;
                };
                let other_mapped = other_mapped.clone();
                let geometry = *other_node.data().geometry();
                assert!(other_mapped.is_stack());

                let surfaces = this_tree
                    .traverse_pre_order(&this_desc.node)
                    .unwrap()
                    .flat_map(|node| match node.data() {
                        Data::Mapped { mapped, .. } => Some(mapped.windows().map(|(s, _)| s)),
                        _ => None,
                    })
                    .flatten()
                    .collect::<Vec<_>>();
                let cleanup = this_tree
                    .children_ids(&this_desc.node)
                    .unwrap()
                    .cloned()
                    .collect::<Vec<_>>();
                for node in cleanup {
                    let _ = this_tree.remove_node(node, RemoveBehavior::DropChildren);
                }

                let other_stack = other_mapped.stack_ref()?;
                let other_idx = other_stack
                    .surfaces()
                    .position(|s| &s == other_surface)
                    .unwrap();
                for (i, surface) in surfaces.into_iter().enumerate() {
                    if this.output != other_output {
                        surface.output_leave(&this.output);
                        surface.output_enter(&other_output, surface.bbox());
                        toplevel_leave_output(&surface, &this.output);
                        toplevel_enter_output(&surface, &other_output);
                    }
                    if this_desc.handle != other_desc.handle {
                        toplevel_leave_workspace(&surface, &this_desc.handle);
                        toplevel_enter_workspace(&surface, &other_desc.handle);
                    }
                    other_stack.add_window(surface, Some(other_idx + i));
                }
                if this.output != other_output {
                    other_surface.output_leave(&other_output);
                    toplevel_leave_output(other_surface, &other_output);
                    toplevel_enter_output(other_surface, &this.output);
                }
                if this_desc.handle != other_desc.handle {
                    toplevel_leave_workspace(other_surface, &other_desc.handle);
                    toplevel_enter_workspace(other_surface, &this_desc.handle);
                }
                other_stack.remove_window(&other_surface);

                let mapped: CosmicMapped = CosmicWindow::new(
                    other_surface.clone(),
                    other_stack.loop_handle(),
                    this.theme.clone(),
                )
                .into();
                mapped.set_tiled(true);
                mapped.refresh();
                if this.output != other_output {
                    mapped.output_enter(&this.output, mapped.bbox());
                }

                *mapped.tiling_node_id.lock().unwrap() = Some(this_desc.node.clone());
                this_tree
                    .get_mut(&this_desc.node)
                    .unwrap()
                    .replace_data(Data::Mapped {
                        mapped,
                        last_geometry: geometry,
                        minimize_rect: None,
                    });
            }
            (Some(this_surface), Some(other_surface)) => {
                if !this_surface.alive() || !other_surface.alive() {
                    return None;
                }

                let this_mapped =
                    this_tree
                        .get(&this_desc.node)
                        .ok()
                        .and_then(|node| match node.data() {
                            Data::Mapped { mapped, .. } => Some(mapped.clone()),
                            _ => None,
                        })?;
                let other_tree = other_tree.as_mut().unwrap_or(&mut this_tree);
                let other_mapped =
                    other_tree
                        .get(&other_desc.node)
                        .ok()
                        .and_then(|node| match node.data() {
                            Data::Mapped { mapped, .. } => Some(mapped.clone()),
                            _ => None,
                        })?;

                let this_stack = this_mapped.stack_ref()?;
                let other_stack = other_mapped.stack_ref()?;

                let this_idx = this_stack
                    .surfaces()
                    .position(|s| &s == this_surface)
                    .unwrap();
                let other_idx = other_stack
                    .surfaces()
                    .position(|s| &s == other_surface)
                    .unwrap();
                let this_was_active = &this_stack.active() == this_surface;
                let other_was_active = &other_stack.active() == other_surface;
                this_stack.add_window(other_surface.clone(), Some(this_idx));
                this_stack.remove_window(&this_surface);
                other_stack.add_window(this_surface.clone(), Some(other_idx));

                if this.output != other_output {
                    toplevel_leave_output(this_surface, &this.output);
                    toplevel_leave_output(other_surface, &other_output);
                    toplevel_enter_output(this_surface, &other_output);
                    toplevel_enter_output(other_surface, &this.output);
                }
                if this_desc.handle != other_desc.handle {
                    toplevel_leave_workspace(this_surface, &this_desc.handle);
                    toplevel_leave_workspace(other_surface, &other_desc.handle);
                    toplevel_enter_workspace(this_surface, &other_desc.handle);
                    toplevel_enter_workspace(other_surface, &this_desc.handle);
                }

                other_stack.remove_window(&other_surface);
                if this_was_active {
                    this_stack.set_active(&other_surface);
                }
                if other_was_active {
                    other_stack.set_active(&this_surface);
                }

                return other
                    .as_ref()
                    .unwrap_or(&this)
                    .node_desc_to_focus(other_desc);
            }
        }

        let this_gaps = this.gaps();
        let blocker = TilingLayout::update_positions(&this.output, &mut this_tree, this_gaps);
        this.queue.push_tree(this_tree, ANIMATION_DURATION, blocker);

        let has_other_tree = other_tree.is_some();
        if let Some(mut other_tree) = other_tree {
            let (other_queue, gaps) = if let Some(other) = other.as_mut() {
                let other_gaps = other.gaps();
                (&mut other.queue, other_gaps)
            } else {
                (&mut this.queue, this_gaps)
            };
            let blocker = TilingLayout::update_positions(&other_output, &mut other_tree, gaps);
            other_queue.push_tree(other_tree, ANIMATION_DURATION, blocker);
        }

        match (&this_desc.stack_window, &other_desc.stack_window) {
            (None, None) if !has_other_tree => this.node_desc_to_focus(&this_desc),
            //(None, Some(_)) => None,
            _ => other
                .as_ref()
                .unwrap_or(&this)
                .node_desc_to_focus(&other_desc),
        }
    }

    pub fn node_desc_to_focus(&self, desc: &NodeDesc) -> Option<KeyboardFocusTarget> {
        let tree = &self.queue.trees.back().unwrap().0;
        let data = tree.get(&desc.node).ok()?.data();
        match data {
            Data::Mapped { mapped, .. } => Some(KeyboardFocusTarget::Element(mapped.clone())),
            Data::Group { alive, .. } => Some(KeyboardFocusTarget::Group(WindowGroup {
                node: desc.node.clone(),
                alive: Arc::downgrade(alive),
                focus_stack: tree
                    .children_ids(&desc.node)
                    .unwrap()
                    .cloned()
                    .collect::<Vec<_>>(),
            })),
            _ => None,
        }
    }

    pub fn tree(&self) -> &Tree<Data> {
        &self.queue.trees.back().unwrap().0
    }

    pub fn unmap(&mut self, window: &CosmicMapped) -> bool {
        if self.unmap_window_internal(window, false) {
            window.output_leave(&self.output);
            window.set_tiled(false);
            *window.tiling_node_id.lock().unwrap() = None;
            true
        } else {
            false
        }
    }

    pub fn unmap_as_placeholder(&mut self, window: &CosmicMapped) -> Option<NodeId> {
        let node_id = window.tiling_node_id.lock().unwrap().take()?;

        let data = self
            .queue
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

        window.output_leave(&self.output);
        window.set_tiled(false);
        Some(node_id)
    }

    pub fn unmap_minimize(
        &mut self,
        window: &CosmicMapped,
        to: Rectangle<i32, Local>,
    ) -> Option<MinimizedTilingState> {
        let node_id = window.tiling_node_id.lock().unwrap().clone()?;
        let state = {
            let tree = &self.queue.trees.back().unwrap().0;
            tree.get(&node_id).unwrap().parent().and_then(|parent_id| {
                let parent = tree.get(&parent_id).unwrap();
                let idx = parent
                    .children()
                    .iter()
                    .position(|id| id == &node_id)
                    .unwrap();
                if let Data::Group {
                    orientation, sizes, ..
                } = parent.data()
                {
                    if sizes.len() == 2 {
                        // this group will be flattened
                        Some(MinimizedTilingState {
                            parent: None,
                            sibling: parent.children().iter().cloned().find(|id| id != &node_id),
                            orientation: *orientation,
                            idx,
                            sizes: sizes.clone(),
                        })
                    } else {
                        Some(MinimizedTilingState {
                            parent: Some(parent_id.clone()),
                            sibling: None,
                            orientation: *orientation,
                            idx,
                            sizes: sizes.clone(),
                        })
                    }
                } else {
                    None
                }
            })
        };

        if self.unmap_window_internal(window, true) {
            let tree = &mut self
                .queue
                .trees
                .get_mut(self.queue.trees.len() - 2)
                .unwrap()
                .0;
            if let Data::Mapped {
                minimize_rect: minimize_to,
                ..
            } = tree.get_mut(&node_id).unwrap().data_mut()
            {
                *minimize_to = Some(to);
            }
            window.set_minimized(true);
        }

        state
    }

    fn unmap_window_internal(&mut self, mapped: &CosmicMapped, minimizing: bool) -> bool {
        let tiling_node_id = mapped.tiling_node_id.lock().unwrap().as_ref().cloned();
        let gaps = self.gaps();

        if let Some(node_id) = tiling_node_id {
            if self
                .queue
                .trees
                .back()
                .unwrap()
                .0
                .get(&node_id)
                .map(|node| node.data().is_mapped(Some(mapped)))
                .unwrap_or(false)
            {
                let mut tree = self.queue.trees.back().unwrap().0.copy_clone();

                TilingLayout::unmap_internal(&mut tree, &node_id);

                let duration = if minimizing {
                    MINIMIZE_ANIMATION_DURATION
                } else {
                    ANIMATION_DURATION
                };
                let blocker = TilingLayout::update_positions(&self.output, &mut tree, gaps);
                self.queue.push_tree(tree, duration, blocker);

                return true;
            }
        }
        false
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
                    let other_child = tree.children_ids(&id).unwrap().next().cloned().unwrap();
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

    // TODO: Move would needs this to be accurate during animations
    pub fn element_geometry(&self, elem: &CosmicMapped) -> Option<Rectangle<i32, Local>> {
        if let Some(id) = elem.tiling_node_id.lock().unwrap().as_ref() {
            let node = self.queue.trees.back().unwrap().0.get(id).ok()?;
            let data = node.data();
            assert!(data.is_mapped(Some(elem)));
            let geo = *data.geometry();
            Some(geo)
        } else {
            None
        }
    }

    pub fn move_current_node(&mut self, direction: Direction, seat: &Seat<State>) -> MoveResult {
        let gaps = self.gaps();

        let mut tree = self.queue.trees.back().unwrap().0.copy_clone();

        let Some(target) = seat.get_keyboard().unwrap().current_focus() else {
            return MoveResult::None;
        };
        let Some((node_id, data)) = TilingLayout::currently_focused_node(&mut tree, target) else {
            return MoveResult::None;
        };

        // stacks may handle movement internally
        if let FocusedNodeData::Window(window) = data.clone() {
            match window.handle_move(direction) {
                StackMoveResult::Handled => return MoveResult::Done,
                StackMoveResult::MoveOut(surface, loop_handle) => {
                    let mapped: CosmicMapped =
                        CosmicWindow::new(surface, loop_handle, self.theme.clone()).into();
                    mapped.output_enter(&self.output, mapped.bbox());
                    let orientation = match direction {
                        Direction::Left | Direction::Right => Orientation::Vertical,
                        Direction::Up | Direction::Down => Orientation::Horizontal,
                    };

                    let new_node = Node::new(Data::Mapped {
                        mapped: mapped.clone(),
                        last_geometry: Rectangle::from_loc_and_size((0, 0), (100, 100)),
                        minimize_rect: None,
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

                    let blocker = TilingLayout::update_positions(&self.output, &mut tree, gaps);
                    self.queue.push_tree(tree, ANIMATION_DURATION, blocker);
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

                let blocker = TilingLayout::update_positions(&self.output, &mut tree, gaps);
                self.queue.push_tree(tree, ANIMATION_DURATION, blocker);
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

                let blocker = TilingLayout::update_positions(&self.output, &mut tree, gaps);
                self.queue.push_tree(tree, ANIMATION_DURATION, blocker);
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

                let blocker = TilingLayout::update_positions(&self.output, &mut tree, gaps);
                self.queue.push_tree(tree, ANIMATION_DURATION, blocker);
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
                    alive,
                    focus_stack,
                }
                .into(),
            ),
        }
    }

    pub fn next_focus<'a>(
        &self,
        direction: FocusDirection,
        seat: &Seat<State>,
        focus_stack: impl Iterator<Item = &'a CosmicMapped> + 'a,
        swap_desc: Option<NodeDesc>,
    ) -> FocusResult {
        let tree = &self.queue.trees.back().unwrap().0;

        let Some(target) = seat.get_keyboard().unwrap().current_focus() else {
            return FocusResult::None;
        };
        let Some(focused) = TilingLayout::currently_focused_node(tree, target).or_else(|| {
            TilingLayout::last_active_window(tree, focus_stack)
                .map(|(id, mapped)| (id, FocusedNodeData::Window(mapped)))
        }) else {
            return FocusResult::None;
        };

        let (last_node_id, data) = focused;

        // stacks may handle focus internally
        if let FocusedNodeData::Window(window) = data.clone() {
            if window.handle_focus(
                direction,
                swap_desc.clone().filter(|desc| desc.node == last_node_id),
            ) {
                return FocusResult::Handled;
            }
        }

        if direction == FocusDirection::In {
            if swap_desc
                .as_ref()
                .map(|desc| desc.node == last_node_id)
                .unwrap_or(false)
            {
                return FocusResult::Handled; // abort
            }

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
                                alive: Arc::downgrade(alive),
                                focus_stack: stack,
                            }
                            .into(),
                        ),
                        Data::Placeholder { .. } => FocusResult::None,
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
                if swap_desc
                    .as_ref()
                    .map(|desc| {
                        tree.traverse_pre_order_ids(group)
                            .unwrap()
                            .any(|node| node == desc.node)
                    })
                    .unwrap_or(false)
                {
                    return FocusResult::Handled; // abort
                }

                return FocusResult::Some(
                    WindowGroup {
                        node: group.clone(),
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
                    if let Some(desc) = swap_desc.as_ref() {
                        if let Some(replacement_id) = tree
                            .ancestor_ids(node_id.unwrap())
                            .unwrap()
                            .find(|anchestor| *anchestor == &desc.node)
                            .or_else(|| {
                                tree.children_ids(node_id.unwrap())
                                    .unwrap()
                                    .find(|child| *child == &desc.node)
                            })
                        {
                            return match tree.get(replacement_id).unwrap().data() {
                                Data::Group { alive, .. } => {
                                    FocusResult::Some(KeyboardFocusTarget::Group(WindowGroup {
                                        node: replacement_id.clone(),
                                        alive: Arc::downgrade(&alive),
                                        focus_stack: tree
                                            .children_ids(replacement_id)
                                            .unwrap()
                                            .cloned()
                                            .collect(),
                                    }))
                                }
                                Data::Mapped { mapped, .. } => {
                                    if mapped.is_stack()
                                        && desc.stack_window.is_none()
                                        && replacement_id == &desc.node
                                    {
                                        mapped.stack_ref().unwrap().focus_stack();
                                    }
                                    FocusResult::Some(KeyboardFocusTarget::Element(mapped.clone()))
                                }
                                _ => unreachable!(),
                            };
                        }
                    }

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
                            if mapped.is_stack()
                                && swap_desc
                                    .as_ref()
                                    .map(|desc| {
                                        desc.stack_window.is_none()
                                            && &desc.node == node_id.unwrap()
                                    })
                                    .unwrap_or(false)
                            {
                                mapped.stack_ref().unwrap().focus_stack();
                            }
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

    pub fn update_orientation(&mut self, new_orientation: Option<Orientation>, seat: &Seat<State>) {
        let gaps = self.gaps();

        let Some(target) = seat.get_keyboard().unwrap().current_focus() else {
            return;
        };

        let mut tree = self.queue.trees.back().unwrap().0.copy_clone();
        if let Some((last_active, _)) = TilingLayout::currently_focused_node(&tree, target) {
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

                    let blocker = TilingLayout::update_positions(&self.output, &mut tree, gaps);
                    self.queue.push_tree(tree, ANIMATION_DURATION, blocker);
                }
            }
        }
    }

    pub fn toggle_stacking(
        &mut self,
        mapped: &CosmicMapped,
        mut focus_stack: FocusStackMut,
    ) -> Option<KeyboardFocusTarget> {
        let gaps = self.gaps();

        let Some(node_id) = mapped.tiling_node_id.lock().unwrap().clone() else {
            return None;
        };

        let mut tree = self.queue.trees.back().unwrap().0.copy_clone();
        if tree.get(&node_id).is_err() {
            return None;
        }

        let result = if mapped.is_window() {
            // if it is just a window
            match tree.get_mut(&node_id).unwrap().data_mut() {
                Data::Mapped { mapped, .. } => {
                    mapped.convert_to_stack((&self.output, mapped.bbox()), self.theme.clone());
                    focus_stack.append(&mapped);
                    KeyboardFocusTarget::Element(mapped.clone())
                }
                _ => unreachable!(),
            }
        } else {
            // if we have a stack
            let mut surfaces = mapped.windows().map(|(s, _)| s);
            let first = surfaces.next().expect("Stack without a window?");
            let focused = mapped.active_window();

            let mut new_elements = Vec::new();

            let handle = match tree.get_mut(&node_id).unwrap().data_mut() {
                Data::Mapped { mapped, .. } => {
                    let handle = mapped.loop_handle();
                    mapped.convert_to_surface(
                        first,
                        (&self.output, mapped.bbox()),
                        self.theme.clone(),
                    );
                    new_elements.push(mapped.clone());
                    handle
                }
                _ => unreachable!(),
            };

            // map the rest
            let mut current_node = node_id.clone();
            for other in surfaces {
                other.try_force_undecorated(false);
                other.set_tiled(false);
                let focused = other == focused;
                let window = CosmicMapped::from(CosmicWindow::new(
                    other,
                    handle.clone(),
                    self.theme.clone(),
                ));
                window.output_enter(&self.output, window.bbox());

                {
                    let layer_map = layer_map_for_output(&self.output);
                    window.set_bounds(layer_map.non_exclusive_zone().size);
                }

                TilingLayout::map_to_tree(
                    &mut tree,
                    window.clone(),
                    &self.output,
                    Some(current_node),
                    None,
                    None,
                );

                let node = window.tiling_node_id.lock().unwrap().clone().unwrap();
                if focused {
                    new_elements.insert(0, window);
                } else {
                    new_elements.push(window);
                }
                current_node = node;
            }

            let node = tree.get(&node_id).unwrap();
            let node_id = if current_node != node_id {
                node.parent().cloned().unwrap_or(node_id)
            } else {
                node_id
            };

            for elem in new_elements.iter().rev() {
                focus_stack.append(elem);
            }

            match tree.get(&node_id).unwrap().data() {
                Data::Group { alive, .. } => KeyboardFocusTarget::Group(WindowGroup {
                    node: node_id.clone(),
                    alive: Arc::downgrade(alive),
                    focus_stack: vec![new_elements[0]
                        .tiling_node_id
                        .lock()
                        .unwrap()
                        .clone()
                        .unwrap()],
                }),
                Data::Mapped { mapped, .. } => KeyboardFocusTarget::Element(mapped.clone()),
                _ => unreachable!(),
            }
        };

        let blocker = TilingLayout::update_positions(&self.output, &mut tree, gaps);
        self.queue.push_tree(tree, ANIMATION_DURATION, blocker);

        Some(result)
    }

    pub fn toggle_stacking_focused(
        &mut self,
        seat: &Seat<State>,
        mut focus_stack: FocusStackMut,
    ) -> Option<KeyboardFocusTarget> {
        let gaps = self.gaps();

        let Some(target) = seat.get_keyboard().unwrap().current_focus() else {
            return None;
        };

        let mut tree = self.queue.trees.back().unwrap().0.copy_clone();
        if let Some((last_active, last_active_data)) =
            TilingLayout::currently_focused_node(&tree, target)
        {
            match last_active_data {
                FocusedNodeData::Window(mapped) => {
                    return self.toggle_stacking(&mapped, focus_stack);
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
                        return None;
                    }
                    let handle = handle.unwrap();
                    let stack = CosmicStack::new(surfaces.into_iter(), handle, self.theme.clone());

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
                    stack.output_enter(&self.output, stack.bbox());
                    stack.set_activate(true);
                    stack.active().send_configure();
                    stack.refresh();

                    let mapped = CosmicMapped::from(stack);
                    *mapped.last_geometry.lock().unwrap() = Some(geo);
                    *mapped.tiling_node_id.lock().unwrap() = Some(last_active);
                    focus_stack.append(&mapped);
                    *data = Data::Mapped {
                        mapped: mapped.clone(),
                        last_geometry: geo,
                        minimize_rect: None,
                    };

                    let blocker = TilingLayout::update_positions(&self.output, &mut tree, gaps);
                    self.queue.push_tree(tree, ANIMATION_DURATION, blocker);

                    return Some(KeyboardFocusTarget::Element(mapped));
                }
            }
        }

        None
    }

    pub fn recalculate(&mut self) {
        let gaps = self.gaps();

        let mut tree = self.queue.trees.back().unwrap().0.copy_clone();
        let blocker = TilingLayout::update_positions(&self.output, &mut tree, gaps);
        self.queue.push_tree(tree, ANIMATION_DURATION, blocker);
    }

    #[profiling::function]
    pub fn refresh(&mut self) {
        let dead_windows = self
            .mapped()
            .map(|(w, _)| w.clone())
            .filter(|w| !w.alive())
            .collect::<Vec<_>>();
        for dead_window in dead_windows.iter() {
            self.unmap_window_internal(dead_window, false);
        }

        for (mapped, _) in self.mapped() {
            mapped.refresh();
        }
    }

    pub fn animations_going(&self) -> bool {
        self.queue.animation_start.is_some()
    }

    pub fn update_animation_state(&mut self) -> HashMap<ClientId, Client> {
        let mut clients = HashMap::new();
        for blocker in self.pending_blockers.drain(..) {
            clients.extend(blocker.signal_ready());
        }

        if let Some(start) = self.queue.animation_start {
            let duration_since_start = Instant::now().duration_since(start);
            if duration_since_start
                >= self
                    .queue
                    .trees
                    .get(1)
                    .expect("Animation going without second tree?")
                    .1
            {
                let _ = self.queue.animation_start.take();
                let _ = self.queue.trees.pop_front();
                let front = self.queue.trees.front_mut().unwrap();
                if let Some(root_id) = front.0.root_node_id() {
                    for node in front
                        .0
                        .traverse_pre_order_ids(root_id)
                        .unwrap()
                        .collect::<Vec<_>>()
                        .into_iter()
                    {
                        if let Data::Mapped { minimize_rect, .. } =
                            front.0.get_mut(&node).unwrap().data_mut()
                        {
                            minimize_rect.take();
                        }
                    }
                }
                let _ = front.2.take();
            } else {
                return clients;
            }
        }

        let ready_trees = self
            .queue
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
            self.queue
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
            let (_, duration, blocker) = self.queue.trees.get_mut(1).unwrap();
            *duration = other_duration
                .map(|other| other.max(*duration))
                .unwrap_or(*duration);
            if let Some(blocker) = blocker {
                clients.extend(blocker.signal_ready());
            }
            self.queue.animation_start = Some(Instant::now());
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

    pub fn resize_request(
        &self,
        mut node_id: NodeId,
        edge: ResizeEdge,
    ) -> Option<(NodeId, usize, Orientation)> {
        let tree = self.tree();

        while let Some(group_id) = tree.get(&node_id).unwrap().parent().cloned() {
            let orientation = tree.get(&group_id).unwrap().data().orientation();
            let node_idx = tree
                .children_ids(&group_id)
                .unwrap()
                .position(|id| id == &node_id)
                .unwrap();
            let total = tree.children_ids(&group_id).unwrap().count();
            if orientation == Orientation::Vertical {
                if node_idx > 0 && edge.contains(ResizeEdge::LEFT) {
                    return Some((group_id, node_idx - 1, orientation));
                }
                if node_idx < total - 1 && edge.contains(ResizeEdge::RIGHT) {
                    return Some((group_id, node_idx, orientation));
                }
            } else {
                if node_idx > 0 && edge.contains(ResizeEdge::TOP) {
                    return Some((group_id, node_idx - 1, orientation));
                }
                if node_idx < total - 1 && edge.contains(ResizeEdge::BOTTOM) {
                    return Some((group_id, node_idx, orientation));
                }
            }

            node_id = group_id;
        }

        None
    }

    pub fn resize(
        &mut self,
        focused: &KeyboardFocusTarget,
        direction: ResizeDirection,
        edges: ResizeEdge,
        amount: i32,
    ) -> bool {
        let gaps = self.gaps();

        let mut tree = self.queue.trees.back().unwrap().0.copy_clone();
        let Some(root_id) = tree.root_node_id() else {
            return false;
        };
        let Some(mut node_id) = (match TilingLayout::currently_focused_node(&tree, focused.clone())
        {
            Some((_id, FocusedNodeData::Window(mapped))) =>
            // we need to make sure the id belongs to this tree..
            {
                tree.traverse_pre_order_ids(root_id)
                    .unwrap()
                    .find(|id| tree.get(id).unwrap().data().is_mapped(Some(&mapped)))
            }
            Some((id, FocusedNodeData::Group(_, _))) => Some(id), // in this case the workspace handle was already matched, so the id is to be trusted
            _ => None,
        }) else {
            return false;
        };

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
            let blocker = TilingLayout::update_positions(&self.output, &mut tree, gaps);
            self.queue.push_tree(tree, None, blocker);

            return true;
        }

        true
    }

    pub fn stacking_indicator(&self) -> Option<Rectangle<i32, Local>> {
        if let Some(TargetZone::WindowStack(_, geo)) =
            self.last_overview_hover.as_ref().map(|(_, zone)| zone)
        {
            Some(*geo)
        } else {
            None
        }
    }

    pub fn cleanup_drag(&mut self) {
        let gaps = self.gaps();

        let mut tree = self.queue.trees.back().unwrap().0.copy_clone();

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

            let blocker = TilingLayout::update_positions(&self.output, &mut tree, gaps);
            self.queue.push_tree(tree, ANIMATION_DURATION, blocker);
        }
    }

    pub fn drop_window(&mut self, window: CosmicMapped) -> (CosmicMapped, Point<i32, Local>) {
        let gaps = self.gaps();

        let mut tree = self.queue.trees.back().unwrap().0.copy_clone();

        window.output_enter(&self.output, window.bbox());
        {
            let layer_map = layer_map_for_output(&self.output);
            window.set_bounds(layer_map.non_exclusive_zone().size);
        }

        let mapped = match self.last_overview_hover.as_ref().map(|x| &x.1) {
            Some(TargetZone::GroupEdge(group_id, direction)) if tree.get(&group_id).is_ok() => {
                let new_id = tree
                    .insert(
                        Node::new(Data::Mapped {
                            mapped: window.clone(),
                            last_geometry: Rectangle::from_loc_and_size((0, 0), (100, 100)),
                            minimize_rect: None,
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
                            minimize_rect: None,
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
                    minimize_rect: None,
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
                            minimize_rect: None,
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
                        mapped.convert_to_stack((&self.output, mapped.bbox()), self.theme.clone());
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
                    &self.output,
                    None,
                    None,
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

        let blocker = TilingLayout::update_positions(&self.output, &mut tree, gaps);
        self.queue.push_tree(tree, ANIMATION_DURATION, blocker);

        let location = self.element_geometry(&mapped).unwrap().loc;
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
        mut target: KeyboardFocusTarget,
    ) -> Option<(NodeId, FocusedNodeData)> {
        // if the focus is currently on a popup, treat it's toplevel as the target
        if let KeyboardFocusTarget::Popup(popup) = target {
            let toplevel_surface = match popup {
                PopupKind::Xdg(xdg) => get_popup_toplevel(&xdg),
                PopupKind::InputMethod(_) => unreachable!(),
            }?;
            let root_id = tree.root_node_id()?;
            let node =
                tree.traverse_pre_order(root_id)
                    .unwrap()
                    .find(|node| match node.data() {
                        Data::Mapped { mapped, .. } => mapped
                            .windows()
                            .any(|(w, _)| w.wl_surface().as_deref() == Some(&toplevel_surface)),
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
                let node = tree.get(&window_group.node).ok()?;
                if node.data().is_group() {
                    return Some((
                        window_group.node,
                        FocusedNodeData::Group(window_group.focus_stack, window_group.alive),
                    ));
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

    #[profiling::function]
    fn update_positions(
        output: &Output,
        tree: &mut Tree<Data>,
        gaps: (i32, i32),
    ) -> Option<TilingBlocker> {
        if let Some(root_id) = tree.root_node_id() {
            let mut configures = Vec::new();

            let (outer, inner) = gaps;
            let mut geo = layer_map_for_output(&output)
                .non_exclusive_zone()
                .as_local();
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
                        .next()
                        .cloned()
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
                                let internal_geometry = geo.to_global(&output);
                                mapped.set_geometry(internal_geometry);
                                if let Some(serial) = mapped.configure() {
                                    configures.push((mapped.active_window(), serial));
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

    pub fn element_under(&self, location_f64: Point<f64, Local>) -> Option<KeyboardFocusTarget> {
        let location = location_f64.to_i32_round();

        for (mapped, geo) in self.mapped() {
            if !mapped.bbox().contains((location - geo.loc).as_logical()) {
                continue;
            }
            if mapped.is_in_input_region(
                &((location_f64 - geo.loc.to_f64()).as_logical() + mapped.geometry().loc.to_f64()),
            ) {
                return Some(mapped.clone().into());
            }
        }

        None
    }

    pub fn surface_under(
        &mut self,
        location_f64: Point<f64, Local>,
        overview: OverviewMode,
    ) -> Option<(PointerFocusTarget, Point<f64, Local>)> {
        let gaps = self.gaps();
        let last_overview_hover = &mut self.last_overview_hover;
        let placeholder_id = &self.placeholder_id;
        let tree = &self.queue.trees.back().unwrap().0;
        let root = tree.root_node_id()?;
        let location = location_f64.to_i32_round();

        {
            let output_geo =
                Rectangle::from_loc_and_size((0, 0), self.output.geometry().size.as_logical())
                    .as_local();
            if !output_geo.contains(location) {
                return None;
            }
        }

        if !matches!(
            overview,
            OverviewMode::Started(_, _) | OverviewMode::Active(_)
        ) {
            last_overview_hover.take();
        }

        if matches!(overview, OverviewMode::None) {
            for (mapped, geo) in self.mapped() {
                if !mapped.bbox().contains((location - geo.loc).as_logical()) {
                    continue;
                }
                if let Some((target, surface_offset)) = mapped.focus_under(
                    (location_f64 - geo.loc.to_f64()).as_logical() + mapped.geometry().loc.to_f64(),
                ) {
                    return Some((
                        target,
                        geo.loc.to_f64() - mapped.geometry().loc.as_local().to_f64()
                            + surface_offset.as_local(),
                    ));
                }
            }

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
                        ..
                    },
                )) => {
                    let test_point = (location.to_f64() - last_geometry.loc.to_f64()
                        + mapped.geometry().loc.to_f64().as_local())
                    .as_logical();
                    mapped
                        .focus_under(test_point)
                        .map(|(surface, surface_offset)| {
                            (
                                surface,
                                last_geometry.loc.to_f64()
                                    - mapped.geometry().loc.as_local().to_f64()
                                    + surface_offset.as_local(),
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
                            output: self.output.downgrade(),
                            left_up_idx: idx,
                            orientation,
                        }
                        .into(),
                        (last_geometry.loc
                            + tree
                                .children(&id)
                                .unwrap()
                                .skip(idx)
                                .next()
                                .map(|node| {
                                    let geo = node.data().geometry();
                                    geo.loc + geo.size
                                })
                                .unwrap())
                        .to_f64(),
                    ))
                }
                _ => None,
            }
        } else if matches!(
            overview.active_trigger(),
            Some(Trigger::Pointer(_) | Trigger::Touch(_))
        ) {
            let non_exclusive_zone = layer_map_for_output(&self.output)
                .non_exclusive_zone()
                .as_local();
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
                None,
                None,
                self.theme.cosmic(),
            )
            .0;

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
                                .find(|child_id| tree.get(child_id).unwrap().data().is_mapped(None))
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
                                    (Direction::Left, relative_loc / last_geometry.size.w as f64)
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
                                } else if let TargetZone::WindowSplit(node_id, _) = old_target_zone
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
                                } else if let TargetZone::GroupEdge(node_id, _) = old_target_zone {
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
                                    TilingLayout::new_group(&mut tree, &node_id, &id, orientation)
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
                                } else if let TargetZone::GroupInterior(node_id, idx) = &target_zone
                                {
                                    if let Ok(node) = tree.get_mut(&node_id) {
                                        match node.data_mut() {
                                            Data::Group { pill_indicator, .. } => {
                                                *pill_indicator = Some(PillIndicator::Inner(*idx));
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
                                        &self.output,
                                        &mut tree,
                                        gaps,
                                    );
                                    self.queue.push_tree(tree, duration, blocker);
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
    }

    pub fn mapped(&self) -> impl Iterator<Item = (&CosmicMapped, Rectangle<i32, Local>)> {
        let tree = &self.queue.trees.back().unwrap().0;
        let iter = if let Some(root) = tree.root_node_id() {
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
                        } => (mapped, last_geometry.clone()),
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
                                } => (mapped, last_geometry.clone()),
                                _ => unreachable!(),
                            }),
                    ),
            )
        } else {
            None
        };
        iter.into_iter().flatten()
    }

    pub fn windows(&self) -> impl Iterator<Item = (CosmicSurface, Rectangle<i32, Local>)> + '_ {
        self.mapped().flat_map(|(mapped, geo)| {
            mapped.windows().map(move |(w, p)| {
                (w, {
                    let mut geo = geo.clone();
                    geo.loc += p.as_local();
                    geo.size -= p.to_size().as_local();
                    geo
                })
            })
        })
    }

    pub fn has_node(&self, node: &NodeId) -> bool {
        let tree = &self.queue.trees.back().unwrap().0;
        tree.root_node_id()
            .map(|root| {
                tree.traverse_pre_order_ids(root)
                    .unwrap()
                    .any(|id| &id == node)
            })
            .unwrap_or(false)
    }

    pub fn merge(&mut self, mut other: TilingLayout) {
        let gaps = self.gaps();

        let src = other.queue.trees.pop_back().unwrap().0;
        let mut dst = self.queue.trees.back().unwrap().0.copy_clone();

        let orientation = match self.output.geometry().size {
            x if x.w >= x.h => Orientation::Vertical,
            _ => Orientation::Horizontal,
        };
        TilingLayout::merge_trees(src, &mut dst, orientation);

        let blocker = TilingLayout::update_positions(&self.output, &mut dst, gaps);
        self.queue.push_tree(dst, ANIMATION_DURATION, blocker);
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

    #[profiling::function]
    pub fn render<R>(
        &self,
        renderer: &mut R,
        seat: Option<&Seat<State>>,
        non_exclusive_zone: Rectangle<i32, Local>,
        overview: (OverviewMode, Option<(SwapIndicator, Option<&Tree<Data>>)>),
        resize_indicator: Option<(ResizeMode, ResizeIndicator)>,
        indicator_thickness: u8,
        theme: &cosmic::theme::CosmicTheme,
    ) -> Result<SplitRenderElements<CosmicMappedRenderElement<R>>, OutputNotMapped>
    where
        R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
        <R as Renderer>::TextureId: Send + Clone + 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        CosmicWindowRenderElement<R>: RenderElement<R>,
        CosmicStackRenderElement<R>: RenderElement<R>,
    {
        let output_scale = self.output.current_scale().fractional_scale();

        let (target_tree, duration, _) = if self.queue.animation_start.is_some() {
            self.queue
                .trees
                .get(1)
                .expect("Animation ongoing, should have two trees")
        } else {
            self.queue.trees.front().unwrap()
        };
        let reference_tree = self
            .queue
            .animation_start
            .is_some()
            .then(|| &self.queue.trees.front().unwrap().0);

        let percentage = if let Some(animation_start) = self.queue.animation_start {
            let percentage = Instant::now().duration_since(animation_start).as_millis() as f32
                / duration.as_millis() as f32;
            ease(EaseInOutCubic, 0.0, 1.0, percentage)
        } else {
            1.0
        };
        let draw_groups = overview.0.alpha();

        let mut elements = SplitRenderElements::default();

        let is_overview = !matches!(overview.0, OverviewMode::None);
        let is_mouse_tiling = (matches!(overview.0.trigger(), Some(Trigger::Pointer(_))))
            .then(|| self.last_overview_hover.as_ref().map(|x| &x.1));
        let swap_desc = if let Some(Trigger::KeyboardSwap(_, desc)) = overview.0.trigger() {
            Some(desc.clone())
        } else {
            None
        };

        // all gone windows and fade them out
        let old_geometries = if let Some(reference_tree) = reference_tree.as_ref() {
            let (geometries, _) = if let Some(transition) = draw_groups {
                Some(geometries_for_groupview(
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
                    swap_desc.clone(),
                    overview.1.as_ref().and_then(|(_, tree)| tree.clone()),
                    theme,
                ))
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
                indicator_thickness,
                swap_desc.is_some(),
                theme,
            ));

            geometries
        } else {
            None
        };

        let (geometries, group_elements) = if let Some(transition) = draw_groups {
            Some(geometries_for_groupview(
                target_tree,
                &mut *renderer,
                non_exclusive_zone,
                seat,
                transition,
                transition,
                output_scale,
                &self.placeholder_id,
                is_mouse_tiling,
                swap_desc.clone(),
                overview.1.as_ref().and_then(|(_, tree)| tree.clone()),
                theme,
            ))
        } else {
            None
        }
        .unzip();

        // all alive windows
        elements.extend(render_new_tree(
            target_tree,
            reference_tree,
            renderer,
            non_exclusive_zone,
            geometries,
            old_geometries,
            is_overview,
            seat,
            &self.output,
            percentage,
            draw_groups,
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
            overview,
            resize_indicator,
            swap_desc.clone(),
            &self.swapping_stack_surface_id,
            &self.placeholder_id,
            theme,
        ));

        // tiling hints
        if let Some(group_elements) = group_elements {
            elements.w_elements.extend(group_elements);
        }

        Ok(elements)
    }

    fn gaps(&self) -> (i32, i32) {
        let g = self.theme.cosmic().gaps;
        (g.0 as i32, g.1 as i32)
    }
}

const GAP_KEYBOARD: i32 = 8;
const GAP_MOUSE: i32 = 32;
const PLACEHOLDER_GAP_MOUSE: i32 = 8;
const WINDOW_BACKDROP_BORDER: i32 = 4;
const WINDOW_BACKDROP_GAP: i32 = 12;

const MAX_SWAP_WINDOW_SIZE: (i32, i32) = (360, 240);

fn swap_factor(size: Size<i32, Logical>) -> f64 {
    let target_w = std::cmp::min(size.w, MAX_SWAP_WINDOW_SIZE.0);
    let target_h = std::cmp::min(size.h, MAX_SWAP_WINDOW_SIZE.1);
    (target_w as f64 / size.w as f64).min(target_h as f64 / size.h as f64)
}

fn swap_geometry(
    size: Size<i32, Logical>,
    relative_to: Rectangle<i32, Local>,
) -> Rectangle<i32, Local> {
    let factor = swap_factor(size);

    let new_size = Size::from((
        (size.w as f64 * factor).round() as i32,
        (size.h as f64 * factor).round() as i32,
    ));

    let loc = Point::from((
        relative_to.loc.x + relative_to.size.w - new_size.w,
        relative_to.loc.y,
    ));

    Rectangle::from_loc_and_size(loc, new_size)
}

fn geometries_for_groupview<'a, R>(
    tree: &Tree<Data>,
    renderer: impl Into<Option<&'a mut R>>,
    non_exclusive_zone: Rectangle<i32, Local>,
    seat: Option<&Seat<State>>,
    alpha: f32,
    transition: f32,
    output_scale: f64,
    placeholder_id: &Id,
    mouse_tiling: Option<Option<&TargetZone>>,
    swap_desc: Option<NodeDesc>,
    swap_tree: Option<&Tree<Data>>,
    _theme: &cosmic::theme::CosmicTheme,
) -> (
    HashMap<NodeId, Rectangle<i32, Local>>,
    Vec<CosmicMappedRenderElement<R>>,
)
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer + 'a,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    CosmicWindowRenderElement<R>: RenderElement<R>,
{
    // we need to recalculate geometry for all elements, if we are drawing groups
    let gap: i32 = (if mouse_tiling.is_some() {
        GAP_MOUSE
    } else {
        GAP_KEYBOARD
    } as f32
        * transition)
        .round() as i32;
    let mut renderer = renderer.into();

    let root = tree.root_node_id();
    let mut stack = Vec::new();
    if swap_tree.is_some() {
        // push bogos value, that will get ignored anyway
        stack.push((Rectangle::from_loc_and_size((0, 0), (320, 240)), 0));
    }
    if root.is_some() {
        stack.push((non_exclusive_zone, 0));
    }

    let mut elements = Vec::new();
    let mut geometries: HashMap<NodeId, Rectangle<i32, Local>> = HashMap::new();
    let alpha = alpha * transition;

    let focused = seat
        .and_then(|seat| {
            seat.get_keyboard()
                .unwrap()
                .current_focus()
                .and_then(|target| TilingLayout::currently_focused_node(&tree, target))
        })
        .map(|(id, _)| id);
    let focused_geo = if let Some(focused_id) = focused.as_ref() {
        Some(*tree.get(focused_id).unwrap().data().geometry())
    } else {
        None
    };

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

    for (tree, node_id) in root
        .into_iter()
        .flat_map(|root| tree.traverse_pre_order_ids(root).unwrap())
        .map(|id| (tree, id))
        .chain(
            swap_tree
                .as_ref()
                .filter(|_| swap_desc.as_ref().unwrap().stack_window.is_none())
                .into_iter()
                .flat_map(|tree| {
                    tree.traverse_pre_order_ids(&swap_desc.as_ref().unwrap().node)
                        .unwrap()
                })
                .map(|id| (*swap_tree.as_ref().unwrap(), id)),
        )
    {
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

            let render_potential_group = swap_desc.is_none()
                && has_potential_groups
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

            let group_color = GROUP_COLOR;

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
                            && swap_desc.is_none()
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
                        if (render_potential_group || render_active_child) && Some(&node_id) != root
                        {
                            elements.push(
                                IndicatorShader::element(
                                    *renderer,
                                    Key::Group(Arc::downgrade(alive)),
                                    geo,
                                    4,
                                    if render_active_child { 16 } else { 8 },
                                    alpha * if render_potential_group { 0.40 } else { 1.0 },
                                    output_scale,
                                    group_color,
                                )
                                .into(),
                            );
                        }
                        if mouse_tiling.is_some()
                            && pill_indicator.is_some()
                            && Some(&node_id) != root
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
                                    group_color,
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
                                    Some(PillIndicator::Outer(dir)) => match (dir, orientation) {
                                        (Direction::Left, Orientation::Horizontal)
                                        | (Direction::Right, Orientation::Horizontal)
                                        | (Direction::Up, Orientation::Vertical)
                                        | (Direction::Down, Orientation::Vertical) => true,
                                        (Direction::Left, Orientation::Vertical)
                                        | (Direction::Up, Orientation::Horizontal) => own_idx == 0,
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
                                        group_color,
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
                                        group_color,
                                    )
                                    .into(),
                                );
                            }

                            geo = remaining_geo;
                        };
                    }

                    if matches!(swap_desc, Some(ref desc) if &desc.node == &node_id) {
                        if let Some(renderer) = renderer.as_mut() {
                            elements.push(
                                BackdropShader::element(
                                    *renderer,
                                    Key::Group(Arc::downgrade(alive)),
                                    geo,
                                    8.,
                                    alpha
                                        * if focused
                                            .as_ref()
                                            .map(|focused| {
                                                focused == &swap_desc.as_ref().unwrap().node
                                            })
                                            .unwrap_or(false)
                                        {
                                            0.4
                                        } else {
                                            0.15
                                        },
                                    group_color,
                                )
                                .into(),
                            );
                        }
                        let swap_geo = swap_geometry(
                            geo.size.as_logical(),
                            focused_geo.unwrap_or({
                                let mut geo = non_exclusive_zone;
                                geo.loc += (WINDOW_BACKDROP_BORDER, WINDOW_BACKDROP_BORDER).into();
                                geo.size -=
                                    (WINDOW_BACKDROP_BORDER * 2, WINDOW_BACKDROP_BORDER * 2).into();
                                geo
                            }),
                        );
                        geo = ease(
                            Linear,
                            EaseRectangle(geo),
                            EaseRectangle(swap_geo),
                            transition,
                        )
                        .unwrap();
                        geometries.insert(node_id.clone(), geo);
                    };

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
                                    if let Some(PillIndicator::Inner(pill_idx)) = pill_indicator {
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
                                                        group_color,
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
                                    if let Some(PillIndicator::Inner(pill_idx)) = pill_indicator {
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
                                                        group_color,
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
                                    Key::Window(Usage::PotentialGroupIndicator, mapped.key()),
                                    geo,
                                    4,
                                    8,
                                    alpha * 0.40,
                                    output_scale,
                                    group_color,
                                )
                                .into(),
                            );
                            geo.loc += (gap, gap).into();
                            geo.size -= (gap * 2, gap * 2).into();
                        }

                        let accent = ACTIVE_GROUP_COLOR;

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
                                    accent
                                }
                                _ => group_color,
                            };
                            geo.loc += (WINDOW_BACKDROP_BORDER, WINDOW_BACKDROP_BORDER).into();
                            geo.size -=
                                (WINDOW_BACKDROP_BORDER * 2, WINDOW_BACKDROP_BORDER * 2).into();
                            elements.push(
                                BackdropShader::element(
                                    *renderer,
                                    Key::Window(Usage::OverviewBackdrop, mapped.key()),
                                    geo,
                                    8.,
                                    alpha
                                        * if focused
                                            .as_ref()
                                            .map(|focused_id| focused_id == &node_id)
                                            .unwrap_or(color == accent)
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

                    if matches!(swap_desc, Some(ref desc) if &desc.node == &node_id && desc.stack_window.is_none())
                    {
                        let swap_geo = swap_geometry(
                            geo.size.as_logical(),
                            focused_geo.unwrap_or({
                                let mut geo = non_exclusive_zone;
                                geo.loc += (WINDOW_BACKDROP_BORDER, WINDOW_BACKDROP_BORDER).into();
                                geo.size -=
                                    (WINDOW_BACKDROP_BORDER * 2, WINDOW_BACKDROP_BORDER * 2).into();
                                geo
                            }),
                        );
                        geo = ease(
                            Linear,
                            EaseRectangle(geo),
                            EaseRectangle(swap_geo),
                            transition,
                        )
                        .unwrap();
                    };

                    geometries.insert(node_id.clone(), geo);
                }
                Data::Placeholder { .. } => {
                    geo.loc += (element_gap_left, element_gap_up).into();
                    geo.size -= (element_gap_left, element_gap_up).into();
                    geo.size -= (element_gap_right, element_gap_down).into();

                    if let Some(renderer) = renderer.as_mut() {
                        geo.loc += (WINDOW_BACKDROP_BORDER, WINDOW_BACKDROP_BORDER).into();
                        geo.size -= (WINDOW_BACKDROP_BORDER * 2, WINDOW_BACKDROP_BORDER * 2).into();
                        elements.push(
                            BackdropShader::element(
                                *renderer,
                                placeholder_id.clone(),
                                geo,
                                8.,
                                alpha * 0.4,
                                group_color,
                            )
                            .into(),
                        );
                    }

                    geometries.insert(node_id.clone(), geo);
                }
            }
        }
    }

    (geometries, elements)
}

fn render_old_tree<R>(
    reference_tree: &Tree<Data>,
    target_tree: &Tree<Data>,
    renderer: &mut R,
    geometries: Option<HashMap<NodeId, Rectangle<i32, Local>>>,
    output_scale: f64,
    percentage: f32,
    indicator_thickness: u8,
    is_swap_mode: bool,
    theme: &cosmic::theme::CosmicTheme,
) -> SplitRenderElements<CosmicMappedRenderElement<R>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: Send + Clone + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    CosmicWindowRenderElement<R>: RenderElement<R>,
    CosmicStackRenderElement<R>: RenderElement<R>,
{
    let window_hint = crate::theme::active_window_hint(theme);
    let mut elements = SplitRenderElements::default();

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
                        minimize_rect,
                        ..
                    } => (
                        mapped,
                        last_geometry,
                        geometries.get(&node_id).copied(),
                        minimize_rect,
                    ),
                    _ => unreachable!(),
                },
            )
            .filter(|(mapped, _, _, _)| {
                if let Some(root) = target_tree.root_node_id() {
                    is_swap_mode
                        || !target_tree
                            .traverse_pre_order(root)
                            .unwrap()
                            .any(|node| node.data().is_mapped(Some(mapped)))
                } else {
                    true
                }
            })
            .for_each(|(mapped, original_geo, mut scaled_geo, minimize_geo)| {
                if let Some(minimize_geo) = minimize_geo {
                    scaled_geo = Some(
                        ease(
                            EaseInOutCubic,
                            EaseRectangle(*original_geo),
                            EaseRectangle(*minimize_geo),
                            percentage,
                        )
                        .unwrap(),
                    );
                }

                let (scale, offset) = scaled_geo
                    .map(|adapted_geo| scale_to_center(&original_geo, &adapted_geo))
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

                let alpha = if minimize_geo.is_some() {
                    1.0 - ((percentage - 0.5).max(0.0) * 2.0)
                } else {
                    1.0 - percentage
                };

                let elem_geometry = mapped.geometry().to_physical_precise_round(output_scale);
                let SplitRenderElements {
                    w_elements,
                    p_elements,
                } = mapped.split_render_elements::<R, CosmicMappedRenderElement<R>>(
                    renderer,
                    geo.loc.as_logical().to_physical_precise_round(output_scale)
                        - elem_geometry.loc,
                    Scale::from(output_scale),
                    alpha,
                );

                elements
                    .w_elements
                    .extend(w_elements.into_iter().flat_map(|element| {
                        match element {
                            CosmicMappedRenderElement::Stack(elem) => constrain_render_elements(
                                std::iter::once(elem),
                                geo.loc.as_logical().to_physical_precise_round(output_scale)
                                    - elem_geometry.loc,
                                geo.as_logical().to_physical_precise_round(output_scale),
                                elem_geometry,
                                ConstrainScaleBehavior::Stretch,
                                ConstrainAlign::CENTER,
                                output_scale,
                            )
                            .next()
                            .map(CosmicMappedRenderElement::TiledStack),
                            CosmicMappedRenderElement::Window(elem) => constrain_render_elements(
                                std::iter::once(elem),
                                geo.loc.as_logical().to_physical_precise_round(output_scale)
                                    - elem_geometry.loc,
                                geo.as_logical().to_physical_precise_round(output_scale),
                                elem_geometry,
                                ConstrainScaleBehavior::Stretch,
                                ConstrainAlign::CENTER,
                                output_scale,
                            )
                            .next()
                            .map(CosmicMappedRenderElement::TiledWindow),
                            x => Some(x),
                        }
                    }));
                if minimize_geo.is_some() && indicator_thickness > 0 {
                    elements
                        .w_elements
                        .push(CosmicMappedRenderElement::FocusIndicator(
                            IndicatorShader::focus_element(
                                renderer,
                                Key::Window(Usage::FocusIndicator, mapped.clone().key()),
                                geo,
                                indicator_thickness,
                                output_scale,
                                alpha,
                                [window_hint.red, window_hint.green, window_hint.blue],
                            ),
                        ));
                }
                elements.p_elements.extend(p_elements);
            });
    }

    elements
}

fn render_new_tree<R>(
    target_tree: &Tree<Data>,
    reference_tree: Option<&Tree<Data>>,
    renderer: &mut R,
    non_exclusive_zone: Rectangle<i32, Local>,
    geometries: Option<HashMap<NodeId, Rectangle<i32, Local>>>,
    old_geometries: Option<HashMap<NodeId, Rectangle<i32, Local>>>,
    is_overview: bool,
    seat: Option<&Seat<State>>,
    output: &Output,
    percentage: f32,
    transition: Option<f32>,
    indicator_thickness: u8,
    overview: (OverviewMode, Option<(SwapIndicator, Option<&Tree<Data>>)>),
    mut resize_indicator: Option<(ResizeMode, ResizeIndicator)>,
    swap_desc: Option<NodeDesc>,
    swapping_stack_surface_id: &Id,
    placeholder_id: &Id,
    theme: &cosmic::theme::CosmicTheme,
) -> SplitRenderElements<CosmicMappedRenderElement<R>>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: Send + Clone + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    CosmicWindowRenderElement<R>: RenderElement<R>,
    CosmicStackRenderElement<R>: RenderElement<R>,
{
    let focused = seat
        .and_then(|seat| {
            seat.get_keyboard()
                .unwrap()
                .current_focus()
                .and_then(|target| TilingLayout::currently_focused_node(&target_tree, target))
        })
        .map(|(id, _)| id);
    let focused_geo = if let Some(focused) = focused.as_ref() {
        geometries
            .as_ref()
            .and_then(|geometries| geometries.get(focused))
            .or_else(|| {
                target_tree
                    .get(focused)
                    .ok()
                    .map(|node| node.data().geometry())
            })
            .cloned()
    } else {
        None
    }
    .unwrap_or({
        let mut geo = non_exclusive_zone;
        geo.loc += (WINDOW_BACKDROP_BORDER, WINDOW_BACKDROP_BORDER).into();
        geo.size -= (WINDOW_BACKDROP_BORDER * 2, WINDOW_BACKDROP_BORDER * 2).into();
        geo
    });

    let is_active_output = seat
        .map(|seat| &seat.active_output() == output)
        .unwrap_or(false);

    let mut animating_window_elements = Vec::new();
    let mut window_elements = Vec::new();
    let mut popup_elements = Vec::new();

    let mut group_backdrop = None;
    let mut indicators = Vec::new();
    let mut resize_elements = None;
    let mut swap_elements = Vec::new();

    let output_geo = output.geometry();
    let output_scale = output.current_scale().fractional_scale();

    let (swap_indicator, swap_tree) = overview.1.unzip();
    let swap_tree = swap_tree.flatten().filter(|_| is_active_output);
    let swap_desc = swap_desc.filter(|_| is_active_output);
    let window_hint = crate::theme::active_window_hint(theme);
    let group_color = GROUP_COLOR;
    // render placeholder, if we are swapping to an empty workspace
    if target_tree.root_node_id().is_none() && swap_desc.is_some() {
        window_elements.push(
            BackdropShader::element(
                renderer,
                placeholder_id.clone(),
                focused_geo,
                8.,
                transition.unwrap_or(1.0) * 0.4,
                group_color,
            )
            .into(),
        );
    }

    // render single stack window when swapping separately
    if let Some(window) = swap_desc
        .as_ref()
        .and_then(|desc| desc.stack_window.clone())
    {
        let window_geo = window.geometry();
        let swap_geo = ease(
            Linear,
            EaseRectangle({
                let mut geo = focused_geo.clone();
                geo.loc.x += STACK_TAB_HEIGHT;
                geo.size.h -= STACK_TAB_HEIGHT;
                geo
            }),
            EaseRectangle(swap_geometry(window_geo.size, focused_geo)),
            transition.unwrap_or(1.0),
        )
        .unwrap();

        indicators.push(IndicatorShader::focus_element(
            renderer,
            Key::Static(swapping_stack_surface_id.clone()),
            swap_geo,
            4,
            output_scale,
            transition.unwrap_or(1.0),
            [window_hint.red, window_hint.green, window_hint.blue],
        ));

        let render_loc =
            (swap_geo.loc.as_logical() - window_geo.loc).to_physical_precise_round(output_scale);

        swap_elements.extend(
            window
                .render_elements::<CosmicWindowRenderElement<R>>(
                    renderer,
                    render_loc,
                    output_scale.into(),
                    1.0,
                )
                .into_iter()
                .map(|window| {
                    CosmicMappedRenderElement::GrabbedWindow(RescaleRenderElement::from_element(
                        window,
                        swap_geo
                            .loc
                            .as_logical()
                            .to_physical_precise_round(output_scale),
                        ease(
                            Linear,
                            1.0,
                            swap_factor(window_geo.size),
                            transition.unwrap_or(1.0),
                        ),
                    ))
                }),
        )
    }

    // render actual tree nodes
    let old_geometries = old_geometries.unwrap_or_default();
    let geometries = geometries.unwrap_or_default();
    target_tree
        .root_node_id()
        .into_iter()
        .flat_map(|root| target_tree.traverse_pre_order_ids(root).unwrap())
        .map(|id| (target_tree, id))
        .chain(
            swap_tree
                .into_iter()
                .flat_map(|tree| {
                    let sub_root = &swap_desc.as_ref().unwrap().node;
                    if swap_desc.as_ref().unwrap().stack_window.is_none() {
                        Some(
                            tree.traverse_pre_order_ids(sub_root)
                                .unwrap()
                                .map(move |id| (tree, id)),
                        )
                    } else {
                        None
                    }
                })
                .flatten(),
        )
        .for_each(|(target_tree, node_id)| {
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
            let mut old_geo = old_original_geo.map(|original_geo| {
                let (scale, offset) = old_scaled_geo
                    .unwrap()
                    .map(|adapted_geo| scale_to_center(original_geo, adapted_geo))
                    .unwrap_or_else(|| (1.0.into(), (0, 0).into()));
                (
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
                        .unwrap_or(*original_geo),
                    1.0,
                )
            });

            let was_minimized = if let Data::Mapped {
                minimize_rect: Some(minimize_rect),
                ..
            } = &data
            {
                old_geo = Some((*minimize_rect, (percentage * 2.0).min(1.0)));
                true
            } else {
                false
            };

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

            let (geo, alpha, animating) = if let Some((old_geo, alpha)) = old_geo.filter(|_| {
                swap_desc
                    .as_ref()
                    .map(|desc| desc.node != node_id && desc.stack_window.is_none())
                    .unwrap_or(true)
            }) {
                (
                    if was_minimized {
                        ease(
                            EaseInOutCubic,
                            EaseRectangle(old_geo),
                            EaseRectangle(new_geo),
                            percentage,
                        )
                        .unwrap()
                    } else {
                        ease(
                            Linear,
                            EaseRectangle(old_geo),
                            EaseRectangle(new_geo),
                            percentage,
                        )
                        .unwrap()
                    },
                    alpha,
                    old_geo != new_geo,
                )
            } else {
                (new_geo, percentage, false)
            };

            if swap_desc.as_ref().map(|desc| &desc.node) == Some(&node_id)
                || focused.as_ref() == Some(&node_id)
            {
                if indicator_thickness > 0 || data.is_group() {
                    let mut geo = geo.clone();
                    if data.is_group() {
                        let outer_gap: i32 = (if is_overview { GAP_KEYBOARD } else { 4 } as f32
                            * percentage)
                            .round() as i32;
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
                            group_color,
                        ));
                    }

                    if !swap_desc
                        .as_ref()
                        .map(|desc| desc.stack_window.is_some())
                        .unwrap_or(false)
                        || focused.as_ref() == Some(&node_id)
                    {
                        indicators.push(IndicatorShader::focus_element(
                            renderer,
                            match data {
                                Data::Mapped { mapped, .. } => {
                                    Key::Window(Usage::FocusIndicator, mapped.clone().key())
                                }
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
                            alpha,
                            [window_hint.red, window_hint.green, window_hint.blue],
                        ));
                    }

                    if focused.as_ref() == Some(&node_id)
                        && (swap_desc.as_ref().map(|desc| &desc.node) != Some(&node_id)
                            || swap_desc
                                .as_ref()
                                .and_then(|swap_desc| swap_desc.stack_window.as_ref())
                                .zip(focused.as_ref())
                                .map(|(stack_window, focused_id)| {
                                    target_tree
                                        .get(focused_id)
                                        .ok()
                                        .map(|focused| match focused.data() {
                                            Data::Mapped { mapped, .. } => mapped
                                                .stack_ref()
                                                .map(|stack| &stack.active() != stack_window)
                                                .unwrap_or(false),
                                            _ => false,
                                        })
                                        .unwrap_or(false)
                                })
                                .unwrap_or(false))
                    {
                        if let Some(swap) = swap_indicator.as_ref() {
                            swap.resize(geo.size.as_logical());
                            swap.output_enter(output, output_geo.as_logical());
                            swap_elements.extend(
                                swap.render_elements::<CosmicWindowRenderElement<R>>(
                                    renderer,
                                    geo.loc.as_logical().to_physical_precise_round(output_scale),
                                    output_scale.into(),
                                    alpha * overview.0.alpha().unwrap_or(1.0),
                                )
                                .into_iter()
                                .map(CosmicMappedRenderElement::from),
                            );
                        }
                    }
                }

                if let Some((mode, resize)) = resize_indicator.as_mut() {
                    let mut geo = geo.clone();
                    geo.loc -= (18, 18).into();
                    geo.size += (36, 36).into();

                    resize.resize(geo.size.as_logical());
                    resize.output_enter(output, output_geo.as_logical());
                    let possible_edges =
                        TilingLayout::possible_resizes(target_tree, node_id.clone());
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
                                    geo.loc.as_logical().to_physical_precise_round(output_scale),
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
                let elem_geometry = mapped.geometry().to_physical_precise_round(output_scale);
                let SplitRenderElements {
                    mut w_elements,
                    p_elements,
                } = mapped.split_render_elements::<R, CosmicMappedRenderElement<R>>(
                    renderer,
                    //original_location,
                    geo.loc.as_logical().to_physical_precise_round(output_scale)
                        - elem_geometry.loc,
                    Scale::from(output_scale),
                    alpha,
                );
                if swap_desc
                    .as_ref()
                    .filter(|swap_desc| swap_desc.node == node_id)
                    .and_then(|swap_desc| swap_desc.stack_window.as_ref())
                    .zip(focused.as_ref())
                    .map(|(stack_window, focused_id)| {
                        target_tree
                            .get(focused_id)
                            .ok()
                            .map(|focused| match focused.data() {
                                Data::Mapped { mapped, .. } => mapped
                                    .stack_ref()
                                    .map(|stack| &stack.active() == stack_window)
                                    .unwrap_or(false),
                                _ => false,
                            })
                            .unwrap_or(false)
                    })
                    .unwrap_or(false)
                {
                    let mut geo = mapped.active_window_geometry().as_local();
                    geo.loc += original_geo.loc;
                    w_elements.insert(
                        0,
                        CosmicMappedRenderElement::Overlay(BackdropShader::element(
                            renderer,
                            Key::Window(Usage::Overlay, mapped.key()),
                            geo,
                            0.0,
                            0.3,
                            group_color,
                        )),
                    )
                }

                let (behavior, align) = if is_overview {
                    (ConstrainScaleBehavior::Fit, ConstrainAlign::CENTER)
                } else if animating {
                    (ConstrainScaleBehavior::Stretch, ConstrainAlign::TOP_LEFT)
                } else {
                    (ConstrainScaleBehavior::CutOff, ConstrainAlign::TOP_LEFT)
                };

                let w_elements = w_elements.into_iter().flat_map(|element| match element {
                    CosmicMappedRenderElement::Stack(elem) => constrain_render_elements(
                        std::iter::once(elem),
                        geo.loc.as_logical().to_physical_precise_round(output_scale)
                            - elem_geometry.loc,
                        geo.as_logical().to_physical_precise_round(output_scale),
                        elem_geometry,
                        behavior,
                        align,
                        output_scale,
                    )
                    .next()
                    .map(CosmicMappedRenderElement::TiledStack),
                    CosmicMappedRenderElement::Window(elem) => constrain_render_elements(
                        std::iter::once(elem),
                        geo.loc.as_logical().to_physical_precise_round(output_scale)
                            - elem_geometry.loc,
                        geo.as_logical().to_physical_precise_round(output_scale),
                        elem_geometry,
                        behavior,
                        align,
                        output_scale,
                    )
                    .next()
                    .map(CosmicMappedRenderElement::TiledWindow),
                    CosmicMappedRenderElement::Overlay(elem) => constrain_render_elements(
                        std::iter::once(elem),
                        geo.loc.as_logical().to_physical_precise_round(output_scale)
                            - elem_geometry.loc,
                        geo.as_logical().to_physical_precise_round(output_scale),
                        elem_geometry,
                        behavior,
                        align,
                        output_scale,
                    )
                    .next()
                    .map(CosmicMappedRenderElement::TiledOverlay),
                    x => Some(x),
                });
                if swap_desc
                    .as_ref()
                    .map(|swap_desc| {
                        (&swap_desc.node == &node_id
                            || target_tree
                                .ancestor_ids(&node_id)
                                .unwrap()
                                .any(|id| &swap_desc.node == id))
                            && swap_desc.stack_window.is_none()
                    })
                    .unwrap_or(false)
                {
                    swap_elements.extend(w_elements);
                } else {
                    if animating {
                        animating_window_elements.extend(w_elements);
                    } else {
                        window_elements.extend(w_elements);
                    }
                    if !mapped.is_maximized(false) {
                        popup_elements.extend(p_elements);
                    }
                }
            }
        });

    window_elements = resize_elements
        .into_iter()
        .flatten()
        .chain(swap_elements)
        .chain(indicators.into_iter().map(Into::into))
        .chain(window_elements)
        .chain(animating_window_elements)
        .chain(group_backdrop.into_iter().map(Into::into))
        .collect();

    SplitRenderElements {
        w_elements: window_elements,
        p_elements: popup_elements,
    }
}

fn scale_to_center<C>(
    old_geo: &Rectangle<i32, C>,
    new_geo: &Rectangle<i32, C>,
) -> (f64, Point<i32, C>) {
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
