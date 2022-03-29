// SPDX-License-Identifier: GPL-3.0-only

use crate::shell::layout::{Layout, Orientation};
use id_tree::{InsertBehavior, MoveBehavior, Node, NodeId, NodeIdError, RemoveBehavior, Tree};
use smithay::{
    desktop::{layer_map_for_output, Kind, Space, Window},
    reexports::wayland_protocols::xdg_shell::server::xdg_toplevel::State as XdgState,
    utils::Rectangle,
    wayland::{output::Output, seat::Seat},
};
use std::{cell::RefCell, collections::HashMap};

#[derive(Debug)]
pub struct TilingLayout {
    idx: u8,
    gaps: (i32, i32),
}

#[derive(Debug)]
pub enum Data {
    Fork {
        orientation: Orientation,
        ratio: f64,
    },
    Stack {
        active: usize,
        len: usize,
    },
    Window(Window),
}

#[derive(Debug, Clone)]
pub struct WindowInfo {
    node: NodeId,
    output: Output,
}

pub struct OutputInfo {
    trees: HashMap<u8, Tree<Data>>,
}

impl Default for OutputInfo {
    fn default() -> OutputInfo {
        OutputInfo {
            trees: HashMap::new(),
        }
    }
}

impl Data {
    fn fork() -> Data {
        Data::Fork {
            orientation: Orientation::Vertical,
            ratio: 0.5,
        }
    }
}

impl TilingLayout {
    pub fn new(idx: u8) -> TilingLayout {
        TilingLayout { idx, gaps: (0, 4) }
    }
}

impl Layout for TilingLayout {
    fn map_window<'a>(
        &mut self,
        space: &mut Space,
        window: &Window,
        seat: &Seat,
        mut focus_stack: Box<dyn Iterator<Item = &'a Window> + 'a>,
    ) {
        {
            let output = super::output_from_seat(seat, space);
            output
                .user_data()
                .insert_if_missing(|| RefCell::new(OutputInfo::default()));
            let mut output_info = output
                .user_data()
                .get::<RefCell<OutputInfo>>()
                .unwrap()
                .borrow_mut();
            let tree = &mut output_info.trees.entry(self.idx).or_insert_with(Tree::new);

            let new_window = Node::new(Data::Window(window.clone()));

            let last_active = focus_stack
                .find_map(|window| tree.root_node_id()
                    .and_then(|root| tree.traverse_pre_order_ids(root).unwrap()
                        .find(|id| matches!(tree.get(id).map(|n| n.data()), Ok(Data::Window(w)) if w == window))
                    )
                );
            let window_id = if let Some(ref node_id) = last_active {
                let parent_id = tree.get(node_id).unwrap().parent().cloned();
                if let Some(stack_id) = parent_id
                    .filter(|id| matches!(tree.get(id).unwrap().data(), Data::Stack { .. }))
                {
                    // we add to the stack
                    let window_id = tree
                        .insert(new_window, InsertBehavior::UnderNode(&stack_id))
                        .unwrap();
                    if let Data::Stack {
                        ref mut len,
                        ref mut active,
                    } = tree.get_mut(&stack_id).unwrap().data_mut()
                    {
                        *active = *len;
                        *len += 1;
                    }
                    Ok(window_id)
                } else {
                    // we create a new fork
                    new_fork(tree, node_id, new_window)
                }
            } else {
                // nothing? then we add to the root
                if let Some(root_id) = tree.root_node_id().cloned() {
                    new_fork(tree, &root_id, new_window)
                } else {
                    tree.insert(new_window, InsertBehavior::AsRoot)
                }
            }
            .unwrap();

            {
                let user_data = window.user_data();
                let window_info = WindowInfo {
                    node: window_id.clone(),
                    output: output.clone(),
                };
                // insert or update
                if !user_data.insert_if_missing(|| RefCell::new(window_info.clone())) {
                    *user_data.get::<RefCell<WindowInfo>>().unwrap().borrow_mut() = window_info;
                }
            }
        }
        self.refresh(space);
    }

    fn update_orientation<'a>(
        &mut self,
        new_orientation: Orientation,
        seat: &Seat,
        space: &mut Space,
        mut focus_stack: Box<dyn Iterator<Item = &'a Window> + 'a>,
    ) {
        {
            let output = super::output_from_seat(seat, space);
            output
                .user_data()
                .insert_if_missing(|| RefCell::new(OutputInfo::default()));
            let mut output_info = output
                .user_data()
                .get::<RefCell<OutputInfo>>()
                .unwrap()
                .borrow_mut();
            let tree = &mut output_info.trees.entry(self.idx).or_insert_with(Tree::new);

            let last_active = focus_stack
                .find_map(|window| tree.root_node_id()
                    .and_then(|root| tree.traverse_pre_order_ids(root).unwrap()
                        .find(|id| matches!(tree.get(id).map(|n| n.data()), Ok(Data::Window(w)) if w == window))
                    )
                );

            if let Some(ref node_id) = last_active {
                let mut node_id = node_id.clone();
                while let Some(parent_id) = tree.get(&node_id).unwrap().parent().cloned() {
                    if let &mut Data::Fork {
                        ref mut orientation,
                        ..
                    } = tree.get_mut(&parent_id).unwrap().data_mut()
                    {
                        *orientation = new_orientation;
                        break;
                    }
                    node_id = parent_id;
                }
            }
        }
        self.refresh(space);
    }

    fn refresh(&mut self, space: &mut Space) {
        while let Some(dead_windows) =
            Some(update_space_positions(self.idx, space, self.gaps)).filter(|v| !v.is_empty())
        {
            for window in dead_windows {
                self.unmap_window(&window);
            }
        }
    }
}

impl TilingLayout {
    fn unmap_window(&mut self, window: &Window) {
        if let Some(info) = window.user_data().get::<RefCell<WindowInfo>>() {
            let output = &info.borrow().output;
            let mut output_info = output
                .user_data()
                .get::<RefCell<OutputInfo>>()
                .unwrap()
                .borrow_mut();
            let tree = &mut output_info.trees.entry(self.idx).or_insert_with(Tree::new);

            let node_id = info.borrow().node.clone();
            let parent_id = tree
                .get(&node_id)
                .ok()
                .and_then(|node| node.parent())
                .cloned();
            let parent_parent_id = parent_id.as_ref().and_then(|parent_id| {
                tree.get(parent_id)
                    .ok()
                    .and_then(|node| node.parent())
                    .cloned()
            });

            // remove self
            slog_scope::debug!("Remove window {:?}", window);
            let _ = tree.remove_node(node_id.clone(), RemoveBehavior::DropChildren);

            // fixup parent node
            match parent_id {
                Some(id) if matches!(tree.get(&id).unwrap().data(), Data::Fork { .. }) => {
                    slog_scope::debug!("Removing Fork");
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
                Some(id) if matches!(tree.get(&id).unwrap().data(), Data::Stack { .. }) => {
                    if tree.children_ids(&id).unwrap().count() == 0 {
                        slog_scope::debug!("Removing Stack");
                        // remove stack
                        let _ = tree.remove_node(id.clone(), RemoveBehavior::DropChildren);
                        // TODO now we need to untangle the parent_parent
                        // So we should REFACTOR this unmap function to not only work with windows
                    } else {
                        // fixup stack values
                        if let Data::Stack {
                            ref mut active,
                            ref mut len,
                        } = tree.get_mut(&id).unwrap().data_mut()
                        {
                            *len -= 1;
                            *active = std::cmp::max(*active, *len - 1);
                        }
                    }
                }
                None => {} // root
                _ => unreachable!(),
            }
            // update_space_positions(space, self.gaps);
        }
    }
}

fn new_fork(
    tree: &mut Tree<Data>,
    old_id: &NodeId,
    new: Node<Data>,
) -> Result<NodeId, NodeIdError> {
    let new_fork = Node::new(Data::fork());
    let old = tree.get(old_id)?;
    let parent_id = old.parent().cloned();
    let pos = parent_id.as_ref().and_then(|parent_id| {
        tree.children_ids(parent_id)
            .unwrap()
            .position(|id| id == old_id)
    });

    let fork_id = tree
        .insert(
            new_fork,
            if let Some(parent) = parent_id.as_ref() {
                InsertBehavior::UnderNode(parent)
            } else {
                InsertBehavior::AsRoot
            },
        )
        .unwrap();

    tree.move_node(old_id, MoveBehavior::ToParent(&fork_id))
        .unwrap();
    // keep position
    if let Some(old_pos) = pos {
        tree.make_nth_sibling(&fork_id, old_pos).unwrap();
    }
    tree.insert(new, InsertBehavior::UnderNode(&fork_id))
}

fn update_space_positions(idx: u8, space: &mut Space, gaps: (i32, i32)) -> Vec<Window> {
    let mut dead_windows = Vec::new();
    let (outer, inner) = gaps;
    for output in space.outputs().cloned().collect::<Vec<_>>().into_iter() {
        if let Some(mut info) = output
            .user_data()
            .get::<RefCell<OutputInfo>>()
            .map(|x| x.borrow_mut())
        {
            let tree = &mut info.trees.entry(idx).or_insert_with(Tree::new);
            if let Some(root) = tree.root_node_id() {
                let mut stack = Vec::new();

                let mut geo = Some(layer_map_for_output(&output).non_exclusive_zone());
                // TODO saturate? minimum?
                if let Some(mut geo) = geo.as_mut() {
                    geo.loc.x += outer;
                    geo.loc.y += outer;
                    geo.size.w -= outer * 2;
                    geo.size.h -= outer * 2;
                }

                for node in tree.traverse_pre_order(root).unwrap() {
                    let geo = stack.pop().unwrap_or(geo);
                    match node.data() {
                        Data::Fork { orientation, ratio } => {
                            if let Some(geo) = geo {
                                match orientation {
                                    Orientation::Horizontal => {
                                        let top_size = (
                                            geo.size.w,
                                            ((geo.size.h as f64) * ratio).ceil() as i32,
                                        );
                                        stack.push(Some(Rectangle::from_loc_and_size(
                                            (geo.loc.x, geo.loc.y + top_size.1),
                                            (geo.size.w, geo.size.h - top_size.1),
                                        )));
                                        stack.push(Some(Rectangle::from_loc_and_size(
                                            geo.loc, top_size,
                                        )));
                                    }
                                    Orientation::Vertical => {
                                        let left_size = (
                                            ((geo.size.w as f64) * ratio).ceil() as i32,
                                            geo.size.h,
                                        );
                                        stack.push(Some(Rectangle::from_loc_and_size(
                                            (geo.loc.x + left_size.0, geo.loc.y),
                                            (geo.size.w - left_size.0, geo.size.h),
                                        )));
                                        stack.push(Some(Rectangle::from_loc_and_size(
                                            geo.loc, left_size,
                                        )));
                                    }
                                }
                            } else {
                                stack.push(None);
                                stack.push(None);
                            }
                        }
                        Data::Stack { active, len } => {
                            for i in 0..*len {
                                if i == *active {
                                    stack.push(geo);
                                } else {
                                    stack.push(None);
                                }
                            }
                        }
                        Data::Window(window) => {
                            if window.toplevel().alive() {
                                if let Some(geo) = geo {
                                    #[allow(irrefutable_let_patterns)]
                                    if let Kind::Xdg(xdg) = &window.toplevel() {
                                        let ret = xdg.with_pending_state(|state| {
                                            state.size = Some(
                                                (geo.size.w - inner * 2, geo.size.h - inner * 2)
                                                    .into(),
                                            );
                                            state.states.set(XdgState::TiledLeft);
                                            state.states.set(XdgState::TiledRight);
                                            state.states.set(XdgState::TiledTop);
                                            state.states.set(XdgState::TiledBottom);
                                        });
                                        if ret.is_ok() {
                                            xdg.send_configure();
                                        }
                                    }
                                    let window_geo = window.geometry();
                                    space.map_window(
                                        &window,
                                        (
                                            geo.loc.x + inner - window_geo.loc.x,
                                            geo.loc.y + inner - window_geo.loc.y,
                                        ),
                                        false,
                                    );
                                }
                            } else {
                                dead_windows.push(window.clone());
                            }
                        }
                    }
                }
            }
        }
    }
    dead_windows
}
