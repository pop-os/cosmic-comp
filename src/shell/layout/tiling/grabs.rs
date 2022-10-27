// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::{focus::target::PointerFocusTarget, layout::Orientation},
    utils::prelude::*,
};
use id_tree::NodeId;
use smithay::{
    input::pointer::{
        AxisFrame, ButtonEvent, GrabStartData as PointerGrabStartData, MotionEvent, PointerGrab,
        PointerInnerHandle,
    },
    output::{Output, WeakOutput},
    utils::{Logical, Point},
};

use super::Data;

pub struct ResizeForkGrab {
    start_data: PointerGrabStartData<State>,
    idx: usize,
    initial_size_upleft: i32,
    initial_size_downright: i32,
    node: NodeId,
    output: WeakOutput,
}

impl ResizeForkGrab {
    pub fn new(
        start_data: PointerGrabStartData<State>,
        node: NodeId,
        output: &Output,
        data: &Data,
        idx: usize,
    ) -> ResizeForkGrab {
        let sizes = match data {
            Data::Group { ref sizes, .. } => sizes,
            _ => panic!("Resizing without a group?!?"),
        };

        ResizeForkGrab {
            start_data,
            idx,
            initial_size_upleft: sizes.iter().take(idx + 1).sum(),
            initial_size_downright: sizes.iter().skip(idx + 1).sum(),
            node,
            output: output.downgrade(),
        }
    }
}

impl PointerGrab<State> for ResizeForkGrab {
    fn motion(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<i32, Logical>)>,
        event: &MotionEvent,
    ) {
        // While the grab is active, no client has pointer focus
        handle.motion(data, None, event);

        let delta = event.location - self.start_data.location;

        if let Some(output) = self.output.upgrade() {
            let tiling_layer = &mut data.common.shell.active_space_mut(&output).tiling_layer;
            if let Some(tree) = tiling_layer.trees.get_mut(&output) {
                if tree.get(&self.node).is_ok() {
                    let orientation = tree.get(&self.node).unwrap().data().orientation();
                    let delta = match orientation {
                        Orientation::Vertical => delta.x,
                        Orientation::Horizontal => delta.y,
                    }
                    .round() as i32;

                    let upleft_node_id =
                        match tree.children_ids(&self.node).unwrap().skip(self.idx).next() {
                            Some(elem) => elem,
                            None => {
                                return;
                            }
                        };
                    let downright_node_id = match tree
                        .children_ids(&self.node)
                        .unwrap()
                        .skip(self.idx + 1)
                        .next()
                    {
                        Some(elem) => elem,
                        None => {
                            return;
                        }
                    };

                    let next_mapped = |mut node| loop {
                        if let Some(node_id) = node {
                            match tree.get(&node_id).unwrap().data() {
                                Data::Group { orientation: o, .. } if o == &orientation => {
                                    node = tree.children_ids(&node_id).unwrap().last().cloned();
                                }
                                _ => {
                                    break node_id;
                                }
                            }
                        } else {
                            unreachable!()
                        }
                    };
                    let upleft_mapped_id = next_mapped(Some(upleft_node_id.clone()));
                    let downright_mapped_id = next_mapped(Some(downright_node_id.clone()));

                    let new_upleft_size = self.initial_size_upleft + delta;
                    let new_downright_size = self.initial_size_downright - delta;
                    let new_upleft_mapped_size = match orientation {
                        Orientation::Horizontal => {
                            tree.get(&upleft_mapped_id)
                                .unwrap()
                                .data()
                                .geometry()
                                .size
                                .h
                        }
                        Orientation::Vertical => {
                            tree.get(&upleft_mapped_id)
                                .unwrap()
                                .data()
                                .geometry()
                                .size
                                .w
                        }
                    } + delta;
                    let new_downright_mapped_size = match orientation {
                        Orientation::Horizontal => {
                            tree.get(&downright_mapped_id)
                                .unwrap()
                                .data()
                                .geometry()
                                .size
                                .h
                        }
                        Orientation::Vertical => {
                            tree.get(&downright_mapped_id)
                                .unwrap()
                                .data()
                                .geometry()
                                .size
                                .w
                        }
                    } - delta;

                    if new_upleft_mapped_size > 100 && new_downright_mapped_size > 100 {
                        // lets update
                        {
                            let node = tree.get_mut(&self.node).unwrap();
                            let data = node.data_mut();
                            match data {
                                Data::Group { sizes, .. } => {
                                    sizes[self.idx] = new_upleft_size;
                                    sizes[self.idx + 1] = new_downright_size;
                                }
                                _ => unreachable!(),
                            };
                        }
                        for (mapped_id, mapped_size) in &[
                            (upleft_mapped_id, new_upleft_mapped_size),
                            (downright_mapped_id, new_downright_mapped_size),
                        ] {
                            let parent = tree.get(mapped_id).unwrap().parent().cloned().unwrap();
                            if parent != self.node {
                                let idx = tree
                                    .children_ids(&parent)
                                    .unwrap()
                                    .position(|id| id == mapped_id)
                                    .unwrap();
                                let node = tree.get_mut(&parent).unwrap();
                                let data = node.data_mut();
                                match data {
                                    Data::Group { sizes, .. } => {
                                        sizes[idx] = *mapped_size;
                                    }
                                    _ => unreachable!(),
                                };
                            }
                        }
                        return tiling_layer.refresh();
                    }
                }
            }
        }
    }

    fn button(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &ButtonEvent,
    ) {
        handle.button(data, event);
        if handle.current_pressed().is_empty() {
            // No more buttons are pressed, release the grab.
            handle.unset_grab(data, event.serial, event.time);
        }
    }

    fn axis(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        details: AxisFrame,
    ) {
        handle.axis(data, details)
    }

    fn start_data(&self) -> &PointerGrabStartData<State> {
        &self.start_data
    }
}
