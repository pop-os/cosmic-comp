// SPDX-License-Identifier: GPL-3.0-only

pub use smithay::{
    desktop::Space,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    utils::{Logical, Rectangle, Size},
    wayland::output::Output,
};
use std::{cell::Cell, mem::MaybeUninit};

pub const MAX_WORKSPACES: usize = 10; // TODO?

pub struct ActiveWorkspace(Cell<Option<usize>>);
impl ActiveWorkspace {
    fn new() -> Self {
        ActiveWorkspace(Cell::new(None))
    }
    pub fn get(&self) -> Option<usize> {
        self.0.get()
    }
    fn set(&self, active: usize) -> Option<usize> {
        self.0.replace(Some(active))
    }
    fn clear(&self) -> Option<usize> {
        self.0.replace(None)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Mode {
    OutputBound,
    Global { active: usize },
}

impl Mode {
    pub fn output_bound() -> Mode {
        Mode::OutputBound
    }

    pub fn global() -> Mode {
        Mode::Global { active: 0 }
    }
}

pub struct Workspaces {
    mode: Mode,
    outputs: Vec<Output>,
    pub spaces: [Space; MAX_WORKSPACES],
}

const UNINIT_SPACE: MaybeUninit<Space> = MaybeUninit::uninit();

impl Workspaces {
    pub fn new() -> Self {
        Workspaces {
            mode: Mode::global(),
            outputs: Vec::new(),
            spaces: unsafe {
                let mut spaces = [UNINIT_SPACE; MAX_WORKSPACES];
                spaces.fill_with(|| MaybeUninit::new(Space::new(None)));
                std::mem::transmute(spaces)
            },
        }
    }

    pub fn map_output(&mut self, output: &Output) {
        match self.mode {
            Mode::OutputBound => {
                output
                    .user_data()
                    .insert_if_missing(|| ActiveWorkspace::new());

                let (idx, space) = self
                    .spaces
                    .iter_mut()
                    .enumerate()
                    .find(|(_, x)| x.outputs().next().is_none())
                    .expect("More then 10 outputs?");
                output
                    .user_data()
                    .get::<ActiveWorkspace>()
                    .unwrap()
                    .set(idx);
                space.map_output(output, 1.0, (0, 0));
                self.outputs.push(output.clone());
            }
            Mode::Global { active } => {
                // just put new outputs on the right of the previous ones.
                // in the future we will only need that as a fallback and need to read saved configurations here
                let space = &mut self.spaces[active];
                let x = space
                    .outputs()
                    .map(|output| space.output_geometry(&output).unwrap())
                    .fold(0, |acc, geo| std::cmp::max(acc, geo.loc.x + geo.size.w));
                space.map_output(output, 1.0, (x, 0));
                self.outputs.push(output.clone());
            }
        }
    }

    pub fn unmap_output(&mut self, output: &Output) {
        match self.mode {
            Mode::OutputBound => {
                if let Some(idx) = output
                    .user_data()
                    .get::<ActiveWorkspace>()
                    .and_then(|a| a.get())
                {
                    self.spaces[idx].unmap_output(output);
                    self.outputs.retain(|o| o != output);
                }
            }
            Mode::Global { active } => {
                self.spaces[active].unmap_output(output);
                self.outputs.retain(|o| o != output);
                // TODO move windows and outputs farther on the right / or load save config for remaining monitors
            }
        }
    }

    pub fn output_size(&self, output: &Output) -> Size<i32, Logical> {
        let space = self.active_space(output);
        space
            .output_geometry(&output)
            .unwrap_or(Rectangle::from_loc_and_size((0, 0), (0, 0)))
            .size
    }

    pub fn output_geometry(&self, output: &Output) -> Rectangle<i32, Logical> {
        // due to our different modes, we cannot just ask the space for the global output coordinates,
        // because for `Mode::OutputBound` the origin will always be (0, 0)

        // TODO: Add a proper grid like structure, for now the outputs just extend to the right
        let pos =
            self.outputs
                .iter()
                .take_while(|o| o != &output)
                .fold((0, 0), |(x, y), output| {
                    let size = self.output_size(output);
                    (x + size.w, y)
                });

        Rectangle::from_loc_and_size(pos, self.output_size(output))
    }

    pub fn activate(&mut self, output: &Output, idx: usize) {
        match self.mode {
            Mode::OutputBound => {
                // TODO check for other outputs already occupying that space
                if let Some(active) = output.user_data().get::<ActiveWorkspace>() {
                    if let Some(old_idx) = active.set(idx) {
                        self.spaces[old_idx].unmap_output(output);
                    }
                    self.spaces[idx].map_output(output, 1.0, (0, 0));
                }
                // TODO translate windows from previous space size into new size
            }
            Mode::Global { ref mut active } => {
                let old = *active;
                *active = idx;
                for output in &self.outputs {
                    let loc = self.spaces[old].output_geometry(output).unwrap().loc;
                    self.spaces[old].unmap_output(output);
                    self.spaces[*active].map_output(output, 1.0, loc);
                }
            }
        };
    }

    #[cfg(feature = "debug")]
    pub fn mode(&self) -> &Mode {
        &self.mode
    }

    pub fn set_mode(&mut self, mode: Mode) {
        match (&mut self.mode, mode) {
            (Mode::OutputBound, Mode::Global { .. }) => {
                let active = self
                    .outputs
                    .iter()
                    .next()
                    .map(|o| {
                        o.user_data()
                            .get::<ActiveWorkspace>()
                            .unwrap()
                            .get()
                            .unwrap()
                    })
                    .unwrap_or(0);
                let mut x = 0;

                for output in &self.outputs {
                    let old_active = output
                        .user_data()
                        .get::<ActiveWorkspace>()
                        .unwrap()
                        .clear()
                        .unwrap();
                    let width = self.spaces[old_active]
                        .output_geometry(output)
                        .unwrap()
                        .size
                        .w;
                    self.spaces[old_active].unmap_output(output);
                    self.spaces[active].map_output(output, 1.0, (x, 0));
                    x += width;
                }

                self.mode = Mode::Global { active };
                // TODO move windows into new bounds
            }
            (Mode::Global { active }, new @ Mode::OutputBound) => {
                for output in &self.outputs {
                    self.spaces[*active].unmap_output(output);
                }

                self.mode = new;
                let outputs = self.outputs.drain(..).collect::<Vec<_>>();
                for output in &outputs {
                    self.map_output(output);
                }
                // TODO move windows into new bounds
                // TODO active should probably be mapped somewhere
            }
            _ => {}
        };
    }

    pub fn outputs(&self) -> impl Iterator<Item = &Output> {
        self.outputs.iter()
    }

    pub fn active_space(&self, output: &Output) -> &Space {
        match &self.mode {
            Mode::OutputBound => {
                let active = output
                    .user_data()
                    .get::<ActiveWorkspace>()
                    .unwrap()
                    .get()
                    .unwrap();
                &self.spaces[active]
            }
            Mode::Global { active } => &self.spaces[*active],
        }
    }

    pub fn active_space_mut(&mut self, output: &Output) -> &mut Space {
        match &self.mode {
            Mode::OutputBound => {
                let active = output
                    .user_data()
                    .get::<ActiveWorkspace>()
                    .unwrap()
                    .get()
                    .unwrap();
                &mut self.spaces[active]
            }
            Mode::Global { active } => &mut self.spaces[*active],
        }
    }

    pub fn space_for_surface(&self, surface: &WlSurface) -> Option<&Space> {
        self.spaces
            .iter()
            .find(|space| space.window_for_surface(surface).is_some())
    }

    pub fn space_for_surface_mut(&mut self, surface: &WlSurface) -> Option<&mut Space> {
        self.spaces
            .iter_mut()
            .find(|space| space.window_for_surface(surface).is_some())
    }

    pub fn refresh(&mut self) {
        for space in &mut self.spaces {
            space.refresh()
        }
    }

    pub fn commit(&mut self, surface: &WlSurface) {
        for space in &mut self.spaces {
            space.commit(surface)
        }
    }
}
