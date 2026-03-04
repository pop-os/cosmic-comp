// SPDX-License-Identifier: GPL-3.0-only

use clap_lex::OsStrExt;
use rustix::path::Arg;
use smithay::{
    backend::{
        SwapBuffersError,
        allocator::{
            Allocator,
            dmabuf::{AnyError, Dmabuf, DmabufAllocator},
            gbm::GbmAllocator,
        },
        drm::{CreateDrmNodeError, DrmNode},
        renderer::{
            gles::{GlesError, GlesRenderer, ffi},
            glow::GlowRenderer,
            multigpu::{ApiDevice, GraphicsApi},
        },
    },
    reexports::drm::control::Device,
};
use std::{
    borrow::{Borrow, BorrowMut},
    cell::Cell,
    ffi::{CStr, c_char},
};
use std::{
    collections::HashMap,
    fmt,
    os::unix::prelude::AsFd,
    sync::atomic::{AtomicBool, Ordering},
};

/// Errors raised by the [`GbmGlesBackend`]
#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// OpenGL error
    #[error(transparent)]
    Gl(#[from] GlesError),
    /// Error creating a drm node
    #[error(transparent)]
    DrmNode(#[from] CreateDrmNodeError),
}

impl From<Error> for SwapBuffersError {
    fn from(err: Error) -> SwapBuffersError {
        match err {
            x @ Error::DrmNode(_) => SwapBuffersError::ContextLost(Box::new(x)),
            Error::Gl(x) => x.into(),
        }
    }
}

/// A [`GraphicsApi`] utilizing user-provided GBM Devices and OpenGL ES for rendering.
pub struct GbmGlowBackend<A: AsFd + 'static> {
    devices: HashMap<DrmNode, (GbmAllocator<A>, Cell<Option<GlowRenderer>>)>,
    needs_enumeration: AtomicBool,
}

impl<A: AsFd + fmt::Debug + 'static> fmt::Debug for GbmGlowBackend<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GbmGlesBackend")
            .field("devices", &self.devices.keys())
            .field("needs_enumeration", &self.needs_enumeration)
            .finish()
    }
}

impl<A: AsFd + 'static> Default for GbmGlowBackend<A> {
    fn default() -> Self {
        GbmGlowBackend {
            devices: HashMap::new(),
            needs_enumeration: AtomicBool::new(true),
        }
    }
}

impl<A: AsFd + Clone + Send + 'static> GbmGlowBackend<A> {
    pub fn new() -> Self {
        GbmGlowBackend {
            devices: HashMap::new(),
            needs_enumeration: AtomicBool::new(false),
        }
    }

    pub fn current_devices(&self) -> impl Iterator<Item = &DrmNode> {
        self.devices.keys()
    }

    pub fn add_node(&mut self, node: DrmNode, gbm: GbmAllocator<A>, renderer: GlowRenderer) {
        if self.devices.contains_key(&node) {
            return;
        }

        self.devices.insert(node, (gbm, Cell::new(Some(renderer))));
        self.needs_enumeration.store(true, Ordering::SeqCst);
    }

    /// Remove a given node from the api
    pub fn remove_node(&mut self, node: &DrmNode) {
        if self.devices.remove(node).is_some() {
            self.needs_enumeration.store(true, Ordering::SeqCst);
        }
    }
}

impl<A: AsFd + Device + Clone + 'static> GraphicsApi for GbmGlowBackend<A> {
    type Device = GbmGlowDevice;
    type Error = Error;

    fn enumerate(&self, list: &mut Vec<Self::Device>) -> Result<(), Self::Error> {
        self.needs_enumeration.store(false, Ordering::SeqCst);

        // remove old stuff
        list.retain(|renderer| {
            self.devices
                .keys()
                .any(|node| renderer.node.dev_id() == node.dev_id())
        });

        // add new stuff
        let new_renderers = self
            .devices
            .iter()
            // but don't replace already initialized renderers
            .filter(|(node, _)| {
                !list
                    .iter()
                    .any(|renderer| renderer.node.dev_id() == node.dev_id())
            })
            .flat_map(|(node, (allocator, renderer))| {
                let mut renderer = renderer.replace(None)?;
                let is_intel = allocator
                    .as_ref()
                    .get_driver()
                    .is_ok_and(|drv| drv.name().contains("i915"));
                let intel_export_quirk = is_intel
                    && BorrowMut::<GlesRenderer>::borrow_mut(&mut renderer)
                        .with_context(|gl| unsafe {
                            CStr::from_ptr(gl.GetString(ffi::RENDERER) as *const c_char)
                        })
                        .is_ok_and(|name| {
                            name.as_str().is_ok_and(|name| {
                                name.contains("MTL") || name.contains("ARL") || name.contains("LNL")
                            })
                        });

                Some(GbmGlowDevice {
                    node: *node,
                    renderer,
                    intel_export_quirk,
                    allocator: Box::new(DmabufAllocator(allocator.clone())),
                })
            })
            .collect::<Vec<GbmGlowDevice>>();
        list.extend(new_renderers);

        Ok(())
    }

    fn needs_enumeration(&self) -> bool {
        self.needs_enumeration.load(Ordering::Acquire)
    }

    fn identifier() -> &'static str {
        "gbm_glow"
    }
}

/// [`ApiDevice`] of the [`GbmGlesBackend`]
pub struct GbmGlowDevice {
    node: DrmNode,
    renderer: GlowRenderer,
    intel_export_quirk: bool,
    allocator: Box<dyn Allocator<Buffer = Dmabuf, Error = AnyError>>,
}

impl fmt::Debug for GbmGlowDevice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GbmGlesDevice")
            .field("node", &self.node)
            .field("renderer", &self.renderer)
            .finish_non_exhaustive()
    }
}

impl ApiDevice for GbmGlowDevice {
    type Renderer = GlowRenderer;

    fn renderer(&self) -> &Self::Renderer {
        &self.renderer
    }
    fn renderer_mut(&mut self) -> &mut Self::Renderer {
        &mut self.renderer
    }
    fn allocator(&mut self) -> &mut dyn Allocator<Buffer = Dmabuf, Error = AnyError> {
        self.allocator.as_mut()
    }
    fn node(&self) -> &DrmNode {
        &self.node
    }
    fn can_do_cross_device_imports(&self) -> bool {
        !Borrow::<GlesRenderer>::borrow(&self.renderer).is_software()
    }

    fn should_do_cross_device_exports(&self) -> bool {
        !self.intel_export_quirk
    }
}
