use std::{
    collections::HashMap,
    fmt,
    os::fd::AsFd,
    sync::atomic::{AtomicBool, Ordering},
};

use smithay::backend::{
    allocator::{
        dmabuf::{AnyError, Dmabuf, DmabufAllocator},
        gbm::{GbmAllocator, GbmBufferFlags, GbmDevice},
        Allocator,
    },
    drm::DrmNode,
    renderer::{
        multigpu::{ApiDevice, GraphicsApi},
        pixman::{PixmanError, PixmanRenderer},
    },
};
use tracing::warn;

#[derive(Debug)]
pub struct GbmPixmanBackend<A: AsFd + 'static> {
    devices: HashMap<DrmNode, GbmAllocator<A>>,
    needs_enumeration: AtomicBool,
    allocator_flags: GbmBufferFlags,
}

pub struct GbmPixmanDevice {
    node: DrmNode,
    allocator: Box<dyn Allocator<Buffer = Dmabuf, Error = AnyError>>,
    renderer: PixmanRenderer,
}

impl fmt::Debug for GbmPixmanDevice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GbmPixmanDevice")
            .field("node", &self.node)
            .field("allocator", &"...")
            .field("renderer", &self.renderer)
            .finish()
    }
}

impl<A: AsFd + 'static> GbmPixmanBackend<A> {
    pub fn new() -> Self {
        GbmPixmanBackend {
            devices: HashMap::new(),
            needs_enumeration: AtomicBool::new(false),
            allocator_flags: GbmBufferFlags::RENDERING,
        }
    }

    pub fn with_allocator_flags(allocator_flags: GbmBufferFlags) -> Self {
        GbmPixmanBackend {
            devices: HashMap::new(),
            needs_enumeration: AtomicBool::new(false),
            allocator_flags,
        }
    }

    pub fn set_allocator_flags(&mut self, flags: GbmBufferFlags) {
        self.allocator_flags = flags;
    }

    pub fn add_node(&mut self, node: DrmNode, gbm: GbmDevice<A>) {
        if self.devices.contains_key(&node) {
            return;
        }

        let allocator = GbmAllocator::new(gbm, self.allocator_flags);
        self.devices.insert(node, allocator);
        self.needs_enumeration.store(true, Ordering::SeqCst);
    }

    /// Remove a given node from the api
    pub fn remove_node(&mut self, node: &DrmNode) {
        if self.devices.remove(node).is_some() {
            self.needs_enumeration.store(true, Ordering::SeqCst);
        }
    }
}

impl<A: AsFd + Clone + 'static> GraphicsApi for GbmPixmanBackend<A> {
    type Device = GbmPixmanDevice;

    type Error = PixmanError;

    fn enumerate(&self, list: &mut Vec<Self::Device>) -> Result<(), Self::Error> {
        self.needs_enumeration.store(false, Ordering::SeqCst);

        list.retain(|renderer| {
            self.devices
                .keys()
                .any(|node| renderer.node.dev_id() == node.dev_id())
        });

        // add new stuff
        let new_renderers = self
            .devices
            .iter()
            .filter(|(node, _)| {
                !list
                    .iter()
                    .any(|renderer| renderer.node.dev_id() == node.dev_id())
            })
            .map(|(node, gbm)| {
                Ok(GbmPixmanDevice {
                    node: *node,
                    allocator: Box::new(DmabufAllocator(gbm.clone()))
                        as Box<dyn Allocator<Buffer = Dmabuf, Error = AnyError>>,
                    renderer: PixmanRenderer::new()?,
                })
            })
            .flat_map(|x: Result<GbmPixmanDevice, PixmanError>| match x {
                Ok(x) => Some(x),
                Err(x) => {
                    warn!("Skipping pixman device: {}", x);
                    None
                }
            })
            .collect::<Vec<GbmPixmanDevice>>();
        list.extend(new_renderers);

        Ok(())
    }

    fn identifier() -> &'static str {
        "gbm_pixman"
    }

    fn needs_enumeration(&self) -> bool {
        self.needs_enumeration.load(Ordering::SeqCst)
    }
}

impl ApiDevice for GbmPixmanDevice {
    type Renderer = PixmanRenderer;

    fn renderer(&self) -> &Self::Renderer {
        &self.renderer
    }

    fn renderer_mut(&mut self) -> &mut Self::Renderer {
        &mut self.renderer
    }

    fn allocator(&mut self) -> &mut dyn Allocator<Buffer = Dmabuf, Error = AnyError> {
        &mut self.allocator
    }

    fn node(&self) -> &DrmNode {
        &self.node
    }

    fn can_do_cross_device_imports(&self) -> bool {
        false
    }
}
