// SPDX-License-Identifier: GPL-3.0-only

//! Version 2 of `ext_background_effect_v1`.
//!
//! Version 1 is the upstream staging protocol and is implemented by smithay.
//! We bind the global ourselves instead, because version 2 adds three requests
//! upstream does not have -- blur strength, corner rounding, and a whole-surface
//! mode -- and a client cannot get those through smithay's version 1 dispatch.
//!
//! Version 1 clients are unaffected: they negotiate version 1 and only ever send
//! the requests upstream defines.

// Re-export only the actual code, and then only use this re-export
// The `generated` module below is just some boilerplate to properly isolate stuff
// and avoid exposing internal details.
pub use generated::{ext_background_effect_manager_v1, ext_background_effect_surface_v1};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/ext-background-effect-v1.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/ext-background-effect-v1.xml");
}

use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
        backend::GlobalId, protocol::wl_surface::WlSurface,
    },
    wayland::compositor::{self, RegionAttributes},
};

use ext_background_effect_manager_v1::{Capability, ExtBackgroundEffectManagerV1};
use ext_background_effect_surface_v1::ExtBackgroundEffectSurfaceV1;

/// Handler for the background-effect protocol.
///
/// Each callback reports one piece of pending state. All of it is
/// double-buffered by the protocol, so implementations should write to the
/// surface's pending cached state and let the commit apply it together.
pub trait BackgroundEffectHandler: 'static {
    /// The effects this compositor can actually apply.
    fn capabilities(&self) -> Capability {
        Capability::Blur
    }

    /// The client set a blur region. Supersedes any whole-surface request.
    fn set_blur_region(&mut self, surface: WlSurface, region: RegionAttributes);

    /// The client removed the effect.
    fn unset_blur_region(&mut self, surface: WlSurface);

    /// The client asked for the whole surface to be blurred, tracking its size.
    fn set_blur_whole_surface(&mut self, surface: WlSurface);

    /// The client asked for a blur strength. `None` means the compositor
    /// default; the value is a hint and may be clamped.
    fn set_blur_radius(&mut self, surface: WlSurface, radius: Option<u32>);

    /// The client asked for the blurred area's corners to be rounded,
    /// clockwise from top-left.
    fn set_corner_radius(&mut self, surface: WlSurface, radii: [u32; 4]);
}

/// State and global for the background-effect protocol.
#[derive(Debug)]
pub struct BackgroundEffectState {
    global: GlobalId,
}

impl BackgroundEffectState {
    /// Advertise the global.
    pub fn new<D>(dh: &DisplayHandle) -> Self
    where
        D: GlobalDispatch<ExtBackgroundEffectManagerV1, ()>
            + Dispatch<ExtBackgroundEffectManagerV1, ()>
            + BackgroundEffectHandler
            + 'static,
    {
        let global = dh.create_global::<D, ExtBackgroundEffectManagerV1, _>(2, ());
        Self { global }
    }

    /// The global id, for taking the protocol back down.
    pub fn global(&self) -> GlobalId {
        self.global.clone()
    }
}

/// Ties a surface object back to the surface it was created for.
///
/// Held weakly: the client may destroy the surface while still holding the
/// effect object, and the protocol requires that to be an error rather than a
/// crash.
#[derive(Debug)]
pub struct BackgroundEffectSurfaceData(Weak<WlSurface>);

impl<D> GlobalDispatch<ExtBackgroundEffectManagerV1, (), D> for BackgroundEffectState
where
    D: GlobalDispatch<ExtBackgroundEffectManagerV1, ()>
        + Dispatch<ExtBackgroundEffectManagerV1, ()>
        + BackgroundEffectHandler
        + 'static,
{
    fn bind(
        state: &mut D,
        _dh: &DisplayHandle,
        _client: &Client,
        resource: New<ExtBackgroundEffectManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        let manager = data_init.init(resource, ());
        // The protocol requires this before any other event.
        manager.capabilities(state.capabilities());
    }
}

impl<D> Dispatch<ExtBackgroundEffectManagerV1, (), D> for BackgroundEffectState
where
    D: Dispatch<ExtBackgroundEffectManagerV1, ()>
        + Dispatch<ExtBackgroundEffectSurfaceV1, BackgroundEffectSurfaceData>
        + BackgroundEffectHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        manager: &ExtBackgroundEffectManagerV1,
        request: ext_background_effect_manager_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            ext_background_effect_manager_v1::Request::GetBackgroundEffect { id, surface } => {
                if compositor::with_states(&surface, |states| {
                    states
                        .data_map
                        .insert_if_missing_threadsafe(HasEffect::default);
                    states
                        .data_map
                        .get::<HasEffect>()
                        .map(|taken| taken.0.swap(true, std::sync::atomic::Ordering::AcqRel))
                        .unwrap_or(false)
                }) {
                    manager.post_error(
                        ext_background_effect_manager_v1::Error::BackgroundEffectExists,
                        "surface already has a background effect object",
                    );
                    return;
                }

                data_init.init(id, BackgroundEffectSurfaceData(surface.downgrade()));
            }
            ext_background_effect_manager_v1::Request::Destroy => {}
            _ => unreachable!(),
        }
    }
}

/// Marks a surface as already having an effect object, so a second
/// `get_background_effect` raises the protocol error rather than silently
/// producing two objects writing the same state.
#[derive(Debug, Default)]
struct HasEffect(std::sync::atomic::AtomicBool);

impl<D> Dispatch<ExtBackgroundEffectSurfaceV1, BackgroundEffectSurfaceData, D>
    for BackgroundEffectState
where
    D: Dispatch<ExtBackgroundEffectSurfaceV1, BackgroundEffectSurfaceData>
        + BackgroundEffectHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &ExtBackgroundEffectSurfaceV1,
        request: ext_background_effect_surface_v1::Request,
        data: &BackgroundEffectSurfaceData,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        // Every request but destroy needs the surface, and the protocol says
        // touching a destroyed one is an error rather than a no-op.
        let surface = match data.0.upgrade() {
            Ok(surface) => surface,
            Err(_) => {
                if !matches!(request, ext_background_effect_surface_v1::Request::Destroy) {
                    resource.post_error(
                        ext_background_effect_surface_v1::Error::SurfaceDestroyed,
                        "the associated surface has been destroyed",
                    );
                }
                return;
            }
        };

        match request {
            ext_background_effect_surface_v1::Request::SetBlurRegion { region } => match region {
                Some(region) => {
                    let attrs = compositor::get_region_attributes(&region);
                    state.set_blur_region(surface, attrs);
                }
                None => state.unset_blur_region(surface),
            },
            ext_background_effect_surface_v1::Request::BlurWholeSurface => {
                state.set_blur_whole_surface(surface);
            }
            ext_background_effect_surface_v1::Request::SetBlurRadius { radius } => {
                // 0 is "compositor default", not "no blur".
                state.set_blur_radius(surface, (radius != 0).then_some(radius));
            }
            ext_background_effect_surface_v1::Request::SetCornerRadius {
                top_left,
                top_right,
                bottom_right,
                bottom_left,
            } => {
                state.set_corner_radius(surface, [top_left, top_right, bottom_right, bottom_left]);
            }
            ext_background_effect_surface_v1::Request::Destroy => {
                compositor::with_states(&surface, |states| {
                    if let Some(taken) = states.data_map.get::<HasEffect>() {
                        taken.0.store(false, std::sync::atomic::Ordering::Release);
                    }
                });
                state.unset_blur_region(surface);
            }
            _ => unreachable!(),
        }
    }
}

/// Wire up the protocol for a compositor state type.
#[macro_export]
macro_rules! delegate_background_effect {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::background_effect::ext_background_effect_manager_v1::ExtBackgroundEffectManagerV1: ()
        ] => $crate::wayland::protocols::background_effect::BackgroundEffectState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::background_effect::ext_background_effect_manager_v1::ExtBackgroundEffectManagerV1: ()
        ] => $crate::wayland::protocols::background_effect::BackgroundEffectState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::background_effect::ext_background_effect_surface_v1::ExtBackgroundEffectSurfaceV1: $crate::wayland::protocols::background_effect::BackgroundEffectSurfaceData
        ] => $crate::wayland::protocols::background_effect::BackgroundEffectState);
    };
}
