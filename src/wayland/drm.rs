// SPDX-License-Identifier: GPL-3.0-only

// Re-export only the actual code, and then only use this re-export
// The `generated` module below is just some boilerplate to properly isolate stuff
// and avoid exposing internal details.
//
// You can use all the types from my_protocol as if they went from `wayland_client::protocol`.
pub use generated::server::wl_drm;

mod generated {
    // The generated code tends to trigger a lot of warnings
    // so we isolate it into a very permissive module
    #![allow(dead_code, non_camel_case_types, unused_unsafe, unused_variables)]
    #![allow(non_upper_case_globals, non_snake_case, unused_imports)]

    pub mod server {
        use smithay::reexports::{wayland_commons, wayland_server};

        // These imports are used by the generated code
        pub(crate) use wayland_commons::map::{Object, ObjectMetadata};
        pub(crate) use wayland_commons::smallvec;
        pub(crate) use wayland_commons::wire::{Argument, ArgumentType, Message, MessageDesc};
        pub(crate) use wayland_commons::{Interface, MessageGroup};
        pub(crate) use wayland_server::protocol::wl_buffer;
        pub(crate) use wayland_server::sys;
        pub(crate) use wayland_server::{AnonymousObject, Main, Resource, ResourceMap};
        include!(concat!(env!("OUT_DIR"), "/wl_drm.rs"));
    }
}

use smithay::{
    backend::allocator::{
        dmabuf::{Dmabuf, DmabufFlags},
        Format, Fourcc, Modifier,
    },
    reexports::wayland_server::{Client, Display, Filter, Global, Main},
};

use std::{convert::TryFrom, path::PathBuf};

pub fn init_wl_drm_global<F>(
    display: &mut Display,
    device_path: PathBuf,
    mut formats: Vec<Format>,
    client_filter: F,
) -> Global<wl_drm::WlDrm>
where
    F: FnMut(Client) -> bool + 'static,
{
    formats.dedup_by(|f1, f2| f1.code == f2.code);
    let global = Filter::new(move |(drm, version): (Main<wl_drm::WlDrm>, u32), _, _| {
        drm.quick_assign(move |drm, req, _| match req {
            wl_drm::Request::Authenticate { .. } => drm.authenticated(),
            wl_drm::Request::CreateBuffer { id, .. } => {
                id.as_ref().post_error(
                    wl_drm::Error::InvalidName.to_raw(),
                    String::from("Flink handles are unsupported, use PRIME"),
                );
            }
            wl_drm::Request::CreatePlanarBuffer { id, .. } => {
                id.as_ref().post_error(
                    wl_drm::Error::InvalidName.to_raw(),
                    String::from("Flink handles are unsupported, use PRIME"),
                );
            }
            wl_drm::Request::CreatePrimeBuffer {
                id,
                name,
                width,
                height,
                format,
                offset0,
                stride0,
                ..
            } => {
                let format = match Fourcc::try_from(format) {
                    Ok(format) => format,
                    Err(_) => {
                        id.as_ref().post_error(
                            wl_drm::Error::InvalidFormat.to_raw(),
                            String::from("Format not advertised by wl_drm"),
                        );
                        return;
                    }
                };

                if width < 1 || height < 1 {
                    id.as_ref().post_error(
                        wl_drm::Error::InvalidFormat.to_raw(),
                        String::from("width or height not positive"),
                    );
                    return;
                }

                let mut dma = Dmabuf::builder((width, height), format, DmabufFlags::empty());
                dma.add_plane(name, 0, offset0 as u32, stride0 as u32, Modifier::Invalid);
                id.as_ref()
                    .user_data()
                    .set_threadsafe(|| dma.build().unwrap());
                id.quick_assign(|_, _, _| {});
                slog_scope::trace!("Created a new validated dma wl_buffer via wl_drm.");
            }
        });
        drm.device(device_path.to_string_lossy().into_owned());
        if version >= 2 {
            drm.capabilities(wl_drm::Capability::Prime.to_raw());
        }
        for format in &formats {
            if let Some(converted) = wl_drm::Format::from_raw(format.code as u32) {
                drm.format(converted.to_raw());
            }
        }
    });
    display.create_global_with_filter(2, global, client_filter)
}
