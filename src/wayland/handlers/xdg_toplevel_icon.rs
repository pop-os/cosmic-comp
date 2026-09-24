// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    reexports::wayland_server::protocol::{wl_buffer::WlBuffer, wl_shm, wl_surface::WlSurface},
    wayland::{
        compositor::with_states,
        shm::with_buffer_contents,
        xdg_toplevel_icon::{ToplevelIconCachedState, XdgToplevelIconHandler},
    },
};

use crate::{
    state::State,
    wayland::protocols::toplevel_info::{
        MAX_RASTER_BYTES, MAX_RASTER_SOURCE_PIXELS, MAX_RASTER_SOURCES, RgbaIcon, WindowIcon,
    },
};

fn shm_pixels_to_rgba(
    data: &[u8],
    width: i32,
    height: i32,
    stride: i32,
    format: wl_shm::Format,
) -> Option<Vec<u8>> {
    if width <= 0 || height <= 0 {
        return None;
    }
    if !matches!(
        format,
        wl_shm::Format::Argb8888
            | wl_shm::Format::Xrgb8888
            | wl_shm::Format::Abgr8888
            | wl_shm::Format::Xbgr8888
    ) {
        return None;
    }

    let width = usize::try_from(width).ok()?;
    let height = usize::try_from(height).ok()?;
    if width.checked_mul(height)? > MAX_RASTER_SOURCE_PIXELS {
        return None;
    }
    let stride = usize::try_from(stride).ok()?;
    let row_len = width.checked_mul(4)?;
    if stride < row_len {
        return None;
    }
    let len = height.checked_mul(row_len)?;
    let required = (height - 1).checked_mul(stride)?.checked_add(row_len)?;
    if data.len() < required {
        return None;
    }

    let mut rgba = Vec::with_capacity(len);
    for row in data.chunks(stride).take(height) {
        for pixel in row[..row_len].chunks_exact(4) {
            let packed = u32::from_ne_bytes(pixel.try_into().ok()?);
            let alpha = if matches!(format, wl_shm::Format::Xrgb8888 | wl_shm::Format::Xbgr8888) {
                255
            } else {
                (packed >> 24) as u8
            };
            let unpremultiply = |channel: u8| {
                if alpha == 0 {
                    0
                } else {
                    u8::try_from(
                        (u32::from(channel) * 255 + u32::from(alpha) / 2) / u32::from(alpha),
                    )
                    .unwrap_or(255)
                }
            };
            let (red, green, blue) =
                if matches!(format, wl_shm::Format::Abgr8888 | wl_shm::Format::Xbgr8888) {
                    (packed as u8, (packed >> 8) as u8, (packed >> 16) as u8)
                } else {
                    ((packed >> 16) as u8, (packed >> 8) as u8, packed as u8)
                };
            rgba.extend_from_slice(&[
                unpremultiply(red),
                unpremultiply(green),
                unpremultiply(blue),
                alpha,
            ]);
        }
    }
    Some(rgba)
}

fn shm_layout(
    width: i32,
    height: i32,
    stride: i32,
    offset: i32,
    pool_len: usize,
) -> Option<(usize, usize, usize)> {
    let width = usize::try_from(width).ok()?;
    let height = usize::try_from(height).ok()?;
    let stride = usize::try_from(stride).ok()?;
    let offset = usize::try_from(offset).ok()?;
    let pixels = width.checked_mul(height)?;
    if pixels == 0 || pixels > MAX_RASTER_SOURCE_PIXELS {
        return None;
    }
    let row_len = width.checked_mul(4)?;
    if stride < row_len {
        return None;
    }
    let packed_len = height.checked_mul(row_len)?;
    let span = (height - 1).checked_mul(stride)?.checked_add(row_len)?;
    let end = offset.checked_add(span)?;
    (end <= pool_len).then_some((row_len, end, packed_len))
}

fn buffer_to_icon(buffer: &WlBuffer, max_bytes: usize) -> Option<RgbaIcon> {
    with_buffer_contents(buffer, |ptr, pool_len, metadata| {
        let offset = usize::try_from(metadata.offset).ok()?;
        let stride = usize::try_from(metadata.stride).ok()?;
        let height = usize::try_from(metadata.height).ok()?;
        let (row_len, _, packed_len) = shm_layout(
            metadata.width,
            metadata.height,
            metadata.stride,
            metadata.offset,
            pool_len,
        )?;
        if packed_len > max_bytes {
            return None;
        }

        let mut bytes = vec![0; packed_len];
        for row in 0..height {
            unsafe {
                std::ptr::copy_nonoverlapping(
                    ptr.add(offset + row * stride),
                    bytes.as_mut_ptr().add(row * row_len),
                    row_len,
                );
            }
        }
        let pixels = shm_pixels_to_rgba(
            &bytes,
            metadata.width,
            metadata.height,
            i32::try_from(row_len).ok()?,
            metadata.format,
        )?;
        RgbaIcon::new(
            u32::try_from(metadata.width).ok()?,
            u32::try_from(metadata.height).ok()?,
            pixels,
        )
    })
    .ok()?
}

pub(super) fn icon_for_surface(surface: &WlSurface) -> Option<WindowIcon> {
    with_states(surface, |states| {
        let mut cached = states.cached_state.get::<ToplevelIconCachedState>();
        let icon = cached.current();

        let name = icon.icon_name().map(str::to_owned);
        let mut remaining = MAX_RASTER_BYTES;
        let mut buffers = Vec::new();
        for (buffer, _scale) in icon.buffers() {
            if buffers.len() == MAX_RASTER_SOURCES {
                break;
            }
            let Some(rgba) = buffer_to_icon(buffer, remaining) else {
                continue;
            };
            remaining -= rgba.pixels().len();
            buffers.push(rgba);
        }

        WindowIcon::new(name, buffers)
    })
}

impl XdgToplevelIconHandler for State {}
