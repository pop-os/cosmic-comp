// SPDX-License-Identifier: GPL-3.0-only

use crate::utils::prelude::*;
use smithay::{
    backend::renderer::{
        element::{
            surface::{render_elements_from_surface_tree, WaylandSurfaceRenderElement},
            texture::{TextureBuffer, TextureRenderElement},
        },
        ImportAll, ImportMem, Renderer,
    },
    input::{
        pointer::{CursorImageAttributes, CursorImageStatus},
        Seat,
    },
    reexports::wayland_server::protocol::wl_surface,
    render_elements,
    utils::{IsAlive, Logical, Monotonic, Point, Scale, Time, Transform},
    wayland::compositor::{get_role, with_states},
};
use std::{
    any::{Any, TypeId},
    cell::RefCell,
    collections::HashMap,
    io::Read,
    sync::Mutex,
    time::Duration,
};
use xcursor::{
    parser::{parse_xcursor, Image},
    CursorTheme,
};

static FALLBACK_CURSOR_DATA: &[u8] = include_bytes!("../../../resources/cursor.rgba");

#[derive(Debug, Clone)]
pub struct Cursor {
    icons: Vec<Image>,
    size: u32,
}

impl Cursor {
    pub fn load() -> Cursor {
        let name = std::env::var("XCURSOR_THEME")
            .ok()
            .unwrap_or_else(|| "default".into());
        let size = std::env::var("XCURSOR_SIZE")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(24);

        let theme = CursorTheme::load(&name);
        let icons = load_icon(&theme)
            .map_err(|err| {
                slog_scope::warn!("Unable to load xcursor: {}, using fallback cursor", err)
            })
            .unwrap_or_else(|_| {
                vec![Image {
                    size: 32,
                    width: 64,
                    height: 64,
                    xhot: 1,
                    yhot: 1,
                    delay: 1,
                    pixels_rgba: Vec::from(FALLBACK_CURSOR_DATA),
                    pixels_argb: vec![], //unused
                }]
            });

        Cursor { icons, size }
    }

    pub fn get_image(&self, scale: u32, millis: u32) -> Image {
        let size = self.size * scale;
        frame(millis, size, &self.icons)
    }
}

impl Default for Cursor {
    fn default() -> Cursor {
        Cursor::load()
    }
}

fn nearest_images(size: u32, images: &[Image]) -> impl Iterator<Item = &Image> {
    // Follow the nominal size of the cursor to choose the nearest
    let nearest_image = images
        .iter()
        .min_by_key(|image| (size as i32 - image.size as i32).abs())
        .unwrap();

    images.iter().filter(move |image| {
        image.width == nearest_image.width && image.height == nearest_image.height
    })
}

fn frame(mut millis: u32, size: u32, images: &[Image]) -> Image {
    let total = nearest_images(size, images).fold(0, |acc, image| acc + image.delay);
    millis %= total;

    for img in nearest_images(size, images) {
        if millis < img.delay {
            return img.clone();
        }
        millis -= img.delay;
    }

    unreachable!()
}

#[derive(thiserror::Error, Debug)]
enum Error {
    #[error("Theme has no default cursor")]
    NoDefaultCursor,
    #[error("Error opening xcursor file: {0}")]
    File(#[from] std::io::Error),
    #[error("Failed to parse XCursor file")]
    Parse,
}

fn load_icon(theme: &CursorTheme) -> Result<Vec<Image>, Error> {
    let icon_path = theme.load_icon("default").ok_or(Error::NoDefaultCursor)?;
    let mut cursor_file = std::fs::File::open(&icon_path)?;
    let mut cursor_data = Vec::new();
    cursor_file.read_to_end(&mut cursor_data)?;
    parse_xcursor(&cursor_data).ok_or(Error::Parse)
}

render_elements! {
    pub CursorRenderElement<R> where R: ImportAll;
    Static=TextureRenderElement<<R as Renderer>::TextureId>,
    Surface=WaylandSurfaceRenderElement,
}

pub fn draw_surface_cursor<R: Renderer + ImportAll>(
    surface: &wl_surface::WlSurface,
    location: impl Into<Point<i32, Logical>>,
    scale: impl Into<Scale<f64>>,
) -> Vec<CursorRenderElement<R>>
where
{
    let mut position = location.into();
    let scale = scale.into();
    let h = with_states(&surface, |states| {
        states
            .data_map
            .get::<Mutex<CursorImageAttributes>>()
            .unwrap()
            .lock()
            .unwrap()
            .hotspot
    });
    position -= h;

    render_elements_from_surface_tree(surface, position.to_physical_precise_round(scale), scale)
}

pub fn draw_dnd_icon<R: Renderer + ImportAll>(
    surface: &wl_surface::WlSurface,
    location: impl Into<Point<i32, Logical>>,
    scale: impl Into<Scale<f64>>,
) -> Vec<CursorRenderElement<R>> {
    if get_role(&surface) != Some("dnd_icon") {
        slog_scope::warn!(
            "Trying to display as a dnd icon a surface that does not have the DndIcon role."
        );
    }
    let scale = scale.into();
    render_elements_from_surface_tree(
        surface,
        location.into().to_physical_precise_round(scale),
        scale,
    )
}

pub struct CursorState {
    pub cursor: Cursor,
    current_image: RefCell<Option<Image>>,
    image_cache: RefCell<HashMap<(TypeId, usize), Vec<(Image, Box<dyn Any + 'static>)>>>,
}

impl Default for CursorState {
    fn default() -> CursorState {
        CursorState {
            cursor: Cursor::default(),
            current_image: RefCell::new(None),
            image_cache: RefCell::new(HashMap::new()),
        }
    }
}

pub fn draw_cursor<R>(
    renderer: &mut R,
    seat: &Seat<State>,
    location: Point<f64, Logical>,
    scale: Scale<f64>,
    time: Time<Monotonic>,
    draw_default: bool,
) -> Vec<CursorRenderElement<R>>
where
    R: Renderer + ImportMem + ImportAll,
    <R as Renderer>::TextureId: Clone + 'static,
{
    // draw the cursor as relevant
    // reset the cursor if the surface is no longer alive
    let cursor_status = seat
        .user_data()
        .get::<RefCell<CursorImageStatus>>()
        .map(|cell| {
            let mut cursor_status = cell.borrow_mut();
            if let CursorImageStatus::Surface(ref surface) = *cursor_status {
                if !surface.alive() {
                    *cursor_status = CursorImageStatus::Default;
                }
            }
            cursor_status.clone()
        })
        .unwrap_or(CursorImageStatus::Default);

    if let CursorImageStatus::Surface(ref wl_surface) = cursor_status {
        return draw_surface_cursor(wl_surface, location.to_i32_round(), scale);
    } else if draw_default && CursorImageStatus::Default == cursor_status {
        let integer_scale = scale.x.max(scale.y).ceil() as u32;

        let seat_userdata = seat.user_data();
        seat_userdata.insert_if_missing(CursorState::default);
        let state = seat_userdata.get::<CursorState>().unwrap();
        let frame = state.cursor.get_image(
            integer_scale,
            Into::<Duration>::into(time).as_millis() as u32,
        );

        let mut cache = state.image_cache.borrow_mut();
        let pointer_images = cache
            .entry((TypeId::of::<TextureBuffer<R::TextureId>>(), renderer.id()))
            .or_default();

        let maybe_image = pointer_images
            .iter()
            .find_map(|(image, texture)| if image == &frame { Some(texture) } else { None })
            .and_then(|texture| texture.downcast_ref::<TextureBuffer<R::TextureId>>());
        let pointer_image = match maybe_image {
            Some(image) => image,
            None => {
                let texture = TextureBuffer::from_memory(
                    renderer,
                    &frame.pixels_rgba,
                    (frame.width as i32, frame.height as i32),
                    false,
                    integer_scale as i32,
                    Transform::Normal,
                    None,
                )
                .expect("Failed to import cursor bitmap");
                pointer_images.push((frame.clone(), Box::new(texture.clone())));
                pointer_images
                    .last()
                    .and_then(|(_, i)| i.downcast_ref::<TextureBuffer<R::TextureId>>())
                    .unwrap()
            }
        };

        let hotspot = Point::<i32, Logical>::from((frame.xhot as i32, frame.yhot as i32)).to_f64();
        *state.current_image.borrow_mut() = Some(frame);

        return vec![CursorRenderElement::Static(
            TextureRenderElement::from_texture_buffer(
                (location - hotspot).to_physical(scale),
                pointer_image,
                None,
                None,
                None,
            ),
        )];
    } else {
        Vec::new()
    }
}
