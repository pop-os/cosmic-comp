// SPDX-License-Identifier: GPL-3.0-only

use crate::utils::prelude::*;
use smithay::{
    backend::{
        allocator::Fourcc,
        renderer::{
            element::{
                memory::{MemoryRenderBuffer, MemoryRenderBufferRenderElement},
                surface::{render_elements_from_surface_tree, WaylandSurfaceRenderElement},
                Kind,
            },
            ImportAll, ImportMem, Renderer,
        },
    },
    input::{
        pointer::{CursorImageAttributes, CursorImageStatus},
        Seat,
    },
    reexports::wayland_server::protocol::wl_surface,
    render_elements,
    utils::{
        Buffer as BufferCoords, IsAlive, Logical, Monotonic, Point, Scale, Size, Time, Transform,
    },
    wayland::compositor::{get_role, with_states},
};
use std::{cell::RefCell, collections::HashMap, io::Read, sync::Mutex, time::Duration};
use tracing::warn;
use xcursor::{
    parser::{parse_xcursor, Image},
    CursorTheme,
};

static FALLBACK_CURSOR_DATA: &[u8] = include_bytes!("../../../resources/cursor.rgba");

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CursorShape {
    Default,
    ColResize,
    RowResize,
    Grab,
    EastResize,
    WestResize,
    NorthResize,
    SouthResize,
    NorthEastResize,
    NorthWestResize,
    SouthEastResize,
    SouthWestResize,
}

impl ToString for CursorShape {
    fn to_string(&self) -> String {
        match self {
            CursorShape::Default => "default",
            CursorShape::ColResize => "col-resize",
            CursorShape::RowResize => "row-resize",
            CursorShape::Grab => "grabbing",
            CursorShape::EastResize => "e-resize",
            CursorShape::WestResize => "w-resize",
            CursorShape::NorthResize => "n-resize",
            CursorShape::SouthResize => "s-resize",
            CursorShape::NorthEastResize => "ne-resize",
            CursorShape::NorthWestResize => "nw-resize",
            CursorShape::SouthEastResize => "se-resize",
            CursorShape::SouthWestResize => "sw-resize",
        }
        .to_string()
    }
}

#[derive(Debug, Clone)]
pub struct Cursor {
    icons: Vec<Image>,
    size: u32,
}

impl Cursor {
    pub fn load(theme: &CursorTheme, shape: CursorShape, size: u32) -> Cursor {
        let icons = load_icon(&theme, shape)
            .map_err(|err| warn!(?err, "Unable to load xcursor, using fallback cursor"))
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

fn load_icon(theme: &CursorTheme, shape: CursorShape) -> Result<Vec<Image>, Error> {
    let icon_path = theme
        .load_icon(&shape.to_string())
        .ok_or(Error::NoDefaultCursor)?;
    let mut cursor_file = std::fs::File::open(&icon_path)?;
    let mut cursor_data = Vec::new();
    cursor_file.read_to_end(&mut cursor_data)?;
    parse_xcursor(&cursor_data).ok_or(Error::Parse)
}

render_elements! {
    pub CursorRenderElement<R> where R: ImportAll + ImportMem;
    Static=MemoryRenderBufferRenderElement<R>,
    Surface=WaylandSurfaceRenderElement<R>,
}

pub fn draw_surface_cursor<R>(
    renderer: &mut R,
    surface: &wl_surface::WlSurface,
    location: impl Into<Point<i32, Logical>>,
    scale: impl Into<Scale<f64>>,
) -> Vec<(CursorRenderElement<R>, Point<i32, BufferCoords>)>
where
    R: Renderer + ImportAll,
    <R as Renderer>::TextureId: Clone + 'static,
{
    let position = location.into();
    let scale = scale.into();
    let h = with_states(&surface, |states| {
        states
            .data_map
            .get::<Mutex<CursorImageAttributes>>()
            .unwrap()
            .lock()
            .unwrap()
            .hotspot
            .to_buffer(
                1,
                Transform::Normal,
                &Size::from((1, 1)), /* Size doesn't matter for Transform::Normal */
            )
    });

    render_elements_from_surface_tree(
        renderer,
        surface,
        position.to_physical_precise_round(scale),
        scale,
        1.0,
        Kind::Cursor,
    )
    .into_iter()
    .map(|elem| (elem, h))
    .collect()
}

#[profiling::function]
pub fn draw_dnd_icon<R>(
    renderer: &mut R,
    surface: &wl_surface::WlSurface,
    location: impl Into<Point<i32, Logical>>,
    scale: impl Into<Scale<f64>>,
) -> Vec<WaylandSurfaceRenderElement<R>>
where
    R: Renderer + ImportAll,
    <R as Renderer>::TextureId: Clone + 'static,
{
    if get_role(&surface) != Some("dnd_icon") {
        warn!(
            ?surface,
            "Trying to display as a dnd icon a surface that does not have the DndIcon role."
        );
    }
    let scale = scale.into();
    render_elements_from_surface_tree(
        renderer,
        surface,
        location.into().to_physical_precise_round(scale),
        scale,
        1.0,
        Kind::Unspecified,
    )
}

pub struct CursorState {
    current_cursor: RefCell<CursorShape>,
    pub cursors: HashMap<CursorShape, Cursor>,
    current_image: RefCell<Option<Image>>,
    image_cache: RefCell<Vec<(Image, MemoryRenderBuffer)>>,
}

impl CursorState {
    pub fn set_shape(&self, shape: CursorShape) {
        *self.current_cursor.borrow_mut() = shape;
    }
}

pub fn load_cursor_theme() -> (CursorTheme, u32) {
    let name = std::env::var("XCURSOR_THEME")
        .ok()
        .unwrap_or_else(|| "default".into());
    let size = std::env::var("XCURSOR_SIZE")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(24);
    (CursorTheme::load(&name), size)
}

impl Default for CursorState {
    fn default() -> CursorState {
        let (theme, size) = load_cursor_theme();
        CursorState {
            current_cursor: RefCell::new(CursorShape::Default),
            cursors: {
                let mut map = HashMap::new();
                map.insert(
                    CursorShape::Default,
                    Cursor::load(&theme, CursorShape::Default, size),
                );
                map.insert(
                    CursorShape::ColResize,
                    Cursor::load(&theme, CursorShape::ColResize, size),
                );
                map.insert(
                    CursorShape::RowResize,
                    Cursor::load(&theme, CursorShape::RowResize, size),
                );
                map.insert(
                    CursorShape::Grab,
                    Cursor::load(&theme, CursorShape::Grab, size),
                );
                map.insert(
                    CursorShape::NorthResize,
                    Cursor::load(&theme, CursorShape::NorthResize, size),
                );
                map.insert(
                    CursorShape::SouthResize,
                    Cursor::load(&theme, CursorShape::SouthResize, size),
                );
                map.insert(
                    CursorShape::EastResize,
                    Cursor::load(&theme, CursorShape::EastResize, size),
                );
                map.insert(
                    CursorShape::WestResize,
                    Cursor::load(&theme, CursorShape::WestResize, size),
                );
                map.insert(
                    CursorShape::NorthEastResize,
                    Cursor::load(&theme, CursorShape::NorthEastResize, size),
                );
                map.insert(
                    CursorShape::SouthEastResize,
                    Cursor::load(&theme, CursorShape::SouthEastResize, size),
                );
                map.insert(
                    CursorShape::NorthWestResize,
                    Cursor::load(&theme, CursorShape::NorthWestResize, size),
                );
                map.insert(
                    CursorShape::SouthWestResize,
                    Cursor::load(&theme, CursorShape::SouthWestResize, size),
                );
                map
            },
            current_image: RefCell::new(None),
            image_cache: RefCell::new(Vec::new()),
        }
    }
}

#[profiling::function]
pub fn draw_cursor<R>(
    renderer: &mut R,
    seat: &Seat<State>,
    location: Point<f64, Logical>,
    scale: Scale<f64>,
    time: Time<Monotonic>,
    draw_default: bool,
) -> Vec<(CursorRenderElement<R>, Point<i32, BufferCoords>)>
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
                    *cursor_status = CursorImageStatus::default_named();
                }
            }
            cursor_status.clone()
        })
        .unwrap_or(CursorImageStatus::default_named());

    if let CursorImageStatus::Surface(ref wl_surface) = cursor_status {
        return draw_surface_cursor(renderer, wl_surface, location.to_i32_round(), scale);
    // TODO: Handle other named cursors
    } else if draw_default && CursorImageStatus::default_named() == cursor_status {
        let integer_scale = scale.x.max(scale.y).ceil() as u32;

        let seat_userdata = seat.user_data();
        let state = seat_userdata.get::<CursorState>().unwrap();
        let frame = state
            .cursors
            .get(&*state.current_cursor.borrow())
            .unwrap()
            .get_image(
                integer_scale,
                Into::<Duration>::into(time).as_millis() as u32,
            );

        let mut pointer_images = state.image_cache.borrow_mut();

        let maybe_image =
            pointer_images
                .iter()
                .find_map(|(image, texture)| if image == &frame { Some(texture) } else { None });
        let pointer_image = match maybe_image {
            Some(image) => image,
            None => {
                let buffer = MemoryRenderBuffer::from_slice(
                    &frame.pixels_rgba,
                    Fourcc::Argb8888,
                    (frame.width as i32, frame.height as i32),
                    integer_scale as i32,
                    Transform::Normal,
                    None,
                );
                pointer_images.push((frame.clone(), buffer));
                pointer_images.last().map(|(_, i)| i).unwrap()
            }
        };

        let hotspot = Point::<i32, BufferCoords>::from((frame.xhot as i32, frame.yhot as i32));
        *state.current_image.borrow_mut() = Some(frame);

        return vec![(
            CursorRenderElement::Static(
                MemoryRenderBufferRenderElement::from_buffer(
                    renderer,
                    location.to_physical(scale),
                    pointer_image,
                    None,
                    None,
                    None,
                    Kind::Cursor,
                )
                .expect("Failed to import cursor bitmap"),
            ),
            hotspot,
        )];
    } else {
        Vec::new()
    }
}
