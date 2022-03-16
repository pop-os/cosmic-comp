// SPDX-License-Identifier: GPL-3.0-only

use crate::state::get_dnd_icon;
use smithay::{
    backend::renderer::{Frame, ImportAll, ImportMem, Renderer, Texture},
    desktop::space::{RenderElement, SpaceOutputTuple, SurfaceTree},
    reexports::wayland_server::protocol::wl_surface,
    utils::{Logical, Point, Rectangle, Size, Transform},
    wayland::{
        compositor::{get_role, with_states},
        seat::{CursorImageAttributes, CursorImageStatus, Seat},
    },
};
use std::{
    any::{Any, TypeId},
    cell::RefCell,
    collections::HashMap,
    io::Read,
    sync::Mutex,
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

pub fn draw_surface_cursor(
    surface: wl_surface::WlSurface,
    location: impl Into<Point<i32, Logical>>,
) -> SurfaceTree
where
{
    let mut position = location.into();
    let ret = with_states(&surface, |states| {
        Some(
            states
                .data_map
                .get::<Mutex<CursorImageAttributes>>()
                .unwrap()
                .lock()
                .unwrap()
                .hotspot,
        )
    })
    .unwrap_or(None);
    position -= match ret {
        Some(h) => h,
        None => {
            slog_scope::warn!(
                "Trying to display as a cursor a surface that does not have the CursorImage role."
            );
            (0, 0).into()
        }
    };
    SurfaceTree { surface, position }
}

pub fn draw_dnd_icon(
    surface: wl_surface::WlSurface,
    location: impl Into<Point<i32, Logical>>,
) -> SurfaceTree {
    if get_role(&surface) != Some("dnd_icon") {
        slog_scope::warn!(
            "Trying to display as a dnd icon a surface that does not have the DndIcon role."
        );
    }
    SurfaceTree {
        surface,
        position: location.into(),
    }
}

pub struct PointerElement<T: Texture> {
    texture: T,
    position: Point<i32, Logical>,
    size: Size<i32, Logical>,
    new_frame: bool,
}

impl<T: Texture> PointerElement<T> {
    pub fn new(
        texture: T,
        relative_pointer_pos: Point<i32, Logical>,
        new_frame: bool,
    ) -> PointerElement<T> {
        let size = texture.size().to_logical(1, Transform::Normal);
        PointerElement {
            texture,
            position: relative_pointer_pos,
            size,
            new_frame,
        }
    }
}

impl<R> RenderElement<R> for PointerElement<<R as Renderer>::TextureId>
where
    R: Renderer + ImportAll,
    <R as Renderer>::TextureId: 'static,
{
    fn id(&self) -> usize {
        0
    }

    fn geometry(&self) -> Rectangle<i32, Logical> {
        Rectangle::from_loc_and_size(self.position, self.size)
    }

    fn accumulated_damage(
        &self,
        _: Option<SpaceOutputTuple<'_, '_>>,
    ) -> Vec<Rectangle<i32, Logical>> {
        if self.new_frame {
            vec![Rectangle::from_loc_and_size((0, 0), self.size)]
        } else {
            vec![]
        }
    }

    fn draw(
        &self,
        _renderer: &mut R,
        frame: &mut <R as Renderer>::Frame,
        scale: f64,
        position: Point<i32, Logical>,
        damage: &[Rectangle<i32, Logical>],
        _log: &slog::Logger,
    ) -> Result<(), <R as Renderer>::Error> {
        frame.render_texture_at(
            &self.texture,
            position.to_f64().to_physical(scale as f64).to_i32_round(),
            1,
            scale as f64,
            Transform::Normal,
            &*damage
                .iter()
                .map(|rect| rect.to_buffer(1, Transform::Normal, &self.size))
                .collect::<Vec<_>>(),
            1.0,
        )?;
        Ok(())
    }
}

struct CursorState {
    cursor: Cursor,
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

pub fn draw_cursor<R, I>(
    renderer: &mut R,
    seat: &Seat,
    location: Point<i32, Logical>,
    start_time: &std::time::Instant,
    draw_default: bool,
) -> Option<I>
where
    I: From<SurfaceTree> + From<PointerElement<<R as Renderer>::TextureId>>,
    R: Renderer + ImportAll + ImportMem,
    <R as Renderer>::TextureId: Clone + 'static,
{
    // draw the dnd icon if applicable
    {
        if let Some(wl_surface) = get_dnd_icon(seat) {
            if wl_surface.as_ref().is_alive() {
                return Some(draw_dnd_icon(wl_surface, location).into());
            }
        }
    }

    // draw the cursor as relevant
    {
        // reset the cursor if the surface is no longer alive
        let cursor_status = seat
            .user_data()
            .get::<RefCell<CursorImageStatus>>()
            .map(|cell| {
                let mut cursor_status = cell.borrow_mut();
                if let CursorImageStatus::Image(ref surface) = *cursor_status {
                    if !surface.as_ref().is_alive() {
                        *cursor_status = CursorImageStatus::Default;
                    }
                }
                cursor_status.clone()
            })
            .unwrap_or(CursorImageStatus::Default);

        if let CursorImageStatus::Image(wl_surface) = cursor_status {
            Some(draw_surface_cursor(wl_surface.clone(), location).into())
        } else if draw_default {
            let seat_userdata = seat.user_data();
            seat_userdata.insert_if_missing(CursorState::default);
            let state = seat_userdata.get::<CursorState>().unwrap();
            let frame = state
                .cursor
                .get_image(1, start_time.elapsed().as_millis() as u32);
            let new_frame = state.current_image.borrow().as_ref() != Some(&frame);

            let mut cache = state.image_cache.borrow_mut();
            let pointer_images = cache
                .entry((TypeId::of::<<R as Renderer>::TextureId>(), renderer.id()))
                .or_default();
            let pointer_image = pointer_images
                .iter()
                .find_map(|(image, texture)| if image == &frame { Some(texture) } else { None })
                .and_then(|texture| {
                    texture
                        .downcast_ref::<<R as Renderer>::TextureId>()
                        .cloned()
                })
                .unwrap_or_else(|| {
                    let texture = renderer
                        .import_memory(
                            &frame.pixels_rgba,
                            (frame.width as i32, frame.height as i32).into(),
                            false,
                        )
                        .expect("Failed to import cursor bitmap");
                    pointer_images.push((frame.clone(), Box::new(texture.clone())));
                    texture
                });
            let hotspot = Point::<i32, Logical>::from((frame.xhot as i32, frame.yhot as i32));
            *state.current_image.borrow_mut() = Some(frame);

            Some(PointerElement::new(pointer_image.clone(), location - hotspot, new_frame).into())
        } else {
            None
        }
    }
}
