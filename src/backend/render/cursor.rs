// SPDX-License-Identifier: GPL-3.0-only

use crate::{utils::prelude::*, wayland::handlers::data_device::get_dnd_icon};
use smithay::{
    backend::renderer::{Frame, ImportAll, ImportMem, Renderer, Texture},
    desktop::space::{RenderElement, SpaceOutputTuple, SurfaceTree},
    reexports::wayland_server::protocol::wl_surface,
    utils::{IsAlive, Logical, Physical, Point, Rectangle, Scale, Size, Transform},
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
    SurfaceTree {
        surface,
        position,
        z_index: 100,
    }
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
        z_index: 100,
    }
}

pub struct PointerElement<T: Texture> {
    seat_id: usize,
    texture: T,
    position: Point<f64, Logical>,
    size: Size<i32, Logical>,
    new_frame: bool,
}

impl<T: Texture> PointerElement<T> {
    pub fn new(
        seat: &Seat<State>,
        texture: T,
        relative_pointer_pos: Point<f64, Logical>,
        new_frame: bool,
    ) -> PointerElement<T> {
        let size = texture.size().to_logical(1, Transform::Normal);
        PointerElement {
            seat_id: seat.id(),
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
        self.seat_id
    }

    fn location(&self, scale: impl Into<Scale<f64>>) -> Point<f64, Physical> {
        self.position.to_physical(scale)
    }

    fn geometry(&self, scale: impl Into<Scale<f64>>) -> Rectangle<i32, Physical> {
        Rectangle::from_loc_and_size(self.position, self.size.to_f64())
            .to_physical(scale)
            .to_i32_round()
    }

    fn accumulated_damage(
        &self,
        scale: impl Into<Scale<f64>>,
        _: Option<SpaceOutputTuple<'_, '_>>,
    ) -> Vec<Rectangle<i32, Physical>> {
        if self.new_frame {
            let scale = scale.into();
            vec![Rectangle::from_loc_and_size(
                self.position.to_physical(scale).to_i32_round(),
                self.size.to_physical_precise_round(scale),
            )]
        } else {
            vec![]
        }
    }

    fn opaque_regions(
        &self,
        _scale: impl Into<Scale<f64>>,
    ) -> Option<Vec<Rectangle<i32, Physical>>> {
        None
    }

    fn draw(
        &self,
        _renderer: &mut R,
        frame: &mut <R as Renderer>::Frame,
        scale: impl Into<Scale<f64>>,
        position: Point<f64, Physical>,
        damage: &[Rectangle<i32, Physical>],
        _log: &slog::Logger,
    ) -> Result<(), <R as Renderer>::Error> {
        let scale = scale.into();
        frame.render_texture_at(
            &self.texture,
            position.to_i32_round(),
            1,
            scale,
            Transform::Normal,
            &damage.iter().copied().map(|mut rect| {
                rect.loc -= self.position.to_physical(scale).to_i32_round();
                rect
            }).collect::<Vec<_>>(),
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
    seat: &Seat<State>,
    location: Point<f64, Logical>,
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
            if wl_surface.alive() {
                return Some(draw_dnd_icon(wl_surface, location.to_i32_round()).into());
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
                    if !surface.alive() {
                        *cursor_status = CursorImageStatus::Default;
                    }
                }
                cursor_status.clone()
            })
            .unwrap_or(CursorImageStatus::Default);

        if let CursorImageStatus::Image(wl_surface) = cursor_status {
            Some(draw_surface_cursor(wl_surface.clone(), location.to_i32_round()).into())
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
            let hotspot =
                Point::<i32, Logical>::from((frame.xhot as i32, frame.yhot as i32)).to_f64();
            *state.current_image.borrow_mut() = Some(frame);

            Some(PointerElement::new(seat, pointer_image.clone(), location - hotspot, new_frame).into())
        } else {
            None
        }
    }
}
