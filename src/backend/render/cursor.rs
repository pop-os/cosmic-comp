// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render::{
        element::AsGlowRenderer,
        wayland::{SurfaceRenderElement, push_render_elements_from_surface_tree},
    },
    utils::prelude::*,
    wayland::handlers::compositor::FRAME_TIME_FILTER,
};
use smithay::{
    backend::{
        allocator::Fourcc,
        renderer::{
            ImportAll, ImportMem, Renderer,
            element::{
                Kind,
                memory::{MemoryRenderBuffer, MemoryRenderBufferRenderElement},
            },
        },
    },
    desktop::utils::bbox_from_surface_tree,
    input::{
        Seat,
        pointer::{CursorIcon, CursorImageAttributes, CursorImageStatus},
    },
    reexports::{
        calloop::{
            RegistrationToken,
            timer::{TimeoutAction, Timer},
        },
        wayland_server::protocol::wl_surface,
    },
    render_elements,
    utils::{
        Buffer as BufferCoords, Logical, Monotonic, Physical, Point, Scale, Size, Time, Transform,
    },
    wayland::compositor::{get_role, with_states},
};
use std::{
    collections::HashMap,
    io::Read,
    sync::Mutex,
    time::{Duration, Instant},
};
use tracing::warn;
use xcursor::{
    CursorTheme,
    parser::{Image, parse_xcursor},
};

static FALLBACK_CURSOR_DATA: &[u8] = include_bytes!("../../../resources/cursor.rgba");

#[derive(Debug, Clone)]
pub struct Cursor {
    icons: Vec<Image>,
    size: u32,
}

impl Cursor {
    pub fn load(theme: &CursorTheme, shape: CursorIcon, size: u32) -> Cursor {
        let icons = load_icon(theme, shape)
            .map_err(|err| warn!(?err, "Unable to load xcursor, using fallback cursor"))
            .or_else(|_| load_icon(theme, CursorIcon::Default))
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
        .min_by_key(|image| u32::abs_diff(size, image.size))
        .unwrap();

    images.iter().filter(move |image| {
        image.width == nearest_image.width && image.height == nearest_image.height
    })
}

fn frame(mut millis: u32, size: u32, images: &[Image]) -> Image {
    let total = nearest_images(size, images).fold(0, |acc, image| acc + image.delay);

    if total == 0 {
        millis = 0;
    } else {
        millis %= total;
    }

    for img in nearest_images(size, images) {
        if millis <= img.delay {
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
}

fn cursor_aliases(name: &str) -> &[&str] {
    match name {
        "default" => &["default", "left_ptr", "arrow"],
        "pointer" => &["pointer", "hand2", "hand"],
        "text" => &["text", "xterm"],
        "wait" => &["wait", "watch"],
        "progress" => &["progress", "left_ptr_watch"],

        "ew-resize" => &["ew-resize", "h_double_arrow", "sb_h_double_arrow"],
        "ns-resize" => &["ns-resize", "v_double_arrow", "sb_v_double_arrow"],
        "nw-resize" => &["nw-resize", "top_left_corner"],
        "ne-resize" => &["ne-resize", "top_right_corner"],
        "sw-resize" => &["sw-resize", "bottom_left_corner"],
        "se-resize" => &["se-resize", "bottom_right_corner"],

        "w-resize" => &["w-resize", "left_side"],
        "e-resize" => &["e-resize", "right_side"],
        "n-resize" => &["n-resize", "top_side"],
        "s-resize" => &["s-resize", "bottom_side"],

        "move" => &["move", "fleur"],
        "not-allowed" => &["not-allowed", "crossed_circle"],
        "crosshair" => &["crosshair", "cross"],
        "help" => &["help", "question_arrow", "left_ptr_help"],

        _ => &[],
    }
}

fn load_icon(theme: &CursorTheme, shape: CursorIcon) -> Result<Vec<Image>, Error> {
    let shape_name = shape.to_string();

    for name in cursor_aliases(&shape_name)
        .iter()
        .copied()
        .chain(std::iter::once(shape_name.as_str()))
    {
        if let Some(icon_path) = theme.load_icon(name) {
            let mut cursor_file = std::fs::File::open(&icon_path)?;
            let mut cursor_data = Vec::new();
            cursor_file.read_to_end(&mut cursor_data)?;

            if let Some(images) = parse_xcursor(&cursor_data) {
                return Ok(images);
            }
        }
    }

    Err(Error::NoDefaultCursor)
}

render_elements! {
    pub CursorRenderElement<R> where R: ImportAll + ImportMem + AsGlowRenderer;
    Static=MemoryRenderBufferRenderElement<R>,
    Surface=SurfaceRenderElement<R>,
}

pub fn draw_surface_cursor<R>(
    renderer: &mut R,
    surface: &wl_surface::WlSurface,
    location: Point<f64, Logical>,
    scale: impl Into<Scale<f64>>,
    blur_strength: usize,
    push: &mut dyn FnMut(CursorRenderElement<R>, Point<i32, Physical>),
) where
    R: Renderer + ImportAll + AsGlowRenderer,
    R::TextureId: Clone + 'static,
{
    let scale = scale.into();
    let h = with_states(surface, |states| {
        states
            .data_map
            .get::<Mutex<CursorImageAttributes>>()
            .unwrap()
            .lock()
            .unwrap()
            .hotspot
            .to_physical_precise_round(scale)
    });

    push_render_elements_from_surface_tree(
        renderer,
        surface,
        location.to_physical(scale).to_i32_round(),
        bbox_from_surface_tree(surface, location.to_i32_round()).to_f64(),
        scale,
        1.0,
        false,
        [0; 4],
        None,
        blur_strength,
        Kind::Cursor,
        &mut |elem| push(elem.into(), h),
        None,
    );
}

#[profiling::function]
pub fn draw_dnd_icon<R>(
    renderer: &mut R,
    surface: &wl_surface::WlSurface,
    location: Point<f64, Logical>,
    scale: impl Into<Scale<f64>>,
    blur_strength: usize,
    push: &mut dyn FnMut(SurfaceRenderElement<R>),
) where
    R: Renderer + ImportAll + AsGlowRenderer,
    R::TextureId: Clone + 'static,
{
    if get_role(surface) != Some("dnd_icon") {
        warn!(
            ?surface,
            "Trying to display as a dnd icon a surface that does not have the DndIcon role."
        );
    }
    let scale = scale.into();
    push_render_elements_from_surface_tree(
        renderer,
        surface,
        location.to_physical(scale).to_i32_round(),
        bbox_from_surface_tree(surface, location.to_i32_round()).to_f64(),
        scale,
        1.0,
        false,
        [0; 4],
        None,
        blur_strength,
        FRAME_TIME_FILTER,
        push,
        None,
    );
}

pub type CursorState = Mutex<CursorStateInner>;
pub struct CursorStateInner {
    current_cursor: Option<CursorIcon>,

    cursor_theme: CursorTheme,
    cursor_size: u32,

    cursors: HashMap<CursorIcon, Cursor>,
    current_image: Option<Image>,
    image_cache: Vec<(Image, MemoryRenderBuffer)>,

    hidden: bool,
    idle_timer: Option<RegistrationToken>,
    last_armed: Option<Instant>,
}

impl CursorStateInner {
    pub fn set_shape(&mut self, shape: CursorIcon) {
        self.current_cursor = Some(shape);
    }

    pub fn unset_shape(&mut self) {
        self.current_cursor = None;
    }

    pub fn get_named_cursor(&mut self, shape: CursorIcon) -> &Cursor {
        self.cursors
            .entry(shape)
            .or_insert_with(|| Cursor::load(&self.cursor_theme, shape, self.cursor_size))
    }

    pub fn size(&self) -> u32 {
        self.cursor_size
    }
}

pub fn load_cursor_env() -> (String, u32) {
    let name = std::env::var("XCURSOR_THEME")
        .ok()
        .unwrap_or_else(|| "default".into());
    let size = std::env::var("XCURSOR_SIZE")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(24);
    (name, size)
}

pub fn load_cursor_theme() -> (CursorTheme, u32) {
    let (name, size) = load_cursor_env();
    (CursorTheme::load(&name), size)
}

impl Default for CursorStateInner {
    fn default() -> CursorStateInner {
        let (theme, size) = load_cursor_theme();
        CursorStateInner {
            current_cursor: None,

            cursor_size: size,
            cursor_theme: theme,

            cursors: HashMap::new(),
            current_image: None,
            image_cache: Vec::new(),

            hidden: false,
            idle_timer: None,
            last_armed: None,
        }
    }
}

#[profiling::function]
pub fn draw_cursor<R>(
    renderer: &mut R,
    seat: &Seat<State>,
    location: Point<f64, Logical>,
    scale: Scale<f64>,
    buffer_scale: f64,
    time: Time<Monotonic>,
    blur_strength: usize,
    draw_default: bool,
    push: &mut dyn FnMut(CursorRenderElement<R>, Point<i32, Physical>),
) where
    R: Renderer + ImportMem + ImportAll + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
{
    // draw the cursor as relevant
    let cursor_status = seat.cursor_image_status();

    let seat_userdata = seat.user_data();
    let mut state_ref = seat_userdata.get::<CursorState>().unwrap().lock().unwrap();
    let state = &mut *state_ref;

    if state.hidden {
        return;
    }

    let named_cursor = state.current_cursor.or(match cursor_status {
        CursorImageStatus::Named(named_cursor) => Some(named_cursor),
        _ => None,
    });
    if let Some(current_cursor) = named_cursor {
        if !draw_default && current_cursor == CursorIcon::Default {
            return;
        }

        let integer_scale = (scale.x.max(scale.y) * buffer_scale).ceil() as u32;
        let frame = state
            .get_named_cursor(current_cursor)
            .get_image(integer_scale, time.as_millis());
        let actual_scale = (frame.size / state.size()).max(1);

        let pointer_images = &mut state.image_cache;
        let maybe_image = pointer_images
            .iter()
            .find_map(|(image, texture)| if image == &frame { Some(texture) } else { None });
        let pointer_image = match maybe_image {
            Some(image) => image,
            None => {
                let buffer = MemoryRenderBuffer::from_slice(
                    &frame.pixels_rgba,
                    Fourcc::Argb8888,
                    (frame.width as i32, frame.height as i32),
                    actual_scale as i32,
                    Transform::Normal,
                    None,
                );
                pointer_images.push((frame.clone(), buffer));
                pointer_images.last().map(|(_, i)| i).unwrap()
            }
        };

        let hotspot = Point::<i32, BufferCoords>::from((frame.xhot as i32, frame.yhot as i32))
            .to_logical(
                actual_scale as i32,
                Transform::Normal,
                &Size::from((frame.width as i32, frame.height as i32)),
            );
        state.current_image = Some(frame);

        push(
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
            hotspot.to_physical_precise_round(scale),
        );
    } else if let CursorImageStatus::Surface(ref wl_surface) = cursor_status {
        draw_surface_cursor(renderer, wl_surface, location, scale, blur_strength, push);
    }
}

const ACTIVITY_THROTTLE: Duration = Duration::from_millis(100);

/// Reveal the cursor and (re)arm the idle-hide timer; returns true if it was previously hidden
pub fn notify_cursor_activity(state: &State, seat: &Seat<State>) -> bool {
    let timeout = state.common.config.cosmic_conf.cursor_hide_timeout;
    let loop_handle = &state.common.event_loop_handle;
    let cursor_state = seat.user_data().get::<CursorState>().unwrap();
    let now = Instant::now();

    let (was_hidden, old_token) = {
        let mut inner = cursor_state.lock().unwrap();
        let was_hidden = inner.hidden;
        inner.hidden = false;

        let throttled = timeout.is_some()
            && !was_hidden
            && inner.idle_timer.is_some()
            && inner
                .last_armed
                .is_some_and(|t| now.duration_since(t) < ACTIVITY_THROTTLE);
        if throttled {
            return was_hidden;
        }

        let old_token = inner.idle_timer.take();
        inner.last_armed = None;
        (was_hidden, old_token)
    };

    if let Some(token) = old_token {
        loop_handle.remove(token);
    }

    if let Some(secs) = timeout {
        let timer = Timer::from_duration(Duration::from_secs(secs as u64));
        let seat = seat.clone();
        if let Ok(token) = loop_handle.insert_source(timer, move |_, _, state| {
            hide_cursor(state, &seat);
            TimeoutAction::Drop
        }) {
            let mut inner = cursor_state.lock().unwrap();
            inner.idle_timer = Some(token);
            inner.last_armed = Some(now);
        }
    }

    was_hidden
}

fn hide_cursor(state: &mut State, seat: &Seat<State>) {
    if let Some(ptr) = seat.get_pointer()
        && ptr.is_grabbed()
    {
        return;
    }
    let cursor_state = seat.user_data().get::<CursorState>().unwrap();
    {
        let mut inner = cursor_state.lock().unwrap();
        inner.hidden = true;
        inner.idle_timer = None;
        inner.last_armed = None;
    }
    let outputs: Vec<_> = state.common.shell.read().outputs().cloned().collect();
    for output in outputs {
        state.backend.schedule_render(&output);
    }
}
