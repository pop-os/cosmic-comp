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
    collections::{HashMap, VecDeque},
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
    pub CursorRenderElement<R> where R: ImportAll + ImportMem + AsGlowRenderer, R::TextureId: Send;
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

    // shake-to-find
    shake_path: VecDeque<PathSample>,
    shake_path_position: Point<f64, Logical>,
    magnify_until: Option<Instant>,
    magnify_target: f32,
    magnification: f32,
    anim_from: f32,
    anim_start: Option<Instant>,
}

/// One sampled pointer position on the recent motion path.
#[derive(Clone, Copy)]
struct PathSample {
    position: Point<f64, Logical>,
    time: Instant,
}

/// How far back the motion path is considered when looking for a shake.
const SHAKE_INTERVAL: Duration = Duration::from_millis(1000);
/// Path-length / bounding-box-diagonal ratio required to count as a shake.
const SHAKE_SENSITIVITY: f64 = 4.0;
/// Minimum bounding-box diagonal (logical px) before a shake is considered.
const SHAKE_DIAGONAL_MIN: f64 = 100.0;
/// Two deltas count as "the same direction" if both lie within this tolerance.
const SHAKE_SAME_SIGN_TOLERANCE: f64 = 1.0;
/// Keep the cursor enlarged for this long after the last detected shake.
const SHAKE_HOLD: Duration = Duration::from_millis(2000);
/// Extra magnification added by each shake, growing from the normal cursor size.
const OVER_MAGNIFICATION: f32 = 1.0;
/// Duration of the grow/shrink animation.
const MAGNIFICATION_ANIM: Duration = Duration::from_millis(200);

/// small movement is ignored and direction stays the same
fn same_direction(a: f64, b: f64) -> bool {
    (a >= -SHAKE_SAME_SIGN_TOLERANCE && b >= -SHAKE_SAME_SIGN_TOLERANCE)
        || (a <= SHAKE_SAME_SIGN_TOLERANCE && b <= SHAKE_SAME_SIGN_TOLERANCE)
}

/// `InOutCubic` easing
fn ease_in_out_cubic(t: f32) -> f32 {
    if t < 0.5 {
        4.0 * t * t * t
    } else {
        let f = -2.0 * t + 2.0;
        1.0 - f * f * f / 2.0
    }
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

    /// Feed one relative-motion event into the shake detector.
    pub fn detect_shake(&mut self, delta: Point<f64, Logical>, now: Instant) {
        // Drop samples that have aged out of the time window.
        while let Some(oldest) = self.shake_path.front() {
            if now.duration_since(oldest.time) >= SHAKE_INTERVAL {
                self.shake_path.pop_front();
            } else {
                break;
            }
        }

        if delta.x != 0.0 || delta.y != 0.0 {
            self.shake_path_position += delta;
            let sample = PathSample {
                position: self.shake_path_position,
                time: now,
            };

            if self.shake_path.len() >= 2 {
                let last = self.shake_path[self.shake_path.len() - 1].position;
                let prev = self.shake_path[self.shake_path.len() - 2].position;
                let last_delta = last - prev;
                if same_direction(last_delta.x, delta.x) && same_direction(last_delta.y, delta.y) {
                    *self.shake_path.back_mut().unwrap() = sample;
                } else {
                    self.shake_path.push_back(sample);
                }
            } else {
                self.shake_path.push_back(sample);
            }
        }

        if self.shake_path.len() < 2 {
            return;
        }

        let first = self.shake_path[0].position;
        let (mut left, mut top, mut right, mut bottom) = (first.x, first.y, first.x, first.y);
        let mut path_length = 0.0;
        for i in 1..self.shake_path.len() {
            let p = self.shake_path[i].position;
            left = left.min(p.x);
            top = top.min(p.y);
            right = right.max(p.x);
            bottom = bottom.max(p.y);

            let step = p - self.shake_path[i - 1].position;
            path_length += step.x.hypot(step.y);
        }

        let diagonal = (right - left).hypot(bottom - top);
        if diagonal < SHAKE_DIAGONAL_MIN {
            return;
        }

        // Path noticeably longer than the diagonal => a shake gesture.
        if path_length / diagonal > SHAKE_SENSITIVITY {
            self.grow(now);
            self.shake_path.clear();
        }
    }

    /// grow the cursor by one more increment (unbounded)
    fn grow(&mut self, now: Instant) {
        self.animate_to(self.magnify_target + OVER_MAGNIFICATION, now);
        self.magnify_until = Some(now + SHAKE_HOLD);
    }

    /// Start a 200ms `InOutCubic` tween from the current size to `target`.
    fn animate_to(&mut self, target: f32, now: Instant) {
        if (target - self.magnify_target).abs() < f32::EPSILON {
            return;
        }
        self.anim_from = self.magnification;
        self.anim_start = Some(now);
        self.magnify_target = target;
    }

    /// Advance the magnification animation and return the current factor.
    pub fn animated_magnification(&mut self, now: Instant) -> f32 {
        // Begin shrinking back once the hold window elapses.
        if let Some(until) = self.magnify_until
            && now >= until
        {
            self.magnify_until = None;
            self.animate_to(1.0, now);
        }

        self.magnification = match self.anim_start {
            Some(start) => {
                let t = (now.duration_since(start).as_secs_f32()
                    / MAGNIFICATION_ANIM.as_secs_f32())
                .clamp(0.0, 1.0);
                if t >= 1.0 {
                    self.anim_start = None;
                }
                self.anim_from + (self.magnify_target - self.anim_from) * ease_in_out_cubic(t)
            }
            None => self.magnify_target,
        };
        self.magnification
    }

    /// Whether the cursor is currently magnified or pending; drives continued redraws.
    pub fn is_magnifying(&self) -> bool {
        self.magnify_until.is_some()
            || self.anim_start.is_some()
            || self.magnification > 1.001
            || self.magnify_target > 1.001
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

            shake_path: VecDeque::new(),
            shake_path_position: Point::from((0.0, 0.0)),
            magnify_until: None,
            magnify_target: 1.0,
            magnification: 1.0,
            anim_from: 1.0,
            anim_start: None,
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
