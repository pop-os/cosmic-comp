use crate::shell::element::surface::PopupShadow;
use crate::{
    backend::render::{
        IndicatorShader, Key, Usage, cursor::CursorState, element::AsGlowRenderer,
        shadow::ShadowShader, wayland::SurfaceRenderElement,
    },
    hooks::{Decorations, HOOKS},
    shell::{
        element::{CosmicMappedKey, CosmicMappedKeyInner},
        focus::target::PointerFocusTarget,
        grabs::{ReleaseMode, ResizeEdge},
    },
    state::State,
    utils::{
        iced::{IcedElement, IcedRenderElement, Program},
        prelude::*,
    },
    wayland::handlers::surface_embed::{get_embed_render_info, is_surface_embedded},
};
use calloop::LoopHandle;
use cosmic_comp_config::AppearanceConfig;
use iced_core::{Color, mouse::Interaction as MouseInteraction};
use iced_runtime::Task;
use smithay::{
    backend::{
        drm::DrmNode,
        input::KeyState,
        renderer::{
            ImportAll, ImportMem, Renderer,
            element::{Element, Id as RendererId, Kind, RenderElement, UnderlyingStorage},
            gles::element::PixelShaderElement,
            glow::GlowRenderer,
            utils::{CommitCounter, DamageSet, OpaqueRegions},
        },
    },
    desktop::{WindowSurfaceType, space::SpaceElement},
    input::{
        Seat,
        keyboard::{KeyboardTarget, KeysymHandle, ModifiersState},
        pointer::{
            AxisFrame, ButtonEvent, CursorIcon, CursorImageStatus, GestureHoldBeginEvent,
            GestureHoldEndEvent, GesturePinchBeginEvent, GesturePinchEndEvent,
            GesturePinchUpdateEvent, GestureSwipeBeginEvent, GestureSwipeEndEvent,
            GestureSwipeUpdateEvent, MotionEvent, PointerTarget, RelativeMotionEvent,
        },
        touch::{
            DownEvent, FrameMarker, MotionEvent as TouchMotionEvent, OrientationEvent, ShapeEvent,
            TouchTarget, UpEvent,
        },
    },
    output::Output,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    utils::{
        Buffer, IsAlive, Logical, Physical, Point, Rectangle, Scale, Serial, Size, Transform,
        user_data::UserDataMap,
    },
    wayland::{pointer_constraints::with_pointer_constraint, seat::WaylandFocus},
};
use std::{
    borrow::Cow,
    fmt,
    hash::Hash,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, AtomicU8, Ordering},
    },
};
use wayland_backend::server::ObjectId;

use super::CosmicSurface;

pub const RESIZE_BORDER: i32 = 10;

/// Blur strength handed to upstream's background-effect blur for window (and popup) surfaces.
///
/// MERGE: upstream derives this from cosmic's `frosted_windows`/`frosted` theme settings
/// (`0` = off, `1` = light, `2` = strong). `CompTheme` has no such toggle, so the fork uses a
/// fixed strength — surfaces that never publish a blur region are unaffected, since
/// `BlurElement::from_surface` returns `None` for them.
pub const WINDOW_BLUR_STRENGTH: usize = 2;

/// Tracks which CosmicWindow currently has pointer_over_window=true.
/// Updated by focus_under() on each pointer motion event.
/// This provides geometry-based hover detection that cannot be fooled by fast pointer movement.
static POINTER_HOVERED_WINDOW: std::sync::LazyLock<Mutex<Option<CosmicWindow>>> =
    std::sync::LazyLock::new(|| Mutex::new(None));

/// Flag indicating whether focus_under() was called and hit a window during this motion cycle.
static POINTER_HOVER_UPDATED: std::sync::LazyLock<AtomicBool> =
    std::sync::LazyLock::new(|| AtomicBool::new(false));

/// Whether hover tracking is active for the current motion cycle.
/// When false, focus_under() skips all hover state updates.
static HOVER_TRACKING_ACTIVE: std::sync::LazyLock<AtomicBool> =
    std::sync::LazyLock::new(|| AtomicBool::new(false));

/// Called from input handlers BEFORE surface_under() to start the hover cycle.
/// Resets the "updated" flag. After surface_under completes, call finalize_pointer_hover().
pub fn begin_pointer_hover_check() {
    POINTER_HOVER_UPDATED.store(false, Ordering::SeqCst);
    HOVER_TRACKING_ACTIVE.store(true, Ordering::SeqCst);
}

/// Called from input handlers AFTER surface_under() completes.
/// If no window's focus_under() was triggered (pointer is not over any window),
/// clears the previously-tracked hovered window.
pub fn finalize_pointer_hover() {
    HOVER_TRACKING_ACTIVE.store(false, Ordering::SeqCst);
    if !POINTER_HOVER_UPDATED.load(Ordering::SeqCst) {
        // No window was hit this cycle — clear the old one
        let mut guard = POINTER_HOVERED_WINDOW.lock().unwrap();
        if let Some(old) = guard.take() {
            old.set_pointer_over_window(false);
            old.0.force_update();
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CosmicWindow(pub(super) IcedElement<CosmicWindowInternal>);

impl fmt::Debug for CosmicWindow {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CosmicWindow")
            .field("internal", &self.0)
            .finish_non_exhaustive()
    }
}

#[derive(Debug)]
pub struct CosmicWindowInternal {
    pub(super) window: CosmicSurface,
    activated: AtomicBool,
    /// TODO: This needs to be per seat
    pointer_entered: AtomicU8,
    /// Whether any pointer is currently over the window (header, content, or resize borders).
    pointer_over_window: AtomicBool,
    last_title: Mutex<String>,
    /// Cached app icon handle, resolved once and refreshed when app_id changes.
    cached_icon: Mutex<(String, Option<super::header_bar::AppIcon>)>,
    /// Client-set toplevel icon (xdg-toplevel-icon protocol), taking priority
    /// over `cached_icon`. Tuple: (fingerprint of the committed icon, resolved
    /// icon). Refreshed when the fingerprint changes.
    client_icon: Mutex<(String, Option<super::header_bar::AppIcon>)>,
    /// Desktop override from .desktop file with X-Cosmic-AppIdMatch.
    /// Tuple: (app_id used for lookup, optional override).
    desktop_override: Mutex<(String, Option<DesktopOverride>)>,
    tiled: AtomicBool,
    /// Whether the window fills the output zone (position 0,0 and size >= zone).
    /// Used to give square corners to non-maximized windows that visually fill the screen.
    fills_output_zone: AtomicBool,
    theme: Mutex<crate::comp_theme::CompTheme>,
    appearance_conf: Mutex<AppearanceConfig>,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Focus {
    Header = 1,
    ResizeTop,
    ResizeLeft,
    ResizeRight,
    ResizeBottom,
    ResizeTopRight,
    ResizeTopLeft,
    ResizeBottomRight,
    ResizeBottomLeft,
}

impl Focus {
    pub fn under(
        surface: &CosmicSurface,
        header_height: i32,
        location: Point<f64, Logical>,
    ) -> Option<Focus> {
        let geo = surface.geometry();
        let loc = location.to_i32_floor::<i32>() - geo.loc;
        if loc.y < 0 && loc.x < 0 {
            Some(Focus::ResizeTopLeft)
        } else if loc.y < 0 && loc.x >= geo.size.w {
            Some(Focus::ResizeTopRight)
        } else if loc.y < 0 {
            Some(Focus::ResizeTop)
        } else if loc.y >= header_height + geo.size.h && loc.x < 0 {
            Some(Focus::ResizeBottomLeft)
        } else if loc.y >= header_height + geo.size.h && loc.x >= geo.size.w {
            Some(Focus::ResizeBottomRight)
        } else if loc.y >= header_height + geo.size.h {
            Some(Focus::ResizeBottom)
        } else if loc.x < 0 {
            Some(Focus::ResizeLeft)
        } else if loc.x >= geo.size.w {
            Some(Focus::ResizeRight)
        } else if loc.y < header_height {
            Some(Focus::Header)
        } else {
            None
        }
    }

    pub fn cursor_shape(&self) -> CursorIcon {
        match self {
            Focus::ResizeTopLeft => CursorIcon::NwResize,
            Focus::ResizeTopRight => CursorIcon::NeResize,
            Focus::ResizeTop => CursorIcon::NResize,
            Focus::ResizeBottomLeft => CursorIcon::SwResize,
            Focus::ResizeBottomRight => CursorIcon::SeResize,
            Focus::ResizeBottom => CursorIcon::SResize,
            Focus::ResizeLeft => CursorIcon::WResize,
            Focus::ResizeRight => CursorIcon::EResize,
            Focus::Header => CursorIcon::Default,
        }
    }

    /// # Safety
    /// `value` must be in the range of `Focus`
    pub unsafe fn from_u8(value: u8) -> Option<Focus> {
        match value {
            0 => None,
            focus => unsafe { Some(std::mem::transmute::<u8, Focus>(focus)) },
        }
    }
}

/// Maps an iced `mouse::Interaction` to a smithay `CursorIcon` so the
/// compositor can display the correct cursor for SSD header bar widgets.
pub(crate) fn mouse_interaction_to_cursor_icon(interaction: MouseInteraction) -> CursorIcon {
    match interaction {
        MouseInteraction::Pointer => CursorIcon::Pointer,
        MouseInteraction::Grab => CursorIcon::Grab,
        MouseInteraction::Grabbing => CursorIcon::Grabbing,
        MouseInteraction::Text => CursorIcon::Text,
        MouseInteraction::Crosshair => CursorIcon::Crosshair,
        MouseInteraction::NotAllowed => CursorIcon::NotAllowed,
        MouseInteraction::ResizingHorizontally => CursorIcon::ColResize,
        MouseInteraction::ResizingVertically => CursorIcon::RowResize,
        MouseInteraction::Move => CursorIcon::Move,
        MouseInteraction::Copy => CursorIcon::Copy,
        MouseInteraction::Help => CursorIcon::Help,
        MouseInteraction::Cell => CursorIcon::Cell,
        MouseInteraction::Progress => CursorIcon::Progress,
        MouseInteraction::ZoomIn => CursorIcon::ZoomIn,
        MouseInteraction::ZoomOut => CursorIcon::ZoomOut,
        _ => CursorIcon::Default,
    }
}

/// Target size (in pixels) for pre-scaling raster icons.
/// Set to 2× the SSD icon display size (18px in header_bar.rs) so that on
/// HiDPI displays (scale 2.0) tiny_skia does a 1:1 blit, and on 1× displays
/// it only needs a clean 2:1 bilinear downscale.
const ICON_PRESCALE_SIZE: u32 = 36;

/// Resolve an application icon for the SSD header bar.
///
/// Lookup order:
///   1. XDG icon theme via resolved path (exact match)
///   2. XDG icon theme via resolved path (case-insensitive)
///   3. `<datadir>/pixmaps/{name}.{ext}` for common extensions, over every XDG
///      data directory (`/usr/local/share:/usr/share` by default)
///
/// For ALL raster icons (PNG from theme or pixmaps), the image is pre-scaled
/// to ICON_PRESCALE_SIZE using Lanczos3 filtering.
fn resolve_app_icon(app_id: &str) -> Option<super::header_bar::AppIcon> {
    use crate::utils::xdg_icon::resolve_icon_path;

    let size = ICON_PRESCALE_SIZE as f32;

    // 1. Exact name in icon theme
    if let Some(path) = resolve_icon_path(app_id, size) {
        return Some(prescale_icon_from_path(&path.to_string_lossy()));
    }

    // 2. Lower-case fallback
    let lower = app_id.to_lowercase();
    if lower != app_id
        && let Some(path) = resolve_icon_path(&lower, size)
    {
        return Some(prescale_icon_from_path(&path.to_string_lossy()));
    }

    // 3. Direct pixmap lookup
    for dir in crate::utils::xdg_dirs::data_dirs("pixmaps") {
        for ext in &["png", "svg", "xpm"] {
            let pixmap = dir.join(format!("{lower}.{ext}"));
            if pixmap.exists() {
                return Some(prescale_icon_from_path(&pixmap.to_string_lossy()));
            }
        }
    }

    None
}

/// Load an icon from a file path, applying Lanczos3 pre-scaling for raster
/// images (PNG) and leaking SVG bytes for static lifetime.
fn prescale_icon_from_path(path: &str) -> super::header_bar::AppIcon {
    if path.ends_with(".svg") || path.ends_with(".svgz") {
        // icetron's title_icon API wants &'static [u8], so the bytes are
        // leaked -- memoized by path, so each icon file leaks ONCE ever.
        // Leaking per call turned into the weekend leak: this runs from
        // `refresh()` for client-set named icons, several times a second.
        static SVG_BYTES: std::sync::LazyLock<
            Mutex<std::collections::HashMap<String, &'static [u8]>>,
        > = std::sync::LazyLock::new(Default::default);
        let mut cache = SVG_BYTES.lock().unwrap();
        let bytes = cache.entry(path.to_owned()).or_insert_with(|| {
            std::fs::read(path)
                .map(|bytes| &*Box::leak(bytes.into_boxed_slice()))
                .unwrap_or(&[])
        });
        // The freedesktop convention: a `-symbolic` icon is a single-colour
        // glyph meant to be tinted, anything else is artwork to leave alone.
        let symbolic = path.ends_with("-symbolic.svg") || path.ends_with("-symbolic.svgz");
        return super::header_bar::AppIcon::Svg { bytes, symbolic };
    }

    // Raster image: pre-scale with Lanczos3 for crisp rendering
    prescale_raster_icon(path).unwrap_or_else(|| {
        super::header_bar::AppIcon::Image(iced_core::image::Handle::from_path(path))
    })
}

/// Load a raster image and pre-scale it to ICON_PRESCALE_SIZE using
/// Lanczos3 filtering. Returns RGBA pixels wrapped in an iced Handle.
fn prescale_raster_icon(path: &str) -> Option<super::header_bar::AppIcon> {
    use image::imageops::FilterType;

    let img = image::open(path).ok()?;
    let resized = img.resize(ICON_PRESCALE_SIZE, ICON_PRESCALE_SIZE, FilterType::Lanczos3);
    let rgba = resized.to_rgba8();
    let (w, h) = rgba.dimensions();
    let pixels = rgba.into_raw();

    Some(super::header_bar::AppIcon::Image(
        iced_core::image::Handle::from_rgba(w, h, pixels),
    ))
}

/// Read the client-set toplevel icon (xdg-toplevel-icon protocol) committed on
/// `surface`, if it changed. Returns `None` when the committed icon still
/// matches `last_fingerprint`, WITHOUT building an icon: this runs on every
/// `refresh()`, and building first to compare after meant an icon-theme lookup
/// (leaking SVG bytes) or a full shm decode several times a second, forever.
/// A `Some(("", None))` means the client has no icon (fall back to app_id).
fn read_client_toplevel_icon(
    surface: &WlSurface,
    last_fingerprint: &str,
) -> Option<(String, Option<super::header_bar::AppIcon>)> {
    use smithay::reexports::wayland_server::Resource;
    use smithay::wayland::compositor::with_states;
    use smithay::wayland::xdg_toplevel_icon::ToplevelIconCachedState;

    with_states(surface, |states| {
        let mut guard = states.cached_state.get::<ToplevelIconCachedState>();
        let current = guard.current();

        // Named icon: resolve through the icon theme like an app_id.
        if let Some(name) = current.icon_name() {
            let fingerprint = format!("name:{name}");
            if fingerprint == last_fingerprint {
                return None;
            }
            let icon = resolve_app_icon(name);
            return Some((fingerprint, icon));
        }

        // Pixel buffers: pick the largest square buffer and decode it.
        if let Some((buffer, _scale)) = current
            .buffers()
            .iter()
            .max_by_key(|(b, _)| shm_buffer_edge(b))
        {
            let fingerprint = format!("buf:{:?}", buffer.id());
            if fingerprint == last_fingerprint {
                return None;
            }
            let icon = shm_buffer_to_app_icon(buffer);
            return Some((fingerprint, icon));
        }

        if last_fingerprint.is_empty() {
            return None;
        }
        Some((String::new(), None))
    })
}

/// Edge length (square) of an shm buffer, or 0 if it can't be read.
fn shm_buffer_edge(
    buffer: &smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer,
) -> i32 {
    use smithay::wayland::shm::with_buffer_contents;
    with_buffer_contents(buffer, |_ptr, _len, data| data.width).unwrap_or(0)
}

/// Decode an shm pixel buffer (ARGB/XRGB 8888) into an `AppIcon::Image`.
fn shm_buffer_to_app_icon(
    buffer: &smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer,
) -> Option<super::header_bar::AppIcon> {
    use smithay::reexports::wayland_server::protocol::wl_shm::Format;
    use smithay::wayland::shm::with_buffer_contents;

    with_buffer_contents(buffer, |ptr, len, data| {
        let width = data.width.max(0) as usize;
        let height = data.height.max(0) as usize;
        let stride = data.stride.max(0) as usize;
        if width == 0 || height == 0 || stride < width * 4 {
            return None;
        }
        // SAFETY: smithay guarantees `ptr`/`len` describe the mapped buffer for
        // the duration of this callback.
        let bytes = unsafe { std::slice::from_raw_parts(ptr, len) };
        if bytes.len() < (height - 1) * stride + width * 4 {
            return None;
        }
        let opaque = matches!(data.format, Format::Xrgb8888);
        let mut rgba = Vec::with_capacity(width * height * 4);
        for y in 0..height {
            let row = &bytes[y * stride..y * stride + width * 4];
            for px in row.chunks_exact(4) {
                // wl_shm ARGB8888/XRGB8888 are little-endian: bytes are B, G, R, A.
                let (b, g, r, a) = (px[0], px[1], px[2], px[3]);
                let a = if opaque { 255 } else { a };
                rgba.extend_from_slice(&[r, g, b, a]);
            }
        }
        Some(super::header_bar::AppIcon::Image(
            iced_core::image::Handle::from_rgba(width as u32, height as u32, rgba),
        ))
    })
    .ok()
    .flatten()
}

/// Override properties parsed from a .desktop file with `X-Cosmic-AppIdMatch`.
#[derive(Debug, Clone, Default)]
struct DesktopOverride {
    forced_title: Option<String>,
    forced_icon: Option<String>,
}

/// Simple glob matching supporting `*` (any sequence) and `?` (single char).
fn glob_match(pattern: &str, text: &str) -> bool {
    let (p, t) = (pattern.as_bytes(), text.as_bytes());
    let (mut px, mut tx) = (0usize, 0usize);
    let (mut star_px, mut star_tx) = (usize::MAX, 0usize);
    while tx < t.len() {
        if px < p.len() && (p[px] == b'?' || p[px] == t[tx]) {
            px += 1;
            tx += 1;
        } else if px < p.len() && p[px] == b'*' {
            star_px = px;
            star_tx = tx;
            px += 1;
        } else if star_px != usize::MAX {
            px = star_px + 1;
            star_tx += 1;
            tx = star_tx;
        } else {
            return false;
        }
    }
    while px < p.len() && p[px] == b'*' {
        px += 1;
    }
    px == p.len()
}

/// Scan XDG application directories for a .desktop file whose
/// `X-Cosmic-AppIdMatch` glob matches `app_id`, returning any
/// `X-Cosmic-ForcedTitle` / `X-Cosmic-ForcedIcon` overrides.
fn find_desktop_override(app_id: &str) -> Option<DesktopOverride> {
    if app_id.is_empty() {
        return None;
    }
    let search_dirs = crate::utils::xdg_dirs::data_dirs("applications");
    for dir in &search_dirs {
        let entries = match std::fs::read_dir(dir) {
            Ok(e) => e,
            Err(_) => continue,
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) != Some("desktop") {
                continue;
            }
            if let Some(ovr) = parse_desktop_override(&path, app_id) {
                return Some(ovr);
            }
        }
    }
    None
}

/// Parse a single .desktop file for X-Cosmic override keys.
fn parse_desktop_override(path: &std::path::Path, app_id: &str) -> Option<DesktopOverride> {
    let content = std::fs::read_to_string(path).ok()?;
    let mut in_desktop_entry = false;
    let mut match_pattern: Option<String> = None;
    let mut forced_title: Option<String> = None;
    let mut forced_icon: Option<String> = None;
    for line in content.lines() {
        let line = line.trim();
        if line.starts_with('[') {
            if in_desktop_entry && match_pattern.is_some() {
                break;
            }
            in_desktop_entry = line == "[Desktop Entry]";
            continue;
        }
        if !in_desktop_entry {
            continue;
        }
        if let Some(val) = line.strip_prefix("X-Cosmic-AppIdMatch=") {
            match_pattern = Some(val.to_string());
        } else if let Some(val) = line.strip_prefix("X-Cosmic-ForcedTitle=") {
            forced_title = Some(val.to_string());
        } else if let Some(val) = line.strip_prefix("X-Cosmic-ForcedIcon=") {
            forced_icon = Some(val.to_string());
        }
    }
    let pattern = match_pattern?;
    if !glob_match(&pattern, app_id) {
        return None;
    }
    if forced_title.is_none() && forced_icon.is_none() {
        return None;
    }
    Some(DesktopOverride {
        forced_title,
        forced_icon,
    })
}

impl CosmicWindowInternal {
    /// SSD header height derived from the current theme's window control style.
    fn ssd_height(&self) -> i32 {
        icetron_p::prelude::header_height(&**self.theme.lock().unwrap()) as i32
    }

    pub fn swap_focus(&self, focus: Option<Focus>) -> Option<Focus> {
        let value = focus.map_or(0, |x| x as u8);
        unsafe { Focus::from_u8(self.pointer_entered.swap(value, Ordering::SeqCst)) }
    }

    pub fn current_focus(&self) -> Option<Focus> {
        unsafe { Focus::from_u8(self.pointer_entered.load(Ordering::SeqCst)) }
    }

    /// returns if the window has any current or pending server-side decorations
    pub fn has_ssd(&self, pending: bool) -> bool {
        !self.window.is_decorated(pending)
    }

    /// returns if the window is currently tiled
    pub fn is_tiled(&self) -> bool {
        self.tiled.load(Ordering::Acquire)
    }

    fn has_tiled_state(&self) -> bool {
        self.window.is_tiled(false).unwrap_or(false)
    }

    /// Compute corner radius based on window state, appearance config, and theme.
    /// This is a helper that can be called from within `with_program` closures.
    pub fn compute_corner_radius(
        &self,
        geometry_size: Size<i32, Logical>,
        default_radius: u8,
    ) -> [u8; 4] {
        let is_embedded = is_surface_embedded(&self.window);
        let has_ssd_raw = self.has_ssd(false);
        let has_ssd = has_ssd_raw && !is_embedded;
        let is_tiled = self.is_tiled();
        let is_maximized = self.window.is_maximized(false);
        let appearance = self.appearance_conf.lock().unwrap();

        // Maximized windows always have 0 corner radius
        if is_maximized {
            return [0; 4];
        }

        // Non-maximized windows that fill the output zone also get square corners
        if self.fills_output_zone.load(Ordering::Acquire) {
            return [0; 4];
        }

        // X11 transient children (utility/toolbar windows) get 0 corner radius
        // unless the surface itself explicitly sets a radius.
        // This matches GNOME/KWin behavior where sub-windows have square corners.
        let is_x11_transient = self
            .window
            .x11_surface()
            .is_some_and(|x| x.is_transient_for().is_some());
        if is_x11_transient {
            let surface_corners = self.window.corner_radius(geometry_size);
            return surface_corners.unwrap_or([0; 4]);
        }

        let clip = (!is_tiled && appearance.clip_floating_windows)
            || (is_tiled && appearance.clip_tiled_windows);
        let round = !is_tiled || appearance.clip_tiled_windows;
        let radii = if round {
            {
                self.theme
                    .lock()
                    .unwrap()
                    .radius_window()
                    .map(|x| x.round() as u8)
            }
        } else {
            [0; 4]
        };

        let surface_corners = self.window.corner_radius(geometry_size);

        match (has_ssd, clip) {
            (true, true) => {
                // SSD: compositor owns decorations — always use theme radius.
                // Client-set protocol values are ignored so theme changes propagate immediately.
                radii
            }
            (false, true) => {
                // CSD: client draws its own corners — prefer client hint if set.
                surface_corners.unwrap_or(radii)
            }
            (_, false) => surface_corners.unwrap_or([default_radius; 4]),
        }
    }
}

impl CosmicWindow {
    /// SSD header height for this window, derived from its theme.
    pub fn ssd_height(&self) -> i32 {
        self.0.with_program(|p| p.ssd_height())
    }

    pub fn new(
        window: impl Into<CosmicSurface>,
        handle: LoopHandle<'static, crate::state::State>,
        theme: crate::comp_theme::CompTheme,
        appearance: AppearanceConfig,
    ) -> CosmicWindow {
        let window = window.into();
        let width = window.geometry().size.w;
        let last_title = window.title();
        let app_id = window.app_id();
        let desktop_ovr = find_desktop_override(&app_id);
        let icon_name = desktop_ovr
            .as_ref()
            .and_then(|o| o.forced_icon.as_deref())
            .unwrap_or(&app_id)
            .to_string();
        // MERGE: upstream sets `theme.transparent = theme.cosmic().frosted_windows` here.
        // `CompTheme` has no `transparent`/`frosted_windows` equivalent, so it is dropped;
        // window frosting is driven by `WINDOW_BLUR_STRENGTH` below instead.

        // Note: We intentionally do NOT set_tiled based on clip_floating_windows.
        // The tiled protocol state should only reflect actual tiling status,
        // not visual clipping preferences. Clipping is handled compositor-side.

        // Create the window with no icon — resolve asynchronously to avoid
        // blocking the compositor loop with XDG icon lookup + image I/O.
        let cosmic_window = CosmicWindow(IcedElement::new(
            CosmicWindowInternal {
                window,
                activated: AtomicBool::new(false),
                pointer_entered: AtomicU8::new(0),
                pointer_over_window: AtomicBool::new(false),
                last_title: Mutex::new(last_title),
                cached_icon: Mutex::new((app_id.clone(), None)),
                client_icon: Mutex::new((String::new(), None)),
                desktop_override: Mutex::new((app_id.clone(), desktop_ovr)),
                tiled: AtomicBool::new(false),
                fills_output_zone: AtomicBool::new(false),
                theme: Mutex::new(theme.clone()),
                appearance_conf: Mutex::new(appearance),
            },
            (width, icetron_p::prelude::header_height(&*theme) as i32),
            handle.clone(),
            theme,
        ));

        // Resolve the app icon on a background thread so the expensive XDG
        // lookup + Lanczos3 rescaling doesn't block the compositor event loop.
        // The header bar gracefully shows just the title while icon is None.
        let element = cosmic_window.0.clone();
        let app_id_clone = app_id;
        std::thread::spawn(move || {
            let icon = resolve_app_icon(&icon_name);
            element.with_program(|p| {
                *p.cached_icon.lock().unwrap() = (app_id_clone, icon);
            });
            element.force_update();
        });

        cosmic_window
    }

    pub fn pending_size(&self) -> Option<Size<i32, Logical>> {
        self.0.with_program(|p| {
            let mut size = p.window.pending_size()?;
            if p.has_ssd(true) {
                size.h += p.ssd_height();
            }
            Some(size)
        })
    }

    pub fn last_server_size(&self) -> Option<Size<i32, Logical>> {
        self.0.with_program(|p| {
            let mut size = p.window.last_server_size()?;
            if p.has_ssd(false) {
                size.h += p.ssd_height();
            }
            Some(size)
        })
    }

    pub fn set_geometry(&self, geo: Rectangle<i32, Global>) {
        self.0.with_program(|p| {
            let ssd_height = if p.has_ssd(true) { p.ssd_height() } else { 0 };
            let loc = (geo.loc.x, geo.loc.y + ssd_height);
            let size = (geo.size.w, std::cmp::max(geo.size.h - ssd_height, 0));
            p.window
                .set_geometry(Rectangle::new(loc.into(), size.into()), ssd_height as u32);
        });
    }

    pub fn on_commit(&self, surface: &WlSurface) {
        let mut geo = None;
        self.0.with_program(|p| {
            if &p.window == surface {
                p.window.0.on_commit();
                geo = Some(p.window.geometry());
            }
        });
        if let Some(geo) = geo {
            self.0.resize(Size::from((geo.size.w, self.ssd_height())));
        }
    }

    pub fn surface(&self) -> CosmicSurface {
        self.0.with_program(|p| p.window.clone())
    }

    pub fn focus_under(
        &self,
        mut relative_pos: Point<f64, Logical>,
        surface_type: WindowSurfaceType,
        seat: Option<&Seat<State>>,
    ) -> Option<(PointerFocusTarget, Point<f64, Logical>)> {
        let has_constraint = if let Some(seat) = seat
            && let Some(pointer) = seat.get_pointer()
            && let Some(surface) = self.wl_surface()
            && with_pointer_constraint(&surface, &pointer, |constraint| {
                constraint.is_some_and(|c| c.is_active())
            }) {
            true
        } else {
            false
        };

        let result = self.0.with_program(|p| {
            let mut offset = Point::from((0., 0.));
            let mut window_ui = None;
            let has_ssd = p.has_ssd(false);
            let has_blur = p.window.has_blur();
            let is_embedded = is_surface_embedded(&p.window);

            if (!has_constraint && (has_ssd || p.has_tiled_state() || has_blur))
                && !is_embedded
                && surface_type.contains(WindowSurfaceType::TOPLEVEL)
            {
                let geo = p.window.geometry();

                let point_i32 = relative_pos.to_i32_floor::<i32>();
                let ssd_height = if has_ssd { p.ssd_height() } else { 0 };

                if (point_i32.x - geo.loc.x >= -RESIZE_BORDER && point_i32.x - geo.loc.x < 0)
                    || (point_i32.y - geo.loc.y >= -RESIZE_BORDER && point_i32.y - geo.loc.y < 0)
                    || (point_i32.x - geo.loc.x >= geo.size.w
                        && point_i32.x - geo.loc.x < geo.size.w + RESIZE_BORDER)
                    || (point_i32.y - geo.loc.y >= geo.size.h + ssd_height
                        && point_i32.y - geo.loc.y < geo.size.h + ssd_height + RESIZE_BORDER)
                {
                    window_ui = Some((
                        PointerFocusTarget::WindowUI(self.clone()),
                        Point::from((0., 0.)),
                    ));
                }

                if has_ssd && (point_i32.y - geo.loc.y < p.ssd_height()) {
                    window_ui = Some((
                        PointerFocusTarget::WindowUI(self.clone()),
                        Point::from((0., 0.)),
                    ));
                }
            }

            if has_ssd {
                relative_pos.y -= p.ssd_height() as f64;
                offset.y += p.ssd_height() as f64;
            }

            window_ui.or_else(|| {
                p.window
                    .focus_under(relative_pos, surface_type)
                    .map(|(target, surface_offset)| (target, offset + surface_offset))
            })
        });

        // Whenever focus_under returns Some, the pointer is geometrically within this window.
        // Only update hover state if hover tracking is active (not during grabs).
        if result.is_some() && HOVER_TRACKING_ACTIVE.load(Ordering::SeqCst) {
            POINTER_HOVER_UPDATED.store(true, Ordering::SeqCst);
            let mut guard = POINTER_HOVERED_WINDOW.lock().unwrap();
            let is_same = guard.as_ref() == Some(self);
            if !is_same {
                // Clear old window if different
                if let Some(old) = guard.take() {
                    old.set_pointer_over_window(false);
                    old.0.force_update();
                }
                // Set new window
                self.0.with_program(|p| {
                    p.pointer_over_window.store(true, Ordering::SeqCst);
                });
                self.0.force_update();
                *guard = Some(self.clone());
            }
        }

        result
    }

    /// Set whether the pointer is currently over this window.
    /// Called from PointerFocusTarget enter/leave to track hover state.
    pub fn set_pointer_over_window(&self, value: bool) {
        self.0.with_program(|p| {
            p.pointer_over_window.store(value, Ordering::SeqCst);
        });
    }

    pub fn contains_surface(&self, window: &CosmicSurface) -> bool {
        self.0.with_program(|p| &p.window == window)
    }

    pub fn offset(&self) -> Point<i32, Logical> {
        let has_ssd = self.0.with_program(|p| p.has_ssd(false));
        if has_ssd {
            Point::from((0, self.ssd_height()))
        } else {
            Point::from((0, 0))
        }
    }

    pub(super) fn loop_handle(&self) -> LoopHandle<'static, crate::state::State> {
        self.0.loop_handle()
    }

    pub fn push_popup_render_elements<R>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
        alpha: f32,
        scanout_node: Option<DrmNode>,
        push: &mut dyn FnMut(CosmicWindowRenderElement<R>),
        shadow: Option<PopupShadow<'_>>,
    ) where
        R: Renderer + AsGlowRenderer + ImportAll + ImportMem,
        R::TextureId: Send + Clone + 'static,
    {
        let has_ssd = self.0.with_program(|p| p.has_ssd(false));

        let window_loc = if has_ssd {
            location + Point::from((0, (self.ssd_height() as f64 * scale.y) as i32))
        } else {
            location
        };

        self.0.with_program(|p| {
            p.window.push_popup_render_elements(
                renderer,
                window_loc,
                scale,
                alpha,
                scanout_node,
                WINDOW_BLUR_STRENGTH,
                &mut |elem| push(elem.into()),
                shadow,
            )
        })
    }

    pub fn shadow_render_element<R, C>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        max_size: Option<Size<i32, Logical>>,
        output_scale: Scale<f64>,
        scale: f64,
        alpha: f32,
    ) -> Option<C>
    where
        R: AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
        C: From<CosmicWindowRenderElement<R>>,
    {
        self.0.with_program(|p| {
            if is_surface_embedded(&p.window) {
                return None;
            }

            let has_ssd = p.has_ssd(false);
            let is_tiled = p.is_tiled();
            let activated = p.window.is_activated(false);
            let has_blur = p.window.has_blur();
            let is_maximized = p.window.is_maximized(false);

            // Skip shadows for maximized windows
            if is_maximized {
                return None;
            }

            // Extract appearance config values we need
            let (shadow_tiled_windows, clip_floating_windows) = {
                let appearance = p.appearance_conf.lock().unwrap();
                (
                    appearance.shadow_tiled_windows,
                    appearance.clip_floating_windows,
                )
            };

            // Blur windows always get shadows (they're typically overlay-style windows)
            // For other windows: tiled uses shadow_tiled_windows config, floating uses clip or ssd
            let should_draw_shadow = if has_blur {
                true
            } else if is_tiled {
                shadow_tiled_windows
            } else {
                clip_floating_windows || has_ssd
            };

            if !should_draw_shadow {
                return None;
            }

            // Get the geometry size for corner radius lookup
            let geo_size = SpaceElement::geometry(&p.window).size;

            // Use helper function to compute corner radius (avoids code duplication)
            let radii = p.compute_corner_radius(geo_size, 0);

            // Extract shadow parameters from theme
            let theme = p.theme.lock().unwrap();
            let shadow_layers = if activated {
                theme.shadow_window()
            } else {
                theme.shadow_window_unfocused()
            };
            drop(theme);

            // Use the first shadow layer (the shader supports a single shadow)
            let shadow = shadow_layers.first()?;
            let shadow_color = [
                shadow.color.r,
                shadow.color.g,
                shadow.color.b,
                shadow.color.a,
            ];
            let shadow_offset = [shadow.offset.x, shadow.offset.y];
            let shadow_softness = shadow.blur_radius;

            let mut geo = SpaceElement::geometry(&p.window).to_f64();
            if has_ssd {
                geo.size.h += p.ssd_height() as f64;
            }
            geo = geo.upscale(scale);
            geo.loc += location.to_f64().to_logical(output_scale);
            if let Some(max_size) = max_size {
                geo.size = geo.size.clamp(Size::default(), max_size.to_f64());
            }

            let window_key =
                CosmicMappedKey(CosmicMappedKeyInner::Window(Arc::downgrade(&self.0.0)));

            Some(
                CosmicWindowRenderElement::Shadow(ShadowShader::element(
                    renderer,
                    window_key,
                    geo.to_i32_round().as_local(),
                    radii,
                    alpha,
                    output_scale.x,
                    shadow_color,
                    shadow_offset,
                    shadow_softness,
                ))
                .into(),
            )
        })
    }

    pub fn push_render_elements<R>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        max_size: Option<Size<i32, Logical>>,
        scale: Scale<f64>,
        alpha: f32,
        scanout_override: Option<bool>,
        scanout_node: Option<DrmNode>,
        push_above: &mut dyn FnMut(CosmicWindowRenderElement<R>),
        push_below: &mut dyn FnMut(CosmicWindowRenderElement<R>),
    ) where
        R: AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
    {
        let embed_render_info = self.0.with_program(|p| get_embed_render_info(&p.window));
        let is_embedded = embed_render_info.is_some();
        let embed_corner_radius = embed_render_info.map(|info| info.corner_radius);

        let (has_ssd, is_tiled, is_maximized, mut radii, appearance, has_blur) =
            self.0.with_program(|p| {
                let geo_size = SpaceElement::geometry(&p.window).size;
                (
                    p.has_ssd(false),
                    p.is_tiled(),
                    p.window.is_maximized(false),
                    embed_corner_radius.unwrap_or_else(|| p.compute_corner_radius(geo_size, 0)),
                    *p.appearance_conf.lock().unwrap(),
                    p.window.has_blur(),
                )
            });
        let clip = ((!is_tiled && appearance.clip_floating_windows)
            || (is_tiled && appearance.clip_tiled_windows))
            && !is_maximized;

        if has_ssd && !clip && !is_embedded {
            // bottom corners
            radii[0] = 0;
            radii[2] = 0;
            if is_tiled {
                // top corners
                radii[1] = 0;
                radii[3] = 0;
            }
        }

        let window_loc = if has_ssd && !is_embedded {
            location + Point::from((0, (self.ssd_height() as f64 * scale.y) as i32))
        } else {
            location
        };

        // MERGE: upstream also reads `bg_divider` here for its border color; the fork takes the
        // border color from `CompTheme::window_border_color()` below instead.
        let mut geo = self
            .0
            .with_program(|p| SpaceElement::geometry(&p.window).to_f64());
        geo.loc += location.to_f64().to_logical(scale);
        if has_ssd && !is_embedded {
            geo.size.h += self.ssd_height() as f64;
        }
        if let Some(max_size) = max_size {
            geo.size = geo.size.clamp(Size::default(), max_size.to_f64());
        }

        if ((has_ssd || clip) && !is_maximized && !has_blur) || is_embedded {
            let window_key =
                CosmicMappedKey(CosmicMappedKeyInner::Window(Arc::downgrade(&self.0.0)));

            let (border_color, border_thickness) = self.0.with_program(|p| {
                let theme = p.theme.lock().unwrap();
                let c = theme.window_border_color();
                ([c.r, c.g, c.b], theme.window_border_width() as u8)
            });
            let border_alpha = self.0.with_program(|p| {
                let theme = p.theme.lock().unwrap();
                theme.window_border_color().a
            });
            // SSD windows: draw the border inset (inside the geo) so the
            // border overlays the header/surface edges with no gap at the top.
            // CSD windows: same — draw the border inside the geo.
            let elem = CosmicWindowRenderElement::Border(IndicatorShader::element(
                renderer,
                Key::Window(Usage::Border, window_key.clone()),
                geo.to_i32_round().as_local(),
                border_thickness,
                radii,
                border_alpha * alpha,
                scale.x,
                border_color,
            ));
            push_above(elem);
        }

        // MERGE: clipping/rounding of the toplevel surface now happens inside
        // `CosmicSurface::push_render_elements` (upstream folded ClippedSurfaceRenderElement into
        // SurfaceRenderElement), so the fork only forwards `clip` + `radii`.
        self.0.with_program(|p| {
            let mut radii = radii;
            if has_ssd && !is_embedded {
                // top corners are covered by the SSD header
                radii[1] = 0;
                radii[3] = 0;
            }
            p.window.push_render_elements(
                renderer,
                window_loc,
                scale,
                alpha,
                scanout_override,
                scanout_node,
                clip,
                radii,
                WINDOW_BLUR_STRENGTH,
                &mut |elem| push_above(elem.into()),
                Some(&mut |elem| push_below(elem.into())),
            );
        });

        if has_ssd && !is_embedded {
            // bottom corners belong to the surface, not the header
            radii[0] = 0;
            radii[2] = 0;
            let ssd_loc = location
                + self
                    .0
                    .with_program(|p| p.window.geometry().loc.to_physical_precise_round(scale));
            self.0.push_render_elements(
                renderer,
                ssd_loc,
                scale,
                alpha,
                radii,
                &mut |elem| push_above(elem.into()),
                Some(&mut |elem| push_below(elem.into())),
            );
        }
    }

    pub(crate) fn set_theme(&self, theme: crate::comp_theme::CompTheme) {
        self.0.with_program(|p| {
            *p.theme.lock().unwrap() = theme.clone();
        });
        self.0.set_theme(theme);
        // Resize the IcedElement to the new SSD height so there's no gap
        // between header and window content after a theme change.
        let geo = self.0.with_program(|p| p.window.geometry());
        self.0.resize(Size::from((geo.size.w, self.ssd_height())));
    }

    pub fn update_appearance_conf(&self, appearance: &AppearanceConfig) {
        self.0.with_program(|p| {
            let mut conf = p.appearance_conf.lock().unwrap();
            if &*conf != appearance {
                *conf = *appearance;
                // Note: We do NOT modify tiled state based on clip_floating_windows.
                // The tiled protocol state should only reflect actual tiling status.
                // Clipping is handled compositor-side using the appearance config.
            }
        })
    }

    pub(crate) fn force_redraw(&self) {
        self.0.force_redraw();
    }

    pub fn min_size(&self) -> Option<Size<i32, Logical>> {
        self.0
            .with_program(|p| p.window.min_size_without_ssd())
            .map(|size| {
                if self.0.with_program(|p| !p.window.is_decorated(false)) {
                    size + (0, self.ssd_height()).into()
                } else {
                    size
                }
            })
    }
    pub fn max_size(&self) -> Option<Size<i32, Logical>> {
        self.0
            .with_program(|p| p.window.max_size_without_ssd())
            .map(|size| {
                if self.0.with_program(|p| !p.window.is_decorated(false)) {
                    size + (0, self.ssd_height()).into()
                } else {
                    size
                }
            })
    }

    pub fn set_tiled(&self, tiled: bool) {
        self.0.with_program(|p| {
            p.tiled.store(tiled, Ordering::Release);
            // Always send actual tiled state to the client via protocol
            p.window.set_tiled(tiled);
        });
    }

    /// Mark whether this window visually fills the entire output zone.
    /// When true, corner radius is forced to 0 (square) even if not protocol-maximized.
    pub fn set_fills_output_zone(&self, fills: bool) {
        self.0
            .with_program(|p| p.fills_output_zone.store(fills, Ordering::Release));
    }

    pub fn corner_radius(&self, geometry_size: Size<i32, Logical>, default_radius: u8) -> [u8; 4] {
        self.0
            .with_program(|p| p.compute_corner_radius(geometry_size, default_radius))
    }

    /// Check if this window has KDE blur effect enabled
    pub fn has_blur(&self) -> bool {
        self.0.with_program(|p| p.window.has_blur())
    }

    /// Check if this window has server-side decorations (SSD header)
    pub fn has_ssd(&self) -> bool {
        self.0.with_program(|p| p.has_ssd(false))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Message {
    DragStart,
    Minimize,
    Maximize,
    Close,
    Menu,
}

impl Program for CosmicWindowInternal {
    type Message = Message;

    fn program_name() -> &'static str {
        "CosmicWindow"
    }

    fn update(
        &mut self,
        message: Self::Message,
        loop_handle: &crate::utils::iced::ProgramLoop,
        last_seat: Option<&(Seat<State>, Serial)>,
    ) -> Task<Self::Message> {
        match message {
            Message::DragStart => {
                if let Some((seat, serial)) = last_seat.cloned()
                    && let Some(surface) = self.window.wl_surface().map(Cow::into_owned)
                {
                    loop_handle.insert_idle(move |state| {
                        let res = state.common.shell.write().move_request(
                            &surface,
                            &seat,
                            serial,
                            ReleaseMode::NoMouseButtons,
                            false,
                            &state.common.config,
                            &state.common.event_loop_handle,
                            false,
                        );
                        if let Some((grab, focus)) = res {
                            if grab.is_touch_grab() {
                                seat.get_touch().unwrap().set_grab(state, grab, serial);
                            } else {
                                seat.get_pointer()
                                    .unwrap()
                                    .set_grab(state, grab, serial, focus);
                                // Re-set Grabbing cursor after set_grab(), because
                                // Focus::Clear triggers leave() on the SSD header
                                // which calls unset_shape() and resets the cursor.
                                let cursor_state = seat.user_data().get::<CursorState>().unwrap();
                                cursor_state.lock().unwrap().set_shape(CursorIcon::Grabbing);
                            }
                        }
                    });
                }
            }
            Message::Minimize => {
                if let Some(surface) = self.window.wl_surface().map(Cow::into_owned) {
                    loop_handle.insert_idle(move |state| {
                        let mut shell = state.common.shell.write();
                        shell.minimize_request(&surface)
                    });
                }
            }
            Message::Maximize => {
                if let Some(surface) = self.window.wl_surface().map(Cow::into_owned) {
                    loop_handle.insert_idle(move |state| {
                        let mut shell = state.common.shell.write();
                        if let Some(mapped) = shell.element_for_surface(&surface).cloned() {
                            let seat = shell.seats.last_active().clone();
                            shell.maximize_toggle(&mapped, &seat, &state.common.event_loop_handle)
                        }
                    });
                }
            }
            Message::Close => self.window.close(),
            Message::Menu => {
                if let Some((seat, serial)) = last_seat.cloned()
                    && let Some(surface) = self.window.wl_surface().map(Cow::into_owned)
                {
                    let ssd_h = self.ssd_height();
                    loop_handle.insert_idle(move |state| {
                        let shell = state.common.shell.read();
                        if let Some(mapped) = shell.element_for_surface(&surface).cloned() {
                            let position = if let Some((output, set)) =
                                shell.workspaces.sets.iter().find(|(_, set)| {
                                    set.sticky_layer.mapped().any(|m| m == &mapped)
                                }) {
                                set.sticky_layer
                                    .element_geometry(&mapped)
                                    .unwrap()
                                    .loc
                                    .to_global(output)
                            } else if let Some(workspace) = shell.space_for(&mapped) {
                                let Some(elem_geo) = workspace.element_geometry(&mapped) else {
                                    return;
                                };
                                elem_geo.loc.to_global(&workspace.output)
                            } else {
                                return;
                            };

                            let pointer = seat.get_pointer().unwrap();
                            let mut cursor = pointer.current_location().to_i32_round();
                            cursor.y -= ssd_h;

                            let res = shell.menu_request(
                                false,
                                &surface,
                                &seat,
                                serial,
                                cursor - position.as_logical(),
                                false,
                                &state.common.config,
                                &state.common.event_loop_handle,
                            );

                            std::mem::drop(shell);
                            if let Some((grab, focus)) = res {
                                pointer.set_grab(state, grab, serial, focus);
                            }
                        }
                    });
                }
            }
        }
        Task::none()
    }

    // MERGE: upstream paints the header background opaque when maximized
    // (`theme.cosmic().background(frosted_windows).base`). The fork keeps the header fully
    // transparent so the themed header bar (and any client blur behind it) shows through.
    fn background_color(&self, _theme: &crate::comp_theme::CompTheme) -> Color {
        Color::TRANSPARENT
    }

    fn foreground(
        &self,
        _pixels: &mut tiny_skia::PixmapMut<'_>,
        _damage: &[Rectangle<i32, Buffer>],
        _scale: f32,
        _theme: &crate::comp_theme::CompTheme,
    ) {
    }

    fn view<'a>(
        &'a self,
        theme: &'a crate::comp_theme::CompTheme,
    ) -> crate::utils::iced::CompElement<'a, Self::Message> {
        HOOKS.get().unwrap().window_decorations.view(self, theme)
    }
}

#[derive(Debug)]
pub struct DefaultDecorations;

impl Decorations<CosmicWindowInternal, Message> for DefaultDecorations {
    fn view<'a>(
        &'a self,
        win: &'a CosmicWindowInternal,
        theme: &'a crate::comp_theme::CompTheme,
    ) -> crate::utils::iced::CompElement<'a, Message> {
        // Use forced title from .desktop override if available
        let title = {
            let ovr = win.desktop_override.lock().unwrap();
            ovr.1
                .as_ref()
                .and_then(|o| o.forced_title.clone())
                .unwrap_or_else(|| win.last_title.lock().unwrap().clone())
        };

        let hovered = win.pointer_over_window.load(Ordering::SeqCst);
        let focused = win.activated.load(Ordering::SeqCst);

        let mut header = super::header_bar::header_bar()
            .title(title)
            .on_drag(Message::DragStart)
            .on_close(Message::Close)
            .on_minimize(Message::Minimize)
            .on_maximize(Message::Maximize)
            .on_right_click(Message::Menu)
            .focused(focused)
            .hovered(hovered)
            .maximized(
                win.window.is_maximized(false) || win.fills_output_zone.load(Ordering::Acquire),
            )
            .theme(theme);

        // Pass the application icon if resolved — a client-set toplevel icon
        // (xdg-toplevel-icon) takes priority over the app_id-based icon.
        let icon = win
            .client_icon
            .lock()
            .unwrap()
            .1
            .clone()
            .or_else(|| win.cached_icon.lock().unwrap().1.clone());
        if let Some(icon) = icon {
            header = header.app_icon(icon);
        }

        header.into()
    }
}

impl IsAlive for CosmicWindow {
    fn alive(&self) -> bool {
        self.0.with_program(|p| p.window.alive())
    }
}

impl SpaceElement for CosmicWindow {
    fn bbox(&self) -> Rectangle<i32, Logical> {
        self.0.with_program(|p| {
            let mut bbox = SpaceElement::bbox(&p.window);
            let has_ssd = p.has_ssd(false);
            let has_blur = p.window.has_blur();
            let is_embedded = is_surface_embedded(&p.window);

            if (has_ssd || p.has_tiled_state() || has_blur) && !is_embedded {
                bbox.loc -= Point::from((RESIZE_BORDER, RESIZE_BORDER));
                bbox.size += Size::from((RESIZE_BORDER * 2, RESIZE_BORDER * 2));
            }
            if has_ssd {
                bbox.size.h += p.ssd_height();
            }

            bbox
        })
    }
    fn is_in_input_region(&self, point: &Point<f64, Logical>) -> bool {
        self.focus_under(*point, WindowSurfaceType::ALL, None)
            .is_some()
    }
    fn set_activate(&self, activated: bool) {
        if self
            .0
            .with_program(|p| p.activated.load(Ordering::SeqCst) != activated)
        {
            SpaceElement::set_activate(&self.0, activated);
            self.0.with_program(|p| {
                p.activated.store(activated, Ordering::SeqCst);
                SpaceElement::set_activate(&p.window, activated);
            });
            self.0.force_update();
        }
    }
    #[profiling::function]
    fn output_enter(&self, output: &Output, overlap: Rectangle<i32, Logical>) {
        SpaceElement::output_enter(&self.0, output, overlap);
        self.0
            .with_program(|p| SpaceElement::output_enter(&p.window, output, overlap));
    }
    #[profiling::function]
    fn output_leave(&self, output: &Output) {
        SpaceElement::output_leave(&self.0, output);
        self.0
            .with_program(|p| SpaceElement::output_leave(&p.window, output));
    }
    fn geometry(&self) -> Rectangle<i32, Logical> {
        self.0.with_program(|p| {
            let mut geo = SpaceElement::geometry(&p.window);
            if p.has_ssd(false) {
                geo.size.h += p.ssd_height();
            }
            geo
        })
    }
    fn z_index(&self) -> u8 {
        self.0.with_program(|p| SpaceElement::z_index(&p.window))
    }
    #[profiling::function]
    fn refresh(&self) {
        if self.0.with_program(|p| {
            SpaceElement::refresh(&p.window);
            if !p.has_ssd(true) {
                return false;
            }

            let title = p.window.title();
            let mut last_title = p.last_title.lock().unwrap();
            let title_changed = if *last_title != title {
                *last_title = title;
                true
            } else {
                false
            };

            // Refresh cached icon and desktop override if app_id changed
            let mut needs_redraw = title_changed;
            let app_id = p.window.app_id();
            let mut cached = p.cached_icon.lock().unwrap();
            if cached.0 != app_id {
                let new_ovr = find_desktop_override(&app_id);
                let icon_name = new_ovr
                    .as_ref()
                    .and_then(|o| o.forced_icon.as_deref())
                    .unwrap_or(&app_id);
                cached.1 = resolve_app_icon(icon_name);
                cached.0 = app_id.clone();
                drop(cached);
                *p.desktop_override.lock().unwrap() = (app_id, new_ovr);
                needs_redraw = true;
            } else {
                drop(cached);
            }

            // Pick up a client-set toplevel icon (xdg-toplevel-icon protocol);
            // it takes priority over the app_id-based icon in the header.
            if let Some(surface) = p.window.wl_surface() {
                let mut client = p.client_icon.lock().unwrap();
                if let Some((fingerprint, icon)) = read_client_toplevel_icon(&surface, &client.0) {
                    client.0 = fingerprint;
                    client.1 = icon;
                    needs_redraw = true;
                }
            }

            needs_redraw
        }) {
            self.0.force_update();
        } else {
            SpaceElement::refresh(&self.0);
        }
    }
}

impl KeyboardTarget<State> for CosmicWindow {
    fn enter(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        keys: Vec<KeysymHandle<'_>>,
        serial: Serial,
    ) {
        self.0
            .with_program(|p| KeyboardTarget::enter(&p.window, seat, data, keys, serial))
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial) {
        self.0
            .with_program(|p| KeyboardTarget::leave(&p.window, seat, data, serial))
    }
    fn key(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        key: KeysymHandle<'_>,
        state: KeyState,
        serial: Serial,
        time: u32,
    ) {
        self.0
            .with_program(|p| KeyboardTarget::key(&p.window, seat, data, key, state, serial, time))
    }
    fn modifiers(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        modifiers: ModifiersState,
        serial: Serial,
    ) {
        self.0
            .with_program(|p| KeyboardTarget::modifiers(&p.window, seat, data, modifiers, serial))
    }
}

impl PointerTarget<State> for CosmicWindow {
    fn enter(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        let mut event = event.clone();
        let is_header = self.0.with_program(|p| {
            if is_surface_embedded(&p.window) {
                return false;
            }

            let has_ssd = p.has_ssd(false);
            let has_blur = p.window.has_blur();
            if has_ssd || p.has_tiled_state() || has_blur {
                let Some(next) = Focus::under(
                    &p.window,
                    if has_ssd { p.ssd_height() } else { 0 },
                    event.location,
                ) else {
                    return false;
                };

                let old_focus = p.swap_focus(Some(next));
                assert_eq!(old_focus, None);

                // For Header focus, let the iced widget tree control the cursor
                // (grab for drag area, pointer for buttons)
                if !matches!(next, Focus::Header) {
                    let cursor_state = seat.user_data().get::<CursorState>().unwrap();
                    cursor_state.lock().unwrap().set_shape(next.cursor_shape());
                    seat.set_cursor_image_status(CursorImageStatus::default_named());
                }

                return matches!(next, Focus::Header);
            }
            false
        });

        event.location -= self.0.with_program(|p| p.window.geometry().loc.to_f64());
        PointerTarget::enter(&self.0, seat, data, &event);

        // After iced processes the event, read the mouse_interaction from the
        // widget tree and map it to a compositor cursor for the SSD header.
        if is_header {
            let cursor_icon = mouse_interaction_to_cursor_icon(self.0.mouse_interaction());
            let cursor_state = seat.user_data().get::<CursorState>().unwrap();
            cursor_state.lock().unwrap().set_shape(cursor_icon);
            seat.set_cursor_image_status(CursorImageStatus::default_named());
        }
    }

    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        let mut event = event.clone();
        let is_header = self.0.with_program(|p| {
            if is_surface_embedded(&p.window) {
                return false;
            }

            let has_ssd = p.has_ssd(false);
            let has_blur = p.window.has_blur();
            if has_ssd || p.has_tiled_state() || has_blur {
                let Some(next) = Focus::under(
                    &p.window,
                    if has_ssd { p.ssd_height() } else { 0 },
                    event.location,
                ) else {
                    return false;
                };
                let _previous = p.swap_focus(Some(next));

                // For Header focus, let the iced widget tree control the cursor
                // (grab for drag area, pointer for buttons)
                if !matches!(next, Focus::Header) {
                    let cursor_state = seat.user_data().get::<CursorState>().unwrap();
                    cursor_state.lock().unwrap().set_shape(next.cursor_shape());
                    seat.set_cursor_image_status(CursorImageStatus::default_named());
                }

                return matches!(next, Focus::Header);
            }
            false
        });

        event.location -= self.0.with_program(|p| p.window.geometry().loc.to_f64());
        PointerTarget::motion(&self.0, seat, data, &event);

        // After iced processes the event, read the mouse_interaction from the
        // widget tree and map it to a compositor cursor for the SSD header.
        if is_header {
            let cursor_icon = mouse_interaction_to_cursor_icon(self.0.mouse_interaction());
            let cursor_state = seat.user_data().get::<CursorState>().unwrap();
            cursor_state.lock().unwrap().set_shape(cursor_icon);
            seat.set_cursor_image_status(CursorImageStatus::default_named());
        }
    }

    fn relative_motion(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &RelativeMotionEvent,
    ) {
    }

    fn button(&self, seat: &Seat<State>, data: &mut State, event: &ButtonEvent) {
        if self.0.with_program(|p| is_surface_embedded(&p.window)) {
            return;
        }

        match self.0.with_program(|p| p.current_focus()) {
            Some(Focus::Header) => PointerTarget::button(&self.0, seat, data, event),
            Some(x) => {
                let serial = event.serial;
                let seat = seat.clone();
                let Some(surface) = self.wl_surface().map(Cow::into_owned) else {
                    return;
                };

                // Only start a resize if the left button was pressed
                if event.state != smithay::backend::input::ButtonState::Pressed
                    || event.button != 0x110
                {
                    return;
                }
                self.0.loop_handle().insert_idle(move |state| {
                    let res = state.common.shell.write().resize_request(
                        &surface,
                        &seat,
                        serial,
                        match x {
                            Focus::ResizeTop => ResizeEdge::TOP,
                            Focus::ResizeTopLeft => ResizeEdge::TOP_LEFT,
                            Focus::ResizeTopRight => ResizeEdge::TOP_RIGHT,
                            Focus::ResizeBottom => ResizeEdge::BOTTOM,
                            Focus::ResizeBottomLeft => ResizeEdge::BOTTOM_LEFT,
                            Focus::ResizeBottomRight => ResizeEdge::BOTTOM_RIGHT,
                            Focus::ResizeLeft => ResizeEdge::LEFT,
                            Focus::ResizeRight => ResizeEdge::RIGHT,
                            Focus::Header => unreachable!(),
                        },
                        state.common.config.cosmic_conf.edge_snap_threshold,
                        false,
                    );

                    if let Some((grab, focus)) = res {
                        if grab.is_touch_grab() {
                            seat.get_touch().unwrap().set_grab(state, grab, serial);
                        } else {
                            seat.get_pointer()
                                .unwrap()
                                .set_grab(state, grab, serial, focus);
                        }
                    }
                });
            }
            None => {}
        }
    }

    fn axis(&self, seat: &Seat<State>, data: &mut State, frame: AxisFrame) {
        if let Some(Focus::Header) = self.0.with_program(|p| p.current_focus()) {
            PointerTarget::axis(&self.0, seat, data, frame)
        }
    }

    fn frame(&self, seat: &Seat<State>, data: &mut State) {
        if let Some(Focus::Header) = self.0.with_program(|p| p.current_focus()) {
            PointerTarget::frame(&self.0, seat, data)
        }
    }

    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial, time: u32) {
        self.0.with_program(|p| {
            let cursor_state = seat.user_data().get::<CursorState>().unwrap();
            cursor_state.lock().unwrap().unset_shape();
            let _previous = p.swap_focus(None);
        });
        PointerTarget::leave(&self.0, seat, data, serial, time);

        // If focus_under() was NOT triggered for this window in the current motion cycle,
        // the pointer has truly left (jumped off-window). Clear hover immediately.
        // If focus_under() WAS triggered, the pointer moved from header→content (same window).
        let guard = POINTER_HOVERED_WINDOW.lock().unwrap();
        let still_tracked = guard.as_ref() == Some(self);
        let was_updated = POINTER_HOVER_UPDATED.load(Ordering::SeqCst);
        if !still_tracked || !was_updated {
            drop(guard);
            self.set_pointer_over_window(false);
            self.0.force_update();
        }
    }

    fn gesture_swipe_begin(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &GestureSwipeBeginEvent,
    ) {
    }

    fn gesture_swipe_update(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &GestureSwipeUpdateEvent,
    ) {
    }

    fn gesture_swipe_end(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &GestureSwipeEndEvent,
    ) {
    }

    fn gesture_pinch_begin(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &GesturePinchBeginEvent,
    ) {
    }

    fn gesture_pinch_update(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &GesturePinchUpdateEvent,
    ) {
    }

    fn gesture_pinch_end(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &GesturePinchEndEvent,
    ) {
    }

    fn gesture_hold_begin(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &GestureHoldBeginEvent,
    ) {
    }

    fn gesture_hold_end(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &GestureHoldEndEvent,
    ) {
    }
}

impl TouchTarget<State> for CosmicWindow {
    fn down(&self, seat: &Seat<State>, data: &mut State, event: &DownEvent) {
        let mut event = event.clone();
        self.0.with_program(|p| {
            event.location -= p.window.geometry().loc.to_f64();
        });
        TouchTarget::down(&self.0, seat, data, &event)
    }

    fn up(&self, seat: &Seat<State>, data: &mut State, event: &UpEvent) {
        TouchTarget::up(&self.0, seat, data, event)
    }

    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &TouchMotionEvent) {
        let mut event = event.clone();
        event.location -= self.0.with_program(|p| p.window.geometry().loc.to_f64());
        TouchTarget::motion(&self.0, seat, data, &event)
    }

    fn frame(&self, seat: &Seat<State>, data: &mut State, frame: FrameMarker) {
        TouchTarget::frame(&self.0, seat, data, frame)
    }

    fn cancel(&self, seat: &Seat<State>, data: &mut State, frame: FrameMarker) {
        TouchTarget::cancel(&self.0, seat, data, frame)
    }

    fn shape(&self, seat: &Seat<State>, data: &mut State, event: &ShapeEvent) {
        TouchTarget::shape(&self.0, seat, data, event)
    }

    fn orientation(&self, seat: &Seat<State>, data: &mut State, event: &OrientationEvent) {
        TouchTarget::orientation(&self.0, seat, data, event)
    }

    fn last_frame(&self, seat: &Seat<State>, data: &mut State) -> Option<FrameMarker> {
        TouchTarget::last_frame(&self.0, seat, data)
    }
}

impl WaylandFocus for CosmicWindow {
    fn wl_surface(&self) -> Option<Cow<'_, WlSurface>> {
        self.0.with_program(|p| {
            p.window
                .wl_surface()
                .map(|s| Cow::Owned(Cow::into_owned(s)))
        })
    }

    fn same_client_as(&self, object_id: &ObjectId) -> bool {
        self.0.with_program(|p| p.window.same_client_as(object_id))
    }
}

pub enum CosmicWindowRenderElement<R: AsGlowRenderer + ImportAll + ImportMem> {
    Header(IcedRenderElement<R>),
    Shadow(PixelShaderElement),
    Border(PixelShaderElement),
    Window(SurfaceRenderElement<R>),
}

impl<R: AsGlowRenderer + ImportAll + ImportMem> From<IcedRenderElement<R>>
    for CosmicWindowRenderElement<R>
{
    fn from(value: IcedRenderElement<R>) -> Self {
        Self::Header(value)
    }
}

impl<R: AsGlowRenderer + ImportAll + ImportMem> From<SurfaceRenderElement<R>>
    for CosmicWindowRenderElement<R>
{
    fn from(value: SurfaceRenderElement<R>) -> Self {
        Self::Window(value)
    }
}

impl<R> Element for CosmicWindowRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    R::TextureId: Send + 'static,
{
    fn id(&self) -> &RendererId {
        match self {
            CosmicWindowRenderElement::Header(elem) => elem.id(),
            CosmicWindowRenderElement::Shadow(elem) => elem.id(),
            CosmicWindowRenderElement::Border(elem) => elem.id(),
            CosmicWindowRenderElement::Window(elem) => elem.id(),
        }
    }

    fn current_commit(&self) -> CommitCounter {
        match self {
            CosmicWindowRenderElement::Header(elem) => elem.current_commit(),
            CosmicWindowRenderElement::Shadow(elem) => elem.current_commit(),
            CosmicWindowRenderElement::Border(elem) => elem.current_commit(),
            CosmicWindowRenderElement::Window(elem) => elem.current_commit(),
        }
    }

    fn src(&self) -> Rectangle<f64, Buffer> {
        match self {
            CosmicWindowRenderElement::Header(elem) => elem.src(),
            CosmicWindowRenderElement::Shadow(elem) => elem.src(),
            CosmicWindowRenderElement::Border(elem) => elem.src(),
            CosmicWindowRenderElement::Window(elem) => elem.src(),
        }
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        match self {
            CosmicWindowRenderElement::Header(elem) => elem.geometry(scale),
            CosmicWindowRenderElement::Shadow(elem) => elem.geometry(scale),
            CosmicWindowRenderElement::Border(elem) => elem.geometry(scale),
            CosmicWindowRenderElement::Window(elem) => elem.geometry(scale),
        }
    }

    fn location(&self, scale: Scale<f64>) -> Point<i32, Physical> {
        match self {
            CosmicWindowRenderElement::Header(elem) => elem.location(scale),
            CosmicWindowRenderElement::Shadow(elem) => elem.location(scale),
            CosmicWindowRenderElement::Border(elem) => elem.location(scale),
            CosmicWindowRenderElement::Window(elem) => elem.location(scale),
        }
    }

    fn transform(&self) -> Transform {
        match self {
            CosmicWindowRenderElement::Header(elem) => elem.transform(),
            CosmicWindowRenderElement::Shadow(elem) => elem.transform(),
            CosmicWindowRenderElement::Border(elem) => elem.transform(),
            CosmicWindowRenderElement::Window(elem) => elem.transform(),
        }
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<CommitCounter>,
    ) -> DamageSet<i32, Physical> {
        match self {
            CosmicWindowRenderElement::Header(elem) => elem.damage_since(scale, commit),
            CosmicWindowRenderElement::Shadow(elem) => elem.damage_since(scale, commit),
            CosmicWindowRenderElement::Border(elem) => elem.damage_since(scale, commit),
            CosmicWindowRenderElement::Window(elem) => elem.damage_since(scale, commit),
        }
    }

    fn opaque_regions(&self, scale: Scale<f64>) -> OpaqueRegions<i32, Physical> {
        match self {
            CosmicWindowRenderElement::Header(elem) => elem.opaque_regions(scale),
            CosmicWindowRenderElement::Shadow(elem) => elem.opaque_regions(scale),
            CosmicWindowRenderElement::Border(elem) => elem.opaque_regions(scale),
            CosmicWindowRenderElement::Window(elem) => elem.opaque_regions(scale),
        }
    }

    fn alpha(&self) -> f32 {
        match self {
            CosmicWindowRenderElement::Header(elem) => elem.alpha(),
            CosmicWindowRenderElement::Shadow(elem) => elem.alpha(),
            CosmicWindowRenderElement::Border(elem) => elem.alpha(),
            CosmicWindowRenderElement::Window(elem) => elem.alpha(),
        }
    }

    fn kind(&self) -> Kind {
        match self {
            CosmicWindowRenderElement::Header(elem) => elem.kind(),
            CosmicWindowRenderElement::Shadow(elem) => elem.kind(),
            CosmicWindowRenderElement::Border(elem) => elem.kind(),
            CosmicWindowRenderElement::Window(elem) => elem.kind(),
        }
    }

    fn is_framebuffer_effect(&self) -> bool {
        match self {
            CosmicWindowRenderElement::Header(elem) => elem.is_framebuffer_effect(),
            CosmicWindowRenderElement::Shadow(elem) => elem.is_framebuffer_effect(),
            CosmicWindowRenderElement::Border(elem) => elem.is_framebuffer_effect(),
            CosmicWindowRenderElement::Window(elem) => elem.is_framebuffer_effect(),
        }
    }
}

impl<R> RenderElement<R> for CosmicWindowRenderElement<R>
where
    R: AsGlowRenderer,
    R::TextureId: Send + 'static,
{
    fn draw(
        &self,
        frame: &mut <R>::Frame<'_, '_>,
        src: Rectangle<f64, Buffer>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
        opaque_regions: &[Rectangle<i32, Physical>],
        cache: Option<&UserDataMap>,
    ) -> Result<(), <R>::Error> {
        match self {
            CosmicWindowRenderElement::Header(elem) => {
                elem.draw(frame, src, dst, damage, opaque_regions, cache)
            }
            CosmicWindowRenderElement::Shadow(elem) | CosmicWindowRenderElement::Border(elem) => {
                RenderElement::<GlowRenderer>::draw(
                    elem,
                    R::glow_frame_mut(frame),
                    src,
                    dst,
                    damage,
                    opaque_regions,
                    cache,
                )
                .map_err(R::from_gles_error)
            }
            CosmicWindowRenderElement::Window(elem) => {
                elem.draw(frame, src, dst, damage, opaque_regions, cache)
            }
        }
    }

    fn underlying_storage(&self, renderer: &mut R) -> Option<UnderlyingStorage<'_>> {
        match self {
            CosmicWindowRenderElement::Header(elem) => elem.underlying_storage(renderer),
            CosmicWindowRenderElement::Shadow(elem) | CosmicWindowRenderElement::Border(elem) => {
                elem.underlying_storage(renderer.glow_renderer_mut())
            }
            CosmicWindowRenderElement::Window(elem) => elem.underlying_storage(renderer),
        }
    }

    fn capture_framebuffer(
        &self,
        frame: &mut <R>::Frame<'_, '_>,
        src: Rectangle<f64, Buffer>,
        dst: Rectangle<i32, Physical>,
        cache: &UserDataMap,
    ) -> Result<(), <R>::Error> {
        match self {
            CosmicWindowRenderElement::Header(elem) => {
                elem.capture_framebuffer(frame, src, dst, cache)
            }
            CosmicWindowRenderElement::Shadow(elem) | CosmicWindowRenderElement::Border(elem) => {
                RenderElement::<GlowRenderer>::capture_framebuffer(
                    elem,
                    R::glow_frame_mut(frame),
                    src,
                    dst,
                    cache,
                )
                .map_err(R::from_gles_error)
            }
            CosmicWindowRenderElement::Window(elem) => {
                elem.capture_framebuffer(frame, src, dst, cache)
            }
        }
    }
}
