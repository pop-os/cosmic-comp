// SPDX-License-Identifier: GPL-3.0-only

//! Real-time background blur implementation using Kawase blur algorithm.
//!
//! This module provides blur rendering for windows that request the KDE blur protocol.
//! The algorithm works by:
//! 1. Rendering background elements to an offscreen texture (excluding windows above the target)
//! 2. Optionally downsampling for performance (when `COSMIC_BLUR_DOWNSAMPLE=1`)
//! 3. Applying multiple Kawase blur iterations (ping-pong rendering)
//! 4. Caching blurred textures per-window for reuse when content hasn't changed
//! 5. Compositing the blurred result behind the target window
//!
//! # Architecture
//!
//! - `BlurRenderState`: Per-output state managing textures for blur passes
//! - `BlurredTextureInfo`: Cached blur result with metadata for invalidation
//! - `BlurCaptureContext`: Context passed through render pipeline for window exclusion
//! - Global caches: `BLUR_TEXTURE_CACHE` (per-window), `BLUR_GROUP_CONTENT_STATE` (invalidation)
//!
//! # Performance Optimizations
//!
//! - **Cache invalidation**: Tracks content hash via commit counters + geometry
//! - **Grouped captures**: Consecutive blur windows share a single capture
//! - **Optional downsampling**: Reduces blur texture size for less GPU work
//! - **Window exclusion**: Grabbed windows and windows above target are excluded via `BlurCaptureContext`

use smithay::{
    backend::{
        allocator::{Fourcc, dmabuf::Dmabuf},
        renderer::{
            Bind, Offscreen, Renderer,
            damage::OutputDamageTracker,
            element::{
                Element, Kind,
                texture::{TextureRenderBuffer, TextureRenderElement},
            },
            gles::{
                GlesComputeProgram, GlesError, GlesRenderer, GlesTexProgram, GlesTexture, Uniform,
                element::TextureShaderElement, ffi,
            },
        },
    },
    utils::{Logical, Physical, Point, Rectangle, Scale, Size, Transform},
};
use std::borrow::{Borrow, BorrowMut};
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::sync::{LazyLock, Mutex, RwLock};
use std::time::{Duration, Instant};
use wayland_backend::server::ObjectId;

use super::element::AsGlowRenderer;
use crate::shell::element::{CosmicMapped, CosmicMappedKey};
// For `CosmicMappedKey::alive` and `WlSurface::alive`, which is how a cached
// blur texture learns its owner is gone (see `reap_dead_blur_textures`).
use smithay::reexports::wayland_server::{Resource, protocol::wl_surface::WlSurface};
use smithay::utils::IsAlive;

// =============================================================================
// Constants
// =============================================================================

/// Downsample factor for blur textures.
/// Factor 2 gives 720×450 from 1440×900. A 2× bilinear downsample is nearly
/// lossless (no aliasing). The Dual Kawase algorithm then progressively halves
/// from there, blurring at each step to avoid the artifacts of a large single-step
/// downsample.
pub const BLUR_DOWNSAMPLE_FACTOR: i32 = 2;

// Blur backdrop styling constants
pub const BLUR_TINT_COLOR: [f32; 3] = [1.0, 1.0, 1.0];
/// Strength of the tint overlay (0.30 = 30% opacity)
pub const BLUR_TINT_STRENGTH: f32 = 0.15;
/// Default frosted-glass border alpha (0.0 = no border). Per-surface overridable
/// via the blur protocol's `set_border`.
pub const BLUR_BORDER_STRENGTH: f32 = 0.2;
/// Alpha for fallback solid color when blur texture not available
pub const BLUR_FALLBACK_ALPHA: f32 = 0.95;
/// Fallback color when blur texture not available (light gray-blue)
pub const BLUR_FALLBACK_COLOR: [f32; 3] = [0.9, 0.9, 0.95];

/// Minimum interval between blur updates when only content changes (e.g., cursor blink).
/// This throttles blur re-computation to reduce GPU load from high-frequency updates.
pub fn blur_throttle_interval() -> Duration {
    Duration::from_millis(100)
}

// =============================================================================
// Runtime Blur Configuration (updated from cosmic-config)
// =============================================================================

/// Global blur enabled flag, updated from config.
static BLUR_ENABLED: AtomicBool = AtomicBool::new(true);
/// Global blur intensity as f32 bits, updated from config. Range 0.0-1.0.
static BLUR_INTENSITY_BITS: AtomicU32 = AtomicU32::new(0); // initialized in init_blur_config

/// Initialize blur config globals from CosmicCompConfig values.
pub fn init_blur_config(enabled: bool, intensity: f32) {
    set_blur_enabled(enabled);
    set_blur_intensity(intensity);
}

/// Update blur enabled state from config.
pub fn set_blur_enabled(enabled: bool) {
    BLUR_ENABLED.store(enabled, Ordering::Relaxed);
}

/// Update blur intensity from config (0.0 = disabled, 1.0 = maximum).
pub fn set_blur_intensity(intensity: f32) {
    let clamped = intensity.clamp(0.0, 1.0);
    BLUR_INTENSITY_BITS.store(clamped.to_bits(), Ordering::Relaxed);
}

/// Check if blur is enabled via config.
pub fn blur_config_enabled() -> bool {
    BLUR_ENABLED.load(Ordering::Relaxed)
}

/// Get the configured blur intensity (0.0-1.0).
pub fn blur_config_intensity() -> f32 {
    f32::from_bits(BLUR_INTENSITY_BITS.load(Ordering::Relaxed))
}

/// Map blur intensity (0.0-1.0) to Dual Kawase levels (1-6).
pub fn effective_blur_levels() -> u32 {
    let intensity = blur_config_intensity();
    // Linear map: 0.0 → 1 level, 1.0 → 6 levels
    let levels = 1.0 + intensity * 5.0;
    (levels.round() as u32).clamp(1, 6)
}

/// Map blur intensity (0.0-1.0) to Dual Kawase offset (0.5-5.0).
pub fn effective_blur_offset() -> f32 {
    let intensity = blur_config_intensity();
    // Linear map: 0.0 → 0.5, 1.0 → 5.0
    0.5 + intensity * 4.5
}

// -----------------------------------------------------------------------------
// Per-surface blur radius → intensity (org_kde_kwin_blur set_radius)
// -----------------------------------------------------------------------------
//
// A client can request a custom blur radius (in pixels) per surface via the blur
// protocol. We map that radius onto the SAME 0.0..1.0 intensity axis the global
// config slider uses, so a custom radius is just an alternate way to pick a point
// on the existing levels/offset curve. Surfaces that don't request a radius keep
// using the global config intensity (see `resolve_blur_params`).

/// Lower bound of the radius→intensity map: ~1px ⇒ near-zero blur (1 level).
pub const BLUR_RADIUS_MIN_PX: f32 = 1.0;
/// Upper bound of the radius→intensity map: ~100px ⇒ full blur (6 levels / 5.0
/// offset). Larger client values clamp here. (The design's `blur(40px)` lands
/// partway up the curve, leaving headroom for stronger blur.)
pub const BLUR_RADIUS_MAX_PX: f32 = 100.0;

/// Map a per-surface pixel radius to the 0.0..1.0 intensity domain, clamped.
/// Non-finite values (NaN/±inf from a malformed client) and ≤MIN map to 0.0 so
/// they can never produce a NaN offset (which would be fatal to the blur shader).
fn radius_to_intensity(radius_px: f32) -> f32 {
    if !radius_px.is_finite() {
        return 0.0;
    }
    let t = (radius_px - BLUR_RADIUS_MIN_PX) / (BLUR_RADIUS_MAX_PX - BLUR_RADIUS_MIN_PX);
    t.clamp(0.0, 1.0)
}

/// Map a per-surface blur radius (pixels) to Dual Kawase levels (1-6).
/// Same curve shape as [`effective_blur_levels`], driven by the radius.
pub fn blur_levels_for_radius(radius_px: f32) -> u32 {
    let intensity = radius_to_intensity(radius_px);
    let levels = 1.0 + intensity * 5.0;
    (levels.round() as u32).clamp(1, 6)
}

/// Map a per-surface blur radius (pixels) to Dual Kawase offset (0.5-5.0).
/// Same curve shape as [`effective_blur_offset`], driven by the radius.
pub fn blur_offset_for_radius(radius_px: f32) -> f32 {
    let intensity = radius_to_intensity(radius_px);
    0.5 + intensity * 4.5
}

/// Resolve the `(levels, offset)` Dual Kawase parameters for an optional
/// per-surface radius. `None` ⇒ the global config intensity (unchanged
/// behaviour for surfaces that don't opt in); `Some(px)` ⇒ radius-derived.
/// Single source of truth so every blur call site stays consistent.
pub fn resolve_blur_params(radius_px: Option<f32>) -> (u32, f32) {
    match radius_px {
        Some(r) => (blur_levels_for_radius(r), blur_offset_for_radius(r)),
        None => (effective_blur_levels(), effective_blur_offset()),
    }
}

// =============================================================================
// Environment Configuration
// =============================================================================

/// Check if blur texture downsampling is enabled via env var.
/// Downsampling is enabled by default for strong CSS-like blur effect.
/// Set `COSMIC_BLUR_DOWNSAMPLE=0` to disable (weaker blur, more GPU work).
pub fn blur_downsample_enabled() -> bool {
    crate::utils::env::bool_var("COSMIC_BLUR_DOWNSAMPLE").unwrap_or(true)
}

// =============================================================================
// Blur Render State
// =============================================================================

/// State for managing blur effect rendering per-output.
///
/// Blur requires rendering background content to an offscreen texture,
/// applying multiple Kawase blur passes (ping-pong), then compositing
/// the result behind the target surface.
///
/// Optimization: Blur passes can run at reduced resolution (1/BLUR_DOWNSAMPLE_FACTOR)
/// since blur removes high-frequency detail anyway.
///
/// Note: Window blur and layer blur have SEPARATE ping/pong textures to avoid
/// cache pollution. Window blur uses texture_a/texture_b, layer blur uses
/// layer_texture_a/layer_texture_b. This is necessary because cached blur
/// textures are references to these buffers, not copies.
#[derive(Debug)]
pub struct BlurRenderState {
    /// Texture containing the previous frame's content at full resolution (blur source)
    pub background_texture: Option<TextureRenderBuffer<GlesTexture>>,
    /// Downsampled texture for blur input (from background_texture)
    pub downsampled_texture: Option<TextureRenderBuffer<GlesTexture>>,
    /// Offscreen texture for window blur passes (ping) - at reduced resolution
    pub texture_a: Option<TextureRenderBuffer<GlesTexture>>,
    /// Offscreen texture for window blur passes (pong) - at reduced resolution
    pub texture_b: Option<TextureRenderBuffer<GlesTexture>>,
    /// Offscreen texture for layer blur passes (ping) - at reduced resolution
    /// Separate from texture_a to avoid cache pollution between window and layer blur
    pub layer_texture_a: Option<TextureRenderBuffer<GlesTexture>>,
    /// Offscreen texture for layer blur passes (pong) - at reduced resolution
    /// Separate from texture_b to avoid cache pollution between window and layer blur
    pub layer_texture_b: Option<TextureRenderBuffer<GlesTexture>>,
    /// Damage tracker for the blur textures
    pub damage_tracker: Option<OutputDamageTracker>,
    /// Size of the full-resolution background texture (screen size)
    pub screen_size: Size<i32, Physical>,
    /// Size of the downsampled blur textures
    pub texture_size: Size<i32, Physical>,
    /// Scale factor
    pub scale: Scale<f64>,
    /// Whether blur has been applied this frame
    pub blur_applied: bool,
    /// Whether texture allocation has permanently failed (don't retry)
    pub allocation_failed: bool,
}

impl Default for BlurRenderState {
    fn default() -> Self {
        Self {
            background_texture: None,
            downsampled_texture: None,
            texture_a: None,
            texture_b: None,
            layer_texture_a: None,
            layer_texture_b: None,
            damage_tracker: None,
            screen_size: Size::from((0, 0)),
            texture_size: Size::from((0, 0)),
            scale: Scale::from(1.0),
            blur_applied: false,
            allocation_failed: false,
        }
    }
}

impl BlurRenderState {
    /// Internal helper to create all blur textures with a specific format.
    /// Returns Ok(()) on success, or Err with the error on failure.
    fn create_textures_with_format<R: AsGlowRenderer + Offscreen<GlesTexture>>(
        &mut self,
        renderer: &mut R,
        format: Fourcc,
        size: Size<i32, Physical>,
        scale: Scale<f64>,
        downsample_enabled: bool,
        blur_size: Size<i32, Physical>,
    ) -> Result<(), R::Error> {
        // Full-size background texture (stores previous frame for blur source)
        let full_buffer_size = size.to_logical(1).to_buffer(1, Transform::Normal);
        let bg_tex = Offscreen::<GlesTexture>::create_buffer(renderer, format, full_buffer_size)?;
        self.background_texture = Some(TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            bg_tex,
            1,
            Transform::Normal,
            None,
        ));

        // Downsampled buffer size for blur passes
        let blur_buffer_size = blur_size.to_logical(1).to_buffer(1, Transform::Normal);

        // Downsampled texture (intermediate for downsample) - only when downsampling enabled
        if downsample_enabled {
            let ds_tex =
                Offscreen::<GlesTexture>::create_buffer(renderer, format, blur_buffer_size)?;
            self.downsampled_texture = Some(TextureRenderBuffer::from_texture(
                renderer.glow_renderer(),
                ds_tex,
                1,
                Transform::Normal,
                None,
            ));
        } else {
            self.downsampled_texture = None;
        }

        // Create texture A (ping) - at blur resolution (reduced or full)
        let tex_a = Offscreen::<GlesTexture>::create_buffer(renderer, format, blur_buffer_size)?;
        self.texture_a = Some(TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            tex_a,
            1,
            Transform::Normal,
            None,
        ));

        // Create texture B (pong) - at reduced resolution
        let tex_b = Offscreen::<GlesTexture>::create_buffer(renderer, format, blur_buffer_size)?;
        self.texture_b = Some(TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            tex_b,
            1,
            Transform::Normal,
            None,
        ));

        // Create layer texture A (ping) - separate from window blur to avoid cache pollution
        let layer_tex_a =
            Offscreen::<GlesTexture>::create_buffer(renderer, format, blur_buffer_size)?;
        self.layer_texture_a = Some(TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            layer_tex_a,
            1,
            Transform::Normal,
            None,
        ));

        // Create layer texture B (pong) - separate from window blur to avoid cache pollution
        let layer_tex_b =
            Offscreen::<GlesTexture>::create_buffer(renderer, format, blur_buffer_size)?;
        self.layer_texture_b = Some(TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            layer_tex_b,
            1,
            Transform::Normal,
            None,
        ));

        // Create damage tracker
        self.damage_tracker = Some(OutputDamageTracker::new(size, scale, Transform::Normal));

        self.screen_size = size;
        self.texture_size = blur_size;
        self.scale = scale;
        self.blur_applied = false;
        Ok(())
    }

    /// Create or resize blur textures if needed.
    /// Creates full-size background texture and optionally downsampled blur textures.
    ///
    /// If the requested format is not supported (e.g., 10-bit formats on software renderers),
    /// this will automatically fall back to Argb8888.
    ///
    /// Returns `Ok(true)` if textures are ready, `Ok(false)` if allocation was previously
    /// marked as failed (skipped), or `Err` on new failure.
    pub fn ensure_textures<R: AsGlowRenderer + Offscreen<GlesTexture>>(
        &mut self,
        renderer: &mut R,
        format: Fourcc,
        size: Size<i32, Physical>,
        scale: Scale<f64>,
    ) -> Result<bool, R::Error> {
        // If allocation has previously failed permanently, skip without error
        if self.allocation_failed {
            return Ok(false);
        }

        // Remap a BGRA-ordered 10-bit scanout format (AR30/XR30) to the
        // GLES-renderable equivalent before allocating; the Argb8888 retry below
        // still covers any other unsupported format. See `offscreen_render_format`.
        let format = super::offscreen_render_format(format);

        let downsample_enabled = blur_downsample_enabled();

        // Calculate blur size (downsampled if enabled, full size otherwise)
        let downsample_factor =
            crate::backend::render::gpu_profiler::effective_blur_downsample_factor();
        let blur_size: Size<i32, Physical> = if downsample_enabled {
            Size::from((
                (size.w / downsample_factor).max(1),
                (size.h / downsample_factor).max(1),
            ))
        } else {
            size
        };

        // Only recreate if size changed
        let textures_valid = if downsample_enabled {
            self.screen_size == size
                && self.texture_a.is_some()
                && self.texture_b.is_some()
                && self.layer_texture_a.is_some()
                && self.layer_texture_b.is_some()
                && self.background_texture.is_some()
                && self.downsampled_texture.is_some()
        } else {
            self.screen_size == size
                && self.texture_a.is_some()
                && self.texture_b.is_some()
                && self.layer_texture_a.is_some()
                && self.layer_texture_b.is_some()
                && self.background_texture.is_some()
        };

        if textures_valid {
            return Ok(true);
        }

        tracing::debug!(
            screen_w = size.w,
            screen_h = size.h,
            blur_w = blur_size.w,
            blur_h = blur_size.h,
            downsample_enabled = downsample_enabled,
            "Creating blur textures"
        );

        // Try to create textures with the requested format first
        match self.create_textures_with_format(
            renderer,
            format,
            size,
            scale,
            downsample_enabled,
            blur_size,
        ) {
            Ok(()) => Ok(true),
            Err(err) => {
                // Check if this is an unsupported pixel format error
                let err_str = format!("{:?}", err);
                if err_str.contains("UnsupportedPixelFormat") && format != Fourcc::Argb8888 {
                    // Clear any partially created textures before retry
                    self.background_texture = None;
                    self.downsampled_texture = None;
                    self.texture_a = None;
                    self.texture_b = None;
                    self.layer_texture_a = None;
                    self.layer_texture_b = None;
                    self.damage_tracker = None;

                    tracing::info!(
                        ?format,
                        "Blur texture format not supported, falling back to Argb8888"
                    );

                    // Try again with Argb8888 as fallback (universally supported)
                    self.create_textures_with_format(
                        renderer,
                        Fourcc::Argb8888,
                        size,
                        scale,
                        downsample_enabled,
                        blur_size,
                    )?;
                    return Ok(true);
                }
                // Other errors are returned as-is
                Err(err)
            }
        }
    }

    /// Get background texture (previous frame content at full resolution)
    pub fn background_texture(&self) -> Option<&TextureRenderBuffer<GlesTexture>> {
        self.background_texture.as_ref()
    }

    /// Get downsampled texture
    pub fn downsampled_texture(&self) -> Option<&TextureRenderBuffer<GlesTexture>> {
        self.downsampled_texture.as_ref()
    }

    /// Get texture A for rendering (at reduced resolution)
    pub fn texture_a(&self) -> Option<&TextureRenderBuffer<GlesTexture>> {
        self.texture_a.as_ref()
    }

    /// Get texture B for rendering (at reduced resolution)
    pub fn texture_b(&self) -> Option<&TextureRenderBuffer<GlesTexture>> {
        self.texture_b.as_ref()
    }

    /// Check if blur state is ready for rendering
    pub fn is_ready(&self) -> bool {
        let base_ready = self.background_texture.is_some()
            && self.texture_a.is_some()
            && self.texture_b.is_some()
            && self.layer_texture_a.is_some()
            && self.layer_texture_b.is_some();

        if blur_downsample_enabled() {
            base_ready && self.downsampled_texture.is_some()
        } else {
            base_ready
        }
    }
}

// =============================================================================
// Blur Texture Cache
// =============================================================================

/// Blurred texture data for a window
#[derive(Debug, Clone)]
pub struct BlurredTextureInfo {
    /// The blurred texture buffer
    pub texture: TextureRenderBuffer<GlesTexture>,
    /// Size of the blurred texture (may be downsampled)
    pub size: Size<i32, Physical>,
    /// Original screen size (for coordinate mapping when downsampled)
    pub screen_size: Size<i32, Physical>,
    /// Scale factor
    pub scale: Scale<f64>,
    /// Hash of background state for cache invalidation.
    /// Changes when windows below move/resize or z-order changes.
    pub background_state_hash: u64,
    /// Logical `(w, h)` of the surface region this texture was captured for.
    /// `background_state_hash` only tracks what's *behind* the surface, so this
    /// is compared separately to force a re-blur when the surface itself resizes
    /// (otherwise the stale, smaller texture is stretched over the new area,
    /// flashing the unblurred desktop for a frame).
    pub capture_size: (i32, i32),
    /// Whether the captured scene covered the whole output with opaque content.
    ///
    /// A blur backdrop paints `blurred.a * blur_opacity * mask`, so when the
    /// capture was fully opaque a settled, square-cornered, alpha-1.0 window
    /// paints alpha 1 over its whole geometry -- it occludes everything below it
    /// even though it declares no opaque region to the damage tracker.
    ///
    /// Measured rather than assumed: a desktop with no opaque base (no wallpaper,
    /// or one with alpha) leaves this false everywhere and the blur work below
    /// such a window goes on being done.
    pub capture_was_opaque: bool,
}

/// Cache key combining output name and the window's identity key.
///
/// `CosmicMappedKey` holds a `Weak`, so a cached entry can be asked whether its
/// window still exists and reaped once it does not — see
/// [`reap_dead_blur_textures`]. This used to key on a hash of the `Weak`'s raw
/// pointer, which both leaked (nothing could tell a dead entry from a live one)
/// and risked a freed window's address being reused by a new window that then
/// inherited its blurred backdrop.
type BlurCacheKey = (String, CosmicMappedKey);

/// Global cache of blurred textures per window per output.
/// For iterative rendering: each blur window has its own cached texture
/// capturing everything behind it (including other blurred windows below).
pub static BLUR_TEXTURE_CACHE: LazyLock<RwLock<HashMap<BlurCacheKey, BlurredTextureInfo>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Cache key for blur group content state: (output_name, capture_z_threshold)
type BlurGroupContentKey = (String, usize);

/// Tracks the last known content state for blur groups.
/// Used to detect when elements have changed (via their commit counter).
pub static BLUR_GROUP_CONTENT_STATE: LazyLock<RwLock<HashMap<BlurGroupContentKey, u64>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Tracks the last time blur was computed for each group.
/// Used for throttling blur updates when content changes frequently.
pub static BLUR_GROUP_LAST_UPDATE: LazyLock<RwLock<HashMap<BlurGroupContentKey, Instant>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Store a blurred texture for a specific window on an output.
/// This is used for iterative multi-pass blur where each blur window
/// gets its own texture capturing everything behind it.
pub fn cache_blur_texture_for_window(
    output_name: &str,
    window_key: &CosmicMappedKey,
    info: BlurredTextureInfo,
) {
    let key = (output_name.to_string(), window_key.clone());
    if let Ok(mut cache) = BLUR_TEXTURE_CACHE.write() {
        cache.insert(key, info);
    }
}

/// Get the cached blurred texture for a specific window on an output.
pub fn get_cached_blur_texture_for_window(
    output_name: &str,
    window_key: &CosmicMappedKey,
) -> Option<BlurredTextureInfo> {
    let key = (output_name.to_string(), window_key.clone());
    if let Ok(cache) = BLUR_TEXTURE_CACHE.read() {
        cache.get(&key).cloned()
    } else {
        None
    }
}

/// Clear all blur textures for an output (e.g., on window close)
pub fn clear_blur_textures_for_output(output_name: &str) {
    if let Ok(mut cache) = BLUR_TEXTURE_CACHE.write() {
        cache.retain(|(out, _), _| out != output_name);
    }
}

// =============================================================================
// Layer Surface Blur Cache
// =============================================================================

/// Cache key for layer surface blur: (output_name, surface_id)
type LayerBlurCacheKey = (String, ObjectId);

/// Global cache of blurred textures for layer surfaces per output.
pub static LAYER_BLUR_TEXTURE_CACHE: LazyLock<
    RwLock<HashMap<LayerBlurCacheKey, BlurredTextureInfo>>,
> = LazyLock::new(|| RwLock::new(HashMap::new()));

/// Store a blurred texture for a specific layer surface on an output.
pub fn cache_blur_texture_for_layer(
    output_name: &str,
    surface_id: ObjectId,
    info: BlurredTextureInfo,
) {
    let key = (output_name.to_string(), surface_id);
    if let Ok(mut cache) = LAYER_BLUR_TEXTURE_CACHE.write() {
        cache.insert(key, info);
    }
}

/// Get the cached blurred texture for a specific layer surface on an output.
pub fn get_cached_blur_texture_for_layer(
    output_name: &str,
    surface_id: &ObjectId,
) -> Option<BlurredTextureInfo> {
    let key = (output_name.to_string(), surface_id.clone());
    if let Ok(cache) = LAYER_BLUR_TEXTURE_CACHE.read() {
        cache.get(&key).cloned()
    } else {
        None
    }
}

/// Clear all layer blur textures for an output
pub fn clear_layer_blur_textures_for_output(output_name: &str) {
    if let Ok(mut cache) = LAYER_BLUR_TEXTURE_CACHE.try_write() {
        cache.retain(|(out, _), _| out != output_name);
    }
}

/// Drop the cached blur texture for a destroyed layer surface, on every output.
///
/// Layer surfaces come and go for the whole session (a panel or launcher
/// re-creating its surface, a menu opening and closing), and each one that was
/// blurred owns a full-output-sized GPU texture in here. Without this the cache
/// only ever shrank when an entire output was removed, so those textures
/// accumulated for as long as the compositor ran. Keyed across outputs because a
/// surface may have been blurred on more than one.
pub fn clear_layer_blur_texture_for_surface(surface_id: &ObjectId) {
    if let Ok(mut cache) = LAYER_BLUR_TEXTURE_CACHE.write() {
        let before = cache.len();
        cache.retain(|(_, id), _| id != surface_id);
        let removed = before - cache.len();
        if removed > 0 {
            tracing::debug!(
                removed,
                remaining = cache.len(),
                "dropped layer blur textures for destroyed surface"
            );
        }
    }
}

/// Live handles for the surfaces owning entries in [`LAYER_BLUR_TEXTURE_CACHE`].
///
/// That cache is keyed by `ObjectId`, which cannot answer "does this surface
/// still exist?". Keeping the `WlSurface` beside it can. Cloning a `WlSurface`
/// does not keep the client's object alive — it only lets us ask.
static LAYER_BLUR_SURFACE_HANDLES: LazyLock<RwLock<HashMap<ObjectId, WlSurface>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Remember the surface behind a blurred layer entry, so its texture can be
/// reclaimed once the surface dies.
///
/// Call this wherever a surface joins the blur set (see `update_layer_blur_state`).
/// It is the only way popups become reclaimable: a layer-shell popup is blurred
/// under its OWN surface id, but popups never reach `layer_destroyed`, so the
/// per-surface hook there — which only ever sees the parent layer surface — can
/// never match them.
pub fn record_layer_blur_surface(surface: &WlSurface) {
    if let Ok(mut handles) = LAYER_BLUR_SURFACE_HANDLES.write() {
        handles
            .entry(surface.id())
            .or_insert_with(|| surface.clone());
    }
}

/// Drop cached window blur textures whose window no longer exists.
///
/// The window cache has no destroy hook to hang off — a window can go away via
/// xdg, X11, or the client simply disconnecting — so instead of chasing every
/// path we ask each key's `Weak` whether its window is still alive. Cheap: the
/// map holds one entry per *blurred* window per output, and this only takes the
/// write lock when it can get it uncontended, so a render thread mid-frame is
/// never blocked. Throttled, since `refresh` runs every loop iteration.
pub fn reap_dead_blur_textures() {
    static LAST_REAP: Mutex<Option<Instant>> = Mutex::new(None);

    {
        let Ok(mut last) = LAST_REAP.lock() else {
            return;
        };
        let now = Instant::now();
        if last.is_some_and(|t| now.duration_since(t) < Duration::from_secs(1)) {
            return;
        }
        *last = Some(now);
    }

    if let Ok(mut cache) = BLUR_TEXTURE_CACHE.try_write() {
        let before = cache.len();
        cache.retain(|(_, key), _| key.alive());
        let removed = before - cache.len();
        if removed > 0 {
            tracing::debug!(
                removed,
                remaining = cache.len(),
                "reaped blur textures for closed windows"
            );
        }
    }

    // Layer surfaces. Popups are the real churn here: every blurred menu open
    // mints a fresh `wl_surface`, and popups never reach `layer_destroyed`, so
    // `clear_layer_blur_texture_for_surface` cannot reclaim them — it only ever
    // sees the parent layer surface's id. Each stranded entry pins a
    // full-output-sized GPU texture, so this sweep is what keeps opening menus
    // from leaking GPU memory for the life of the session.
    let dead: HashSet<ObjectId> = match LAYER_BLUR_SURFACE_HANDLES.read() {
        Ok(handles) => handles
            .iter()
            .filter(|(_, surface)| !surface.alive())
            .map(|(id, _)| id.clone())
            .collect(),
        Err(_) => HashSet::new(),
    };
    if dead.is_empty() {
        return;
    }

    // Only forget the handles if the textures actually went with them, so a
    // contended `try_write` retries next tick instead of stranding the entries
    // permanently.
    let mut swept = false;
    if let Ok(mut cache) = LAYER_BLUR_TEXTURE_CACHE.try_write() {
        let before = cache.len();
        cache.retain(|(_, id), _| !dead.contains(id));
        swept = true;
        let removed = before - cache.len();
        if removed > 0 {
            tracing::debug!(
                removed,
                remaining = cache.len(),
                "reaped layer blur textures for dead surfaces"
            );
        }
    }
    if swept && let Ok(mut handles) = LAYER_BLUR_SURFACE_HANDLES.write() {
        handles.retain(|id, _| !dead.contains(id));
    }
}

/// Invalidate all blur caches (window + layer + content state).
/// Called when blur intensity changes to force re-computation.
pub fn invalidate_all_blur_caches() {
    if let Ok(mut cache) = BLUR_TEXTURE_CACHE.write() {
        cache.clear();
    }
    if let Ok(mut cache) = LAYER_BLUR_TEXTURE_CACHE.write() {
        cache.clear();
    }
    if let Ok(mut cache) = BLUR_GROUP_CONTENT_STATE.write() {
        cache.clear();
    }
}

/// Information about a layer surface with blur, cached from the main thread.
#[derive(Debug, Clone)]
pub struct LayerBlurSurfaceInfo {
    /// Surface ObjectId for cache key
    pub surface_id: ObjectId,
    /// Geometry of the layer surface
    pub geometry: Rectangle<i32, Logical>,
    /// Layer type (Bottom, Top, Overlay, Background)
    pub layer: smithay::wayland::shell::wlr_layer::Layer,
    /// Custom blur radius in pixels, or None for compositor default.
    pub blur_radius: Option<f32>,
}

/// Cached layer surface info for rendering (includes the actual LayerSurface).
/// Used by render thread to avoid calling layer_map_for_output.
#[derive(Clone)]
pub struct CachedLayerSurface {
    /// The layer surface (cloned for thread safety)
    pub surface: smithay::desktop::LayerSurface,
    /// Location relative to output
    pub location: Point<i32, Logical>,
}

/// Convert Layer enum to a hashable u8 key
fn layer_to_key(layer: smithay::wayland::shell::wlr_layer::Layer) -> u8 {
    use smithay::wayland::shell::wlr_layer::Layer;
    match layer {
        Layer::Background => 0,
        Layer::Bottom => 1,
        Layer::Top => 2,
        Layer::Overlay => 3,
    }
}

/// Cache tracking ALL layer surfaces per output, organized by layer type.
/// Updated from the main thread when layer surfaces change, read by render thread.
/// Key: (output_name, layer_key), Value: list of cached layer surfaces
pub static OUTPUT_LAYER_SURFACES: LazyLock<RwLock<HashMap<(String, u8), Vec<CachedLayerSurface>>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Get cached layer surfaces for a specific layer on an output (non-blocking).
/// Returns empty vec if lock can't be acquired.
pub fn get_cached_layer_surfaces(
    output_name: &str,
    layer: smithay::wayland::shell::wlr_layer::Layer,
) -> Vec<CachedLayerSurface> {
    if let Ok(cache) = OUTPUT_LAYER_SURFACES.try_read() {
        cache
            .get(&(output_name.to_string(), layer_to_key(layer)))
            .cloned()
            .unwrap_or_default()
    } else {
        Vec::new()
    }
}

/// Update the cached layer surfaces for a specific layer on an output.
/// Called from the main thread when layer surfaces change.
pub fn set_cached_layer_surfaces(
    output_name: &str,
    layer: smithay::wayland::shell::wlr_layer::Layer,
    surfaces: Vec<CachedLayerSurface>,
) {
    if let Ok(mut cache) = OUTPUT_LAYER_SURFACES.try_write() {
        let key = (output_name.to_string(), layer_to_key(layer));
        if surfaces.is_empty() {
            cache.remove(&key);
        } else {
            cache.insert(key, surfaces);
        }
    }
}

/// Clear all cached layer surfaces for an output.
pub fn clear_cached_layer_surfaces(output_name: &str) {
    if let Ok(mut cache) = OUTPUT_LAYER_SURFACES.try_write() {
        cache.retain(|(out, _), _| out != output_name);
    }
}

/// Cache tracking layer surfaces with blur for each output.
/// Updated from the main thread when layer surfaces change, read by render thread.
/// Key: output_name, Value: list of layer blur surface info
pub static OUTPUT_LAYER_BLUR_SURFACES: LazyLock<
    RwLock<HashMap<String, Vec<LayerBlurSurfaceInfo>>>,
> = LazyLock::new(|| RwLock::new(HashMap::new()));

/// Check if an output has any layer surfaces with blur (non-blocking).
/// Returns false if the lock can't be acquired (safe default for render thread).
pub fn output_has_layer_blur(output_name: &str) -> bool {
    if let Ok(cache) = OUTPUT_LAYER_BLUR_SURFACES.try_read() {
        cache
            .get(output_name)
            .map(|v| !v.is_empty())
            .unwrap_or(false)
    } else {
        false
    }
}

/// Get cached layer blur surface info for an output (non-blocking).
/// Returns empty vec if lock can't be acquired.
pub fn get_layer_blur_surfaces(output_name: &str) -> Vec<LayerBlurSurfaceInfo> {
    if let Ok(cache) = OUTPUT_LAYER_BLUR_SURFACES.try_read() {
        cache.get(output_name).cloned().unwrap_or_default()
    } else {
        Vec::new()
    }
}

/// Update the layer blur surfaces for an output.
/// Called from the main thread when layer surfaces are added/removed/changed.
pub fn set_layer_blur_surfaces(output_name: &str, surfaces: Vec<LayerBlurSurfaceInfo>) {
    if let Ok(mut cache) = OUTPUT_LAYER_BLUR_SURFACES.try_write() {
        if surfaces.is_empty() {
            cache.remove(output_name);
        } else {
            cache.insert(output_name.to_string(), surfaces);
        }
    }
}

/// Cache for layer blur content state: output_name -> content_hash
/// Used to detect when background content has changed and blur needs re-rendering
pub static LAYER_BLUR_CONTENT_STATE: LazyLock<RwLock<HashMap<String, u64>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Get the stored content hash for layer blur on an output
pub fn get_layer_blur_content_hash(output_name: &str) -> Option<u64> {
    if let Ok(cache) = LAYER_BLUR_CONTENT_STATE.read() {
        cache.get(output_name).copied()
    } else {
        None
    }
}

/// Store the content hash for layer blur after rendering
pub fn store_layer_blur_content_hash(output_name: &str, hash: u64) {
    if let Ok(mut cache) = LAYER_BLUR_CONTENT_STATE.write() {
        cache.insert(output_name.to_string(), hash);
    }
}

// =============================================================================
// Content Hash for Cache Invalidation
// =============================================================================

/// Compute a hash of element content state (commit counters + geometry).
/// Commit counters change when surfaces commit new content (terminal updates, etc.)
///
/// NOTE: We intentionally DO NOT include element IDs in the hash because some
/// render elements (like shadows, decorations) get recreated each frame with
/// new ExternalIds. We rely on commit counters + geometry for change detection.
/// Per-element commit fingerprints of the last blur, for diagnosing invalidation.
///
/// The content hash collapses the whole capture into one number, which answers
/// "did anything change" but never "what". This keeps the inputs so an
/// invalidation can be attributed to a specific surface.
static BLUR_GROUP_ELEMENT_COMMITS: LazyLock<
    RwLock<HashMap<BlurGroupContentKey, Vec<(String, String)>>>,
> = LazyLock::new(|| RwLock::new(HashMap::new()));

/// Records this capture's fingerprints and returns what changed since the last
/// one: `(changed, added, removed)` element identities.
///
/// Only for diagnostics -- it allocates per element, so callers must gate it on
/// performance logging being enabled.
pub fn diff_blur_group_elements<E: Element>(
    output_name: &str,
    z_threshold: usize,
    elements: &[E],
    scale: Scale<f64>,
) -> (Vec<String>, Vec<String>, Vec<String>) {
    let current: Vec<(String, String)> = elements
        .iter()
        .map(|e| {
            (
                format!("{:?}", e.id()),
                format!("{:?}|{:?}", e.current_commit(), e.geometry(scale)),
            )
        })
        .collect();

    let key = (output_name.to_string(), z_threshold);
    let previous = BLUR_GROUP_ELEMENT_COMMITS
        .read()
        .unwrap()
        .get(&key)
        .cloned();
    BLUR_GROUP_ELEMENT_COMMITS
        .write()
        .unwrap()
        .insert(key, current.clone());

    let Some(previous) = previous else {
        return (
            Vec::new(),
            current.iter().map(|(id, _)| id.clone()).collect(),
            Vec::new(),
        );
    };

    let prev_map: HashMap<&str, &str> = previous
        .iter()
        .map(|(id, c)| (id.as_str(), c.as_str()))
        .collect();
    let cur_ids: std::collections::HashSet<&str> =
        current.iter().map(|(id, _)| id.as_str()).collect();

    let mut changed = Vec::new();
    let mut added = Vec::new();
    for (id, commit) in &current {
        match prev_map.get(id.as_str()) {
            Some(prev_commit) if prev_commit != &commit.as_str() => changed.push(id.clone()),
            Some(_) => {}
            None => added.push(id.clone()),
        }
    }
    let removed = previous
        .iter()
        .filter(|(id, _)| !cur_ids.contains(id.as_str()))
        .map(|(id, _)| id.clone())
        .collect();

    (changed, added, removed)
}

pub fn compute_element_content_hash<E: Element>(
    z_threshold: usize,
    elements: &[E],
    scale: Scale<f64>,
) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();

    // Include z-threshold
    z_threshold.hash(&mut hasher);

    // Include element count
    elements.len().hash(&mut hasher);

    // Include each element's commit counter (changes on content update) and geometry
    for elem in elements {
        // Commit counter - THIS changes when surface content updates
        // CommitCounter doesn't implement Hash, so we use Debug format
        let commit = elem.current_commit();
        let commit_str = format!("{:?}", commit);
        commit_str.hash(&mut hasher);

        // Also include geometry
        let geo = elem.geometry(scale);
        geo.loc.x.hash(&mut hasher);
        geo.loc.y.hash(&mut hasher);
        geo.size.w.hash(&mut hasher);
        geo.size.h.hash(&mut hasher);
    }

    hasher.finish()
}

/// Get the stored content hash for a blur group
pub fn get_blur_group_content_hash(output_name: &str, capture_z_threshold: usize) -> Option<u64> {
    let key = (output_name.to_string(), capture_z_threshold);
    if let Ok(cache) = BLUR_GROUP_CONTENT_STATE.read() {
        cache.get(&key).copied()
    } else {
        None
    }
}

/// Store the content hash for a blur group after rendering
pub fn store_blur_group_content_hash(output_name: &str, capture_z_threshold: usize, hash: u64) {
    let key = (output_name.to_string(), capture_z_threshold);
    if let Ok(mut cache) = BLUR_GROUP_CONTENT_STATE.write() {
        cache.insert(key, hash);
    }
}

/// Get the last blur update time for a group
pub fn get_blur_group_last_update(
    output_name: &str,
    capture_z_threshold: usize,
) -> Option<Instant> {
    let key = (output_name.to_string(), capture_z_threshold);
    if let Ok(cache) = BLUR_GROUP_LAST_UPDATE.read() {
        cache.get(&key).copied()
    } else {
        None
    }
}

/// Store the last blur update time for a group
pub fn store_blur_group_last_update(output_name: &str, capture_z_threshold: usize) {
    let key = (output_name.to_string(), capture_z_threshold);
    if let Ok(mut cache) = BLUR_GROUP_LAST_UPDATE.write() {
        cache.insert(key, Instant::now());
    }
}

/// Check if blur should be throttled (content changed but update was too recent).
/// Returns true if we should skip re-blurring this frame.
pub fn should_throttle_blur(output_name: &str, capture_z_threshold: usize) -> bool {
    if let Some(last_update) = get_blur_group_last_update(output_name, capture_z_threshold) {
        last_update.elapsed() < blur_throttle_interval()
    } else {
        false
    }
}

/// Layer blur last update timestamps: hash_key -> Instant
static LAYER_BLUR_LAST_UPDATE: LazyLock<RwLock<HashMap<String, Instant>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Store the last layer blur update time
pub fn store_layer_blur_last_update(hash_key: &str) {
    if let Ok(mut cache) = LAYER_BLUR_LAST_UPDATE.write() {
        cache.insert(hash_key.to_string(), Instant::now());
    }
}

/// Check if layer blur should be throttled (content changed but update was too recent).
/// Returns true if we should skip re-blurring this frame.
pub fn should_throttle_layer_blur(hash_key: &str) -> bool {
    if let Ok(cache) = LAYER_BLUR_LAST_UPDATE.read()
        && let Some(last_update) = cache.get(hash_key)
    {
        return last_update.elapsed() < blur_throttle_interval();
    }
    false
}

/// Copy a blur texture to a new texture for caching.
///
/// This is necessary because multiple blur groups share the same ping/pong textures.
/// After blurring group 0, we must copy the result to a dedicated texture before
/// processing group 1, otherwise group 1's blur will overwrite group 0's cached texture.
///
/// Returns a new `TextureRenderBuffer` containing a copy of the source texture.
pub fn copy_blur_texture_for_cache<R>(
    renderer: &mut R,
    source: &TextureRenderBuffer<GlesTexture>,
    size: Size<i32, Physical>,
) -> Result<TextureRenderBuffer<GlesTexture>, GlesError>
where
    R: Renderer + Bind<Dmabuf> + Offscreen<GlesTexture> + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
{
    use smithay::backend::renderer::{Color32F, Frame as FrameTrait};
    use smithay::utils::Buffer as BufferCoords;

    // Create a new texture buffer for the copy
    let new_texture = Offscreen::<GlesTexture>::create_buffer(
        renderer,
        Fourcc::Abgr8888,
        Size::from((size.w, size.h)),
    )
    .map_err(|_| GlesError::UnknownSize)?;

    let mut new_buffer = TextureRenderBuffer::from_texture(
        renderer.glow_renderer(),
        new_texture,
        1,
        Transform::Normal,
        None,
    );

    // Create a texture element from the source at position (0, 0)
    let src_elem = TextureRenderElement::from_texture_render_buffer(
        Point::<f64, Physical>::from((0.0, 0.0)),
        source,
        Some(1.0),
        None,                                               // No src_rect - use full texture
        Some(Size::<i32, Logical>::from((size.w, size.h))), // Same size
        Kind::Unspecified,
    );

    new_buffer.render().draw::<_, GlesError>(|tex| {
        let glow = renderer.glow_renderer_mut();
        let mut target = glow.bind(tex)?;

        use smithay::backend::renderer::Renderer as RendererTrait;
        let mut frame = glow.render(&mut target, size, Transform::Normal)?;

        // Clear and render the source texture
        use smithay::backend::renderer::element::RenderElement;
        use smithay::backend::renderer::glow::GlowRenderer;

        let clear_damage = [Rectangle::from_size(size)];
        frame.clear(Color32F::from([0.0, 0.0, 0.0, 0.0]), &clear_damage)?;

        // Source: full texture in buffer coordinates
        let src: Rectangle<f64, BufferCoords> =
            Rectangle::from_size((size.w as f64, size.h as f64).into());
        // Destination: full output in physical coordinates
        let dst: Rectangle<i32, Physical> = Rectangle::from_size(size);
        let damage = [dst];

        <TextureRenderElement<GlesTexture> as RenderElement<GlowRenderer>>::draw(
            &src_elem,
            &mut frame,
            src,
            dst,
            &damage,
            &[],
            None,
        )?;

        drop(frame);

        // Return damage in buffer coordinates
        let buffer_size: Size<i32, Logical> = size.to_logical(1);
        let damage_rect = Rectangle::from_size(size);
        Ok(vec![damage_rect.to_logical(1).to_buffer(
            1,
            Transform::Normal,
            &buffer_size,
        )])
    })?;

    Ok(new_buffer)
}

/// Compute a hash of the background state for blur cache invalidation.
/// This hash changes when:
/// - The z-threshold changes (different windows visible)
/// - Any element's geometry changes (position/size)
/// - Element count changes
pub fn compute_background_state_hash<E: Element>(
    z_threshold: usize,
    elements: &[E],
    scale: Scale<f64>,
) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();

    // Include z-threshold
    z_threshold.hash(&mut hasher);

    // Include element count
    elements.len().hash(&mut hasher);

    // Include each element's geometry (position + size)
    for elem in elements {
        let geo = elem.geometry(scale);
        geo.loc.x.hash(&mut hasher);
        geo.loc.y.hash(&mut hasher);
        geo.size.w.hash(&mut hasher);
        geo.size.h.hash(&mut hasher);
    }

    hasher.finish()
}

// =============================================================================
// Blur Capture Context
// =============================================================================

/// Context for blur capture rendering.
/// This replaces the thread-local globals and is passed through the render pipeline.
#[derive(Clone, PartialEq)]
pub struct BlurCaptureContext {
    /// Z-index threshold: windows at or above this index will be excluded from capture
    pub exclude_z_threshold: usize,
    /// Window key being grabbed/moved - should be excluded from blur capture
    pub grabbed_window_key: Option<CosmicMappedKey>,
}

impl BlurCaptureContext {
    /// Create a new blur capture context
    pub fn new(exclude_z_threshold: usize, grabbed_window_key: Option<CosmicMappedKey>) -> Self {
        Self {
            exclude_z_threshold,
            grabbed_window_key,
        }
    }

    /// Check if a window at the given z-index should be excluded
    pub fn is_z_index_excluded(&self, z_idx: usize) -> bool {
        z_idx >= self.exclude_z_threshold
    }

    /// Check if a window is the grabbed window (by key)
    pub fn is_window_grabbed(&self, window: &CosmicMapped) -> bool {
        if let Some(ref grabbed_key) = self.grabbed_window_key {
            window.key() == *grabbed_key
        } else {
            false
        }
    }
}

// =============================================================================
// Blur Processing Functions
// =============================================================================

/// Downsample a texture by rendering it to a smaller target.
/// This uses GPU bilinear filtering for high-quality downscaling.
pub fn downsample_texture<R>(
    renderer: &mut R,
    src_texture: &TextureRenderBuffer<GlesTexture>,
    dst_texture: &mut TextureRenderBuffer<GlesTexture>,
    src_size: Size<i32, Physical>,
    dst_size: Size<i32, Physical>,
) -> Result<(), GlesError>
where
    R: Renderer + Bind<Dmabuf> + Offscreen<GlesTexture> + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
{
    let _span = tracing::debug_span!(
        "downsample_texture",
        src_w = src_size.w,
        src_h = src_size.h,
        dst_w = dst_size.w,
        dst_h = dst_size.h,
    )
    .entered();

    // Create a texture element from the source at position (0, 0)
    // The element will be rendered to fill the destination texture
    let src_elem = TextureRenderElement::from_texture_render_buffer(
        Point::<f64, Physical>::from((0.0, 0.0)),
        src_texture,
        Some(1.0),
        None, // No src_rect - use full texture
        Some(Size::<i32, Logical>::from((dst_size.w, dst_size.h))), // Scale to dst size
        Kind::Unspecified,
    );

    dst_texture.render().draw::<_, GlesError>(|tex| {
        let glow = renderer.glow_renderer_mut();
        let mut target = glow.bind(tex)?;

        use smithay::backend::renderer::Renderer as RendererTrait;
        let mut frame = glow.render(&mut target, dst_size, Transform::Normal)?;

        // Clear and render the downsampled texture
        use smithay::backend::renderer::element::RenderElement;
        use smithay::backend::renderer::glow::GlowRenderer;
        use smithay::backend::renderer::{Color32F, Frame as FrameTrait};
        use smithay::utils::Buffer as BufferCoords;

        let clear_damage = [Rectangle::from_size(dst_size)];
        frame.clear(Color32F::from([0.0, 0.0, 0.0, 0.0]), &clear_damage)?;

        // Source: full texture in buffer coordinates (original size)
        let src: Rectangle<f64, BufferCoords> =
            Rectangle::from_size((src_size.w as f64, src_size.h as f64).into());
        // Destination: full output in physical coordinates (downsampled size)
        let dst: Rectangle<i32, Physical> = Rectangle::from_size(dst_size);
        let damage = [dst];

        <TextureRenderElement<GlesTexture> as RenderElement<GlowRenderer>>::draw(
            &src_elem,
            &mut frame,
            src,
            dst,
            &damage,
            &[],
            None,
        )?;

        drop(frame);

        // Return damage in buffer coordinates
        let buffer_size: Size<i32, Logical> = dst_size.to_logical(1);
        let damage_rect = Rectangle::from_size(dst_size);
        Ok(vec![damage_rect.to_logical(1).to_buffer(
            1,
            Transform::Normal,
            &buffer_size,
        )])
    })?;

    Ok(())
}

// =============================================================================
// Dual Kawase Blur (Progressive Downsample/Upsample)
// =============================================================================

/// Apply Dual Kawase blur using progressive downsample + upsample.
///
/// This is significantly faster than the old Kawase ping-pong approach:
/// - Old: 12 iterations × 2 passes = 24 shader passes at fixed resolution
/// - New: N downsample + N upsample = 2N passes (typically N=4 → 8 passes)
///   Each downsample level halves resolution, so later passes are very cheap.
///
/// The algorithm:
/// 1. Start with `src_texture` at `src_size`
/// 2. Downsample N times: apply blur kernel while halving resolution each step
/// 3. Upsample N times: apply blur kernel while doubling resolution each step
/// 4. Final result is in `pong_texture` at `src_size`
///
/// Parameters:
/// - `renderer`: The GPU renderer
/// - `src_texture`: Input texture (downsampled background capture)
/// - `ping_texture`/`pong_texture`: Working textures at src_size (reused for final output)
/// - `src_size`: Size of the input/output textures
/// - `levels`: Number of downsample/upsample iterations (2-5, typically 4)
/// - `offset`: Blur spread per level (1.0-3.0, higher = stronger)
pub fn apply_dual_kawase_blur<R>(
    renderer: &mut R,
    src_texture: &TextureRenderBuffer<GlesTexture>,
    _ping_texture: &mut TextureRenderBuffer<GlesTexture>,
    pong_texture: &mut TextureRenderBuffer<GlesTexture>,
    src_size: Size<i32, Physical>,
    levels: u32,
    offset: f32,
) -> Result<(), GlesError>
where
    R: Renderer + Bind<Dmabuf> + Offscreen<GlesTexture> + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
{
    // Compute does the same work without binding a framebuffer or clearing once
    // per pass, so prefer it and keep this function as the fallback. Deciding
    // here rather than at the call sites means a caller cannot accidentally end
    // up with no blur at all on a pre-3.1 context.
    if compute_blur_allowed() && !COMPUTE_BLUR_DISABLED.load(Ordering::Relaxed) {
        match apply_dual_kawase_blur_compute(
            renderer,
            src_texture,
            pong_texture,
            src_size,
            levels,
            offset,
        ) {
            Ok(true) => {
                COMPUTE_BLUR_USED.store(true, Ordering::Relaxed);
                return Ok(());
            }
            // No compute support on this context: nothing to report, this is the
            // path that is meant to run.
            Ok(false) => {}
            Err(err) => {
                // A driver that fails a dispatch once will keep failing, so stop
                // trying instead of logging every frame.
                COMPUTE_BLUR_DISABLED.store(true, Ordering::Relaxed);
                tracing::warn!(?err, "Compute blur failed, falling back to fragment blur");
            }
        }
    }

    let _span = tracing::info_span!(
        "dual_kawase_blur",
        levels = levels,
        offset = offset,
        src_w = src_size.w,
        src_h = src_size.h,
    )
    .entered();

    let levels = levels.clamp(1, 8);

    // Build mip chain sizes: each level halves the resolution
    let mut mip_sizes: Vec<Size<i32, Physical>> = Vec::with_capacity(levels as usize + 1);
    mip_sizes.push(src_size); // Level 0 = full resolution
    for i in 1..=levels {
        let prev = mip_sizes[i as usize - 1];
        mip_sizes.push(Size::from(((prev.w / 2).max(1), (prev.h / 2).max(1))));
    }

    // Allocate temporary textures for intermediate mip levels
    // We need one texture per intermediate level (levels 1..levels-1)
    // Level 0 = src_texture (input), level N (smallest) stored in ping
    // Upsample chain reuses these textures
    let mut mip_textures: Vec<TextureRenderBuffer<GlesTexture>> =
        Vec::with_capacity(levels as usize);
    for i in 1..=levels {
        let mip_size = mip_sizes[i as usize];
        let buffer_size = mip_size.to_logical(1).to_buffer(1, Transform::Normal);
        let tex = Offscreen::<GlesTexture>::create_buffer(renderer, Fourcc::Abgr8888, buffer_size)
            .map_err(|_| GlesError::UnknownSize)?;
        let buffer = TextureRenderBuffer::from_texture(
            renderer.glow_renderer(),
            tex,
            1,
            Transform::Normal,
            None,
        );
        mip_textures.push(buffer);
    }

    let downsample_shader = super::DualKawaseDownsampleShader::get(renderer);
    let upsample_shader = super::DualKawaseUpsampleShader::get(renderer);

    // === DOWNSAMPLE CHAIN ===
    // Level 0→1: src_texture → mip_textures[0]
    // Level 1→2: mip_textures[0] → mip_textures[1]
    // ...
    // Level N-1→N: mip_textures[N-2] → mip_textures[N-1]
    for i in 0..levels as usize {
        let input_size = mip_sizes[i];
        let output_size = mip_sizes[i + 1];
        let half_texel = [0.5 / input_size.w as f32, 0.5 / input_size.h as f32];

        if i == 0 {
            // First pass: read from src_texture, write to mip_textures[0]
            apply_dual_kawase_pass(
                renderer,
                src_texture,
                &mut mip_textures[0],
                &downsample_shader,
                half_texel,
                offset,
                input_size,
                output_size,
            )?;
        } else {
            // Split to get immutable ref to [i-1] and mutable ref to [i]
            let (left, right) = mip_textures.split_at_mut(i);
            let input = &left[i - 1];
            let output = &mut right[0];
            apply_dual_kawase_pass(
                renderer,
                input,
                output,
                &downsample_shader,
                half_texel,
                offset,
                input_size,
                output_size,
            )?;
        }
    }

    // === UPSAMPLE CHAIN ===
    // Level N→N-1: mip_textures[N-1] → mip_textures[N-2]  (if N >= 2)
    // ...
    // Level 2→1: mip_textures[1] → mip_textures[0]
    // Level 1→0: mip_textures[0] → pong_texture
    for i in (0..levels as usize).rev() {
        let output_size = mip_sizes[i];
        let input_size = mip_sizes[i + 1];
        let half_texel = [0.5 / input_size.w as f32, 0.5 / input_size.h as f32];

        if i == 0 {
            // Final upsample: read from mip_textures[0], write to pong_texture
            apply_dual_kawase_pass(
                renderer,
                &mip_textures[0],
                pong_texture,
                &upsample_shader,
                half_texel,
                offset,
                input_size,
                output_size,
            )?;
        } else if i == levels as usize - 1 {
            // First upsample: read from mip_textures[N-1], write to mip_textures[N-2]
            // Split at i to get mutable ref to [i-1] and immutable ref to [i]
            let (left, right) = mip_textures.split_at_mut(i);
            let input = &right[0];
            let output = &mut left[i - 1];
            apply_dual_kawase_pass(
                renderer,
                input,
                output,
                &upsample_shader,
                half_texel,
                offset,
                input_size,
                output_size,
            )?;
        } else {
            // Middle upsample: read from mip_textures[i] (previous upsample output), write to mip_textures[i-1]
            // Previous step (i+1) wrote its result to mip_textures[i], so we read from right[0]
            let (left, right) = mip_textures.split_at_mut(i);
            let input = &right[0]; // index i (result of previous upsample step)
            let output = &mut left[i - 1];
            apply_dual_kawase_pass(
                renderer,
                input,
                output,
                &upsample_shader,
                half_texel,
                offset,
                input_size,
                output_size,
            )?;
        }
    }

    tracing::trace!(
        levels = levels,
        offset = offset,
        total_passes = levels * 2,
        "Dual Kawase blur complete"
    );

    Ok(())
}

/// Apply a single Dual Kawase pass (either downsample or upsample)
///
/// `src_size` is the actual pixel dimensions of the source texture buffer.
/// `dst_size` is the pixel dimensions of the destination framebuffer.
/// These differ because each downsample halves and each upsample doubles.
fn apply_dual_kawase_pass<R>(
    renderer: &mut R,
    src_texture: &TextureRenderBuffer<GlesTexture>,
    dst_texture: &mut TextureRenderBuffer<GlesTexture>,
    shader: &GlesTexProgram,
    half_texel: [f32; 2],
    offset: f32,
    src_size: Size<i32, Physical>,
    dst_size: Size<i32, Physical>,
) -> Result<(), GlesError>
where
    R: Renderer + Bind<Dmabuf> + Offscreen<GlesTexture> + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
{
    let src_elem = TextureRenderElement::from_texture_render_buffer(
        Point::<f64, Physical>::from((0.0, 0.0)),
        src_texture,
        Some(1.0),
        None,
        Some(Size::<i32, Logical>::from((dst_size.w, dst_size.h))),
        Kind::Unspecified,
    );

    let shader_elem = TextureShaderElement::new(
        src_elem,
        shader.clone(),
        vec![
            Uniform::new("half_texel", half_texel),
            Uniform::new("offset", offset),
        ],
    );

    dst_texture.render().draw::<_, GlesError>(|tex| {
        let glow = renderer.glow_renderer_mut();
        let mut target = glow.bind(tex)?;

        use smithay::backend::renderer::Renderer as RendererTrait;
        let mut frame = glow.render(&mut target, dst_size, Transform::Normal)?;

        use smithay::backend::renderer::Color32F;
        use smithay::backend::renderer::Frame as FrameTrait;
        let clear_damage = [Rectangle::from_size(dst_size)];
        frame.clear(Color32F::from([0.0, 0.0, 0.0, 0.0]), &clear_damage)?;

        use smithay::backend::renderer::element::RenderElement;
        use smithay::backend::renderer::glow::GlowRenderer;
        use smithay::utils::Buffer as BufferCoords;

        // src rectangle must match the actual source buffer dimensions,
        // NOT the destination size. Otherwise UVs exceed 0-1 range when
        // upsampling (src smaller than dst) causing edge-clamped flat color.
        let src: Rectangle<f64, BufferCoords> =
            Rectangle::from_size((src_size.w as f64, src_size.h as f64).into());
        let dst: Rectangle<i32, Physical> = Rectangle::from_size(dst_size);
        let damage = [dst];

        <TextureShaderElement as RenderElement<GlowRenderer>>::draw(
            &shader_elem,
            &mut frame,
            src,
            dst,
            &damage,
            &[],
            None,
        )?;

        drop(frame);

        let buffer_size: Size<i32, Logical> = dst_size.to_logical(1);
        let damage_rect = Rectangle::from_size(dst_size);
        Ok(vec![damage_rect.to_logical(1).to_buffer(
            1,
            Transform::Normal,
            &buffer_size,
        )])
    })?;

    Ok(())
}

// =============================================================================
// Runtime Blur Diagnostics
// =============================================================================

// =============================================================================
// HasBlur Trait
// =============================================================================

/// Trait for elements that may have blur enabled
pub trait HasBlur {
    /// Check if this element has blur enabled
    fn has_blur(&self) -> bool;
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_radius_maps_min_to_no_blur_max_to_full() {
        // 1px (and anything below) ⇒ minimum blur (1 level, 0.5 offset).
        assert_eq!(blur_levels_for_radius(BLUR_RADIUS_MIN_PX), 1);
        assert!((blur_offset_for_radius(BLUR_RADIUS_MIN_PX) - 0.5).abs() < 1e-6);
        assert_eq!(blur_levels_for_radius(0.0), 1);
        assert_eq!(blur_levels_for_radius(-5.0), 1);

        // 40px (and anything above) ⇒ maximum blur (6 levels, 5.0 offset).
        assert_eq!(blur_levels_for_radius(BLUR_RADIUS_MAX_PX), 6);
        assert!((blur_offset_for_radius(BLUR_RADIUS_MAX_PX) - 5.0).abs() < 1e-6);
        assert_eq!(blur_levels_for_radius(120.0), 6);
        assert!((blur_offset_for_radius(120.0) - 5.0).abs() < 1e-6);
    }

    #[test]
    fn test_radius_levels_are_monotonic_nondecreasing() {
        let mut last = 0;
        let mut r = 0.0;
        while r <= 120.0 {
            let lvl = blur_levels_for_radius(r);
            assert!(lvl >= last, "levels must not decrease as radius grows");
            assert!((1..=6).contains(&lvl));
            last = lvl;
            r += 0.5;
        }
        // The top of the range must actually reach full blur.
        assert_eq!(blur_levels_for_radius(BLUR_RADIUS_MAX_PX), 6);
    }

    #[test]
    fn test_non_finite_radius_is_safe_never_nan() {
        // A malformed client value must never produce a NaN offset (shader-fatal)
        // and must collapse to the minimum (treated like "no custom radius").
        for bad in [f32::NAN, f32::INFINITY, f32::NEG_INFINITY] {
            let off = blur_offset_for_radius(bad);
            assert!(off.is_finite(), "offset must stay finite for {bad:?}");
            assert!((off - 0.5).abs() < 1e-6);
            assert_eq!(blur_levels_for_radius(bad), 1);
        }
    }

    #[test]
    fn test_resolve_none_matches_global_config() {
        // None must be byte-for-byte the previous global behaviour.
        set_blur_intensity(0.7);
        assert_eq!(
            resolve_blur_params(None),
            (effective_blur_levels(), effective_blur_offset())
        );
        // Some(px) must be the radius-derived params, independent of the global.
        assert_eq!(
            resolve_blur_params(Some(BLUR_RADIUS_MAX_PX)),
            (
                blur_levels_for_radius(BLUR_RADIUS_MAX_PX),
                blur_offset_for_radius(BLUR_RADIUS_MAX_PX)
            )
        );
    }

    #[test]
    fn test_mip_size_chain_calculation() {
        // Test: mip chain sizes are computed correctly with progressive halving
        let src_size = Size::<i32, Physical>::from((720, 450));
        let levels: u32 = 5;

        let mut mip_sizes: Vec<Size<i32, Physical>> = Vec::with_capacity(levels as usize + 1);
        mip_sizes.push(src_size);
        for i in 1..=levels {
            let prev = mip_sizes[i as usize - 1];
            mip_sizes.push(Size::from(((prev.w / 2).max(1), (prev.h / 2).max(1))));
        }

        assert_eq!(mip_sizes.len(), 6);
        assert_eq!(mip_sizes[0], Size::from((720, 450)));
        assert_eq!(mip_sizes[1], Size::from((360, 225)));
        assert_eq!(mip_sizes[2], Size::from((180, 112)));
        assert_eq!(mip_sizes[3], Size::from((90, 56)));
        assert_eq!(mip_sizes[4], Size::from((45, 28)));
        assert_eq!(mip_sizes[5], Size::from((22, 14)));
    }

    #[test]
    fn test_mip_size_chain_1080p() {
        // Test with common 1080p resolution (after 2x downsample: 960×540)
        let src_size = Size::<i32, Physical>::from((960, 540));
        let levels: u32 = 5;

        let mut mip_sizes: Vec<Size<i32, Physical>> = Vec::with_capacity(levels as usize + 1);
        mip_sizes.push(src_size);
        for i in 1..=levels {
            let prev = mip_sizes[i as usize - 1];
            mip_sizes.push(Size::from(((prev.w / 2).max(1), (prev.h / 2).max(1))));
        }

        assert_eq!(mip_sizes[0], Size::from((960, 540)));
        assert_eq!(mip_sizes[1], Size::from((480, 270)));
        assert_eq!(mip_sizes[2], Size::from((240, 135)));
        assert_eq!(mip_sizes[3], Size::from((120, 67)));
        assert_eq!(mip_sizes[4], Size::from((60, 33)));
        assert_eq!(mip_sizes[5], Size::from((30, 16)));
    }

    #[test]
    fn test_mip_sizes_never_zero() {
        // Test: mip sizes never reach 0 due to .max(1)
        let src_size = Size::<i32, Physical>::from((16, 9));
        let levels: u32 = 5;

        let mut mip_sizes: Vec<Size<i32, Physical>> = Vec::with_capacity(levels as usize + 1);
        mip_sizes.push(src_size);
        for i in 1..=levels {
            let prev = mip_sizes[i as usize - 1];
            mip_sizes.push(Size::from(((prev.w / 2).max(1), (prev.h / 2).max(1))));
        }

        for size in &mip_sizes {
            assert!(size.w >= 1, "Width must be >= 1, got {}", size.w);
            assert!(size.h >= 1, "Height must be >= 1, got {}", size.h);
        }
    }

    #[test]
    fn test_downsample_indices_match_sizes() {
        // Test: downsample loop correctly pairs input_size and output_size
        let src_size = Size::<i32, Physical>::from((720, 450));
        let levels: u32 = 5;

        let mut mip_sizes: Vec<Size<i32, Physical>> = Vec::with_capacity(levels as usize + 1);
        mip_sizes.push(src_size);
        for i in 1..=levels {
            let prev = mip_sizes[i as usize - 1];
            mip_sizes.push(Size::from(((prev.w / 2).max(1), (prev.h / 2).max(1))));
        }

        // Simulate the downsample loop
        for i in 0..levels as usize {
            let input_size = mip_sizes[i];
            let output_size = mip_sizes[i + 1];

            // Input should always be larger than output (downsampling)
            assert!(
                input_size.w >= output_size.w,
                "Downsample step {}: input_w {} < output_w {}",
                i,
                input_size.w,
                output_size.w
            );
            assert!(
                input_size.h >= output_size.h,
                "Downsample step {}: input_h {} < output_h {}",
                i,
                input_size.h,
                output_size.h
            );

            // Output should be approximately half of input
            assert!(
                output_size.w == (input_size.w / 2).max(1),
                "Step {}: output_w {} != input_w/2 {}",
                i,
                output_size.w,
                (input_size.w / 2).max(1)
            );
        }
    }

    #[test]
    fn test_upsample_indices_match_sizes() {
        // Test: upsample loop correctly pairs input_size and output_size
        let src_size = Size::<i32, Physical>::from((720, 450));
        let levels: u32 = 5;

        let mut mip_sizes: Vec<Size<i32, Physical>> = Vec::with_capacity(levels as usize + 1);
        mip_sizes.push(src_size);
        for i in 1..=levels {
            let prev = mip_sizes[i as usize - 1];
            mip_sizes.push(Size::from(((prev.w / 2).max(1), (prev.h / 2).max(1))));
        }

        // Simulate the upsample loop (reverse)
        for i in (0..levels as usize).rev() {
            let output_size = mip_sizes[i];
            let input_size = mip_sizes[i + 1];

            // Input should always be smaller than output (upsampling)
            assert!(
                input_size.w <= output_size.w,
                "Upsample step {}: input_w {} > output_w {}",
                i,
                input_size.w,
                output_size.w
            );
            assert!(
                input_size.h <= output_size.h,
                "Upsample step {}: input_h {} > output_h {}",
                i,
                input_size.h,
                output_size.h
            );
        }

        // Final upsample output should be src_size
        let final_output = mip_sizes[0];
        assert_eq!(
            final_output, src_size,
            "Final upsample should return to src_size"
        );
    }

    #[test]
    fn test_half_texel_calculation() {
        // Test: half_texel is correctly computed for each level
        let sizes = vec![
            Size::<i32, Physical>::from((720, 450)),
            Size::<i32, Physical>::from((360, 225)),
            Size::<i32, Physical>::from((180, 112)),
        ];

        for size in &sizes {
            let half_texel = [0.5 / size.w as f32, 0.5 / size.h as f32];
            assert!(half_texel[0] > 0.0, "half_texel.x must be positive");
            assert!(half_texel[1] > 0.0, "half_texel.y must be positive");
            assert!(half_texel[0] < 1.0, "half_texel.x must be < 1.0");
            assert!(half_texel[1] < 1.0, "half_texel.y must be < 1.0");
        }

        // For 720x450: half_texel = (0.000694, 0.00111)
        let ht = [0.5 / 720.0f32, 0.5 / 450.0f32];
        assert!((ht[0] - 0.000694).abs() < 0.0001);
        assert!((ht[1] - 0.001111).abs() < 0.0001);
    }

    #[test]
    fn test_src_dst_size_consistency_downsample() {
        // Test: in downsample pass, src_size is the SOURCE texture dimension
        // and dst_size is the DESTINATION texture dimension
        let src_size = Size::<i32, Physical>::from((720, 450));
        let levels: u32 = 5;

        let mut mip_sizes: Vec<Size<i32, Physical>> = Vec::with_capacity(levels as usize + 1);
        mip_sizes.push(src_size);
        for i in 1..=levels {
            let prev = mip_sizes[i as usize - 1];
            mip_sizes.push(Size::from(((prev.w / 2).max(1), (prev.h / 2).max(1))));
        }

        // Downsample: reading from mip[i] (input_size), writing to mip[i+1] (output_size)
        for i in 0..levels as usize {
            let input_size = mip_sizes[i]; // src_size param = source buffer dims
            let output_size = mip_sizes[i + 1]; // dst_size param = destination framebuffer dims

            // The src rectangle should match the source buffer
            let src_rect_w = input_size.w as f64;
            let src_rect_h = input_size.h as f64;

            // UV calculation: src_rect / texture_size should be <= 1.0
            // (texture_size == input_size for the source texture)
            let u_max = src_rect_w / input_size.w as f64;
            let v_max = src_rect_h / input_size.h as f64;
            assert!(
                u_max <= 1.0,
                "Step {}: U max {} > 1.0 (would sample out of bounds!)",
                i,
                u_max
            );
            assert!(
                v_max <= 1.0,
                "Step {}: V max {} > 1.0 (would sample out of bounds!)",
                i,
                v_max
            );

            // If we had used dst_size as src (the old bug), UVs would exceed 1.0 during upsampling
            // For downsample it happens to work since dst < src, but it's still wrong
            let wrong_u = output_size.w as f64 / input_size.w as f64;
            let wrong_v = output_size.h as f64 / input_size.h as f64;
            assert!(
                wrong_u <= 1.0,
                "Downsample step {}: even wrong UVs stay in bounds (dst < src)",
                i
            );
            assert!(wrong_v <= 1.0);
        }
    }

    #[test]
    fn test_src_dst_size_consistency_upsample() {
        // Test: in upsample pass, src_size is the SOURCE (smaller) texture dimension
        // and dst_size is the DESTINATION (larger) framebuffer dimension
        // The OLD BUG: using dst_size as src_rect caused UV > 1.0 during upsampling
        let src_size = Size::<i32, Physical>::from((720, 450));
        let levels: u32 = 5;

        let mut mip_sizes: Vec<Size<i32, Physical>> = Vec::with_capacity(levels as usize + 1);
        mip_sizes.push(src_size);
        for i in 1..=levels {
            let prev = mip_sizes[i as usize - 1];
            mip_sizes.push(Size::from(((prev.w / 2).max(1), (prev.h / 2).max(1))));
        }

        // Upsample: reading from mip[i+1] (input_size), writing to mip[i] (output_size)
        for i in (0..levels as usize).rev() {
            let output_size = mip_sizes[i];
            let input_size = mip_sizes[i + 1];

            // CORRECT: src_rect = input_size (source texture dimensions)
            let correct_u = input_size.w as f64 / input_size.w as f64;
            let correct_v = input_size.h as f64 / input_size.h as f64;
            assert_eq!(correct_u, 1.0, "Correct UV should be exactly 1.0");
            assert_eq!(correct_v, 1.0, "Correct UV should be exactly 1.0");

            // BUG: if we used output_size (dst) as src_rect, UVs would exceed 1.0
            // because output_size > input_size during upsampling
            let wrong_u = output_size.w as f64 / input_size.w as f64;
            let wrong_v = output_size.h as f64 / input_size.h as f64;
            assert!(
                wrong_u > 1.0,
                "Step {}: OLD BUG would cause UV {} > 1.0 (reading past texture!)",
                i,
                wrong_u
            );
            assert!(
                wrong_v > 1.0,
                "Step {}: OLD BUG would cause UV {} > 1.0 (reading past texture!)",
                i,
                wrong_v
            );
        }
    }

    #[test]
    fn test_blur_downsample_factor() {
        // Test: blur_size calculation with downsample factor
        let screen_size = Size::<i32, Physical>::from((1440, 900));
        let downsample_factor = BLUR_DOWNSAMPLE_FACTOR;

        let blur_size: Size<i32, Physical> = Size::from((
            (screen_size.w / downsample_factor).max(1),
            (screen_size.h / downsample_factor).max(1),
        ));

        assert_eq!(blur_size, Size::from((720, 450)));
    }

    #[test]
    fn test_total_reduction_factor() {
        // Test: total reduction from screen to smallest mip at max intensity
        let screen_size = Size::<i32, Physical>::from((1440, 900));
        let blur_size: Size<i32, Physical> = Size::from((720i32, 450i32)); // after 2x downsample

        // At max intensity (1.0), effective_blur_levels() returns 6
        set_blur_intensity(1.0);
        let levels = effective_blur_levels();

        let mut size = blur_size;
        for _ in 0..levels {
            size = Size::from(((size.w / 2).max(1), (size.h / 2).max(1)));
        }

        // Smallest mip should be very small at max intensity
        let total_reduction_w = screen_size.w as f32 / size.w as f32;
        assert!(
            total_reduction_w > 30.0,
            "Total reduction should be >30x at max intensity, got {}x",
            total_reduction_w
        );
    }

    #[test]
    fn test_offset_within_safe_range() {
        // Test: effective_blur_offset is always in a safe range
        for i in 0..=10 {
            let intensity = i as f32 / 10.0;
            set_blur_intensity(intensity);
            let offset = effective_blur_offset();
            assert!(
                offset >= 0.5,
                "Offset too low at intensity {intensity}: {offset}"
            );
            assert!(
                offset <= 5.0,
                "Offset too high at intensity {intensity}: {offset}"
            );
        }
    }

    #[test]
    fn test_levels_within_bounds() {
        // Test: effective_blur_levels is always reasonable
        for i in 0..=10 {
            let intensity = i as f32 / 10.0;
            set_blur_intensity(intensity);
            let levels = effective_blur_levels();
            assert!(
                levels >= 1,
                "Need at least 1 level at intensity {intensity}"
            );
            assert!(
                levels <= 6,
                "More than 6 levels is wasteful at intensity {intensity}, got {levels}"
            );
        }
    }
}

// =============================================================================
// Compute blur
// =============================================================================

/// Set when a compute dispatch has failed, to stop retrying it every frame.
static COMPUTE_BLUR_DISABLED: AtomicBool = AtomicBool::new(false);

/// Whether compute blur may be used at all.
///
/// `COSMIC_BLUR_COMPUTE=0` forces the fragment path on a machine that supports
/// compute. That makes the two paths comparable on one binary, and gives users
/// an escape hatch if a driver miscompiles the compute shaders in a way that
/// still reports success.
fn compute_blur_allowed() -> bool {
    static ALLOWED: LazyLock<bool> = LazyLock::new(|| {
        !matches!(
            std::env::var("COSMIC_BLUR_COMPUTE").as_deref(),
            Ok("0") | Ok("false")
        )
    });
    *ALLOWED
}

/// Records that a compute blur actually ran, so the profiler can report which
/// path its timings came from rather than which one was merely available.
static COMPUTE_BLUR_USED: AtomicBool = AtomicBool::new(false);

/// Describes the blur path in use, for the performance report.
pub fn active_blur_path() -> &'static str {
    // Order matters: a run that used compute and then hit a dispatch error is
    // now on the fragment path, and the report has to say so rather than stay
    // latched on the first path that ever ran.
    if COMPUTE_BLUR_DISABLED.load(Ordering::Relaxed) {
        "fragment (compute failed)"
    } else if !compute_blur_allowed() {
        "fragment (COSMIC_BLUR_COMPUTE=0)"
    } else if COMPUTE_BLUR_USED.load(Ordering::Relaxed) {
        "compute"
    } else {
        "fragment"
    }
}

/// Workgroup size in both compute Kawase shaders. Dispatch counts must round up
/// to a multiple of this, which is why the shaders bounds-check.
const KAWASE_WORKGROUP: i32 = 8;

/// Reusable mip chain for the blur passes, keyed by the chain it serves.
///
/// The fragment path allocates its intermediate textures on every call and frees
/// them again at the end -- `levels` allocations per blurred window per frame.
/// The compute path cannot do that even if it wanted to, because its textures
/// need immutable storage, so the chain is created once and kept. Sizes repeat
/// frame to frame, so the cache hits essentially always after the first frame.
///
/// Held in the renderer's EGL user data rather than a global, so the textures
/// cannot outlive the context that made them or be handed to a second GPU's
/// renderer.
#[derive(Debug, Default)]
pub struct ComputeMipPool(Mutex<HashMap<(i32, i32, u32), Vec<TextureRenderBuffer<GlesTexture>>>>);

impl ComputeMipPool {
    /// Returns the chain for `src_size`/`levels`, creating it on first use.
    ///
    /// Entries are cloned out rather than borrowed because the caller needs the
    /// renderer mutably to dispatch; the clone is of handles, not pixels.
    fn get_or_create<R>(
        renderer: &mut R,
        src_size: Size<i32, Physical>,
        levels: u32,
        mip_sizes: &[Size<i32, Physical>],
    ) -> Option<Vec<TextureRenderBuffer<GlesTexture>>>
    where
        R: AsGlowRenderer,
    {
        let key = (src_size.w, src_size.h, levels);

        {
            let glow = renderer.glow_renderer();
            let gles: &GlesRenderer = Borrow::borrow(glow);
            if let Some(pool) = gles.egl_context().user_data().get::<ComputeMipPool>() {
                if let Some(chain) = pool.0.lock().unwrap().get(&key) {
                    return Some(chain.clone());
                }
            } else {
                return None;
            }
        }

        // Bound the pool. Keys vary with output size and blur intensity, so the
        // set is small in practice, but a run that cycles through many
        // resolutions would otherwise hold every chain it ever built. Dropping
        // the map only releases this cache's handles; chains still in use are
        // kept alive by the caller's clone.
        const MAX_CHAINS: usize = 8;

        let mut chain = Vec::with_capacity(levels as usize);
        for mip_size in mip_sizes.iter().take(levels as usize + 1).skip(1) {
            let buffer_size = mip_size.to_logical(1).to_buffer(1, Transform::Normal);
            let gles: &mut GlesRenderer = BorrowMut::borrow_mut(renderer.glow_renderer_mut());
            let tex = gles.create_compute_buffer(buffer_size).ok()?;
            chain.push(TextureRenderBuffer::from_texture(
                renderer.glow_renderer(),
                tex,
                1,
                Transform::Normal,
                None,
            ));
        }

        let glow = renderer.glow_renderer();
        let gles: &GlesRenderer = Borrow::borrow(glow);
        let pool = gles.egl_context().user_data().get::<ComputeMipPool>()?;
        let mut chains = pool.0.lock().unwrap();
        if chains.len() >= MAX_CHAINS {
            chains.clear();
        }
        chains.insert(key, chain.clone());
        Some(chain)
    }
}

/// Runs one compute Kawase pass, sampling `src` and writing `dst` via imageStore.
///
/// Unlike the fragment equivalent this binds no framebuffer and clears nothing:
/// every destination texel is written by exactly one invocation, so a clear
/// would only be redundant bandwidth.
fn dispatch_kawase_pass<R>(
    renderer: &mut R,
    program: GlesComputeProgram,
    src: &GlesTexture,
    dst: &GlesTexture,
    half_texel: [f32; 2],
    offset: f32,
    dst_size: Size<i32, Physical>,
) -> Result<(), GlesError>
where
    R: AsGlowRenderer,
{
    let src_id = src.tex_id();
    let dst_id = dst.tex_id();
    let gles: &mut GlesRenderer = BorrowMut::borrow_mut(renderer.glow_renderer_mut());

    // Round the group count up so the last partial block is still covered; the
    // shaders discard invocations that land outside the image.
    let groups = (
        ((dst_size.w + KAWASE_WORKGROUP - 1) / KAWASE_WORKGROUP).max(1) as u32,
        ((dst_size.h + KAWASE_WORKGROUP - 1) / KAWASE_WORKGROUP).max(1) as u32,
        1,
    );

    unsafe {
        gles.dispatch_compute(&program, groups, |gl, prog| {
            let name = |s: &[u8]| gl.GetUniformLocation(prog, s.as_ptr() as *const _);

            gl.ActiveTexture(ffi::TEXTURE0);
            gl.BindTexture(ffi::TEXTURE_2D, src_id);

            // Set the sampler state explicitly. The fragment path gets this for
            // free because smithay's render_texture sets it on every draw, but a
            // compute dispatch samples the texture as it finds it. That matters
            // most for the chain's first input, which comes from the caller as a
            // plain offscreen buffer: GL's default minification filter expects
            // mipmaps, the buffer has only level 0, and sampling a
            // mipmap-incomplete texture returns solid black rather than failing.
            // CLAMP_TO_EDGE also keeps the edge taps matching the fragment path.
            gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_MIN_FILTER, ffi::LINEAR as i32);
            gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_MAG_FILTER, ffi::LINEAR as i32);
            gl.TexParameteri(
                ffi::TEXTURE_2D,
                ffi::TEXTURE_WRAP_S,
                ffi::CLAMP_TO_EDGE as i32,
            );
            gl.TexParameteri(
                ffi::TEXTURE_2D,
                ffi::TEXTURE_WRAP_T,
                ffi::CLAMP_TO_EDGE as i32,
            );

            gl.Uniform1i(name(b"tex\0"), 0);

            // The shader declares `binding = 0` for the image, so unit 0 here.
            gl.BindImageTexture(0, dst_id, 0, ffi::FALSE, 0, ffi::WRITE_ONLY, ffi::RGBA8);

            gl.Uniform2f(name(b"half_texel\0"), half_texel[0], half_texel[1]);
            gl.Uniform1f(name(b"offset\0"), offset);
            gl.Uniform2i(name(b"dst_size\0"), dst_size.w, dst_size.h);
        })
    }
}

/// Dual Kawase blur using compute shaders, with the same result as
/// [`apply_dual_kawase_blur`].
///
/// Returns `false` when this context cannot run it -- no compute support, or no
/// cached chain -- and the caller must use the fragment path instead.
///
/// The final upsample is deliberately still a fragment pass. `pong_texture`
/// belongs to the caller and has mutable storage, so imageStore cannot target
/// it; doing that last step by drawing writes the result where the caller
/// expects it without an extra copy.
pub fn apply_dual_kawase_blur_compute<R>(
    renderer: &mut R,
    src_texture: &TextureRenderBuffer<GlesTexture>,
    pong_texture: &mut TextureRenderBuffer<GlesTexture>,
    src_size: Size<i32, Physical>,
    levels: u32,
    offset: f32,
) -> Result<bool, GlesError>
where
    R: Renderer + Bind<Dmabuf> + Offscreen<GlesTexture> + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
{
    let Some((downsample, upsample)) = super::DualKawaseComputeShaders::get(renderer) else {
        return Ok(false);
    };

    let _span = tracing::info_span!(
        "dual_kawase_blur_compute",
        levels = levels,
        offset = offset,
        src_w = src_size.w,
        src_h = src_size.h,
    )
    .entered();

    let levels = levels.clamp(1, 8);

    let mut mip_sizes: Vec<Size<i32, Physical>> = Vec::with_capacity(levels as usize + 1);
    mip_sizes.push(src_size);
    for i in 1..=levels {
        let prev = mip_sizes[i as usize - 1];
        mip_sizes.push(Size::from(((prev.w / 2).max(1), (prev.h / 2).max(1))));
    }

    let Some(mips) = ComputeMipPool::get_or_create(renderer, src_size, levels, &mip_sizes) else {
        return Ok(false);
    };

    // === DOWNSAMPLE CHAIN === src -> mips[0] -> ... -> mips[levels-1]
    for i in 0..levels as usize {
        let input_size = mip_sizes[i];
        let half_texel = [0.5 / input_size.w as f32, 0.5 / input_size.h as f32];
        let src = if i == 0 {
            src_texture.texture().clone()
        } else {
            mips[i - 1].texture().clone()
        };
        dispatch_kawase_pass(
            renderer,
            downsample,
            &src,
            &mips[i].texture().clone(),
            half_texel,
            offset,
            mip_sizes[i + 1],
        )?;
    }

    // === UPSAMPLE CHAIN === mips[levels-1] -> ... -> mips[0], then a fragment
    // pass for the last level so the result lands in the caller's texture.
    for i in (1..levels as usize).rev() {
        let input_size = mip_sizes[i + 1];
        let half_texel = [0.5 / input_size.w as f32, 0.5 / input_size.h as f32];
        let src = mips[i].texture().clone();
        dispatch_kawase_pass(
            renderer,
            upsample,
            &src,
            &mips[i - 1].texture().clone(),
            half_texel,
            offset,
            mip_sizes[i],
        )?;
    }

    let input_size = mip_sizes[1];
    let half_texel = [0.5 / input_size.w as f32, 0.5 / input_size.h as f32];
    apply_dual_kawase_pass(
        renderer,
        &mips[0],
        pong_texture,
        &super::DualKawaseUpsampleShader::get(renderer),
        half_texel,
        offset,
        input_size,
        mip_sizes[0],
    )?;

    tracing::trace!(
        levels = levels,
        dispatches = levels * 2 - 1,
        "Dual Kawase compute blur complete"
    );

    Ok(true)
}
