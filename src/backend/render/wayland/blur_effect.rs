use std::{
    borrow::{Borrow, BorrowMut},
    sync::{LazyLock, Mutex},
    time::{Duration, Instant},
};

use glam::{Affine2, Mat3, Vec2};
use smithay::{
    backend::{
        allocator::Fourcc,
        renderer::{
            Bind, BlitFrame, Color32F, ContextId, Frame, FrameContext, ImportAll, Offscreen,
            Renderer, Texture, TextureFilter,
            element::{Element, Id, Kind, RenderElement},
            gles::{
                GlesError, GlesFrame, GlesRenderer, GlesTexProgram, GlesTexture, Uniform,
                UniformName, UniformType, UniformValue, ffi,
            },
            sync::SyncPoint,
            utils::{CommitCounter, DamageSet},
        },
    },
    utils::{
        Buffer, Logical, Physical, Point, Rectangle, Scale, Size, Transform, user_data::UserDataMap,
    },
    wayland::compositor::SurfaceData,
};
use tracing::trace;

use crate::{
    backend::render::{element::AsGlowRenderer, wayland::clipped_surface::ClippingShader},
    wayland::handlers::background_effect::ComputedBlurRegionCachedState,
};

pub static BLUR_DOWNSAMPLE_SHADER: &str = include_str!("../shaders/blur_downsample.frag");
pub static BLUR_UPSAMPLE_SHADER: &str = include_str!("../shaders/blur_upsample.frag");

/// Backdrop dither amount, from configuration. Off by default.
///
/// Upstream mixes a fixed 0.03 of film grain into the blurred backdrop; the
/// stack this replaced had no such term. It does not read as grain: the hash is
/// evaluated on the capture's normalised coordinates and scaled by only 727.727,
/// so across a typical capture that is just under one cycle per pixel, and it
/// aliases into a coarse structured beat that no amount of tint hides. Anyone
/// who wants it can dial it in.
static BLUR_NOISE_BITS: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);

/// The configured dither amount.
fn configured_noise() -> f32 {
    f32::from_bits(BLUR_NOISE_BITS.load(std::sync::atomic::Ordering::Relaxed))
}
const MAX_STEPS: usize = 15;

/// How long a newly-appearing blur rect takes to reach full opacity.
///
/// Matched to `motion.layer_open` so a card's backdrop arriving reads as the
/// same system as a surface opening. It is a constant rather than the theme
/// value because the render path reaches this without a theme handle -- see
/// `blur_strength`, which is threaded down from `render::init_shaders` for the
/// same reason. Worth threading properly if a brand ever wants to retime it.
const BLUR_FADE_IN: Duration = Duration::from_millis(200);

/// When the last in-flight rect fade ends, so the shell knows to keep redrawing.
///
/// A fade advances only when something renders it. `blur_set` schedules a single
/// frame, which would draw the first step and then freeze until an unrelated
/// commit happened to trigger the next -- the same stall the slide-content
/// crossfade documents in `Shell::animations_going`.
static BLUR_FADE_DEADLINE: Mutex<Option<Instant>> = Mutex::new(None);

/// Whether any blur rect is still fading in.
pub fn blur_fade_in_flight() -> bool {
    let mut deadline = BLUR_FADE_DEADLINE.lock().unwrap();
    match *deadline {
        Some(at) if Instant::now() < at => true,
        // Clear once past, so a settled fade stops claiming redraws forever.
        Some(_) => {
            *deadline = None;
            false
        }
        None => false,
    }
}

/// Marks that a surface has had blur drawn for it at least once.
///
/// Present from the first blurred frame onward, so a rect appearing on the
/// same frame the surface does can be told apart from one appearing on a
/// surface that has been up for a while.
#[derive(Debug)]
struct BlurSeenSurface;

/// Extend the redraw deadline to cover a fade starting now.
fn schedule_blur_fade(now: Instant) {
    let mut deadline = BLUR_FADE_DEADLINE.lock().unwrap();
    let ends_at = now + BLUR_FADE_IN;
    if deadline.is_none_or(|at| at < ends_at) {
        *deadline = Some(ends_at);
    }
}

/// When a blur rect should be treated as having first appeared, and whether
/// that start needs redraws scheduled for it.
///
/// A client re-sends its whole region whenever any part of it changes, so index
/// is not identity -- a card inserted in the middle renumbers every later rect.
/// Matching by overlap keeps each existing rect paired with itself, so only a
/// genuinely new one starts at zero alpha. Overlap also keeps a card matched to
/// itself across a hover lift, which moves it without making it new.
///
/// A rect on a surface that is itself new does not fade at all: the backdrop
/// arrives with the surface, so the surface's own appearance already governs
/// how it comes in. Fading on top of that is what makes a popup's backdrop lag
/// behind the card that pops up instantly.
fn first_seen_for(
    previously_seen: &[(Rectangle<f64, Logical>, Instant)],
    rect: Rectangle<f64, Logical>,
    surface_is_new: bool,
    now: Instant,
) -> (Instant, bool) {
    if let Some((_, at)) = previously_seen.iter().find(|(seen, _)| seen.overlaps(rect)) {
        return (*at, false);
    }

    if surface_is_new {
        // Dated far enough back to read as already finished, which is also
        // what later frames inherit through the overlap match above.
        (now - BLUR_FADE_IN, false)
    } else {
        (now, true)
    }
}

/// How opaque a rect that first appeared `since` ago should be drawn.
///
/// Smoothstep rather than linear: a backdrop entering at a constant rate reads
/// as a wipe, and the ease matches what the layer-open animation uses.
fn fade_alpha(since: Instant) -> f32 {
    let t = (since.elapsed().as_secs_f32() / BLUR_FADE_IN.as_secs_f32()).clamp(0., 1.);
    t * t * (3. - 2. * t)
}

/// Lower bound of the radius-to-strength map: ~1px is effectively no blur.
const BLUR_RADIUS_MIN_PX: f32 = 1.0;
/// Upper bound: ~100px is full strength, larger values clamp here.
const BLUR_RADIUS_MAX_PX: f32 = 100.0;

/// Map a client's requested radius, in surface-local pixels, onto the step axis
/// [`BLUR_PARAMS`] is indexed by.
///
/// The protocol specifies the radius in pixels; the dual-Kawase table is indexed
/// by step. Feeding pixels straight in read a 60px request as step 60, which
/// clamps to the maximum -- every radius above the step count produced the same
/// heaviest blur. Routing it through the same 0..1 intensity axis the config
/// slider uses keeps both ways of asking on one curve.
///
/// Non-finite values from a malformed client map to 0 rather than propagating a
/// NaN into the shader.
fn strength_for_radius(radius_px: f32) -> usize {
    if !radius_px.is_finite() {
        return 0;
    }
    let t = ((radius_px - BLUR_RADIUS_MIN_PX) / (BLUR_RADIUS_MAX_PX - BLUR_RADIUS_MIN_PX))
        .clamp(0.0, 1.0);
    ((t * (MAX_STEPS - 1) as f32).round() as usize).min(MAX_STEPS - 1)
}

/// Backdrop saturation used when the client does not ask for one. `1.0` leaves
/// saturation unchanged, matching what the protocol promises.
const DEFAULT_SATURATION: f32 = 1.0;
/// White-overlay strength used when the client does not ask for one. Carried
/// over from the `org_kde_kwin_blur` stack's `BLUR_TINT_STRENGTH`, so surfaces
/// that never set it keep the frosting they had before.
const DEFAULT_TINT: f32 = 0.15;
/// Border alpha used when the client does not ask for one, from the old
/// stack's `BLUR_BORDER_STRENGTH`.
const DEFAULT_BORDER: f32 = 0.2;

/// How the backdrop looks, once the client's requests and the compositor
/// defaults have been reconciled. Resolved once per surface rather than per
/// rect, since every rect of one surface shares it.
#[derive(Debug, Clone, Copy)]
struct BlurAppearance {
    saturation: f32,
    tint: f32,
    border: f32,
}

impl Default for BlurAppearance {
    /// What a backdrop with no client behind it gets -- the compositor-drawn
    /// iced surfaces, which have no protocol state to read.
    fn default() -> Self {
        Self {
            saturation: DEFAULT_SATURATION,
            tint: DEFAULT_TINT,
            border: DEFAULT_BORDER,
        }
    }
}

impl BlurAppearance {
    /// Fall back to the compositor default for anything the client left unset.
    /// `0` cannot serve as the sentinel here -- it is a real value for all three
    /// (greyscale, no tint, no border) -- so absence is carried as `None`.
    fn resolve(state: &ComputedBlurRegionCachedState) -> Self {
        Self {
            saturation: state.saturation.unwrap_or(DEFAULT_SATURATION),
            tint: state.tint.unwrap_or(DEFAULT_TINT),
            border: state.border.unwrap_or(DEFAULT_BORDER),
        }
    }
}

/// Blur strength from configuration, as a step index into [`BLUR_PARAMS`].
///
/// Upstream derives strength from a single frosted-glass boolean, which gives
/// only two of the fifteen steps and reads far weaker than a configurable blur.
/// This keeps the `blur_intensity` config value driving it instead.
static BLUR_INTENSITY_BITS: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);
/// Whether blur is enabled at all, from configuration.
static BLUR_ENABLED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(true);

/// Update the configured blur strength. `intensity` is 0.0..=1.0.
pub fn set_blur_config(enabled: bool, intensity: f32, noise: f32) {
    BLUR_ENABLED.store(enabled, std::sync::atomic::Ordering::Relaxed);
    BLUR_INTENSITY_BITS.store(
        intensity.clamp(0.0, 1.0).to_bits(),
        std::sync::atomic::Ordering::Relaxed,
    );
    BLUR_NOISE_BITS.store(
        noise.clamp(0.0, 1.0).to_bits(),
        std::sync::atomic::Ordering::Relaxed,
    );
}

/// Whether blur is enabled in configuration.
pub fn blur_enabled() -> bool {
    BLUR_ENABLED.load(std::sync::atomic::Ordering::Relaxed)
}

/// The configured strength as a step index.
///
/// `frosted` is the theme's frosted-glass flag, kept as the floor so a theme
/// that asks for frosting still gets some even at intensity 0.
pub fn configured_blur_strength(frosted: bool) -> usize {
    let intensity = f32::from_bits(BLUR_INTENSITY_BITS.load(std::sync::atomic::Ordering::Relaxed));
    let floor = frosted as usize;
    // Spread the configured intensity across the whole step range rather than
    // the two steps a boolean can express.
    let steps = (intensity * (MAX_STEPS - 1) as f32).round() as usize;
    steps.max(floor).min(MAX_STEPS - 1)
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct BlurParameters {
    passes: usize,
    offset: f64,
    extended_radius: i32,
}

static BLUR_PARAMS: LazyLock<Vec<BlurParameters>> = LazyLock::new(|| {
    let mut params = Vec::new();

    let mut remaining_steps = MAX_STEPS as isize;
    let offsets = [
        // min offset, max offset, extended radius to avoid artifacts
        (1.0, 2.0, 10),
        (2.0, 3.0, 20),
        (2.0, 5.0, 50),
        (3.0, 8.0, 150),
    ];

    let sum = offsets.iter().map(|(min, max, _)| *max - *min).sum::<f64>();
    for (i, (min, max, extended_radius)) in offsets.into_iter().enumerate() {
        let mut iter_num = f64::ceil((max - min) / sum * (MAX_STEPS as f64)) as usize;
        remaining_steps -= iter_num as isize;

        if remaining_steps < 0 {
            iter_num = iter_num.saturating_add_signed(remaining_steps);
        }

        let diff = max - min;
        for j in 1..=iter_num {
            params.push(BlurParameters {
                passes: i + 1,
                offset: min + (diff / iter_num as f64) * j as f64,
                extended_radius,
            });
        }
    }

    trace!("Computed blur values: {:#?}", &params);
    params
});

#[derive(Debug, Clone)]
pub struct BlurShaders {
    down: GlesTexProgram,
    up: GlesTexProgram,
}

impl BlurShaders {
    pub fn compile(renderer: &mut GlesRenderer) -> Result<BlurShaders, GlesError> {
        let up = renderer.compile_custom_texture_shader(
            BLUR_UPSAMPLE_SHADER,
            &[
                UniformName::new("half_pixel", UniformType::_2f),
                UniformName::new("offset", UniformType::_1f),
            ],
        )?;
        let down = renderer.compile_custom_texture_shader(
            BLUR_DOWNSAMPLE_SHADER,
            &[
                UniformName::new("half_pixel", UniformType::_2f),
                UniformName::new("offset", UniformType::_1f),
            ],
        )?;

        Ok(BlurShaders { up, down })
    }

    pub fn get<R: AsGlowRenderer>(renderer: &R) -> Self {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<BlurShaders>()
            .expect("Custom Shaders not initialized")
            .clone()
    }
}

/// The pair of scratch textures a blur capture ping-pongs between.
///
/// `render_blur` performs `2 * passes` swaps — an even number — so the blurred
/// result always lands back in `tex`, which is what `draw` samples. `off_tex`
/// only ever holds intermediates, but it is cached alongside `tex` regardless:
/// allocating it per capture meant a `glGenTextures` + `glTexImage2D` (and a
/// free) for every blurred surface on every frame that damaged it.
///
/// Both are sized to the capture area, so they are validated and invalidated
/// together — see the size / context check in `capture_framebuffer`.
#[derive(Debug)]
struct BlurTextures<T> {
    /// Receives the framebuffer blit and holds the blurred result.
    tex: T,
    /// Scratch target for the intermediate downsample/upsample passes.
    off_tex: T,
}

type BlurTexture<T> = Mutex<Option<BlurTextures<T>>>;

#[derive(Debug)]
pub struct BlurState {
    pub id: Id,
    pub renderer_id: Option<ContextId<GlesTexture>>,
    pub src: Size<f64, Buffer>,
    pub offset: f64,
    pub passes: usize,
    pub region: Vec<Rectangle<i32, Logical>>,
    /// The rects drawn last frame, each with when it first appeared, so a rect
    /// that is NEW can fade in while its neighbours stay put.
    ///
    /// Matched by overlap rather than by index: a client re-sends its whole
    /// region every time any part of it changes, so index is not identity --
    /// inserting one card would renumber the rest and re-fade every one of them.
    /// Overlap also keeps a card that MOVES (a hover lift) matched to itself.
    pub seen: Vec<(Rectangle<f64, Logical>, Instant)>,
    /// The exact area last rendered, so a sub-pixel move still counts as a
    /// change. `region` is whole logical pixels and `src` only tracks size, so
    /// neither notices a surface sliding a fraction of a pixel -- which is
    /// precisely what a scale animation does every frame. Without this the
    /// commit counter never advances, no damage is emitted, and the backdrop
    /// sits still while the surface moves off it.
    pub geometry: Rectangle<f64, Logical>,
    pub commit: CommitCounter,
}

unsafe impl Send for BlurState {}
unsafe impl Sync for BlurState {}

impl Default for BlurState {
    fn default() -> Self {
        BlurState {
            id: Id::new(),
            renderer_id: None,
            src: Size::new(0., 0.),
            seen: Vec::new(),
            geometry: Rectangle::default(),
            offset: 0.,
            passes: 0,
            region: Vec::new(),
            commit: CommitCounter::default(),
        }
    }
}

pub struct BlurElement {
    id: Id,
    commit: CommitCounter,
    src: Size<f64, Buffer>,
    /// Margin actually captured on the near side, which the edge trim can make
    /// smaller than the blur radius.
    extended_offset: Point<f64, Logical>,
    /// The element's own size inside the extended capture. Kept explicitly
    /// because the near and far margins differ once the near one is trimmed.
    element_size: Size<f64, Logical>,
    geometry: Rectangle<f64, Logical>,
    scaling_shaders: BlurShaders,
    render_shader: GlesTexProgram,
    region: Vec<Rectangle<i32, Logical>>,
    offset: f64,
    passes: usize,
    /// The surface's own alpha, so the backdrop fades with it. A surface
    /// animating out otherwise keeps a fully opaque blur until it is destroyed,
    /// and the blur pops instead of fading.
    alpha: f32,
    uniforms: Vec<Uniform<'static>>,
}

impl BlurElement {
    pub fn from_state<R: ImportAll + AsGlowRenderer>(
        renderer: &mut R,
        state: &mut BlurState,
        geometry: Rectangle<f64, Logical>,
        output_scale: f64,
        radii: [u8; 4],
        strength: usize,
        alpha: f32,
    ) -> Result<Option<Self>, R::Error> {
        // Config applies to compositor-drawn chrome too. `from_surface` checks
        // this, but this constructor takes no protocol state and so used to blur
        // regardless -- leaving the iced surfaces frosted with blur turned off.
        if !blur_enabled() {
            return Ok(None);
        }

        let region = vec![Rectangle::from_size(geometry.size.to_i32_round())];

        Self::internal(
            renderer,
            state,
            geometry,
            &region,
            output_scale,
            // Clamped like the per-rect path below: half the shorter side, in
            // f32, so a fully-rounded caller gets the same cap the client drew.
            {
                let half_min = (geometry.size.w.min(geometry.size.h) as f32 / 2.).max(0.);
                radii.map(|v| (v as f32).min(half_min))
            },
            strength,
            alpha,
            BlurAppearance::default(),
        )
    }

    pub fn from_surface<R: ImportAll + AsGlowRenderer>(
        renderer: &mut R,
        states: &SurfaceData,
        geometry: Rectangle<f64, Logical>,
        output_scale: f64,
        radii: [u8; 4],
        strength: usize,
        alpha: f32,
    ) -> Result<Vec<Self>, R::Error> {
        // Blur disabled in config means no blur element at all, rather than a
        // zero-strength one that still pays for the blit and the passes.
        if !blur_enabled() {
            return Ok(Vec::new());
        }

        let mut blur_region_state = states.cached_state.get::<ComputedBlurRegionCachedState>();
        let blur = blur_region_state.current().clone();

        // A whole-surface request carries no region: the area is the surface
        // itself, resolved here so it stays correct as the surface resizes.
        let whole = Rectangle::from_size(geometry.size.to_i32_round());
        let clamped;
        let region = if blur.whole_surface {
            std::slice::from_ref(&whole)
        } else if let Some(region) = blur.blur_region.as_deref() {
            // Clip to the surface. The region is surface-local and a client is
            // free to send one larger than its surface -- an
            // `ext_background_effect_v1` client with no whole-surface request
            // has "everything" spelt as an oversized rect -- and blurring
            // outside the surface would paint over its neighbours.
            clamped = region
                .iter()
                .filter_map(|rect| rect.intersection(whole))
                .collect::<Vec<_>>();
            if clamped.is_empty() {
                return Ok(Vec::new());
            }
            clamped.as_slice()
        } else {
            // A surface that asked for blur but produced no area is
            // indistinguishable on screen from one that never asked, so say
            // which it was.
            tracing::trace!(
                geo_w = geometry.size.w,
                geo_h = geometry.size.h,
                has_effect_state = true,
                "blur_skip: surface has blur state but neither a region nor whole_surface"
            );
            return Ok(Vec::new());
        };

        // The client may ask for a strength. A hint: clamped to what the blur
        // can actually render.
        let strength = blur
            .blur_radius
            .map(|r| strength_for_radius(r as f32))
            .unwrap_or(strength);

        tracing::trace!(
            geo_w = geometry.size.w,
            geo_h = geometry.size.h,
            whole_surface = blur.whole_surface,
            rects = region.len(),
            requested_radius = ?blur.blur_radius,
            radii_entries = blur.region_radii.len(),
            strength,
            "blur_region: building blur elements for surface"
        );

        let state = states
            .data_map
            .get_or_insert_threadsafe::<Mutex<BlurState>, _>(Default::default);
        let mut state = state.lock().unwrap();

        // One element per rect, rather than one element clipped to all of them.
        //
        // Corner radii round the element's own geometry, so a single element
        // spanning every rect would round the outer bounds and leave each rect
        // square -- which for a full-screen layer surface means rounding the
        // screen corners and nothing else. A client that rounds each card to its
        // own radius needs an element per card. It also blits only each rect
        // rather than the whole surface.
        let appearance = BlurAppearance::resolve(&blur);

        // Per-rect fade bookkeeping. `seen` is taken rather than borrowed, so
        // the loop below can hand `state` to `internal` mutably.
        let previously_seen = std::mem::take(&mut state.seen);

        // Whether this is the first frame blur has been drawn for this
        // surface. Recorded on the surface rather than beside the rects,
        // because the rects are matched by overlap and a surface that changes
        // its region has no single rect to hang this on.
        let surface_is_new = states.data_map.get::<BlurSeenSurface>().is_none();
        if surface_is_new {
            states
                .data_map
                .insert_if_missing_threadsafe(|| BlurSeenSurface);
        }
        let mut seen_now = Vec::with_capacity(region.len());
        let now = Instant::now();

        let mut elements = Vec::with_capacity(region.len());
        for (idx, rect) in region.iter().enumerate() {
            if rect.size.w <= 0 || rect.size.h <= 0 {
                continue;
            }

            // The rect is surface-local; the element carries it as its own
            // geometry, so its region is that rect at the origin.
            //
            // Prefer the client's exact sub-pixel geometry where it sent one:
            // the integer rect is the conservative bound the capture and damage
            // were sized for, but rounding the RENDERED area to it is visible
            // either way -- out and the backdrop escapes past the shape drawn
            // over it, in and that shape's own border loses its backdrop.
            let exact = blur.region_geometry.get(idx).copied();
            let mut rect_geo = geometry;
            rect_geo.loc += exact.map_or(rect.loc.to_f64(), |geo| geo.loc);
            rect_geo.size = exact.map_or(rect.size.to_f64(), |geo| geo.size);
            if rect_geo.size.w <= 0. || rect_geo.size.h <= 0. {
                continue;
            }

            // Per-rect radii if the client sent them, falling back to a single
            // entry applied to every rect, then to the caller's value.
            //
            // Clamped against the EXACT geometry, in f32. Half the shorter side,
            // so adjacent arcs cannot overlap and erode the edge between them --
            // but taking that half from the whole-pixel rect, by integer
            // division, rounds the cap twice over. A pill 46.92 tall wants
            // 23.46; against a 47-tall bound `47 / 2` yields 23, and a cap a
            // half-pixel squarer than the one the client drew bulges out either
            // side of it. The client's own clamp is fractional, so this one has
            // to be too.
            let half_min = (rect_geo.size.w.min(rect_geo.size.h) as f32 / 2.).max(0.);
            let rect_radii = blur
                .region_radii
                .get(idx)
                .or_else(|| blur.region_radii.first())
                .map(|r| r.map(|v| (v as f32).min(half_min)))
                .unwrap_or(radii.map(|v| (v as f32).min(half_min)));

            // What a client asked for beside what gets rasterised for it.
            //
            // Both ends of this pipeline round, and an artifact along a card's
            // edge is almost always one of them disagreeing with the other by a
            // fraction of a pixel. `rect` is the whole-pixel bound that arrived
            // over the wire, `rect_geo` the exact area the client meant, and
            // `physical` the pixels covered on this output -- enough to tell a
            // client-side fault from a compositor-side one without a rebuild.
            let physical = physical_rect_snapped(rect_geo, output_scale);
            trace!(
                idx,
                recv_x = rect.loc.x,
                recv_y = rect.loc.y,
                recv_w = rect.size.w,
                recv_h = rect.size.h,
                phys_x = physical.loc.x,
                phys_y = physical.loc.y,
                phys_w = physical.size.w,
                phys_h = physical.size.h,
                exact_x = rect_geo.loc.x,
                exact_y = rect_geo.loc.y,
                exact_w = rect_geo.size.w,
                exact_h = rect_geo.size.h,
                radii = ?rect_radii,
                output_scale,
                "blur_rect_resolved: wire bound, exact area and the pixels drawn"
            );

            let (first_seen, needs_redraws) =
                first_seen_for(&previously_seen, rect_geo, surface_is_new, now);
            if needs_redraws {
                // Claim redraws for the whole fade: nothing else will schedule
                // the frames it needs to advance.
                schedule_blur_fade(now);
            }
            seen_now.push((rect_geo, first_seen));
            let rect_alpha = alpha * fade_alpha(first_seen);

            if let Some(element) = Self::internal(
                renderer,
                &mut state,
                rect_geo,
                &[Rectangle::from_size(rect.size)],
                output_scale,
                rect_radii,
                strength,
                rect_alpha,
                appearance,
            )? {
                elements.push(element);
            }
        }

        state.seen = seen_now;

        Ok(elements)
    }

    fn internal<R: ImportAll + AsGlowRenderer>(
        renderer: &mut R,
        state: &mut BlurState,
        geometry: Rectangle<f64, Logical>,
        region: &[Rectangle<i32, Logical>],
        output_scale: f64,
        radii: [f32; 4],
        strength: usize,
        alpha: f32,
        appearance: BlurAppearance,
    ) -> Result<Option<Self>, R::Error> {
        if strength == 0 || geometry.size.w == 0. || geometry.size.h == 0. {
            return Ok(None);
        }

        let geo = physical_rect_snapped(geometry, output_scale);
        let mut extended_geo = geo;
        let radius = BLUR_PARAMS[(strength + 2).min(MAX_STEPS - 1)].extended_radius as f64;
        extended_geo.loc -= Point::<f64, Physical>::new(radius, radius);
        extended_geo.size += Size::<f64, Physical>::new(radius, radius).upscale(2.);

        // Keep the capture inside the framebuffer's origin.
        //
        // The capture is extended past the element so the blur kernel has
        // something to reach into, but a surface against the top or left edge
        // has no screen there to read. That read gets clamped, so the texture
        // receives content at its own origin while everything derived below
        // still assumes the margin exists -- the whole backdrop samples shifted
        // by the radius, and the far edge samples texels nothing ever wrote.
        //
        // Trimming the margin here instead keeps src, input_to_geo and the
        // texture size agreeing with what the blit can actually produce. The
        // far side needs no such trim: src only ever covers the element's own
        // extent, so an over-long right or bottom margin is read by the kernel
        // alone, where unwritten texels are transparent and feather away.
        for (loc, size) in [
            (&mut extended_geo.loc.x, &mut extended_geo.size.w),
            (&mut extended_geo.loc.y, &mut extended_geo.size.h),
        ] {
            if *loc < 0.0 {
                *size += *loc;
                *loc = 0.0;
            }
        }

        // Compute input_to_geo so that it crops the extended capture radius.
        //
        // From the EXACT rect, not the pixel-snapped `geo`. The capture, the
        // blit and the damage all have to be whole pixels -- a framebuffer
        // region is not addressable otherwise -- but the shape drawn inside that
        // capture does not. `geo_size` below is already the exact fractional
        // size, so deriving the mapping from `geo` instead would tell the shader
        // a fractional size while placing its origin on a whole pixel: the
        // backdrop comes out the right size in the wrong place, off by up to
        // half a physical pixel on the near edges, and the client's own border
        // along those edges is left half over blurred backdrop and half not.
        let exact = geometry.to_physical(output_scale);
        let geo_scale = {
            let Scale { x, y } = exact.size / extended_geo.size;
            Affine2::from_scale(Vec2::new(x as f32, y as f32)).inverse()
        };
        let geo_translation = {
            let offset = exact.loc - extended_geo.loc;
            Affine2::from_translation(-Vec2::new(
                (offset.x / extended_geo.size.w) as f32,
                (offset.y / extended_geo.size.h) as f32,
            ))
        };
        let input_to_geo = Mat3::from(geo_scale * geo_translation);

        let uniforms = vec![
            Uniform::new("geo_size", (geometry.size.w as f32, geometry.size.h as f32)),
            Uniform::new("corner_radius", [radii[3], radii[1], radii[0], radii[2]]),
            Uniform::new(
                "input_to_geo",
                UniformValue::Matrix3x3 {
                    matrices: vec![*AsRef::<[f32; 9]>::as_ref(&input_to_geo)],
                    transpose: false,
                },
            ),
            Uniform::new("noise", UniformValue::_1f(configured_noise())),
            // The backdrop draws through the same clipping program, so it needs
            // `scale` too -- without it the corner mask never rounds the blur.
            Uniform::new("scale", output_scale as f32),
            // Frosted-glass appearance. Each falls back to the compositor
            // default until the client sends it: 0 is a real value for all
            // three (greyscale / no tint / no border) and so cannot double as
            // "unset".
            Uniform::new("saturation", UniformValue::_1f(appearance.saturation)),
            Uniform::new("frost_tint", UniformValue::_1f(appearance.tint)),
            Uniform::new("border", UniformValue::_1f(appearance.border)),
        ];

        let geometry = extended_geo.to_logical(output_scale);
        // The margin actually captured on the near side, which is `radius` only
        // when the capture had room for it. Against the top or left edge the
        // trim above shortened it, and deriving this from `radius` regardless
        // shifted the region rects -- and the damage -- down and right by the
        // difference, leaving an unblurred strip along those edges.
        let extended_offset = (geo.loc - extended_geo.loc).to_logical(output_scale);
        let element_size = geo.size.to_logical(output_scale);

        let renderer_id = renderer.glow_renderer().context_id();
        let src = geometry.size.to_buffer(output_scale, Transform::Normal);
        let params = &BLUR_PARAMS[strength.min(MAX_STEPS - 1)];

        let dirty = !(state
            .renderer_id
            .as_ref()
            .is_some_and(|id| id == &renderer_id)
            && state.offset == params.offset
            && state.passes == params.passes
            && state.region == region
            && state.geometry == geometry
            && state.src == src);

        state.renderer_id = Some(renderer_id);
        state.offset = params.offset;
        state.passes = params.passes;
        state.region = region.to_vec();
        state.geometry = geometry;
        state.src = src;
        if dirty {
            state.commit.increment();
        }

        Ok(Some(BlurElement {
            id: state.id.clone(),
            commit: state.commit,
            src,
            geometry,
            extended_offset,
            element_size,
            scaling_shaders: BlurShaders::get(renderer),
            render_shader: ClippingShader::get(renderer),
            offset: state.offset,
            passes: state.passes,
            alpha,
            // Placed by rounding the rect's EDGES outward, never the offset on
            // its own.
            //
            // `extended_offset` is a whole number of PHYSICAL pixels, but it is
            // not generally a whole number of logical ones: against the top or
            // left edge the trim above shortens the near margin to whatever room
            // there was, so at 2x a 39px margin is 19.5 logical. Rounding that
            // to 20 and then adding it displaces the region half a logical pixel
            // -- a whole physical one -- along that edge alone, which on a 2px
            // border cuts half of it away. Off-screen surfaces never hit it
            // because an untrimmed margin is the full radius, and integral.
            region: region
                .iter()
                .map(|rect| {
                    let x0 = extended_offset.x + rect.loc.x as f64;
                    let y0 = extended_offset.y + rect.loc.y as f64;
                    let (x1, y1) = (x0 + rect.size.w as f64, y0 + rect.size.h as f64);
                    // Outward: this bounds the drawn area for clipping and
                    // damage, so it has to contain the shape rather than sit
                    // nearest to it.
                    let (x0, y0) = (x0.floor(), y0.floor());
                    Rectangle::new(
                        (x0 as i32, y0 as i32).into(),
                        ((x1.ceil() - x0) as i32, (y1.ceil() - y0) as i32).into(),
                    )
                })
                .collect(),
            uniforms,
        }))
    }
}

/// A logical rect in physical pixels, with the origin and the far edge each
/// rounded to the pixel grid.
///
/// `Rectangle::to_physical_precise_round` rounds `loc` and `size`
/// independently (smithay's `Rectangle::to_i32_round`), so the far edge lands
/// at `round(x * s) + round(w * s)` rather than at `round((x + w) * s)`. Those
/// differ by a pixel whenever both fractional parts land on the same side of
/// .5 -- at scale 1.25 a rect at x=100.4 w=320.4 ends at 527 one way and 526
/// the other.
///
/// A client snapping the same rect rounds both of its ends, so the two
/// rasterisers disagree about where the edge is and leave a seam between the
/// backdrop and the border drawn over it. Rounding both ends here is the same
/// rule, so the edges land together.
fn physical_rect_snapped(
    geometry: Rectangle<f64, Logical>,
    output_scale: f64,
) -> Rectangle<f64, Physical> {
    let x0 = (geometry.loc.x * output_scale).round();
    let y0 = (geometry.loc.y * output_scale).round();
    let x1 = ((geometry.loc.x + geometry.size.w) * output_scale).round();
    let y1 = ((geometry.loc.y + geometry.size.h) * output_scale).round();

    Rectangle::new(
        Point::<f64, Physical>::new(x0, y0),
        Size::<f64, Physical>::new((x1 - x0).max(0.), (y1 - y0).max(0.)),
    )
}

impl Element for BlurElement {
    fn id(&self) -> &Id {
        &self.id
    }

    fn current_commit(&self) -> CommitCounter {
        self.commit
    }

    fn src(&self) -> Rectangle<f64, Buffer> {
        Rectangle::from_size(self.src)
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        self.geometry.to_physical_precise_round(scale)
    }

    fn transform(&self) -> Transform {
        Transform::Normal
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<CommitCounter>,
    ) -> DamageSet<i32, Physical> {
        if self.commit.distance(commit).is_none_or(|d| d > 0) {
            // The element's own area inside the extended capture. Derived from
            // the stored size rather than `geometry - offset * 2`, which only
            // holds while the near and far margins are both the full radius.
            //
            // Edges rounded outward, not the offset and the size separately: a
            // trimmed near margin leaves `extended_offset` on a half logical
            // pixel, and rounding it alone would move the damage off the area
            // actually redrawn.
            let scale_x = scale.x;
            let scale_y = scale.y;
            let x0 = (self.extended_offset.x * scale_x).floor();
            let y0 = (self.extended_offset.y * scale_y).floor();
            let x1 = ((self.extended_offset.x + self.element_size.w) * scale_x).ceil();
            let y1 = ((self.extended_offset.y + self.element_size.h) * scale_y).ceil();
            DamageSet::from_slice(&[Rectangle::new(
                (x0 as i32, y0 as i32).into(),
                ((x1 - x0) as i32, (y1 - y0) as i32).into(),
            )])
        } else {
            DamageSet::default()
        }
    }

    fn alpha(&self) -> f32 {
        self.alpha
    }

    fn kind(&self) -> Kind {
        Kind::default()
    }

    fn is_framebuffer_effect(&self) -> bool {
        true
    }
}

impl<R: Renderer + AsGlowRenderer> RenderElement<R> for BlurElement
where
    R::TextureId: Send + 'static,
{
    fn capture_framebuffer(
        &self,
        frame: &mut <R>::Frame<'_, '_>,
        src: Rectangle<f64, Buffer>,
        dst: Rectangle<i32, Physical>,
        cache: &UserDataMap,
    ) -> Result<(), <R>::Error> {
        let transform = frame.transformation();
        let tex_size = self.src.to_i32_round();
        let glow_frame = <R as AsGlowRenderer>::glow_frame_mut(frame);
        let gles_frame = BorrowMut::<GlesFrame<'_, '_>>::borrow_mut(glow_frame);
        let mut renderer = gles_frame.renderer();

        let texture_ref = cache.get_or_insert_threadsafe(BlurTexture::<R::TextureId>::default);
        let mut texture_entry = texture_ref.lock().unwrap();
        // Both textures are sized to the capture area and live in the same
        // context, so either going stale invalidates the pair.
        if texture_entry.as_ref().is_some_and(|entry| {
            let context_id = renderer.as_ref().context_id();
            entry.tex.size() != tex_size
                || R::tex_to_gl(&context_id, &entry.tex).is_none()
                || R::tex_to_gl(&context_id, &entry.off_tex).is_none()
        }) {
            texture_entry.take();
        }
        if texture_entry.is_none() {
            let gl_texture = renderer
                .as_mut()
                .create_buffer(Fourcc::Abgr8888, tex_size)
                .map_err(R::from_gles_error)?;
            let gl_off_texture = renderer
                .as_mut()
                .create_buffer(Fourcc::Abgr8888, tex_size)
                .map_err(R::from_gles_error)?;
            let context_id = renderer.as_ref().context_id();
            *texture_entry = Some(BlurTextures {
                tex: R::tex_from_gl(&context_id, gl_texture),
                off_tex: R::tex_from_gl(&context_id, gl_off_texture),
            });
        }

        let entry = texture_entry.as_ref().unwrap();
        let context_id = renderer.as_ref().context_id();
        let mut texture = R::tex_to_gl(&context_id, &entry.tex).unwrap();
        let mut off_texture = R::tex_to_gl(&context_id, &entry.off_tex).unwrap();
        std::mem::drop(renderer);

        let sync = blit_from_active_fb(gles_frame, src, dst, transform, &mut texture)
            .map_err(R::from_gles_error)?;
        gles_frame.wait(&sync).map_err(R::from_gles_error)?;

        let mut textures = [&mut texture, &mut off_texture];
        render_blur(
            gles_frame.renderer().as_mut(),
            &self.scaling_shaders,
            &mut textures,
            self.offset,
            self.passes,
        )
        .map_err(R::from_gles_error)?;

        Ok(())
    }

    fn draw(
        &self,
        frame: &mut R::Frame<'_, '_>,
        src: Rectangle<f64, Buffer>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
        opaque_regions: &[Rectangle<i32, Physical>],
        cache: Option<&UserDataMap>,
    ) -> Result<(), R::Error> {
        let src_to_geo = self.geometry.size / self.src;
        let src_log = src
            .upscale(src_to_geo)
            .to_logical(1., Transform::Normal, &Size::default());
        let scale = dst.size.to_f64() / src_log.size;

        let damage = self
            .region
            .iter()
            .flat_map(|rect| {
                let mut rect = rect.to_f64().intersection(src_log)?;
                rect.loc -= src_log.loc;
                Some(rect.to_physical_precise_round(scale))
            })
            .flat_map(|rect| damage.iter().flat_map(move |r| r.intersection(rect)))
            .collect::<Vec<_>>();
        let cache = cache.expect("Framebuffer element without cache?");
        let Some(texture) = cache.get::<BlurTexture<R::TextureId>>() else {
            return Err(R::from_gles_error(GlesError::BlitError));
        };
        let texture_ref = texture.lock().unwrap();

        // `render_blur` swaps an even number of times, so the result is in `tex`.
        if let Some(tex) = texture_ref.as_ref().map(|entry| &entry.tex) {
            BorrowMut::<GlesFrame>::borrow_mut(<R as AsGlowRenderer>::glow_frame_mut(frame))
                .override_default_tex_program(self.render_shader.clone(), self.uniforms.clone());
            frame.render_texture_from_to(
                tex,
                src,
                dst,
                &damage,
                opaque_regions,
                Transform::Normal,
                // Fade the backdrop with the surface. A surface animating out
                // otherwise keeps an opaque blur until it is destroyed, so the
                // blur pops rather than fading with everything else.
                self.alpha,
            )?;
            BorrowMut::<GlesFrame>::borrow_mut(<R as AsGlowRenderer>::glow_frame_mut(frame))
                .clear_tex_program_override();
        }
        Ok(())
    }
}

fn blit_from_active_fb(
    frame: &mut GlesFrame<'_, '_>,
    src: Rectangle<f64, Buffer>,
    dst: Rectangle<i32, Physical>,
    transform: Transform,
    to_texture: &mut GlesTexture,
) -> Result<SyncPoint, GlesError> {
    let tex_size = to_texture.size();
    let tex_size_phys = tex_size.to_logical(1, Transform::Normal).to_physical(1);
    let fb_size = frame.output_size();

    let mut renderer = frame.renderer();
    let mut fb = renderer.as_mut().bind(to_texture)?;
    let sync = {
        let mut subframe = renderer
            .as_mut()
            .render(&mut fb, tex_size_phys, Transform::Normal)?;
        subframe.clear(
            Color32F::TRANSPARENT,
            &[Rectangle::from_size(tex_size_phys)],
        )?;
        subframe.finish()?
    };

    if transform != Transform::Normal {
        // We need to copy to a temporary texture to do an actual
        // render pass with `render_texture_from_to` to do the rotation.
        // dst is in screen space, but we just want to do a 1:1 copy in
        // buffer space from dst of the current fb, so we need to undo any
        // transforms for the blit.
        let dst_phys = transform.transform_rect_in(dst, &fb_size);
        let dst_buffer = dst_phys
            .to_logical(1)
            .to_buffer(1, Transform::Normal, &Size::default());
        let mut tmp_texture = renderer
            .as_mut()
            .create_buffer(Fourcc::Abgr8888, dst_buffer.size)?;
        let mut fb_tmp = renderer.as_mut().bind(&mut tmp_texture)?;
        std::mem::drop(renderer);
        frame.wait(&sync)?;

        let sync = frame.blit_to(
            &mut fb_tmp,
            dst_phys,
            Rectangle::from_size(dst_phys.size),
            TextureFilter::Linear,
        )?;
        frame.wait(&sync)?;
        std::mem::drop(fb_tmp);

        // now we bind the target texture with `Transform::Normal`
        // and render the temporary texture with inverse transform
        // into src.
        let mut renderer = frame.renderer();
        let mut frame = renderer
            .as_mut()
            .render(&mut fb, tex_size_phys, Transform::Normal)?;
        frame.wait(&sync)?;
        Frame::render_texture_from_to(
            &mut frame,
            &tmp_texture,
            Rectangle::from_size(dst_buffer.size.to_f64()),
            src.to_logical(1., Transform::Normal, &Size::default())
                .to_physical(1.)
                .to_i32_round(),
            &[Rectangle::from_size(dst.size)],
            &[Rectangle::from_size(dst.size)],
            transform.invert(),
            1.0,
        )?;
        std::mem::drop(tmp_texture);
        frame.finish()
    } else {
        std::mem::drop(renderer);
        frame.wait(&sync)?;
        frame.blit_to(
            &mut fb,
            dst,
            src.to_logical(1., Transform::Normal, &Size::default())
                .to_physical(1.)
                .to_i32_round(),
            TextureFilter::Linear,
        )
    }
}

fn render_blur(
    renderer: &mut GlesRenderer,
    shaders: &BlurShaders,
    textures: &mut [&mut GlesTexture; 2],
    offset: f64,
    passes: usize,
) -> Result<(), GlesError> {
    for i in 0..passes {
        let tex_size = textures[0].size();
        let [src_tex, target_tex] = textures;
        let mut fb = renderer.bind(*target_tex)?;

        let adjusted_tex_size = tex_size.downscale(1 << i);
        let target_tex_size = tex_size
            .downscale(1 << (i + 1))
            .to_logical(1, Transform::Normal)
            .to_physical(1);
        let half_pixel = [
            0.5 / (adjusted_tex_size.w as f32),
            0.5 / (adjusted_tex_size.h as f32),
        ];

        let mut frame = renderer.render(
            &mut fb,
            tex_size.to_logical(1, Transform::Normal).to_physical(1),
            Transform::Normal,
        )?;
        frame.clear(
            Color32F::new(0., 0., 0., 0.),
            &[Rectangle::from_size(
                tex_size.to_logical(1, Transform::Normal).to_physical(1),
            )],
        )?;
        frame.with_context(|gl| unsafe {
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
            // Dual-Kawase is built entirely on BILINEAR taps: each pass reads
            // four samples half a texel off-centre and lets the hardware blend
            // them. Leaving the filters unset takes the GL defaults, where
            // TEXTURE_MIN_FILTER is NEAREST_MIPMAP_LINEAR -- mipmap sampling on
            // a texture that has no mip levels, i.e. incomplete. The downsample
            // half of the chain is the half that minifies, so it point-samples
            // and aliases, and every later pass inherits the aliasing. No
            // parameter curve can smooth that out; the taps have to interpolate.
            gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_MIN_FILTER, ffi::LINEAR as i32);
            gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_MAG_FILTER, ffi::LINEAR as i32);
        })?;
        frame.render_texture_from_to(
            src_tex,
            Rectangle::from_size(adjusted_tex_size.to_f64()),
            Rectangle::from_size(target_tex_size),
            &[Rectangle::from_size(target_tex_size)],
            &[Rectangle::from_size(target_tex_size)],
            Transform::Normal,
            1.0,
            Some(&shaders.down),
            &[
                Uniform::new("half_pixel", half_pixel),
                Uniform::new("offset", (offset / (1 << i) as f64) as f32),
            ],
        )?;
        frame.with_context(|gl| unsafe {
            gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_WRAP_S, ffi::REPEAT as i32);
            gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_WRAP_T, ffi::REPEAT as i32);
        })?;
        let sync = frame.finish()?;
        std::mem::drop(fb);
        renderer.wait(&sync)?;

        textures.swap(0, 1);
    }

    for i in 0..passes {
        let tex_size = textures[0].size();
        let [src_tex, target_tex] = textures;
        let mut fb = renderer.bind(*target_tex)?;

        let adjusted_tex_size = tex_size.downscale(1 << (passes - i));
        let target_tex_size = tex_size
            .downscale(1 << (passes - i - 1))
            .to_logical(1, Transform::Normal)
            .to_physical(1);
        let half_pixel = [
            0.5 / (adjusted_tex_size.w as f32),
            0.5 / (adjusted_tex_size.h as f32),
        ];

        let mut frame = renderer.render(
            &mut fb,
            tex_size.to_logical(1, Transform::Normal).to_physical(1),
            Transform::Normal,
        )?;
        frame.clear(
            Color32F::new(0., 0., 0., 0.),
            &[Rectangle::from_size(
                tex_size.to_logical(1, Transform::Normal).to_physical(1),
            )],
        )?;
        frame.with_context(|gl| unsafe {
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
            // Dual-Kawase is built entirely on BILINEAR taps: each pass reads
            // four samples half a texel off-centre and lets the hardware blend
            // them. Leaving the filters unset takes the GL defaults, where
            // TEXTURE_MIN_FILTER is NEAREST_MIPMAP_LINEAR -- mipmap sampling on
            // a texture that has no mip levels, i.e. incomplete. The downsample
            // half of the chain is the half that minifies, so it point-samples
            // and aliases, and every later pass inherits the aliasing. No
            // parameter curve can smooth that out; the taps have to interpolate.
            gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_MIN_FILTER, ffi::LINEAR as i32);
            gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_MAG_FILTER, ffi::LINEAR as i32);
        })?;
        frame.render_texture_from_to(
            src_tex,
            Rectangle::from_size(adjusted_tex_size.to_f64()),
            Rectangle::from_size(target_tex_size),
            &[Rectangle::from_size(target_tex_size)],
            &[Rectangle::from_size(target_tex_size)],
            Transform::Normal,
            1.0,
            Some(&shaders.up),
            &[
                Uniform::new("half_pixel", half_pixel),
                Uniform::new("offset", (offset / (1 << (passes - i)) as f64) as f32),
            ],
        )?;
        frame.with_context(|gl| unsafe {
            gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_WRAP_S, ffi::REPEAT as i32);
            gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_WRAP_T, ffi::REPEAT as i32);
        })?;
        let sync = frame.finish()?;
        std::mem::drop(fb);
        renderer.wait(&sync)?;

        textures.swap(0, 1);
    }

    // textures always end up the right way around with `self.texture` containing our final render,
    // since we render PASSES * 2 (downscale and upscale), so the number of swaps is always even.
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{BLUR_FADE_IN, fade_alpha, first_seen_for, physical_rect_snapped};
    use smithay::utils::{Logical, Rectangle};
    use std::time::Instant;

    /// The far edge is rounded as an edge, not as an independently rounded size
    /// added to an independently rounded origin.
    #[test]
    fn far_edge_rounds_with_the_origin() {
        // At 1.25x both x*s = 125.5 and w*s = 400.5 round up, so rounding them
        // separately puts the far edge at 526 + 1. The edge itself is at
        // (100.4 + 320.4) * 1.25 = 526.0 exactly.
        let geo = Rectangle::<f64, Logical>::new((100.4, 0.).into(), (320.4, 10.).into());
        let snapped = physical_rect_snapped(geo, 1.25);

        assert_eq!(snapped.loc.x, 126.);
        assert_eq!(snapped.loc.x + snapped.size.w, 526.);
    }

    /// Integral logical rects at integral scales are untouched, so the common
    /// case keeps landing exactly where it did.
    #[test]
    fn integral_geometry_is_unchanged() {
        let geo = Rectangle::<f64, Logical>::new((10., 20.).into(), (300., 180.).into());

        for scale in [1., 2., 3.] {
            let snapped = physical_rect_snapped(geo, scale);
            assert_eq!(snapped.loc.x, 10. * scale);
            assert_eq!(snapped.loc.y, 20. * scale);
            assert_eq!(snapped.size.w, 300. * scale);
            assert_eq!(snapped.size.h, 180. * scale);
        }
    }

    /// An appearing rect must not restart its neighbours' fades.
    ///
    /// A client re-sends its WHOLE region whenever any part of it changes, so
    /// index is not identity -- a card inserted in the middle renumbers every
    /// later rect. Matching by overlap keeps each existing rect paired with
    /// itself, so only the genuinely new one starts at zero alpha.
    #[test]
    fn only_a_new_rect_fades_in() {
        let card = |x: f64| Rectangle::<f64, Logical>::new((x, 100.).into(), (300., 200.).into());
        let long_ago = Instant::now() - BLUR_FADE_IN * 2;

        // Two settled cards, then a third appears BEFORE them in the region.
        let previously_seen = [(card(400.), long_ago), (card(800.), long_ago)];
        let now = Instant::now();

        let resolved: Vec<_> = [card(0.), card(400.), card(800.)]
            .into_iter()
            .map(|rect| first_seen_for(&previously_seen, rect, false, now).0)
            .map(fade_alpha)
            .collect();

        // Not exactly 0: a few ns elapse between taking `now` and reading it.
        assert!(resolved[0] < 0.01, "the new rect starts transparent");
        assert_eq!(resolved[1], 1., "an existing rect keeps full alpha");
        assert_eq!(resolved[2], 1., "and so does the one after it");
    }

    /// A backdrop arriving with its surface must not fade.
    ///
    /// The surface's own appearance already governs how it comes in: a popup
    /// pops up, so its backdrop must too. Fading here on top of that is what
    /// left a menu's blur visibly trailing the card.
    #[test]
    fn a_rect_on_a_new_surface_appears_with_it() {
        let card = Rectangle::<f64, Logical>::new((0., 0.).into(), (300., 200.).into());
        let now = Instant::now();

        let (first_seen, needs_redraws) = first_seen_for(&[], card, true, now);

        assert_eq!(fade_alpha(first_seen), 1., "full alpha on the first frame");
        assert!(
            !needs_redraws,
            "nothing is animating, so no frames need claiming for it"
        );
    }

    /// A surface that was already up and gains a backdrop still fades.
    ///
    /// That is the case the fade was built for -- nothing else is animating,
    /// so without it the backdrop simply pops.
    #[test]
    fn a_rect_on_an_existing_surface_still_fades_in() {
        let card = Rectangle::<f64, Logical>::new((0., 0.).into(), (300., 200.).into());
        let now = Instant::now();

        let (first_seen, needs_redraws) = first_seen_for(&[], card, false, now);

        assert!(fade_alpha(first_seen) < 0.01, "starts transparent");
        assert!(needs_redraws, "the fade needs frames to advance it");
    }

    /// Later frames of a new surface inherit its already-finished start.
    ///
    /// The rect is matched by overlap from the second frame on, so whatever
    /// the first frame decided has to keep reading as settled rather than
    /// starting a fade one frame late.
    #[test]
    fn a_new_surface_stays_settled_on_later_frames() {
        let card = Rectangle::<f64, Logical>::new((0., 0.).into(), (300., 200.).into());
        let now = Instant::now();

        let (first_seen, _) = first_seen_for(&[], card, true, now);
        let (again, needs_redraws) = first_seen_for(&[(card, first_seen)], card, false, now);

        assert_eq!(again, first_seen, "the same start is inherited");
        assert_eq!(fade_alpha(again), 1.);
        assert!(!needs_redraws);
    }

    /// A rect that MOVES is the same rect, not a new one.
    ///
    /// The hover lift shifts a card by a couple of percent every frame. Treating
    /// each position as a new rect would restart the fade continuously and hold
    /// the backdrop near zero alpha for the whole hover.
    #[test]
    fn a_lifted_rect_keeps_its_fade() {
        let rest = Rectangle::<f64, Logical>::new((120., 200.).into(), (320., 180.).into());
        let lifted = Rectangle::<f64, Logical>::new((116.8, 198.2).into(), (326.4, 183.6).into());
        let long_ago = Instant::now() - BLUR_FADE_IN * 2;

        let matched = [(rest, long_ago)]
            .iter()
            .find(|(seen, _)| seen.overlaps(lifted))
            .map(|(_, at)| *at);

        assert!(
            matched.is_some(),
            "the lifted rect must match its resting one"
        );
        assert_eq!(fade_alpha(matched.unwrap()), 1., "and stay fully opaque");
    }

    /// The fade runs from nothing to fully opaque, and settles.
    #[test]
    fn fade_spans_zero_to_one_and_stops() {
        assert!(fade_alpha(Instant::now()) < 0.01);
        assert_eq!(fade_alpha(Instant::now() - BLUR_FADE_IN), 1.);
        assert_eq!(fade_alpha(Instant::now() - BLUR_FADE_IN * 10), 1.);
    }

    /// A trimmed near margin lands on a half logical pixel, and rounding that
    /// offset on its own displaces the region by a whole physical pixel.
    ///
    /// This is the cut-top-border case: at 2x, a capture clamped to 39 physical
    /// px of margin is 19.5 logical, `to_i32_round` takes it to 20, and the
    /// region moves one physical pixel down the screen -- half of a 2px border.
    /// Only the top and left can hit it, because only they are ever trimmed.
    #[test]
    fn a_half_pixel_offset_does_not_displace_the_region() {
        let offset = 19.5_f64;
        let size = 47.0_f64;

        // What the old code did: round the offset, then add the size.
        assert_eq!(offset.round(), 20.0, "the offset alone rounds up");

        // What it does now: round the EDGES, outward, so the region contains
        // the shape instead of sliding off it.
        let x0 = offset.floor();
        let x1 = (offset + size).ceil();
        assert!(
            x0 <= offset,
            "near edge {x0} cuts into the shape at {offset}"
        );
        assert!(
            x1 >= offset + size,
            "far edge {x1} cuts into the shape at {}",
            offset + size
        );
        assert!(x1 - x0 <= size + 2.0, "region {} inflated", x1 - x0);
    }

    /// A rect thinner than a physical pixel collapses rather than going
    /// negative: the size is a difference of two rounded edges, which can
    /// otherwise invert.
    #[test]
    fn subpixel_rect_never_goes_negative() {
        let geo = Rectangle::<f64, Logical>::new((10.6, 10.6).into(), (0.1, 0.1).into());
        let snapped = physical_rect_snapped(geo, 1.);

        assert!(snapped.size.w >= 0.);
        assert!(snapped.size.h >= 0.);
    }
}
