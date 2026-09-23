// SPDX-License-Identifier: GPL-3.0-only

//! Conservative CVT reduced-blanking fallback modes for panel-fit connectors.

use smithay::reexports::drm::control::{Mode, ModeFlags, ModeTypeFlags};

use crate::backend::kms::drm_helpers::calculate_refresh_rate;

pub const SYNC_TOLERANCE_MHZ: u32 = 1;
pub const REFRESH_MATCH_TOLERANCE_MHZ: u32 = 100;

const COMMON_RESOLUTIONS: &[(u16, u16)] = &[
    // 4:3
    (800, 600),
    (1024, 768),
    (1152, 864),
    (1280, 960),
    (1400, 1050),
    (1440, 1080),
    (1600, 1200),
    (1920, 1440),
    (2048, 1536),
    // 16:10
    (1280, 800),
    (1440, 900),
    (1680, 1050),
    (1920, 1200),
    (2560, 1600),
    // 16:9
    (1280, 720),
    (1366, 768),
    (1600, 900),
    (1920, 1080),
    (2048, 1152),
    (2560, 1440),
    (2880, 1620),
    (3200, 1800),
    (3840, 2160),
    (4096, 2304),
    (5120, 2880),
];
const COMMON_REFRESH_RATES: &[u32] = &[60, 90, 120, 144, 165, 240];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModeSource {
    Driver,
    FallbackLandscape,
    FallbackPortrait,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct KmsMode {
    pub mode: Mode,
    pub source: ModeSource,
    pub preferred: bool,
}

impl KmsMode {
    pub fn is_fallback(self) -> bool {
        !matches!(self.source, ModeSource::Driver)
    }
}

/// Build CVT-RB v1 timings. This is the reduced-blanking timing family used by
/// Mutter for refresh rates divisible by 60; it is suitable for panel fitting,
/// not a claim that arbitrary external displays accept CVT timings.
fn cvt_rb_mode(width: u16, height: u16, refresh_hz: u32) -> Mode {
    debug_assert_eq!(refresh_hz % 60, 0);
    let hdisplay = width / 8 * 8;
    let htotal = hdisplay + 160;
    let vfront_porch: u16 = 3;
    let vsync: u16 = 6;
    // CVT-RB requires at least 460 us vertical blanking. Clock is rounded
    // down to the 250 kHz granularity used by cvt.
    let h_period_ns = (1_000_000_000u64 / refresh_hz as u64 - 460_000)
        / (height as u64 + vfront_porch as u64 + vsync as u64);
    let vblank = (460_000u64 / h_period_ns + 1) as u16;
    let vtotal = height + vblank;
    let clock = ((htotal as u64 * vtotal as u64 * refresh_hz as u64 / 1_000) / 250 * 250) as u32;

    let mut raw = drm_ffi::drm_mode_modeinfo {
        clock,
        hdisplay,
        hsync_start: hdisplay + 48,
        hsync_end: hdisplay + 80,
        htotal,
        hskew: 0,
        vdisplay: height,
        vsync_start: height + vfront_porch,
        vsync_end: height + vfront_porch + vsync,
        vtotal,
        vscan: 0,
        vrefresh: 0,
        flags: (ModeFlags::PHSYNC | ModeFlags::NVSYNC).bits(),
        type_: ModeTypeFlags::empty().bits(),
        name: [0; 32],
    };
    let name = format!("{width}x{height}R");
    for (dst, src) in raw.name.iter_mut().zip(name.bytes()) {
        *dst = src as i8;
    }
    raw.into()
}

pub fn fallback_catalog() -> impl Iterator<Item = KmsMode> {
    COMMON_RESOLUTIONS.iter().flat_map(|&(width, height)| {
        COMMON_REFRESH_RATES
            .iter()
            .copied()
            .filter(|refresh| refresh % 60 == 0)
            .flat_map(move |refresh| {
                let landscape = KmsMode {
                    mode: cvt_rb_mode(width, height, refresh),
                    source: ModeSource::FallbackLandscape,
                    preferred: false,
                };
                let portrait = KmsMode {
                    mode: cvt_rb_mode(height, width, refresh),
                    source: ModeSource::FallbackPortrait,
                    preferred: false,
                };
                [landscape, portrait]
            })
    })
}

pub fn fallback_modes(
    real_modes: &[KmsMode],
    scaling_supported: bool,
    tiled: bool,
) -> Vec<KmsMode> {
    if real_modes.is_empty() || !scaling_supported || tiled {
        return Vec::new();
    }

    let (max_width, max_height, max_refresh, max_clock) = real_modes.iter().fold(
        (0u16, 0u16, 60_000u32, 0u32),
        |(width, height, refresh, clock), mode| {
            (
                width.max(mode.mode.size().0),
                height.max(mode.mode.size().1),
                refresh.max(calculate_refresh_rate(mode.mode)),
                clock.max(mode.mode.clock()),
            )
        },
    );
    let landscape = max_width > max_height;

    fallback_catalog()
        .filter(|candidate| matches!(candidate.source, ModeSource::FallbackLandscape) == landscape)
        .filter(|candidate| {
            let (width, height) = candidate.mode.size();
            let refresh = calculate_refresh_rate(candidate.mode);
            width <= max_width
                && height <= max_height
                && refresh <= max_refresh + SYNC_TOLERANCE_MHZ
                && candidate.mode.clock() <= max_clock
        })
        .filter(|candidate| {
            let (width, height) = candidate.mode.size();
            let refresh = calculate_refresh_rate(candidate.mode);
            !real_modes.iter().any(|real| {
                real.mode.size() == (width, height)
                    && refresh.abs_diff(calculate_refresh_rate(real.mode)) < SYNC_TOLERANCE_MHZ
            })
        })
        .filter(|candidate| {
            let refresh = calculate_refresh_rate(candidate.mode);
            real_modes.iter().any(|real| {
                refresh.abs_diff(calculate_refresh_rate(real.mode)) < REFRESH_MATCH_TOLERANCE_MHZ
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn driver(width: u16, height: u16, refresh: u32) -> KmsMode {
        let mode = cvt_rb_mode(width, height, refresh);
        // Native modes are represented by their complete driver timings in
        // production; CVT is sufficient for exercising pure filtering here.
        KmsMode {
            mode,
            source: ModeSource::Driver,
            preferred: true,
        }
    }

    #[test]
    fn target_timing_matches_cvt_rb() {
        let mode = cvt_rb_mode(1920, 1200, 60);
        assert_eq!(mode.size(), (1920, 1200));
        assert_eq!(mode.clock(), 154_000);
        assert_eq!(mode.hsync(), (1968, 2000, 2080));
        assert_eq!(mode.vsync(), (1203, 1209, 1235));
        assert!(mode.flags().contains(ModeFlags::PHSYNC | ModeFlags::NVSYNC));
        assert_eq!(calculate_refresh_rate(mode), 59_950);
    }

    #[test]
    fn panel_4k_16_10_gets_1920_1200() {
        let modes = fallback_modes(&[driver(3840, 2400, 60)], true, false);
        assert!(
            modes.iter().any(|mode| mode.mode.size() == (1920, 1200)
                && calculate_refresh_rate(mode.mode) == 59_950)
        );
    }

    #[test]
    fn capability_and_tile_guards_work() {
        let real = [driver(3840, 2400, 60)];
        assert!(fallback_modes(&real, false, false).is_empty());
        assert!(fallback_modes(&real, true, true).is_empty());
    }

    #[test]
    fn real_modes_are_not_duplicated_and_native_stays_preferred() {
        let real = [driver(3840, 2400, 60), driver(1920, 1200, 60)];
        let fallbacks = fallback_modes(&real, true, false);
        assert!(
            !fallbacks
                .iter()
                .any(|mode| mode.mode.size() == (1920, 1200))
        );
        assert!(real[0].preferred);
        assert!(fallbacks.iter().all(|mode| !mode.preferred));
    }

    #[test]
    fn limits_and_refresh_matching_are_enforced() {
        let modes = fallback_modes(&[driver(1920, 1200, 60)], true, false);
        assert!(
            modes
                .iter()
                .all(|mode| mode.mode.size().0 <= 1920 && mode.mode.size().1 <= 1200)
        );
        assert!(modes.iter().all(|mode| mode.mode.clock() <= 154_000));
        assert!(
            !modes
                .iter()
                .any(|mode| calculate_refresh_rate(mode.mode) > 60_000)
        );
    }

    #[test]
    fn portrait_catalog_is_selected() {
        let modes = fallback_modes(&[driver(2400, 3840, 60)], true, false);
        assert!(
            modes
                .iter()
                .all(|mode| matches!(mode.source, ModeSource::FallbackPortrait))
        );
        assert!(modes.iter().any(|mode| mode.mode.size() == (1200, 1920)));
    }
}
