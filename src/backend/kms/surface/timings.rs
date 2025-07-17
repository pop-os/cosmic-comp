use std::{collections::VecDeque, num::NonZeroU64, time::Duration};

use smithay::{
    backend::drm::DrmNode,
    utils::{Clock, Monotonic, Time},
};
use tracing::{debug, error};

const BASE_SAFETY_MARGIN: Duration = Duration::from_millis(3);
const SAMPLE_TIME_WINDOW: usize = 5;

pub struct Timings {
    refresh_interval_ns: Option<NonZeroU64>,
    min_refresh_interval_ns: Option<NonZeroU64>,
    vrr: bool,
    vendor: Option<u32>,

    pub pending_frame: Option<PendingFrame>,
    pub previous_frames: VecDeque<Frame>,
}

#[derive(Debug)]
pub struct PendingFrame {
    render_start: Time<Monotonic>,
    render_duration_elements: Option<Duration>,
    render_duration_draw: Option<Duration>,
    presentation_submitted: Option<Time<Monotonic>>,
}

#[derive(Debug)]
pub struct Frame {
    pub render_start: Time<Monotonic>,
    pub render_duration_elements: Duration,
    pub render_duration_draw: Duration,
    pub presentation_submitted: Time<Monotonic>,
    pub presentation_presented: Time<Monotonic>,
}

impl Frame {
    fn render_time(&self) -> Duration {
        self.render_duration_elements + self.render_duration_draw
    }

    fn submit_time(&self) -> Duration {
        Time::elapsed(&self.render_start, self.presentation_submitted)
    }

    fn frame_time(&self) -> Duration {
        Time::elapsed(&self.render_start, self.presentation_presented)
    }
}

impl Timings {
    const CLEANUP: usize = 360;

    pub fn new(
        refresh_interval: Option<Duration>,
        min_interval: Option<Duration>,
        vrr: bool,
        node: DrmNode,
    ) -> Self {
        let refresh_interval_ns = if let Some(interval) = &refresh_interval {
            assert_eq!(interval.as_secs(), 0);
            Some(NonZeroU64::new(interval.subsec_nanos().into()).unwrap())
        } else {
            None
        };

        let min_refresh_interval_ns = if let Some(interval) = &min_interval {
            assert_eq!(interval.as_secs(), 0);
            Some(NonZeroU64::new(interval.subsec_nanos().into()).unwrap())
        } else {
            None
        };

        let vendor = if let Ok(vendor) = std::fs::read_to_string(format!(
            "/sys/class/drm/renderD{}/device/vendor",
            node.minor()
        )) {
            u32::from_str_radix(&vendor.trim()[2..], 16).ok()
        } else {
            None
        };

        Self {
            refresh_interval_ns,
            min_refresh_interval_ns,
            vrr,
            vendor,

            pending_frame: None,
            previous_frames: VecDeque::new(),
        }
    }

    pub fn refresh_interval(&self) -> Duration {
        match self.refresh_interval_ns {
            Some(ns) => Duration::from_nanos(ns.get()),
            None => Duration::ZERO,
        }
    }

    pub fn set_refresh_interval(&mut self, interval: Option<Duration>) {
        self.refresh_interval_ns = interval
            .map(|duration| duration.subsec_nanos() as u64)
            .and_then(NonZeroU64::new);

        self.previous_frames.clear();
    }

    pub fn set_min_refresh_interval(&mut self, min_interval: Option<Duration>) {
        self.min_refresh_interval_ns = min_interval
            .map(|duration| duration.subsec_nanos() as u64)
            .and_then(NonZeroU64::new);
    }

    pub fn set_vrr(&mut self, vrr: bool) {
        self.vrr = vrr;
    }

    pub fn vrr(&self) -> bool {
        self.vrr
    }

    pub fn start_render(&mut self, clock: &Clock<Monotonic>) {
        self.pending_frame = Some(PendingFrame {
            render_start: clock.now(),
            render_duration_elements: None,
            render_duration_draw: None,
            presentation_submitted: None,
        });
    }

    pub fn elements_done(&mut self, clock: &Clock<Monotonic>) {
        if let Some(frame) = self.pending_frame.as_mut() {
            frame.render_duration_elements = Some(Time::elapsed(&frame.render_start, clock.now()));
        }
    }

    pub fn draw_done(&mut self, clock: &Clock<Monotonic>) {
        if let Some(frame) = self.pending_frame.as_mut() {
            frame.render_duration_draw = Some(
                Time::elapsed(&frame.render_start, clock.now())
                    - frame
                        .render_duration_elements
                        .clone()
                        .unwrap_or(Duration::ZERO),
            );
        }
    }

    pub fn submitted_for_presentation(&mut self, clock: &Clock<Monotonic>) {
        if let Some(frame) = self.pending_frame.as_mut() {
            frame.presentation_submitted = Some(clock.now());
        }
    }

    pub fn presented(&mut self, value: Time<Monotonic>) {
        if let Some(frame) = self.pending_frame.take() {
            let new_frame = Frame {
                render_start: frame.render_start,
                render_duration_elements: frame.render_duration_elements.unwrap_or_default(),
                render_duration_draw: frame.render_duration_draw.unwrap_or_default(),
                presentation_submitted: frame.presentation_submitted.unwrap(),
                presentation_presented: value,
            };
            if new_frame.render_start > new_frame.presentation_submitted {
                debug!(
                    "frame time overflowed: {}",
                    new_frame.frame_time().as_millis()
                );
            }
            self.previous_frames.push_back(new_frame);

            if let Some(overflow) = self.previous_frames.len().checked_sub(Self::CLEANUP * 2) {
                self.previous_frames = self.previous_frames.split_off(overflow + Self::CLEANUP);
            }
        }
    }

    pub fn discard_current_frame(&mut self) {
        let _ = self.pending_frame.take();
    }

    pub fn max_rendertime(&self) -> Duration {
        self.previous_frames
            .iter()
            .map(|f| f.render_time())
            .max()
            .unwrap_or(Duration::ZERO)
    }

    pub fn min_rendertime(&self) -> Duration {
        self.previous_frames
            .iter()
            .map(|f| f.render_time())
            .min()
            .unwrap_or(Duration::ZERO)
    }

    pub fn max_frametime(&self, window: usize) -> Duration {
        self.previous_frames
            .iter()
            .rev()
            .take(window)
            .map(|f| f.frame_time())
            .max()
            .unwrap_or(Duration::ZERO)
    }

    pub fn min_frametime(&self, window: usize) -> Duration {
        self.previous_frames
            .iter()
            .rev()
            .take(window)
            .map(|f| f.frame_time())
            .min()
            .unwrap_or(Duration::ZERO)
    }

    pub fn avg_rendertime(&self) -> Duration {
        let Some(sum_rendertime) = self
            .previous_frames
            .iter()
            .map(|f| f.render_time())
            .try_fold(Duration::ZERO, |acc, x| acc.checked_add(x))
        else {
            return Duration::ZERO;
        };

        sum_rendertime
            .checked_div(self.previous_frames.len() as u32)
            .unwrap_or(Duration::ZERO)
    }

    pub fn avg_submittime(&self, window: usize) -> Option<Duration> {
        if self.previous_frames.len() < window || window == 0 {
            return None;
        }

        Some(
            self.previous_frames
                .iter()
                .rev()
                .take(window)
                .map(|f| f.submit_time())
                .try_fold(Duration::ZERO, |acc, x| acc.checked_add(x))?
                / (window.min(self.previous_frames.len()) as u32),
        )
    }

    pub fn avg_frametime(&self, window: usize) -> Option<Duration> {
        if self.previous_frames.len() < window || window == 0 {
            return None;
        }

        Some(
            self.previous_frames
                .iter()
                .rev()
                .take(window)
                .map(|f| f.frame_time())
                .try_fold(Duration::ZERO, |acc, x| acc.checked_add(x))?
                / (window.min(self.previous_frames.len()) as u32),
        )
    }

    pub fn avg_fps(&self) -> f64 {
        if self.previous_frames.is_empty() {
            return 0.0;
        }
        let secs = match (self.previous_frames.front(), self.previous_frames.back()) {
            (Some(Frame { render_start, .. }), Some(end_frame)) => {
                Time::elapsed(render_start, end_frame.render_start.clone()) + end_frame.frame_time()
            }
            _ => {
                return 0.0;
            }
        }
        .as_secs_f64();

        1.0 / (secs / self.previous_frames.len() as f64)
    }

    pub fn next_presentation_time(&self, clock: &Clock<Monotonic>) -> Duration {
        let mut now = clock.now().into();

        let Some(refresh_interval_ns) = self.refresh_interval_ns else {
            return Duration::ZERO;
        };
        let Some(last_presentation_time): Option<Duration> = self
            .previous_frames
            .back()
            .map(|frame| frame.presentation_presented.into())
        else {
            return Duration::ZERO;
        };
        let refresh_interval_ns = refresh_interval_ns.get();

        if now <= last_presentation_time {
            // Got an early VBlank.
            let orig_now = now;
            now += Duration::from_nanos(refresh_interval_ns);

            if now < last_presentation_time {
                // Not sure when this can happen.
                error!(
                    now = ?orig_now,
                    ?last_presentation_time,
                    "got a 2+ early VBlank, {:?} until presentation",
                    last_presentation_time - now,
                );
                now = last_presentation_time + Duration::from_nanos(refresh_interval_ns);
            }
        }

        let since_last = now - last_presentation_time;
        let since_last_ns =
            since_last.as_secs() * 1_000_000_000 + u64::from(since_last.subsec_nanos());
        let to_next_ns = (since_last_ns / refresh_interval_ns + 1) * refresh_interval_ns;

        // If VRR is enabled and more than one frame passed since last presentation, assume that we
        // can present immediately.
        if self.vrr && to_next_ns > refresh_interval_ns {
            Duration::ZERO
        } else {
            last_presentation_time + Duration::from_nanos(to_next_ns) - now
        }
    }

    pub fn past_min_render_time(&self, clock: &Clock<Monotonic>) -> bool {
        let now: Duration = clock.now().into();
        let Some(min_refresh_interval_ns) = self.min_refresh_interval_ns else {
            return true;
        };
        let Some(last_presentation_time): Option<Duration> = self
            .previous_frames
            .back()
            .map(|frame| frame.presentation_presented.into())
        else {
            return true;
        };

        let min_refresh_interval_ns = min_refresh_interval_ns.get();
        if now <= last_presentation_time {
            return false;
        }

        const MIN_MARGIN: Duration = Duration::from_millis(3);
        let baseline = if let Some(refresh_interval_ns) = self.refresh_interval_ns {
            MIN_MARGIN.max(Duration::from_nanos(refresh_interval_ns.get() / 2))
        } else {
            MIN_MARGIN
        };

        let next_presentation_time =
            last_presentation_time + Duration::from_nanos(min_refresh_interval_ns);
        let deadline = next_presentation_time.saturating_sub(
            if let Some(avg_submittime) = self.avg_submittime(SAMPLE_TIME_WINDOW) {
                avg_submittime
            } else {
                baseline
            } + BASE_SAFETY_MARGIN,
        );

        now >= deadline
    }

    pub fn next_render_time(&self, clock: &Clock<Monotonic>) -> Duration {
        let Some(refresh_interval) = self.refresh_interval_ns else {
            return Duration::ZERO; // we don't know what to expect, so render immediately.
        };

        const MIN_MARGIN: Duration = Duration::from_millis(3);
        let baseline = MIN_MARGIN.max(Duration::from_nanos(refresh_interval.get() / 2));

        let estimated_presentation_time = self.next_presentation_time(clock);
        if estimated_presentation_time.is_zero() {
            return Duration::ZERO;
        }

        // HACK: Nvidia returns `page_flip`/`commit` early, so we have no information to optimize latency on submission.
        if self.vendor == Some(0x10de) {
            return Duration::ZERO;
        }

        let Some(avg_submittime) = self.avg_submittime(SAMPLE_TIME_WINDOW) else {
            return estimated_presentation_time.saturating_sub(baseline + BASE_SAFETY_MARGIN);
        };

        let margin = avg_submittime + BASE_SAFETY_MARGIN;
        estimated_presentation_time.saturating_sub(margin)
    }
}
