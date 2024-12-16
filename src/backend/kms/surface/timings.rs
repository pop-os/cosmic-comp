use std::{collections::VecDeque, num::NonZeroU64, time::Duration};

use smithay::utils::{Clock, Monotonic, Time};
use tracing::error;

const FRAME_TIME_BUFFER: Duration = Duration::from_millis(1);
const FRAME_TIME_WINDOW: usize = 3;

pub struct Timings {
    refresh_interval_ns: Option<NonZeroU64>,
    vrr: bool,

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

    fn frame_time(&self) -> Duration {
        Time::elapsed(&self.render_start, self.presentation_presented)
    }
}

impl Timings {
    const WINDOW_SIZE: usize = 360;

    pub fn new(refresh_interval: Option<Duration>, vrr: bool) -> Self {
        let refresh_interval_ns = if let Some(interval) = &refresh_interval {
            assert_eq!(interval.as_secs(), 0);
            Some(NonZeroU64::new(interval.subsec_nanos().into()).unwrap())
        } else {
            None
        };

        Self {
            refresh_interval_ns,
            vrr,

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

    pub fn set_vrr(&mut self, vrr: bool) {
        self.vrr = vrr;
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
                    - frame.render_duration_elements.unwrap_or(Duration::ZERO),
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
            self.previous_frames.push_back(Frame {
                render_start: frame.render_start,
                render_duration_elements: frame.render_duration_elements.unwrap_or_default(),
                render_duration_draw: frame.render_duration_draw.unwrap_or_default(),
                presentation_submitted: frame.presentation_submitted.unwrap(),
                presentation_presented: value,
            });
            while self.previous_frames.len() > Self::WINDOW_SIZE {
                self.previous_frames.pop_front();
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
        if self.previous_frames.is_empty() {
            return Duration::ZERO;
        }
        self.previous_frames
            .iter()
            .map(|f| f.render_time())
            .sum::<Duration>()
            / (self.previous_frames.len() as u32)
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
                .sum::<Duration>()
                / (window.min(self.previous_frames.len()) as u32),
        )
    }

    pub fn avg_fps(&self) -> f64 {
        if self.previous_frames.is_empty() {
            return 0.0;
        }
        let secs = match (self.previous_frames.front(), self.previous_frames.back()) {
            (Some(Frame { render_start, .. }), Some(end_frame)) => {
                Time::elapsed(render_start, end_frame.render_start) + end_frame.frame_time()
            }
            _ => Duration::ZERO,
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

    pub fn next_render_time(&self, clock: &Clock<Monotonic>) -> Duration {
        let estimated_presentation_time = self.next_presentation_time(clock);
        if estimated_presentation_time.is_zero() {
            return Duration::ZERO;
        }

        let Some(avg_frametime) = self.avg_frametime(FRAME_TIME_WINDOW) else {
            return Duration::ZERO;
        };

        estimated_presentation_time.saturating_sub(avg_frametime + FRAME_TIME_BUFFER)
    }
}
