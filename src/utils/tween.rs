use keyframe::{CanTween, num_traits::Float};
use smithay::utils::{Coordinate, Point, Rectangle, Size};

pub struct EasePoint<N: Coordinate, Kind>(pub Point<N, Kind>);
pub struct EaseSize<N: Coordinate, Kind>(pub Size<N, Kind>);
pub struct EaseRectangle<N: Coordinate, Kind>(pub Rectangle<N, Kind>);

impl<N: Coordinate, Kind> CanTween for EasePoint<N, Kind> {
    fn ease(from: Self, to: Self, time: impl Float) -> Self {
        let x = N::from_f64(CanTween::ease(from.0.x.to_f64(), to.0.x.to_f64(), time).round());
        let y = N::from_f64(CanTween::ease(from.0.y.to_f64(), to.0.y.to_f64(), time).round());
        EasePoint((x, y).into())
    }
}

impl<N: Coordinate, Kind> CanTween for EaseSize<N, Kind> {
    fn ease(from: Self, to: Self, time: impl Float) -> Self {
        let w = N::from_f64(CanTween::ease(from.0.w.to_f64(), to.0.w.to_f64(), time).round());
        let h = N::from_f64(CanTween::ease(from.0.h.to_f64(), to.0.h.to_f64(), time).round());
        EaseSize((w, h).into())
    }
}

impl<N: Coordinate, Kind> CanTween for EaseRectangle<N, Kind> {
    fn ease(from: Self, to: Self, time: impl Float) -> Self {
        EaseRectangle(Rectangle::new(
            CanTween::ease(EasePoint(from.0.loc), EasePoint(to.0.loc), time).unwrap(),
            CanTween::ease(EaseSize(from.0.size), EaseSize(to.0.size), time).unwrap(),
        ))
    }
}

impl<N: Coordinate, Kind> EasePoint<N, Kind> {
    pub fn unwrap(self) -> Point<N, Kind> {
        self.0
    }
}
impl<N: Coordinate, Kind> EaseSize<N, Kind> {
    pub fn unwrap(self) -> Size<N, Kind> {
        self.0
    }
}
impl<N: Coordinate, Kind> EaseRectangle<N, Kind> {
    pub fn unwrap(self) -> Rectangle<N, Kind> {
        self.0
    }
}
