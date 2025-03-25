use std::sync::Mutex;

use smithay::{
    output::Output,
    utils::{Coordinate, Logical, Point, Rectangle, Size},
};

use crate::shell::zoom::OutputZoomState;

use super::prelude::OutputExt;

/// Marker type for coordinates in global space
#[derive(Debug)]
pub struct Global;

/// Marker type for coordinates in workspace local space
#[derive(Debug)]
pub struct Local;

pub trait PointExt<C: Coordinate> {
    fn as_global(self) -> Point<C, Global>;
    fn as_local(self) -> Point<C, Local>;
}

pub trait PointGlobalExt<C: Coordinate> {
    fn to_local(self, output: &Output) -> Point<C, Local>;
    fn to_zoomed(self, output: &Output) -> Point<C, Local>;
    fn as_logical(self) -> Point<C, Logical>;
}

pub trait PointLocalExt<C: Coordinate> {
    fn to_global(self, output: &Output) -> Point<C, Global>;
    fn as_logical(self) -> Point<C, Logical>;
}

pub trait SizeExt<C: Coordinate> {
    fn as_logical(self) -> Size<C, Logical>;
    fn as_local(self) -> Size<C, Local>;
    fn as_global(self) -> Size<C, Global>;
}

pub trait RectExt<C: Coordinate> {
    fn as_global(self) -> Rectangle<C, Global>;
    fn as_local(self) -> Rectangle<C, Local>;
}

pub trait RectGlobalExt<C: Coordinate> {
    fn to_local(self, output: &Output) -> Rectangle<C, Local>;
    fn as_logical(self) -> Rectangle<C, Logical>;
}

pub trait RectLocalExt<C: Coordinate> {
    fn to_global(self, output: &Output) -> Rectangle<C, Global>;
    fn as_logical(self) -> Rectangle<C, Logical>;
}

impl<C: Coordinate> PointExt<C> for Point<C, Logical> {
    fn as_global(self) -> Point<C, Global> {
        (self.x, self.y).into()
    }

    fn as_local(self) -> Point<C, Local> {
        (self.x, self.y).into()
    }
}

impl<C: Coordinate> PointGlobalExt<C> for Point<C, Global> {
    fn to_local(self, output: &Output) -> Point<C, Local> {
        let point = (self.to_f64() - output.geometry().loc.to_f64()).as_logical();
        (C::from_f64(point.x), C::from_f64(point.y)).into()
    }

    fn to_zoomed(self, output: &Output) -> Point<C, Local> {
        let zoomed_output_geometry = output.zoomed_geometry().unwrap();
        let level = output
            .user_data()
            .get::<Mutex<OutputZoomState>>()
            .unwrap()
            .lock()
            .unwrap()
            .current_level();
        let point = (self.to_f64() - zoomed_output_geometry.loc.to_f64())
            .upscale(level)
            .as_logical();
        (C::from_f64(point.x), C::from_f64(point.y)).into()
    }

    fn as_logical(self) -> Point<C, Logical> {
        (self.x, self.y).into()
    }
}

impl<C: Coordinate> PointLocalExt<C> for Point<C, Local> {
    fn to_global(self, output: &Output) -> Point<C, Global> {
        let point =
            (self.to_f64().as_logical() + output.geometry().loc.to_f64().as_logical()).as_global();
        (C::from_f64(point.x), C::from_f64(point.y)).into()
    }

    fn as_logical(self) -> Point<C, Logical> {
        (self.x, self.y).into()
    }
}

impl<C: Coordinate> SizeExt<C> for Size<C, Global> {
    fn as_logical(self) -> Size<C, Logical> {
        (self.w, self.h).into()
    }
    fn as_global(self) -> Size<C, Global> {
        self
    }
    fn as_local(self) -> Size<C, Local> {
        (self.w, self.h).into()
    }
}

impl<C: Coordinate> SizeExt<C> for Size<C, Local> {
    fn as_logical(self) -> Size<C, Logical> {
        (self.w, self.h).into()
    }
    fn as_global(self) -> Size<C, Global> {
        (self.w, self.h).into()
    }
    fn as_local(self) -> Size<C, Local> {
        self
    }
}

impl<C: Coordinate> SizeExt<C> for Size<C, Logical> {
    fn as_logical(self) -> Size<C, Logical> {
        self
    }
    fn as_global(self) -> Size<C, Global> {
        (self.w, self.h).into()
    }
    fn as_local(self) -> Size<C, Local> {
        (self.w, self.h).into()
    }
}

impl<C: Coordinate> RectExt<C> for Rectangle<C, Logical> {
    fn as_global(self) -> Rectangle<C, Global> {
        Rectangle::new(self.loc.as_global(), (self.size.w, self.size.h).into())
    }

    fn as_local(self) -> Rectangle<C, Local> {
        Rectangle::new(self.loc.as_local(), (self.size.w, self.size.h).into())
    }
}

impl<C: Coordinate> RectGlobalExt<C> for Rectangle<C, Global> {
    fn to_local(self, output: &Output) -> Rectangle<C, Local> {
        Rectangle::new(self.loc.to_local(output), (self.size.w, self.size.h).into())
    }

    fn as_logical(self) -> Rectangle<C, Logical> {
        Rectangle::new(self.loc.as_logical(), self.size.as_logical())
    }
}

impl<C: Coordinate> RectLocalExt<C> for Rectangle<C, Local> {
    fn to_global(self, output: &Output) -> Rectangle<C, Global> {
        Rectangle::new(
            self.loc.to_global(output),
            (self.size.w, self.size.h).into(),
        )
    }

    fn as_logical(self) -> Rectangle<C, Logical> {
        Rectangle::new(self.loc.as_logical(), self.size.as_logical())
    }
}
