use super::window::{Focus, RESIZE_BORDER};
use crate::{
    backend::render::{
        IndicatorShader, Key, Usage, cursor::CursorState, element::AsGlowRenderer,
        shadow::ShadowShader, wayland::SurfaceRenderElement,
    },
    hooks::{Decorations, HOOKS},
    shell::{
        ModalBehavior,
        element::{
            CosmicMappedKey, CosmicMappedKeyInner,
            surface::{CosmicSurface, WeakCosmicSurface},
        },
        focus::target::PointerFocusTarget,
        grabs::{GrabType, ReleaseMode, ResizeEdge},
        layout::tiling::NodeDesc,
    },
    state::State,
    utils::{
        iced::{IcedElement, IcedRenderElement, Program},
        prelude::*,
    },
};
use calloop::LoopHandle;
use cosmic::{
    Apply, Element as CosmicElement, Theme,
    iced::{
        Alignment,
        core::{Background, Border, Color, Length, border::Radius},
        id::Id,
        runtime::Task,
        widget::scrollable::AbsoluteOffset,
    },
    theme, widget,
};
use cosmic_comp_config::AppearanceConfig;
use cosmic_settings_config::shortcuts;
use shortcuts::action::{Direction, FocusDirection};
use smithay::{
    backend::{
        drm::DrmNode,
        input::{InputTime, KeyState, TabletToolDescriptor},
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
            AxisFrame as PointerAxisFrame, ButtonEvent as PointerButtonEvent, CursorImageStatus,
            GestureHoldBeginEvent, GestureHoldEndEvent, GesturePinchBeginEvent,
            GesturePinchEndEvent, GesturePinchUpdateEvent, GestureSwipeBeginEvent,
            GestureSwipeEndEvent, GestureSwipeUpdateEvent, MotionEvent as PointerMotionEvent,
            PointerTarget, RelativeMotionEvent,
        },
        tablet::{
            Tablet, TabletSeatTrait,
            tool::{
                AxisFrame as ToolAxisFrame, ButtonEvent as ToolButtonEvent,
                DownEvent as ToolDownEvent, MotionEvent as ToolMotionEvent, TabletToolTarget,
                UpEvent as ToolUpEvent,
            },
        },
        touch::{
            DownEvent as TouchDownEvent, FrameMarker, MotionEvent as TouchMotionEvent,
            OrientationEvent, ShapeEvent, TouchTarget, UpEvent as TouchUpEvent,
        },
    },
    output::Output,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    utils::{
        Buffer, IsAlive, Logical, Physical, Point, Rectangle, Scale, Serial, Size, Transform,
        user_data::UserDataMap,
    },
    wayland::seat::WaylandFocus,
};
use std::{
    borrow::Cow,
    fmt,
    hash::Hash,
    mem,
    sync::{Arc, LazyLock},
};

mod tab;
mod tabs;

use self::{
    tab::{Tab, TabMessage},
    tabs::Tabs,
};

static SCROLLABLE_ID: LazyLock<Id> = LazyLock::new(|| Id::new("scrollable"));

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CosmicStack(pub(super) IcedElement<CosmicStackInternal>);

impl fmt::Debug for CosmicStack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CosmicStack")
            .field("internal", &self.0)
            .finish_non_exhaustive()
    }
}

#[derive(Debug)]
pub struct CosmicStackInternal {
    windows: Vec<CosmicSurface>,
    // Index of active window
    active: usize,
    activated: bool,
    group_focused: bool,
    previous_index: Option<(Serial, usize)>,
    scroll_to_focus: bool,
    // Child window with keyboard focus
    previous_keyboard: Option<WeakCosmicSurface>,
    pointer_entered: Option<Focus>,
    touch_serial: u32,
    tablet_serial: u32,
    potential_drag: Option<usize>,
    override_alive: bool,
    geometry: Option<Rectangle<i32, Global>>,
    mask: Option<tiny_skia::Mask>,
    tiled: bool,
    theme: cosmic::Theme,
    appearance_conf: AppearanceConfig,
}

impl CosmicStackInternal {
    pub fn swap_focus(&mut self, focus: Option<Focus>) -> Option<Focus> {
        mem::replace(&mut self.pointer_entered, focus)
    }

    pub fn current_focus(&self) -> Option<Focus> {
        self.pointer_entered
    }

    fn keyboard_leave_if_previous(&mut self, seat: &Seat<State>, data: &mut State, serial: Serial) {
        if self
            .previous_keyboard
            .as_ref()
            .is_none_or(|previous| self.windows[self.active] != *previous)
        {
            if let Some(window) = self
                .previous_keyboard
                .take()
                .and_then(|prev| self.windows.iter().find(|w| **w == prev))
            {
                KeyboardTarget::leave(window, seat, data, serial);
            }
            KeyboardTarget::enter(
                &self.windows[self.active],
                seat,
                data,
                Vec::new(), /* TODO */
                serial,
            );
            self.previous_keyboard = Some(self.windows[self.active].downgrade());
        }
    }
}

pub const TAB_HEIGHT: i32 = 24;

#[derive(Debug, Clone)]
pub enum MoveResult {
    Handled,
    MoveOut(CosmicSurface, LoopHandle<'static, crate::state::State>),
    Default,
}

impl CosmicStack {
    pub fn new<I: Into<CosmicSurface>>(
        windows: impl Iterator<Item = I>,
        handle: LoopHandle<'static, crate::state::State>,
        mut theme: cosmic::Theme,
        appearance: AppearanceConfig,
    ) -> CosmicStack {
        theme.transparent = theme.cosmic().frosted_windows;
        let windows = windows.map(Into::into).collect::<Vec<_>>();
        assert!(!windows.is_empty());

        for window in &windows {
            window.try_force_undecorated(true);
            window.set_tiled(true);
            window.send_configure();
        }

        let width = windows[0].geometry().size.w;
        CosmicStack(IcedElement::new(
            CosmicStackInternal {
                windows,
                active: 0,
                activated: false,
                group_focused: false,
                previous_index: None,
                scroll_to_focus: false,
                previous_keyboard: None,
                pointer_entered: None,
                touch_serial: 0,
                tablet_serial: 0,
                potential_drag: None,
                override_alive: true,
                geometry: None,
                mask: None,
                tiled: false,
                theme: theme.clone(),
                appearance_conf: appearance,
            },
            (width, TAB_HEIGHT),
            handle,
            theme,
        ))
    }

    pub fn add_window(
        &self,
        window: impl Into<CosmicSurface>,
        idx: Option<usize>,
        moved_into: Option<&Seat<State>>,
    ) {
        let window = window.into();
        window.try_force_undecorated(true);
        window.set_tiled(true);
        self.0.with_program(|p| {
            let last_mod_serial = moved_into.and_then(|seat| seat.last_modifier_change());
            if !p
                .previous_index
                .is_some_and(|(serial, _)| Some(serial) == last_mod_serial)
            {
                p.previous_index = last_mod_serial.map(|s| (s, p.active));
            }

            if let Some(geo) = &mut p.geometry {
                geo.loc.y += TAB_HEIGHT;
                geo.size.h -= TAB_HEIGHT;
                window.set_geometry(*geo, TAB_HEIGHT as u32);
            }
            window.send_configure();
            if let Some(idx) = idx {
                p.windows.insert(idx, window);
                p.active = idx;
            } else {
                p.windows.push(window);
                p.active = p.windows.len() - 1;
            }
            p.scroll_to_focus = true;
        });
        self.0
            .resize(Size::from((self.active().geometry().size.w, TAB_HEIGHT)));
        self.0.force_redraw()
    }

    pub fn remove_window(&self, window: &CosmicSurface) {
        self.0.with_program(|p| {
            if p.windows.len() == 1 {
                p.override_alive = false;
                let window = p.windows.first().unwrap();
                window.try_force_undecorated(false);
                window.set_tiled(false);
                return;
            }

            let Some(idx) = p.windows.iter().position(|w| w == window) else {
                return;
            };
            let window = p.windows.remove(idx);
            // TODO Send `leave` if window matches `p.previous_keyboard`?
            window.try_force_undecorated(false);
            window.set_tiled(false);

            p.active = p.active.min(p.windows.len() - 1);
            p.previous_index.take_if(|(_, idx)| *idx >= p.windows.len());
        });
        self.0
            .resize(Size::from((self.active().geometry().size.w, TAB_HEIGHT)));
        self.0.force_redraw()
    }

    pub fn remove_idx(&self, idx: usize) -> Option<CosmicSurface> {
        let window = self.0.with_program(|p| {
            if p.windows.len() == 1 {
                p.override_alive = false;
                let window = p.windows.first().unwrap();
                window.try_force_undecorated(false);
                window.set_tiled(false);
                return Some(window.clone());
            }
            if p.windows.len() <= idx {
                return None;
            }
            let window = p.windows.remove(idx);
            // TODO Send `leave` if window matches `p.previous_keyboard`?
            window.try_force_undecorated(false);
            window.set_tiled(false);

            p.active = p.active.min(p.windows.len() - 1);
            p.previous_index.take_if(|(_, idx)| *idx >= p.windows.len());

            Some(window)
        });
        self.0
            .resize(Size::from((self.active().geometry().size.w, TAB_HEIGHT)));
        self.0.force_redraw();
        window
    }

    pub fn len(&self) -> usize {
        self.0.with_program(|p| p.windows.len())
    }

    pub fn handle_focus(
        &self,
        seat: &Seat<State>,
        direction: FocusDirection,
        swap: Option<NodeDesc>,
    ) -> bool {
        let (result, update) = self.0.with_program(|p| {
            let last_mod_serial = seat.last_modifier_change();
            if !p
                .previous_index
                .is_some_and(|(serial, _)| Some(serial) == last_mod_serial)
            {
                p.previous_index = last_mod_serial.map(|s| (s, p.active));
            }

            match direction {
                FocusDirection::Left => {
                    if !p.group_focused {
                        if let Some(new) = p.active.checked_sub(1) {
                            p.scroll_to_focus = true;
                            p.active = new;
                            (true, true)
                        } else {
                            let old = p.active;
                            p.active = p.previous_index.unwrap().1;
                            if p.active != old {
                                p.scroll_to_focus = true;
                                (false, true)
                            } else {
                                (false, false)
                            }
                        }
                    } else {
                        (false, false)
                    }
                }
                FocusDirection::Right => {
                    if !p.group_focused {
                        let max = p.windows.len();
                        if p.active < max - 1 {
                            p.scroll_to_focus = true;
                            p.active += 1;
                            (true, true)
                        } else {
                            let old = p.active;
                            p.active = p.previous_index.unwrap().1;
                            if old != p.active {
                                p.scroll_to_focus = true;
                                (false, true)
                            } else {
                                (false, false)
                            }
                        }
                    } else {
                        (false, false)
                    }
                }
                FocusDirection::Out if swap.is_none() => {
                    if !mem::replace(&mut p.group_focused, true) {
                        p.windows.iter().for_each(|w| {
                            w.set_activated(false);
                            w.send_configure();
                        });
                        (true, true)
                    } else {
                        (false, false)
                    }
                }
                FocusDirection::In if swap.is_none() => {
                    if !mem::replace(&mut p.group_focused, false) {
                        p.windows.iter().enumerate().for_each(|(i, w)| {
                            w.set_activated(p.active == i);
                            w.send_configure();
                        });

                        (true, true)
                    } else {
                        (false, false)
                    }
                }
                FocusDirection::Up | FocusDirection::Down => {
                    if !p.group_focused {
                        let old = p.active;
                        p.active = p.previous_index.unwrap().1;
                        if old != p.active {
                            p.scroll_to_focus = true;
                        }
                        (false, true)
                    } else {
                        (false, false)
                    }
                }
                _ => (false, false),
            }
        });

        if update {
            self.0
                .resize(Size::from((self.active().geometry().size.w, TAB_HEIGHT)));
        }

        result
    }

    pub fn handle_move(&self, direction: Direction) -> MoveResult {
        let loop_handle = self.0.loop_handle();
        let result = self.0.with_program(|p| {
            if p.group_focused {
                return MoveResult::Default;
            }

            let next = match direction {
                Direction::Left => p.active.checked_sub(1),
                Direction::Right => (p.active + 1 < p.windows.len()).then_some(p.active + 1),
                Direction::Down | Direction::Up => None,
            };

            if let Some(val) = next {
                let old = p.active;
                p.active = val;
                p.windows.swap(old, val);
                p.scroll_to_focus = true;
                MoveResult::Handled
            } else {
                if p.windows.len() == 1 {
                    return MoveResult::Default;
                }
                let window = p.windows.remove(p.active);
                // TODO Send `leave` if window matches `p.previous_keyboard`?
                if let Some(prev_idx) = p
                    .previous_index
                    .map(|(_, idx)| idx)
                    .filter(|idx| *idx < p.windows.len())
                {
                    p.active = prev_idx;
                    p.scroll_to_focus = true;
                } else if p.active == p.windows.len() {
                    p.active -= 1;
                    p.scroll_to_focus = true;
                }
                window.try_force_undecorated(false);
                window.set_tiled(false);

                MoveResult::MoveOut(window, loop_handle)
            }
        });

        if !matches!(result, MoveResult::Default) {
            self.0
                .resize(Size::from((self.active().geometry().size.w, TAB_HEIGHT)));
        }

        result
    }

    pub fn active(&self) -> CosmicSurface {
        self.0.with_program(|p| p.windows[p.active].clone())
    }

    pub fn has_active(&self, window: &CosmicSurface) -> bool {
        self.0.with_program(|p| &p.windows[p.active] == window)
    }

    pub fn whole_stack_focused(&self) -> bool {
        self.0.with_program(|p| p.group_focused)
    }

    pub fn set_active<S>(&self, window: &S)
    where
        CosmicSurface: PartialEq<S>,
    {
        self.0.with_program(|p| {
            if let Some(val) = p.windows.iter().position(|w| w == window) {
                p.active = val;
            }
        });
        self.0
            .resize(Size::from((self.active().geometry().size.w, TAB_HEIGHT)));
        self.0.force_redraw()
    }

    pub fn set_tiled(&self, tiled: bool) {
        self.0.with_program(|p| {
            p.tiled = tiled;
        });
    }

    pub fn surfaces(&self) -> impl Iterator<Item = CosmicSurface> {
        self.0.with_program(|p| p.windows.clone().into_iter())
    }

    pub fn focus_under(
        &self,
        mut relative_pos: Point<f64, Logical>,
        surface_type: WindowSurfaceType,
    ) -> Option<(PointerFocusTarget, Point<f64, Logical>)> {
        self.0.with_program(|p| {
            let mut stack_ui = None;
            let geo = p.windows[p.active].geometry();

            if surface_type.contains(WindowSurfaceType::TOPLEVEL) {
                let point_i32 = relative_pos.to_i32_floor::<i32>();
                if (point_i32.x - geo.loc.x >= -RESIZE_BORDER && point_i32.x - geo.loc.x < 0)
                    || (point_i32.y - geo.loc.y >= -RESIZE_BORDER && point_i32.y - geo.loc.y < 0)
                    || (point_i32.x - geo.loc.x >= geo.size.w
                        && point_i32.x - geo.loc.x < geo.size.w + RESIZE_BORDER)
                    || (point_i32.y - geo.loc.y >= geo.size.h + TAB_HEIGHT
                        && point_i32.y - geo.loc.y < geo.size.h + TAB_HEIGHT + RESIZE_BORDER)
                {
                    stack_ui = Some((
                        PointerFocusTarget::StackUI(self.clone()),
                        Point::from((0., 0.)),
                    ));
                }

                if point_i32.y - geo.loc.y < TAB_HEIGHT {
                    stack_ui = Some((
                        PointerFocusTarget::StackUI(self.clone()),
                        Point::from((0., 0.)),
                    ));
                }
            }

            relative_pos.y -= TAB_HEIGHT as f64;

            let active_window = &p.windows[p.active];
            stack_ui.or_else(|| {
                active_window.focus_under(relative_pos, surface_type).map(
                    |(target, surface_offset)| {
                        (
                            target,
                            surface_offset.to_f64() + Point::from((0., TAB_HEIGHT as f64)),
                        )
                    },
                )
            })
        })
    }

    pub fn offset(&self) -> Point<i32, Logical> {
        Point::from((0, TAB_HEIGHT))
    }

    pub fn pending_size(&self) -> Option<Size<i32, Logical>> {
        self.0
            .with_program(|p| p.geometry.map(|geo| geo.size.as_logical()))
    }

    pub fn last_server_size(&self) -> Option<Size<i32, Logical>> {
        let mut size = self.active().last_server_size()?;
        // if stacked the window doesn't have SSD, instead it has a tab
        size.h += TAB_HEIGHT;
        Some(size)
    }

    pub fn set_geometry(&self, geo: Rectangle<i32, Global>) {
        self.0.with_program(|p| {
            let loc = (geo.loc.x, geo.loc.y + TAB_HEIGHT);
            let size = (geo.size.w, geo.size.h - TAB_HEIGHT);

            let win_geo = Rectangle::new(loc.into(), size.into());
            for window in p.windows.iter() {
                window.set_geometry(win_geo, TAB_HEIGHT as u32);
            }

            p.geometry = Some(geo);
            p.mask.take();
        });
    }

    pub fn on_commit(&self, surface: &WlSurface) {
        if let Some(surface) = self.surfaces().find(|w| w == surface) {
            surface.0.on_commit();
            if self.active() == surface {
                self.0
                    .resize(Size::from((surface.geometry().size.w, TAB_HEIGHT)));
            }
        }
    }

    pub(in super::super) fn focus_stack(&self) {
        self.0.with_program(|p| {
            p.group_focused = true;
        });
    }

    pub(in super::super) fn loop_handle(&self) -> LoopHandle<'static, crate::state::State> {
        self.0.loop_handle()
    }

    pub fn push_popup_render_elements<R>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
        alpha: f32,
        scanout_node: Option<DrmNode>,
        push: &mut dyn FnMut(CosmicStackRenderElement<R>),
    ) where
        R: AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
    {
        let window_loc = location + Point::from((0, (TAB_HEIGHT as f64 * scale.y) as i32));
        self.0.with_program(|p| {
            let frosted = if p.theme.cosmic().frosted_windows {
                (p.theme.cosmic().frosted as u8 + 1) as usize
            } else {
                0
            };

            p.windows[p.active].push_popup_render_elements(
                renderer,
                window_loc,
                scale,
                alpha,
                scanout_node,
                frosted,
                &mut |elem| push(elem.into()),
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
        C: From<CosmicStackRenderElement<R>>,
    {
        self.0.with_program(|p| {
            if p.windows[p.active].is_maximized(false) {
                return None;
            }

            let round = p.appearance_conf.clip_tiled_windows || !p.tiled;
            if p.tiled && !p.appearance_conf.shadow_tiled_windows {
                return None;
            }
            let radii = if round {
                p.theme
                    .cosmic()
                    .radius_s()
                    .map(|x| if x < 4.0 { x } else { x + 4.0 })
                    .map(|x| (x * scale as f32).round() as u8)
            } else {
                [0, 0, 0, 0]
            };

            let mut geo = SpaceElement::geometry(&p.windows[p.active]).to_f64();
            geo.size.h += TAB_HEIGHT as f64;
            if let Some(max_size) = max_size {
                geo.size = geo.size.clamp(Size::default(), max_size.to_f64());
            }

            geo = geo.upscale(scale);
            geo.loc += location.to_f64().to_logical(output_scale);

            let window_key =
                CosmicMappedKey(CosmicMappedKeyInner::Stack(Arc::downgrade(&self.0.0)));

            Some(
                CosmicStackRenderElement::Shadow(ShadowShader::element(
                    renderer,
                    window_key,
                    geo.to_i32_round().as_local(),
                    radii,
                    if p.activated { alpha } else { alpha * 0.75 },
                    output_scale.x,
                    p.theme.cosmic().is_dark,
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
        push_above: &mut dyn FnMut(CosmicStackRenderElement<R>),
        push_below: &mut dyn FnMut(CosmicStackRenderElement<R>),
    ) where
        R: AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
    {
        if !self.0.with_program(|p| p.override_alive) {
            return;
        }

        let geometry = self
            .0
            .with_program(|p| p.windows[p.active].geometry())
            .to_physical_precise_round(scale);
        let stack_loc = location + geometry.loc;
        let window_loc = location + Point::from((0, (TAB_HEIGHT as f64 * scale.y) as i32));
        let frosted = self.0.with_program(|p| {
            if p.theme.cosmic().frosted_windows {
                (p.theme.cosmic().frosted as u8 + 1) as usize
            } else {
                0
            }
        });
        let radii = self.0.with_program(|p| {
            let maximized = p.windows[p.active].is_maximized(false);
            let round = (p.appearance_conf.clip_tiled_windows || !p.tiled) && !maximized;
            round.then(|| {
                p.theme
                    .cosmic()
                    .radius_s()
                    .map(|x| if x < 4.0 { x } else { x + 4.0 })
                    .map(|x| x.round() as u8)
            })
        });

        self.0.with_program(|p| {
            let maximized = p.windows[p.active].is_maximized(false);

            let mut geo = SpaceElement::geometry(&p.windows[p.active]).to_f64();
            geo.loc += location.to_f64().to_logical(scale);
            geo.size.h += TAB_HEIGHT as f64;
            if let Some(max_size) = max_size {
                geo.size = geo.size.clamp(Size::default(), max_size.to_f64());
            }

            let window_key =
                CosmicMappedKey(CosmicMappedKeyInner::Stack(Arc::downgrade(&self.0.0)));

            if !maximized {
                let (r, g, b, a) = p.theme.cosmic().bg_divider().into_components();
                push_above(CosmicStackRenderElement::Border(IndicatorShader::element(
                    renderer,
                    Key::Window(Usage::Border, window_key.clone()),
                    geo.to_i32_round().as_local(),
                    1,
                    radii.unwrap_or([0; 4]),
                    a * alpha,
                    scale.x,
                    [r, g, b],
                )));
            };

            let radii = radii.map(|[_, _, c, d]| [0, 0, c, d]);
            p.windows[p.active].push_render_elements(
                renderer,
                window_loc,
                scale,
                alpha,
                scanout_override,
                scanout_node,
                radii.is_some(),
                radii.unwrap_or([0; 4]),
                frosted,
                &mut |elem| push_above(elem.into()),
                Some(&mut |elem| push_below(elem.into())),
            );
        });

        self.0.push_render_elements(
            renderer,
            stack_loc,
            scale,
            alpha,
            radii.map(|[_, b, _, d]| [0, b, 0, d]).unwrap_or([0; 4]),
            &mut |elem| push_above(elem.into()),
            Some(&mut |elem| push_below(elem.into())),
        );
    }

    pub(crate) fn set_theme(&self, mut theme: cosmic::Theme) {
        theme.transparent = theme.cosmic().frosted_windows;
        self.0.with_program(|p| {
            p.theme = theme.clone();
        });
        self.0.set_theme(theme);
    }

    pub fn update_appearance_conf(&self, appearance: &AppearanceConfig) {
        if self.0.with_program(|p| {
            if p.appearance_conf != *appearance {
                p.appearance_conf = *appearance;
                true
            } else {
                false
            }
        }) {
            self.0.force_redraw();
            self.0.force_update();
        }
    }

    pub(crate) fn force_redraw(&self) {
        self.0.force_redraw();
    }

    fn start_drag(&self, data: &mut State, seat: &Seat<State>, serial: Serial) {
        if let Some(dragged_out) = self.0.with_program(|p| p.potential_drag.take())
            && let Some(surface) = self.0.with_program(|p| p.windows.get(dragged_out).cloned())
        {
            let seat = seat.clone();
            surface.try_force_undecorated(false);
            surface.send_configure();
            if let Some(surface) = surface.wl_surface().map(Cow::into_owned) {
                let _ = data.common.event_loop_handle.insert_idle(move |state| {
                    let res = state.common.shell.write().move_request(
                        &surface,
                        &seat,
                        serial,
                        ReleaseMode::NoMouseButtons,
                        true,
                        &state.common.config,
                        &state.common.event_loop_handle,
                        false,
                        ModalBehavior::Block,
                    );
                    if let Some((grab, focus)) = res {
                        match grab.grab_type() {
                            GrabType::Touch => {
                                seat.get_touch().unwrap().set_grab(state, grab, serial)
                            }
                            GrabType::Pointer => seat
                                .get_pointer()
                                .unwrap()
                                .set_grab(state, grab, serial, focus),
                            GrabType::TabletTool => seat
                                .tablet_seat()
                                .get_tool(grab.tool().unwrap())
                                .unwrap()
                                .set_grab(state, grab, InputTime::now(), serial, focus),
                        }
                    }
                });
            }
        }
    }

    pub fn min_size(&self) -> Option<Size<i32, Logical>> {
        self.surfaces()
            .fold(None, |min_size, window| {
                let win_min_size = window.min_size_without_ssd();
                match (min_size, win_min_size) {
                    (None, None) => None,
                    (None, x) | (x, None) => x,
                    (Some(min1), Some(min2)) => {
                        Some((min1.w.max(min2.w), min1.h.max(min2.h)).into())
                    }
                }
            })
            .map(|size| size + (0, TAB_HEIGHT).into())
    }
    pub fn max_size(&self) -> Option<Size<i32, Logical>> {
        let theoretical_max = self
            .surfaces()
            .fold(None, |max_size, window| {
                let win_max_size = window.max_size_without_ssd();
                match (max_size, win_max_size) {
                    (None, None) => None,
                    (None, x) | (x, None) => x,
                    (Some(max1), Some(max2)) => Some(
                        (
                            if max1.w == 0 {
                                max2.w
                            } else if max2.w == 0 {
                                max1.w
                            } else {
                                max1.w.min(max2.w)
                            },
                            if max1.h == 0 {
                                max2.h
                            } else if max2.h == 0 {
                                max1.h
                            } else {
                                max1.h.min(max2.h)
                            },
                        )
                            .into(),
                    ),
                }
            })
            .map(|size| size + (0, TAB_HEIGHT).into());
        // The problem is, with accumulated sizes, the minimum size could be larger than our maximum...
        let min_size = self.min_size();
        match (theoretical_max, min_size) {
            (None, _) => None,
            (Some(max), None) => Some(max),
            (Some(max), Some(min)) => Some((max.w.max(min.w), max.h.max(min.h)).into()),
        }
    }

    pub fn corner_radius(&self, geometry_size: Size<i32, Logical>, default_radius: u8) -> [u8; 4] {
        self.0.with_program(|p| {
            let active_window = &p.windows[p.active];
            let maximized = active_window.is_maximized(false);

            let round = (p.appearance_conf.clip_tiled_windows || !p.tiled) && !maximized;
            let radii = p
                .theme
                .cosmic()
                .radius_s()
                .map(|x| if x < 4.0 { x } else { x + 4.0 })
                .map(|val| val.round() as u8);

            if !round {
                let mut corners = active_window
                    .corner_radius(geometry_size)
                    .unwrap_or([default_radius; 4]);

                corners[0] = 0;
                corners[1] = 0;

                corners
            } else {
                let mut corners = active_window.corner_radius(geometry_size).unwrap_or(radii);

                corners[0] = radii[0];
                corners[1] = radii[1];
                corners[2] = radii[2].max(corners[2]);
                corners[3] = radii[3].max(corners[3]);

                corners
            }
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Message {
    DragStart,
    Menu,
    TabMenu(usize),
    PotentialTabDragStart(usize),
    Activate(usize),
    Close(usize),
    ScrollForward,
    ScrollBack,
    Scrolled,
}

impl TabMessage for Message {
    fn activate(idx: usize) -> Self {
        Message::Activate(idx)
    }

    fn scroll_back() -> Self {
        Message::ScrollBack
    }

    fn scroll_further() -> Self {
        Message::ScrollForward
    }

    fn populate_scroll(&mut self, mut current_offset: AbsoluteOffset) -> Option<AbsoluteOffset> {
        match self {
            Message::ScrollBack => Some({
                current_offset.x -= 10.;
                current_offset
            }),
            Message::ScrollForward => Some({
                current_offset.x += 10.;
                current_offset
            }),
            _ => None,
        }
    }

    fn scrolled() -> Self {
        Message::Scrolled
    }
}

impl Program for CosmicStackInternal {
    type Message = Message;

    fn update(
        &mut self,
        message: Self::Message,
        loop_handle: &LoopHandle<'static, crate::state::State>,
        last_seat: Option<&(Seat<State>, Serial)>,
    ) -> Task<Self::Message> {
        match message {
            Message::DragStart => {
                if let Some((seat, serial)) = last_seat.cloned()
                    && let Some(surface) =
                        self.windows[self.active].wl_surface().map(Cow::into_owned)
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
                            ModalBehavior::Block,
                        );
                        if let Some((grab, focus)) = res {
                            match grab.grab_type() {
                                GrabType::Touch => {
                                    seat.get_touch().unwrap().set_grab(state, grab, serial)
                                }
                                GrabType::Pointer => seat
                                    .get_pointer()
                                    .unwrap()
                                    .set_grab(state, grab, serial, focus),
                                GrabType::TabletTool => seat
                                    .tablet_seat()
                                    .get_tool(grab.tool().unwrap())
                                    .unwrap()
                                    .set_grab(state, grab, InputTime::now(), serial, focus),
                            }
                        }
                    });
                }
            }
            Message::PotentialTabDragStart(idx) => {
                self.potential_drag = Some(idx);
            }
            Message::Activate(idx) => {
                self.potential_drag = None;
                if let Some(surface) = self.windows.get(idx).cloned() {
                    loop_handle.insert_idle(move |state| {
                        if let Some(mapped) =
                            state.common.shell.read().element_for_surface(&surface)
                        {
                            mapped.stack_ref().unwrap().set_active(&surface);
                        }
                    });
                    self.scroll_to_focus = true;
                }
            }
            Message::Close(idx) => {
                if let Some(val) = self.windows.get(idx) {
                    val.close()
                }
            }
            Message::Scrolled => {
                self.scroll_to_focus = false;
            }
            Message::Menu => {
                if let Some((seat, serial)) = last_seat.cloned()
                    && let Some(surface) =
                        self.windows[self.active].wl_surface().map(Cow::into_owned)
                {
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

                            let mut cursor = seat
                                .get_pointer()
                                .unwrap()
                                .current_location()
                                .to_i32_round();
                            cursor.y -= TAB_HEIGHT;
                            let res = shell.menu_request(
                                false,
                                &surface,
                                &seat,
                                serial,
                                cursor - position.as_logical(),
                                true,
                                &state.common.config,
                                &state.common.event_loop_handle,
                            );

                            std::mem::drop(shell);
                            if let Some((grab, focus)) = res {
                                seat.get_pointer()
                                    .unwrap()
                                    .set_grab(state, grab, serial, focus);
                            }
                        }
                    });
                }
            }
            Message::TabMenu(idx) => {
                if let Some((seat, serial)) = last_seat.cloned()
                    && let Some(surface) = self.windows[idx].wl_surface().map(Cow::into_owned)
                {
                    loop_handle.insert_idle(move |state| {
                        let shell = state.common.shell.read();
                        if let Some(mapped) = shell.element_for_surface(&surface).cloned()
                            && let Some(workspace) = shell.space_for(&mapped)
                        {
                            let Some(elem_geo) = workspace.element_geometry(&mapped) else {
                                return;
                            };
                            let position = elem_geo.loc.to_global(&workspace.output);

                            let mut cursor = seat
                                .get_pointer()
                                .unwrap()
                                .current_location()
                                .to_i32_round();
                            cursor.y -= TAB_HEIGHT;
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
                                seat.get_pointer()
                                    .unwrap()
                                    .set_grab(state, grab, serial, focus);
                            }
                        }
                    });
                }
            }
            _ => unreachable!(),
        }
        Task::none()
    }

    fn view(&self) -> CosmicElement<'_, Self::Message> {
        HOOKS.get().unwrap().stack_decorations.view(self)
    }

    fn foreground(
        &self,
        pixels: &mut tiny_skia::PixmapMut<'_>,
        damage: &[Rectangle<i32, Buffer>],
        scale: f32,
        theme: &Theme,
    ) {
        if self.group_focused {
            let border = Rectangle::new(
                (0, ((TAB_HEIGHT as f32 * scale) - scale).floor() as i32).into(),
                (pixels.width() as i32, scale.ceil() as i32).into(),
            );

            let mut paint = tiny_skia::Paint::default();
            let (b, g, r, a) = theme.cosmic().accent_color().into_components();
            paint.set_color(tiny_skia::Color::from_rgba(r, g, b, a).unwrap());

            for rect in damage {
                if let Some(overlap) = rect.intersection(border) {
                    pixels.fill_rect(
                        tiny_skia::Rect::from_xywh(
                            overlap.loc.x as f32,
                            overlap.loc.y as f32,
                            overlap.size.w as f32,
                            overlap.size.h as f32,
                        )
                        .unwrap(),
                        &paint,
                        Default::default(),
                        None,
                    )
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct DefaultDecorations;

impl Decorations<CosmicStackInternal, Message> for DefaultDecorations {
    fn view(&self, stack: &CosmicStackInternal) -> cosmic::Element<'_, Message> {
        if stack.geometry.is_none() {
            return widget::space().into();
        };

        let elements = [
            widget::icon::from_name("window-stack-symbolic")
                .size(16)
                .prefer_svg(true)
                .icon()
                .class(if stack.group_focused {
                    theme::Svg::custom(|theme| widget::svg::Style {
                        color: Some(if theme.cosmic().is_dark {
                            Color::BLACK
                        } else {
                            Color::WHITE
                        }),
                    })
                } else {
                    theme::Svg::Default
                })
                .apply(widget::container)
                .center_y(24)
                .center_x(64)
                .apply(widget::mouse_area)
                .on_press(Message::DragStart)
                .on_right_press(Message::Menu)
                .into(),
            CosmicElement::new(
                Tabs::new(
                    stack.windows.iter().enumerate().map(|(i, w)| {
                        let user_data = w.user_data();
                        user_data.insert_if_missing(Id::unique);
                        Tab::new(
                            w.title(),
                            w.app_id(),
                            user_data.get::<Id>().unwrap().clone(),
                        )
                        .on_press(Message::PotentialTabDragStart(i))
                        .on_right_click(Message::TabMenu(i))
                        .on_close(Message::Close(i))
                    }),
                    stack.active,
                    stack.windows[stack.active].is_activated(false),
                    stack.group_focused,
                )
                .id(SCROLLABLE_ID.clone())
                .force_visible(stack.scroll_to_focus.then_some(stack.active))
                .height(Length::Fill)
                .width(Length::Fill),
            ),
            widget::space()
                .height(24)
                .width(64)
                .apply(widget::mouse_area)
                .on_press(Message::DragStart)
                .on_right_press(Message::Menu)
                .into(),
        ];

        let radius = if stack.windows[stack.active].is_maximized(false)
            || (stack.tiled && !stack.appearance_conf.clip_tiled_windows)
        {
            Radius::from(0.0)
        } else {
            let radii = stack
                .theme
                .cosmic()
                .radius_s()
                .map(|x| if x < 4.0 { x } else { x + 4.0 });
            Radius::from([radii[0], radii[1], 0., 0.])
        };
        let group_focused = stack.group_focused;

        widget::row(elements)
            .height(TAB_HEIGHT as u16)
            .width(Length::Fill)
            .apply(widget::container)
            .align_y(Alignment::Center)
            .class(theme::Container::custom(move |theme| {
                let cosmic_theme = theme.cosmic();

                let mut background = if group_focused {
                    cosmic_theme.accent_color()
                } else {
                    cosmic_theme.primary_container_color()
                };
                if cosmic_theme.frosted_windows {
                    background.alpha = cosmic_theme.alpha_map.blurred_alpha(cosmic_theme.frosted);
                }

                widget::container::Style {
                    snap: true,
                    icon_color: Some(
                        cosmic_theme
                            .background(cosmic_theme.frosted_windows)
                            .on
                            .into(),
                    ),
                    text_color: Some(
                        cosmic_theme
                            .background(cosmic_theme.frosted_windows)
                            .on
                            .into(),
                    ),
                    background: Some(Background::Color(background.into())),
                    border: Border {
                        radius,
                        width: 0.0,
                        color: Color::TRANSPARENT,
                    },
                    shadow: Default::default(),
                }
            }))
            .into()
    }
}

impl IsAlive for CosmicStack {
    fn alive(&self) -> bool {
        self.0
            .with_program(|p| p.override_alive && p.windows.iter().any(IsAlive::alive))
    }
}

impl SpaceElement for CosmicStack {
    fn bbox(&self) -> Rectangle<i32, Logical> {
        self.0.with_program(|p| {
            let mut bbox = SpaceElement::bbox(&p.windows[p.active]);
            bbox.loc -= Point::from((RESIZE_BORDER, RESIZE_BORDER));
            bbox.size += Size::from((RESIZE_BORDER * 2, RESIZE_BORDER * 2));
            bbox.size.h += TAB_HEIGHT;
            bbox
        })
    }
    fn is_in_input_region(&self, point: &Point<f64, Logical>) -> bool {
        self.focus_under(*point, WindowSurfaceType::ALL).is_some()
    }
    fn set_activate(&self, activated: bool) {
        SpaceElement::set_activate(&self.0, activated);
        let changed = self.0.with_program(|p| {
            if !p.group_focused {
                p.windows.iter().enumerate().for_each(|(i, w)| {
                    w.set_activated(activated && p.active == i);
                    w.send_configure();
                });
            }
            mem::replace(&mut p.activated, activated) != activated
        });

        if changed {
            self.0.force_redraw();
        }
    }
    fn output_enter(&self, output: &Output, overlap: Rectangle<i32, Logical>) {
        SpaceElement::output_enter(&self.0, output, overlap);
        self.0.with_program(|p| {
            p.windows
                .iter()
                .for_each(|w| SpaceElement::output_enter(w, output, overlap))
        })
    }
    fn output_leave(&self, output: &Output) {
        SpaceElement::output_leave(&self.0, output);
        self.0.with_program(|p| {
            p.windows
                .iter()
                .for_each(|w| SpaceElement::output_leave(w, output))
        })
    }
    fn geometry(&self) -> Rectangle<i32, Logical> {
        self.0.with_program(|p| {
            let mut geo = SpaceElement::geometry(&p.windows[p.active]);
            geo.size.h += TAB_HEIGHT;
            geo
        })
    }
    fn z_index(&self) -> u8 {
        self.0
            .with_program(|p| SpaceElement::z_index(&p.windows[p.active]))
    }
    fn refresh(&self) {
        self.0.with_program(|p| {
            // don't let the stack become empty
            let old_active = p.active;
            let active = p.windows[old_active].clone();
            p.windows.retain(IsAlive::alive);
            if p.windows.is_empty() {
                p.windows.push(active);
            }

            p.active = p.active.min(p.windows.len() - 1);

            if old_active != p.active {
                self.0
                    .resize(Size::from((self.active().geometry().size.w, TAB_HEIGHT)));
                self.0.force_redraw();
            }

            p.windows.iter().enumerate().for_each(|(i, w)| {
                if i == p.active {
                    w.set_suspended(false);
                } else {
                    w.set_suspended(true);
                }
                w.send_configure();

                SpaceElement::refresh(w)
            });
        });
        SpaceElement::refresh(&self.0);
    }
}

impl KeyboardTarget<State> for CosmicStack {
    fn enter(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        keys: Vec<KeysymHandle<'_>>,
        serial: Serial,
    ) {
        self.0.with_program(|p| {
            KeyboardTarget::enter(&p.windows[p.active], seat, data, keys, serial);
            p.previous_keyboard = Some(p.windows[p.active].downgrade());
        })
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial) {
        self.0.with_program(|p| {
            p.group_focused = false;
            if let Some(window) = p
                .previous_keyboard
                .take()
                .and_then(|prev| p.windows.iter().find(|w| **w == prev))
            {
                KeyboardTarget::leave(window, seat, data, serial)
            }
        })
    }
    fn key(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        key: KeysymHandle<'_>,
        state: KeyState,
        serial: Serial,
        time: InputTime,
    ) {
        self.0.with_program(|p| {
            p.keyboard_leave_if_previous(seat, data, serial);
            if !p.group_focused {
                KeyboardTarget::key(&p.windows[p.active], seat, data, key, state, serial, time)
            }
        })
    }
    fn modifiers(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        modifiers: ModifiersState,
        serial: Serial,
    ) {
        self.0.with_program(|p| {
            p.keyboard_leave_if_previous(seat, data, serial);
            if !p.group_focused {
                KeyboardTarget::modifiers(&p.windows[p.active], seat, data, modifiers, serial)
            }
        })
    }
}

impl PointerTarget<State> for CosmicStack {
    fn enter(&self, seat: &Seat<State>, data: &mut State, event: &PointerMotionEvent) {
        let mut event = event.clone();
        self.0.with_program(|p| {
            let active_window = &p.windows[p.active];
            let Some(next) = Focus::under(active_window, TAB_HEIGHT, event.location) else {
                return;
            };
            let _old_focus = p.swap_focus(Some(next));

            let mut cursor_state = seat
                .user_data()
                .get::<CursorState>()
                .unwrap()
                .lock()
                .unwrap();
            cursor_state.set_shape(next.cursor_shape());
            seat.set_cursor_image_status(CursorImageStatus::default_named());
        });

        event.location -= self
            .0
            .with_program(|p| p.windows[p.active].geometry().loc.to_f64());
        PointerTarget::enter(&self.0, seat, data, &event)
    }

    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &PointerMotionEvent) {
        let mut event = event.clone();
        self.0.with_program(|p| {
            let active_window = &p.windows[p.active];
            let Some(next) = Focus::under(active_window, TAB_HEIGHT, event.location) else {
                return;
            };
            let _previous = p.swap_focus(Some(next));

            let mut cursor_state = seat
                .user_data()
                .get::<CursorState>()
                .unwrap()
                .lock()
                .unwrap();
            cursor_state.set_shape(next.cursor_shape());
            seat.set_cursor_image_status(CursorImageStatus::default_named());
        });

        let active_window_geo = self.0.with_program(|p| p.windows[p.active].geometry());
        event.location -= active_window_geo.loc.to_f64();

        PointerTarget::motion(&self.0, seat, data, &event);
        if event.location.y < 0.0
            || event.location.y > TAB_HEIGHT as f64
            || event.location.x < 64.0
            || event.location.x > (active_window_geo.size.w as f64 - 64.0)
        {
            self.start_drag(data, seat, event.serial);
        }
    }

    fn relative_motion(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &RelativeMotionEvent,
    ) {
    }

    fn button(&self, seat: &Seat<State>, data: &mut State, event: &PointerButtonEvent) {
        match self.0.with_program(|p| p.current_focus()) {
            Some(Focus::Header) => PointerTarget::button(&self.0, seat, data, event),
            Some(x) => {
                let serial = event.serial;
                let seat = seat.clone();
                let Some(surface) = self.0.with_program(|p| {
                    let window = &p.windows[p.active];
                    window.wl_surface().map(Cow::into_owned)
                }) else {
                    return;
                };
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
                        ModalBehavior::Block,
                    );
                    if let Some((grab, focus)) = res
                        && let GrabType::Pointer = grab.grab_type()
                    {
                        seat.get_pointer()
                            .unwrap()
                            .set_grab(state, grab, serial, focus);
                    }
                });
            }
            None => {}
        }
    }

    fn axis(&self, seat: &Seat<State>, data: &mut State, frame: PointerAxisFrame) {
        if let Some(Focus::Header) = self.0.with_program(|p| p.current_focus()) {
            PointerTarget::axis(&self.0, seat, data, frame)
        }
    }

    fn frame(&self, seat: &Seat<State>, data: &mut State) {
        if let Some(Focus::Header) = self.0.with_program(|p| p.current_focus()) {
            PointerTarget::frame(&self.0, seat, data)
        }
    }

    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial, time: InputTime) {
        self.0.with_program(|p| {
            let mut cursor_state = seat
                .user_data()
                .get::<CursorState>()
                .unwrap()
                .lock()
                .unwrap();
            cursor_state.unset_shape();
            let _previous = p.swap_focus(None);
        });

        PointerTarget::leave(&self.0, seat, data, serial, time);

        if let Some(dragged_out) = self.0.with_program(|p| p.potential_drag.take())
            && let Some(surface) = self.0.with_program(|p| p.windows.get(dragged_out).cloned())
        {
            let seat = seat.clone();
            surface.try_force_undecorated(false);
            surface.send_configure();
            if let Some(surface) = surface.wl_surface().map(Cow::into_owned) {
                let _ = data.common.event_loop_handle.insert_idle(move |state| {
                    let res = state.common.shell.write().move_request(
                        &surface,
                        &seat,
                        serial,
                        ReleaseMode::NoMouseButtons,
                        true,
                        &state.common.config,
                        &state.common.event_loop_handle,
                        false,
                        ModalBehavior::Block,
                    );
                    if let Some((grab, focus)) = res
                        && let GrabType::Pointer = grab.grab_type()
                    {
                        seat.get_pointer()
                            .unwrap()
                            .set_grab(state, grab, serial, focus);
                    }
                });
            }
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

impl TouchTarget<State> for CosmicStack {
    fn down(&self, seat: &Seat<State>, data: &mut State, event: &TouchDownEvent) {
        let mut event = event.clone();
        let active_window_geo = self.0.with_program(|p| p.windows[p.active].geometry());
        event.location -= active_window_geo.loc.to_f64();
        self.0.with_program(|p| {
            p.touch_serial = event.serial.into();
        });
        TouchTarget::down(&self.0, seat, data, &event)
    }

    fn up(&self, seat: &Seat<State>, data: &mut State, event: &TouchUpEvent) {
        TouchTarget::up(&self.0, seat, data, event)
    }

    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &TouchMotionEvent) {
        let mut event = event.clone();
        let active_window_geo = self.0.with_program(|p| p.windows[p.active].geometry());
        event.location -= active_window_geo.loc.to_f64();
        TouchTarget::motion(&self.0, seat, data, &event);

        if event.location.y < 0.0
            || event.location.y > TAB_HEIGHT as f64
            || event.location.x < 64.0
            || event.location.x > (active_window_geo.size.w as f64 - 64.0)
        {
            self.start_drag(data, seat, self.0.with_program(|p| p.touch_serial).into());
        }
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

impl TabletToolTarget<State> for CosmicStack {
    fn proximity_in(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        tool_descriptor: &TabletToolDescriptor,
        tablet: &Tablet,
        serial: Serial,
    ) {
        TabletToolTarget::proximity_in(&self.0, seat, data, tool_descriptor, tablet, serial)
    }

    fn proximity_out(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        tool_descriptor: &TabletToolDescriptor,
    ) {
        self.0.with_program(|p| {
            let mut cursor_state = seat
                .user_data()
                .get::<CursorState>()
                .unwrap()
                .lock()
                .unwrap();
            cursor_state.unset_shape();
            let _previous = p.swap_focus(None);
        });
        TabletToolTarget::proximity_out(&self.0, seat, data, tool_descriptor);
    }

    fn down(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        tool_descriptor: &TabletToolDescriptor,
        event: &ToolDownEvent,
    ) {
        self.0.with_program(|p| {
            p.tablet_serial = event.serial.into();
        });
        match self.0.with_program(|p| p.current_focus()) {
            Some(Focus::Header) => {
                TabletToolTarget::down(&self.0, seat, data, tool_descriptor, event)
            }
            Some(x) => {
                let serial = self.0.with_program(|p| p.tablet_serial).into();
                let seat = seat.clone();
                let Some(surface) = self.0.with_program(|p| {
                    let window = &p.windows[p.active];
                    window.wl_surface().map(Cow::into_owned)
                }) else {
                    return;
                };
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
                        ModalBehavior::Block,
                    );
                    if let Some((grab, focus)) = res
                        && let GrabType::TabletTool = grab.grab_type()
                    {
                        seat.tablet_seat()
                            .get_tool(grab.tool().unwrap())
                            .unwrap()
                            .set_grab(state, grab, InputTime::now(), serial, focus);
                    }
                });
            }
            None => {}
        }
    }

    fn up(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        tool_descriptor: &TabletToolDescriptor,
        event: &ToolUpEvent,
    ) {
        TabletToolTarget::up(&self.0, seat, data, tool_descriptor, event)
    }

    fn motion(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        tool_descriptor: &TabletToolDescriptor,
        event: &ToolMotionEvent,
    ) {
        let mut event = event.clone();
        self.0.with_program(|p| {
            let active_window = &p.windows[p.active];
            let Some(next) = Focus::under(active_window, TAB_HEIGHT, event.location) else {
                return;
            };
            let _previous = p.swap_focus(Some(next));

            let mut cursor_state = seat
                .user_data()
                .get::<CursorState>()
                .unwrap()
                .lock()
                .unwrap();
            cursor_state.set_shape(next.cursor_shape());
            seat.set_cursor_image_status(CursorImageStatus::default_named());
        });

        let active_window_geo = self.0.with_program(|p| p.windows[p.active].geometry());
        event.location -= active_window_geo.loc.to_f64();

        TabletToolTarget::motion(&self.0, seat, data, tool_descriptor, &event);
        if event.location.y < 0.0
            || event.location.y > TAB_HEIGHT as f64
            || event.location.x < 64.0
            || event.location.x > (active_window_geo.size.w as f64 - 64.0)
        {
            self.start_drag(data, seat, self.0.with_program(|p| p.tablet_serial).into());
        }
    }

    fn axis(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        tool_descriptor: &TabletToolDescriptor,
        frame: ToolAxisFrame,
    ) {
        TabletToolTarget::axis(&self.0, seat, data, tool_descriptor, frame)
    }

    fn button(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        tool_descriptor: &TabletToolDescriptor,
        event: &ToolButtonEvent,
    ) {
        TabletToolTarget::button(&self.0, seat, data, tool_descriptor, event)
    }

    fn frame(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        tool_descriptor: &TabletToolDescriptor,
        time: InputTime,
    ) {
        TabletToolTarget::frame(&self.0, seat, data, tool_descriptor, time)
    }
}

pub enum CosmicStackRenderElement<R: Renderer + ImportAll + ImportMem> {
    Header(IcedRenderElement<R>),
    Shadow(PixelShaderElement),
    Border(PixelShaderElement),
    Window(SurfaceRenderElement<R>),
}

impl<R: Renderer + ImportAll + ImportMem> From<IcedRenderElement<R>>
    for CosmicStackRenderElement<R>
{
    fn from(value: IcedRenderElement<R>) -> Self {
        Self::Header(value)
    }
}

impl<R: Renderer + ImportAll + ImportMem> From<SurfaceRenderElement<R>>
    for CosmicStackRenderElement<R>
{
    fn from(value: SurfaceRenderElement<R>) -> Self {
        Self::Window(value)
    }
}

impl<R> Element for CosmicStackRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    R::TextureId: Send + 'static,
{
    fn id(&self) -> &RendererId {
        match self {
            CosmicStackRenderElement::Header(elem) => elem.id(),
            CosmicStackRenderElement::Shadow(elem) => elem.id(),
            CosmicStackRenderElement::Border(elem) => elem.id(),
            CosmicStackRenderElement::Window(elem) => elem.id(),
        }
    }

    fn current_commit(&self) -> CommitCounter {
        match self {
            CosmicStackRenderElement::Header(elem) => elem.current_commit(),
            CosmicStackRenderElement::Shadow(elem) => elem.current_commit(),
            CosmicStackRenderElement::Border(elem) => elem.current_commit(),
            CosmicStackRenderElement::Window(elem) => elem.current_commit(),
        }
    }

    fn src(&self) -> Rectangle<f64, Buffer> {
        match self {
            CosmicStackRenderElement::Header(elem) => elem.src(),
            CosmicStackRenderElement::Shadow(elem) => elem.src(),
            CosmicStackRenderElement::Border(elem) => elem.src(),
            CosmicStackRenderElement::Window(elem) => elem.src(),
        }
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        match self {
            CosmicStackRenderElement::Header(elem) => elem.geometry(scale),
            CosmicStackRenderElement::Shadow(elem) => elem.geometry(scale),
            CosmicStackRenderElement::Border(elem) => elem.geometry(scale),
            CosmicStackRenderElement::Window(elem) => elem.geometry(scale),
        }
    }

    fn location(&self, scale: Scale<f64>) -> Point<i32, Physical> {
        match self {
            CosmicStackRenderElement::Header(elem) => elem.location(scale),
            CosmicStackRenderElement::Shadow(elem) => elem.location(scale),
            CosmicStackRenderElement::Border(elem) => elem.location(scale),
            CosmicStackRenderElement::Window(elem) => elem.location(scale),
        }
    }

    fn transform(&self) -> Transform {
        match self {
            CosmicStackRenderElement::Header(elem) => elem.transform(),
            CosmicStackRenderElement::Shadow(elem) => elem.transform(),
            CosmicStackRenderElement::Border(elem) => elem.transform(),
            CosmicStackRenderElement::Window(elem) => elem.transform(),
        }
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<CommitCounter>,
    ) -> DamageSet<i32, Physical> {
        match self {
            CosmicStackRenderElement::Header(elem) => elem.damage_since(scale, commit),
            CosmicStackRenderElement::Shadow(elem) => elem.damage_since(scale, commit),
            CosmicStackRenderElement::Border(elem) => elem.damage_since(scale, commit),
            CosmicStackRenderElement::Window(elem) => elem.damage_since(scale, commit),
        }
    }

    fn opaque_regions(&self, scale: Scale<f64>) -> OpaqueRegions<i32, Physical> {
        match self {
            CosmicStackRenderElement::Header(elem) => elem.opaque_regions(scale),
            CosmicStackRenderElement::Shadow(elem) => elem.opaque_regions(scale),
            CosmicStackRenderElement::Border(elem) => elem.opaque_regions(scale),
            CosmicStackRenderElement::Window(elem) => elem.opaque_regions(scale),
        }
    }

    fn alpha(&self) -> f32 {
        match self {
            CosmicStackRenderElement::Header(elem) => elem.alpha(),
            CosmicStackRenderElement::Shadow(elem) => elem.alpha(),
            CosmicStackRenderElement::Border(elem) => elem.alpha(),
            CosmicStackRenderElement::Window(elem) => elem.alpha(),
        }
    }

    fn kind(&self) -> Kind {
        match self {
            CosmicStackRenderElement::Header(elem) => elem.kind(),
            CosmicStackRenderElement::Shadow(elem) => elem.kind(),
            CosmicStackRenderElement::Border(elem) => elem.kind(),
            CosmicStackRenderElement::Window(elem) => elem.kind(),
        }
    }

    fn is_framebuffer_effect(&self) -> bool {
        match self {
            CosmicStackRenderElement::Header(elem) => elem.is_framebuffer_effect(),
            CosmicStackRenderElement::Shadow(elem) => elem.is_framebuffer_effect(),
            CosmicStackRenderElement::Border(elem) => elem.is_framebuffer_effect(),
            CosmicStackRenderElement::Window(elem) => elem.is_framebuffer_effect(),
        }
    }
}

impl<R> RenderElement<R> for CosmicStackRenderElement<R>
where
    R: AsGlowRenderer,
    R::TextureId: Send + 'static,
{
    fn draw(
        &self,
        frame: &mut R::Frame<'_, '_>,
        src: Rectangle<f64, Buffer>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
        opaque_regions: &[Rectangle<i32, Physical>],
        cache: Option<&UserDataMap>,
    ) -> Result<(), <R>::Error> {
        match self {
            CosmicStackRenderElement::Header(elem) => {
                elem.draw(frame, src, dst, damage, opaque_regions, cache)
            }
            CosmicStackRenderElement::Shadow(elem) | CosmicStackRenderElement::Border(elem) => {
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
            CosmicStackRenderElement::Window(elem) => {
                elem.draw(frame, src, dst, damage, opaque_regions, cache)
            }
        }
    }

    fn underlying_storage(&self, renderer: &mut R) -> Option<UnderlyingStorage<'_>> {
        match self {
            CosmicStackRenderElement::Header(elem) => elem.underlying_storage(renderer),
            CosmicStackRenderElement::Shadow(elem) | CosmicStackRenderElement::Border(elem) => {
                elem.underlying_storage(renderer.glow_renderer_mut())
            }
            CosmicStackRenderElement::Window(elem) => elem.underlying_storage(renderer),
        }
    }

    fn capture_framebuffer(
        &self,
        frame: &mut R::Frame<'_, '_>,
        src: Rectangle<f64, Buffer>,
        dst: Rectangle<i32, Physical>,
        cache: &UserDataMap,
    ) -> Result<(), <R>::Error> {
        match self {
            CosmicStackRenderElement::Header(elem) => {
                elem.capture_framebuffer(frame, src, dst, cache)
            }
            CosmicStackRenderElement::Shadow(elem) | CosmicStackRenderElement::Border(elem) => {
                RenderElement::<GlowRenderer>::capture_framebuffer(
                    elem,
                    R::glow_frame_mut(frame),
                    src,
                    dst,
                    cache,
                )
                .map_err(R::from_gles_error)
            }
            CosmicStackRenderElement::Window(elem) => {
                elem.capture_framebuffer(frame, src, dst, cache)
            }
        }
    }
}
