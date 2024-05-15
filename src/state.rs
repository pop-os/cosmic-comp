// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::{kms::KmsState, winit::WinitState, x11::X11State},
    config::{Config, OutputConfig, OutputState},
    input::gestures::GestureState,
    shell::{grabs::SeatMoveGrabState, CosmicSurface, SeatExt, Shell},
    utils::prelude::OutputExt,
    wayland::protocols::{
        drm::WlDrmState,
        image_source::ImageSourceState,
        output_configuration::OutputConfigurationState,
        screencopy::ScreencopyState,
        toplevel_info::ToplevelInfoState,
        toplevel_management::{ManagementCapabilities, ToplevelManagementState},
        workspace::{WorkspaceClientState, WorkspaceState, WorkspaceUpdateGuard},
    },
    xwayland::XWaylandState,
};
use anyhow::Context;
use i18n_embed::{
    fluent::{fluent_language_loader, FluentLanguageLoader},
    DesktopLanguageRequester,
};
use once_cell::sync::Lazy;
use rust_embed::RustEmbed;
#[cfg(feature = "debug")]
use smithay::utils::Rectangle;
use smithay::{
    backend::{
        allocator::dmabuf::Dmabuf,
        drm::DrmNode,
        renderer::{
            element::{
                default_primary_scanout_output_compare, utils::select_dmabuf_feedback,
                RenderElementStates,
            },
            glow::GlowRenderer,
            ImportDma,
        },
    },
    desktop::{
        layer_map_for_output,
        utils::{
            send_dmabuf_feedback_surface_tree, send_frames_surface_tree,
            surface_presentation_feedback_flags_from_states, surface_primary_scanout_output,
            take_presentation_feedback_surface_tree, update_surface_primary_scanout_output,
            with_surfaces_surface_tree, OutputPresentationFeedback,
        },
        PopupManager,
    },
    input::{pointer::CursorImageStatus, SeatState},
    output::{Mode as OutputMode, Output, Scale},
    reexports::{
        calloop::{LoopHandle, LoopSignal},
        wayland_protocols::xdg::shell::server::xdg_toplevel::WmCapabilities,
        wayland_protocols_misc::server_decoration::server::org_kde_kwin_server_decoration_manager::Mode,
        wayland_server::{
            backend::{ClientData, ClientId, DisconnectReason},
            protocol::{wl_shm, wl_surface::WlSurface},
            Client, DisplayHandle, Resource,
        },
    },
    utils::{Clock, IsAlive, Monotonic},
    wayland::{
        compositor::{CompositorClientState, CompositorState},
        dmabuf::{DmabufFeedback, DmabufGlobal, DmabufState},
        fixes::FixesState,
        fractional_scale::{with_fractional_scale, FractionalScaleManagerState},
        idle_inhibit::IdleInhibitManagerState,
        idle_notify::IdleNotifierState,
        input_method::InputMethodManagerState,
        keyboard_shortcuts_inhibit::KeyboardShortcutsInhibitState,
        output::OutputManagerState,
        pointer_constraints::PointerConstraintsState,
        pointer_gestures::PointerGesturesState,
        presentation::PresentationState,
        seat::WaylandFocus,
        security_context::{SecurityContext, SecurityContextState},
        selection::{
            data_device::DataDeviceState, primary_selection::PrimarySelectionState,
            wlr_data_control::DataControlState,
        },
        session_lock::SessionLockManagerState,
        shell::{
            kde::decoration::KdeDecorationState,
            wlr_layer::WlrLayerShellState,
            xdg::{decoration::XdgDecorationState, XdgShellState},
        },
        shm::ShmState,
        tablet_manager::TabletManagerState,
        text_input::TextInputManagerState,
        viewporter::ViewporterState,
        virtual_keyboard::VirtualKeyboardManagerState,
        xdg_activation::XdgActivationState,
        xwayland_keyboard_grab::XWaylandKeyboardGrabState,
        xwayland_shell::XWaylandShellState,
    },
    xwayland::XWaylandClientData,
};
use time::UtcOffset;
use tracing::error;

use std::{
    cell::RefCell,
    collections::{HashSet, VecDeque},
    ffi::OsString,
    process::Child,
    sync::{Arc, Once, RwLock},
    time::{Duration, Instant},
};

#[derive(RustEmbed)]
#[folder = "resources/i18n"]
struct Localizations;

pub static LANG_LOADER: Lazy<FluentLanguageLoader> = Lazy::new(|| fluent_language_loader!());

#[macro_export]
macro_rules! fl {
    ($message_id:literal) => {{
        i18n_embed_fl::fl!($crate::state::LANG_LOADER, $message_id)
    }};

    ($message_id:literal, $($args:expr),*) => {{
        i18n_embed_fl::fl!($crate::state::LANG_LOADER, $message_id, $($args), *)
    }};
}

pub struct ClientState {
    pub compositor_client_state: CompositorClientState,
    pub workspace_client_state: WorkspaceClientState,
    pub advertised_drm_node: Option<DrmNode>,
    pub privileged: bool,
    pub evls: LoopSignal,
    pub security_context: Option<SecurityContext>,
}
impl ClientData for ClientState {
    fn initialized(&self, _client_id: ClientId) {}
    fn disconnected(&self, _client_id: ClientId, _reason: DisconnectReason) {
        self.evls.wakeup();
    }
}

pub fn advertised_node_for_client(client: &Client) -> Option<DrmNode> {
    // Lets check the global drm-node the client got either through default-feedback or wl_drm
    if let Some(normal_client) = client.get_data::<ClientState>() {
        return normal_client.advertised_drm_node.clone();
    }
    // last but not least all xwayland-surfaces should also share a single node
    if let Some(xwayland_client) = client.get_data::<XWaylandClientData>() {
        return xwayland_client.user_data().get::<DrmNode>().cloned();
    }
    None
}

pub fn advertised_node_for_surface(w: &WlSurface, dh: &DisplayHandle) -> Option<DrmNode> {
    let client = dh.get_client(w.id()).ok()?;
    advertised_node_for_client(&client)
}

#[derive(Debug)]
pub struct State {
    pub backend: BackendData,
    pub common: Common,
    pub ready: Once,
}

#[derive(Debug)]
pub struct Common {
    pub config: Config,

    pub socket: OsString,
    pub display_handle: DisplayHandle,
    pub event_loop_handle: LoopHandle<'static, State>,
    pub event_loop_signal: LoopSignal,

    pub popups: PopupManager,
    pub shell: Arc<RwLock<Shell>>,

    pub clock: Clock<Monotonic>,
    pub should_stop: bool,
    pub local_offset: time::UtcOffset,
    pub gesture_state: Option<GestureState>,

    pub kiosk_child: Option<Child>,
    pub theme: cosmic::Theme,

    #[cfg(feature = "debug")]
    pub egui: Egui,

    // wayland state
    pub compositor_state: CompositorState,
    pub data_device_state: DataDeviceState,
    pub dmabuf_state: DmabufState,
    pub fractional_scale_state: FractionalScaleManagerState,
    pub keyboard_shortcuts_inhibit_state: KeyboardShortcutsInhibitState,
    pub output_state: OutputManagerState,
    pub output_configuration_state: OutputConfigurationState<State>,
    pub presentation_state: PresentationState,
    pub primary_selection_state: PrimarySelectionState,
    pub data_control_state: Option<DataControlState>,
    pub image_source_state: ImageSourceState,
    pub screencopy_state: ScreencopyState,
    pub seat_state: SeatState<State>,
    pub session_lock_manager_state: SessionLockManagerState,
    pub idle_notifier_state: IdleNotifierState<State>,
    pub idle_inhibit_manager_state: IdleInhibitManagerState,
    pub idle_inhibiting_surfaces: HashSet<WlSurface>,
    pub shm_state: ShmState,
    pub wl_drm_state: WlDrmState<Option<DrmNode>>,
    pub viewporter_state: ViewporterState,
    pub kde_decoration_state: KdeDecorationState,
    pub xdg_decoration_state: XdgDecorationState,

    // shell-related wayland state
    pub xdg_shell_state: XdgShellState,
    pub layer_shell_state: WlrLayerShellState,
    pub toplevel_info_state: ToplevelInfoState<State, CosmicSurface>,
    pub toplevel_management_state: ToplevelManagementState,
    pub xdg_activation_state: XdgActivationState,
    pub workspace_state: WorkspaceState<State>,
    pub xwayland_state: Option<XWaylandState>,
    pub xwayland_shell_state: XWaylandShellState,
}

#[derive(Debug)]
pub enum BackendData {
    X11(X11State),
    Winit(WinitState),
    Kms(KmsState),
    // TODO
    // Wayland(WaylandState),
    Unset,
}

#[derive(Debug, Clone)]
pub struct SurfaceDmabufFeedback {
    pub render_feedback: DmabufFeedback,
    pub scanout_feedback: DmabufFeedback,
}

impl BackendData {
    pub fn kms(&mut self) -> &mut KmsState {
        match self {
            BackendData::Kms(ref mut kms_state) => kms_state,
            _ => unreachable!("Called kms in non kms backend"),
        }
    }

    pub fn x11(&mut self) -> &mut X11State {
        match self {
            BackendData::X11(ref mut x11_state) => x11_state,
            _ => unreachable!("Called x11 in non x11 backend"),
        }
    }

    pub fn winit(&mut self) -> &mut WinitState {
        match self {
            BackendData::Winit(ref mut winit_state) => winit_state,
            _ => unreachable!("Called winit in non winit backend"),
        }
    }

    pub fn apply_config_for_output(
        &mut self,
        output: &Output,
        test_only: bool,
        shell: &mut Shell,
        loop_handle: &LoopHandle<'_, State>,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        xdg_activation_state: &XdgActivationState,
    ) -> Result<(), anyhow::Error> {
        let result = match self {
            BackendData::Kms(ref mut state) => state.apply_config_for_output(
                output,
                shell,
                test_only,
                loop_handle,
                workspace_state,
                xdg_activation_state,
            ),
            BackendData::Winit(ref mut state) => state.apply_config_for_output(output, test_only),
            BackendData::X11(ref mut state) => state.apply_config_for_output(output, test_only),
            _ => unreachable!("No backend set when applying output config"),
        };

        if result.is_ok() {
            // apply to Output
            let final_config = output
                .user_data()
                .get::<RefCell<OutputConfig>>()
                .unwrap()
                .borrow();

            let mode = Some(OutputMode {
                size: final_config.mode_size(),
                refresh: final_config.mode_refresh() as i32,
            })
            .filter(|m| match output.current_mode() {
                None => true,
                Some(c_m) => m.size != c_m.size || m.refresh != c_m.refresh,
            });
            let transform =
                Some(final_config.transform.into()).filter(|x| *x != output.current_transform());
            let scale = Some(final_config.scale)
                .filter(|x| *x != output.current_scale().fractional_scale());
            let location =
                Some(final_config.position.into()).filter(|x| *x != output.current_location());
            output.change_current_state(mode, transform, scale.map(Scale::Fractional), location);

            output.set_adaptive_sync(final_config.vrr);
            output.set_mirroring(match &final_config.enabled {
                OutputState::Mirroring(conn) => shell
                    .outputs()
                    .find(|output| &output.name() == conn)
                    .cloned(),
                _ => None,
            });

            layer_map_for_output(output).arrange();
        }

        result
    }

    pub fn schedule_render(&mut self, loop_handle: &LoopHandle<'_, State>, output: &Output) {
        match self {
            BackendData::Winit(_) => {} // We cannot do this on the winit backend.
            // Winit has a very strict render-loop and skipping frames breaks atleast the wayland winit-backend.
            // Swapping with damage (which should be empty on these frames) is likely good enough anyway.
            BackendData::X11(ref mut state) => state.schedule_render(output),
            BackendData::Kms(ref mut state) => {
                if let Err(err) = state.schedule_render(loop_handle, output, None) {
                    error!(?err, "Failed to schedule event, are we shutting down?");
                }
            }
            _ => unreachable!("No backend was initialized"),
        }
    }

    pub fn dmabuf_imported(
        &mut self,
        client: Option<Client>,
        global: &DmabufGlobal,
        dmabuf: Dmabuf,
    ) -> Result<Option<DrmNode>, anyhow::Error> {
        match self {
            BackendData::Kms(ref mut state) => {
                return state
                    .dmabuf_imported(client, global, dmabuf)
                    .map(|node| Some(node))
            }
            BackendData::Winit(ref mut state) => {
                state.backend.renderer().import_dmabuf(&dmabuf, None)?;
            }
            BackendData::X11(ref mut state) => {
                state.renderer.import_dmabuf(&dmabuf, None)?;
            }
            _ => unreachable!("No backend set when importing dmabuf"),
        };
        Ok(None)
    }
}

pub fn client_has_no_security_context(client: &Client) -> bool {
    client
        .get_data::<ClientState>()
        .map_or(true, |client_state| client_state.security_context.is_none())
}

pub fn client_is_privileged(client: &Client) -> bool {
    client
        .get_data::<ClientState>()
        .map_or(false, |client_state| client_state.privileged)
}

fn enable_wayland_security() -> bool {
    std::env::var("COSMIC_ENABLE_WAYLAND_SECURITY")
        .map(|x| {
            x == "1"
                || x.to_lowercase() == "true"
                || x.to_lowercase() == "yes"
                || x.to_lowercase() == "y"
        })
        .unwrap_or(false)
}

impl State {
    pub fn new(
        dh: &DisplayHandle,
        socket: OsString,
        handle: LoopHandle<'static, State>,
        signal: LoopSignal,
    ) -> State {
        let requested_languages = DesktopLanguageRequester::requested_languages();
        i18n_embed::select(&*LANG_LOADER, &Localizations, &requested_languages)
            .with_context(|| "Failed to load languages")
            .unwrap();

        #[cfg(feature = "profile-with-tracy")]
        unsafe {
            time::util::local_offset::set_soundness(time::util::local_offset::Soundness::Unsound);
        }
        let local_offset = UtcOffset::current_local_offset().expect("No yet multithreaded");
        #[cfg(feature = "profile-with-tracy")]
        unsafe {
            time::util::local_offset::set_soundness(time::util::local_offset::Soundness::Sound);
        }
        let clock = Clock::new();
        let config = Config::load(&handle);
        let compositor_state = CompositorState::new::<Self>(dh);
        let data_device_state = DataDeviceState::new::<Self>(dh);
        let dmabuf_state = DmabufState::new();
        let fractional_scale_state = FractionalScaleManagerState::new::<State>(dh);
        let keyboard_shortcuts_inhibit_state = KeyboardShortcutsInhibitState::new::<Self>(dh);
        let output_state = OutputManagerState::new_with_xdg_output::<Self>(dh);
        let output_configuration_state = OutputConfigurationState::new(dh, client_is_privileged);
        let presentation_state = PresentationState::new::<Self>(dh, clock.id() as u32);
        let primary_selection_state = PrimarySelectionState::new::<Self>(dh);
        let image_source_state = ImageSourceState::new::<Self, _>(dh, client_is_privileged);
        let screencopy_state = ScreencopyState::new::<Self, _>(dh, client_is_privileged);
        let shm_state =
            ShmState::new::<Self>(dh, vec![wl_shm::Format::Xbgr8888, wl_shm::Format::Abgr8888]);
        let seat_state = SeatState::<Self>::new();
        let viewporter_state = ViewporterState::new::<Self>(dh);
        let wl_drm_state = WlDrmState::<Option<DrmNode>>::default();
        let kde_decoration_state = KdeDecorationState::new::<Self>(&dh, Mode::Client);
        let xdg_decoration_state = XdgDecorationState::new::<Self>(&dh);
        let session_lock_manager_state =
            SessionLockManagerState::new::<Self, _>(&dh, client_is_privileged);
        XWaylandKeyboardGrabState::new::<Self>(&dh);
        let xwayland_shell_state = XWaylandShellState::new::<Self>(&dh);
        PointerConstraintsState::new::<Self>(&dh);
        PointerGesturesState::new::<Self>(&dh);
        TabletManagerState::new::<Self>(&dh);
        SecurityContextState::new::<Self, _>(&dh, client_has_no_security_context);
        InputMethodManagerState::new::<Self, _>(&dh, client_is_privileged);
        TextInputManagerState::new::<Self>(&dh);
        VirtualKeyboardManagerState::new::<State, _>(&dh, client_is_privileged);
        FixesState::new::<Self>(&dh);

        let idle_notifier_state = IdleNotifierState::<Self>::new(&dh, handle.clone());
        let idle_inhibit_manager_state = IdleInhibitManagerState::new::<State>(&dh);
        let idle_inhibiting_surfaces = HashSet::new();

        let data_control_state = config.static_conf.data_control_enabled.then(|| {
            DataControlState::new::<Self, _>(dh, Some(&primary_selection_state), |_| true)
        });

        let shell = Arc::new(RwLock::new(Shell::new(&config)));

        let layer_shell_state =
            WlrLayerShellState::new_with_filter::<State, _>(dh, client_is_privileged);
        let xdg_shell_state = XdgShellState::new_with_capabilities::<State>(
            dh,
            [
                WmCapabilities::Fullscreen,
                WmCapabilities::Maximize,
                WmCapabilities::Minimize,
                WmCapabilities::WindowMenu,
            ],
        );
        let xdg_activation_state = XdgActivationState::new::<State>(dh);
        let toplevel_info_state = ToplevelInfoState::new(dh, client_is_privileged);
        let toplevel_management_state = ToplevelManagementState::new::<State, _>(
            dh,
            vec![
                ManagementCapabilities::Close,
                ManagementCapabilities::Activate,
                ManagementCapabilities::Maximize,
                ManagementCapabilities::Minimize,
                ManagementCapabilities::MoveToWorkspace,
            ],
            client_is_privileged,
        );
        let workspace_state = WorkspaceState::new(dh, client_is_privileged);

        if let Err(err) = crate::dbus::init(&handle) {
            tracing::warn!(?err, "Failed to initialize dbus handlers");
        }

        State {
            common: Common {
                config,
                socket,
                display_handle: dh.clone(),
                event_loop_handle: handle,
                event_loop_signal: signal,

                popups: PopupManager::default(),
                shell,

                local_offset,

                clock,
                should_stop: false,
                gesture_state: None,

                kiosk_child: None,
                theme: cosmic::theme::system_preference(),

                #[cfg(feature = "debug")]
                egui: Egui {
                    active: false,
                    state: smithay_egui::EguiState::new(Rectangle::from_loc_and_size(
                        (0, 0),
                        (800, 600),
                    )),
                },

                compositor_state,
                data_device_state,
                dmabuf_state,
                fractional_scale_state,
                idle_notifier_state,
                idle_inhibit_manager_state,
                idle_inhibiting_surfaces,
                image_source_state,
                screencopy_state,
                shm_state,
                seat_state,
                session_lock_manager_state,
                keyboard_shortcuts_inhibit_state,
                output_state,
                output_configuration_state,
                presentation_state,
                primary_selection_state,
                data_control_state,
                viewporter_state,
                wl_drm_state,
                kde_decoration_state,
                xdg_decoration_state,
                xdg_shell_state,
                layer_shell_state,
                toplevel_info_state,
                toplevel_management_state,
                xdg_activation_state,
                workspace_state,
                xwayland_state: None,
                xwayland_shell_state,
            },
            backend: BackendData::Unset,
            ready: Once::new(),
        }
    }

    pub fn new_client_state(&self) -> ClientState {
        ClientState {
            compositor_client_state: CompositorClientState::default(),
            workspace_client_state: WorkspaceClientState::default(),
            advertised_drm_node: match &self.backend {
                BackendData::Kms(kms_state) => Some(kms_state.primary_node),
                _ => None,
            },
            privileged: !enable_wayland_security(),
            evls: self.common.event_loop_signal.clone(),
            security_context: None,
        }
    }
}

impl Common {
    pub fn send_frames(
        &self,
        output: &Output,
        render_element_states: &RenderElementStates,
        mut dmabuf_feedback: impl FnMut(DrmNode) -> Option<SurfaceDmabufFeedback>,
    ) {
        let time = self.clock.now();
        let throttle = Some(Duration::from_secs(1));
        let shell = self.shell.read().unwrap();

        if let Some(session_lock) = shell.session_lock.as_ref() {
            if let Some(lock_surface) = session_lock.surfaces.get(output) {
                with_surfaces_surface_tree(lock_surface.wl_surface(), |_surface, states| {
                    with_fractional_scale(states, |fraction_scale| {
                        fraction_scale
                            .set_preferred_scale(output.current_scale().fractional_scale());
                    });
                });
                send_frames_surface_tree(
                    lock_surface.wl_surface(),
                    output,
                    time,
                    Some(Duration::ZERO),
                    surface_primary_scanout_output,
                );
                if let Some(feedback) =
                    advertised_node_for_surface(lock_surface.wl_surface(), &self.display_handle)
                        .and_then(|source| dmabuf_feedback(source))
                {
                    send_dmabuf_feedback_surface_tree(
                        &lock_surface.wl_surface(),
                        output,
                        surface_primary_scanout_output,
                        |surface, _| {
                            select_dmabuf_feedback(
                                surface,
                                render_element_states,
                                &feedback.render_feedback,
                                &feedback.scanout_feedback,
                            )
                        },
                    )
                }
            }
        }

        for seat in shell
            .seats
            .iter()
            .filter(|seat| &seat.active_output() == output)
        {
            let cursor_status = seat
                .user_data()
                .get::<RefCell<CursorImageStatus>>()
                .map(|cell| {
                    let mut cursor_status = cell.borrow_mut();
                    if let CursorImageStatus::Surface(ref surface) = *cursor_status {
                        if !surface.alive() {
                            *cursor_status = CursorImageStatus::default_named();
                        }
                    }
                    cursor_status.clone()
                })
                .unwrap_or(CursorImageStatus::default_named());

            if let CursorImageStatus::Surface(wl_surface) = cursor_status {
                send_frames_surface_tree(&wl_surface, output, time, Some(Duration::ZERO), |_, _| {
                    None
                })
            }

            if let Some(move_grab) = seat.user_data().get::<SeatMoveGrabState>() {
                if let Some(grab_state) = move_grab.borrow().as_ref() {
                    let window = grab_state.window();
                    window.with_surfaces(|surface, states| {
                        let primary_scanout_output = update_surface_primary_scanout_output(
                            surface,
                            output,
                            states,
                            render_element_states,
                            default_primary_scanout_output_compare,
                        );
                        if let Some(output) = primary_scanout_output {
                            with_fractional_scale(states, |fraction_scale| {
                                fraction_scale
                                    .set_preferred_scale(output.current_scale().fractional_scale());
                            });
                        }
                    });
                    window.send_frame(output, time, throttle, surface_primary_scanout_output);
                    if let Some(feedback) = window
                        .wl_surface()
                        .and_then(|wl_surface| {
                            advertised_node_for_surface(&wl_surface, &self.display_handle)
                        })
                        .and_then(|source| dmabuf_feedback(source))
                    {
                        window.send_dmabuf_feedback(
                            output,
                            &feedback,
                            render_element_states,
                            surface_primary_scanout_output,
                        );
                    }
                }
            }
        }

        shell
            .workspaces
            .sets
            .get(output)
            .unwrap()
            .sticky_layer
            .mapped()
            .for_each(|mapped| {
                let window = mapped.active_window();
                window.with_surfaces(|surface, states| {
                    let primary_scanout_output = update_surface_primary_scanout_output(
                        surface,
                        output,
                        states,
                        render_element_states,
                        |_current_output, _current_state, next_output, _next_state| next_output,
                    );
                    if let Some(output) = primary_scanout_output {
                        with_fractional_scale(states, |fraction_scale| {
                            fraction_scale
                                .set_preferred_scale(output.current_scale().fractional_scale());
                        });
                    }
                });
                window.send_frame(output, time, throttle, surface_primary_scanout_output);
                if let Some(feedback) = window
                    .wl_surface()
                    .and_then(|wl_surface| {
                        advertised_node_for_surface(&wl_surface, &self.display_handle)
                    })
                    .and_then(|source| dmabuf_feedback(source))
                {
                    window.send_dmabuf_feedback(
                        output,
                        &feedback,
                        render_element_states,
                        surface_primary_scanout_output,
                    );
                }
            });

        let active = shell.active_space(output);
        active.mapped().for_each(|mapped| {
            let window = mapped.active_window();
            window.with_surfaces(|surface, states| {
                let primary_scanout_output = update_surface_primary_scanout_output(
                    surface,
                    output,
                    states,
                    render_element_states,
                    |_current_output, _current_state, next_output, _next_state| next_output,
                );
                if let Some(output) = primary_scanout_output {
                    with_fractional_scale(states, |fraction_scale| {
                        fraction_scale
                            .set_preferred_scale(output.current_scale().fractional_scale());
                    });
                }
            });
            window.send_frame(output, time, throttle, surface_primary_scanout_output);
            if let Some(feedback) = window
                .wl_surface()
                .and_then(|wl_surface| {
                    advertised_node_for_surface(&wl_surface, &self.display_handle)
                })
                .and_then(|source| dmabuf_feedback(source))
            {
                window.send_dmabuf_feedback(
                    output,
                    &feedback,
                    render_element_states,
                    surface_primary_scanout_output,
                );
            }
        });
        active.minimized_windows.iter().for_each(|m| {
            let window = m.window.active_window();
            window.send_frame(output, time, throttle, |_, _| None);
        });

        for space in shell
            .workspaces
            .spaces_for_output(output)
            .filter(|w| w.handle != active.handle)
        {
            space.mapped().for_each(|mapped| {
                let window = mapped.active_window();
                window.send_frame(output, time, throttle, |_, _| None);
            });
            space.minimized_windows.iter().for_each(|m| {
                let window = m.window.active_window();
                window.send_frame(output, time, throttle, |_, _| None);
            })
        }

        shell.override_redirect_windows.iter().for_each(|or| {
            if let Some(wl_surface) = or.wl_surface() {
                with_surfaces_surface_tree(&wl_surface, |surface, states| {
                    let primary_scanout_output = update_surface_primary_scanout_output(
                        surface,
                        output,
                        states,
                        render_element_states,
                        default_primary_scanout_output_compare,
                    );
                    if let Some(output) = primary_scanout_output {
                        with_fractional_scale(states, |fraction_scale| {
                            fraction_scale
                                .set_preferred_scale(output.current_scale().fractional_scale());
                        });
                    }
                });
                send_frames_surface_tree(
                    &wl_surface,
                    output,
                    time,
                    throttle,
                    surface_primary_scanout_output,
                );
                if let Some(feedback) =
                    advertised_node_for_surface(&wl_surface, &self.display_handle)
                        .and_then(|source| dmabuf_feedback(source))
                {
                    send_dmabuf_feedback_surface_tree(
                        &wl_surface,
                        output,
                        surface_primary_scanout_output,
                        |surface, _| {
                            select_dmabuf_feedback(
                                surface,
                                render_element_states,
                                &feedback.render_feedback,
                                &feedback.scanout_feedback,
                            )
                        },
                    )
                }
            }
        });

        let map = smithay::desktop::layer_map_for_output(output);
        for layer_surface in map.layers() {
            layer_surface.with_surfaces(|surface, states| {
                let primary_scanout_output = update_surface_primary_scanout_output(
                    surface,
                    output,
                    states,
                    render_element_states,
                    default_primary_scanout_output_compare,
                );
                if let Some(output) = primary_scanout_output {
                    with_fractional_scale(states, |fraction_scale| {
                        fraction_scale
                            .set_preferred_scale(output.current_scale().fractional_scale());
                    });
                }
            });
            layer_surface.send_frame(output, time, throttle, surface_primary_scanout_output);
            if let Some(feedback) =
                advertised_node_for_surface(layer_surface.wl_surface(), &self.display_handle)
                    .and_then(|source| dmabuf_feedback(source))
            {
                layer_surface.send_dmabuf_feedback(
                    output,
                    surface_primary_scanout_output,
                    |surface, _| {
                        select_dmabuf_feedback(
                            surface,
                            render_element_states,
                            &feedback.render_feedback,
                            &feedback.scanout_feedback,
                        )
                    },
                );
            }
        }
    }

    pub fn take_presentation_feedback(
        &self,
        output: &Output,
        render_element_states: &RenderElementStates,
    ) -> OutputPresentationFeedback {
        let mut output_presentation_feedback = OutputPresentationFeedback::new(output);
        let shell = self.shell.read().unwrap();

        let active = shell.active_space(output);
        active.mapped().for_each(|mapped| {
            mapped.active_window().take_presentation_feedback(
                &mut output_presentation_feedback,
                surface_primary_scanout_output,
                |surface, _| {
                    surface_presentation_feedback_flags_from_states(surface, render_element_states)
                },
            );
        });

        shell.override_redirect_windows.iter().for_each(|or| {
            if let Some(wl_surface) = or.wl_surface() {
                take_presentation_feedback_surface_tree(
                    &wl_surface,
                    &mut output_presentation_feedback,
                    surface_primary_scanout_output,
                    |surface, _| {
                        surface_presentation_feedback_flags_from_states(
                            surface,
                            render_element_states,
                        )
                    },
                )
            }
        });

        let map = smithay::desktop::layer_map_for_output(output);
        for layer_surface in map.layers() {
            layer_surface.take_presentation_feedback(
                &mut output_presentation_feedback,
                surface_primary_scanout_output,
                |surface, _| {
                    surface_presentation_feedback_flags_from_states(surface, render_element_states)
                },
            );
        }

        output_presentation_feedback
    }
}

#[cfg(feature = "debug")]
#[derive(Debug)]
pub struct Egui {
    pub active: bool,
    pub state: smithay_egui::EguiState,
}

#[derive(Debug)]
pub struct Fps {
    #[cfg(feature = "debug")]
    pub rd: Option<renderdoc::RenderDoc<renderdoc::V110>>,
    #[cfg(feature = "debug")]
    pub state: smithay_egui::EguiState,
    pending_frame: Option<PendingFrame>,
    pub frames: VecDeque<Frame>,
}

#[derive(Debug)]
struct PendingFrame {
    start: Instant,
    duration_elements: Option<Duration>,
    duration_render: Option<Duration>,
    duration_displayed: Option<Duration>,
}

#[derive(Debug)]
pub struct Frame {
    pub start: Instant,
    pub duration_elements: Duration,
    pub duration_render: Duration,
    pub duration_displayed: Duration,
}

impl Frame {
    fn _render_time(&self) -> Duration {
        self.duration_elements + self.duration_render
    }

    fn frame_time(&self) -> Duration {
        self.duration_elements + self.duration_render
    }

    fn time_to_display(&self) -> Duration {
        self.duration_elements + self.duration_render + self.duration_displayed
    }
}

impl From<PendingFrame> for Frame {
    fn from(pending: PendingFrame) -> Self {
        Frame {
            start: pending.start,
            duration_elements: pending.duration_elements.unwrap_or(Duration::ZERO),
            duration_render: pending.duration_render.unwrap_or(Duration::ZERO),
            duration_displayed: pending.duration_displayed.unwrap_or(Duration::ZERO),
        }
    }
}

impl Fps {
    const WINDOW_SIZE: usize = 360;

    pub fn start(&mut self) {
        self.pending_frame = Some(PendingFrame {
            start: Instant::now(),
            duration_elements: None,
            duration_render: None,
            duration_displayed: None,
        });
    }

    pub fn elements(&mut self) {
        if let Some(frame) = self.pending_frame.as_mut() {
            frame.duration_elements = Some(Instant::now().duration_since(frame.start));
        }
    }

    pub fn render(&mut self) {
        if let Some(frame) = self.pending_frame.as_mut() {
            frame.duration_render = Some(
                Instant::now().duration_since(frame.start)
                    - frame.duration_elements.clone().unwrap_or(Duration::ZERO),
            );
        }
    }

    pub fn displayed(&mut self) {
        if let Some(mut frame) = self.pending_frame.take() {
            frame.duration_displayed = Some(
                Instant::now().duration_since(frame.start)
                    - frame.duration_elements.clone().unwrap_or(Duration::ZERO)
                    - frame.duration_render.clone().unwrap_or(Duration::ZERO),
            );

            self.frames.push_back(frame.into());
            while self.frames.len() > Fps::WINDOW_SIZE {
                self.frames.pop_front();
            }
        }
    }

    pub fn max_frametime(&self) -> Duration {
        self.frames
            .iter()
            .map(|f| f.frame_time())
            .max()
            .unwrap_or(Duration::ZERO)
    }

    pub fn min_frametime(&self) -> Duration {
        self.frames
            .iter()
            .map(|f| f.frame_time())
            .min()
            .unwrap_or(Duration::ZERO)
    }

    pub fn max_time_to_display(&self) -> Duration {
        self.frames
            .iter()
            .map(|f| f.time_to_display())
            .max()
            .unwrap_or(Duration::ZERO)
    }

    pub fn min_time_to_display(&self) -> Duration {
        self.frames
            .iter()
            .map(|f| f.time_to_display())
            .min()
            .unwrap_or(Duration::ZERO)
    }

    pub fn avg_frametime(&self) -> Duration {
        if self.frames.is_empty() {
            return Duration::ZERO;
        }
        self.frames.iter().map(|f| f.frame_time()).sum::<Duration>() / (self.frames.len() as u32)
    }

    pub fn avg_time_to_display(&self, window: usize) -> Duration {
        self.frames
            .iter()
            .rev()
            .take(window)
            .map(|f| f.time_to_display())
            .sum::<Duration>()
            / window as u32
    }

    pub fn avg_fps(&self) -> f64 {
        if self.frames.is_empty() {
            return 0.0;
        }
        let secs = match (self.frames.front(), self.frames.back()) {
            (Some(Frame { start, .. }), Some(end_frame)) => {
                end_frame.start.duration_since(*start) + end_frame.frame_time()
            }
            _ => Duration::ZERO,
        }
        .as_secs_f64();
        1.0 / (secs / self.frames.len() as f64)
    }
}

#[cfg(feature = "debug")]
static INTEL_LOGO: &'static [u8] = include_bytes!("../resources/icons/intel.svg");
#[cfg(feature = "debug")]
static AMD_LOGO: &'static [u8] = include_bytes!("../resources/icons/amd.svg");
#[cfg(feature = "debug")]
static NVIDIA_LOGO: &'static [u8] = include_bytes!("../resources/icons/nvidia.svg");

impl Fps {
    pub fn new(_renderer: &mut GlowRenderer) -> Fps {
        #[cfg(feature = "debug")]
        let state = {
            let state = smithay_egui::EguiState::new(smithay::utils::Rectangle::from_loc_and_size(
                (0, 0),
                (400, 800),
            ));
            let mut visuals: egui::style::Visuals = Default::default();
            visuals.window_shadow.extrusion = 0.0;
            state.context().set_visuals(visuals);
            state
                .load_svg(_renderer, String::from("intel"), INTEL_LOGO)
                .unwrap();
            state
                .load_svg(_renderer, String::from("amd"), AMD_LOGO)
                .unwrap();
            state
                .load_svg(_renderer, String::from("nvidia"), NVIDIA_LOGO)
                .unwrap();
            state
        };

        Fps {
            #[cfg(feature = "debug")]
            state,
            #[cfg(feature = "debug")]
            rd: renderdoc::RenderDoc::new().ok(),
            pending_frame: None,
            frames: VecDeque::with_capacity(Fps::WINDOW_SIZE + 1),
        }
    }
}

pub fn avg_fps<'a>(iter: impl Iterator<Item = &'a Duration>) -> f64 {
    let sum_secs = iter.map(|d| d.as_secs_f64()).sum::<f64>();
    1.0 / (sum_secs / Fps::WINDOW_SIZE as f64)
}
