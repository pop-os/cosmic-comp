// Native `one.playtron.GameMode` D-Bus interface for cosmic-comp.
//
// An app-centric gaming-control interface. Because cosmic-comp is a Wayland
// compositor that already tracks apps by app_id, the interface is compact: no
// X11 window enumeration, no XWayland topology, no per-window atom plumbing. It
// exposes the controls a game-mode client (the session/launcher manager) needs
// to drive game mode, with typed state instead of magic u32s + atoms.
//
// Architecture:
//   * A shared snapshot (`GameModeShared`) holds the readable state; the
//     interface getters serve straight from it.
//   * Control calls become `GameModeCommand`s pushed into the calloop event
//     loop, applied by `State::handle_game_mode_command`.
//   * The compositor pushes state changes + signals back out through
//     `GameModeBridge`.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, OnceLock};

use calloop::{LoopHandle, channel::Sender};
use futures_executor::ThreadPool;
use smithay::backend::drm::VrrSupport;
use smithay::output::Output;
use smithay::utils::IsAlive;
use smithay::xwayland::X11Surface;
use tracing::{debug, info, trace, warn};
use zbus::{interface, object_server::SignalEmitter};

use crate::logger::GAMING_TARGET;
use crate::shell::focus::target::KeyboardFocusTarget;
use crate::shell::{CosmicSurface, GameMode, Shell, WorkspaceDelta};
use crate::state::State;
use crate::utils::prelude::OutputExt;
use crate::wayland::protocols::workspace::WorkspaceHandle;

/// Well-known bus name cosmic-comp owns for gaming control.
const BUS_NAME: &str = "one.playtron.GameMode";
/// The single object path; one object, one interface (no ObjectManager needed).
const OBJECT_PATH: &str = "/one/playtron/GameMode";

/// App id the launcher shell presents as. `FocusedAppId == LAUNCHER_APP_ID`
/// means the launcher is in focus (i.e. not in a game).
///
/// A game window reports this via its own `STEAM_GAME` atom. The launcher,
/// however, is a native Wayland toplevel and can't carry an X11 atom, so it is
/// instead recognized by its Wayland `app_id` (see [`LAUNCHER_APP_IDS`]).
pub const LAUNCHER_APP_ID: u32 = 769;

/// Wayland `app_id`s (xdg_toplevel app_id, or X11 WM_CLASS) recognized as the
/// launcher shell, matched case-insensitively as a fallback when a focused
/// window has no `STEAM_GAME` atom. This keeps `FocusedAppId == LAUNCHER_APP_ID`
/// working for a native-Wayland launcher (which can't carry `STEAM_GAME`).
/// Add the launcher's `app_id` here if it ships under a different one.
const LAUNCHER_APP_IDS: &[&str] = &["one.playtron.grid", "grid"];

/// Variable-refresh-rate policy.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum VrrMode {
    /// Enable VRR automatically for a fullscreen game (the default).
    #[default]
    Auto,
    /// Always on.
    On,
    /// Always off.
    Off,
}

impl VrrMode {
    pub fn as_str(self) -> &'static str {
        match self {
            VrrMode::Auto => "auto",
            VrrMode::On => "on",
            VrrMode::Off => "off",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "auto" => Some(VrrMode::Auto),
            "on" => Some(VrrMode::On),
            "off" => Some(VrrMode::Off),
            _ => None,
        }
    }
}

/// How the game's render resolution is scaled to the physical output.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum ScalingMode {
    /// Present at native resolution, no scaling.
    #[default]
    Native,
    /// Integer (pixel-perfect) scaling.
    Integer,
    /// FSR spatial upscaling.
    Fsr,
    /// Aspect-preserving fit (letterbox).
    Fit,
    /// Aspect-preserving fill (pillarbox / crop).
    Fill,
    /// Non-uniform stretch to fill.
    Stretch,
}

impl ScalingMode {
    pub fn as_str(self) -> &'static str {
        match self {
            ScalingMode::Native => "native",
            ScalingMode::Integer => "integer",
            ScalingMode::Fsr => "fsr",
            ScalingMode::Fit => "fit",
            ScalingMode::Fill => "fill",
            ScalingMode::Stretch => "stretch",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "native" => Some(ScalingMode::Native),
            "integer" => Some(ScalingMode::Integer),
            "fsr" => Some(ScalingMode::Fsr),
            "fit" => Some(ScalingMode::Fit),
            "fill" => Some(ScalingMode::Fill),
            "stretch" => Some(ScalingMode::Stretch),
            _ => None,
        }
    }
}

/// The readable snapshot of game-mode state, served to clients. Minimal — no
/// window registry, no atoms.
#[derive(Debug, Default)]
pub struct GameModeShared {
    // State
    pub active: bool,
    /// The app currently in exclusive game mode (0 if none).
    pub active_app_id: u32,
    /// The app currently focused (the game, the launcher shell, or 0).
    pub focused_app_id: u32,
    pub overlay_visible: bool,

    // Tunables
    pub fps_limit: u32,
    pub tearing: bool,
    pub vrr_mode: VrrMode,
    pub hdr_enabled: bool,
    pub scale_width: u32,
    pub scale_height: u32,
    pub scale_mode: ScalingMode,

    // Capabilities / display
    pub vrr_supported: bool,
    pub hdr_supported: bool,
    pub tearing_supported: bool,
    pub refresh_rates: Vec<u32>,
    /// Current refresh rate of the game's output, in Hz.
    pub display_refresh_rate: u32,
    /// Whether the game's output is an external display (vs the internal panel).
    pub display_external: bool,
}

/// Shared I/O the interface uses: read state + send control commands.
#[derive(Debug)]
pub struct GameModeIo {
    pub shared: Mutex<GameModeShared>,
    cmd: Mutex<Sender<GameModeCommand>>,
    /// Live recent frame time (ns) of the output showing the game, written by the
    /// KMS surface thread (via [`Shell`]) and read by `AppFrametimeNs` for Auto-TDP.
    /// 0 when no game is fullscreen.
    frametime_ns: Arc<AtomicU64>,
}

impl GameModeIo {
    fn send(&self, cmd: GameModeCommand) {
        if let Ok(tx) = self.cmd.lock()
            && let Err(err) = tx.send(cmd)
        {
            warn!(?err, "Failed to forward game-mode command into event loop");
        }
    }

    fn with_shared<R>(&self, f: impl FnOnce(&GameModeShared) -> R) -> R {
        f(&self.shared.lock().unwrap())
    }
}

/// Control requests, applied on the compositor event loop.
#[derive(Debug, Clone)]
pub enum GameModeCommand {
    /// Enter game mode for an app id, plus the pid of the client that asked —
    /// used to recognize the windows it goes on to launch (see
    /// `GameMode::controller_pid`).
    Enter {
        app_id: u32,
        caller_pid: Option<u32>,
    },
    Exit,
    SetFpsLimit(u32),
    SetScaling {
        width: u32,
        height: u32,
        mode: ScalingMode,
    },
    SetTearing(bool),
    SetVrr(VrrMode),
    SetHdr(bool),
    SetOverlay {
        visible: bool,
        blocking: bool,
    },
}

/// Compositor-side handle: mutate the snapshot and emit signals. Stored on
/// [`Common`](crate::state::Common); the connection is wired in asynchronously
/// once `serve` connects.
#[derive(Clone)]
pub struct GameModeBridge {
    io: Arc<GameModeIo>,
    conn: Arc<OnceLock<zbus::Connection>>,
    executor: ThreadPool,
}

impl std::fmt::Debug for GameModeBridge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GameModeBridge")
            .field("connected", &self.conn.get().is_some())
            .finish()
    }
}

impl GameModeBridge {
    pub fn shared(&self) -> &Mutex<GameModeShared> {
        &self.io.shared
    }

    /// Shared handle the KMS surface thread writes the game's live frame time into
    /// (ns). Wired into [`Shell`] at startup so `AppFrametimeNs` reads live values.
    pub fn frametime_handle(&self) -> Arc<AtomicU64> {
        self.io.frametime_ns.clone()
    }

    fn spawn(&self, fut: impl std::future::Future<Output = ()> + Send + 'static) {
        self.executor.spawn_ok(fut);
    }

    fn with<R>(&self, f: impl FnOnce(&GameModeShared) -> R) -> R {
        f(&self.io.shared.lock().unwrap())
    }

    /// `StateChanged` + the Active/ActiveAppId property changes.
    pub fn notify_state_changed(&self) {
        let Some(conn) = self.conn.get().cloned() else {
            return;
        };
        let (active, app_id) = self.with(|s| (s.active, s.active_app_id));
        self.spawn(async move {
            if let Ok(emitter) = SignalEmitter::new(&conn, OBJECT_PATH) {
                let _ = GameModeInterface::state_changed(&emitter, active, app_id).await;
            }
            if let Ok(iref) = conn
                .object_server()
                .interface::<_, GameModeInterface>(OBJECT_PATH)
                .await
            {
                let e = iref.signal_emitter().clone();
                let iface = iref.get().await;
                let _ = iface.active_changed(&e).await;
                let _ = iface.active_app_id_changed(&e).await;
            }
        });
    }

    /// `FocusChanged` + the FocusedAppId/OverlayVisible property changes.
    pub fn notify_focus_changed(&self) {
        let Some(conn) = self.conn.get().cloned() else {
            return;
        };
        let (app_id, overlay) = self.with(|s| (s.focused_app_id, s.overlay_visible));
        self.spawn(async move {
            if let Ok(emitter) = SignalEmitter::new(&conn, OBJECT_PATH) {
                let _ = GameModeInterface::focus_changed(&emitter, app_id, overlay).await;
            }
            if let Ok(iref) = conn
                .object_server()
                .interface::<_, GameModeInterface>(OBJECT_PATH)
                .await
            {
                let e = iref.signal_emitter().clone();
                let iface = iref.get().await;
                let _ = iface.focused_app_id_changed(&e).await;
                let _ = iface.overlay_visible_changed(&e).await;
            }
        });
    }

    /// `CapabilitiesChanged` + the capability property changes.
    pub fn notify_capabilities_changed(&self) {
        let Some(conn) = self.conn.get().cloned() else {
            return;
        };
        self.spawn(async move {
            if let Ok(emitter) = SignalEmitter::new(&conn, OBJECT_PATH) {
                let _ = GameModeInterface::capabilities_changed(&emitter).await;
            }
            if let Ok(iref) = conn
                .object_server()
                .interface::<_, GameModeInterface>(OBJECT_PATH)
                .await
            {
                let e = iref.signal_emitter().clone();
                let iface = iref.get().await;
                let _ = iface.vrr_supported_changed(&e).await;
                let _ = iface.hdr_supported_changed(&e).await;
                let _ = iface.tearing_supported_changed(&e).await;
                let _ = iface.refresh_rates_changed(&e).await;
                let _ = iface.display_refresh_rate_changed(&e).await;
                let _ = iface.display_external_changed(&e).await;
            }
        });
    }

    /// `LauncherKeyPressed` — the launcher key (bare Super) changed state while
    /// game mode is active (`pressed` = key-down). Pure event, no state to mirror.
    pub fn notify_launcher_key(&self, pressed: bool) {
        let Some(conn) = self.conn.get().cloned() else {
            return;
        };
        self.spawn(async move {
            if let Ok(emitter) = SignalEmitter::new(&conn, OBJECT_PATH) {
                let _ = GameModeInterface::launcher_key_pressed(&emitter, pressed).await;
            }
        });
    }

    /// Property changes for the tunables (fps limit / tearing / vrr / scaling).
    pub fn notify_tunables_changed(&self) {
        let Some(conn) = self.conn.get().cloned() else {
            return;
        };
        self.spawn(async move {
            if let Ok(iref) = conn
                .object_server()
                .interface::<_, GameModeInterface>(OBJECT_PATH)
                .await
            {
                let e = iref.signal_emitter().clone();
                let iface = iref.get().await;
                let _ = iface.fps_limit_changed(&e).await;
                let _ = iface.tearing_changed(&e).await;
                let _ = iface.vrr_changed(&e).await;
                let _ = iface.hdr_enabled_changed(&e).await;
                let _ = iface.scaling_changed(&e).await;
            }
        });
    }
}

// ───────────────────────────── the interface ───────────────────────────────

/// `one.playtron.GameMode` — cosmic-comp's gaming-control interface.
pub struct GameModeInterface {
    io: Arc<GameModeIo>,
}

#[interface(name = "one.playtron.GameMode")]
impl GameModeInterface {
    /// Enter exclusive game mode for `app_id`: make that app the fullscreen game
    /// (engaging the direct-scanout + VRR + tearing fast paths) and clear the
    /// rest with an animated transition.
    async fn enter_game_mode(
        &self,
        app_id: u32,
        #[zbus(header)] header: zbus::message::Header<'_>,
        #[zbus(connection)] conn: &zbus::Connection,
    ) {
        // Resolve who asked. Windows this client subsequently spawns (the game it
        // launches) are recognized by process ancestry at MAP time, so they can be
        // placed on the game-mode output straight away instead of appearing on
        // whichever output the cursor happens to be on and then being moved.
        let caller_pid = match header.sender() {
            Some(sender) => zbus::fdo::DBusProxy::new(conn)
                .await
                .ok()
                .and_then(|proxy| {
                    futures_executor::block_on(
                        proxy.get_connection_unix_process_id(sender.to_owned().into()),
                    )
                    .ok()
                }),
            None => None,
        };
        self.io.send(GameModeCommand::Enter { app_id, caller_pid });
    }

    /// Leave game mode and return to the launcher.
    async fn exit_game_mode(&self) {
        self.io.send(GameModeCommand::Exit);
    }

    /// Cap the in-game frame rate (0 = uncapped).
    async fn set_fps_limit(&self, fps: u32) {
        self.io.send(GameModeCommand::SetFpsLimit(fps));
    }

    /// Set the game's render resolution + how it scales to the output.
    /// `mode` is one of `native|integer|fsr|fit|fill|stretch`.
    async fn set_scaling(&self, width: u32, height: u32, mode: &str) -> zbus::fdo::Result<()> {
        let mode = ScalingMode::parse(mode).ok_or_else(|| {
            zbus::fdo::Error::InvalidArgs(format!("unknown scaling mode {mode:?}"))
        })?;
        self.io.send(GameModeCommand::SetScaling {
            width,
            height,
            mode,
        });
        Ok(())
    }

    /// Allow tearing (immediate / non-vblank-latched flips) for the game.
    async fn set_tearing(&self, enabled: bool) {
        self.io.send(GameModeCommand::SetTearing(enabled));
    }

    /// Set the VRR policy: `off|on|auto`.
    async fn set_vrr(&self, mode: &str) -> zbus::fdo::Result<()> {
        let mode = VrrMode::parse(mode)
            .ok_or_else(|| zbus::fdo::Error::InvalidArgs(format!("unknown vrr mode {mode:?}")))?;
        self.io.send(GameModeCommand::SetVrr(mode));
        Ok(())
    }

    /// Enable/disable HDR output. No-op while `HdrSupported` is false (cosmic-comp
    /// has no HDR pipeline yet) — present for interface completeness.
    async fn set_hdr(&self, enabled: bool) {
        self.io.send(GameModeCommand::SetHdr(enabled));
    }

    /// Show/hide the launcher overlay over the game. `blocking` routes pointer
    /// input to the overlay while the game keeps rendering (the QAM behaviour).
    async fn set_overlay(&self, visible: bool, blocking: bool) {
        self.io
            .send(GameModeCommand::SetOverlay { visible, blocking });
    }

    // ── state ──
    #[zbus(property)]
    async fn active(&self) -> bool {
        self.io.with_shared(|s| s.active)
    }

    #[zbus(property)]
    async fn active_app_id(&self) -> u32 {
        self.io.with_shared(|s| s.active_app_id)
    }

    #[zbus(property)]
    async fn focused_app_id(&self) -> u32 {
        self.io.with_shared(|s| s.focused_app_id)
    }

    #[zbus(property)]
    async fn overlay_visible(&self) -> bool {
        self.io.with_shared(|s| s.overlay_visible)
    }

    // ── tunables (read; set via the methods above) ──
    #[zbus(property)]
    async fn fps_limit(&self) -> u32 {
        self.io.with_shared(|s| s.fps_limit)
    }

    #[zbus(property)]
    async fn tearing(&self) -> bool {
        self.io.with_shared(|s| s.tearing)
    }

    #[zbus(property)]
    async fn vrr(&self) -> String {
        self.io.with_shared(|s| s.vrr_mode.as_str().to_string())
    }

    #[zbus(property)]
    async fn hdr_enabled(&self) -> bool {
        self.io.with_shared(|s| s.hdr_enabled)
    }

    /// `(width, height, mode)` of the current game-mode scaling.
    #[zbus(property)]
    async fn scaling(&self) -> (u32, u32, String) {
        self.io.with_shared(|s| {
            (
                s.scale_width,
                s.scale_height,
                s.scale_mode.as_str().to_string(),
            )
        })
    }

    // ── capabilities ──
    #[zbus(property)]
    async fn vrr_supported(&self) -> bool {
        self.io.with_shared(|s| s.vrr_supported)
    }

    #[zbus(property)]
    async fn hdr_supported(&self) -> bool {
        self.io.with_shared(|s| s.hdr_supported)
    }

    #[zbus(property)]
    async fn tearing_supported(&self) -> bool {
        self.io.with_shared(|s| s.tearing_supported)
    }

    #[zbus(property)]
    async fn refresh_rates(&self) -> Vec<u32> {
        self.io.with_shared(|s| s.refresh_rates.clone())
    }

    /// Current refresh rate of the game's output, in Hz.
    #[zbus(property)]
    async fn display_refresh_rate(&self) -> u32 {
        self.io.with_shared(|s| s.display_refresh_rate)
    }

    /// Whether the game's output is an external display (vs the internal panel).
    #[zbus(property)]
    async fn display_external(&self) -> bool {
        self.io.with_shared(|s| s.display_external)
    }

    // ── metrics ──
    /// Recent frame time (ns) of `app_id` if it's the active fullscreen game, else
    /// 0. The per-app frame-timing source for Auto-TDP.
    async fn app_frametime_ns(&self, app_id: u32) -> u64 {
        if self
            .io
            .with_shared(|s| s.active && s.active_app_id == app_id)
        {
            self.io.frametime_ns.load(Ordering::Relaxed)
        } else {
            0
        }
    }

    // ── signals ──
    #[zbus(signal)]
    async fn state_changed(
        emitter: &SignalEmitter<'_>,
        active: bool,
        app_id: u32,
    ) -> zbus::Result<()>;

    #[zbus(signal)]
    async fn focus_changed(
        emitter: &SignalEmitter<'_>,
        app_id: u32,
        overlay_visible: bool,
    ) -> zbus::Result<()>;

    #[zbus(signal)]
    async fn capabilities_changed(emitter: &SignalEmitter<'_>) -> zbus::Result<()>;

    /// The launcher key (bare Super) changed state while game mode is active —
    /// `pressed` is true on key-down, false on key-up. Emitted on BOTH edges so
    /// the launcher (matching `LAUNCHER_APP_IDS`) can distinguish tap vs hold or
    /// change behavior later. The compositor takes no action itself.
    #[zbus(signal)]
    async fn launcher_key_pressed(emitter: &SignalEmitter<'_>, pressed: bool) -> zbus::Result<()>;
}

// ──────────────────────────── lifecycle / wiring ───────────────────────────

/// Set up the game-mode control bridge: shared snapshot, a command channel into
/// the event loop, and the D-Bus server on the executor.
pub fn init(handle: &LoopHandle<'static, State>, executor: &ThreadPool) -> GameModeBridge {
    let (cmd_tx, cmd_rx) = calloop::channel::channel::<GameModeCommand>();

    let io = Arc::new(GameModeIo {
        shared: Mutex::new(GameModeShared::default()),
        cmd: Mutex::new(cmd_tx),
        frametime_ns: Arc::new(AtomicU64::new(0)),
    });

    if let Err(err) = handle.insert_source(cmd_rx, |event, _, state| {
        if let calloop::channel::Event::Msg(cmd) = event {
            // Defer to an idle callback rather than running inline: handling a
            // command can enter/exit game mode, which fullscreens/minimizes windows
            // and drops their decoration iced elements — and an iced element's Drop
            // unregisters a calloop source. Doing that here, while this channel
            // source is mid-dispatch and the loop's source registry is borrowed,
            // panics ("RefCell already borrowed"). The idle runs once dispatch has
            // released the loop. (This mirrors how the input path defers window ops.)
            state
                .common
                .event_loop_handle
                .insert_idle(move |state| state.handle_game_mode_command(cmd));
        }
    }) {
        warn!(?err, "Failed to register game-mode command channel");
    }

    let conn = Arc::new(OnceLock::new());

    let serve_io = io.clone();
    let serve_conn = conn.clone();
    executor.spawn_ok(async move {
        match serve(serve_io).await {
            Ok(connection) => {
                let _ = serve_conn.set(connection);
                debug!("Serving {BUS_NAME}");
            }
            Err(err) => {
                warn!(
                    ?err,
                    "Failed to serve {BUS_NAME}; gaming control unavailable"
                );
            }
        }
    });

    GameModeBridge {
        io,
        conn,
        executor: executor.clone(),
    }
}

async fn serve(io: Arc<GameModeIo>) -> zbus::Result<zbus::Connection> {
    let conn = zbus::Connection::session().await?;
    conn.object_server()
        .at(OBJECT_PATH, GameModeInterface { io })
        .await?;
    conn.request_name(BUS_NAME).await?;
    Ok(conn)
}

// ─────────────────────────── compositor side ───────────────────────────────

impl State {
    /// Apply a control request that arrived over the game-mode D-Bus interface.
    pub fn handle_game_mode_command(&mut self, cmd: GameModeCommand) {
        let bridge = self.common.game_mode_bridge.clone();
        match cmd {
            GameModeCommand::Enter { app_id, caller_pid } => {
                debug!(target: GAMING_TARGET, app_id, ?caller_pid, launcher = app_id == LAUNCHER_APP_ID, "cmd: enter game mode");
                if caller_pid.is_some() {
                    self.common.shell.write().game_mode.controller_pid = caller_pid;
                }
                self.enter_game_mode(app_id);
                self.refresh_game_mode_state();
            }
            GameModeCommand::Exit => {
                debug!(target: GAMING_TARGET, "cmd: exit game mode");
                self.exit_game_mode();
                self.refresh_game_mode_state();
            }
            GameModeCommand::SetFpsLimit(fps) => {
                bridge.shared().lock().unwrap().fps_limit = fps;
                // The KMS surface thread reads this and caps the presentation
                // rate for the output showing the fullscreen game.
                self.common.shell.write().game_mode_fps_limit = fps;
                debug!(fps, "game-mode: set fps limit");
                bridge.notify_tunables_changed();
            }
            GameModeCommand::SetTearing(enabled) => {
                bridge.shared().lock().unwrap().tearing = enabled;
                self.common.shell.write().tearing_allowed = enabled;
                debug!(enabled, "game-mode: set tearing");
                bridge.notify_tunables_changed();
            }
            GameModeCommand::SetVrr(mode) => {
                bridge.shared().lock().unwrap().vrr_mode = mode;
                // The KMS surface thread reads this and overrides the output's
                // adaptive-sync policy while a game is fullscreen.
                self.common.shell.write().game_mode_vrr = mode;
                debug!(mode = mode.as_str(), "game-mode: set vrr");
                bridge.notify_tunables_changed();
            }
            GameModeCommand::SetHdr(enabled) => {
                bridge.shared().lock().unwrap().hdr_enabled = enabled;
                // No-op until cosmic-comp grows an HDR output pipeline; the state
                // is tracked so clients read back what they set.
                debug!(enabled, "game-mode: set hdr (no-op, HDR unsupported)");
                bridge.notify_tunables_changed();
            }
            GameModeCommand::SetScaling {
                width,
                height,
                mode,
            } => {
                {
                    let mut s = bridge.shared().lock().unwrap();
                    s.scale_width = width;
                    s.scale_height = height;
                    s.scale_mode = mode;
                }
                // TODO(gaming, PLAN.md Phase 3): resolution spoof + upscaler.
                debug!(
                    width,
                    height,
                    mode = mode.as_str(),
                    "game-mode: set scaling"
                );
                bridge.notify_tunables_changed();
            }
            GameModeCommand::SetOverlay { visible, blocking } => {
                // The native QAM can't set the X11 overlay marker, so it asserts
                // visibility over D-Bus; OR'd with real overlay-window presence.
                self.common.shell.write().game_mode.overlay_asserted = visible;
                self.refresh_overlay_visible();
                // `blocking` routes input to the QAM over the game (best-effort:
                // grab a present overlay/launcher window; restore on release).
                if visible && blocking {
                    self.grab_overlay_input();
                } else {
                    self.release_game_mode_input_grab();
                }
                debug!(target: GAMING_TARGET, visible, blocking, "cmd: set overlay");
            }
        }
    }

    /// Rebuild the game-mode snapshot (focus, display caps) from current
    /// compositor state and emit change notifications.
    pub fn refresh_game_mode_state(&mut self) {
        let bridge = self.common.game_mode_bridge.clone();
        let shell = self.common.shell.read();

        let active = shell.game_mode.active;
        let active_app_id = shell.game_mode.app_id.unwrap_or(0);

        // Focused app id: the `STEAM_GAME` app id of the focused window (0 if it
        // is untagged — e.g. a native Wayland toplevel; see `LAUNCHER_APP_ID`).
        let mut focused_app_id = shell
            .seats
            .last_active()
            .get_keyboard()
            .and_then(|kbd| kbd.current_focus())
            .map(|target| match target {
                // A fullscreen game's focus target is `Fullscreen`, which
                // `focused_element` can't resolve — read the app id directly.
                KeyboardFocusTarget::Fullscreen(surface) => app_id_of(&surface),
                KeyboardFocusTarget::Element(mapped) => app_id_of(&mapped.active_window()),
                _ => 0,
            })
            .unwrap_or(0);
        // In game mode the game (or an active overlay grab) is what's focused —
        // fall back to it when the live keyboard focus doesn't resolve (e.g. the
        // compositor's VT isn't foreground), so `FocusedAppId` tracks the game
        // rather than reading 0.
        if active && focused_app_id == 0 {
            focused_app_id = shell
                .game_mode
                .input_grab
                .as_ref()
                .map(app_id_of)
                .unwrap_or(active_app_id);
        }

        // Report caps for the output the GAME is on (not just the first output),
        // so refresh rate / VRR / external are correct on multi-monitor setups.
        let output = shell
            .game_mode
            .output
            .clone()
            .or_else(|| shell.outputs().next().cloned());
        // Tearing support is a device cap the game's KMS surface thread probes and
        // publishes (see `surface_thread`); read what it last wrote.
        let tearing_supported = shell
            .game_mode_tearing_supported
            .load(std::sync::atomic::Ordering::Relaxed);
        drop(shell);

        let display_refresh_rate = output
            .as_ref()
            .and_then(|o| o.current_mode())
            .map(|mode| (mode.refresh as f64 / 1000.0).round() as u32)
            .unwrap_or(0);
        let refresh_rates = if display_refresh_rate > 0 {
            vec![display_refresh_rate]
        } else {
            Vec::new()
        };
        let display_external = output.as_ref().map(|o| !o.is_internal()).unwrap_or(false);
        // VRR support: probed from the game's output connector (adaptive sync).
        let vrr_supported = matches!(
            output.as_ref().and_then(|o| o.adaptive_sync_support()),
            Some(VrrSupport::Supported | VrrSupport::RequiresModeset)
        );
        // HDR has no output pipeline yet, so it is never supported.
        let hdr_supported = false;

        let (state_changed, focus_changed, caps_changed) = {
            let mut s = bridge.shared().lock().unwrap();
            let state_changed = s.active != active || s.active_app_id != active_app_id;
            let focus_changed = s.focused_app_id != focused_app_id;
            let caps_changed = s.refresh_rates != refresh_rates
                || s.display_refresh_rate != display_refresh_rate
                || s.display_external != display_external
                || s.vrr_supported != vrr_supported
                || s.hdr_supported != hdr_supported
                || s.tearing_supported != tearing_supported;

            s.active = active;
            s.active_app_id = active_app_id;
            s.focused_app_id = focused_app_id;
            s.refresh_rates = refresh_rates;
            s.display_refresh_rate = display_refresh_rate;
            s.display_external = display_external;
            s.vrr_supported = vrr_supported;
            s.hdr_supported = hdr_supported;
            s.tearing_supported = tearing_supported;

            (state_changed, focus_changed, caps_changed)
        };

        if state_changed {
            bridge.notify_state_changed();
        }
        if focus_changed {
            bridge.notify_focus_changed();
        }
        if caps_changed {
            bridge.notify_capabilities_changed();
        }

        // The fullscreened game's window died (e.g. closed via Alt+F4) and nothing
        // else exits game mode, leaving it stranded on a dead surface. Return to
        // the launcher — it is still fullscreen on its own workspace, so switching
        // there shows it full-size (and the dead game's now-empty workspace reaps).
        let dead_app = {
            let shell = self.common.shell.read();
            let gm = &shell.game_mode;
            (gm.active && gm.game_surface.as_ref().is_some_and(|s| !s.alive()))
                .then_some((gm.app_id, gm.pending_app_id))
        };
        if let Some((app_id, pending)) = dead_app {
            if app_id == Some(LAUNCHER_APP_ID) {
                // The launcher itself died — leave game mode entirely, sliding back
                // to the desktop instead of an instant jump when its now-empty
                // workspace reaps.
                info!(target: GAMING_TARGET, "launcher window gone; leaving game mode");
                self.exit_game_mode();
            } else if pending != Some(LAUNCHER_APP_ID) {
                // The game surface died. A launching game churns through many
                // transient splash/helper windows before its real window settles;
                // if another LIVE window still carries this app id, re-adopt it
                // (find_game_surface picks the largest) instead of bouncing to the
                // launcher — otherwise the launch flaps game<->launcher on every
                // transient window death and the real game window (which maps late)
                // never gets adopted. Only when NO live window with this app id
                // remains do we return to the launcher; playserve also drives the
                // return when the game process actually exits.
                let re_adopt = app_id
                    .filter(|&id| id != LAUNCHER_APP_ID)
                    .filter(|&id| find_game_surface(&self.common.shell.read(), id).is_some());
                if let Some(id) = re_adopt {
                    info!(target: GAMING_TARGET, app_id = id, "game surface died; re-adopting another live window of the same game");
                    self.enter_game_mode(id);
                } else {
                    info!(target: GAMING_TARGET, "game window gone; returning to the launcher");
                    self.enter_game_mode(LAUNCHER_APP_ID);
                }
            }
        }

        // The game-mode window must stay fullscreen while active. Something else
        // (e.g. an exclusive layer-shell surface like the start menu taking focus
        // on another output) can drop its fullscreen out from under us; restore it
        // on its output WITHOUT stealing keyboard focus, so game mode is exclusive
        // and inviolable but the panel/menu the user opened still works.
        let restore_fullscreen = {
            let shell = self.common.shell.read();
            let gm = &shell.game_mode;
            let alive_surface = gm
                .game_surface
                .as_ref()
                .filter(|s| gm.active && s.alive())
                .cloned();
            alive_surface.and_then(|game| {
                let still_fullscreen = shell
                    .workspaces
                    .spaces()
                    .any(|ws| ws.get_fullscreen_surfaces().any(|f| f.surface == game));
                (!still_fullscreen)
                    .then(|| gm.output.clone().map(|output| (game, output)))
                    .flatten()
            })
        };
        if let Some((game, output)) = restore_fullscreen {
            warn!(target: GAMING_TARGET, "game surface lost fullscreen; restoring");
            let loop_handle = self.common.event_loop_handle.clone();
            let _ = self
                .common
                .shell
                .write()
                .fullscreen_request(&game, output, &loop_handle);
        }

        // Upscale: request a fill for the active game surface so a
        // game whose buffer is smaller than the output is scaled up (via the DRM
        // plane's hardware scaler). If the KMS thread latched a scale-reject
        // (`game_mode_scale_rejected`), stop requesting it and letterbox instead,
        // so a scanout-only buffer is never composited to black. The workspace
        // computes the fit rect and clears it when the buffer isn't smaller.
        {
            let scale_rejected = self
                .common
                .shell
                .read()
                .game_mode_scale_rejected
                .load(std::sync::atomic::Ordering::Relaxed);
            let game = {
                let shell = self.common.shell.read();
                shell
                    .game_mode
                    .active
                    .then(|| shell.game_mode.game_surface.clone())
                    .flatten()
            };
            if let Some(game) = game {
                // Only UPSCALE real games (which may render below the output). The
                // launcher (769) is a UI that renders AT output size; if it's ever
                // transiently/wrongly smaller it must letterbox (its own committed
                // buffer), never be scanned-out-then-composited-to-black. Also skip
                // when a prior scale was rejected (the letterbox fallback).
                let game_app_id = app_id_of(&game);
                let want_scale = !scale_rejected && game_app_id != LAUNCHER_APP_ID;
                let mut shell = self.common.shell.write();
                let matched_ws = if let Some(ws) = shell
                    .workspaces
                    .spaces_mut()
                    .find(|ws| ws.get_fullscreen_surfaces().any(|f| f.surface == game))
                {
                    ws.set_fullscreen_scale_to(&game, want_scale);
                    true
                } else {
                    false
                };
                drop(shell);
                // Per-tick (~150ms) so trace-level: a wrongly-set scale_to (or a
                // scale_rejected latch inherited across apps) letterboxes a small
                // centred image on a grey/black field (bug 1 look-alike).
                trace!(
                    target: GAMING_TARGET,
                    app_id = game_app_id,
                    scale_rejected,
                    want_scale,
                    matched_ws,
                    "upscale reconciled"
                );
            }
        }

        // Safety-net retry for a deferred enter (e.g. the game window mapped
        // since the last tick) and for a game surface retagged onto another window.
        self.try_resolve_pending_game_mode();
        self.refresh_active_game_surface();

        // Recompute the game's controlled children (its own dialogs / login
        // windows), which the render + input paths use to decide what may appear
        // above the game. Cheap: one workspace scan, and only while game mode is up.
        {
            let mut shell = self.common.shell.write();
            let base = shell
                .game_mode
                .active
                .then(|| shell.game_mode.game_surface.clone())
                .flatten();
            let children = match (base, shell.game_mode.app_id) {
                (Some(base), Some(app_id)) => resolve_game_children(&shell, &base, app_id),
                _ => Vec::new(),
            };
            if shell.game_mode.children != children {
                debug!(
                    target: GAMING_TARGET,
                    count = children.len(),
                    "game-mode controlled children changed"
                );
                shell.game_mode.children = children;
            }
        }

        // Focus reconciliation: nothing INVISIBLE may hold the keyboard. A window
        // can leave the controlled set while focused — most easily by fullscreening
        // itself, which moves it out of the workspace's mapped elements — and it
        // then renders nothing while still receiving keystrokes, which is
        // indistinguishable from a hung game. Hand the keyboard back to the game.
        let restore_focus = {
            let shell = self.common.shell.read();
            let seat = shell.seats.last_active().clone();
            let focused = seat
                .get_keyboard()
                .and_then(|kbd| kbd.current_focus())
                .and_then(|target| target.active_window());
            let base = shell
                .game_mode
                .active
                .then(|| shell.game_mode.game_surface.clone())
                .flatten();
            match (base, focused) {
                (Some(base), Some(focused))
                    if focused != base
                        && !shell.game_mode.children.contains(&focused)
                        && shell.game_mode.overlay_surface.as_ref() != Some(&focused)
                        && shell.game_mode.input_grab.as_ref() != Some(&focused)
                        && shell.game_mode_hides(&focused) =>
                {
                    Some((base, seat))
                }
                _ => None,
            }
        };
        if let Some((base, seat)) = restore_focus {
            debug!(target: GAMING_TARGET, "focus was on a window game mode does not render; restoring it to the game");
            Shell::set_focus(
                self,
                Some(&KeyboardFocusTarget::Fullscreen(base)),
                &seat,
                None,
                false,
            );
        }

        // Safety net for overlay presence (an overlay window mapped/unmapped) and
        // for an input grab whose window has gone away — the low-latency paths are
        // the property/map hooks, this catches anything they miss.
        self.refresh_overlay_visible();
        let stale_grab = {
            let shell = self.common.shell.read();
            shell.game_mode.input_grab.as_ref().is_some_and(|w| {
                !shell
                    .workspaces
                    .spaces()
                    .flat_map(|ws| ws.mapped())
                    .any(|m| m.windows().any(|(s, _)| s == *w))
            })
        };
        if stale_grab {
            self.release_game_mode_input_grab();
        }
    }

    /// Enter exclusive gaming mode for `app_id`: fullscreen the window tagged
    /// with that `STEAM_GAME` app id (which engages the VRR/direct-scanout/
    /// tearing fast path) and minimize everything else on its workspace. If no
    /// mapped window carries the app id yet, the request is deferred and resolved
    /// once one appears (see `try_resolve_pending_game_mode`).
    pub fn enter_game_mode(&mut self, app_id: u32) {
        let loop_handle = self.common.event_loop_handle.clone();
        // Preserve any client overlay assertion across a cross-app rebuild (the
        // GameMode literal below would otherwise reset it via `..Default`).
        let prev_overlay_asserted = self.common.shell.read().game_mode.overlay_asserted;

        // Already active on this app id AND still tracking a LIVE, correctly-tagged
        // surface for it — clear any pending marker and return. The liveness check
        // is essential for a relaunch: a native-Wayland launcher (Grid) has no map
        // hook, so the old session's dead surface is only reaped on the throttled
        // refresh tick. Without it, a relaunch's Enter(769) races that tick, sees
        // the stale `active && app_id==769`, and returns WITHOUT fullscreening the
        // NEW window — leaving it a small un-configured window (black). Falling
        // through re-resolves via find_game_surface and re-fullscreens the current
        // window with a fresh output-size configure.
        //
        // The early return is ALSO conditional on the adopted surface still being
        // the best candidate. A game maps a loading banner before its real window,
        // so we can legitimately adopt the banner first; without this check the
        // early return would pin game mode to that banner for the rest of the
        // session (the real window maps later and would never be adopted, since the
        // banner stays alive and correctly tagged). Falling through re-resolves and
        // upgrades to the real window; once it IS the best, this early-returns again.
        {
            let shell = self.common.shell.read();
            let current = shell
                .game_mode
                .game_surface
                .as_ref()
                .filter(|s| s.alive() && app_id_of(s) == app_id);
            if shell.game_mode.active
                && shell.game_mode.app_id == Some(app_id)
                && let Some(current) = current
                && find_game_surface(&shell, app_id).is_none_or(|(best, ..)| &best == current)
            {
                drop(shell);
                debug!(target: GAMING_TARGET, app_id, "enter no-op: already active on the best-ranked surface for this app id");
                self.common.shell.write().game_mode.pending_app_id = None;
                return;
            }
        }

        // A new game/app is entering — clear any prior scale-reject latch so it
        // retries the upscale rather than inheriting the last app's.
        self.common
            .shell
            .read()
            .game_mode_scale_rejected
            .store(false, std::sync::atomic::Ordering::Relaxed);

        // Resolve the app's window anywhere: already fullscreen on its own
        // game-mode workspace, or a normal window on the desktop. Defer if it
        // isn't mapped yet (resolved later from the refresh tick / property hook).
        let resolved = {
            let shell = self.common.shell.read();
            find_game_surface(&shell, app_id)
        };
        let Some((game, source_ws, output, is_fullscreen)) = resolved else {
            self.common.shell.write().game_mode.pending_app_id = Some(app_id);
            info!(target: GAMING_TARGET, app_id, "game mode deferred: no window with this app id yet");
            return;
        };

        // Game mode owns a display, so a game must appear THERE regardless of where
        // its window happened to map. A window with no placement of its own lands on
        // the seat's active output (`map_window`), i.e. wherever the cursor is — so
        // launching a game while the cursor sits on another output would otherwise
        // drag game mode along with it. Adopt onto the output game mode already
        // owns, and treat the window as needing a move even if it managed to
        // fullscreen itself on the wrong one.
        let (output, is_fullscreen, relocate_fullscreen) = {
            let shell = self.common.shell.read();
            match shell.game_mode.output.clone() {
                Some(current) if shell.game_mode.active && current != output => {
                    info!(
                        target: GAMING_TARGET,
                        app_id,
                        from = %output.name(),
                        to = %current.name(),
                        "app mapped on another output; moving it to the game-mode output"
                    );
                    (current, false, is_fullscreen)
                }
                _ => (output, is_fullscreen, false),
            }
        };

        let seat = self.common.shell.read().seats.last_active().clone();
        // Record the normal-desktop workspace only on the first entry into game
        // mode, so a full exit can return there; app switches keep that origin.
        let first_entry = !self.common.shell.read().game_mode.active;
        let dbg_output = output.name();

        let focus_target = {
            let mut shell = self.common.shell.write();

            let home_workspace = if first_entry {
                shell.active_space(&output).map(|ws| ws.handle)
            } else {
                shell.game_mode.home_workspace
            };

            let focus = if is_fullscreen {
                // Already fullscreen on its own workspace — switch to it with a
                // cross-fade (each game-mode workspace is a single fullscreen
                // surface, so this dissolves launcher<->game, no
                // slide).
                info!(target: GAMING_TARGET, app_id, "cross-fading to game-mode workspace");
                if let Some(idx) = shell.workspaces.idx_for_handle(&output, &source_ws) {
                    let _ = shell.activate(
                        &output,
                        idx,
                        WorkspaceDelta::new_crossfade(),
                        &mut self.common.workspace_state.update(),
                    );
                }
                Some(KeyboardFocusTarget::Fullscreen(game.clone()))
            } else {
                // Normal desktop window: move it onto the trailing empty (clean)
                // workspace — background/shell are suppressed there by
                // `game_mode_exclusive` — sliding to it, then fullscreen it.
                // Nothing is minimized; the desktop it came from is left intact.
                let target = shell
                    .workspaces
                    .spaces_for_output(&output)
                    .find(|ws| ws.is_empty())
                    .or_else(|| shell.workspaces.spaces_for_output(&output).last())
                    .map(|ws| ws.handle);
                if relocate_fullscreen {
                    // It fullscreened itself on the output we are moving it AWAY
                    // from. A fullscreen surface is not a mapped element, so
                    // move_window cannot relocate it — drop fullscreen first and
                    // let the fullscreen_request below re-apply it on the target.
                    let _ = shell.unfullscreen_request(&game, &loop_handle);
                }
                if let Some(target) = target {
                    info!(target: GAMING_TARGET, app_id, "moving app onto a clean game-mode workspace");
                    let _ = shell.move_window(
                        Some(&seat),
                        &game,
                        &source_ws,
                        &target,
                        true, // follow: slide to the new workspace
                        None,
                        &mut self.common.workspace_state.update(),
                        &loop_handle,
                    );
                }
                shell.fullscreen_request(&game, output.clone(), &loop_handle)
            };

            shell.game_mode = GameMode {
                active: true,
                app_id: Some(app_id),
                game_surface: Some(game),
                output: Some(output),
                home_workspace,
                pending_app_id: None,
                overlay_asserted: prev_overlay_asserted,
                ..Default::default()
            };

            focus
        };

        // The single state write that flips game mode on and latches which
        // surface/output/workspace it targets — the anchor for diagnosing the grey
        // entrance slide (bug 1) and the stuck-frame-on-return (bug 3).
        info!(
            target: GAMING_TARGET,
            app_id,
            launcher = app_id == LAUNCHER_APP_ID,
            is_fullscreen,
            first_entry,
            output = %dbg_output,
            "game mode engaged: latched game surface"
        );

        // Give the game keyboard focus so it receives input immediately.
        if let Some(target) = focus_target {
            Shell::set_focus(self, Some(&target), &seat, None, true);
        }
    }

    /// If a deferred [`State::enter_game_mode`] is pending and a window resolving
    /// to that app id now exists, enter game mode for it. Called from the
    /// `map_window_notify` and `STEAM_GAME` property hooks (X11) and the refresh
    /// tick — the latter is what resolves a native-Wayland target like the launcher
    /// (which hits neither X11 hook), driven by its own map/commit activity.
    pub fn try_resolve_pending_game_mode(&mut self) {
        let pending = self.common.shell.read().game_mode.pending_app_id;
        if let Some(app_id) = pending {
            let found = {
                let shell = self.common.shell.read();
                find_game_surface(&shell, app_id).is_some()
            };
            if found {
                self.enter_game_mode(app_id);
                // Reconcile overlay_active + the D-Bus OverlayVisible mirror now,
                // matching the explicit Enter-command path (which refreshes after).
                self.refresh_overlay_visible();
            }
        }
    }

    /// Re-resolve the fullscreened game surface after a `STEAM_GAME` tag change.
    ///
    /// `enter_game_mode` treats a re-enter for the *same* app id as a no-op, so
    /// moving an app id from one window to another (e.g. a client switching
    /// between overlay windows that share an app id) would otherwise leave the
    /// old window fullscreened. This detects that the active game surface has lost
    /// its app-id tag and switches to whichever mapped window still carries it.
    /// It runs synchronously in the property/refresh hooks (no render in between),
    /// so the swap is seamless. A no-op for a normal game, whose window keeps its
    /// tag for the whole session.
    pub fn refresh_active_game_surface(&mut self) {
        let (app_id, overlay_asserted) = {
            let shell = self.common.shell.read();
            let gm = &shell.game_mode;
            // Only when actively fullscreening a concrete surface.
            let (true, Some(app_id), Some(surface)) =
                (gm.active, gm.app_id, gm.game_surface.as_ref())
            else {
                return;
            };
            // Current surface still resolves to the active app id — nothing to do.
            // Uses `app_id_of` (not the raw atom) so the launcher, matched by its
            // Wayland `app_id`, isn't seen as "untagged" and re-resolved every tick.
            if app_id_of(surface) == app_id {
                return;
            }
            (app_id, gm.overlay_asserted)
        };

        // The fullscreened surface lost its tag. Switch to another window still
        // carrying the active app id, if one exists (`find_game_surface` now also
        // sees fullscreen windows). If nothing carries it, leave the current
        // surface up: this may be a transient untag mid-retag, or the window is
        // closing (the refresh tick's destroy handling covers a vanished surface).
        // We never exit game mode from here on our own.
        let has_replacement = {
            let shell = self.common.shell.read();
            find_game_surface(&shell, app_id).is_some()
        };
        if has_replacement {
            info!(target: GAMING_TARGET, app_id, "re-resolving game surface after a tag change");
            // exit returns to the desktop (reaping the stale workspace); enter then
            // fullscreens the newly-tagged window on a fresh workspace.
            self.exit_game_mode();
            self.enter_game_mode(app_id);
            // exit_game_mode's mem::take cleared any client overlay assertion, and
            // enter_game_mode re-read it as false — restore it so a QAM overlay and
            // its fast-path gate survive the swap.
            self.common.shell.write().game_mode.overlay_asserted = overlay_asserted;
            self.refresh_overlay_visible();
        }
    }

    /// Fully leave game mode: un-fullscreen the current app (so its clean
    /// game-mode workspace empties and auto-reaps) and return to the normal
    /// desktop workspace we entered from. Nothing was minimized, so there is
    /// nothing to restore.
    pub fn exit_game_mode(&mut self) {
        let loop_handle = self.common.event_loop_handle.clone();
        let mut shell = self.common.shell.write();
        if !shell.game_mode.active {
            shell.game_mode.pending_app_id = None;
            return;
        }

        let game_mode = std::mem::take(&mut shell.game_mode);
        // The scale-reject latch lives on Shell (not the GameMode struct that
        // mem::take just reset), so clear it here too — a fresh game must not
        // inherit the previous one's letterbox latch.
        shell
            .game_mode_scale_rejected
            .store(false, std::sync::atomic::Ordering::Relaxed);

        // Un-fullscreen the game so its now-empty game-mode workspace auto-reaps.
        let dbg_game_app_id = game_mode.game_surface.as_ref().map(app_id_of);
        if let Some(game) = &game_mode.game_surface {
            let _ = shell.unfullscreen_request(game, &loop_handle);
        }

        // Slide back to the desktop workspace game mode was entered from. That
        // workspace may have been auto-reaped once the app moved off it and it
        // emptied, so fall back to the first workspace on the game's output —
        // exit must never strand us on the game-mode workspace.
        let target = game_mode
            .home_workspace
            .and_then(|home| {
                shell
                    .workspaces
                    .space_for_handle(&home)
                    .map(|w| w.output().clone())
                    .and_then(|output| {
                        shell
                            .workspaces
                            .idx_for_handle(&output, &home)
                            .map(|idx| (output, idx))
                    })
            })
            .or_else(|| game_mode.output.clone().map(|output| (output, 0)));
        if let Some((output, idx)) = target {
            let dbg_target_output = output.name();
            let _ = shell.activate(
                &output,
                idx,
                WorkspaceDelta::new_shortcut(),
                &mut self.common.workspace_state.update(),
            );
            drop(shell);
            // Bug 3: if the launcher/desktop doesn't take over here, the paused
            // game's last frame stays the scanned-out primary. Correlate this with
            // the KMS "primary-plane surface changed" trace on the same output.
            info!(
                target: GAMING_TARGET,
                game_app_id = ?dbg_game_app_id,
                target_output = %dbg_target_output,
                target_idx = idx,
                "exited game mode: returned to desktop/launcher workspace"
            );
        } else {
            drop(shell);
            warn!(
                target: GAMING_TARGET,
                game_app_id = ?dbg_game_app_id,
                "exited game mode: NO target workspace resolved (stranded on game-mode workspace)"
            );
        }
    }

    /// Recompute whether a gaming overlay is up over the game — a real
    /// `STEAM_OVERLAY`/`GAMESCOPE_EXTERNAL_OVERLAY` window or a client
    /// `SetOverlay(true)` assertion. Updates the render-path flag
    /// (`game_mode.overlay_active`, which drops the tearing/scanout fast path so
    /// the overlay composites) and the D-Bus `OverlayVisible`/`FocusChanged`.
    pub fn refresh_overlay_visible(&mut self) {
        let bridge = self.common.game_mode_bridge.clone();
        let (overlay_active, dbg_asserted, dbg_window_present, dbg_surface_app_id) = {
            let mut shell = self.common.shell.write();
            let asserted = shell.game_mode.overlay_asserted;
            let window_present = overlay_window_present(&shell);
            let active = shell.game_mode.active && (window_present || asserted);
            shell.game_mode.overlay_active = active;
            // Resolve the surface the OverlaySurface render path composites over
            // the game — ONLY for the D-Bus-asserted (Wayland launcher) overlay.
            // A real X11 STEAM_OVERLAY/GAMESCOPE_EXTERNAL_OVERLAY window composites
            // via its own override-redirect render path, so don't stack a launcher
            // for it (that would wrongly show Grid over the game on Shift+Tab).
            let is_overlay_window = |w: &CosmicSurface| {
                w.is_overlay() || LAUNCHER_APP_IDS.contains(&w.app_id().to_lowercase().as_str())
            };
            let surface = (active && asserted)
                .then(|| {
                    shell.workspaces.spaces().find_map(|ws| {
                        // Normal mapped windows first, then fullscreen surfaces: when
                        // game mode has latched the launcher it is FULLSCREEN, so it
                        // lives in `fullscreen_surfaces`, not `mapped()` — scanning
                        // only `mapped()` here is why the QAM resolved to None.
                        ws.mapped()
                            .flat_map(|m| m.windows().map(|(s, _)| s))
                            .find(|w| is_overlay_window(w))
                            .or_else(|| {
                                ws.get_fullscreen_surfaces()
                                    .map(|f| f.surface.clone())
                                    .find(|w| is_overlay_window(w))
                            })
                    })
                })
                .flatten();
            let surface_app_id = surface.as_ref().map(app_id_of);
            shell.game_mode.overlay_surface = surface;
            (active, asserted, window_present, surface_app_id)
        };
        let changed = {
            let mut s = bridge.shared().lock().unwrap();
            let changed = s.overlay_visible != overlay_active;
            s.overlay_visible = overlay_active;
            changed
        };
        if changed {
            // Edge-triggered (only when overlay_active flips) so it's info-safe on
            // release builds. BUG-2 signature: asserted=true with surface_app_id=None
            // means the QAM was requested but no launcher/overlay window resolved to
            // composite over the game (overlay_active gates dropping direct scanout;
            // overlay_surface is what the OverlaySurface render stage stacks on top).
            debug!(
                target: GAMING_TARGET,
                overlay_active,
                asserted = dbg_asserted,
                window_present = dbg_window_present,
                surface_app_id = ?dbg_surface_app_id,
                "overlay visibility changed"
            );
            bridge.notify_focus_changed();
        }
    }

    /// Grant or release the per-window input grab from `window`'s
    /// `STEAM_INPUT_FOCUS` (non-zero = grab). Only redirects seat focus — never
    /// changes fullscreen/minimize, so the scanout/VRR fast path stays engaged.
    /// Override-redirect overlays (which can't be element-focused) are left to
    /// grab input themselves; this is a no-op for them.
    pub fn update_game_mode_input_grab(&mut self, x11: &X11Surface) {
        if !self.common.shell.read().game_mode.active {
            return;
        }
        // Resolve the CANONICAL mapped surface for this X11 window (stable Window
        // id — building a fresh `CosmicSurface::from` would never compare equal).
        // `None` for override-redirect windows: they aren't in the workspace and
        // self-grab via X, so there is nothing for the compositor to do.
        let canonical = {
            let shell = self.common.shell.read();
            shell
                .element_for_x11_window_id(x11.window_id())
                .map(|m| m.active_window())
        };
        let Some(window) = canonical else { return };
        if x11.steam_input_focus().is_some_and(|v| v != 0) {
            self.set_game_mode_input_grab(Some(window));
        } else if self.common.shell.read().game_mode.input_grab.as_ref() == Some(&window) {
            self.set_game_mode_input_grab(None);
        }
    }

    /// Release any active input grab (restore focus to the game). Called on exit
    /// and when the grab window goes away.
    pub fn release_game_mode_input_grab(&mut self) {
        if self.common.shell.read().game_mode.input_grab.is_some() {
            self.set_game_mode_input_grab(None);
        }
    }

    /// Best-effort input grab for the native QAM (`SetOverlay(blocking)`): route
    /// focus to a present overlay window, else the launcher.
    fn grab_overlay_input(&mut self) {
        if !self.common.shell.read().game_mode.active {
            return;
        }
        let target = {
            let shell = self.common.shell.read();
            shell
                .workspaces
                .spaces()
                .flat_map(|ws| ws.mapped())
                .flat_map(|m| m.windows().map(|(s, _)| s))
                .find(|w| {
                    w.is_overlay() || LAUNCHER_APP_IDS.contains(&w.app_id().to_lowercase().as_str())
                })
        };
        if let Some(window) = target {
            self.set_game_mode_input_grab(Some(window));
        }
    }

    /// Redirect seat keyboard focus to `target` over the game (or back to the
    /// game when `None`), tracking it in `game_mode.input_grab`. Only records a
    /// grab for a window the compositor can actually focus — override-redirect
    /// overlays self-grab via X and are left untouched (no grab recorded), so the
    /// stale-grab tick never thrashes them.
    fn set_game_mode_input_grab(&mut self, target: Option<CosmicSurface>) {
        let seat = self.common.shell.read().seats.last_active().clone();
        // (store, do_focus, focus_arg): `do_focus=false` means leave focus alone
        // (an override-redirect grab); `focus_arg=None` with `do_focus=true` means
        // let normal focus management re-derive a target.
        let (store, do_focus, focus_arg): (
            Option<CosmicSurface>,
            bool,
            Option<KeyboardFocusTarget>,
        ) = {
            let shell = self.common.shell.read();
            match &target {
                Some(window) => match focus_target_for(&shell, window) {
                    Some(f) => (target.clone(), true, Some(f)),
                    None => (None, false, None),
                },
                None => (
                    None,
                    true,
                    shell
                        .game_mode
                        .game_surface
                        .clone()
                        .map(KeyboardFocusTarget::Fullscreen),
                ),
            }
        };
        self.common.shell.write().game_mode.input_grab = store;
        if do_focus {
            Shell::set_focus(self, focus_arg.as_ref(), &seat, None, true);
        }
    }
}

/// App id for a surface: its `STEAM_GAME` atom if tagged (games carry their own),
/// else the launcher id if its Wayland `app_id` is a known launcher (see
/// [`LAUNCHER_APP_IDS`] — the launcher is native Wayland and can't carry the X11
/// atom), else 0.
pub fn app_id_of(surface: &CosmicSurface) -> u32 {
    if let Some(appid) = surface.steam_appid() {
        return appid;
    }
    // Steam Big Picture (a Steam client window with no game appid) is the
    // base-layer launcher, like the desktop launcher shell.
    if surface.is_steam_client() {
        return LAUNCHER_APP_ID;
    }
    let app_id = surface.app_id().to_lowercase();
    if LAUNCHER_APP_IDS.contains(&app_id.as_str()) {
        return LAUNCHER_APP_ID;
    }
    0
}

/// Find the mapped window tagged with `app_id` (`STEAM_GAME`), searching every
/// workspace on every output. Returns the window, the output it currently lives
/// on, and its co-workspace peers (everything else on that workspace, to be
/// minimized). `None` if no mapped window carries that app id yet.
/// Locate the window carrying `app_id` anywhere, returning it with its workspace
/// handle, output, and whether it is ALREADY fullscreen (i.e. already in game mode
/// on that workspace) rather than a normal window on the desktop. Matches on
/// `app_id_of` (not the raw `STEAM_GAME` atom) so the native-Wayland launcher —
/// which can't carry the atom and is recognized by its Wayland `app_id` (see
/// `LAUNCHER_APP_IDS`) — resolves too.
fn find_game_surface(
    shell: &Shell,
    app_id: u32,
) -> Option<(CosmicSurface, WorkspaceHandle, Output, bool)> {
    // 0 is `app_id_of`'s "unknown" sentinel (any untagged, non-launcher window),
    // never a valid game-mode target. Guard against it so an accidental
    // `enter_game(0)` can't fullscreen an arbitrary window.
    if app_id == 0 {
        return None;
    }
    // A launching game spawns MANY windows all carrying its STEAM_GAME id (1x1
    // IME/input helpers, a loading banner/splash, then the real game window, which
    // maps LATE). Collect every LIVE match and take the best-ranked one; skipping
    // dead surfaces means a just-closed transient is never re-adopted.
    let mut candidates: Vec<(CosmicSurface, WorkspaceHandle, Output, bool)> = Vec::new();
    for ws in shell.workspaces.spaces() {
        for f in ws.get_fullscreen_surfaces() {
            if f.surface.alive() && app_id_of(&f.surface) == app_id {
                candidates.push((f.surface.clone(), ws.handle, ws.output().clone(), true));
            }
        }
        for mapped in ws.mapped() {
            let surface = mapped.active_window();
            if surface.alive() && app_id_of(&surface) == app_id {
                candidates.push((surface, ws.handle, ws.output().clone(), false));
            }
        }
    }
    // Ascending sort, then pop the max = best candidate.
    candidates.sort_by_key(|(surface, _, _, is_fullscreen)| {
        (base_candidate_rank(surface), *is_fullscreen)
    });
    candidates.pop()
}

/// The windows that belong WITH the adopted game — its own dialogs, EULA /
/// launcher windows and in-prefix login/browser windows — and may therefore
/// render above it under strict control.
///
/// Membership is an ALLOWLIST rooted at `base`, which is what keeps strict
/// control intact: a window unrelated to the adopted game (notably a Proton game
/// that raw-fullscreens itself before playserve tags it) matches none of these
/// and stays invisible.
///
///   * same `STEAM_GAME` id — playserve tags every window in the app's reaper pid
///     tree, so a game's dialogs and its in-prefix CEF login window already carry
///     the game's id;
///   * same pid — only same-process windows may composite over the game;
///   * `WM_TRANSIENT_FOR` pointing at the base.
///
/// 1x1/zero-area stubs are excluded: games map those as IME/input helpers and
/// offscreen login stubs, and they must never be presented.
///
/// Whether `surface` belongs with the game adopted as `base`.
///
/// The single source of truth for controlled-set membership: the render path
/// (via `resolve_game_children`) and the focus path (`Shell::game_mode_hides`)
/// must agree, or a window renders without being focusable — or worse, takes the
/// keyboard while invisible. Evaluated directly against `base` so it is also
/// correct for a window that just mapped, before the refresh tick has rebuilt
/// `GameMode::children`.
pub fn is_game_child(base: &CosmicSurface, app_id: u32, surface: &CosmicSurface) -> bool {
    if surface == base || !surface.alive() || surface.is_useless() {
        return false;
    }
    let base_pid = base.pid();
    let base_window_id = base.x11_window_id();
    app_id_of(surface) == app_id
        || (base_pid.is_some() && surface.pid() == base_pid)
        || (base_window_id.is_some() && surface.transient_for() == base_window_id)
}

fn resolve_game_children(shell: &Shell, base: &CosmicSurface, app_id: u32) -> Vec<CosmicSurface> {
    let Some(ws) = shell
        .workspaces
        .spaces()
        .find(|ws| ws.get_fullscreen_surfaces().any(|f| &f.surface == base))
    else {
        return Vec::new();
    };
    let mut children: Vec<CosmicSurface> = ws
        .mapped()
        .map(|mapped| mapped.active_window())
        .filter(|surface| is_game_child(base, app_id, surface))
        .collect();
    // Apply the session manager's base-layer priority: a window whose app id
    // appears EARLIER in the list stacks above one that appears later, which is
    // how a custom webview is put over a still-running game without focusing it.
    // Windows the list doesn't mention keep their existing relative order behind
    // the ones it does (stable sort, unlisted sorts last).
    let priority = |surface: &CosmicSurface| {
        let id = app_id_of(surface);
        shell
            .game_mode
            .baselayer_appids
            .iter()
            .position(|listed| *listed == id)
            .unwrap_or(usize::MAX)
    };
    if !shell.game_mode.baselayer_appids.is_empty() {
        children.sort_by_key(priority);
    }
    children
}

/// How suitable `surface` is to be game mode's fullscreen base, ordered so that
/// GREATER is better: prefer a real toplevel over a menu/splash artifact, and
/// among equals prefer the one with the most committed content.
///
/// This is what keeps a game's loading banner from being adopted (and then
/// fullscreened) in place of the real game window that maps a moment later.
fn base_candidate_rank(surface: &CosmicSurface) -> (u8, u8, i64) {
    use smithay::desktop::space::SpaceElement as _;

    let substantial = u8::from(!surface.is_useless());
    let real_toplevel = u8::from(!surface.maybe_a_dropdown());
    let size = surface.bbox().size;
    let area = i64::from(size.w.max(0)) * i64::from(size.h.max(0));
    (substantial, real_toplevel, area)
}

/// Whether any `STEAM_OVERLAY`/`GAMESCOPE_EXTERNAL_OVERLAY` window is currently
/// mapped — a normal toplevel or an override-redirect window (the Steam overlay
/// is typically override-redirect).
fn overlay_window_present(shell: &Shell) -> bool {
    shell
        .workspaces
        .spaces()
        .any(|ws| ws.mapped().any(|m| m.active_window().is_overlay()))
        || shell.override_redirect_windows.iter().any(|s| {
            s.steam_overlay().is_some_and(|v| v != 0)
                || s.external_overlay().is_some_and(|v| v != 0)
        })
}

/// The keyboard-focus target for `window` if it is a mapped element. Returns
/// `None` for override-redirect windows (which grab input themselves and can't
/// be element-focused).
fn focus_target_for(shell: &Shell, window: &CosmicSurface) -> Option<KeyboardFocusTarget> {
    shell
        .workspaces
        .spaces()
        .flat_map(|ws| ws.mapped())
        .find(|m| m.windows().any(|(s, _)| s == *window))
        .map(|m| KeyboardFocusTarget::from(m.clone()))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// End-to-end over a live session bus: own the well-known name, then from a
    /// separate client connection introspect the member surface, read a
    /// property, and drive a control method. Uses only client→server calls (a
    /// self-introspect deadlocks under `zbus::block_on`). Skips if no bus.
    #[test]
    fn interface_over_bus() {
        zbus::block_on(async {
            let (tx, rx) = calloop::channel::channel::<GameModeCommand>();
            std::mem::forget(rx);
            let io = Arc::new(GameModeIo {
                shared: Mutex::new(GameModeShared {
                    active: true,
                    active_app_id: 1234,
                    focused_app_id: LAUNCHER_APP_ID,
                    ..Default::default()
                }),
                cmd: Mutex::new(tx),
                frametime_ns: Arc::new(AtomicU64::new(0)),
            });
            let conn = match zbus::Connection::session().await {
                Ok(c) => c,
                Err(_) => {
                    eprintln!("no session bus; skipping");
                    return;
                }
            };
            if conn
                .object_server()
                .at(OBJECT_PATH, GameModeInterface { io })
                .await
                .is_err()
                || conn.request_name(BUS_NAME).await.is_err()
            {
                eprintln!("skipping: could not own {BUS_NAME}");
                return;
            }

            let client = zbus::Connection::session().await.unwrap();

            // Introspect the member surface.
            let xml: String = client
                .call_method(
                    Some(BUS_NAME),
                    OBJECT_PATH,
                    Some("org.freedesktop.DBus.Introspectable"),
                    "Introspect",
                    &(),
                )
                .await
                .unwrap()
                .body()
                .deserialize()
                .unwrap();
            for member in [
                "one.playtron.GameMode",
                "EnterGameMode",
                "ExitGameMode",
                "SetFpsLimit",
                "SetScaling",
                "SetTearing",
                "SetVrr",
                "SetOverlay",
                "Active",
                "ActiveAppId",
                "FocusedAppId",
                "FpsLimit",
                "Tearing",
                "Vrr",
                "Scaling",
                "VrrSupported",
                "TearingSupported",
                "RefreshRates",
                "StateChanged",
                "FocusChanged",
                "CapabilitiesChanged",
            ] {
                assert!(xml.contains(member), "interface missing `{member}`:\n{xml}");
            }

            // Read a property.
            let reply = client
                .call_method(
                    Some(BUS_NAME),
                    OBJECT_PATH,
                    Some("org.freedesktop.DBus.Properties"),
                    "Get",
                    &("one.playtron.GameMode", "ActiveAppId"),
                )
                .await
                .unwrap();
            let value: zbus::zvariant::OwnedValue = reply.body().deserialize().unwrap();
            assert_eq!(u32::try_from(value).unwrap(), 1234);

            // Drive a control method.
            client
                .call_method(
                    Some(BUS_NAME),
                    OBJECT_PATH,
                    Some("one.playtron.GameMode"),
                    "SetFpsLimit",
                    &60u32,
                )
                .await
                .expect("SetFpsLimit over the bus");
        });
    }
}
