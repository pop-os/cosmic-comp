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
use tracing::{debug, info, warn};
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
    Enter(u32),
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

    /// `LauncherKeyPressed` — the launcher key (bare Super) was pressed while
    /// game mode is active. Pure event, no state to mirror.
    pub fn notify_launcher_key(&self) {
        let Some(conn) = self.conn.get().cloned() else {
            return;
        };
        self.spawn(async move {
            if let Ok(emitter) = SignalEmitter::new(&conn, OBJECT_PATH) {
                let _ = GameModeInterface::launcher_key_pressed(&emitter).await;
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
    async fn enter_game_mode(&self, app_id: u32) {
        self.io.send(GameModeCommand::Enter(app_id));
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

    /// The launcher key (bare Super) was pressed while game mode is active. The
    /// launcher (matching `LAUNCHER_APP_IDS`) subscribes and decides what to do
    /// (toggle its overlay, minimize the game, ...). A pure event, no state.
    #[zbus(signal)]
    async fn launcher_key_pressed(emitter: &SignalEmitter<'_>) -> zbus::Result<()>;
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
            GameModeCommand::Enter(app_id) => {
                debug!(app_id, "game-mode: enter");
                self.enter_game_mode(app_id);
                self.refresh_game_mode_state();
            }
            GameModeCommand::Exit => {
                debug!("game-mode: exit");
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
                debug!(visible, blocking, "game-mode: set overlay");
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
                // A game died — return to the launcher (still fullscreen on its own
                // workspace, so it comes back full-size). The pending guard avoids
                // re-firing every tick if the launcher is also gone (deferred);
                // try_resolve_pending_game_mode then quietly retries.
                info!(target: GAMING_TARGET, "game window gone; returning to the launcher");
                self.enter_game_mode(LAUNCHER_APP_ID);
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

        // In game mode the game must FILL the output gamescope-style: a legacy or
        // fixed-res game that commits a smaller buffer is upscaled to fit rather
        // than shown small in a black output. Mark the game's fullscreen surface
        // to render output-filling; the flag lives on the FullscreenSurface, so it
        // clears automatically when the game leaves fullscreen.
        {
            let game = {
                let shell = self.common.shell.read();
                shell
                    .game_mode
                    .active
                    .then(|| shell.game_mode.game_surface.clone())
                    .flatten()
            };
            if let Some(game) = game {
                let mut shell = self.common.shell.write();
                if let Some(ws) = shell
                    .workspaces
                    .spaces_mut()
                    .find(|ws| ws.get_fullscreen_surfaces().any(|f| f.surface == game))
                {
                    ws.set_fullscreen_fill_output(&game, true);
                }
            }
        }

        // Safety-net retry for a deferred enter (e.g. the game window mapped
        // since the last tick) and for a game surface retagged onto another window.
        self.try_resolve_pending_game_mode();
        self.refresh_active_game_surface();

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

        // Already active on this app id — clear any pending marker and return.
        {
            let shell = self.common.shell.read();
            if shell.game_mode.active && shell.game_mode.app_id == Some(app_id) {
                drop(shell);
                self.common.shell.write().game_mode.pending_app_id = None;
                return;
            }
        }

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

        let seat = self.common.shell.read().seats.last_active().clone();
        // Record the normal-desktop workspace only on the first entry into game
        // mode, so a full exit can return there; app switches keep that origin.
        let first_entry = !self.common.shell.read().game_mode.active;

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
                // surface, so this dissolves launcher<->game like gamescope, no
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

        // Un-fullscreen the game so its now-empty game-mode workspace auto-reaps.
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
            let _ = shell.activate(
                &output,
                idx,
                WorkspaceDelta::new_shortcut(),
                &mut self.common.workspace_state.update(),
            );
        }
        drop(shell);
        info!(target: GAMING_TARGET, "exited game mode");
    }

    /// Recompute whether a gaming overlay is up over the game — a real
    /// `STEAM_OVERLAY`/`GAMESCOPE_EXTERNAL_OVERLAY` window or a client
    /// `SetOverlay(true)` assertion. Updates the render-path flag
    /// (`game_mode.overlay_active`, which drops the tearing/scanout fast path so
    /// the overlay composites) and the D-Bus `OverlayVisible`/`FocusChanged`.
    pub fn refresh_overlay_visible(&mut self) {
        let bridge = self.common.game_mode_bridge.clone();
        let overlay_active = {
            let mut shell = self.common.shell.write();
            let asserted = shell.game_mode.overlay_asserted;
            let active =
                shell.game_mode.active && (overlay_window_present(&shell) || asserted);
            shell.game_mode.overlay_active = active;
            // Resolve the surface the OverlaySurface render path composites over
            // the game — ONLY for the D-Bus-asserted (Wayland launcher) overlay.
            // A real X11 STEAM_OVERLAY/GAMESCOPE_EXTERNAL_OVERLAY window composites
            // via its own override-redirect render path, so don't stack a launcher
            // for it (that would wrongly show Grid over the game on Shift+Tab).
            let surface = (active && asserted)
                .then(|| {
                    shell
                        .workspaces
                        .spaces()
                        .flat_map(|ws| ws.mapped())
                        .flat_map(|m| m.windows().map(|(s, _)| s))
                        .find(|w| {
                            w.is_overlay()
                                || LAUNCHER_APP_IDS.contains(&w.app_id().to_lowercase().as_str())
                        })
                })
                .flatten();
            shell.game_mode.overlay_surface = surface;
            active
        };
        let changed = {
            let mut s = bridge.shared().lock().unwrap();
            let changed = s.overlay_visible != overlay_active;
            s.overlay_visible = overlay_active;
            changed
        };
        if changed {
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
fn app_id_of(surface: &CosmicSurface) -> u32 {
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
    shell.workspaces.spaces().find_map(|ws| {
        // A window already fullscreen on a workspace is already in game mode there
        // — return it so we just switch to that workspace instead of re-entering.
        if let Some(fs) = ws
            .get_fullscreen_surfaces()
            .find(|f| app_id_of(&f.surface) == app_id)
        {
            return Some((fs.surface.clone(), ws.handle, ws.output().clone(), true));
        }
        // Otherwise a normal (desktop) window carrying the app id.
        let game = ws
            .mapped()
            .map(|mapped| mapped.active_window())
            .find(|surface| app_id_of(surface) == app_id)?;
        Some((game, ws.handle, ws.output().clone(), false))
    })
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
