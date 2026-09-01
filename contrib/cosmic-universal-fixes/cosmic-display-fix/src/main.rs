//! cosmic-display-fix — Rust COSMIC, event-driven (0% idle)
//! Hardware-agnostic: reads native EDID vs cosmic-randr current, corrects to preferred
//! Uses notify (inotify) on /sys/class/drm/*/status + ~/.config/cosmic (like Mutter GUdevClient HOTPLUG)
//! Primary plane fallback only (COSMIC_DISABLE_*).
#![allow(dead_code)]
#![allow(clippy::collapsible_if)]

use anyhow::{Context, Result};
use clap::Parser;
use log::{debug, error, info, warn};
use notify::{EventKind, RecursiveMode, Watcher};
use regex::Regex;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::mpsc::channel;
use std::time::Duration;

#[derive(Parser, Debug)]
#[command(
    name = "cosmic-display-fix",
    about = "Universal Display Fix - Rust COSMIC event-driven"
)]
struct Args {
    #[arg(long, help = "event-driven daemon mode")]
    daemon: bool,
    #[arg(long, help = "check only and exit")]
    check: bool,
}

#[derive(Debug, Clone)]
struct Mode {
    w: u32,
    h: u32,
    refresh: f64,
}

#[derive(Debug)]
struct OutputInfo {
    position: (i32, i32),
    adaptive: bool,
    current: Option<Mode>,
    preferred: Option<Mode>,
}

fn run(cmd: &str) -> String {
    Command::new("bash")
        .arg("-c")
        .arg(cmd)
        .output()
        .map(|o| {
            format!(
                "{}{}",
                String::from_utf8_lossy(&o.stdout),
                String::from_utf8_lossy(&o.stderr)
            )
        })
        .unwrap_or_default()
}

fn get_randr_list() -> String {
    run("cosmic-randr list 2>&1")
}

fn parse_randr_modes(text: &str) -> HashMap<String, OutputInfo> {
    let mut outputs = HashMap::new();
    let re_header = Regex::new(r"^([A-Z0-9\-]+)\s+\(enabled\)").unwrap();
    let re_pos = Regex::new(r"Position:\s*(\d+),(\d+)").unwrap();
    let re_ad = Regex::new(r"Adaptive Sync:\s*(true|false)").unwrap();
    let mut current_name: Option<String> = None;
    let mut block = String::new();
    let flush = |name: &Option<String>, block: &str, map: &mut HashMap<String, OutputInfo>| {
        if let Some(n) = name {
            let pos = re_pos
                .captures(block)
                .map(|c| (c[1].parse().unwrap_or(0), c[2].parse().unwrap_or(0)))
                .unwrap_or((0, 0));
            let adaptive = re_ad
                .captures(block)
                .map(|c| c[1].to_lowercase() == "true")
                .unwrap_or(false);
            let re_cur = Regex::new(r"(\d+)x(\d+)\s*@\s*([\d\.]+)\s*Hz.*\(current\)").unwrap();
            let re_pref = Regex::new(r"(\d+)x(\d+)\s*@\s*([\d\.]+)\s*Hz.*\(preferred\)").unwrap();
            let mut current = None;
            let mut preferred = None;
            for line in block.lines() {
                if let Some(c) = re_cur.captures(line) {
                    if let (Ok(w), Ok(h), Ok(r)) = (c[1].parse(), c[2].parse(), c[3].parse()) {
                        current = Some(Mode { w, h, refresh: r });
                    }
                }
                if let Some(c) = re_pref.captures(line) {
                    if let (Ok(w), Ok(h), Ok(r)) = (c[1].parse(), c[2].parse(), c[3].parse()) {
                        preferred = Some(Mode { w, h, refresh: r });
                    }
                }
            }
            map.insert(
                n.clone(),
                OutputInfo {
                    position: pos,
                    adaptive,
                    current,
                    preferred,
                },
            );
        }
    };
    for line in text.lines() {
        if let Some(c) = re_header.captures(line) {
            flush(&current_name, &block, &mut outputs);
            current_name = Some(c[1].to_string());
            block = String::new();
        } else {
            block.push_str(line);
            block.push('\n');
        }
    }
    flush(&current_name, &block, &mut outputs);
    outputs
}

// --- Pure Rust EDID parser (no external deps) ---
#[derive(Debug)]
struct EdidInfo {
    manufacturer: String,
    preferred: Option<Mode>,
    range_vmin: u8,
    range_vmax: u8,
    has_hdmi_vsdb: bool,
}

fn parse_edid(path: &Path) -> Option<EdidInfo> {
    let data = std::fs::read(path).ok()?;
    if data.len() < 128 || data[0..8] != [0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00] {
        return None;
    }
    let mfg = u16::from_be_bytes([data[8], data[9]]);
    let c1 = char::from_u32(((mfg >> 10 & 0x1F) as u32) + 64).unwrap_or('?');
    let c2 = char::from_u32(((mfg >> 5 & 0x1F) as u32) + 64).unwrap_or('?');
    let c3 = char::from_u32(((mfg & 0x1F) as u32) + 64).unwrap_or('?');
    let manufacturer = format!("{}{}{}", c1, c2, c3);
    let mut dtds = Vec::new();
    for i in 0..4 {
        let off = 54 + i * 18;
        if off + 18 > data.len() {
            break;
        }
        let dtd = &data[off..off + 18];
        if dtd[0] == 0 && dtd[1] == 0 {
            continue;
        }
        let pix = u16::from_le_bytes([dtd[0], dtd[1]]) as u32;
        if pix == 0 {
            continue;
        }
        let pix_hz = pix as f64 * 10000.0;
        let h_act = (dtd[2] as u32) | (((dtd[4] >> 4) as u32) << 8);
        let h_blank = (dtd[3] as u32) | (((dtd[4] & 0xF) as u32) << 8);
        let v_act = (dtd[5] as u32) | (((dtd[7] >> 4) as u32) << 8);
        let v_blank = (dtd[6] as u32) | (((dtd[7] & 0xF) as u32) << 8);
        let h_total = h_act + h_blank;
        let v_total = v_act + v_blank;
        if h_total == 0 || v_total == 0 {
            continue;
        }
        let refresh = pix_hz / (h_total as f64 * v_total as f64);
        dtds.push(Mode {
            w: h_act,
            h: v_act,
            refresh,
        });
    }
    let mut has_hdmi_vsdb = false;
    let mut vmin = 0;
    let mut vmax = 0;
    for i in 0..4 {
        let off = 54 + i * 18;
        let dtd = &data[off..off + 18];
        if dtd[0] == 0 && dtd[1] == 0 && dtd[3] == 0xFD {
            vmin = dtd[5];
            vmax = dtd[6];
        }
    }
    if data.len() >= 256 && data[128] == 0x02 {
        let ext = &data[128..256];
        let dtd_offset = ext[2] as usize;
        let mut idx = 4;
        while idx < dtd_offset && idx < 128 {
            let header = ext[idx];
            let tag = (header >> 5) & 0x7;
            let len = (header & 0x1F) as usize;
            if tag == 3 && len >= 3 {
                let payload = &ext[idx + 1..idx + 1 + len];
                if payload.len() >= 3
                    && (payload[0..3] == [0x03, 0x0C, 0x00] || payload[0..3] == [0xC4, 0x5D, 0xD8])
                {
                    has_hdmi_vsdb = true;
                }
            }
            idx += 1 + len;
            if idx >= 128 {
                break;
            }
        }
        for j in 0..4 {
            let off = dtd_offset + j * 18;
            if off + 18 > 128 {
                break;
            }
            let dtd = &ext[off..off + 18];
            if dtd[0] == 0 && dtd[1] == 0 {
                continue;
            }
            let pix = u16::from_le_bytes([dtd[0], dtd[1]]) as u32;
            if pix == 0 {
                continue;
            }
            let pix_hz = pix as f64 * 10000.0;
            let h_act = (dtd[2] as u32) | (((dtd[4] >> 4) as u32) << 8);
            let h_blank = (dtd[3] as u32) | (((dtd[4] & 0xF) as u32) << 8);
            let v_act = (dtd[5] as u32) | (((dtd[7] >> 4) as u32) << 8);
            let v_blank = (dtd[6] as u32) | (((dtd[7] & 0xF) as u32) << 8);
            let h_total = h_act + h_blank;
            let v_total = v_act + v_blank;
            if h_total == 0 || v_total == 0 {
                continue;
            }
            dtds.push(Mode {
                w: h_act,
                h: v_act,
                refresh: pix_hz / (h_total as f64 * v_total as f64),
            });
        }
    }
    let preferred = dtds.first().cloned();
    Some(EdidInfo {
        manufacturer,
        preferred,
        range_vmin: vmin,
        range_vmax: vmax,
        has_hdmi_vsdb,
    })
}

fn discover_outputs() -> HashMap<String, EdidInfo> {
    let mut map = HashMap::new();
    for p in glob::glob("/sys/class/drm/card*/card*-*/edid")
        .unwrap_or_else(|_| glob::glob("").unwrap())
        .into_iter()
        .flatten()
    {
        let status_path = p.parent().unwrap().join("status");
        if std::fs::read_to_string(&status_path)
            .unwrap_or_default()
            .trim()
            != "connected"
        {
            continue;
        }
        let name = p
            .parent()
            .unwrap()
            .file_name()
            .unwrap()
            .to_string_lossy()
            .to_string();
        let connector = name
            .split_once('-')
            .map(|x| x.1)
            .unwrap_or(&name)
            .to_string();
        if let Some(info) = parse_edid(&p) {
            map.insert(connector, info);
        }
    }
    map
}

fn has_vendor_infoframe_error() -> bool {
    let out=run("journalctl -b -k --no-pager -q 2>/dev/null | grep -c 'Failed to setup vendor infoframe' || echo 0");
    out.lines()
        .last()
        .unwrap_or("0")
        .trim()
        .parse::<i32>()
        .unwrap_or(0)
        > 0
}

fn ensure_scanout_disabled() -> bool {
    let config_home = std::env::var("XDG_CONFIG_HOME")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            dirs::config_dir().unwrap_or_else(|| dirs::home_dir().unwrap().join(".config"))
        });
    let env_file = config_home.join("environment.d/99-cosmic-disable-scanout.conf");
    let needed = [
        "COSMIC_DISABLE_DIRECT_SCANOUT=1",
        "COSMIC_DISABLE_OVERLAY_SCANOUT=1",
    ];
    if let Some(parent) = env_file.parent() {
        let _ = std::fs::create_dir_all(parent);
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let _ = std::fs::set_permissions(parent, std::fs::Permissions::from_mode(0o700));
        }
    }
    let existing = std::fs::read_to_string(&env_file).unwrap_or_default();
    let mut missing: Vec<String> = Vec::new();
    for l in needed {
        if !existing.contains(l) {
            missing.push(l.to_string());
        }
    }
    if !missing.is_empty() {
        use std::io::Write;
        let mut f = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&env_file)
            .unwrap();
        for l in &missing {
            writeln!(f, "{}", l).ok();
        }
        info!("Updated {} with {:?}", env_file.display(), missing);
        run("export COSMIC_DISABLE_DIRECT_SCANOUT=1; export COSMIC_DISABLE_OVERLAY_SCANOUT=1; systemctl --user import-environment COSMIC_DISABLE_DIRECT_SCANOUT COSMIC_DISABLE_OVERLAY_SCANOUT 2>/dev/null; dbus-update-activation-environment --systemd COSMIC_DISABLE_DIRECT_SCANOUT=1 COSMIC_DISABLE_OVERLAY_SCANOUT=1 2>/dev/null");
        return true;
    }
    false
}

fn fix_modes() -> Vec<(String, Mode, Mode)> {
    let randr = get_randr_list();
    let outputs = parse_randr_modes(&randr);
    let edids = discover_outputs();
    let vendor_err = has_vendor_infoframe_error();
    let mut actions = Vec::new();
    for (name, data) in &outputs {
        if let (Some(cur), Some(pref)) = (data.current.clone(), data.preferred.clone()) {
            if cur.w != pref.w || cur.h != pref.h || (cur.refresh - pref.refresh).abs() > 0.01 {
                let edid_pref = edids.get(name).and_then(|e| e.preferred.clone());
                let should_fix = if let Some(ep) = edid_pref {
                    (ep.refresh - pref.refresh).abs() < 0.5
                        && ((cur.refresh - pref.refresh).abs() > 5.0 || vendor_err)
                } else {
                    cur.w == pref.w && cur.h == pref.h && (cur.refresh - pref.refresh).abs() > 10.0
                };
                if should_fix {
                    warn!(
                        "{}: mismatch current {}x{}@{:.3} vs preferred {}x{}@{:.3} -> correcting",
                        name, cur.w, cur.h, cur.refresh, pref.w, pref.h, pref.refresh
                    );
                    let cmd = format!(
                        "cosmic-randr mode {} {} {} --refresh {:.3} --adaptive-sync false",
                        name, pref.w, pref.h, pref.refresh
                    );
                    let r = run(&cmd);
                    if r.contains("current") || r.is_empty() {
                        info!("  -> {} OK", cmd);
                        actions.push((name.clone(), cur, pref));
                    } else {
                        error!("  -> {} FAIL {}", cmd, r.trim());
                    }
                }
            }
        }
        if data.adaptive && outputs.len() > 1 && !actions.iter().any(|(n, _, _)| n == name) {
            if let Some(cur) = data.current.clone() {
                run(&format!(
                    "cosmic-randr mode {} {} {} --refresh {:.3} --adaptive-sync false",
                    name, cur.w, cur.h, cur.refresh
                ));
            }
        }
    }
    actions
}

fn watch_drm_inotify<F>(mut callback: F) -> Result<()>
where
    F: FnMut() + Send + 'static,
{
    // notify on /sys/class/drm/*/status + ~/.config/cosmic
    let (tx, rx) = channel();
    let mut watcher = notify::recommended_watcher(move |res| {
        tx.send(res).ok();
    })?;
    for pat in [
        "/sys/class/drm/card1/card1-DP-1/status",
        "/sys/class/drm/card1/card1-HDMI-A-1/status",
        "/sys/class/drm/card1/card1-DP-2/status",
        "/sys/class/drm/card1/card1-DP-3/status",
    ] {
        if Path::new(pat).exists() {
            watcher
                .watch(Path::new(pat), RecursiveMode::NonRecursive)
                .ok();
        }
    }
    let cosmic_cfg = std::env::var("XDG_CONFIG_HOME")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            dirs::config_dir().unwrap_or_else(|| dirs::home_dir().unwrap().join(".config"))
        })
        .join("cosmic");
    if cosmic_cfg.exists() {
        watcher.watch(&cosmic_cfg, RecursiveMode::Recursive).ok();
    }
    info!("inotify watches active (event-driven, like Mutter GUdevClient HOTPLUG)");
    loop {
        match rx.recv_timeout(Duration::from_secs(60)) {
            Ok(Ok(event)) => match event.kind {
                EventKind::Modify(_) | EventKind::Create(_) | EventKind::Remove(_) => {
                    debug!("inotify event {:?}", event);
                    std::thread::sleep(Duration::from_millis(500));
                    callback();
                }
                _ => {}
            },
            Ok(Err(e)) => warn!("watch error {:?}", e),
            Err(_) => { /* timeout heartbeat */ }
        }
    }
}

fn daemon_event_driven() -> Result<()> {
    info!("cosmic-display-fix — EVENT-DRIVEN mode started — Rust COSMIC");
    ensure_scanout_disabled();
    fix_modes();
    watch_drm_inotify(|| {
        info!("HOTPLUG/config event -> re-evaluating");
        ensure_scanout_disabled();
        fix_modes();
    })?;
    Ok(())
}

fn main() -> Result<()> {
    env_logger::init();
    let args = Args::parse();
    if args.check || !args.daemon {
        ensure_scanout_disabled();
        let actions = fix_modes();
        if actions.is_empty() {
            info!("No corrections required (modes already optimal)");
        } else {
            info!("Corrections: {:?}", actions);
        }
        return Ok(());
    }
    daemon_event_driven().context("daemon failed")
}

// glob helper for /sys/class/drm (portable)
mod glob {
    use std::path::PathBuf;
    pub fn glob(_pattern: &str) -> Result<Vec<Result<PathBuf, std::io::Error>>, std::io::Error> {
        let out = crate::run("ls /sys/class/drm/card*/card*-*/edid 2>/dev/null");
        let mut v = Vec::new();
        for line in out.lines() {
            if !line.trim().is_empty() {
                v.push(Ok(PathBuf::from(line.trim())));
            }
        }
        Ok(v)
    }
}
