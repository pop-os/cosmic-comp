pub use smithay::{
    backend::input::KeyState,
    input::keyboard::{keysyms as KeySyms, Keysym, ModifiersState},
    output::{Mode, Output},
    reexports::{
        calloop::LoopHandle,
        input::{
            AccelProfile, ClickMethod, Device as InputDevice, ScrollMethod, SendEventsMode,
            TapButtonMap,
        },
    },
    utils::{Logical, Physical, Point, Size, Transform},
};
use tracing::warn;

use cosmic_comp_config::input::*;

#[allow(dead_code)]
pub fn for_device(device: &InputDevice) -> InputConfig {
    InputConfig {
        state: match device.config_send_events_mode() {
            x if x.contains(SendEventsMode::ENABLED) => DeviceState::Enabled,
            x if x.contains(SendEventsMode::DISABLED_ON_EXTERNAL_MOUSE) => {
                DeviceState::DisabledOnExternalMouse
            }
            x if x.contains(SendEventsMode::DISABLED) => DeviceState::Disabled,
            _ => DeviceState::Disabled,
        },
        acceleration: if device.config_accel_is_available() {
            Some(AccelConfig {
                profile: device.config_accel_profile(),
                speed: device.config_accel_speed(),
            })
        } else {
            None
        },
        calibration: device.config_calibration_matrix(),
        click_method: device.config_click_method(),
        disable_while_typing: if device.config_dwt_is_available() {
            Some(device.config_dwt_enabled())
        } else {
            None
        },
        left_handed: if device.config_left_handed_is_available() {
            Some(device.config_left_handed())
        } else {
            None
        },
        middle_button_emulation: if device.config_middle_emulation_is_available() {
            Some(device.config_middle_emulation_enabled())
        } else {
            None
        },
        rotation_angle: if device.config_rotation_is_available() {
            Some(device.config_rotation_angle())
        } else {
            None
        },
        scroll_config: if device
            .config_scroll_methods()
            .iter()
            .any(|x| *x != ScrollMethod::NoScroll)
        {
            Some(ScrollConfig {
                method: device.config_scroll_method(),
                natural_scroll: if device.config_scroll_has_natural_scroll() {
                    Some(device.config_scroll_natural_scroll_enabled())
                } else {
                    None
                },
                scroll_button: if device.config_scroll_method() == Some(ScrollMethod::OnButtonDown)
                {
                    Some(device.config_scroll_button())
                } else {
                    None
                },
            })
        } else {
            None
        },
        tap_config: if device.config_tap_finger_count() > 0 {
            Some(TapConfig {
                enabled: device.config_tap_enabled(),
                button_map: device.config_tap_button_map(),
                drag: device.config_tap_drag_enabled(),
                drag_lock: device.config_tap_drag_lock_enabled(),
            })
        } else {
            None
        },
    }
}

fn get_config<'a, T: 'a, F: Fn(&'a InputConfig) -> Option<T>>(
    device_config: Option<&'a InputConfig>,
    default_config: &'a InputConfig,
    f: F,
) -> Option<T> {
    if let Some(device_config) = device_config {
        if let Some(setting) = f(device_config) {
            return Some(setting);
        }
    }
    f(default_config)
}

pub fn update_device(
    device: &mut InputDevice,
    device_config: Option<&InputConfig>,
    default_config: &InputConfig,
) {
    macro_rules! config {
        ($f:expr) => {
            get_config(device_config, default_config, $f)
        };
    }

    let state = device_config.unwrap_or(default_config).state;
    if let Err(err) = match state {
        DeviceState::Enabled => device.config_send_events_set_mode(SendEventsMode::ENABLED),
        DeviceState::Disabled => device.config_send_events_set_mode(SendEventsMode::DISABLED),
        DeviceState::DisabledOnExternalMouse => {
            device.config_send_events_set_mode(SendEventsMode::DISABLED_ON_EXTERNAL_MOUSE)
        }
    } {
        warn!(
            ?err,
            "Failed to apply mode {:?} for device {:?}.",
            state,
            device.name(),
        );
    }
    if let Some(accel) = config!(|x| x.acceleration.as_ref()) {
        if let Some(profile) = accel.profile {
            if let Err(err) = device.config_accel_set_profile(profile) {
                warn!(
                    ?err,
                    "Failed to apply acceleration profile {:?} for device {:?}.",
                    profile,
                    device.name(),
                );
            }
        }
        if let Err(err) = device.config_accel_set_speed(accel.speed) {
            warn!(
                ?err,
                "Failed to apply acceleration speed {:?} for device {:?}.",
                accel.speed,
                device.name(),
            );
        }
    }
    if let Some(matrix) = config!(|x| x.calibration) {
        if let Err(err) = device.config_calibration_set_matrix(matrix) {
            warn!(
                ?err,
                "Failed to apply calibration matrix {:?} for device {:?}.",
                matrix,
                device.name(),
            );
        }
    }
    if let Some(method) = config!(|x| x.click_method) {
        if let Err(err) = device.config_click_set_method(method) {
            warn!(
                ?err,
                "Failed to apply click method {:?} for device {:?}.",
                method,
                device.name(),
            );
        }
    }
    if let Some(dwt) = config!(|x| x.disable_while_typing) {
        if let Err(err) = device.config_dwt_set_enabled(dwt) {
            warn!(
                ?err,
                "Failed to apply disable-while-typing {:?} for device {:?}.",
                dwt,
                device.name(),
            );
        }
    }
    if let Some(left) = config!(|x| x.left_handed) {
        if let Err(err) = device.config_left_handed_set(left) {
            warn!(
                ?err,
                "Failed to apply left-handed {:?} for device {:?}.",
                left,
                device.name(),
            );
        }
    }
    if let Some(middle) = config!(|x| x.middle_button_emulation) {
        if let Err(err) = device.config_middle_emulation_set_enabled(middle) {
            warn!(
                ?err,
                "Failed to apply middle-button-emulation {:?} for device {:?}.",
                middle,
                device.name(),
            );
        }
    }
    if let Some(angle) = config!(|x| x.rotation_angle) {
        if let Err(err) = device.config_rotation_set_angle(angle) {
            warn!(
                ?err,
                "Failed to apply rotation-angle {:?} for device {:?}",
                angle,
                device.name(),
            );
        }
    }
    if let Some(scroll) = config!(|x| x.scroll_config.as_ref()) {
        if let Some(method) = scroll.method {
            if let Err(err) = device.config_scroll_set_method(method) {
                warn!(
                    ?err,
                    "Failed to apply scroll method {:?} for device {:?}.",
                    method,
                    device.name(),
                );
            }
        }
        if let Some(natural) = scroll.natural_scroll {
            if let Err(err) = device.config_scroll_set_natural_scroll_enabled(natural) {
                warn!(
                    ?err,
                    "Failed to apply natural scrolling {:?} for device {:?}.",
                    natural,
                    device.name(),
                );
            }
        }
        if let Some(button) = scroll.scroll_button {
            if let Err(err) = device.config_scroll_set_button(button) {
                warn!(
                    ?err,
                    "Failed to apply scroll button {:?} for device {:?}.",
                    button,
                    device.name(),
                );
            }
        }
    }
    if let Some(tap) = config!(|x| x.tap_config.as_ref()) {
        if let Err(err) = device.config_tap_set_enabled(tap.enabled) {
            warn!(
                ?err,
                "Failed to apply tap-to-click {:?} for device {:?}.",
                tap.enabled,
                device.name(),
            );
        }
        if let Some(button_map) = tap.button_map {
            if let Err(err) = device.config_tap_set_button_map(button_map) {
                warn!(
                    ?err,
                    "Failed to apply button map {:?} for device {:?}.",
                    button_map,
                    device.name(),
                );
            }
        }
        if let Err(err) = device.config_tap_set_drag_enabled(tap.drag) {
            warn!(
                ?err,
                "Failed to apply tap-drag {:?} for device {:?}.",
                tap.drag,
                device.name(),
            );
        }
        if let Err(err) = device.config_tap_set_drag_lock_enabled(tap.drag_lock) {
            warn!(
                ?err,
                "Failed to apply tap-drag-lock {:?} for device {:?}.",
                tap.drag_lock,
                device.name(),
            );
        }
    }
}
