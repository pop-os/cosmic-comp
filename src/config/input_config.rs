use smithay::reexports::input::{
    Device as InputDevice, DeviceConfigError, ScrollMethod, SendEventsMode,
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
                scroll_factor: None,
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
        map_to_output: None,
    }
}

// Get setting from `device_config` if present, then `default_config`
// Returns `is_default` to indicate this is a default value.
pub fn get_config<'a, T: 'a, F: Fn(&'a InputConfig) -> Option<T>>(
    device_config: Option<&'a InputConfig>,
    default_config: &'a InputConfig,
    f: F,
) -> Option<(T, bool)> {
    if let Some(setting) = device_config.and_then(&f) {
        Some((setting, false))
    } else {
        f(default_config).map(|setting| (setting, true))
    }
}

fn config_set_error<T: std::fmt::Debug>(
    device: &InputDevice,
    setting: &str,
    value: T,
    err: DeviceConfigError,
    is_default: bool,
) {
    if !(is_default && err == DeviceConfigError::Unsupported) {
        warn!(
            ?err,
            "Failed to apply {} {:?} for device {:?}.",
            setting,
            value,
            device.name(),
        );
    }
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
    if let Some((accel, is_default)) = config!(|x| x.acceleration.as_ref()) {
        if let Some(profile) = accel.profile {
            if let Err(err) = device.config_accel_set_profile(profile) {
                config_set_error(device, "acceleration profile", profile, err, is_default);
            }
        }
        if let Err(err) = device.config_accel_set_speed(accel.speed) {
            config_set_error(device, "acceleration speed", accel.speed, err, is_default);
        }
    }
    if let Some((matrix, is_default)) = config!(|x| x.calibration) {
        if let Err(err) = device.config_calibration_set_matrix(matrix) {
            config_set_error(device, "calibration matrix", matrix, err, is_default);
        }
    }
    if let Some((method, is_default)) = config!(|x| x.click_method) {
        if let Err(err) = device.config_click_set_method(method) {
            config_set_error(device, "click method", method, err, is_default);
        }
    }
    if let Some((dwt, is_default)) = config!(|x| x.disable_while_typing) {
        if let Err(err) = device.config_dwt_set_enabled(dwt) {
            config_set_error(device, "disable-while-typing", dwt, err, is_default);
        }
    }
    if let Some((left, is_default)) = config!(|x| x.left_handed) {
        if let Err(err) = device.config_left_handed_set(left) {
            config_set_error(device, "left-handed", left, err, is_default);
        }
    }
    if let Some((middle, is_default)) = config!(|x| x.middle_button_emulation) {
        if let Err(err) = device.config_middle_emulation_set_enabled(middle) {
            config_set_error(device, "middle-button-emulation", middle, err, is_default);
        }
    }
    if let Some((angle, is_default)) = config!(|x| x.rotation_angle) {
        if let Err(err) = device.config_rotation_set_angle(angle) {
            config_set_error(device, "rotation-angle", angle, err, is_default);
        }
    }
    if let Some((scroll, is_default)) = config!(|x| x.scroll_config.as_ref()) {
        if let Some(method) = scroll.method {
            if let Err(err) = device.config_scroll_set_method(method) {
                config_set_error(device, "scroll method", scroll, err, is_default);
            }
        }
        if let Some(natural) = scroll.natural_scroll {
            if let Err(err) = device.config_scroll_set_natural_scroll_enabled(natural) {
                config_set_error(device, "natural scrolling", natural, err, is_default);
            }
        }
        if let Some(button) = scroll.scroll_button {
            if let Err(err) = device.config_scroll_set_button(button) {
                config_set_error(device, "scroll button", button, err, is_default);
            }
        }
    }
    if let Some((tap, is_default)) = config!(|x| x.tap_config.as_ref()) {
        if let Err(err) = device.config_tap_set_enabled(tap.enabled) {
            config_set_error(device, "tap-to-click", tap.enabled, err, is_default);
        }
        if let Some(button_map) = tap.button_map {
            if let Err(err) = device.config_tap_set_button_map(button_map) {
                config_set_error(device, "button map", button_map, err, is_default);
            }
        }
        if let Err(err) = device.config_tap_set_drag_enabled(tap.drag) {
            config_set_error(device, "tap-drag", tap.drag, err, is_default);
        }
        if let Err(err) = device.config_tap_set_drag_lock_enabled(tap.drag_lock) {
            config_set_error(device, "tap-drag-lock", tap.drag_lock, err, is_default);
        }
    }
}
