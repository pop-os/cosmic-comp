// SPDX-License-Identifier: GPL-3.0-only

//! Voice mode configuration
//!
//! This module provides shared configuration for voice mode keybindings
//! that can be used by both cosmic-comp.

use cosmic_config::{Config, ConfigGet, ConfigSet, CosmicConfigEntry};
use serde::{Deserialize, Serialize};
use smithay::input::keyboard::{Keysym, ModifiersState};

/// Configuration ID for voice mode settings
pub const VOICE_CONFIG_ID: &str = "com.playtron.VoiceMode";
pub const VOICE_CONFIG_VERSION: u64 = 1;

/// Voice mode key binding configuration
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VoiceKeyBinding {
    /// Key symbol name (e.g., "F6", "F23")
    pub key: String,
    /// Required modifiers
    pub modifiers: VoiceModifiers,
}

/// Modifier keys for voice bindings
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct VoiceModifiers {
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
    pub logo: bool, // Super/Meta key
}

impl VoiceModifiers {
    pub fn none() -> Self {
        Self::default()
    }

    pub fn shift_logo() -> Self {
        Self {
            shift: true,
            logo: true,
            ..Default::default()
        }
    }

    /// Check if the given smithay modifiers match this configuration
    pub fn matches(&self, mods: &ModifiersState) -> bool {
        // If no modifiers required, always match
        if !self.ctrl && !self.alt && !self.shift && !self.logo {
            return true;
        }

        // Otherwise check that required modifiers are pressed
        (!self.ctrl || mods.ctrl)
            && (!self.alt || mods.alt)
            && (!self.shift || mods.shift)
            && (!self.logo || mods.logo)
    }
}

/// Voice mode configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VoiceConfig {
    /// Primary voice key binding (e.g., Shift+Super+F23 for dedicated button)
    pub primary_binding: VoiceKeyBinding,
    /// Fallback voice key binding (e.g., F6 for development/testing)
    pub fallback_binding: Option<VoiceKeyBinding>,
    /// App ID for the chat window to attach orb to
    pub chat_app_id: String,
    /// Whether voice mode is enabled
    pub enabled: bool,
}

impl Default for VoiceConfig {
    fn default() -> Self {
        Self {
            primary_binding: VoiceKeyBinding {
                key: "Super_L".to_string(),
                modifiers: VoiceModifiers::none(),
            },
            fallback_binding: None,
            chat_app_id: "chat-ui".to_string(),
            enabled: true,
        }
    }
}

impl CosmicConfigEntry for VoiceConfig {
    const VERSION: u64 = VOICE_CONFIG_VERSION;

    fn write_entry(&self, config: &Config) -> Result<(), cosmic_config::Error> {
        config.set("primary_binding", &self.primary_binding)?;
        config.set("fallback_binding", &self.fallback_binding)?;
        config.set("chat_app_id", &self.chat_app_id)?;
        config.set("enabled", self.enabled)?;
        Ok(())
    }

    fn get_entry(config: &Config) -> Result<Self, (Vec<cosmic_config::Error>, Self)> {
        let mut errors = Vec::new();
        let default = Self::default();

        let primary_binding = config.get("primary_binding").unwrap_or_else(|e| {
            errors.push(e);
            default.primary_binding.clone()
        });

        let fallback_binding = config.get("fallback_binding").unwrap_or_else(|e| {
            errors.push(e);
            default.fallback_binding.clone()
        });

        let chat_app_id = config.get("chat_app_id").unwrap_or_else(|e| {
            errors.push(e);
            default.chat_app_id.clone()
        });

        let enabled = config.get("enabled").unwrap_or_else(|e| {
            errors.push(e);
            default.enabled
        });

        let config = Self {
            primary_binding,
            fallback_binding,
            chat_app_id,
            enabled,
        };

        if errors.is_empty() {
            Ok(config)
        } else {
            Err((errors, config))
        }
    }

    fn update_keys<T: AsRef<str>>(
        &mut self,
        config: &Config,
        keys: &[T],
    ) -> (Vec<cosmic_config::Error>, Vec<&'static str>) {
        let mut errors = Vec::new();
        let mut updated = Vec::new();

        for key in keys {
            match key.as_ref() {
                "primary_binding" => match config.get::<VoiceKeyBinding>("primary_binding") {
                    Ok(v) => {
                        self.primary_binding = v;
                        updated.push("primary_binding");
                    }
                    Err(e) => errors.push(e),
                },
                "fallback_binding" => {
                    match config.get::<Option<VoiceKeyBinding>>("fallback_binding") {
                        Ok(v) => {
                            self.fallback_binding = v;
                            updated.push("fallback_binding");
                        }
                        Err(e) => errors.push(e),
                    }
                }
                "chat_app_id" => match config.get::<String>("chat_app_id") {
                    Ok(v) => {
                        self.chat_app_id = v;
                        updated.push("chat_app_id");
                    }
                    Err(e) => errors.push(e),
                },
                "enabled" => match config.get::<bool>("enabled") {
                    Ok(v) => {
                        self.enabled = v;
                        updated.push("enabled");
                    }
                    Err(e) => errors.push(e),
                },
                _ => {}
            }
        }

        (errors, updated)
    }
}

impl VoiceConfig {
    /// Load voice configuration from cosmic-config
    pub fn load() -> Self {
        let config = Config::new(VOICE_CONFIG_ID, VOICE_CONFIG_VERSION)
            .expect("Failed to create voice config");

        match Self::get_entry(&config) {
            Ok(c) => c,
            Err((errors, c)) => {
                for e in errors {
                    tracing::warn!("Voice config error: {}", e);
                }
                c
            }
        }
    }

    /// Create a cosmic-config context for watching changes
    pub fn context() -> Result<Config, cosmic_config::Error> {
        Config::new(VOICE_CONFIG_ID, VOICE_CONFIG_VERSION)
    }

    /// Check if a keysym and modifiers match any configured voice binding
    pub fn matches_binding(&self, keysym: Keysym, modifiers: &ModifiersState) -> bool {
        tracing::trace!(
            keysym_raw = keysym.raw(),
            keysym_name = ?keysym,
            ?modifiers,
            enabled = self.enabled,
            primary_key = %self.primary_binding.key,
            primary_mods = ?self.primary_binding.modifiers,
            fallback_key = ?self.fallback_binding.as_ref().map(|f| &f.key),
            fallback_mods = ?self.fallback_binding.as_ref().map(|f| &f.modifiers),
            "matches_binding called"
        );

        if !self.enabled {
            tracing::trace!("Voice config disabled, skipping");
            return false;
        }

        // Check primary binding
        let primary_match = self.key_matches(&self.primary_binding, keysym, modifiers);
        tracing::trace!(primary_match, "Primary binding check");
        if primary_match {
            return true;
        }

        // Check fallback binding
        if let Some(ref fallback) = self.fallback_binding {
            let fallback_match = self.key_matches(fallback, keysym, modifiers);
            tracing::trace!(fallback_match, fallback_key = %fallback.key, "Fallback binding check");
            if fallback_match {
                return true;
            }
        } else {
            tracing::trace!("No fallback binding configured");
        }

        false
    }

    fn key_matches(
        &self,
        binding: &VoiceKeyBinding,
        keysym: Keysym,
        modifiers: &ModifiersState,
    ) -> bool {
        let is_super_l = keysym == Keysym::Super_L;
        let is_super_r = keysym == Keysym::Super_R;
        tracing::trace!(
            binding_key = %binding.key,
            binding_mods = ?binding.modifiers,
            keysym_raw = keysym.raw(),
            keysym_is_f23 = (keysym == Keysym::F23),
            keysym_is_touchpad_off = (keysym.raw() == 0x1008ffb1),
            is_super_l,
            is_super_r,
            super_l_raw = Keysym::Super_L.raw(),
            super_r_raw = Keysym::Super_R.raw(),
            ?modifiers,
            "key_matches checking"
        );
        let key_matches = match binding.key.as_str() {
            "F1" => keysym == Keysym::F1,
            "F2" => keysym == Keysym::F2,
            "F3" => keysym == Keysym::F3,
            "F4" => keysym == Keysym::F4,
            "F5" => keysym == Keysym::F5,
            "F6" => keysym == Keysym::F6,
            "F7" => keysym == Keysym::F7,
            "F8" => keysym == Keysym::F8,
            "F9" => keysym == Keysym::F9,
            "F10" => keysym == Keysym::F10,
            "F11" => keysym == Keysym::F11,
            "F12" => keysym == Keysym::F12,
            "F13" => keysym == Keysym::F13,
            "F14" => keysym == Keysym::F14,
            "F15" => keysym == Keysym::F15,
            "F16" => keysym == Keysym::F16,
            "F17" => keysym == Keysym::F17,
            "F18" => keysym == Keysym::F18,
            "F19" => keysym == Keysym::F19,
            "F20" => keysym == Keysym::F20,
            "F21" => keysym == Keysym::F21,
            "F22" => keysym == Keysym::F22,
            "F23" => keysym == Keysym::F23,
            "F24" => keysym == Keysym::F24,
            // XF86 multimedia keys - Copilot key on some laptops reports as XF86TouchpadOff via xkbcommon
            "XF86TouchpadOff" => keysym.raw() == 0x1008ffb1, // XF86XK_TouchpadOff = 269025201
            // Super keys (for development/testing)
            "Super_L" => keysym == Keysym::Super_L || keysym == Keysym::Super_R,
            "Super_R" => keysym == Keysym::Super_L || keysym == Keysym::Super_R,
            _ => false,
        };

        let mods_match = binding.modifiers.matches(modifiers);
        tracing::trace!(key_matches, mods_match, "key_matches result");
        key_matches && mods_match
    }

    /// Check if a keysym matches any configured voice binding key (ignoring modifiers)
    /// This is used to detect key release when modifiers may have already been released
    pub fn matches_key_only(&self, keysym: Keysym) -> bool {
        if !self.enabled {
            return false;
        }

        // Check primary binding key
        if self.keysym_matches_key(&self.primary_binding.key, keysym) {
            return true;
        }

        // Check fallback binding key
        if let Some(ref fallback) = self.fallback_binding
            && self.keysym_matches_key(&fallback.key, keysym)
        {
            return true;
        }

        false
    }

    fn keysym_matches_key(&self, key: &str, keysym: Keysym) -> bool {
        match key {
            "F1" => keysym == Keysym::F1,
            "F2" => keysym == Keysym::F2,
            "F3" => keysym == Keysym::F3,
            "F4" => keysym == Keysym::F4,
            "F5" => keysym == Keysym::F5,
            "F6" => keysym == Keysym::F6,
            "F7" => keysym == Keysym::F7,
            "F8" => keysym == Keysym::F8,
            "F9" => keysym == Keysym::F9,
            "F10" => keysym == Keysym::F10,
            "F11" => keysym == Keysym::F11,
            "F12" => keysym == Keysym::F12,
            "F13" => keysym == Keysym::F13,
            "F14" => keysym == Keysym::F14,
            "F15" => keysym == Keysym::F15,
            "F16" => keysym == Keysym::F16,
            "F17" => keysym == Keysym::F17,
            "F18" => keysym == Keysym::F18,
            "F19" => keysym == Keysym::F19,
            "F20" => keysym == Keysym::F20,
            "F21" => keysym == Keysym::F21,
            "F22" => keysym == Keysym::F22,
            "F23" => keysym == Keysym::F23,
            "F24" => keysym == Keysym::F24,
            "XF86TouchpadOff" => keysym.raw() == 0x1008ffb1,
            "Super_L" => keysym == Keysym::Super_L || keysym == Keysym::Super_R,
            "Super_R" => keysym == Keysym::Super_L || keysym == Keysym::Super_R,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = VoiceConfig::default();
        assert!(config.enabled);
        assert_eq!(config.primary_binding.key, "Super_L");
        assert_eq!(config.primary_binding.modifiers, VoiceModifiers::none());
        assert!(config.fallback_binding.is_none());
    }

    /// No default binding claims the OEM AI key, so it falls through to the
    /// shortcut config and can reach `System(Screenshot)` — it sits in the Print
    /// Screen position on the Humain One and used to open chat instead.
    ///
    /// Note the separate hard-coded bare-F18 intercept in `input::handle_key`,
    /// which still claims F18 ahead of the shortcut config.
    #[test]
    fn test_oem_ai_key_is_not_a_voice_binding() {
        let config = VoiceConfig::default();
        let logo_held = ModifiersState {
            logo: true,
            ..Default::default()
        };
        let touchpad_off = Keysym::new(0x1008ffb1);

        for keysym in [Keysym::F23, Keysym::F18, touchpad_off] {
            assert!(!config.matches_binding(keysym, &logo_held));
            assert!(!config.matches_binding(keysym, &ModifiersState::default()));
            assert!(!config.matches_key_only(keysym));
        }
    }

    /// An explicit `F23` binding must match F23 only — it used to alias F18 and
    /// XF86TouchpadOff, which is what claimed the AI key regardless of config.
    #[test]
    fn test_f23_binding_does_not_alias_other_keys() {
        let config = VoiceConfig {
            primary_binding: VoiceKeyBinding {
                key: "F23".to_string(),
                modifiers: VoiceModifiers::none(),
            },
            ..VoiceConfig::default()
        };
        let no_mods = ModifiersState::default();

        assert!(config.matches_binding(Keysym::F23, &no_mods));
        assert!(!config.matches_binding(Keysym::F18, &no_mods));
        assert!(!config.matches_binding(Keysym::new(0x1008ffb1), &no_mods));
    }

    #[test]
    fn test_modifiers_match() {
        let mods = VoiceModifiers::shift_logo();
        let smithay_mods = ModifiersState {
            ctrl: false,
            alt: false,
            shift: true,
            logo: true,
            caps_lock: false,
            num_lock: false,
            ..Default::default()
        };
        assert!(mods.matches(&smithay_mods));
    }
}
