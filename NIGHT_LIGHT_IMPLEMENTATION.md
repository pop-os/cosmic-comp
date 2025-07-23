# Night Light Implementation Plan for cosmic-comp

This document outlines the complete implementation plan for adding hardware gamma ramp-based night light functionality to cosmic-comp.

## Overview

The implementation will add hardware gamma ramp support to cosmic-comp, enabling system-level color temperature adjustment without GPU overhead. This integrates cleanly with the existing DRM/KMS backend architecture.

## Architecture Analysis

### Current State
- **UI exists but disabled** in cosmic-settings - complete night light interface is ready
- **Infrastructure in place** - geolocation, sunrise/sunset calculations already implemented for theme switching
- **D-Bus architecture** is well-established and extensible
- **Configuration system** can easily accommodate night light settings

### Missing Components
- **No gamma control** in cosmic-comp compositor - needs DRM gamma ramp manipulation
- **No D-Bus interface** for night light in cosmic-settings-daemon  
- **No wlr-gamma-control protocol** implementation in the compositor

## Implementation Phases

### Phase 1: Foundation (Week 1-2)

#### 1.1 Dependency Additions (`Cargo.toml`)
```toml
[dependencies]
# Add for color temperature conversion
palette = "0.7"
# Or alternatively for direct temperature conversion:
# blackbody = "0.2"

# Add for wlr protocols
wayland-protocols-wlr = "0.3"
memmap2 = "0.9"
```

#### 1.2 Configuration Extensions (`src/config/mod.rs`)

```rust
// Add to OutputConfig struct
#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct OutputConfig {
    // ... existing fields ...
    
    /// Color temperature in Kelvin (1000-25000), None = disabled
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub color_temperature: Option<u32>,
    
    /// Brightness multiplier (0.0-1.0), None = no adjustment  
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub brightness: Option<f32>,
}

// Extend ScreenFilter for system-wide night light
#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct ScreenFilter {
    pub inverted: bool,
    pub color_filter: Option<ColorFilter>,
    
    /// Night light settings
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub night_light: Option<NightLight>,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct NightLight {
    pub enabled: bool,
    pub temperature: u32,        // Target temperature in Kelvin
    pub transition_duration: u32, // Smooth transition time in minutes
    pub preserve_brightness: bool, // Maintain perceived brightness
}
```

#### 1.3 Gamma Management Module (`src/gamma.rs` - new file)

```rust
// SPDX-License-Identifier: GPL-3.0-only

use anyhow::{anyhow, Result};
use smithay::reexports::drm::control::{
    atomic::AtomicModeReq, crtc, property, AtomicCommitFlags, Device as ControlDevice,
};
use std::collections::HashMap;

/// DRM color LUT entry (matches kernel struct drm_color_lut)
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct ColorLut {
    pub red: u16,
    pub green: u16,
    pub blue: u16,
    pub reserved: u16,
}

/// Gamma capabilities for a CRTC
#[derive(Debug, Clone)]
pub struct GammaCapability {
    pub lut_size: usize,
    pub supports_degamma: bool,
}

/// Gamma state manager for a device
pub struct GammaManager {
    capabilities: HashMap<crtc::Handle, GammaCapability>,
    active_temperatures: HashMap<crtc::Handle, u32>,
}

impl GammaManager {
    pub fn new() -> Self {
        Self {
            capabilities: HashMap::new(),
            active_temperatures: HashMap::new(),
        }
    }

    /// Query gamma capabilities for a CRTC
    pub fn query_capabilities(
        &mut self,
        device: &impl ControlDevice,
        crtc: crtc::Handle,
    ) -> Result<GammaCapability> {
        let lut_size = crate::backend::kms::drm_helpers::get_property_val(
            device, crtc, "GAMMA_LUT_SIZE"
        )?;
        
        let size = match lut_size.0.convert_value(lut_size.1) {
            property::Value::UnsignedRange(size) => size as usize,
            _ => return Err(anyhow!("Invalid GAMMA_LUT_SIZE property type")),
        };

        // Check for DEGAMMA_LUT support
        let supports_degamma = crate::backend::kms::drm_helpers::get_prop(
            device, crtc, "DEGAMMA_LUT_SIZE"
        ).is_ok();

        let capability = GammaCapability { lut_size: size, supports_degamma };
        self.capabilities.insert(crtc, capability.clone());
        
        Ok(capability)
    }

    /// Apply color temperature to a CRTC
    pub fn set_temperature(
        &mut self,
        device: &impl ControlDevice,
        crtc: crtc::Handle,
        temperature: u32,
        brightness: Option<f32>,
    ) -> Result<()> {
        let capability = self.capabilities.get(&crtc)
            .ok_or_else(|| anyhow!("No gamma capability for CRTC"))?;

        let multipliers = kelvin_to_rgb_multipliers(temperature);
        let mut lut = generate_gamma_lut(capability.lut_size, multipliers);

        // Apply brightness adjustment if specified
        if let Some(brightness) = brightness {
            apply_brightness(&mut lut, brightness);
        }

        // Create blob property for LUT
        let blob_data = unsafe {
            std::slice::from_raw_parts(
                lut.as_ptr() as *const u8,
                lut.len() * std::mem::size_of::<ColorLut>(),
            )
        };
        let blob_id = device.create_property_blob(blob_data)?;

        // Apply gamma LUT
        let gamma_lut_prop = crate::backend::kms::drm_helpers::get_prop(
            device, crtc, "GAMMA_LUT"
        )?;
        
        let mut req = AtomicModeReq::new();
        req.add_property(crtc, gamma_lut_prop, property::Value::Blob(blob_id));
        device.atomic_commit(AtomicCommitFlags::ALLOW_MODESET, req)?;

        self.active_temperatures.insert(crtc, temperature);
        Ok(())
    }

    /// Reset gamma to identity (disable color temperature)
    pub fn reset_gamma(
        &mut self,
        device: &impl ControlDevice,
        crtc: crtc::Handle,
    ) -> Result<()> {
        let gamma_lut_prop = crate::backend::kms::drm_helpers::get_prop(
            device, crtc, "GAMMA_LUT"
        )?;

        let mut req = AtomicModeReq::new();
        req.add_property(crtc, gamma_lut_prop, property::Value::Blob(0));
        device.atomic_commit(AtomicCommitFlags::ALLOW_MODESET, req)?;

        self.active_temperatures.remove(&crtc);
        Ok(())
    }

    pub fn get_temperature(&self, crtc: crtc::Handle) -> Option<u32> {
        self.active_temperatures.get(&crtc).copied()
    }

    /// Apply pre-generated gamma LUT to hardware
    pub fn apply_gamma_lut(
        &mut self,
        device: &impl ControlDevice,
        crtc: crtc::Handle,
        lut: &[ColorLut],
    ) -> Result<()> {
        let blob_data = unsafe {
            std::slice::from_raw_parts(
                lut.as_ptr() as *const u8,
                lut.len() * std::mem::size_of::<ColorLut>(),
            )
        };
        let blob_id = device.create_property_blob(blob_data)?;

        let gamma_lut_prop = crate::backend::kms::drm_helpers::get_prop(
            device, crtc, "GAMMA_LUT"
        )?;

        let mut req = AtomicModeReq::new();
        req.add_property(crtc, gamma_lut_prop, property::Value::Blob(blob_id));
        device.atomic_commit(AtomicCommitFlags::ALLOW_MODESET, req)?;

        Ok(())
    }
}

/// Convert color temperature to RGB multipliers using Tanner Helland algorithm
fn kelvin_to_rgb_multipliers(temperature: u32) -> [f32; 3] {
    let temp = (temperature as f32 / 100.0).clamp(10.0, 400.0);

    let red = if temp <= 66.0 {
        1.0
    } else {
        (329.698727446 * (temp - 60.0).powf(-0.1332047592) / 255.0).clamp(0.0, 1.0)
    };

    let green = if temp <= 66.0 {
        (99.4708025861 * temp.ln() - 161.1195681661) / 255.0
    } else {
        (288.1221695283 * (temp - 60.0).powf(-0.0755148492)) / 255.0
    }.clamp(0.0, 1.0);

    let blue = if temp >= 66.0 {
        1.0
    } else if temp <= 19.0 {
        0.0
    } else {
        (138.5177312231 * (temp - 10.0).ln() - 305.0447927307) / 255.0
    }.clamp(0.0, 1.0);

    [red, green, blue]
}

/// Generate gamma LUT from RGB multipliers
fn generate_gamma_lut(size: usize, multipliers: [f32; 3]) -> Vec<ColorLut> {
    (0..size)
        .map(|i| {
            let normalized = i as f32 / (size - 1) as f32;
            ColorLut {
                red: (normalized * multipliers[0] * 65535.0) as u16,
                green: (normalized * multipliers[1] * 65535.0) as u16,
                blue: (normalized * multipliers[2] * 65535.0) as u16,
                reserved: 0,
            }
        })
        .collect()
}

/// Apply brightness adjustment to existing gamma LUT
fn apply_brightness(lut: &mut [ColorLut], brightness: f32) {
    let brightness = brightness.clamp(0.1, 1.0); // Prevent complete blackout
    for entry in lut.iter_mut() {
        entry.red = (entry.red as f32 * brightness) as u16;
        entry.green = (entry.green as f32 * brightness) as u16;
        entry.blue = (entry.blue as f32 * brightness) as u16;
    }
}
```

### Phase 2: KMS Integration (Week 2-3)

#### 2.1 Device Structure Extension (`src/backend/kms/device.rs`)

```rust
// Add to Device struct around line 90
pub struct Device {
    // ... existing fields ...
    
    /// Gamma management for this device
    pub gamma_manager: crate::gamma::GammaManager,
}

// In Device::new() around line 200
impl Device {
    pub fn new(
        // ... existing params ...
    ) -> Result<Self> {
        // ... existing initialization ...
        
        let mut device = Self {
            // ... existing fields ...
            gamma_manager: crate::gamma::GammaManager::new(),
        };

        // ... rest of existing initialization ...
        Ok(device)
    }
}

// In connector_added() around line 514, after output creation
pub fn connector_added(&mut self, /* ... */) -> Result<()> {
    // ... existing code ...
    
    // Query gamma capabilities after CRTC assignment
    if let Some(crtc) = output_info.crtc {
        if let Err(err) = self.gamma_manager.query_capabilities(&mut self.drm, crtc) {
            tracing::warn!("Failed to query gamma capabilities for CRTC {:?}: {}", crtc, err);
        }
    }
    
    // ... existing code ...
}
```

#### 2.2 Configuration Application (`src/backend/kms/mod.rs`)

```rust
// In apply_config_for_outputs() around line 557, after mode setting
impl KmsState {
    pub fn apply_config_for_outputs(
        // ... existing params ...
    ) -> Result<()> {
        // ... existing mode setting code ...

        // Apply gamma settings after successful mode application
        for (&output, output_config) in config.outputs.iter() {
            if let Some(device) = devices.get_mut(&output.node) {
                if let Some(crtc) = device.output_map.get(&output.connector).and_then(|o| o.crtc) {
                    // Apply color temperature from output config
                    if let Some(temperature) = output_config.color_temperature {
                        if let Err(err) = device.gamma_manager.set_temperature(
                            &device.drm,
                            crtc,
                            temperature,
                            output_config.brightness,
                        ) {
                            tracing::warn!("Failed to apply color temperature: {}", err);
                        }
                    }
                    
                    // Apply global night light from screen filter
                    if let Some(night_light) = &config.filters.night_light {
                        if night_light.enabled {
                            if let Err(err) = device.gamma_manager.set_temperature(
                                &device.drm,
                                crtc, 
                                night_light.temperature,
                                if night_light.preserve_brightness { None } else { Some(1.0) },
                            ) {
                                tracing::warn!("Failed to apply night light: {}", err);
                            }
                        }
                    }
                }
            }
        }

        // ... rest of existing code ...
    }
}
```

### Phase 3: State Management (Week 3-4)

#### 3.1 State Structure Extensions (`src/state.rs`)

```rust
// Add gamma module import
pub mod gamma;

// Add to State struct
impl State {
    /// Update night light settings for all outputs
    pub fn update_night_light(&mut self, enabled: bool, temperature: Option<u32>) -> Result<()> {
        let mut config = self.config.write().unwrap();
        
        // Update screen filter configuration
        if config.filters.night_light.is_none() {
            config.filters.night_light = Some(crate::config::NightLight {
                enabled: false,
                temperature: 3400, // Warm default
                transition_duration: 30,
                preserve_brightness: true,
            });
        }
        
        if let Some(ref mut night_light) = config.filters.night_light {
            night_light.enabled = enabled;
            if let Some(temp) = temperature {
                night_light.temperature = temp;
            }
        }
        
        // Apply immediately to all outputs
        if let BackendData::Kms(ref mut kms_state) = self.backend {
            kms_state.apply_config_for_outputs(
                &mut kms_state.drm_devices, 
                &config, 
                &self.common.output_configuration_state,
                &self.common.shell.read().unwrap().outputs().cloned().collect::<Vec<_>>(),
                false,
            )?;
        }
        
        Ok(())
    }

    fn get_gamma_size_for_output(&self, output: &Output) -> Option<usize> {
        // Get gamma LUT size from KMS backend
        if let crate::state::BackendData::Kms(ref kms) = self.backend {
            // Find device and CRTC for this output
            for device in kms.drm_devices.values() {
                if let Some(output_info) = device.outputs.values().find(|o| &o.output == output) {
                    if let Some(crtc) = output_info.crtc {
                        return device.gamma_manager.capabilities.get(&crtc)
                            .map(|cap| cap.lut_size);
                    }
                }
            }
        }
        None
    }

    fn apply_gamma_from_fd(&mut self, wl_output: &WlOutput, fd: std::os::fd::OwnedFd) -> Result<()> {
        use std::os::fd::AsRawFd;
        
        let output = self.common.output_from_wl_output(wl_output)
            .ok_or_else(|| anyhow::anyhow!("Output not found"))?;

        let gamma_size = self.get_gamma_size_for_output(&output)
            .ok_or_else(|| anyhow::anyhow!("No gamma support for output"))?;

        // Memory map the gamma data from client
        let expected_size = gamma_size * 3 * 2; // 3 channels × 16-bit values
        let mmap = unsafe {
            memmap2::MmapOptions::new()
                .len(expected_size)
                .map(&fd)?
        };

        // Parse gamma ramps from mmap (R, G, B arrays)
        let raw_data: &[u16] = unsafe {
            std::slice::from_raw_parts(
                mmap.as_ptr() as *const u16,
                gamma_size * 3,
            )
        };

        let red = &raw_data[0..gamma_size];
        let green = &raw_data[gamma_size..gamma_size * 2]; 
        let blue = &raw_data[gamma_size * 2..gamma_size * 3];

        // Convert to ColorLut format
        let gamma_lut: Vec<crate::gamma::ColorLut> = (0..gamma_size)
            .map(|i| crate::gamma::ColorLut {
                red: red[i],
                green: green[i], 
                blue: blue[i],
                reserved: 0,
            })
            .collect();

        // Apply to hardware via KMS backend
        if let crate::state::BackendData::Kms(ref mut kms) = self.backend {
            for device in kms.drm_devices.values_mut() {
                if let Some(output_info) = device.outputs.values().find(|o| &o.output == output) {
                    if let Some(crtc) = output_info.crtc {
                        device.gamma_manager.apply_gamma_lut(&device.drm, crtc, &gamma_lut)?;
                        break;
                    }
                }
            }
        }

        Ok(())
    }

    fn reset_gamma_for_output(&mut self, wl_output: &WlOutput) -> Result<()> {
        let output = self.common.output_from_wl_output(wl_output)
            .ok_or_else(|| anyhow::anyhow!("Output not found"))?;

        if let crate::state::BackendData::Kms(ref mut kms) = self.backend {
            for device in kms.drm_devices.values_mut() {
                if let Some(output_info) = device.outputs.values().find(|o| &o.output == output) {
                    if let Some(crtc) = output_info.crtc {
                        device.gamma_manager.reset_gamma(&device.drm, crtc)?;
                        break;
                    }
                }
            }
        }

        Ok(())
    }
}
```

### Phase 4: External Interface (Week 4-5)

#### 4.1 D-Bus Interface Extension (`src/dbus/mod.rs`)

```rust
// Add night light module
mod night_light;

// In init() function around line 9
pub fn init(evlh: &LoopHandle<'static, State>) -> Result<Vec<RegistrationToken>> {
    let mut tokens = Vec::new();

    // Initialize Night Light D-Bus interface  
    match night_light::init() {
        Ok(night_light_daemon) => {
            let (tx, rx) = calloop::channel::channel();
            
            let token = evlh
                .insert_source(rx, |event, _, state| {
                    if let calloop::channel::Event::Msg(command) = event {
                        if let Err(err) = handle_night_light_command(state, command) {
                            tracing::error!("Failed to handle night light command: {}", err);
                        }
                    }
                })
                .map_err(|e| e.error)?;
            
            tokens.push(token);
            
            // Start night light service thread
            std::thread::spawn(move || {
                night_light_daemon.run(tx);
            });
        }
        Err(err) => {
            tracing::info!("Failed to initialize night light service: {}", err);
        }
    }

    // ... existing power daemon code ...
    
    Ok(tokens)
}

fn handle_night_light_command(state: &mut State, command: night_light::Command) -> Result<()> {
    match command {
        night_light::Command::SetEnabled(enabled) => {
            state.update_night_light(enabled, None)?;
        }
        night_light::Command::SetTemperature(temperature) => {
            state.update_night_light(true, Some(temperature))?;
        }
    }
    Ok(())
}
```

#### 4.2 Night Light D-Bus Interface (`src/dbus/night_light.rs` - new file)

```rust
// SPDX-License-Identifier: GPL-3.0-only

use anyhow::Result;
use calloop::channel::Sender;
use zbus::{Connection, interface};

#[derive(Debug)]
pub enum Command {
    SetEnabled(bool),
    SetTemperature(u32),
}

pub struct NightLightInterface {
    sender: Sender<Command>,
}

#[interface(name = "com.system76.CosmicComp.NightLight")]
impl NightLightInterface {
    /// Enable or disable night light
    async fn set_enabled(&mut self, enabled: bool) -> zbus::fdo::Result<()> {
        self.sender.send(Command::SetEnabled(enabled))
            .map_err(|_| zbus::fdo::Error::Failed("Failed to send command".into()))?;
        Ok(())
    }

    /// Set color temperature (1000-25000K)
    async fn set_temperature(&mut self, temperature: u32) -> zbus::fdo::Result<()> {
        let clamped_temp = temperature.clamp(1000, 25000);
        self.sender.send(Command::SetTemperature(clamped_temp))
            .map_err(|_| zbus::fdo::Error::Failed("Failed to send command".into()))?;
        Ok(())
    }

    /// Get current temperature setting
    #[zbus(property)]
    async fn temperature(&self) -> u32 {
        // This would need state access - simplified for example
        6500
    }

    /// Get current enabled state  
    #[zbus(property)]
    async fn enabled(&self) -> bool {
        // This would need state access - simplified for example
        false
    }
}

pub fn init() -> Result<NightLightService> {
    Ok(NightLightService::new())
}

pub struct NightLightService {
    connection: Option<Connection>,
}

impl NightLightService {
    fn new() -> Self {
        Self { connection: None }
    }

    pub fn run(&mut self, sender: Sender<Command>) {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            let connection = Connection::session().await.unwrap();
            
            let interface = NightLightInterface { sender };
            
            connection
                .object_server()
                .at("/com/system76/CosmicComp/NightLight", interface)
                .await
                .unwrap();

            connection
                .request_name("com.system76.CosmicComp.NightLight")
                .await
                .unwrap();

            self.connection = Some(connection);
            
            // Keep service running
            loop {
                tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            }
        });
    }
}
```

#### 4.3 wlr-gamma-control Protocol Implementation (`src/wayland/protocols/gamma_control.rs` - new file)

```rust
// SPDX-License-Identifier: GPL-3.0-only

use anyhow::Result;
use smithay::{
    reexports::wayland_server::{
        protocol::wl_output::WlOutput, Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch,
        New, Resource,
    },
    wayland::output::Output,
};
use wayland_protocols_wlr::gamma_control::v1::server::{
    zwlr_gamma_control_manager_v1::{self, ZwlrGammaControlManagerV1},
    zwlr_gamma_control_v1::{self, ZwlrGammaControlV1},
};

use crate::state::State;

/// Global gamma control manager
pub struct GammaControlManagerState;

impl GlobalDispatch<ZwlrGammaControlManagerV1, ()> for State {
    fn bind(
        state: &mut Self,
        handle: &DisplayHandle,
        client: &Client,
        resource: New<ZwlrGammaControlManagerV1>,
        _data: &(),
        data_init: &mut DataInit<'_, Self>,
    ) {
        let manager = data_init.init(resource, ());
        // Manager is now bound - no immediate action needed
    }
}

impl Dispatch<ZwlrGammaControlManagerV1, ()> for State {
    fn request(
        state: &mut Self,
        client: &Client,
        resource: &ZwlrGammaControlManagerV1,
        request: zwlr_gamma_control_manager_v1::Request,
        data: &(),
        dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, Self>,
    ) {
        match request {
            zwlr_gamma_control_manager_v1::Request::GetGammaControl { id, output } => {
                let gamma_control = data_init.init(id, GammaControlState::new(output));
                
                // Send gamma size for this output
                if let Some(output_data) = state.common.output_from_wl_output(&output) {
                    if let Some(size) = state.get_gamma_size_for_output(&output_data) {
                        gamma_control.gamma_size(size as u32);
                    }
                }
            }
            zwlr_gamma_control_manager_v1::Request::Destroy => {
                // Manager destroyed - cleanup handled automatically
            }
            _ => {}
        }
    }
}

/// Per-output gamma control instance
pub struct GammaControlState {
    output: WlOutput,
}

impl GammaControlState {
    fn new(output: WlOutput) -> Self {
        Self { output }
    }
}

impl Dispatch<ZwlrGammaControlV1, GammaControlState> for State {
    fn request(
        state: &mut Self,
        client: &Client,
        resource: &ZwlrGammaControlV1,
        request: zwlr_gamma_control_v1::Request,
        data: &GammaControlState,
        dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, Self>,
    ) {
        match request {
            zwlr_gamma_control_v1::Request::SetGamma { fd } => {
                if let Err(err) = state.apply_gamma_from_fd(&data.output, fd) {
                    tracing::error!("Failed to apply gamma from client: {}", err);
                    resource.failed();
                }
            }
            zwlr_gamma_control_v1::Request::Destroy => {
                // Reset gamma for this output  
                if let Err(err) = state.reset_gamma_for_output(&data.output) {
                    tracing::warn!("Failed to reset gamma: {}", err);
                }
            }
            _ => {}
        }
    }
}
```

#### 4.4 Protocol Registration (`src/wayland/mod.rs`)

```rust
// Add to protocol registration
use gamma_control::GammaControlManagerState;

// In create_global_state()
state.display_handle.create_global::<State, ZwlrGammaControlManagerV1, _>(
    1, // Version
    (),
);
```

### Phase 5: Polish & Features (Week 5-6)

#### 5.1 Advanced Features
- **Per-output temperature control** via configuration
- **Brightness preservation algorithms** to maintain perceived brightness
- **Transition smoothing** for gradual temperature changes
- **Integration hooks** for cosmic-settings-daemon scheduling

#### 5.2 Testing & Validation
- **Hardware compatibility testing** across different GPU vendors
- **Protocol compliance testing** with existing gamma control tools
- **Performance benchmarks** comparing hardware vs software filtering
- **Edge case handling** for displays without gamma support

## Key Integration Points

### Existing cosmic-comp Architecture
- **DRM/KMS Backend**: Uses smithay's comprehensive DRM device management
- **Output Management**: Per-output configuration and atomic mode-setting
- **State Management**: Centralized state with configuration watching
- **Wayland Protocols**: Well-established protocol handler patterns

### COSMIC Ecosystem Integration
- **cosmic-settings**: UI already exists but disabled
- **cosmic-settings-daemon**: D-Bus architecture ready for night light
- **cosmic-config**: Type-safe configuration system supports new settings
- **Theme management**: Existing geolocation and sunrise/sunset calculation

## Benefits

1. **Hardware gamma ramps** - Zero GPU overhead vs current software filtering
2. **Per-output control** - Individual color temperature per display
3. **System-wide night light** - Global night light via screen filters  
4. **Tool compatibility** - Works with existing Linux gamma control applications
5. **Clean integration** - Fits naturally into cosmic-comp's architecture

## File Locations Summary

**New files to create**:
- `src/gamma.rs` - Core gamma management and color temperature conversion
- `src/dbus/night_light.rs` - D-Bus interface for external control
- `src/wayland/protocols/gamma_control.rs` - wlr-gamma-control protocol
- `NIGHT_LIGHT_IMPLEMENTATION.md` - This implementation plan

**Key files to modify**:
- `Cargo.toml` - Add dependencies
- `src/config/mod.rs` - Configuration structures
- `src/backend/kms/device.rs` - Device-level gamma capability  
- `src/backend/kms/mod.rs` - Integration in output configuration
- `src/state.rs` - State management integration
- `src/dbus/mod.rs` - D-Bus interface registration
- `src/wayland/mod.rs` - Protocol registration

This implementation leverages cosmic-comp's existing architecture while providing comprehensive night light functionality that integrates with both the COSMIC desktop environment and standard Linux gamma control tools.