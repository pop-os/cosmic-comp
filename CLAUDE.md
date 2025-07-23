# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Building
- `just build` - Build all COSMIC components in release mode
- `just clean` - Remove all build artifacts
- `just install` - Install all components (requires build first)
- `just sysext` - Create a systemd system extension for testing

### Individual Component Commands
Most components use justfiles with consistent commands:
- `just <component>/build-release` - Build specific component
- `just <component>/check` - Run clippy linting (with pedantic warnings)
- `just <component>/test` - Run tests (where available)
- `just <component>/run` - Run component with debug logging

Examples:
- `just cosmic-settings/check`
- `just cosmic-term/run`
- `just cosmic-files/test`

### Legacy Components (using Make)
Some components still use Makefiles:
- `make -C cosmic-comp all`
- `make -C cosmic-osd all` 
- `make -C cosmic-settings-daemon all`

## Architecture Overview

COSMIC is a modular Rust-based desktop environment built entirely on Wayland. The architecture consists of:

### Core System
- **cosmic-session**: Session manager that orchestrates all components
- **cosmic-comp**: Wayland compositor built on Smithay (the heart of the desktop)
- **cosmic-settings-daemon**: Background daemon managing system configuration

### User Interface Components  
- **cosmic-panel**: Top panel and dock system with pluggable applets
- **cosmic-notifications**: Notification system
- **cosmic-launcher**: Application launcher
- **cosmic-bg**: Wallpaper and background management
- **cosmic-greeter**: Login screen/display manager

### Applications
- **cosmic-files**: File manager
- **cosmic-edit**: Text editor  
- **cosmic-term**: Terminal emulator
- **cosmic-settings**: System settings interface
- **cosmic-store**: Software center/app store
- **cosmic-player**: Media player

### Supporting Systems
- **cosmic-applets**: Collection of panel applets (audio, battery, network, time, etc.)
- **cosmic-idle**: Screen lock and power management
- **xdg-desktop-portal-cosmic**: Desktop portal for app sandboxing and integration

## Key Technologies

- **Wayland**: Native Wayland-only desktop (XWayland for legacy apps)
- **libcosmic**: COSMIC's GUI toolkit built on Iced framework
- **Smithay**: Rust Wayland compositor library (foundation of cosmic-comp)
- **cosmic-protocols**: Custom Wayland protocols for COSMIC-specific features
- **cosmic-config**: Type-safe configuration system
- **D-Bus**: Inter-component communication and system integration

## Component Communication

Components communicate through:
1. **Wayland protocols** (primary method via cosmic-protocols)
2. **D-Bus** for system integration and notifications
3. **Configuration files** in `~/.config/cosmic/` managed by cosmic-config
4. **Systemd** for service management (when available)

## Session Startup Flow

1. cosmic-session starts and creates privileged Wayland socket
2. cosmic-comp (compositor) establishes Wayland display
3. cosmic-settings-daemon starts for configuration management
4. UI components start (cosmic-panel, cosmic-notifications, cosmic-launcher, etc.)
5. xdg-desktop-portal-cosmic enables app integration
6. Desktop is ready for user interaction

## Development Tips

### Configuration System
- All configuration uses cosmic-config for type safety
- Configuration files are monitored for live updates
- Settings changes propagate through cosmic-settings-daemon

### Build System Notes
- Main justfile orchestrates all component builds
- Individual components have their own justfiles with consistent commands
- Cargo workspaces are used within components for related crates
- Use `just sysext` to create a testable system extension

### Testing
- `just build` builds everything needed for testing
- Use individual component `run` commands for debugging
- Most components support `RUST_LOG=debug` for detailed logging
- System extension method (`just sysext`) recommended for full desktop testing

# Development Environment

- You are running in a dedicated Arch Linux container
- Install any tool you want using `pacman`

