# Quick Reference: Building & Validating the Implementation

## Pre-Build Setup

```bash
# Navigate to workspace
cd /workspaces/cosmic-comp

# Ensure dependencies are installed
sudo apt-get update
sudo apt-get install -y libdisplay-info-dev libxkbcommon-dev pkg-config

# Set PKG_CONFIG_PATH if needed
export PKG_CONFIG_PATH=/usr/lib/x86_64-linux-gnu/pkgconfig:$PKG_CONFIG_PATH
```

## Build Commands

```bash
# 1. Check compilation (fast, no artifacts)
cargo check

# 2. Run tests
cargo test --lib input::

# 3. Build debug version (faster compilation)
cargo build

# 4. Build release version (optimized, slower compilation)
cargo build --release

# 5. Check for warnings
cargo clippy

# 6. Format check
cargo fmt --check

# 7. Full quality check
cargo check && cargo clippy && cargo fmt --check && cargo test --lib
```

## Verification Checklist

### Code Quality
```bash
# Run in order:
cargo check          # Verify compiles
cargo clippy         # Check for Rust best practices
cargo fmt --check    # Verify code formatting
cargo test --lib input:: -- --nocapture  # Run tests with output
```

### Git Status
```bash
# Show what changed
git diff src/input/

# Show summary
git status
```

### Files Modified
```bash
# Check which files we modified
git diff --name-only

# Expected output should show:
# src/input/gestures/mod.rs
# src/input/actions.rs
# src/input/mod.rs
```

## Running the Compositor

```bash
# Start the built compositor (debug mode)
./target/debug/cosmic-comp

# Or release mode (for actual use)
./target/release/cosmic-comp

# Run in a nested Wayland session (for testing)
COSMIC_COMP_PATH=./target/debug/cosmic-comp weston-launch
```

## Testing Gestures

### Prerequisites
1. Running in a Wayland session
2. Touchpad that supports gesture events
3. COSMIC shell active

### Test Sequence

```
Test 1: 3-Finger Right Swipe
Expected: Window list appears (Alt-Tab equivalent)
Steps:
  1. Place 3 fingers on touchpad
  2. Swipe right (at least 20px)
  3. Observe: Window switcher popup appears

Test 2: 3-Finger Left Swipe  
Expected: Previous window in switcher
Steps:
  1. Place 3 fingers on touchpad
  2. Swipe left (at least 20px)
  3. Observe: Window switcher shows previous window

Test 3: 4-Finger Horizontal (Horizontal Layout)
Expected: Workspace changes
Steps:
  1. Place 4 fingers on touchpad
  2. Swipe right or left
  3. Observe: Workspace changes (previous behavior)

Test 4: 4-Finger Vertical (Vertical Layout)
Expected: Workspace changes
Steps:
  1. Set workspace layout to vertical
  2. Place 4 fingers on touchpad
  3. Swipe up or down
  4. Observe: Workspace changes

Test 5: Accidental Trigger Prevention
Expected: Small touches don't trigger
Steps:
  1. Place 3 fingers on touchpad
  2. Move < 20px
  3. Observe: Nothing happens
  4. Move > 20px
  5. Observe: Action triggers

Test 6: Rapid Successive Gestures
Expected: Each gesture triggers independently
Steps:
  1. Perform 3-finger right swipe
  2. Immediately perform 3-finger left swipe
  3. Observe: Both actions execute correctly
```

## Debugging

### Enable Detailed Logging
```bash
# Run with RUST_LOG
RUST_LOG=trace ./target/debug/cosmic-comp

# Filter to gesture events only
RUST_LOG=cosmic_comp::input::gestures=trace ./target/debug/cosmic-comp

# Or for actions
RUST_LOG=cosmic_comp::input::actions=trace ./target/debug/cosmic-comp
```

### Common Issues & Solutions

**Issue: Build fails with "libdisplay-info not found"**
```bash
# Solution:
sudo apt-get install libdisplay-info-dev
export PKG_CONFIG_PATH=/usr/lib/x86_64-linux-gnu/pkgconfig:$PKG_CONFIG_PATH
cargo clean
cargo check
```

**Issue: Gestures not detected**
```bash
# Check touchpad supports gestures:
cat /proc/bus/input/devices | grep -i touchpad

# Check gesture events:
RUST_LOG=debug ./target/debug/cosmic-comp 2>&1 | grep -i gesture
```

**Issue: Action triggers twice**
```bash
# Check GestureState.triggered flag is working:
RUST_LOG=cosmic_comp::input=debug ./target/debug/cosmic-comp
# Look for: "triggered = true" in logs
```

**Issue: Accidental triggers on small swipes**
```bash
# Adjust MIN_GESTURE_DISTANCE in src/input/gestures/mod.rs
# Currently set to 20.0 pixels
# Increase for less sensitivity, decrease for more
```

## Performance Profiling

```bash
# Build with profiling enabled
cargo build --release

# Run with profiling
PERF_FLAMEGRAPH=1 ./target/release/cosmic-comp

# Analyze gesture-specific performance
perf record -e task-clock -F 99 -p $(pgrep cosmic-comp)
perf report
```

## CI/CD Preparation

```bash
# Run all checks before committing
./check_all.sh 2>&1

# Or manually:
cargo fmt && \
cargo clippy -- -D warnings && \
cargo check && \
cargo test --lib && \
cargo build --release

# If all pass, ready for upstream:
git add src/input/
git commit -m "feat: Add 3-finger window switcher gestures"
git push origin gesture-3finger-window-switcher
```

## Create Pull Request

```bash
# Ensure we're on the feature branch
git branch -v

# Push changes
git push origin gesture-3finger-window-switcher

# Open GitHub PR:
# Title: "Add native 3-finger horizontal touchpad gestures for Window Switcher"
# Description: See GESTURE_IMPLEMENTATION.md
```

## Files Modified Summary

```
src/input/gestures/mod.rs
  - Added WindowSwitcher and WindowSwitcherPrevious variants to SwipeAction
  - Added triggered field to GestureState
  - Added MIN_GESTURE_DISTANCE constant
  - Updated GestureState::new() and update() methods

src/input/actions.rs
  - Added SERIAL_COUNTER import
  - Added SwipeAction::WindowSwitcher match arm
  - Added SwipeAction::WindowSwitcherPrevious match arm

src/input/mod.rs
  - Modified GestureSwipeBegin finger count check
  - Modified GestureSwipeUpdate 3-finger logic
  - Added accidental trigger prevention check
  - Modified GestureSwipeEnd to handle new actions

GESTURE_IMPLEMENTATION.md (NEW)
  - Comprehensive documentation
  - Architecture decisions
  - Testing instructions
```

## Expected Output After Build

```
$ cargo check
   Compiling cosmic-comp v0.1.0
    Checking cosmic-comp v0.1.0
     Finished `dev` profile [unoptimized + debuginfo] target(s) in X.XXs

$ cargo clippy
    Checking cosmic-comp v0.1.0
    Finished `dev` profile [unoptimized + debuginfo] target(s) in X.XXs

$ cargo build --release
   Compiling cosmic-comp v0.1.0
    Finished `release` profile [optimized] target(s) in X.XXs
```

## Next Steps

1. ✅ Code changes implemented
2. ✅ Compilation verified  
3. ✅ Error checking complete
4. Run: `cargo build --release`
5. Run: Manual gesture testing
6. Create: GitHub pull request
7. Request: Review from maintainers
8. Address: Review feedback
9. Merge: Into cosmic-comp main branch
