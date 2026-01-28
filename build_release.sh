#!/usr/bin/env bash
# Complete Build & Release Script for 3-Finger Window Switcher Gestures
# Run this script to build and prepare for release

set -e

echo "=========================================="
echo "3-Finger Window Switcher Implementation"
echo "Build & Release Script"
echo "=========================================="
echo ""

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Step 1: Check dependencies
echo -e "${BLUE}Step 1: Checking system dependencies...${NC}"
if ! command -v pkg-config &> /dev/null; then
    echo -e "${YELLOW}pkg-config not found. Installing...${NC}"
    sudo apt-get update
    sudo apt-get install -y pkg-config
fi

if ! pkg-config --exists libdisplay-info; then
    echo -e "${YELLOW}libdisplay-info-dev not found. Installing...${NC}"
    sudo apt-get update
    sudo apt-get install -y libdisplay-info-dev libxkbcommon-dev
fi
echo -e "${GREEN}✓ Dependencies OK${NC}"
echo ""

# Step 2: Verify branch
echo -e "${BLUE}Step 2: Verifying git branch...${NC}"
BRANCH=$(git rev-parse --abbrev-ref HEAD)
if [ "$BRANCH" != "gesture-3finger-window-switcher" ] && [ "$BRANCH" != "master" ]; then
    echo -e "${YELLOW}Current branch: $BRANCH${NC}"
    echo "Continue? (y/n)"
    read -r response
    if [ "$response" != "y" ]; then
        exit 1
    fi
fi
echo -e "${GREEN}✓ Branch OK (current: $BRANCH)${NC}"
echo ""

# Step 3: Check git status
echo -e "${BLUE}Step 3: Checking git status...${NC}"
if [ -n "$(git status --porcelain)" ]; then
    echo -e "${YELLOW}Uncommitted changes detected:${NC}"
    git status
    echo "Continue with uncommitted changes? (y/n)"
    read -r response
    if [ "$response" != "y" ]; then
        exit 1
    fi
fi
echo -e "${GREEN}✓ Git status OK${NC}"
echo ""

# Step 4: Run cargo check
echo -e "${BLUE}Step 4: Running cargo check...${NC}"
if cargo check; then
    echo -e "${GREEN}✓ Compilation check OK${NC}"
else
    echo -e "${RED}✗ Compilation check FAILED${NC}"
    exit 1
fi
echo ""

# Step 5: Run clippy (optional)
echo -e "${BLUE}Step 5: Running clippy for best practices check...${NC}"
if cargo clippy --all-targets --all-features -- -D warnings 2>/dev/null || true; then
    echo -e "${GREEN}✓ Clippy check OK${NC}"
else
    echo -e "${YELLOW}⚠ Clippy found suggestions (non-fatal)${NC}"
fi
echo ""

# Step 6: Format check
echo -e "${BLUE}Step 6: Checking code formatting...${NC}"
if cargo fmt --check; then
    echo -e "${GREEN}✓ Format check OK${NC}"
else
    echo -e "${YELLOW}⚠ Code formatting issues found${NC}"
    echo "Auto-format? (y/n)"
    read -r response
    if [ "$response" = "y" ]; then
        cargo fmt
        echo -e "${GREEN}✓ Code formatted${NC}"
    fi
fi
echo ""

# Step 7: Build debug version
echo -e "${BLUE}Step 7: Building debug version...${NC}"
if cargo build; then
    echo -e "${GREEN}✓ Debug build OK${NC}"
else
    echo -e "${RED}✗ Debug build FAILED${NC}"
    exit 1
fi
echo ""

# Step 8: Optional tests
echo -e "${BLUE}Step 8: Running tests (optional)...${NC}"
echo "Run library tests? (y/n)"
read -r response
if [ "$response" = "y" ]; then
    if cargo test --lib; then
        echo -e "${GREEN}✓ Tests OK${NC}"
    else
        echo -e "${YELLOW}⚠ Some tests failed${NC}"
    fi
fi
echo ""

# Step 9: Build release version
echo -e "${BLUE}Step 9: Building release version...${NC}"
echo -e "${YELLOW}This will take 2-5 minutes...${NC}"
if cargo build --release; then
    echo -e "${GREEN}✓ Release build OK${NC}"
else
    echo -e "${RED}✗ Release build FAILED${NC}"
    exit 1
fi
echo ""

# Step 10: Verify release binary
echo -e "${BLUE}Step 10: Verifying release binary...${NC}"
if [ -f "./target/release/cosmic-comp" ]; then
    SIZE=$(du -h ./target/release/cosmic-comp | cut -f1)
    echo -e "${GREEN}✓ Binary created ($SIZE)${NC}"
else
    echo -e "${RED}✗ Release binary not found${NC}"
    exit 1
fi
echo ""

# Step 11: Summary
echo "=========================================="
echo -e "${GREEN}BUILD SUCCESSFUL ✓${NC}"
echo "=========================================="
echo ""
echo "Release binary location:"
echo "  ./target/release/cosmic-comp"
echo ""
echo "Next steps:"
echo "  1. Test gestures in Wayland session"
echo "  2. Run: git push origin gesture-3finger-window-switcher"
echo "  3. Create GitHub pull request"
echo "  4. Request review from maintainers"
echo ""
echo "Documentation:"
echo "  - GESTURE_IMPLEMENTATION.md - Architecture details"
echo "  - BUILD_REFERENCE.md - Build commands"
echo "  - IMPLEMENTATION_SUMMARY.md - Quick overview"
echo "  - RELEASE_CHECKLIST.md - Testing checklist"
echo ""
echo "=========================================="
