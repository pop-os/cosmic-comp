#!/usr/bin/env python3
"""Regenerate the NVIDIA Image Scaling filter coefficients.

The tables are 64 phases x 8 taps of hand-tuned constants; transcribing them by
hand risks a silent single-digit error that produces subtly wrong output with no
failure anywhere. Generating them keeps the values exact and makes an SDK bump a
one-command change.

Usage:
    scripts/generate-nis-coefficients.py path/to/NIS_Config.h

Writes src/backend/render/nis_coefficients.rs, preserving the SDK's MIT notice
(required by its licence, which is why that header is not editorial).
"""

import argparse
import pathlib
import re
import sys

PHASE_COUNT = 64
FILTER_SIZE = 8

LICENCE = """// SPDX-License-Identifier: GPL-3.0-only AND MIT
//
// Filter coefficients for NVIDIA Image Scaling, generated from the NVIDIA Image
// Scaling SDK (NIS_Config.h) by scripts/generate-nis-coefficients.py.
// Do not edit by hand -- regenerate so the values stay exact.
//
// The MIT License(MIT)
//
// Copyright(c) 2022 NVIDIA CORPORATION & AFFILIATES. All rights reserved.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of
// this software and associated documentation files(the "Software"), to deal in
// the Software without restriction, including without limitation the rights to
// use, copy, modify, merge, publish, distribute, sublicense, and / or sell copies of
// the Software, and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions :
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
// FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE AUTHORS OR
// COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
// IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."""


def parse_table(source: str, name: str) -> list[list[float]]:
    """Pull one [kPhaseCount][kFilterSize] float table out of the header."""
    match = re.search(
        rf"constexpr float {name}\[kPhaseCount\]\[kFilterSize\] = \{{(.*?)\n    \}};",
        source,
        re.S,
    )
    if not match:
        sys.exit(f"error: could not find {name} -- has the SDK layout changed?")

    rows = []
    for row in re.findall(r"\{([^{}]*)\}", match.group(1)):
        values = [v.strip().rstrip("f") for v in row.split(",") if v.strip()]
        if len(values) != FILTER_SIZE:
            sys.exit(f"error: {name} row has {len(values)} taps, expected {FILTER_SIZE}")
        rows.append([float(v) for v in values])

    if len(rows) != PHASE_COUNT:
        sys.exit(f"error: {name} has {len(rows)} phases, expected {PHASE_COUNT}")
    return rows


def check_resampling_kernel(scale: list[list[float]]) -> None:
    """A resampling kernel must be normalised, so every phase sums to 1.

    This is the check that catches a mangled parse: wrong values that still
    happen to have the right shape would otherwise sail through.
    """
    for phase, row in enumerate(scale):
        total = sum(row)
        if abs(total - 1.0) > 1e-3:
            sys.exit(f"error: coef_scale phase {phase} sums to {total}, expected 1.0")


def render(rows: list[list[float]]) -> str:
    return "\n".join("    [" + ", ".join(f"{v:.6}" for v in row) + "]," for row in rows)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("config", type=pathlib.Path, help="path to NIS_Config.h")
    parser.add_argument(
        "-o",
        "--output",
        type=pathlib.Path,
        default=pathlib.Path(__file__).parent.parent
        / "src/backend/render/nis_coefficients.rs",
    )
    args = parser.parse_args()

    source = args.config.read_text()
    scale = parse_table(source, "coef_scale")
    usm = parse_table(source, "coef_usm")
    check_resampling_kernel(scale)

    args.output.write_text(
        f"""{LICENCE}

/// Number of subpixel phases the filter is sampled at.
pub const PHASE_COUNT: usize = {PHASE_COUNT};
/// Taps per phase.
pub const FILTER_SIZE: usize = {FILTER_SIZE};

/// Resampling kernel, one row per subpixel phase. Each row sums to 1.
pub const COEF_SCALE: [[f32; FILTER_SIZE]; PHASE_COUNT] = [
{render(scale)}
];

/// Unsharp-mask kernel applied alongside the resample, one row per phase.
pub const COEF_USM: [[f32; FILTER_SIZE]; PHASE_COUNT] = [
{render(usm)}
];
"""
    )
    print(f"wrote {args.output} ({PHASE_COUNT} phases x {FILTER_SIZE} taps, both tables)")


if __name__ == "__main__":
    main()
