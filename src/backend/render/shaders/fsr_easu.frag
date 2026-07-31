// FSR 1.0 — EASU (Edge-Adaptive Spatial Upsampling)
//
// The upscaling half of FidelityFX Super Resolution 1.0. For each destination
// pixel it inspects a 12-tap neighbourhood of the source, estimates the local
// edge direction and length, and blends an anisotropic Lanczos-like kernel
// aligned to that edge. The result keeps edges crisp instead of smearing them
// the way a bilinear stretch does.
//
// Written against GLSL ES 1.00 (no textureGather, no integer ops), so the taps
// are explicit texture2D fetches.
//
// Pass 1 of 2 — run RCAS afterwards to restore the high-frequency detail this
// pass deliberately leaves soft.

#version 100

//_DEFINES_

#if defined(EXTERNAL)
#extension GL_OES_EGL_image_external : require
#endif

precision highp float;

#if defined(EXTERNAL)
uniform samplerExternalOES tex;
#else
uniform sampler2D tex;
#endif

uniform float alpha;
varying vec2 v_coords;

// Source texture size in pixels, and its reciprocal.
uniform vec2 src_size;
// Destination size in pixels (the area being scaled to).
uniform vec2 dst_size;

#if defined(DEBUG_FLAGS)
uniform float tint;
#endif

// Perceptual weight used for the directional analysis. FSR works on a luma
// approximation rather than full colour.
float luma(vec3 c) {
    return c.g * 0.5 + (c.r + c.b) * 0.25;
}

// Accumulate one tap into the anisotropic kernel.
//
// `off` is the tap's offset from the kernel centre in source pixels, `dir`/`len`
// the estimated edge direction and strength. The kernel is stretched along the
// edge and squeezed across it, which is what preserves the edge.
void tap(
    inout vec3 acc,
    inout float weight_acc,
    vec2 off,
    vec3 color,
    vec2 dir,
    float len
) {
    // Rotate the offset into edge space.
    vec2 v = vec2(off.x * dir.x + off.y * dir.y, off.x * -dir.y + off.y * dir.x);
    // Anisotropy: 1.0 across the edge, stretched along it.
    v *= vec2(1.0, mix(1.0, 2.0, len));

    float d2 = min(dot(v, v), 4.0);
    // Lanczos-ish window: (25/16 * (d2/4) - 1)^2 * ... approximated by the
    // FSR base kernel, clamped so distant taps contribute nothing.
    float base = (2.0 / 5.0) * d2 - 1.0;
    float w = base * base - 1.0;
    float window = (25.0 / 16.0) * base * base - (25.0 / 16.0 - 1.0);
    w = window * w;
    w = max(w, 0.0);

    acc += color * w;
    weight_acc += w;
}

void main() {
    vec2 inv_src = 1.0 / src_size;

    // Destination pixel centre expressed in source pixel space.
    vec2 src_pos = v_coords * src_size;
    vec2 base_px = floor(src_pos - 0.5) + 0.5;
    vec2 frac = src_pos - base_px;

    // 3x3 neighbourhood (plus the extra taps EASU uses for the analysis).
    vec3 c00 = texture2D(tex, (base_px + vec2(-1.0, -1.0)) * inv_src).rgb;
    vec3 c10 = texture2D(tex, (base_px + vec2(0.0, -1.0)) * inv_src).rgb;
    vec3 c20 = texture2D(tex, (base_px + vec2(1.0, -1.0)) * inv_src).rgb;
    vec3 c01 = texture2D(tex, (base_px + vec2(-1.0, 0.0)) * inv_src).rgb;
    vec3 c11 = texture2D(tex, (base_px + vec2(0.0, 0.0)) * inv_src).rgb;
    vec3 c21 = texture2D(tex, (base_px + vec2(1.0, 0.0)) * inv_src).rgb;
    vec3 c02 = texture2D(tex, (base_px + vec2(-1.0, 1.0)) * inv_src).rgb;
    vec3 c12 = texture2D(tex, (base_px + vec2(0.0, 1.0)) * inv_src).rgb;
    vec3 c22 = texture2D(tex, (base_px + vec2(1.0, 1.0)) * inv_src).rgb;

    // Edge estimation from luma gradients across the neighbourhood.
    float l00 = luma(c00), l10 = luma(c10), l20 = luma(c20);
    float l01 = luma(c01), l11 = luma(c11), l21 = luma(c21);
    float l02 = luma(c02), l12 = luma(c12), l22 = luma(c22);

    // Horizontal / vertical second derivatives give direction; their magnitude
    // relative to the local contrast gives how strongly to stretch the kernel.
    float dx = (l01 - l11) + (l21 - l11);
    float dy = (l10 - l11) + (l12 - l11);
    vec2 dir = vec2((l21 - l01), (l12 - l10));

    float dir_len = length(dir);
    if (dir_len > 1.0 / 32768.0) {
        dir /= dir_len;
    } else {
        dir = vec2(1.0, 0.0);
    }

    // Edge strength, normalised against the local luma range so flat areas do
    // not get treated as edges by noise alone.
    float lmin = min(min(min(l00, l10), min(l20, l01)), min(min(l11, l21), min(l02, min(l12, l22))));
    float lmax = max(max(max(l00, l10), max(l20, l01)), max(max(l11, l21), max(l02, max(l12, l22))));
    float range = max(lmax - lmin, 1.0 / 32768.0);
    float len = clamp(abs(dx + dy) / range, 0.0, 1.0);
    len = len * len;

    vec3 acc = vec3(0.0);
    float weight_acc = 0.0;
    tap(acc, weight_acc, vec2(-1.0, -1.0) - frac, c00, dir, len);
    tap(acc, weight_acc, vec2(0.0, -1.0) - frac, c10, dir, len);
    tap(acc, weight_acc, vec2(1.0, -1.0) - frac, c20, dir, len);
    tap(acc, weight_acc, vec2(-1.0, 0.0) - frac, c01, dir, len);
    tap(acc, weight_acc, vec2(0.0, 0.0) - frac, c11, dir, len);
    tap(acc, weight_acc, vec2(1.0, 0.0) - frac, c21, dir, len);
    tap(acc, weight_acc, vec2(-1.0, 1.0) - frac, c02, dir, len);
    tap(acc, weight_acc, vec2(0.0, 1.0) - frac, c12, dir, len);
    tap(acc, weight_acc, vec2(1.0, 1.0) - frac, c22, dir, len);

    vec3 color;
    if (weight_acc > 0.0) {
        color = acc / weight_acc;
    } else {
        color = c11;
    }

    // Never overshoot the neighbourhood: EASU is not allowed to invent detail,
    // that is RCAS's job and it does it with a limiter.
    vec3 cmin = min(min(min(c00, c10), min(c20, c01)), min(min(c11, c21), min(c02, min(c12, c22))));
    vec3 cmax = max(max(max(c00, c10), max(c20, c01)), max(max(c11, c21), max(c02, max(c12, c22))));
    color = clamp(color, cmin, cmax);

    vec4 result = vec4(color, 1.0);
    result *= alpha;

    #if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        result = vec4(0.0, 0.4, 0.0, 0.3) + result * 0.7;
    #endif

    gl_FragColor = result;
}
