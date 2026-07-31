// FSR 1.0 — RCAS (Robust Contrast-Adaptive Sharpening)
//
// The sharpening half of FidelityFX Super Resolution 1.0, run after EASU. EASU
// deliberately leaves its output soft because an upscaler that invents detail
// produces ringing; RCAS restores apparent detail with a limiter that prevents
// exactly that.
//
// For each pixel it takes a 5-tap cross, derives how much sharpening the local
// neighbourhood can absorb before clipping, and applies only that much. Flat
// areas are left alone, so noise is not amplified.
//
// Written against GLSL ES 1.00. Runs at destination resolution, so it is a
// straight 1:1 pass.

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

// Reciprocal of the texture size, i.e. one texel in UV space.
uniform vec2 inv_size;
// Sharpening strength. FSR expresses this as a stop value where 0.0 is maximum
// sharpening; this is the already-converted linear factor.
uniform float sharpness;

#if defined(DEBUG_FLAGS)
uniform float tint;
#endif

float luma(vec3 c) {
    return c.g * 0.5 + (c.r + c.b) * 0.25;
}

void main() {
    vec2 uv = v_coords;

    //      b
    //   d  e  f
    //      h
    vec3 b = texture2D(tex, uv + vec2(0.0, -inv_size.y)).rgb;
    vec3 d = texture2D(tex, uv + vec2(-inv_size.x, 0.0)).rgb;
    vec3 e = texture2D(tex, uv).rgb;
    vec3 f = texture2D(tex, uv + vec2(inv_size.x, 0.0)).rgb;
    vec3 h = texture2D(tex, uv + vec2(0.0, inv_size.y)).rgb;

    // Per-channel neighbourhood range: how far the centre can move before it
    // would clip against its own neighbours.
    vec3 mn = min(min(b, d), min(f, h));
    vec3 mx = max(max(b, d), max(f, h));

    // Headroom in both directions, expressed relative to the range. The min()
    // across directions is what keeps the sharpening from overshooting.
    vec3 hit_min = min(mn, e) / (4.0 * mx + 1.0 / 32768.0);
    vec3 hit_max = (1.0 - max(mx, e)) / (4.0 * mx - 4.0 * mn + 1.0 / 32768.0);
    vec3 lobe_rgb = max(-hit_min, hit_max);
    float lobe = max(-0.1875, min(max(lobe_rgb.r, max(lobe_rgb.g, lobe_rgb.b)), 0.0)) * sharpness;

    // Denoise: scale the sharpening down where the local luma range is already
    // tiny, so film grain and dithering are not amplified into speckle.
    float l_b = luma(b), l_d = luma(d), l_e = luma(e), l_f = luma(f), l_h = luma(h);
    float l_min = min(min(l_b, l_d), min(l_f, min(l_h, l_e)));
    float l_max = max(max(l_b, l_d), max(l_f, max(l_h, l_e)));
    float noise = clamp((l_max - l_min) * 4.0, 0.0, 1.0);
    lobe *= noise;

    vec3 color = (b + d + f + h) * lobe + e;
    color /= (4.0 * lobe + 1.0);
    color = clamp(color, 0.0, 1.0);

    vec4 result = vec4(color, 1.0);
    result *= alpha;

    #if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        result = vec4(0.4, 0.0, 0.0, 0.3) + result * 0.7;
    #endif

    gl_FragColor = result;
}
