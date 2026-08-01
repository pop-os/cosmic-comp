// Taken from niri and modified, licensed GPL-3.0

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

#if defined(DEBUG_FLAGS)
uniform float tint;
#endif

uniform vec2 geo_size;
uniform vec4 corner_radius;
uniform mat3 input_to_geo;
// Output scale, used to keep the corner antialiasing band half a *physical*
// pixel wide (fork addition; see rounding_alpha below).
uniform float scale;
// Upstream frosted-glass noise amount (0.0 disables).
uniform float noise;
// Frosted-glass appearance, carried over from the org_kde_kwin_blur v4 stack
// and reachable through ext_background_effect_v1 version 3. Only the blur
// backdrop asks for these; the plain clipped-surface path sets the identity
// values below. NOTE: every call site must set `saturation`, because an unset
// uniform reads as 0 -- which is fully greyscale, not "unchanged".
// `frost_tint` is spelled out because smithay reserves `tint` for its own
// DEBUG_FLAGS overlay above, and declaring it twice fails to compile.
uniform float saturation;  // 1.0 leaves saturation unchanged
uniform float frost_tint;  // 0.0 disables the white overlay
uniform float border;      // 0.0 disables the hairline border

float rounding_alpha(vec2 coords, vec2 size, float half_px) {
    vec2 center;
    float radius;

    if (coords.x < corner_radius.x && coords.y < corner_radius.x) {
        radius = corner_radius.x;
        center = vec2(radius, radius);
    } else if (size.x - corner_radius.y < coords.x && coords.y < corner_radius.y) {
        radius = corner_radius.y;
        center = vec2(size.x - radius, radius);
    } else if (size.x - corner_radius.z < coords.x && size.y - corner_radius.z < coords.y) {
        radius = corner_radius.z;
        center = vec2(size.x - radius, size.y - radius);
    } else if (coords.x < corner_radius.w && size.y - corner_radius.w < coords.y) {
        radius = corner_radius.w;
        center = vec2(radius, size.y - radius);
    } else {
        return 1.0;
    }

    float dist = distance(coords, center);
    return 1.0 - smoothstep(radius - half_px, radius + half_px, dist);
}

// Signed distance to the rounded rect, negative inside. Only the border needs a
// distance over the whole shape; `rounding_alpha` above returns 1.0 away from
// the corners and so cannot describe the straight edges a border has to trace.
float rounded_box_sdf(vec2 coords, vec2 size) {
    vec2 half_size = size * 0.5;
    vec2 p = coords - half_size;
    float radius;
    if (p.x < 0.0) {
        radius = (p.y < 0.0) ? corner_radius.x : corner_radius.w;
    } else {
        radius = (p.y < 0.0) ? corner_radius.y : corner_radius.z;
    }
    vec2 q = abs(p) - half_size + radius;
    return min(max(q.x, q.y), 0.0) + length(max(q, vec2(0.0))) - radius;
}

float hash(vec2 p) {
    vec3 p3 = fract(vec3(p.xyx) * 727.727);
    p3 += dot(p3, p3.xyz + 33.33);
    return fract((p3.x + p3.y) * p3.z);
}

void main() {
    if (alpha <= 0.0) {
        discard;
    }

    vec4 color = texture2D(tex, v_coords);
    #if defined(NO_ALPHA)
    color = vec4(color.rgb, 1.0);
    #endif
    if (color.a <= 0.0) {
        discard;
    }

    // Frosted glass: saturation lift then white overlay, both on the sampled
    // backdrop before it is masked. Blurring averages colour away, so a backdrop
    // reads flatter than what it was sampled from and the lift is what restores
    // it. Colour is premultiplied here, so white is `color.a` and the saturation
    // result stays clamped to it.
    if (saturation != 1.0) {
        float luma = dot(color.rgb, vec3(0.2126, 0.7152, 0.0722));
        color.rgb = clamp(mix(vec3(luma), color.rgb, saturation), 0.0, color.a);
    }
    if (frost_tint > 0.0) {
        color.rgb = mix(color.rgb, vec3(color.a), frost_tint);
    }

    // `coords` below are in logical pixels, so the antialiasing band has to be
    // scaled down to stay half a *physical* pixel wide. Leaving it at 0.5
    // logical made the corner fade `scale` times softer than the outline drawn
    // over it — visible as a light, blurry rim on the corners only at 125%/200%.
    float half_px = 0.5 / scale;

    vec3 coords_geo = input_to_geo * vec3(v_coords, 1.0);
    if (coords_geo.x < 0.0 || 1.0 < coords_geo.x || coords_geo.y < 0.0 || 1.0 < coords_geo.y) {
        // Clip outside geometry.
        color = vec4(0.0);
    } else {
        // Apply corner rounding inside geometry.
        color = color * rounding_alpha(coords_geo.xy * geo_size, geo_size, half_px);
    }

    if (color.a <= 0.0) {
        discard;
    }

    if (noise > 0.0) {
        // Add noise fx
        // This can be used to achieve a glass look
        float noiseHash = hash(v_coords);
        float noiseAmount = (mod(noiseHash, 1.0) - 0.5);
        color.rgb += noiseAmount * noise;
    }

    // Hairline border tracing the same rounded shape as the backdrop, drawn one
    // *physical* pixel wide so it stays hairline at any scale. Applied after the
    // corner mask so it can never extend past the shape it outlines.
    if (border > 0.0) {
        float dist = rounded_box_sdf(coords_geo.xy * geo_size, geo_size);
        float width = 1.0 / scale;
        float outer = 1.0 - smoothstep(-half_px, half_px, dist);
        float inner = 1.0 - smoothstep(-half_px, half_px, dist + width);
        color = mix(color, vec4(color.a), clamp(outer - inner, 0.0, 1.0) * border);
    }

    // Apply final alpha and tint.
    color *= alpha;

    #if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        color = vec4(0.0, 0.2, 0.0, 0.2) + color * 0.8;
    #endif

    gl_FragColor = color;
}
