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
uniform float noise;

float rounding_alpha(vec2 coords, vec2 size) {
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
    float half_px = 0.5;
    return 1.0 - smoothstep(radius - half_px, radius + half_px, dist);
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

    vec3 coords_geo = input_to_geo * vec3(v_coords, 1.0);
    if (coords_geo.x < 0.0 || 1.0 < coords_geo.x || coords_geo.y < 0.0 || 1.0 < coords_geo.y) {
        // Clip outside geometry.
        color = vec4(0.0);
    } else {
        // Apply corner rounding inside geometry.
        color = color * rounding_alpha(coords_geo.xy * geo_size, geo_size);
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

    // Apply final alpha and tint.
    color *= alpha;

    #if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        color = vec4(0.0, 0.2, 0.0, 0.2) + color * 0.8;
    #endif

    gl_FragColor = color;
}
