precision highp float;
uniform float alpha;
#if defined(DEBUG_FLAGS)
uniform float tint;
#endif
uniform vec2 size;
varying vec2 v_coords;

uniform vec3 color;
uniform float thickness;
uniform vec4 radius;
uniform float scale;

float rounding_alpha(vec2 coords, vec2 size, vec4 radius) {
    vec2 center;
    float r;

    if (coords.x < radius.x && coords.y < radius.x) {
        r = radius.x;
        center = vec2(r, r);
    } else if (size.x - radius.y < coords.x && coords.y < radius.y) {
        r = radius.y;
        center = vec2(size.x - r, r);
    } else if (size.x - radius.z < coords.x && size.y - radius.z < coords.y) {
        r = radius.z;
        center = vec2(size.x - r, size.y - r);
    } else if (coords.x < radius.w && size.y - radius.w < coords.y) {
        r = radius.w;
        center = vec2(r, size.y - r);
    } else {
        return 1.0;
    }

    float dist = distance(coords, center);
    float half_px = 0.5 / scale;
    return 1.0 - smoothstep(r - half_px, r + half_px, dist);
}

void main() {
    vec2 location = v_coords * size;

    float outer_alpha = rounding_alpha(v_coords * size, size, radius);
    float inner_alpha = 1.0;

    if (thickness > 0.0) {
        location -= vec2(thickness);
        vec2 inner_size = size - vec2(thickness * 2.0);
        if (0.0 <= location.x && location.x <= inner_size.x
                && 0.0 <= location.y && location.y <= inner_size.y)
        {
            vec4 inner_radius = radius - vec4(thickness);
            inner_alpha = 1.0 - rounding_alpha(location, inner_size, inner_radius);
        }
    }

    vec4 mix_color = mix(vec4(0.0, 0.0, 0.0, 0.0), vec4(color, alpha), outer_alpha * inner_alpha);

    #if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        mix_color = vec4(0.0, 0.3, 0.0, 0.2) + mix_color * 0.8;
    #endif

    gl_FragColor = mix_color;
}
