precision highp float;
uniform float alpha;
#if defined(DEBUG_FLAGS)
uniform float tint;
#endif
uniform vec2 size;
varying vec2 v_coords;

uniform vec3 color;
uniform float radius;

float rounded_box(in vec2 p, in vec2 b, in vec4 r)
{
    r.xy = (p.x > 0.0) ? r.xy : r.zw;
    r.x = (p.y > 0.0) ? r.x : r.y;
    vec2 q = abs(p) - b + r.x;
    return min(max(q.x, q.y), 0.0) + length(max(q, 0.0)) - r.x;
}

void main() {
    vec2 center = size / 2.0;
    vec2 location = v_coords * size;
    vec4 mix_color;
    vec4 corners = vec4(radius);

    float distance = rounded_box(location - center, size / 2.0, corners);
    float smoothedAlpha = 1.0 - smoothstep(0.0, 1.0, distance);

    mix_color = mix(vec4(0.0, 0.0, 0.0, 0.0), vec4(color, alpha), smoothedAlpha);

    #if defined(DEBUG_FLAGS)
    if (tint == 1.0)
        mix_color = vec4(0.0, 0.3, 0.0, 0.2) + mix_color * 0.8;
    #endif

    gl_FragColor = mix_color;
}
