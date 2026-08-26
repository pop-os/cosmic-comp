#version 100

//_DEFINES_

precision highp float;

varying vec2 v_coords;

uniform sampler2D tex;
uniform vec2 half_pixel;
uniform float offset;

uniform float alpha;
#if defined(DEBUG_FLAGS)
uniform float tint;
#endif

void main() {
    vec4 sum = texture2D(tex, v_coords) * 4.0;
    sum += texture2D(tex, v_coords - half_pixel * offset);
    sum += texture2D(tex, v_coords + half_pixel * offset);
    sum += texture2D(tex, v_coords + vec2(half_pixel.x, -half_pixel.y) * offset);
    sum += texture2D(tex, v_coords - vec2(half_pixel.x, -half_pixel.y) * offset);

    if (sum.a == 0.0) {
        gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);
    } else {
        gl_FragColor = sum / sum.a;
    }
}
