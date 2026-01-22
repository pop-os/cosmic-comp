#version 100

precision highp float;

varying vec2 v_coords;

uniform sampler2D tex;
uniform vec2 half_pixel;
uniform float offset;

void main() {
    vec4 sum = texture2D(tex, v_coords + vec2(-half_pixel.x * 2.0, 0.0) * offset);
    sum += texture2D(tex, v_coords + vec2(-half_pixel.x, half_pixel.y) * offset) * 2.0;
    sum += texture2D(tex, v_coords + vec2(0.0, half_pixel.y * 2.0) * offset);
    sum += texture2D(tex, v_coords + vec2(half_pixel.x, half_pixel.y) * offset) * 2.0;
    sum += texture2D(tex, v_coords + vec2(half_pixel.x * 2.0, 0.0) * offset);
    sum += texture2D(tex, v_coords + vec2(half_pixel.x, -half_pixel.y) * offset) * 2.0;
    sum += texture2D(tex, v_coords + vec2(0.0, -half_pixel.y * 2.0) * offset);
    sum += texture2D(tex, v_coords + vec2(-half_pixel.x, -half_pixel.y) * offset) * 2.0;

    gl_FragColor = sum / sum.a;
}
