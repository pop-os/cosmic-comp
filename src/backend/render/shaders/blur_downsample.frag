#version 100

precision highp float;

varying vec2 v_coords;

uniform sampler2D tex;
uniform vec2 half_pixel;
uniform float offset;

void main() {
    vec4 sum = texture2D(tex, v_coords) * 4.0;
    sum += texture2D(tex, v_coords - half_pixel * offset);
    sum += texture2D(tex, v_coords + half_pixel * offset);
    sum += texture2D(tex, v_coords + vec2(half_pixel.x, -half_pixel.y) * offset);
    sum += texture2D(tex, v_coords - vec2(half_pixel.x, -half_pixel.y) * offset);

    gl_FragColor = sum / sum.a;
}
