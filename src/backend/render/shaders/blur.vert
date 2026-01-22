#version 100

uniform mat3 matrix;
uniform mat3 tex_matrix;

attribute vec2 vert;

varying vec2 v_coords;

void main() {
    vec3 pos = vec3(vert, 1.0);
    v_coords = (tex_matrix * pos).xy;
    gl_Position = vec4(matrix * pos, 1.0);
}
