// Voice Orb Fragment Shader
// Animated plasma-like orb with noise distortion and halo effects

precision highp float;

// Standard uniforms
uniform float alpha;
uniform vec2 size;
varying vec2 v_coords;

// Animation uniforms
uniform float time;           // Animation time in seconds
uniform float scale;          // Current scale of the orb (0.0 to 1.0 for grow animation)
uniform float iLowIntensity;  // Smoothed low-frequency audio intensity (0.0 to 1.0)
uniform float iHighIntensity; // Smoothed high-frequency audio intensity (0.0 to 1.0)
uniform float attached;       // 1.0 if attached to window (burst mode), 0.0 if floating

// Position uniforms (for window attachment)
uniform vec2 target_center;   // Target center position (normalized 0-1)
uniform float morph_progress; // Deprecated - kept for compatibility
uniform float cover_scale;    // Scale factor: how much larger orb is than render area
uniform float window_aspect;  // Window aspect ratio (w/h) when attached
uniform float border_radius;  // Border radius in pixels for rounded corners when attached
uniform float viewport_scale; // UV scale factor for viewport padding (1.0 = no padding)
uniform float thinking;       // Thinking mode progress (0.0 = normal, 1.0 = fully thinking)
// Fade-out opacity, carried as a uniform rather than as the element's alpha so
// the render element itself never has to be rebuilt (see VoiceOrbShader::element).
uniform float opacity;

// Configuration constants
const float innerRadius = 0.47;    // Aperture
const float noiseScale = 0.84;     // Turbulence
const float speed = 1.5;           // Temporal Speed

// Colors — derived from 3 base colors: #ccff1a, #00ccb3, #b8e636
const vec3 color1 = vec3(0.000, 0.800, 0.702);      // Primary pulse: #00ccb3 (0, 204, 179)
const vec3 color2 = vec3(0.722, 0.902, 0.212);      // Secondary flow: #b8e636 (184, 230, 54)
const vec3 color3 = vec3(0.000, 0.400, 0.349);      // Core depth: #006659 (0, 102, 89)
const vec3 haloColor = vec3(0.800, 1.000, 0.102);   // Halo accent: #ccff1a (204, 255, 26)
const vec3 bgColor = vec3(0.949, 0.949, 0.980);     // Background A: #F2F2FA Lavender Gray (242, 242, 250)
const vec3 bgColor2 = vec3(0.851, 0.851, 0.922);    // Background B: #D9D9EB Silver Mist (217, 217, 235)
const vec3 audioColor1 = vec3(0.000, 0.600, 0.502); // Bass impact: #009980 (0, 153, 128)
const vec3 audioColor2 = vec3(0.502, 0.800, 0.102); // Treble glow: #80cc1a (128, 204, 26)

const float PI = 3.14159265359;

// Hash function for noise
vec3 hash33(vec3 p3) {
    p3 = fract(p3 * vec3(0.1031, 0.11369, 0.13787));
    p3 += dot(p3, p3.yxz + 19.19);
    return -1.0 + 2.0 * fract(vec3(p3.x + p3.y, p3.x + p3.z, p3.y + p3.z) * p3.zyx);
}

// 3D Simplex noise
float snoise3(vec3 p) {
    const float K1 = 0.333333333;
    const float K2 = 0.166666667;
    
    vec3 i = floor(p + (p.x + p.y + p.z) * K1);
    vec3 d0 = p - (i - (i.x + i.y + i.z) * K2);
    
    vec3 e = step(vec3(0.0), d0 - d0.yzx);
    vec3 i1 = e * (1.0 - e.zxy);
    vec3 i2 = 1.0 - e.zxy * (1.0 - e);
    
    vec3 d1 = d0 - (i1 - K2);
    vec3 d2 = d0 - (i2 - K1);
    vec3 d3 = d0 - 0.5;
    
    vec4 h = max(0.6 - vec4(dot(d0, d0), dot(d1, d1), dot(d2, d2), dot(d3, d3)), 0.0);
    vec4 n = h * h * h * h * vec4(dot(d0, hash33(i)), dot(d1, hash33(i + i1)), dot(d2, hash33(i + i2)), dot(d3, hash33(i + 1.0)));
    
    return dot(vec4(31.316), n);
}

// Extract alpha from color (for premultiplied alpha)
vec4 extractAlpha(vec3 colorIn) {
    vec4 colorOut;
    float maxValue = min(max(max(colorIn.r, colorIn.g), colorIn.b), 1.0);
    if (maxValue > 1e-5) {
        colorOut.rgb = colorIn.rgb * (1.0 / maxValue);
        colorOut.a = maxValue;
    } else {
        colorOut = vec4(0.0);
    }
    return colorOut;
}

// Light falloff functions
float light1(float intensity, float attenuation, float dist) {
    return intensity / (1.0 + dist * attenuation);
}

float light2(float intensity, float attenuation, float dist) {
    return intensity / (1.0 + dist * dist * attenuation);
}

// Hue shift for color variation
vec3 hueShift(vec3 color, float amount) {
    const vec3 k = vec3(0.57735, 0.57735, 0.57735);
    float cosAngle = cos(amount);
    return color * cosAngle + cross(k, color) * sin(amount) + k * dot(k, color) * (1.0 - cosAngle);
}

// Signed distance function for rounded rectangle
// Returns positive value inside, negative outside
float roundedRectSDF(vec2 pos, vec2 halfSize, float radius) {
    vec2 d = abs(pos) - halfSize + vec2(radius);
    return min(max(d.x, d.y), 0.0) + length(max(d, 0.0)) - radius;
}

void main() {
    // Exact coordinate transformation from original shader
    vec2 fragCoord = v_coords * size;
    
    // When attached (burst mode), we need special UV handling to keep orb circular
    vec2 uv;
    vec2 noiseUV;
    
    if (attached > 0.5) {
        // For attached mode: create a square coordinate system centered on window
        // This ensures the orb stays circular regardless of window aspect ratio
        float maxDim = max(size.x, size.y);
        
        // Center the coordinate system and normalize by the larger dimension
        // This creates a square UV space where the orb will be circular
        uv = (fragCoord * 2.0 - size) / maxDim;
        noiseUV = uv;
        
        // Scale down UV so orb appears cover_scale times larger
        uv /= cover_scale;
    } else {
        // Floating mode: use standard height-based normalization
        uv = (fragCoord * 2.0 - size) / size.y;
        noiseUV = uv;
        // Scale UVs by viewport_scale to keep orb visually the same size
        // when the render element has extra padding for glow/halo
        uv *= viewport_scale;
        noiseUV *= viewport_scale;
    }
    
    float animTime = time * speed;
    
    // Frequency band modulations (directly from smoothed intensity channels)
    float bassPulse = pow(iLowIntensity, 1.2) * 1.2;
    float trebleShimmer = pow(iHighIntensity, 1.5) * 1.5;
    
    float ang = atan(uv.y, uv.x);
    float len = length(uv);
    
    // Orb expansion/de-expansion
    float dynamicRadius = innerRadius + bassPulse * 0.15;
    float dynamicNoiseScale = noiseScale + trebleShimmer * 0.2;
    
    // Add some rotation based on audio
    float rotation = animTime * 0.5 + bassPulse * 0.5;
    vec2 rotatedUv = vec2(
        noiseUV.x * cos(rotation) - noiseUV.y * sin(rotation),
        noiseUV.x * sin(rotation) + noiseUV.y * cos(rotation)
    );
    
    // Noise-based distortion - use rotated noiseUV for consistent visual detail during scaling
    float n0 = snoise3(vec3(rotatedUv * dynamicNoiseScale, animTime * 0.5)) * 0.5 + 0.5;
    float r0 = mix(mix(dynamicRadius, 1.0, 0.4), mix(dynamicRadius, 1.0, 0.6), n0);

    // Thinking mode: apply sine wave deformation directly to the orb edge
    if (thinking > 0.01) {
        float wave = sin(ang * 6.0 + time * 1.5) * 0.5 + 0.5;
        wave += (sin(ang * 10.0 - time * 2.0) * 0.5 + 0.5) * 0.4;
        wave /= 1.4;  // normalize
        r0 += wave * 0.08 * thinking;
    }

    float d0 = distance(uv, r0 / len * uv);
    
    // Glow intensity
    float v0 = light1(1.0 + bassPulse * 4.0, 15.0 - bassPulse * 4.0, d0);
    v0 *= smoothstep(r0 * 1.05, r0, len);
    v0 = clamp(v0, 0.0, 1.0);
    float cl = cos(ang + animTime * 2.0 + (bassPulse + trebleShimmer) * 1.5) * 0.5 + 0.5;
    
    // Rotating light point
    float a = animTime * -1.0 - trebleShimmer * 0.2;
    vec2 pos = vec2(cos(a), sin(a)) * r0;
    float d = distance(uv, pos);
    float v1 = light2(2.0 + bassPulse * 2.0, 5.0, d);
    v1 *= light1(1.0, 50.0 - bassPulse * 15.0, d0);
    v1 = min(v1, 1.0);
    
    // Outer halo ring
    float haloR = r0 * (1.15 + trebleShimmer * 0.08);
    float dHalo = abs(len - haloR);
    float v4 = light1(trebleShimmer * 0.5, 60.0, dHalo);
    v4 *= smoothstep(0.12, 0.0, dHalo);
    
    // Edge masks
    float v2 = smoothstep(1.0 + bassPulse * 0.05, mix(dynamicRadius, 1.0, n0 * 0.5), len);
    float v3 = smoothstep(dynamicRadius, mix(dynamicRadius, 1.0, 0.5), len);
    
    // Clamp the mix factors to prevent color values from overshooting (0.0 to 1.0)
    float bassMix = clamp(bassPulse * 1.2, 0.0, 1.0);
    float trebleMix = clamp(trebleShimmer * 1.2, 0.0, 1.0);

    // Directly mix from the base color to the audio color
    vec3 baseCol1 = mix(color1, audioColor1, bassMix);
    vec3 baseCol2 = mix(color2, audioColor2, max(bassMix, trebleMix));
    vec3 baseCol3 = mix(color3, audioColor1, bassMix * 0.5);

    // Transition the halo directly to the treble color
    vec3 haloColFinal = mix(haloColor, audioColor2, trebleMix);
    
    // Combine colors
    vec3 col = mix(baseCol1, baseCol2, cl);
    col = mix(baseCol3, col, v0);
    col += haloColFinal * v4 * 1.0;
    col *= (1.0 - v4 * 0.2);
    col += baseCol1 * bassPulse * 0.15;
    
    col = col * (1.0 + v1) * v2 * v3;
    col = clamp(col, 0.0, 1.0);
    
    // Extract alpha from the computed color
    vec4 portal = extractAlpha(col);
    
    // Background gradient (only visible inside orb area)
    float bgGrad = clamp(len * 0.5 + uv.y * 0.2, 0.0, 1.0);
    vec3 currentBG = mix(bgColor, bgColor2, bgGrad);
    currentBG += bgColor2 * bassPulse * 0.1;
    
    // Mix portal with background
    vec3 finalRGB = mix(currentBG, portal.rgb, portal.a);
    
    // Create circular mask for the orb
    float orbEdge = r0 * 1.15;
    float orbMask = 1.0 - smoothstep(orbEdge - 0.05, orbEdge + 0.05, len);
    
    // Final alpha (scale is handled by geometry size, not shader)
    float finalAlpha = orbMask * alpha * opacity;
    
    // When attached, apply rounded corner mask
    if (attached > 0.5 && border_radius > 0.0) {
        // Calculate position relative to window bounds (in pixels from center)
        vec2 fragPos = v_coords * size - size * 0.5;
        vec2 halfSize = size * 0.5;
        
        // Calculate rounded rect SDF
        float dist = roundedRectSDF(fragPos, halfSize, border_radius);
        
        // Apply smooth edge mask (inside = positive)
        float cornerMask = 1.0 - smoothstep(-1.0, 1.0, dist);
        finalAlpha *= cornerMask;
    }
    
    // Output with premultiplied alpha
    gl_FragColor = vec4(finalRGB * finalAlpha, finalAlpha);
}
