// Vertex shader for the nodes of a graph

#version 330 core

layout (location = 0) in vec2 mesh;

out vec2 frag_mesh; // Pass the mesh to the fragment shader

uniform vec2 scale;  // The scale factor for the x- and y-coordinates
uniform vec2 offset; // The x- and y-offset of the mesh relative to the center of the drawing area

void main()
{
    vec2 pos = (mesh * scale) + offset;
    gl_Position = vec4(pos.x, pos.y, 0.0, 1.0);
}
