// Fragment shader for the nodes of a graph

#version 330 core

in vec2 frag_mesh; // The node mesh passed from the vertex shader

out vec4 color; // The final color

uniform vec3 color; // The color of the node expressed as red-green-blue values

const float fill_limit = 0.85;   // The maximum distance between the center and where the node is filled with color
const float border_limit = 0.95; // The maximum distance between the center and where the node border ends
const vec3 border_color = vec3(0.0f, 0.0f, 0.0f, 1.0f); // The color of the border (e.g. black)
const vec3 empty_color = vec3(0.0f, 0.0f, 0.0f, 0.0f); // The color of the area beyond the border (e.g. transparent "black")

void main()
{
    mesh_distance = distance(frag_mesh, vec2(0f, 0f)); // Calculate the distance from a mesh point to it's center
    if (mesh_distance < fill_limit)
    {
        color = vec4(color, 1.0f);
    }
    else if (mesh_distance < border_limit)
    {
        color = vec4(border_color, 1.0f);
    }
    else
    {
        color = vec4(empty_color, 1.0f);
    }
}
