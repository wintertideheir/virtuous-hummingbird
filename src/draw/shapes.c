#include "shapes.h"

#include "shader.h"

GLuint boxProgram;

void shapesBegin()
{
    const GLchar* boxVertexShaderCode =
        "#version 330 core\n"
        "layout (location = 0) in vec3 pos;\n"
        "layout (location = 1) in vec4 color;\n"
        "out vec4 rgba;\n"
        "void main()\n"
        "{\n"
        "    gl_Position = vec4(pos.x, pos.y, pos.z, 1.0);\n"
        "    rgba = color;\n"
        "}\n";
    const GLchar* boxFragmentShaderCode =
        "#version 330 core\n"
        "out vec4 color;\n"
        "in vec4 rgba;\n"
        "void main()\n"
        "{\n"
        "   color = rgba;\n"
        "}\n";

    boxProgram = createProgram(2, (struct ShaderRequest[]){
                                       (struct ShaderRequest){.type = GL_VERTEX_SHADER,
                                                              .count = 1,
                                                              .code = (const GLchar*[]){boxVertexShaderCode}},
                                       (struct ShaderRequest){.type = GL_FRAGMENT_SHADER,
                                                              .count = 1,
                                                              .code = (const GLchar*[]){boxFragmentShaderCode}}});
}

void shapesGenerateBox(float lower_x, float upper_x, float lower_y, float upper_y,
                       float layer, struct RGBA color,
                       unsigned int *VAO, unsigned int *VBO)
{
    float vertices[] =
    {
        upper_x, upper_y, layer, color.red, color.green, color.blue, color.alpha,
        upper_x, lower_y, layer, color.red, color.green, color.blue, color.alpha,
        lower_x, lower_y, layer, color.red, color.green, color.blue, color.alpha,
        lower_x, upper_y, layer, color.red, color.green, color.blue, color.alpha,
    };

    glGenVertexArrays(1, VAO);
    glGenBuffers(1, VBO);

    glBindVertexArray(*VAO);
    glBindBuffer(GL_ARRAY_BUFFER, *VBO);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 7 * sizeof(float), (void*)0);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, 7 * sizeof(float), (void*)(3*sizeof(float)));
    glEnableVertexAttribArray(1);
}

void shapesDrawBox(unsigned int *VAO)
{
    glUseProgram(boxProgram);
    glBindVertexArray(*VAO);
    glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
}
