#include "shapes.h"

#include "shader.h"

GLuint boxProgram;
GLuint textProgram;

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
    const GLchar* textVertexShaderCode =
        "#version 330 core\n"
        "layout (location = 0) in vec3 posReal;\n"
        "layout (location = 1) in vec4 colorIn;\n"
        "layout (location = 2) in vec2 posTexture;\n"
        "out vec4 colorPass;\n"
        "out vec2 texPos;\n"
        "void main()\n"
        "{\n"
        "    gl_Position = vec4(posReal.x, posReal.y, posReal.z, 1.0);\n"
        "    colorPass = colorIn;\n"
        "    texPos = posTexture;\n"
        "}\n";
    const GLchar* textFragmentShaderCode =
        "#version 330 core\n"
        "in vec4 colorPass;\n"
        "in vec2 texPos;\n"
        "out vec4 color;\n"
        "uniform sampler2D bitmap;\n"
        "void main()\n"
        "{\n"
        "   color = vec4(colorPass.r, colorPass.g, colorPass.b,"
        "                colorPass.a * texture(bitmap, texPos));\n"
        "}\n";

    boxProgram = createProgram(2, (struct ShaderRequest[]){
                                       (struct ShaderRequest){.type = GL_VERTEX_SHADER,
                                                              .count = 1,
                                                              .code = (const GLchar*[]){boxVertexShaderCode}},
                                       (struct ShaderRequest){.type = GL_FRAGMENT_SHADER,
                                                              .count = 1,
                                                              .code = (const GLchar*[]){boxFragmentShaderCode}}});
    textProgram = createProgram(2, (struct ShaderRequest[]){
                                        (struct ShaderRequest){.type = GL_VERTEX_SHADER,
                                                               .count = 1,
                                                               .code = (const GLchar*[]){textVertexShaderCode}},
                                        (struct ShaderRequest){.type = GL_FRAGMENT_SHADER,
                                                               .count = 1,
                                                               .code = (const GLchar*[]){textFragmentShaderCode}}});
}

void shapesGenerateBox(float upper_x, float lower_x, float upper_y, float lower_y,
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

void shapesGenerateText(float upper_x, float lower_x, float upper_y, float lower_y,
                        float layer, struct RGBA color,
                        char* bitmap, int bitmap_width, int bitmap_height,
                        unsigned int* TID, unsigned int *VAO, unsigned int *VBO)
{
    float vertices[] =
    {
        upper_x, upper_y, layer, color.red, color.green, color.blue, color.alpha, 1.0, 0.0,
        upper_x, lower_y, layer, color.red, color.green, color.blue, color.alpha, 1.0, 1.0,
        lower_x, lower_y, layer, color.red, color.green, color.blue, color.alpha, 0.0, 1.0,
        lower_x, upper_y, layer, color.red, color.green, color.blue, color.alpha, 0.0, 0.0,
    };

    glGenVertexArrays(1, VAO);
    glGenBuffers(1, VBO);

    glBindVertexArray(*VAO);
    glBindBuffer(GL_ARRAY_BUFFER, *VBO);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 9 * sizeof(float), (void*)(0));
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, 9 * sizeof(float), (void*)(3*sizeof(float)));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 9 * sizeof(float), (void*)(7*sizeof(float)));
    glEnableVertexAttribArray(2);

    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

    glGenTextures(1, TID);
    glBindTexture(GL_TEXTURE_2D, *TID);

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, bitmap_width, bitmap_height,
                 0, GL_RED, GL_UNSIGNED_BYTE, bitmap);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,     GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,     GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
}

void shapesUpdateBox(float upper_x, float lower_x, float upper_y, float lower_y,
                     float layer, struct RGBA color, unsigned int *VBO)
{
    float vertices[] =
    {
        upper_x, upper_y, layer, color.red, color.green, color.blue, color.alpha,
        upper_x, lower_y, layer, color.red, color.green, color.blue, color.alpha,
        lower_x, lower_y, layer, color.red, color.green, color.blue, color.alpha,
        lower_x, upper_y, layer, color.red, color.green, color.blue, color.alpha,
    };

    glNamedBufferSubData(*VBO, 0, sizeof(vertices), vertices);
}

void shapesUpdateText(float upper_x, float lower_x, float upper_y, float lower_y,
                      float layer, struct RGBA color, unsigned int *VBO)
{
    float vertices[] =
    {
        upper_x, upper_y, layer, color.red, color.green, color.blue, color.alpha, 1.0, 0.0,
        upper_x, lower_y, layer, color.red, color.green, color.blue, color.alpha, 1.0, 1.0,
        lower_x, lower_y, layer, color.red, color.green, color.blue, color.alpha, 0.0, 1.0,
        lower_x, upper_y, layer, color.red, color.green, color.blue, color.alpha, 0.0, 0.0,
    };

    glNamedBufferSubData(*VBO, 0, sizeof(vertices), vertices);
}

void shapesDrawBox(unsigned int *VAO)
{
    glUseProgram(boxProgram);
    glBindVertexArray(*VAO);
    glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
}

void shapesDrawText(unsigned int *TID, unsigned int *VAO)
{
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, *TID);
    glUseProgram(textProgram);
    glBindVertexArray(*VAO);
    glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
}
