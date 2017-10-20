#include "drawing.h"

#include <stdio.h>

GLuint createShader(GLenum type, GLsizei number,
                    const GLchar **code, const GLint *length)
{
  GLuint shader = glCreateShader(type);
  glShaderSource(shader, number, code, length);
  glCompileShader(shader);

  GLint success = 0;
  glGetShaderiv(shader, GL_COMPILE_STATUS, &success);

  if (success == GL_FALSE)
  {
    GLint size = 0;
    glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &size);
    GLchar info[size];
    glGetShaderInfoLog(shader, size, NULL, info);
    fprintf(stderr, "OpenGL shader failed to compile\n%s", info);

	  glDeleteShader(shader);
    earlyExit();
  }

  return shader;
};

GLuint createProgram(const GLchar* vertexShaderCode,
                     const GLchar* fragmentShaderCode) {
  GLuint vertexShader = createShader(GL_VERTEX_SHADER, 1, &vertexShaderCode, NULL);
  GLuint fragmentShader = createShader(GL_FRAGMENT_SHADER, 1, &fragmentShaderCode, NULL);

  GLuint shaderProgram = glCreateProgram();

  glAttachShader(shaderProgram, vertexShader);
  glAttachShader(shaderProgram, fragmentShader);
  glLinkProgram(shaderProgram);

  int success;
  glGetProgramiv(shaderProgram, GL_LINK_STATUS, &success);
  if (!success) {
    GLint size = 0;
    glGetProgramiv(shaderProgram, GL_INFO_LOG_LENGTH, &size);
    GLchar infoLog[size];
    glGetProgramInfoLog(shaderProgram, size, NULL, infoLog);
    fprintf(stderr, "OpenGL program failed to link\n%s", infoLog);

    earlyExit();
  }

  glDeleteShader(vertexShader);
  glDeleteShader(fragmentShader);

  return shaderProgram;
}

void generateShaders()
{
  const GLchar* vertexShaderCode =
  "#version 330 core\n"
  "layout (location = 0) in vec2 pos;\n"
  "out vec2 normalizedPos;\n"
  "uniform vec2 offset;\n"
  "uniform float scale;\n"
  "uniform int windowX;\n"
  "uniform int windowY;\n"
  "void main()\n"
  "{\n"
  "    gl_Position = vec4(2.5 * scale * (pos.xy + offset) / vec2(windowX, windowY), 0.0, 1.0);\n"
  "    normalizedPos = pos;\n"
  "}\n";

  const GLchar* fragmentShaderCode =
  "#version 330 core\n"
  "in vec2 normalizedPos;\n"
  "out vec4 color;\n"
  "float d = distance(normalizedPos, vec2(0.0));\n"
  "float radius = 0.9;\n"
  "float delta = fwidth(d);\n"
  "float innerFactor = 15.0/255.0;\n"
  "void main()\n"
  "{\n"
  "    color = vec4(vec3(1.0), (1 - smoothstep(radius, radius + delta, d))\n"
  "            - ((1 - innerFactor) * (1 - smoothstep(radius - delta, radius, d))));\n"
  "}\n";

  vertexShaderProgram = createProgram(vertexShaderCode, fragmentShaderCode);
}
