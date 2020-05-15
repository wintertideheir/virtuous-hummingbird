#include "shader.h"

#include "error.h"

#include <stdlib.h>

GLuint createShader(struct ShaderRequest shader_request)
{
  GLuint shader = glCreateShader(shader_request.type);
  glShaderSource(shader, shader_request.count, shader_request.code, NULL);
  glCompileShader(shader);

  GLint success = 0;
  glGetShaderiv(shader, GL_COMPILE_STATUS, &success);

  if (success == GL_FALSE)
  {
    GLint size = 0;
    glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &size);
    GLchar log[size];
    glGetShaderInfoLog(shader, size, NULL, log);

    errorReport("OpenGL shader failed to compile\n%s", log);
    errorExit();
  }

  return shader;
};

GLuint createProgram(int shader_req_len, struct ShaderRequest* shader_req) {
  GLuint shaders[shader_req_len];
  GLuint shaderProgram = glCreateProgram();

  for (int i = 0; i < shader_req_len; i++) {
    shaders[i] = createShader(shader_req[i]);
    glAttachShader(shaderProgram, shaders[i]);
  }

  glLinkProgram(shaderProgram);

  int success;
  glGetProgramiv(shaderProgram, GL_LINK_STATUS, &success);
  if (!success) {
    GLint size = 0;
    glGetProgramiv(shaderProgram, GL_INFO_LOG_LENGTH, &size);
    GLchar log[size];
    glGetProgramInfoLog(shaderProgram, size, NULL, log);

    errorReport("OpenGL program failed to link\n%s", log);
    errorExit();
  }

  for (int i = 0; i < shader_req_len; i++) {
    glDeleteShader(shaders[i]);
  }

  return shaderProgram;
}
