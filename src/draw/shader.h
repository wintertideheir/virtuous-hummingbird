#define GLEW_STATIC
#include <GL/glew.h>

struct ShaderRequest {
  GLenum type;
  GLsizei count;
  const GLchar** code;
};

GLuint createProgram(int shader_req_len, struct ShaderRequest* shader_req);
