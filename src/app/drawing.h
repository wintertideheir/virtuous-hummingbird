#include "desktop.h"

#define GLEW_STATIC
#include <GL/glew.h>
#include <GLFW/glfw3.h>

GLFWwindow* window;

GLuint vertexShaderProgram;

GLint scaleUniform;
GLint windowXUniform;
GLint windowYUniform;
GLint offsetUniform;

unsigned int VBO;
unsigned int VAO;

void generateShaders();
