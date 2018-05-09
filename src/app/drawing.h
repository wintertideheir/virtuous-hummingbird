#include "desktop.h"

#define GLEW_STATIC
#include <GL/glew.h>
#include <GLFW/glfw3.h>

GLFWwindow* window;

GLuint vertexShaderProgram;
GLuint edgeShaderProgram;

GLint vertex_scaleUniform;
GLint vertex_windowXUniform;
GLint vertex_windowYUniform;
GLint vertex_offsetUniform;
GLint vertex_viewUniform;
GLint edge_rotationUniform;
GLint edge_lengthUniform;
GLint edge_scaleUniform;
GLint edge_windowXUniform;
GLint edge_windowYUniform;
GLint edge_offsetUniform;
GLint edge_viewUniform;

unsigned int vertex_VBO;
unsigned int vertex_VAO;
unsigned int edge_VBO;
unsigned int edge_VAO;

void generateShaders();
