#include "desktop.h"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

const float minScale = 5;
const float maxScale = 25;
float scale = 15;

int windowX = 800;
int windowY = 600;

GLFWwindow* window;

GLuint shaderProgram;

GLint scaleUniform;
GLint windowXUniform;
GLint windowYUniform;
GLint offsetUniform;

unsigned int VBO;
unsigned int VAO;

int visibleLength;
struct AikaterineVector* visible;

void visibleFind() {
  struct AikaterineVector center = {0, 0};
  struct AikaterineVector offset = {windowX / (2.5 * minScale), windowY / (2.5 * minScale)};
  struct AikaterineRectangle area = {center, offset};
  int* visibleVertices = aikaterine_view(ag, area);
  visibleLength = visibleVertices[0];
  visible = malloc(sizeof(struct AikaterineVector) * visibleLength);
  for (int i = 0; i < visibleLength; i++) {
    visible[i] = aikaterine_idea(ag, visibleVertices[1+i])->pos;
  }
}

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

void framebufferSizeCallback(GLFWwindow *w, int x, int y)
{
  windowX = x;
  windowY = y;
  glProgramUniform1i(shaderProgram, windowXUniform, x);
  glProgramUniform1i(shaderProgram, windowYUniform, y);
  glViewport(0, 0, x, y);
}

void scrollCallback(GLFWwindow* w, double x, double y)
{
  scale = fmin(fmax(scale+y, minScale), maxScale);
  glProgramUniform1f(shaderProgram, scaleUniform, scale);
}

void drawingBegin()
{
  visibleFind();

  if(!glfwInit())
  {
    fprintf(stderr, "GLFW failed to initialize\n");
    earlyExit();
  }

  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_RESIZABLE, GL_TRUE);

  window = glfwCreateWindow(windowX, windowY, "Aikaterine", NULL, NULL);
  if (window == NULL)
  {
    fprintf(stderr, "GLFW failed to create new window\n");
    earlyExit();
  }

  glfwSetWindowSizeLimits(window, windowX, windowY, GLFW_DONT_CARE, GLFW_DONT_CARE);
  glfwMakeContextCurrent(window);

  glewExperimental = GL_TRUE;
  GLenum glewStatus = glewInit();
  if (glewStatus != GLEW_OK)
  {
    fprintf(stderr, "GLEW failed to initialize: %s\n",
            glewGetErrorString(glewStatus));
    earlyExit();
  }

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

  GLuint vertexShader = createShader(GL_VERTEX_SHADER, 1, &vertexShaderCode, NULL);
  GLuint fragmentShader = createShader(GL_FRAGMENT_SHADER, 1, &fragmentShaderCode, NULL);

  shaderProgram = glCreateProgram();

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

  scaleUniform = glGetUniformLocation(shaderProgram, "scale");
  windowXUniform = glGetUniformLocation(shaderProgram, "windowX");
  windowYUniform = glGetUniformLocation(shaderProgram, "windowY");
  offsetUniform = glGetUniformLocation(shaderProgram, "offset");

  glUseProgram(shaderProgram);
  glProgramUniform1f(shaderProgram, scaleUniform, scale);
  glProgramUniform1i(shaderProgram, windowXUniform, windowX);
  glProgramUniform1i(shaderProgram, windowYUniform, windowY);

  int sizex, sizey;
  glfwGetFramebufferSize(window, &sizex, &sizey);
  glViewport(0, 0, sizex, sizey);
  glfwSetFramebufferSizeCallback(window, &framebufferSizeCallback);
  glfwSetScrollCallback(window, &scrollCallback);

  glDeleteShader(vertexShader);
  glDeleteShader(fragmentShader);

  float vertices[] = {
     1.0f,  1.0f,
     1.0f, -1.0f,
    -1.0f,  1.0f,
     1.0f, -1.0f,
    -1.0f, -1.0f,
    -1.0f,  1.0f,
  };

  glGenVertexArrays(1, &VAO);
  glGenBuffers(1, &VBO);
  glBindVertexArray(VAO);
  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(float), (void*)0);
  glEnableVertexAttribArray(0);

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
}

void drawingLoop()
{
  glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
  glClear(GL_COLOR_BUFFER_BIT);

  glBindVertexArray(VAO);
  for (int i = 0; i < visibleLength; i++) {
    glProgramUniform2f(shaderProgram, offsetUniform, visible[i].x, visible[i].y);
    glDrawArrays(GL_TRIANGLES, 0, 6);
  }

  glfwSwapBuffers(window);
  glfwPollEvents();
}

int drawingShouldClose() {
  return glfwWindowShouldClose(window);
}

void drawingEnd()
{
  free(visible);

  glDeleteVertexArrays(1, &VAO);
  glDeleteBuffers(1, &VBO);

  glfwTerminate();
}
