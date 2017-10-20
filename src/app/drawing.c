#include "drawing.h"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

const float minScale = 5;
const float maxScale = 25;
float scale = 15;

int windowX = 800;
int windowY = 600;

int verticesLength;
struct AikaterineVector* vertices;

void findView() {
  struct AikaterineVector center = {0, 0};
  struct AikaterineVector offset = {windowX / (2.5 * minScale), windowY / (2.5 * minScale)};
  struct AikaterineRectangle area = {center, offset};
  struct AikaterineView av = aikaterine_view(ag, area);
  verticesLength = av.verts_len;
  vertices = malloc(sizeof(struct AikaterineVector) * verticesLength);
  for (int i = 0; i < verticesLength; i++) {
    vertices[i] = aikaterine_idea(ag, av.verts[i])->pos;
  }
}

void framebufferSizeCallback(GLFWwindow *w, int x, int y)
{
  windowX = x;
  windowY = y;
  free(vertices);
  findView();
  glProgramUniform1i(vertexShaderProgram, windowXUniform, x);
  glProgramUniform1i(vertexShaderProgram, windowYUniform, y);
  glViewport(0, 0, x, y);
}

void scrollCallback(GLFWwindow* w, double x, double y)
{
  scale = fmin(fmax(scale+y, minScale), maxScale);
  glProgramUniform1f(vertexShaderProgram, scaleUniform, scale);
}

void drawingBegin()
{
  findView();

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

  generateShaders();

  scaleUniform = glGetUniformLocation(vertexShaderProgram, "scale");
  windowXUniform = glGetUniformLocation(vertexShaderProgram, "windowX");
  windowYUniform = glGetUniformLocation(vertexShaderProgram, "windowY");
  offsetUniform = glGetUniformLocation(vertexShaderProgram, "offset");

  glUseProgram(vertexShaderProgram);
  glProgramUniform1f(vertexShaderProgram, scaleUniform, scale);
  glProgramUniform1i(vertexShaderProgram, windowXUniform, windowX);
  glProgramUniform1i(vertexShaderProgram, windowYUniform, windowY);

  int sizex, sizey;
  glfwGetFramebufferSize(window, &sizex, &sizey);
  glViewport(0, 0, sizex, sizey);
  glfwSetFramebufferSizeCallback(window, &framebufferSizeCallback);
  glfwSetScrollCallback(window, &scrollCallback);

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
  for (int i = 0; i < verticesLength; i++) {
    glProgramUniform2f(vertexShaderProgram, offsetUniform, vertices[i].x, vertices[i].y);
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
  free(vertices);

  glDeleteVertexArrays(1, &VAO);
  glDeleteBuffers(1, &VBO);

  glfwTerminate();
}
