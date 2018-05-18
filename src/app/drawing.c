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

const float viewf = 0.05;
float viewX = 0;
float viewY = 0;

struct Line {
  struct AikaterineVector location;
  float rotation;
  float length;
};

int verticesLength;
struct AikaterineVector* vertices;
int edgesLength;
struct Line* edges;

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
  edgesLength = av.edges_len;
  edges = malloc(sizeof(struct Line) * edgesLength);
  for (int i = 0; i < edgesLength; i++) {
    struct AikaterineVector pos1 = aikaterine_idea(ag, av.edges[i][0])->pos;
    struct AikaterineVector pos2 = aikaterine_idea(ag, av.edges[i][1])->pos;
    edges[i].location.x = (pos1.x + pos2.x) / 2;
    edges[i].location.y = (pos1.y + pos2.y) / 2;
    edges[i].rotation = atan2f(pos1.y - pos2.y, pos1.x - pos2.x);
    edges[i].length = sqrt(powf(pos1.x - pos2.x, 2) + powf(pos1.y - pos2.y, 2)) / 2;
      /* The length is divided by two because the edge mesh conforms to the
         boundaries of a two by two square. */
  }
}

void framebufferSizeCallback(GLFWwindow *w, int x, int y)
{
  windowX = x;
  windowY = y;
  free(vertices);
  findView();
  glProgramUniform1i(vertexShaderProgram, vertex_windowXUniform, x);
  glProgramUniform1i(vertexShaderProgram, vertex_windowYUniform, y);
  glProgramUniform1i(edgeShaderProgram, edge_windowXUniform, x);
  glProgramUniform1i(edgeShaderProgram, edge_windowYUniform, y);
  glViewport(0, 0, x, y);
}

void scrollCallback(GLFWwindow* w, double x, double y)
{
  scale = fmin(fmax(scale+y, minScale), maxScale);
  glProgramUniform1f(vertexShaderProgram, vertex_scaleUniform, scale);
  glProgramUniform1f(edgeShaderProgram, edge_scaleUniform, scale);
}

void cursorPosCallback(GLFWwindow* window, double x, double y)
{
  viewX = viewf * -x;
  viewY = viewf * y;
  glProgramUniform2f(vertexShaderProgram, vertex_viewUniform, viewX, viewY);
  glProgramUniform2f(edgeShaderProgram, edge_viewUniform, viewX, viewY);
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

  vertex_scaleUniform = glGetUniformLocation(vertexShaderProgram, "scale");
  vertex_windowXUniform = glGetUniformLocation(vertexShaderProgram, "windowX");
  vertex_windowYUniform = glGetUniformLocation(vertexShaderProgram, "windowY");
  vertex_offsetUniform = glGetUniformLocation(vertexShaderProgram, "offset");
  vertex_viewUniform = glGetUniformLocation(vertexShaderProgram, "view");
  edge_scaleUniform = glGetUniformLocation(edgeShaderProgram, "scale");
  edge_windowXUniform = glGetUniformLocation(edgeShaderProgram, "windowX");
  edge_windowYUniform = glGetUniformLocation(edgeShaderProgram, "windowY");
  edge_offsetUniform = glGetUniformLocation(edgeShaderProgram, "offset");
  edge_rotationUniform = glGetUniformLocation(edgeShaderProgram, "rotation");
  edge_lengthUniform = glGetUniformLocation(edgeShaderProgram, "length");
  edge_viewUniform = glGetUniformLocation(edgeShaderProgram, "view");

  glProgramUniform1f(vertexShaderProgram, vertex_scaleUniform, scale);
  glProgramUniform1i(vertexShaderProgram, vertex_windowXUniform, windowX);
  glProgramUniform1i(vertexShaderProgram, vertex_windowYUniform, windowY);
  glProgramUniform2f(vertexShaderProgram, vertex_viewUniform, viewX, viewY);
  glProgramUniform1f(edgeShaderProgram, edge_scaleUniform, scale);
  glProgramUniform1i(edgeShaderProgram, edge_windowXUniform, windowX);
  glProgramUniform1i(edgeShaderProgram, edge_windowYUniform, windowY);
  glProgramUniform2f(edgeShaderProgram, edge_viewUniform, viewX, viewY);

  int sizex, sizey;
  glfwGetFramebufferSize(window, &sizex, &sizey);
  glViewport(0, 0, sizex, sizey);
  glfwSetFramebufferSizeCallback(window, &framebufferSizeCallback);
  glfwSetScrollCallback(window, &scrollCallback);
  glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED);
  glfwSetCursorPosCallback(window, &cursorPosCallback);

  float vertex[] = {
     1.0f,  1.0f,
     1.0f, -1.0f,
    -1.0f,  1.0f,
     1.0f, -1.0f,
    -1.0f, -1.0f,
    -1.0f,  1.0f,
  };

  float edge[] = {
     -1.0f, -0.125f,
     -1.0f, 0.125f,
     1.0f,  0.025f,
     -1.0f, -0.1f,
     1.0f, 0.025f,
     1.0f,  -0.025f,
  };

  glGenVertexArrays(1, &vertex_VAO);
  glGenBuffers(1, &vertex_VBO);
  glBindVertexArray(vertex_VAO);
  glBindBuffer(GL_ARRAY_BUFFER, vertex_VBO);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertex), vertex, GL_STATIC_DRAW);

  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(float), (void*)0);
  glEnableVertexAttribArray(0);

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);

  glGenVertexArrays(1, &edge_VAO);
  glGenBuffers(1, &edge_VBO);
  glBindVertexArray(edge_VAO);
  glBindBuffer(GL_ARRAY_BUFFER, edge_VBO);
  glBufferData(GL_ARRAY_BUFFER, sizeof(edge), edge, GL_STATIC_DRAW);

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

  glUseProgram(edgeShaderProgram);
  glBindVertexArray(edge_VAO);
  for (int i = 0; i < edgesLength; i++) {
    glProgramUniform2f(edgeShaderProgram, edge_offsetUniform,
                       edges[i].location.x, edges[i].location.y);
    glProgramUniform1f(edgeShaderProgram, edge_rotationUniform, edges[i].rotation);
    glProgramUniform1f(edgeShaderProgram, edge_lengthUniform, edges[i].length);
    glDrawArrays(GL_TRIANGLES, 0, 6);
  }

  glUseProgram(vertexShaderProgram);
  glBindVertexArray(vertex_VAO);
  for (int i = 0; i < verticesLength; i++) {
    glProgramUniform2f(vertexShaderProgram, vertex_offsetUniform,
                       vertices[i].x, vertices[i].y);
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
  free(edges);

  glDeleteVertexArrays(1, &vertex_VAO);
  glDeleteBuffers(1, &vertex_VBO);
  glDeleteVertexArrays(1, &edge_VAO);
  glDeleteBuffers(1, &edge_VBO);

  glfwTerminate();
}
