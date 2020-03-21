#include "draw.h"

#include "shapes.h"
#include "window.h"

#include "app/error.h"
#include "ui/ui.h"

#include <stdlib.h>
#include <math.h>
#include <string.h>

struct UIElement *rootUIElement;

void framebufferSizeCallback(GLFWwindow *w, int x, int y)
{
  windowX = x;
  windowY = y;

  glViewport(0, 0, x, y);

  uielement_update(rootUIElement);
}

void errorCallbackGLFW(int error_code, const char* description)
{
  errorReport("GFLW Error Code:            %i\n", error_code);
  errorReport("GFLW Error Description:     %s\n", description);
}

void drawingBegin()
{
  windowX = 800;
  windowY = 600;

  glfwSetErrorCallback(*errorCallbackGLFW);

  if(!glfwInit())
  {
    errorReport("GLFW failed to initialize\n");
    errorExit();
  }

  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_RESIZABLE, GL_TRUE);

  window = glfwCreateWindow(windowX, windowY, "Aikaterine", NULL, NULL);
  if (window == NULL)
  {
    errorReport("GLFW failed to create new window\n");
    errorExit();
  }

  glfwSetWindowSizeLimits(window, windowX, windowY, GLFW_DONT_CARE, GLFW_DONT_CARE);
  glfwMakeContextCurrent(window);

  glewExperimental = GL_TRUE;
  GLenum glewStatus = glewInit();
  if (glewStatus != GLEW_OK)
  {
    errorReport("GLEW failed to initialize: %s\n", glewGetErrorString(glewStatus));
    errorExit();
  }

  glfwSetFramebufferSizeCallback(window, &framebufferSizeCallback);

  shapesBegin();

  rootUIElement = uielement_box((struct RGBA){0.75, 1.0, 0.75, 1.0},
                                uielement_fixed(uielement_box((struct RGBA){0.5, 1.0, 0.5, 1.0},
                                                uielement_vertical(3, uielement_border(uielement_box((struct RGBA){0.25, 1.0, 0.25, 1.0}, NULL), 15, 15), 1,
                                                                      uielement_border(uielement_box((struct RGBA){0.25, 1.0, 0.25, 1.0}, NULL), 15,  5), 1,
                                                                      uielement_border(uielement_box((struct RGBA){0.25, 1.0, 0.25, 1.0}, NULL), 15, 15), 1)),
                                                500, 300));
  uielement_generate(rootUIElement);
}

void drawingLoop()
{
  glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
  glClear(GL_COLOR_BUFFER_BIT);

  uielement_draw(rootUIElement);

  glfwSwapBuffers(window);
  glfwPollEvents();
}

int drawingShouldClose() {
  return glfwWindowShouldClose(window);
}

void drawingEnd()
{
  glfwTerminate();
}
