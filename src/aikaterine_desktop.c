#define GLEW_STATIC
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <stdio.h>
#include <stdlib.h>

#define return_ glfwTerminate(); return 0;

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
    fprintf(stderr, "OpenGL shader failed to compile (%s)", info);

	  glDeleteShader(shader);
    glfwTerminate();
    exit(EXIT_FAILURE);
  }

  return shader;
};

void framebufferSizeCallback(GLFWwindow *w, int x, int y)
{
  glViewport(0, 0, x, y);
}

int main(int argc, char const *argv[])
{
  if(!glfwInit())
  {
    fprintf(stderr, "GLFW failed to initialize\n");
    return 0;
  }

  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_RESIZABLE, GL_TRUE);

  GLFWwindow* window = glfwCreateWindow(800, 600, "Aikaterine", NULL, NULL);
  if (window == NULL)
  {
    fprintf(stderr, "GLFW failed to create new window\n");
    return_;
  }

  glfwMakeContextCurrent(window);

  glewExperimental = GL_TRUE;
  GLenum glewStatus = glewInit();
  if (glewStatus != GLEW_OK)
  {
    fprintf(stderr, "GLEW failed to initialize (%s)\n",
            glewGetErrorString(glewStatus));
    return_;
  }

  int sizex, sizey;
  glfwGetFramebufferSize(window, &sizex, &sizey);
  glViewport(0, 0, sizex, sizey);
  glfwSetFramebufferSizeCallback(window, &framebufferSizeCallback);

  while (!glfwWindowShouldClose(window)) {
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);
    glfwSwapBuffers(window);
    glfwPollEvents();
  }

  return_;
}
