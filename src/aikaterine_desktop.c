#define GLEW_STATIC
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#define return_ glfwTerminate(); return 0;

const float minScale = 5;
const float maxScale = 25;
float scale = 15;

int windowX = 800;
int windowY = 600;

GLuint shaderProgram;

GLint circleTexture;

GLint scaleUniform;
GLint windowXUniform;
GLint windowYUniform;

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
    glfwTerminate();
    exit(EXIT_FAILURE);
  }

  return shader;
};

// Creates a circular texture by the midpoint circle algorithm.
void generateCircleTexture()
{
  glGenTextures(1, &circleTexture);
  glBindTexture(GL_TEXTURE_2D, circleTexture);

  GLsizei maxLength = pow(2, ceil(log(maxScale * 5) / log(2)));

  GLubyte colorOuter[4] = {255, 255, 255, 255};
  GLubyte colorInner[4] = { 15,  15,  15, 255};

  for (int level = 0; (maxLength / pow(2, level)) >= 1; level++) {
    GLsizei length = maxLength / pow(2, level);
    GLubyte texture[length][length][4];
      // 4th element the alpha value. Here it is used for alignment.
    memset(texture, 0, sizeof(texture));

    #define square(x) ((x) * (x))

    int radius = length/2 - 1;
      // The midpoint circle algorithm draws a circle with a diameter of 2r+1
    int x = radius;
    int y = 0;
    int p = square(x - 0.5) + square(y + 1) - square(radius);

    #undef square

    #define draw(a, b, color) \
      texture[radius+a][radius+b][0] = color[0]; \
      texture[radius+a][radius+b][1] = color[1]; \
      texture[radius+a][radius+b][2] = color[2]; \
      texture[radius+a][radius+b][3] = color[3]; \

    while (x >= y)
    {
      if (y != radius) {
        for (int a = -x+1; a < x; a++) {
          draw(a, y, colorInner);
          draw(a, -y, colorInner);
          draw(y, a, colorInner);
          draw(-y, a, colorInner);
        }
      }

      draw(x, y, colorOuter);
      draw(x, -y, colorOuter);
      draw(y, x, colorOuter);
      draw(y, -x, colorOuter);
      draw(-x, y, colorOuter);
      draw(-x, -y, colorOuter);
      draw(-y, x, colorOuter);
      draw(-y, -x, colorOuter);

      y++;
      if (p > 0)
      {
        x--;
        p += 2 * y - 2 * x + 1;
      } else {
        p += 2 * y + 1;
      }
    }

    #undef draw

    glTexImage2D(GL_TEXTURE_2D, level, GL_RGB, length, length, 0, GL_RGBA, GL_UNSIGNED_BYTE, texture);
  }
}

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

  GLFWwindow* window = glfwCreateWindow(windowX, windowY, "Aikaterine", NULL, NULL);
  if (window == NULL)
  {
    fprintf(stderr, "GLFW failed to create new window\n");
    return_;
  }

  glfwSetWindowSizeLimits(window, windowX, windowY, GLFW_DONT_CARE, GLFW_DONT_CARE);
  glfwMakeContextCurrent(window);

  glewExperimental = GL_TRUE;
  GLenum glewStatus = glewInit();
  if (glewStatus != GLEW_OK)
  {
    fprintf(stderr, "GLEW failed to initialize: %s\n",
            glewGetErrorString(glewStatus));
    return_;
  }

  const GLchar* vertexShaderCode =
    "#version 330 core\n"
    "layout (location = 0) in vec2 pos;\n"
    "uniform float scale;\n"
    "uniform int windowX;\n"
    "uniform int windowY;\n"
    "out vec2 texturePos;\n"
    "void main()\n"
    "{\n"
    "    gl_Position = vec4(5 * scale * pos.xy / vec2(windowX, windowY), 0.0, 1.0);\n"
    "    texturePos = pos.xy + 0.5;\n"
    "}\n";

  const GLchar* fragmentShaderCode =
    "#version 330 core\n"
    "out vec4 FragColor;\n"
    "in vec2 texturePos;\n"
    "uniform sampler2D circleTexture;"
    "void main()\n"
    "{\n"
    "    FragColor = texture(circleTexture, texturePos);\n"
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

    glfwTerminate();
    exit(EXIT_FAILURE);
  }

  scaleUniform = glGetUniformLocation(shaderProgram, "scale");
  windowXUniform = glGetUniformLocation(shaderProgram, "windowX");
  windowYUniform = glGetUniformLocation(shaderProgram, "windowY");

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
     0.5f,  0.5f,
     0.5f, -0.5f,
    -0.5f,  0.5f,
     0.5f, -0.5f,
    -0.5f, -0.5f,
    -0.5f,  0.5f,
  };

  unsigned int VBO;
  unsigned int VAO;

  glGenVertexArrays(1, &VAO);
  glGenBuffers(1, &VBO);
  glBindVertexArray(VAO);
  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(float), (void*)0);
  glEnableVertexAttribArray(0);

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);

  generateCircleTexture();

  while (!glfwWindowShouldClose(window)) {
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);

    glBindVertexArray(VAO);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    glfwSwapBuffers(window);
    glfwPollEvents();
  }

  glDeleteVertexArrays(1, &VAO);
  glDeleteBuffers(1, &VBO);

  return_;
}
