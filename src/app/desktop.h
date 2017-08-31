#define GLEW_STATIC
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <aikaterine.h>

static const float minScale = 5;
static const float maxScale = 25;
float scale;

int windowX;
int windowY;

void drawingBegin();
void drawingLoop();
int drawingShouldClose();
void drawingEnd();

void earlyExit();
