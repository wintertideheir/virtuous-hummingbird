#define GLEW_STATIC
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <aikaterine.h>

AikaterineGraph* ag;

void drawingBegin();
void drawingLoop();
int drawingShouldClose();
void drawingEnd();

void earlyExit();