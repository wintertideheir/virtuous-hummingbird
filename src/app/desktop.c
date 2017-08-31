#include "desktop.h"

#include <stdlib.h>

int main(int argc, char const *argv[])
{
  scale = 15;

  windowX = 800;
  windowY = 600;

  drawingBegin();
  while(!drawingShouldClose())
  {
    drawingLoop();
  }
  drawingEnd();
  return EXIT_SUCCESS;
}

void earlyExit()
{
  drawingEnd();
  exit(EXIT_FAILURE);
}
