#include "desktop.h"

#include <stdlib.h>

int main(int argc, char const *argv[])
{
  ag = aikaterine_new();
  aikaterine_add(ag, (struct AikaterineIdea){0, NULL, (struct AikaterineVector){0, 0}});
  drawingBegin();
  while(!drawingShouldClose())
  {
    drawingLoop();
  }
  drawingEnd();
  aikaterine_free(ag);
  return EXIT_SUCCESS;
}

void earlyExit()
{
  drawingEnd();
  aikaterine_free(ag);
  exit(EXIT_FAILURE);
}
