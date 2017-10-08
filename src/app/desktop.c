#include "desktop.h"

#include <stdlib.h>

int main(int argc, char const *argv[])
{
  ag = aikaterine_new();
  aikaterine_add(ag, (struct AikaterineIdea){0, NULL, (struct AikaterineVector){0, 0}});
  aikaterine_add(ag, (struct AikaterineIdea){0, NULL, (struct AikaterineVector){12, 6}});
  aikaterine_add(ag, (struct AikaterineIdea){0, NULL, (struct AikaterineVector){-5, -9}});
  aikaterine_add(ag, (struct AikaterineIdea){0, NULL, (struct AikaterineVector){21, -8}});
  aikaterine_add(ag, (struct AikaterineIdea){0, NULL, (struct AikaterineVector){-15, 4}});
  aikaterine_add(ag, (struct AikaterineIdea){0, NULL, (struct AikaterineVector){32, -24}});
  aikaterine_add(ag, (struct AikaterineIdea){0, NULL, (struct AikaterineVector){-29, 15}});
  aikaterine_add(ag, (struct AikaterineIdea){0, NULL, (struct AikaterineVector){24, 17}});
  aikaterine_add(ag, (struct AikaterineIdea){0, NULL, (struct AikaterineVector){8, -32}});
  aikaterine_add(ag, (struct AikaterineIdea){0, NULL, (struct AikaterineVector){-16, 31}});
  aikaterine_connect(ag, 0, 1, 0);
  aikaterine_connect(ag, 1, 5, 0);
  aikaterine_connect(ag, 6, 3, 0);
  aikaterine_connect(ag, 4, 3, 0);
  aikaterine_connect(ag, 9, 6, 0);
  aikaterine_connect(ag, 2, 7, 0);
  aikaterine_connect(ag, 2, 8, 0);
  aikaterine_connect(ag, 7, 6, 0);
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
