#include "error.h"

#include "draw/draw.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

int main(int argc, char const *argv[])
{
  drawingBegin();
  while(!drawingShouldClose())
  {
    drawingLoop();
  }
  drawingEnd();
  return EXIT_SUCCESS;
}

void earlyExit(const char* format, ...)
{
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);

  drawingEnd();

  exit(EXIT_FAILURE);
}
