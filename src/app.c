#include "error.h"

#include "draw.h"

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

void errorReport(const char* format, ...)
{
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
}

void errorExit()
{
  drawingEnd();
  exit(EXIT_FAILURE);
}
