#pragma once

struct AikaterineVector {
  float x;
  float y;
};

struct AikaterineRectangle {
  struct AikaterineVector center;
  struct AikaterineVector offset;
    // The offset's attributes should never be less than zero.
};

struct AikaterineIdea {
  int region;
  char* idea;
  struct AikaterineVector pos;
};

typedef struct AikaterineGraph AikaterineGraph;

AikaterineGraph* aikaterine_new();
void aikaterine_add(AikaterineGraph* ag, struct AikaterineIdea vertex);
void aikaterine_remove(AikaterineGraph* ag, int vertex);
void aikaterine_connect(AikaterineGraph* ag, int from, int to, int relation);
void aikaterine_disconnect(AikaterineGraph* ag, int from, int to, int relation);
void aikaterine_free(AikaterineGraph* ag);
/* aikaterine_view returns an array of all vertices in an area. The first
  element of the array is the number of vertices. */
int* aikaterine_view(AikaterineGraph* ag, struct AikaterineRectangle area);
struct AikaterineIdea* aikaterine_idea(AikaterineGraph* ag, int vertex);
