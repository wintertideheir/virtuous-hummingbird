#pragma once

struct AikaterineVector {
  float x;
  float y;
};

struct AikaterineRectangle {
  struct AikaterineVector lower_left;
  struct AikaterineVector upper_right;
};

struct AikaterineIdea {
  int region;
  char* idea;
  struct AikaterineVector pos;
};

struct AikaterineView {
  int verts_len;
  int* verts;
  int edges_len;
  int (*edges)[3];
};

typedef struct AikaterineGraph AikaterineGraph;

AikaterineGraph* aikaterine_new();
void aikaterine_add(AikaterineGraph* ag, struct AikaterineIdea vertex);
void aikaterine_remove(AikaterineGraph* ag, int vertex);
void aikaterine_connect(AikaterineGraph* ag, int from, int to, int relation);
void aikaterine_disconnect(AikaterineGraph* ag, int from, int to, int relation);
void aikaterine_free(AikaterineGraph* ag);
struct AikaterineView aikaterine_view(AikaterineGraph* ag, struct AikaterineRectangle area);
struct AikaterineIdea* aikaterine_idea(AikaterineGraph* ag, int vertex);
struct AikaterineRectangle aikaterine_boundaries(AikaterineGraph* ag);
