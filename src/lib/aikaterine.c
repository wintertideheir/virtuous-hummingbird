#include "aikaterine.h"

#include <glib.h>
#include <stdlib.h>

struct Edge {
  int relation;
  int vertex;
};

struct Vertex {
  struct AikaterineIdea wrapper;
  GArray* edges;
};

struct AikaterineGraph {
  GArray* relations;
  GArray* regions;
  GArray* graph;
};

const int ARRAY_PREALLOCATION = 8;

void vertex_destructor(struct Vertex* v) {
  g_array_free(v->edges, TRUE);
  free(v->wrapper.idea);
}

AikaterineGraph* aikaterine_new() {
  AikaterineGraph* ag = malloc(sizeof(AikaterineGraph));
  ag->relations = g_array_new(FALSE, FALSE, sizeof(char*));
  ag->regions = g_array_new(FALSE, FALSE, sizeof(char*));
  ag->graph = g_array_new(FALSE, FALSE, sizeof(struct Vertex));
  g_array_set_clear_func(ag->graph, (GDestroyNotify) vertex_destructor);
  return ag;
}

void aikaterine_add(AikaterineGraph* ag, struct AikaterineIdea vertex) {
  for (guint i = 0; i < ag->graph->len; i++) {
    struct Vertex* v = &g_array_index(ag->graph, struct Vertex, i);
    if (v->wrapper.region == -1) {
      v->wrapper = vertex;
      v->edges = g_array_new(FALSE, FALSE, sizeof(struct Edge));
      return;
    }
  }
  struct Vertex v = { vertex, g_array_new(FALSE, FALSE, sizeof(struct Edge)) };
  g_array_append_val(ag->graph, v);
}

void aikaterine_remove(AikaterineGraph* ag, int vertex) {
  struct Vertex* v = &g_array_index(ag->graph, struct Vertex, vertex);
  v->wrapper.region = -1;
  g_array_free(v->edges, TRUE);
  free(v->wrapper.idea);
  for (guint x = 0; x < ag->graph->len; x++) {
    GArray* e = g_array_index(ag->graph, struct Vertex, x).edges;
    for (guint y = e->len - 1; y >= 0; y--) {
      struct Edge edge = g_array_index(e, struct Edge, y);
      if (edge.vertex == vertex)
      {
        g_array_remove_index_fast(e, y);
      }
    }
  }
}

void aikaterine_connect(AikaterineGraph* ag, int from, int to, int relation) {
  GArray* edges = g_array_index(ag->graph, struct Vertex, from).edges;
  for (guint i = 0; i < edges->len; i++) {
    struct Edge e = g_array_index(edges, struct Edge, i);
    if (e.vertex == to && e.relation == relation) {
      return;
    }
  }
  struct Edge e = { relation, to };
  g_array_append_val(g_array_index(ag->graph, struct Vertex, from).edges, e);
}

void aikaterine_disconnect(AikaterineGraph* ag, int from, int to, int relation) {
  GArray* edges = g_array_index(ag->graph, struct Vertex, from).edges;
  for (guint i = 0; i < edges->len; i++) {
    struct Edge e = g_array_index(edges, struct Edge, i);
    if (e.vertex == to && e.relation == relation) {
      g_array_remove_index_fast(edges, i);
      return;
    }
  }
}

void aikaterine_free(AikaterineGraph* ag) {
  g_array_free(ag->relations, TRUE);
  g_array_free(ag->regions, TRUE);
  g_array_free(ag->graph, TRUE);
  free(ag);
}

int* aikaterine_view(AikaterineGraph* ag, struct AikaterineRectangle area) {
  int len = 0;
  int pre = ARRAY_PREALLOCATION;
  int* vertices = malloc(1 + pre);
  for (guint i = 0; i < ag->graph->len; i++) {
    struct AikaterineVector pos = g_array_index(ag->graph, struct Vertex, i).wrapper.pos;
    if (pos.x <= (area.center.x + area.offset.x) && pos.x >= (area.center.x - area.offset.x) &&
        pos.y <= (area.center.y + area.offset.y) && pos.y >= (area.center.y - area.offset.y)) {
      len++;
      if (pre < 1) {
        pre = ARRAY_PREALLOCATION - 1;
        vertices = realloc(vertices, 1 + len + ARRAY_PREALLOCATION);
      }
      vertices[len] = i;
    }
  }
  vertices[0] = len;
  return realloc(vertices, 1 + len);
}

struct AikaterineIdea* aikaterine_idea(AikaterineGraph* ag, int vertex) {
  return &g_array_index(ag->graph, struct Vertex, vertex).wrapper;
}
