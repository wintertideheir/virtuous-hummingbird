#include "aikaterine.h"

#include <glib.h>
#include <stdlib.h>

struct Edge {
  int relation;
  int vertex;
};

struct Vertex {
  int region;
  GArray* edges;
  char* idea;
  struct AikaterineVector pos;
};

struct AikaterineGraph {
  GArray* relations;
  GArray* regions;
  GArray* graph;
};

void vertex_destructor(struct Vertex v) {
  g_array_free(v.edges, TRUE);
  free(v.idea);
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
    if (v->region == -1) {
      v->region = vertex.region;
      v->edges = g_array_new(FALSE, FALSE, sizeof(struct Edge));
      v->idea = vertex.idea;
      v->pos = vertex.pos;
      return;
    }
  }
  struct Vertex v = { vertex.region, g_array_new(FALSE, FALSE, sizeof(struct Edge)),
                      vertex.idea, vertex.pos };
  g_array_append_val(ag->graph, v);
}

void aikaterine_remove(AikaterineGraph* ag, int vertex) {
  struct Vertex* v = &g_array_index(ag->graph, struct Vertex, vertex);
  v->region = -1;
  g_array_free(v->edges, TRUE);
  free(v->idea);
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
