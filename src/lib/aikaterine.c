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
  AikaterineGraph* kn = malloc(sizeof(AikaterineGraph));
  kn->relations = g_array_new(FALSE, FALSE, sizeof(char*));
  kn->regions = g_array_new(FALSE, FALSE, sizeof(char*));
  kn->graph = g_array_new(FALSE, FALSE, sizeof(struct Vertex));
  g_array_set_clear_func(kn->graph, (GDestroyNotify) vertex_destructor);
  return kn;
}

void aikaterine_add(AikaterineGraph* kn, struct AikaterineIdea vertex) {
  for (guint i = 0; i < kn->graph->len; i++) {
    struct Vertex* v = &g_array_index(kn->graph, struct Vertex, i);
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
  g_array_append_val(kn->graph, v);
}

void aikaterine_remove(AikaterineGraph* kn, int vertex) {
  struct Vertex* v = &g_array_index(kn->graph, struct Vertex, vertex);
  v->region = -1;
  g_array_free(v->edges, TRUE);
  free(v->idea);
  for (guint x = 0; x < kn->graph->len; x++) {
    GArray* e = g_array_index(kn->graph, struct Vertex, x).edges;
    for (guint y = e->len - 1; y >= 0; y--) {
      struct Edge edge = g_array_index(e, struct Edge, y);
      if (edge.vertex == vertex)
      {
        g_array_remove_index_fast(e, y);
      }
    }
  }
}

void aikaterine_connect(AikaterineGraph* kn, int from, int to, int relation) {
  GArray* edges = g_array_index(kn->graph, struct Vertex, from).edges;
  for (guint i = 0; i < edges->len; i++) {
    struct Edge e = g_array_index(edges, struct Edge, i);
    if (e.vertex == to && e.relation == relation) {
      return;
    }
  }
  struct Edge e = { relation, to };
  g_array_append_val(g_array_index(kn->graph, struct Vertex, from).edges, e);
}

void aikaterine_disconnect(AikaterineGraph* kn, int from, int to, int relation) {
  GArray* edges = g_array_index(kn->graph, struct Vertex, from).edges;
  for (guint i = 0; i < edges->len; i++) {
    struct Edge e = g_array_index(edges, struct Edge, i);
    if (e.vertex == to && e.relation == relation) {
      g_array_remove_index_fast(edges, i);
      return;
    }
  }
}

void aikaterine_free(AikaterineGraph* kn) {
  g_array_free(kn->relations, TRUE);
  g_array_free(kn->regions, TRUE);
  g_array_free(kn->graph, TRUE);
  free(kn);
}
