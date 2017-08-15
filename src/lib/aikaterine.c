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
  struct Vector pos;
};

struct KnowledgeGraph {
  GArray* relations;
  GArray* regions;
  GArray* graph;
};

void vertex_destructor(struct Vertex v) {
  g_array_free(v.edges, TRUE);
  free(v.idea);
}

KnowledgeGraph* knowledge_graph_new() {
  KnowledgeGraph* kn = malloc(sizeof(KnowledgeGraph));
  kn->relations = g_array_new(FALSE, FALSE, sizeof(char*));
  kn->regions = g_array_new(FALSE, FALSE, sizeof(char*));
  kn->graph = g_array_new(FALSE, FALSE, sizeof(struct Vertex));
  g_array_set_clear_func(kn->graph, (GDestroyNotify) vertex_destructor);
  return kn;
}

void knowledge_graph_add(KnowledgeGraph* kn, int region, char* idea, struct Vector pos) {
  for (guint i = 0; i < kn->graph->len; i++) {
    struct Vertex* v = &g_array_index(kn->graph, struct Vertex, i);
    if (v->region == -1) {
      v->region = region;
      v->edges = g_array_new(FALSE, FALSE, sizeof(struct Edge));
      v->idea = idea;
      v->pos = pos;
      return;
    }
  }
  struct Vertex v = { region, g_array_new(FALSE, FALSE, sizeof(struct Edge)), idea, pos };
  g_array_append_val(kn->graph, v);
}

void knowledge_graph_remove(KnowledgeGraph* kn, int vertex) {
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

void knowledge_graph_connect(KnowledgeGraph* kn, int from, int to, int relation) {
  struct Edge e = { relation, to };
  g_array_append_val(g_array_index(kn->graph, struct Vertex, from).edges, e);
}

void knowledge_graph_disconnect(KnowledgeGraph* kn, int from, int to, int relation) {
  GArray* edges = g_array_index(kn->graph, struct Vertex, from).edges;
  for (guint i = 0; i < edges->len; i++) {
    struct Edge e = g_array_index(edges, struct Edge, i);
    if (e.vertex == to && e.relation == relation) {
      g_array_remove_index_fast(edges, i);
      return;
    }
  }
}

void knowledge_graph_free(KnowledgeGraph* kn) {
  g_array_free(kn->relations, TRUE);
  g_array_free(kn->regions, TRUE);
  g_array_free(kn->graph, TRUE);
  free(kn);
}
