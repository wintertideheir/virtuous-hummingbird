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
  const char* idea;
};

struct KnowledgeGraph {
  GArray* relations;
  GArray* regions;
  GArray* graph;
};

KnowledgeGraph* knowledge_graph_new() {
  KnowledgeGraph* kn = malloc(sizeof(KnowledgeGraph));
  kn->relations = g_array_new(FALSE, FALSE, sizeof(const char*));
  kn->regions = g_array_new(FALSE, FALSE, sizeof(const char*));
  kn->graph = g_array_new(FALSE, FALSE, sizeof(struct Vertex));
}

void knowledge_graph_add(KnowledgeGraph* kn, int region, const char* idea) {
  struct Vertex v = { region, g_array_new(FALSE, FALSE, sizeof(struct Edge)), idea };
  g_array_append_val(kn->graph, v);
}

void knowledge_graph_remove(KnowledgeGraph* kn, int vertex) {
  g_array_free(g_array_index(kn->graph, struct Vertex, vertex).edges, TRUE);
  g_array_remove_index_fast(kn->graph, vertex);
}

void knowledge_graph_relate(KnowledgeGraph* kn, int from, int to, int relation) {
  GArray* edges = g_array_index(kn->graph, struct Vertex, from).edges;
  for (guint i = 0; i < edges->len; i++) {
    struct Edge e = g_array_index(edges, struct Edge, i);
    if (e.vertex == to && e.relation == relation) {
      g_array_remove_index_fast(edges, i);
      return;
    }
  }
  struct Edge e = { relation, to };
  g_array_append_val(edges, e);
}

void knowledge_graph_free(KnowledgeGraph* kn) {
  g_array_free(kn->relations, TRUE);
  g_array_free(kn->regions, TRUE);
  g_array_free(kn->graph, TRUE);
  free(kn);
}
