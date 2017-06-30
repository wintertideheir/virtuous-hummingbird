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
  kn->graph = g_array_new(FALSE, FALSE, sizeof(int));
}

void knowledge_graph_free(KnowledgeGraph* kn) {
  g_array_free(kn->relations, TRUE);
  g_array_free(kn->regions, TRUE);
  g_array_free(kn->graph, TRUE);
  free(kn);
}
