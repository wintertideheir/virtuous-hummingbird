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
  GHashTable* relationMap;
  GHashTable* regionMap;
  GArray* graph;
};

KnowledgeGraph* knowledge_graph_new() {
  KnowledgeGraph* kn = malloc(sizeof(KnowledgeGraph));
  kn->relationMap = g_hash_table_new(g_int_hash, g_int_equal);
  kn->regionMap = g_hash_table_new(g_int_hash, g_int_equal);
  kn->graph = g_array_new(FALSE, FALSE, sizeof(int));
}

void knowledge_graph_free(KnowledgeGraph* kn) {
  g_hash_table_destroy(kn->relationMap);
  g_hash_table_destroy(kn->relationMap);
  g_array_free(kn->graph, TRUE);
  free(kn);
}
