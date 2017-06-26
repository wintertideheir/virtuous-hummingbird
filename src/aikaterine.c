#include "aikaterine.h"

#include <glib.h>

struct Edge {
  int relation;
  int vertex;
};

struct Vertex {
  int region;
  GArray edges;
  const char* idea;
};

struct KnowledgeGraph {
  GHashTable* relationMap;
  GHashTable* regionMap;
  GArray graph;
};
