#include <glib.h>

struct Position {
  float x;
  float y;
};

struct Rectangle {
  struct Position center;
  struct Position offset;
};

struct Edge {
  int relation;
  int vertex;
};

struct Vertex {
  int region;
  struct Edge* edges;
  const char* idea;
};

struct KnowledgeGraph {
  GHashTable* relationMap;
  GHashTable* regionMap;
  struct Vertex* graph;
};
