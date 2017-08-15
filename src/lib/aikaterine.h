#pragma once

struct Vector {
  float x;
  float y;
};

struct Rectangle {
  struct Vector center;
  struct Vector offset;
    // The offset's attributes should never be less than zero.
};

typedef struct KnowledgeGraph KnowledgeGraph;

KnowledgeGraph* knowledge_graph_new();
void knowledge_graph_add(KnowledgeGraph* kn, int region, char* idea, struct Vector pos);
void knowledge_graph_remove(KnowledgeGraph* kn, int vertex);
void knowledge_graph_connect(KnowledgeGraph* kn, int from, int to, int relation);
  // Undefined behavior if the edge already exists.
void knowledge_graph_disconnect(KnowledgeGraph* kn, int from, int to, int relation);
void knowledge_graph_free(KnowledgeGraph*);
