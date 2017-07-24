#pragma once

struct Position {
  float x;
  float y;
};

struct Rectangle {
  struct Position center;
  struct Position offset;
};

typedef struct KnowledgeGraph KnowledgeGraph;

KnowledgeGraph* knowledge_graph_new();
void knowledge_graph_add(KnowledgeGraph* kn, int region, char* idea, struct Position pos);
void knowledge_graph_remove(KnowledgeGraph* kn, int vertex);
void knowledge_graph_connect(KnowledgeGraph* kn, int from, int to, int relation);
void knowledge_graph_disconnect(KnowledgeGraph* kn, int from, int to, int relation);
void knowledge_graph_free(KnowledgeGraph*);
