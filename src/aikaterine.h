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
void knowledge_graph_free(KnowledgeGraph*);
