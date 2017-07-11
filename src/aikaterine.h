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
void knowledge_graph_add(KnowledgeGraph*, int, const char*);
void knowledge_graph_remove(KnowledgeGraph*, int);
void knowledge_graph_relate(KnowledgeGraph*, int, int, int);
void knowledge_graph_free(KnowledgeGraph*);
