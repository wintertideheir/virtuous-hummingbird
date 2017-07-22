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
void knowledge_graph_add(KnowledgeGraph*, int, char*);
void knowledge_graph_remove(KnowledgeGraph*, int);
void knowledge_graph_connect(KnowledgeGraph*, int, int, int);
void knowledge_graph_disconnect(KnowledgeGraph*, int, int, int);
void knowledge_graph_free(KnowledgeGraph*);
