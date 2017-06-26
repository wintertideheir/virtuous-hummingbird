#pragma once

struct Position {
  float x;
  float y;
};

struct Rectangle {
  struct Position center;
  struct Position offset;
};

struct KnowledgeGraph;
