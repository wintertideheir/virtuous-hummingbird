#include "aikaterine.h"

#include <glib.h>
#include <stdlib.h>

struct Edge {
  int relation;
  int vertex;
};

struct Vertex {
  struct AikaterineIdea wrapper;
  GArray* edges;
};

struct AikaterineGraph {
  GArray* relations;
  GArray* regions;
  GArray* graph;
};

void vertex_destructor(struct Vertex* v) {
  g_array_free(v->edges, TRUE);
  free(v->wrapper.idea);
}

AikaterineGraph* aikaterine_new() {
  AikaterineGraph* ag = malloc(sizeof(AikaterineGraph));
  ag->relations = g_array_new(FALSE, FALSE, sizeof(char*));
  ag->regions = g_array_new(FALSE, FALSE, sizeof(char*));
  ag->graph = g_array_new(FALSE, FALSE, sizeof(struct Vertex));
  g_array_set_clear_func(ag->graph, (GDestroyNotify) vertex_destructor);
  return ag;
}

void aikaterine_add(AikaterineGraph* ag, struct AikaterineIdea vertex) {
  for (guint i = 0; i < ag->graph->len; i++) {
    struct Vertex* v = &g_array_index(ag->graph, struct Vertex, i);
    if (v->wrapper.region == -1) {
      v->wrapper = vertex;
      v->edges = g_array_new(FALSE, FALSE, sizeof(struct Edge));
      return;
    }
  }
  struct Vertex v = { vertex, g_array_new(FALSE, FALSE, sizeof(struct Edge)) };
  g_array_append_val(ag->graph, v);
}

void aikaterine_remove(AikaterineGraph* ag, int vertex) {
  struct Vertex* v = &g_array_index(ag->graph, struct Vertex, vertex);
  v->wrapper.region = -1;
  g_array_free(v->edges, TRUE);
  free(v->wrapper.idea);
  for (guint x = 0; x < ag->graph->len; x++) {
    GArray* e = g_array_index(ag->graph, struct Vertex, x).edges;
    for (guint y = e->len - 1; y >= 0; y--) {
      struct Edge edge = g_array_index(e, struct Edge, y);
      if (edge.vertex == vertex)
      {
        g_array_remove_index_fast(e, y);
      }
    }
  }
}

void aikaterine_connect(AikaterineGraph* ag, int from, int to, int relation) {
  GArray* edges = g_array_index(ag->graph, struct Vertex, from).edges;
  for (guint i = 0; i < edges->len; i++) {
    struct Edge e = g_array_index(edges, struct Edge, i);
    if (e.vertex == to && e.relation == relation) {
      return;
    }
  }
  struct Edge e = { relation, to };
  g_array_append_val(g_array_index(ag->graph, struct Vertex, from).edges, e);
}

void aikaterine_disconnect(AikaterineGraph* ag, int from, int to, int relation) {
  GArray* edges = g_array_index(ag->graph, struct Vertex, from).edges;
  for (guint i = 0; i < edges->len; i++) {
    struct Edge e = g_array_index(edges, struct Edge, i);
    if (e.vertex == to && e.relation == relation) {
      g_array_remove_index_fast(edges, i);
      return;
    }
  }
}

void aikaterine_free(AikaterineGraph* ag) {
  g_array_free(ag->relations, TRUE);
  g_array_free(ag->regions, TRUE);
  g_array_free(ag->graph, TRUE);
  free(ag);
}

struct AikaterineView aikaterine_view(AikaterineGraph* ag, struct AikaterineRectangle area) {
  struct AikaterineView av;

  GArray* vertices = g_array_new(FALSE, FALSE, sizeof(int));
  for (guint i = 0; i < ag->graph->len; i++) {
    struct AikaterineVector pos = g_array_index(ag->graph, struct Vertex, i).wrapper.pos;
    if (pos.x <= area.upper_right.x && pos.x >= area.lower_left.x &&
        pos.y <= area.upper_right.y && pos.y >= area.lower_left.y) {
      g_array_append_val(vertices, i);
    }
  }
  av.verts_len = vertices->len;
  av.verts = (int*) g_array_free(vertices, FALSE);

  GArray* edges = g_array_new(FALSE, FALSE, 3 * sizeof(int));
  for (guint i = 0; i < ag->graph->len; i++) {
    struct Vertex* v = &g_array_index(ag->graph, struct Vertex, i);
    for (int j = 0; j < av.verts_len; j++) {
      if (i == av.verts[j]) {
        for (guint k = 0; k < v->edges->len; k++) {
          struct Edge* e = &g_array_index(v->edges, struct Edge, k);
          int elem[3] = {i, e->vertex, e->relation};
          g_array_append_val(edges, elem);
        }
        goto end;
      }
    }
    for (guint j = 0; j < v->edges->len; j++) {
      struct Edge* e = &g_array_index(edges, struct Edge, j);
      for (int k = 0; k < av.verts_len; k++) {
        if (e->vertex == av.verts[k]) {
          int elem[3] = {i, e->vertex, e->relation};
          g_array_append_val(edges, elem);
          continue;
        }
      }
    }
    end: continue;
  }
  av.edges_len = edges->len;
  av.edges = (int(*)[3]) g_array_free(edges, FALSE);

  return av;
}

struct AikaterineIdea* aikaterine_idea(AikaterineGraph* ag, int vertex) {
  return &g_array_index(ag->graph, struct Vertex, vertex).wrapper;
}

struct AikaterineRectangle aikaterine_boundaries(AikaterineGraph* ag) {
  float max_x, max_y, min_x, min_y;
  for (guint i = 0; i < ag->graph->len; i++) {
    struct AikaterineVector pos = g_array_index(ag->graph, struct Vertex, i).wrapper.pos;
    if (pos.x > max_x) max_x = pos.x;
    if (pos.y > max_y) max_y = pos.y;
    if (pos.x < min_x) min_x = pos.x;
    if (pos.y < min_y) min_y = pos.y;
  }
  return (struct AikaterineRectangle)
            {(struct AikaterineVector){min_x, min_y},
             (struct AikaterineVector){max_x, max_y}};
}
