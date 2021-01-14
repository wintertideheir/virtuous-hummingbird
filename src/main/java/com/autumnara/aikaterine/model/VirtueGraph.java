package com.autumnara.hummingbird.model;

import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;

/** A graph of virtues with node representations.
  */
final class VirtueGraph extends DefaultDirectedGraph<Virtue, DefaultEdge>
{

    /** Constructor for a virtue graph.
      */
    VirtueGraph()
    {
        super(DefaultEdge.class);
    }

}
