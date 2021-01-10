package com.autumnara.aikaterine.model;

import com.autumnara.aikaterine.VirtueNode;

import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;

/** A graph of virtues with node representations.
  */
final class VirtueGraph extends DefaultDirectedGraph<VirtueNode, DefaultEdge>
{

    /** Constructor for a virtue graph.
      */
    VirtueGraph()
    {
        super(DefaultEdge.class);
    }

}
