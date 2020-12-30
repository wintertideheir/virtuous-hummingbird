package com.autumnara.aikaterine;

import com.autumnara.aikaterine.VirtueNode;

import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.graph.DefaultEdge;

/** A graph of virtues with node representations. */
public final class VirtueGraph extends DirectedAcyclicGraph<VirtueNode, DefaultEdge>
{

    /** Constructor for a virtue graph. */
    public VirtueGraph()
    {
        super(this.getClass().byName("org.jgrapht.graph.DefaultEdge"));
    }

}
