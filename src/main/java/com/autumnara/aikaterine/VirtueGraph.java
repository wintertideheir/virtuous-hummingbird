package com.autumnara.aikaterine;

import com.autumnara.aikaterine.Virtue;

import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.graph.DefaultEdge;

public class VirtueGraph extends DirectedAcyclicGraph<Virtue, DefaultEdge> {

    public VirtueGraph()
    {
        super(DefaultEdge.class);
    }

}