package com.autumnara.aikaterine;

import com.autumnara.aikaterine.Virtue;

import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.graph.DefaultEdge;

public class VirtueGraph extends DirectedAcyclicGraph<Virtue, DefaultEdge> {

    public final Virtue root;

    public VirtueGraph()
    {
        super(DefaultEdge.class);
        this.root = new Virtue("εὐδαιμονία", "Flourishing, prosperity, or blessedness");
        this.addVertex(this.root);
    }

}