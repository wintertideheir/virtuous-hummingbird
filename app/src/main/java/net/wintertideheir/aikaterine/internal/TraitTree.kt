package net.wintertideheir.aikaterine.internal

import org.jgrapht.graph.DefaultEdge
import org.jgrapht.graph.DirectedAcyclicGraph

class TraitTree {

    val tree: DirectedAcyclicGraph<Trait, DefaultEdge> = DirectedAcyclicGraph(DefaultEdge::class.java);

    init {
        tree.addVertex(Trait("Eudaemonia", "Root for all non-orphaned nodes."));
    }

}