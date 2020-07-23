package net.wintertideheir.aikaterine

import org.jgrapht.graph.DefaultEdge
import org.jgrapht.graph.DirectedAcyclicGraph
import java.io.Serializable

class TraitTree : Serializable{

    val tree: DirectedAcyclicGraph<net.wintertideheir.aikaterine.Trait, DefaultEdge> = DirectedAcyclicGraph(DefaultEdge::class.java);

    init {
        tree.addVertex(
            net.wintertideheir.aikaterine.Trait(
                "Eudaemonia",
                "Root for all non-orphaned nodes."
            )
        );
    }

}