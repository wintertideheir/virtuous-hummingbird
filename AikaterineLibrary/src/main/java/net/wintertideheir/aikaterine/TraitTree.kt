package net.wintertideheir.aikaterine

import org.jgrapht.graph.DefaultEdge
import org.jgrapht.graph.DirectedAcyclicGraph
import java.io.Serializable

class TraitTree : Serializable{

    val tree: DirectedAcyclicGraph<net.wintertideheir.aikaterine.Trait, DefaultEdge> = DirectedAcyclicGraph(DefaultEdge::class.java);

    init {
        tree.addVertex(
            net.wintertideheir.aikaterine.Trait(
                0,
                "Eudaemonia",
                "Root for all non-orphaned nodes."
            )
        );
    }

    fun requestId() : Int? {
        val idList = tree.map { it.id }
        for (x in 1 .. Int.MAX_VALUE) {
            if (x !in idList) {
                return x
            }
        }
        return null
    }

}