package net.wintertideheir.aikaterine.internal

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;

class PersonalAnalysis {
    val qualities: Graph<PersonalTrait, DefaultEdge> = DefaultDirectedGraph(DefaultEdge::class.java);
}