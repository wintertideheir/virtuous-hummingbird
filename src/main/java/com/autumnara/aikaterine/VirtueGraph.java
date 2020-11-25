package com.autumnara.aikaterine;

import com.autumnara.aikaterine.Virtue;

import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.graph.DefaultEdge;

/** A {@link org.jgrapht.graph.DirectedAcyclicGraph} of 
    {@link com.autumnara.aikaterine.Virtue} with relevant auxiliary
    functions. */
public class VirtueGraph extends DirectedAcyclicGraph<Virtue, DefaultEdge> {

    /** The highest virtue, εὐδαιμονία, from which all connected edges
        should be directed from. */
    public final Virtue root;

    /** The virtue presently being edited or viewed. The view of the
        graph is centered over this virtue and oriented so that this
        virtue is horizontal and above the root node. */
    private Virtue focus;

    /** Constructor for a virtue graph. The graph is initially
        populated with the root virtue. */
    public VirtueGraph()
    {
        super(DefaultEdge.class);
        this.root = new Virtue("εὐδαιμονία", "Flourishing, prosperity, or blessedness");
        this.addVertex(this.root);
        this.focus = this.root;
    }

}
