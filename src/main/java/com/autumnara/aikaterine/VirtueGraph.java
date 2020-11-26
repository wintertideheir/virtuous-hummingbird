package com.autumnara.aikaterine;

import com.autumnara.aikaterine.Virtue;

import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.graph.DefaultEdge;

import java.lang.Math;

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

    /** The minimum arc length that a virtue must space itself from sibling virtues. */
    private static final float MIN_ARC_LEN = 1;

    /** Constructor for a virtue graph. The graph is initially
        populated with the root virtue. */
    public VirtueGraph()
    {
        super(DefaultEdge.class);
        this.root = new Virtue("εὐδαιμονία", "Flourishing, prosperity, or blessedness");
        this.addVertex(this.root);
        this.focus = this.root;
    }

    /** Render a virtue and it's descendants with a variable-radius radial tree.
        @param virtue    a virtue that is part of this virtue graph
        @param min_angle the minimum angle the virtue can be at from it's ancestor
        @param max_angle the maximum angle the virtue can be at from it's ancestor
        @param distance  distance from the virtue to it's ancestor, relative to
                         {@link com.autumnara.aikaterine.VirtueGraph.root} */
    private void render(Virtue virtue,
                        float  min_angle,
                        float  max_angle,
                        float  distance)
    {
        float delta_angle = max_angle - min_angle;

        virtue.angle    = min_angle + (0.5f * delta_angle);
        virtue.distance = distance;

        Virtue[] descendants = this.getDescendants(this.root).toArray(Virtue[]::new);
        for (int i = 0; i < descendants.length; i++)
        {
            this.render(descendants[i],
                        min_angle + (delta_angle * i       / descendants.length),
                        min_angle + (delta_angle * (i + 1) / descendants.length),
                        Math.max(distance + 1,
                                 MIN_ARC_LEN / delta_angle));
        }
    }

}
