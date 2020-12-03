package com.autumnara.aikaterine;

import com.autumnara.aikaterine.Virtue;
import com.autumnara.aikaterine.ColorRGB;
import com.autumnara.aikaterine.ColorHCL;
import com.autumnara.aikaterine.PositionPolar;
import com.autumnara.aikaterine.PositionRectangular;

import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.graph.DefaultEdge;

import java.util.function.Function;
import static java.lang.Math.*;

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

    /** The minimum distance between virtues. */
    private static final float MIN_DISTANCE = 1;

    /** Constructor for a virtue graph. The graph is initially
        populated with the root virtue. */
    public VirtueGraph()
    {
        super(DefaultEdge.class);
        this.root = new Virtue("εὐδαιμονία", "Flourishing, prosperity, or blessedness");
        this.addVertex(this.root);
        this.focus = this.root;
    }

    /** Render the virtue graph as a variable-radius radial tree. */
    public void render()
    {
        this.render(this.root,
                    PositionPolar.NULL_ANGLE,
                    PositionPolar.REVOLUTION_ANGLE,
                    0);
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
        /* Calculate the x and y coordinates of the virtue by the
         * bisecting angle and distance.
         */
        float angle = 0.5f * (min_angle + max_angle);

        virtue.pos = new PositionRectangular(new PositionPolar(angle, distance));

        /* Calculate the RGB colors from a hue-chroma-lightness
         * interpretation of the angle and distance.
         */
        final float CHROMA_DISTANCE_RATIO = 0.5f;

        float hue = angle / PositionPolar.REVOLUTION_ANGLE;
        float chroma = 1 - (1 / ((CHROMA_DISTANCE_RATIO * distance) + 1));
        float lightness = 0.5f;

        virtue.color = new ColorRGB(new ColorHCL(hue, chroma, lightness));

        //Recurse on descendants
        Virtue[] descendants = this.getDescendants(this.root).toArray(Virtue[]::new);
        float delta_angle = (max_angle - min_angle) / descendants.length;
        for (int i = 0; i < descendants.length; i++)
        {
            this.render(descendants[i],
                        min_angle + (delta_angle * i),
                        min_angle + (delta_angle * (i + 1)),
                        max(distance + MIN_DISTANCE,                       // The distance from this virtue to it's descendants.
                            MIN_DISTANCE * 2 * (float) sin(delta_angle))); // The distance between descendants.
        }
    }

}
