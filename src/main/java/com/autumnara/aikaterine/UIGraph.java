package com.autumnara.aikaterine;

import static java.lang.Math.*;

/** A UI component for drawing and handling virtue graphs.
  */
public class UIGraph extends UIComponent
{

    /** The graph being drawn. */
    private VirtueGraph graph;

    /** Where the view is centered over the graph. */
    private Position center;

    /** How many nodes can be drawn across the shortest window dimension.
        Each node is drawn with a square mesh. */
    private BoundedFloat scale;

    /** Constructor for a UI graph component.
        @param graph the graph being drawn
        @param scale the initial scale of this component. The scale
                     determines how many nodes can be drawn across the
                     shortest window dimension. */
    public UIGraph(VirtueGraph graph,
                   BoundedFloat scale)
    {
        this.graph = graph;
        this.center = new Position(0, 0);
        if (scale.minimum <= 0)
        {
            throw new IllegalArgumentException("UIGraph scale minimum must always be positive.");
        }
        this.scale = scale;
    }

    @Override
    protected void onInitialize() {}

    @Override
    protected void onDraw() {}

    @Override
    protected void onSetBoundaries() {}

    @Override
    protected void onSetReference() {}

    @Override
    protected void onTerminate() {}

}
