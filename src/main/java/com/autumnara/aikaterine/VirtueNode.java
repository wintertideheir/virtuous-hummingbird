package com.autumnara.aikaterine;

/** A virtue represented as a node.

    A virtue with a node representation in two-dimensional space. The
    representation is a circle with a defined position and color.
    Virtues that have similar colors ought to be similar. */
public class VirtueNode extends Virtue
{

    /** Constructor for a virtue represented as a node. */
    public VirtueNode(String              name,
                      String              description,
                      PositionRectangular position,
                      ColorRGB            color)
    {
        super(name, description);
        this.position    = position;
        this.color       = color;
    }

    /** The position of the node in space. */
    public PositionRectangular position;

    /** The color of the virtue. */
    public ColorRGB color;

}
