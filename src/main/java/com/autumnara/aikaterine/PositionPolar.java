package com.autumnara.aikaterine;

/** A position expressed in polar coordinates. */
public final class PositionPolar
{

    /** The polar angle of this position relative to the polar axis. */
    public final float angle;

    /** The radius of this position from the pole. */
    public final float radius;

    /** Constructor for a position from polar angle and radius. */
    public PositionPolar(float angle,
                         float radius)
    {
        this.angle = angle;
        this.radius = radius;
    }

}
