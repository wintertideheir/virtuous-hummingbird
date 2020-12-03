package com.autumnara.aikaterine;

import static java.lang.Math.*;

/** A position expressed in polar coordinates. */
public final class PositionPolar
{
    /** The angle of the polar axis. */
    public final static float NULL_ANGLE = 0;

    /** The angle of a complete revolution about the pole. */
    public final static float REVOLUTION_ANGLE = 2 * (float) PI;

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
