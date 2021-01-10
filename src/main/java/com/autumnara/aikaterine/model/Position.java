package com.autumnara.aikaterine.model;

import static java.lang.Math.*;

/** A position expressed in rectangular Cartesian coordinates.
  */
public final class Position
{

    /** The x-coordinate of this position.
      */
    public final float x;

    /** The y-coordinate of this position.
      */
    public final float y;

    /** Constructor for a position from x- and y-coordinates.
      */
    public Position(float x, float y)
    {
        this.x = x;
        this.y = y;
    }

}
