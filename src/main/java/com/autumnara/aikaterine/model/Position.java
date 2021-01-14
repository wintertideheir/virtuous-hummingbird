package com.autumnara.hummingbird.model;

import static java.lang.Math.*;

/** A position expressed in rectangular Cartesian coordinates.
  */
final class Position
{

    /** The x-coordinate of this position.
      */
    final float x;

    /** The y-coordinate of this position.
      */
    final float y;

    /** Constructor for a position from x- and y-coordinates.
      */
    Position(float x,
             float y)
    {
        this.x = x;
        this.y = y;
    }

}
