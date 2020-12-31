package com.autumnara.aikaterine;

import static java.lang.Math.*;

/** The boundaries of a window.
  */
public final class WindowBoundary
{

    /** The lower bound of this component in OpenGL clip space.
      *
      * Must be between -1f and 1f, inclusive, and less than
      * {@link #xMax}.
      */
    public final float xMin;

    /** The upper bound of this component in OpenGL clip space.
      *
      * Must be between -1f and 1f, inclusive, and greater than
      * {@link #xMin}.
      */
    public final float xMax;

    /** The lower bound of this component in OpenGL clip space.
      *
      * Must be between -1f and 1f, inclusive, and less than
      * {@link #yMax}.
      */
    public final float yMin;

    /** The upper bound of this component in OpenGL clip space.
      *
      * Must be between -1f and 1f, inclusive, and greater than
      * {@link #yMin}.
      */
    public final float yMax;

    public WindowBoundary(float xMin, float xMax,
                          float yMin, float yMax)
    {
        if (xMin > xMax)
        {
            throw new IllegalArgumentException("Window boundary x-minimum is greater than y-maximum.");
        }
        if (yMin > yMax)
        {
            throw new IllegalArgumentException("Window boundary y-minimum is greater than y-maximum.");
        }

        if ((xMin > 1f) || (xMin < -1f))
        {
            throw new IllegalArgumentException("Window boundary x-minimum is not between 1 and -1.");
        }
        if ((xMax > 1f) || (xMax < -1f))
        {
            throw new IllegalArgumentException("Window boundary x-maximum is not between 1 and -1.");
        }
        if ((yMin > 1f) || (yMin < -1f))
        {
            throw new IllegalArgumentException("Window boundary y-minimum is not between 1 and -1.");
        }
        if ((yMax > 1f) || (yMax < -1f))
        {
            throw new IllegalArgumentException("Window boundary y-maximum is not between 1 and -1.");
        }

        this.xMin = xMin;
        this.xMax = xMax;
        this.yMin = yMin;
        this.yMax = yMax;
    }

}
