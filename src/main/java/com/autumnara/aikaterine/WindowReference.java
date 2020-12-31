package com.autumnara.aikaterine;

/** A safe way to share information about a window.
  */
public final class WindowReference
{

    /** The width of this window.
      *
      * Must always be positive.
      */
    public final float width;

    /** The height of this window.
      *
      * Must always be positive.
      */
    public final float height;

    public WindowReference(float width,
                           float height)
    {
        if (width <= 0)
        {
            throw new IllegalArgumentException("Window reference height must be positive.");
        }
        if (height <= 0)
        {
            throw new IllegalArgumentException("Window reference width must be positive.");
        }

        this.width  = width;
        this.height = height;
    }

}
