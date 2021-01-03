package com.autumnara.aikaterine;

import static org.lwjgl.opengl.GL33.glViewport;

public final class Viewport
{

    /** The x-offset of the viewport relative to the bottom-left
      * corner.
      */
    private final int x;

    /** The y-offset of the viewport relative to the bottom-left
      * corner.
      */
    private final int y;

    /** The width of the viewport.
      */
    public final int width;

    /** The height of the viewport.
      */
    public final int height;

    /** Whether this viewport is a root viewport.
      *
      * Root viewports are viewports created for
      * {@link com.autumnara.aikaterine.Window windows} and should not
      * be used by {@link com.autumnara.aikaterine.AbstractView views}.
      */
    public final boolean isRoot;

    /** Constructor for a viewport from it's components.
      *
      * @param x      the positive x-offset of the new viewport
      *               relative to the bottow-left corner of the parent,
      *               in pixels.
      * @param y      the positive y-offset of the new viewport
      *               relative to the bottow-left corner of the parent,
      *               in pixels.
      * @param width  the positive width of the new viewport,
      *               in pixels.
      * @param height the positive height of the new viewport,
      *               in pixels.
      * @param isRoot whether this viewport is a root viewport, and
      *               therefore belongs solely to a window.
      */
    private Viewport(int x,
                     int y,
                     int width,
                     int height,
                     boolean isRoot)
    {
        if (x <= 0)
        {
            throw new IllegalArgumentException("Viewport x-offset must be positive.");
        }
        if (y <= 0)
        {
            throw new IllegalArgumentException("Viewport y-offset must be positive.");
        }
        if (width <= 0) 
        {
            throw new IllegalArgumentException("Viewport width must be positive.");
        }
        if (height <= 0)
        {
            throw new IllegalArgumentException("Viewport height must be positive.");
        }

        this.x      = x;
        this.y      = y;
        this.width  = width;
        this.height = height;
        this.isRoot = isRoot;
    }
    
    /** Copy constructor for a viewport.
      *
      * @param original the viewport to be copied.
      */
    public Viewport(Viewport original)
    {
        return new Viewport(original.x,
                            original.y,
                            original.width,
                            original.height,
                            false);
    }

    /** Constructor for root viewport for a window.
      *
      * @param width  the width of the window in pixels
      * @param height the height of the window in pixels
      */
    public Viewport(int width,
                    int height)
    {
        this(0, 0, width, height, true);
    }

    /** Create a new viewport within this viewport.
      *
      * This method will check to make sure that the new viewport is
      * within the bounds of the parent.
      *
      * @param x      the positive x-offset of the new viewport
      *               relative to the bottow-left corner of the parent,
      *               in pixels.
      * @param y      the positive y-offset of the new viewport
      *               relative to the bottow-left corner of the parent,
      *               in pixels.
      * @param width  the positive width of the new viewport,
      *               in pixels.
      * @param height the positive height of the new viewport,
      *               in pixels.
      */
    public Viewport subViewport(int x,
                                int y,
                                int width,
                                int height)
    {
        if (this.width < (x + width))
        {
            throw new IllegalArgumentException("Derived viewport does not fit in it's parent. " +
                                               "Either it's width and/or x-offset is too large.");
        }
        if (this.height < (y + height))
        {
            throw new IllegalArgumentException("Derived viewport does not fit in it's parent. " +
                                               "Either it's height and/or y-offset is too large.");
        }

        return new Viewport(x, y, width, height, false);
    }

    /** Activate the viewport through
      * {@link org.lwjgl.opengl.GL33.glViewport glViewport}.
      */
    public void activate()
    {
        glViewport(x, y, width, height);
    }

}
