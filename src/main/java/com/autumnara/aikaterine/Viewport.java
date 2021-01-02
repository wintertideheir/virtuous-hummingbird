package com.autumnara.aikaterine;

import static org.lwjgl.opengl.GL33.glViewport;

public final class Viewport
{

    /** The x-offset of the viewport relative to the bottom-left
      * corner.
      */
    private int x;

    /** The y-offset of the viewport relative to the bottom-left
      * corner.
      */
    private int y;

    /** The width of the viewport.
      */
    private int width;

    /** The height of the viewport.
      */
    private int height;

    /** Constructor for a viewport from window dimensions.
      *
      * @param width  the positive width of the window, in pixels.
      * @param height the positive height of the window, in pixels.
      */
    public Viewport(int width,
                    int height)
    {
        if (width <= 0) 
        {
            throw new IllegalArgumentException("Viewport width must be positive.");
        }
        if (height <= 0)
        {
            throw new IllegalArgumentException("Viewport height must be positive.");
        }

        this.x = 0;
        this.y = 0;
        this.width = width;
        this.height = height;
    }

    /** Constructor for a viewport within another viewport.
      *
      * This constructor will check to make sure that the new viewport
      * is within the bounds of the parent.
      *
      * @param parent the parent viewport.
      * @param x      the psoitive x-offset of the new viewport
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
    public Viewport(Viewport parent,
                    int x,
                    int y,
                    int width,
                    int height)
    {
        if (width <= 0) 
        {
            throw new IllegalArgumentException("Viewport width must be positive.");
        }
        if (height <= 0)
        {
            throw new IllegalArgumentException("Viewport height must be positive.");
        }
        if (x <= 0) 
        {
            throw new IllegalArgumentException("Viewport x-offset must be positive.");
        }
        if (y <= 0)
        {
            throw new IllegalArgumentException("Viewport y-offset must be positive.");
        }
        if ((this.width < (x + width))
        {
            throw new IllegalArgumentException("Derived viewport does not fit in it's parent. " +
                                               "Either it's width and/or x-offset is too large.");
        }
        if (this.height < (y + height))
        {
            throw new IllegalArgumentException("Derived viewport does not fit in it's parent. " +
                                               "Either it's height and/or y-offset is too large.");
        }

        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;
    }

    /** Activate the viewport through
      * {@link org.lwjgl.opengl.GL33.glViewport glViewport}.
      */
    public void activate()
    {
        glViewport(x, y, width, height);
    }

    /** Get the width of the viewport in pixels.
      */
    public int getWidth()
    {
        return this.width;
    }

    /** Get the height of the viewport in pixels.
      */
    public int getHeight()
    {
        return this.height;
    }

}
