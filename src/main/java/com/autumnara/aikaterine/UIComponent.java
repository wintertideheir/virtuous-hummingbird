package com.autumnara.aikaterine;

import static java.lang.Math.*;

/** A component that can be drawn at specific positions. By default
    this component does nothing and must be extended for any
    functionality. */
public class UIComponent
{

    /** Whether this component is drawable or not. */
    private boolean drawable = false;

    /** The lower bound of this component in OpenGL clip space.
        Must be between -1f and 1f, and less than {@link #xMax}. */
    protected float xMin;

    /** The upper bound of this component in OpenGL clip space.
        Must be between -1f and 1f, and greater than {@link #xMin}. */
    protected float xMax;

    /** The lower bound of this component in OpenGL clip space.
        Must be between -1f and 1f, and less than {@link #yMax}. */
    protected float yMin;

    /** The upper bound of this component in OpenGL clip space.
        Must be between -1f and 1f, and greater than {@link #yMin}. */
    protected float yMax;

    /** Make this component drawable.
        This method performs basic error checking and then calls
        {@link #onInitialize}. This method must be called before any
        other method. Without calling {@link #setDrawingArea} after,
        drawing this component will draw to the default boundaries (the
        entire window). */
    public final void initialize()
    {
        if (this.drawable)
        {
            throw new IllegalStateException("Component to be initialized is already initialized.");
        }
        this.onInitialize();
        this.drawable = true;
    }

    /** Make this component drawable.
        By default this method does nothing and can be overrided by
        subclasses. */
    protected void onInitialize() {}

    /** Draw this component with it's present configuration.
        This method checks that this component is drawable and then
        calls {@link #onDraw}. */
    public final void draw()
    {
        if (!this.drawable)
        {
            throw new IllegalStateException("Component needs to be initialized before being drawn.");
        }
        this.onDraw();
    }

    /** Draw this component with it's present configuration.
        By default this method does nothing and can be overrided by
        subclasses. Drawing should occur in the boundaries set by
        {@link #setDrawingArea}. */
    protected void onDraw() {}

    /** Set the allocated drawing space by OpenGL window coordinates.
        This method will call {@link #onSetDrawingArea} after setting
        the drawing area.
        @param xMin The lower bound of this component in OpenGL clip
                    space. Must be between -1f and 1f, and less than
                    xMax.
        @param xMax The upper bound of this component in OpenGL clip
                    space. Must be between -1f and 1f, and greater than
                    xMin.
        @param yMin The lower bound of this component in OpenGL clip
                    space. Must be between -1f and 1f, and less than
                    yMax.
        @param yMax The upper bound of this component in OpenGL clip
                    space. Must be between -1f and 1f, and greater than
                    yMin. */
    public final void setDrawingArea(float xMin, float yMin,
                                     float xMax, float yMax)
    {
        if (!this.drawable)
        {
            throw new IllegalStateException("Component needs to be initialized before the drawing area can be set.");
        }

        if (xMin > xMax)
        {
            throw new IllegalArgumentException("Drawing area has a greater x-minimum than x-maximum.");
        }
        if (yMin > yMax)
        {
            throw new IllegalArgumentException("Drawing area has a greater y-minimum than y-maximum.");
        }

        if ((abs(xMin) > 1f) ||
            (abs(xMax) > 1f) ||
            (abs(yMin) > 1f) ||
            (abs(yMax) > 1f))
        {
            throw new IllegalArgumentException("Drawing area components are not normalized.");
        }

        this.xMin = xMin;
        this.xMax = xMax;
        this.yMin = yMin;
        this.yMax = yMax;

        this.onSetDrawingArea();
    }

    /** Update this component after setting the drawing area.
        By default this method does nothing and can be overrided by
        subclasses. */
    protected void onSetDrawingArea() {}

    /** Make this component undrawable and free all of it's resources.
        This method calls {@link #onTerminate} and marks this component
            as undrawable. */
    public final void terminate()
    {
        if (!this.drawable)
        {
            throw new IllegalStateException("Component being terminated is not drawable.");
        }

        this.onTerminate();
        this.drawable = false;
    }

    /** Make this component undrawable and free all of it's resources.
        By default this method does snothing and can be overrided by
        subclasses. */
    protected void onTerminate() {}

}
