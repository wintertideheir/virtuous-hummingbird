package com.autumnara.aikaterine;

import static java.lang.Math.*;

/** A component that can be drawn at specific positions. By default
    this component does nothing and must be extended for any
    functionality. */
public class UIComponent
{

    /** Whether this component is drawable or not. */
    private boolean drawable = false;

    /** The boundaries of this component. */
    public WindowBoundary boundary;

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
        @param boundary the new boundary of this object */
    public final void setDrawingArea(WindowBoundary boundary)
    {
        this.boundary = boundary;
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
