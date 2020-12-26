package com.autumnara.aikaterine;

/** A component that can be drawn at specific positions. By default
    this component does nothing and must be extended for any
    functionality. */
public abstract class UIComponent extends Bounded
{

    /** The boundaries of this component. */
    public WindowBoundary boundary;

    /** Draw this component with it's present configuration.
        This method checks that this component is initialized and then
        calls {@link #onDraw}. */
    public final void draw()
    {
        this.assertInitialized();
        this.onDraw();
    }

    /** Draw this component within {@link #boundary}. */
    protected abstract void onDraw();

    /** Set the allocated drawing space by OpenGL window coordinates.
        This method will call {@link #onSetDrawingArea} after setting
        the drawing area.
        @param boundary the new boundary of this object */
    public final void setDrawingArea(WindowBoundary boundary)
    {
        this.assertInitialized();
        this.boundary = boundary;
        this.onSetDrawingArea();
    }

    /** Update this component after setting the drawing area.
        By default this method does nothing and can be overrided by
        subclasses. */
    protected abstract void onSetDrawingArea();

}
