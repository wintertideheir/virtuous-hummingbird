package com.autumnara.aikaterine;

/** A component that can be drawn.
    By default this component does nothing and must be extended for any
    functionality. */
public abstract class UIComponent extends Bounded
{

    /** The boundaries of this component. */
    protected WindowBoundary boundary;

    /** Set spatial boundaries.
        This method will ensure the object has been initialized, then
        call {@link #onBounded} after setting the drawing area.
        @param boundary the new boundary. */
    public final void bound(WindowBoundary boundary)
    {
        this.assertInitialized();
        this.boundary = boundary;
        this.onBounded();
    }

    /** Update this object after spatial boundaries have been set.
        By default this method does nothing and can be overrided by
        subclasses. */
    protected abstract void onBounded();

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

}
