package com.autumnara.aikaterine;

/** A component that can be drawn.
    By default this component does nothing and must be extended for any
    functionality. */
public abstract class UIComponent extends ResourceObject
{

    /** The boundaries of this component. */
    protected WindowBoundary boundary;

    /** A safe reference on window information. */
    protected WindowReference reference;

    /** Set spatial boundaries.
        This method will ensure the object has been initialized, then
        call {@link #onSetBoundaries} after setting the drawing area.
        @param boundary the new boundary. */
    public final void setBoundaries(WindowBoundary boundary)
    {
        this.assertInitialized();
        this.boundary = boundary;
        this.onSetBoundaries();
    }

    /** Update this object after spatial boundaries have been set. */
    protected abstract void onSetBoundaries();

    /** Update information about the window this component is being
        drawn in.
        This method will ensure the object has been initialized, then
        call {@link #onSetReference} after setting the window
        dimensions.
        @param reference the new reference */
    public final void setReference(WindowReference reference)
    {
        this.assertInitialized();
        this.reference = reference;
        this.onSetReference();
    }

    /** Update this object after the new window reference has been set.
        */
    protected abstract void onSetReference();

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
