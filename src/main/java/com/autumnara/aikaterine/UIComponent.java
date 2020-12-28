package com.autumnara.aikaterine;

/** A component that can be drawn.
    By default this component does nothing and must be extended for any
    functionality. */
public abstract class UIComponent extends ResourceObject
{

    /** The boundaries of this component. */
    protected WindowBoundary boundary;

    /** The length of the window's x-dimension.
        The width of the window. */
    protected int windowX;

    /** The length of the window's y-dimension.
        The height of the window. */
    protected int windowY;

    /** Set spatial boundaries.
        This method will ensure the object has been initialized, then
        call {@link #onBounded} after setting the drawing area.
        @param boundary the new boundary. */
    public final void setBoundaries(WindowBoundary boundary)
    {
        this.assertInitialized();
        this.boundary = boundary;
        this.onBounded();
    }

    /** Update this object after spatial boundaries have been set. */
    protected abstract void onSetBoundaries();

    /** Update the window dimensions this component is being drawn in.
        This method will ensure the object has been initialized, then
        call {@link #onSetWindowSize} after setting the window dimensions.
        @param windowX the new width of the window
        @param windowY the new length of the window */
    public final void setWindowSize(int windowX, int windowY)
    {
        this.assertInitialized();
        this.windowX = windowX;
        this.windowY = windowY;
        this.onSetWindowSize();
    }

    /** Update this object after the new window size has been set. */
    protected abstract void onSetWindowSize();

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
