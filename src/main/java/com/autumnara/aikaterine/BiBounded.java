package com.autumnara.aikaterine;

/** An object that is spatially and temporally bounded.
    This object is bounded both by the internal lifecycle of
    {@link com.autumnara.aikaterine.Bounded} and bounded spatially by
    {@link com.autumnara.aikaterine.WindowBoundary}, hence
    "bibounded." */
public abstract class BiBounded extends Bounded
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

}
