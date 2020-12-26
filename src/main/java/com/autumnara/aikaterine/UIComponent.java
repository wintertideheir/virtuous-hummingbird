package com.autumnara.aikaterine;

/** A component that can be drawn.
    By default this component does nothing and must be extended for any
    functionality. */
public abstract class UIComponent extends BiBounded
{

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
