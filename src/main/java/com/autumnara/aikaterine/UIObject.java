package com.autumnara.aikaterine;

import static java.lang.Math.*;

/** A object that can be drawn.
    By default this object does nothing and must be extended for any
    functionality. */
public class UIObject
{

    /** Whether this object is drawable or not.
        Exists solely for object state validation when calling
        methods. */
    private boolean drawable = false;

    /** Return whether this object is drawable or not.
        Primarily intended for object state validation by subclasses. */
    public boolean drawable()
    {
        return this.drawable;
    }

    /** Make this object drawable.
        This method performs basic error checking and then calls
        {@link #onInitialize}. This method must be called before any
        other method. */
    public final void initialize()
    {
        if (this.drawable)
        {
            throw new IllegalStateException("UI object to be initialized is already initialized.");
        }
        this.onInitialize();
        this.drawable = true;
    }

    /** Make this object drawable.
        By default this method does nothing and can be overrided by
        subclasses. */
    protected void onInitialize() {}

    /** Draw this object with it's present configuration.
        This method checks that this object is drawable and then calls
        {@link #onDraw}. */
    public final void draw()
    {
        if (!this.drawable)
        {
            throw new IllegalStateException("UI object needs to be initialized before being drawn.");
        }
        this.onDraw();
    }

    /** Draw this object with it's present configuration.
        By default this method does nothing and can be overrided by
        subclasses. */
    protected void onDraw() {}

    /** Make this UI object undrawable and free all of it's resources.
        This method calls {@link #onTerminate} and marks this UI object
        as undrawable. */
    public final void terminate()
    {
        if (!this.drawable)
        {
            throw new IllegalStateException("UI object being terminated is not drawable.");
        }

        this.onTerminate();
        this.drawable = false;
    }

    /** Make this UI object undrawable and free all of it's resources.
        By default this method does snothing and can be overrided by
        subclasses. */
    protected void onTerminate() {}

}
