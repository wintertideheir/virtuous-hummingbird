package com.autumnara.aikaterine;

import static java.lang.Math.*;

/** An object with a lifecycle inside the normal lifecycle.
    The normal Java lifecyle begins with object construction and ends
    when the object becomes unreachable. This object has another
    lifecycle that begins at {@link #initialize initialization} and
    ends with {@link #terminate termination}. The object can require
    and enforce that certain methods only be called during this
    sublifecycle. Because Java requires that we construct an object
    before calling methods on it, this object can serve as a base for a
    lazy or delayed constructor. This class also provides a less
    restrictive alternative to {@link java.lang.AutoCloseable}. */
public abstract class SubLifecycle
{

    /** Whether this component has been initialized or not. */
    private boolean initialized = false;

    /** Initialize this object.
        This method checks that this object hasn't already been
        initialized and then calls {@link #initializeInternal}. */
    public final void initialize()
    {
        if (this.initialized)
        {
            throw new IllegalStateException(this.getClass().getName() + " being initialized has already been initialized.");
        }
        this.initializeInternal();
        this.initialized = true;
    }

    /** Initialize the internals of this object. */
    protected abstract void initializeInternal();

    /** Terminate this component.
        This method calls {@link #terminateInternal} and marks this
        component as not initialized. */
    public final void terminate()
    {
        if (!this.initialized)
        {
            throw new IllegalStateException(this.getClass().getName() + " being terminated has already been terminated.");
        }
        this.terminateInternal();
        this.initialized = false;
    }

    /** Terminate this object.
        Objects should free their resources here. */
    protected void terminateInternal() {}

}
