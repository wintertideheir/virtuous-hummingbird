package com.autumnara.aikaterine;

/** An object with a lifecycle inside the normal lifecycle.
    This class is called a "bounded class" because it's bounded in time
    by explicit intialization and termination.
    <br>
    The normal Java lifecyle begins with object construction and ends
    when the object becomes unreachable. This object has another
    lifecycle that begins at {@link #initialize initialization} and
    ends with {@link #terminate termination}. The object can require
    and enforce that certain methods only be called during this
    sublifecycle.
    <br>
    Because Java requires that we construct an object before calling
    methods on it, this object can serve as a base for a lazy or
    delayed constructor. This class also provides a less restrictive
    alternative to {@link java.lang.AutoCloseable}. */
public abstract class Bounded
{

    /** Whether this component has been initialized or not. */
    private boolean initialized = false;

    /** Initialize this object.
        This method checks that this object hasn't already been
        initialized and then calls {@link #onInitialize}. */
    public final void initialize()
    {
        if (this.initialized)
        {
            throw new IllegalStateException(this.getClass().getName() + " being initialized has already been initialized.");
        }
        this.onInitialize();
        this.initialized = true;
    }

    /** Initialize the internals of this object. */
    protected abstract void onInitialize();

    /** Assert that this object has been initialized. */
    public final void assertInitialized()
    {
        if (!this.initialized)
        {
            throw new IllegalStateException(this.getClass().getName() + " has not been initialized.");
        }
    }

    /** Terminate this component.
        This method calls {@link #onTerminate} and marks this
        component as not initialized. */
    public final void terminate()
    {
        if (!this.initialized)
        {
            throw new IllegalStateException(this.getClass().getName() + " being terminated has already been terminated.");
        }
        this.onTerminate();
        this.initialized = false;
    }

    /** Terminate this object.
        Objects should free their resources here. */
    protected void onTerminate() {}

}
