package com.autumnara.aikaterine;

/** An object that needs to be initialized and terminated.

    This class has an internal state that must be intialized to be
    accessed and terminated to properly release it's resources. This
    class is similar to data resources (e.g. files and data streams). */
public abstract class AbstractResource
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
