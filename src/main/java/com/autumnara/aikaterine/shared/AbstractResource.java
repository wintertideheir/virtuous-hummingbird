package com.autumnara.hummingbird.shared;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.stream.Collectors;

/** An object that needs to be initialized and terminated.
  *
  * <p> This class has an internal state that must be intialized to be
  * accessed and terminated to properly release it's resources. This
  * class behaves like a data resources (e.g. files and data streams).
  *
  * <p> Resources should strongly avoid throwing exceptions in
  * {@link #onInitialize} and {@link #onTerminate}.
  *
  */
public abstract class AbstractResource
implements AutoCloseable
{

    /** The stage the resource is in.
      */
    private Stage stage = Stage.CONSTRUCTED;

    private enum Stage
    {
        CONSTRUCTED,
        INITIALIZED,
        TERMINATED
    }

    /** Retireve a resource included with the final JAR as a string.
      *
      * <p> For example, to retrieve the resource
      * "src/resource/default.cfg", call
      * {@code this.getResourceAsString("default.cfg")},
      * assuming "src/resource" is the resource directory.
      *
      * @param filename the filename of the resource, relative to the
      *                 resource directory. This value should
      *                 <i>not</i> begin with a backslash.
      *
      * @return The contents of the resource
      */
    public static final String getResourceAsString(String filename)
    {
        return new BufferedReader(new InputStreamReader(AbstractResource.class.getResourceAsStream("/" + filename)))
            .lines()
            .collect(Collectors.joining("\n"));
    }

    /** Initialize this object.
      * 
      * This method checks that this object hasn't already been
      * initialized and then calls {@link #onInitialize}.
      */
    public final void initialize()
    {
        if (this.stage == Stage.INITIALIZED)
        {
            throw new IllegalStateException("Resource being initialized has already been initialized.");
        }
        if (this.stage == Stage.TERMINATED)
        {
            throw new IllegalStateException("Resource being initialized has already been terminated.");
        }
        this.onInitialize();
        this.stage = Stage.INITIALIZED;
    }

    /** Initialize the internals of this object.
      */
    protected abstract void onInitialize();

    /** Assert that this object is active.
      */
    public final void assertActive()
    {
        if (this.stage == Stage.CONSTRUCTED)
        {
            throw new IllegalStateException("Resource has not been initialized.");
        }
        if (this.stage == Stage.TERMINATED)
        {
            throw new IllegalStateException("Resource has been terminated.");
        }
    }

    /** Return whether this object is active.
     */
    public final boolean isActive()
    {
        return this.stage == Stage.INITIALIZED;
    }

    /** Terminate this component.
      *
      * This method calls {@link #onTerminate} and marks this
      * component as terminated.
      */
    public final void terminate()
    {
        if (this.stage == Stage.CONSTRUCTED)
        {
            throw new IllegalStateException("Resource being terminated has not been initialized yet.");
        }
        if (this.stage == Stage.TERMINATED)
        {
            throw new IllegalStateException("Resource being terminated has already been terminated.");
        }

        this.onTerminate();
        this.stage = Stage.TERMINATED;
    }

    /** Terminate this object.
      *
      * Objects should free their resources here.
      */
    protected abstract void onTerminate();

    /** {@inheritDoc}
      *
      * This method merely calls {@link #terminate}.
      */
    @Override
    public void close()
    {
        this.terminate();
    }

}
