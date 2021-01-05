package com.autumnara.aikaterine;

/** A wrapper for a promised value.
  *
  * The promiser and promised should both hold references to this
  * object. Both ought to know when the promised value should be
  * available, and the promised value should be set only once.
  */
public final class Promise<T>
{

    /** A reference to the promised value.
      */
    private T promised;

    /** Get the promised value.
      */
    public T get()
    {
        if (this.promised == null)
        {
            throw new IllegalStateException("Promised value is missing.");
        }

        return this.promised;
    }

    /** Set the promised value.
      *
      * Must only be used used once. Should only be used by the promiser.
      */
    public void set(T promised)
    {
        if (this.promised != null)
        {
            throw new IllegalStateException("Promises cannot be set more than once.");
        }

        this.promised = promised;
    }

}
