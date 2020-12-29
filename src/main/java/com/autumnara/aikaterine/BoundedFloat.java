package com.autumnara.aikaterine;

import static java.lang.Math.*;

/** A float bounded within a certain range. */
public final class BoundedFloat
{

    /** The maximum value. */
    public final float maximum;

    /** The minimum value. */
    public final float minimum;

    /** The current value. */
    private float value;

    /** Constructor for a bounded float.
        @param minimum the minimum value. Must be less than the
                       maximum.
        @param value   the initial value. Must be between the minimum
                       and maximum.
        @param maximum the maximum value */
    public BoundedFloat(float minimum,
                        float value,
                        float maximum)
    {
        if (!(minimum < maximum))
        {
            throw new IllegalArgumentException("Bounded float minimum must be less than the maximum.");
        }

        this.minimum = minimum;
        this.maximum = maximum;

        this.setValue(value);
    }

    /** Get the value. */
    public float getValue()
    {
        return this.value;
    }

    /** Set the value.
        @param value the new value. Must be between the minimum and
                     maximum. */
    public void setValue(float value)
    {
        if ((this.minimum > value) || (this.maximum < value))
        {
            throw new IllegalArgumentException("Bounded float value must be between minimum and maximum.");
        }

        this.value = value;
    }

    /** Set the value, safely. This method will not throw an exception
        if the value is not within the range, and instead limit it to
        the new minimum or maximum.
        @param value the proposed new value */
    public void setValueSafe(float value)
    {
        this.value = max(this.minimum, min(value, this.maximum));
    }

}
