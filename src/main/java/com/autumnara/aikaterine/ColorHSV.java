package com.autumnara.aikaterine;

/** A color in hue-saturation-value representation.
    {@link #equals} is naive to singularities in HSV color space. */
public final class ColorHSV
{

    /** The normalized hue.
        Must be between {@code 0f} and {@code 1f}. */
    public final float hue;

    /** The normalized saturation.
        Must be between {@code 0f} and {@code 1f}. */
    public final float saturation;

    /** The normalized value.
        Must be between {@code 0f} and {@code 1f}. */
    public final float value;

    /** Constructor for an HSV color from it's components.
        @param hue        the normalized hue.
                          Must be between {@code 0f} and {@code 1f}.
        @param saturation the normalized saturation. 
                          Must be between {@code 0f} and {@code 1f}.
        @param value      the normalized value.
                          Must be between {@code 0f} and {@code 1f}. */
    public ColorHSV(float hue,
                    float saturation,
                    float value)
    {
        if (hue < 0f || hue > 1f)
        {
            throw new IllegalArgumentException("Hue passed to HSV color constuctor wasn't normalized.");
        }
        if (saturation < 0f || saturation > 1f)
        {
            throw new IllegalArgumentException("Saturation passed to HSV color constuctor wasn't normalized.");
        }
        if (value < 0f || value > 1f)
        {
            throw new IllegalArgumentException("Value passed to HSV color constuctor wasn't normalized.");
        }

        this.hue        = hue;
        this.saturation = saturation;
        this.value      = value;
    }

}
