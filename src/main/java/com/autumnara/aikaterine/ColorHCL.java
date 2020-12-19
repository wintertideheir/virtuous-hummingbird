package com.autumnara.aikaterine;

import static java.lang.Math.*;

/** A color in hue-saturation-lightness representation.
    This class is based on the
    <a href="https://en.wikipedia.org/wiki/Ostwald_color_system">
        Ostwald color system
    </a>,
    and therefore limits the sum of the the chroma and the distance of
    the lightness from {@code 0.5f} to {@code 1f}. This class is naive
    to singularities in HCL color space, so {@link #equals} will only
    compare the {@link #hue}, {@link #chroma}, and {@link #lightness}
    against itself. */
public final class ColorHCL
{

    /** The normalized hue.
        Must be between {@code 0f} and {@code 1f}. */
    public final float hue;

    /** The normalized chroma.
        Must be between {@code 0f} and {@code 1f}. */
    public final float chroma;

    /** The normalized lightness.
        Must be between {@code 0f} and {@code 1f}. */
    public final float lightness;

    /** Constructor for an HCL color from it's components.
        @param hue       the normalized hue.
                         Must be between {@code 0f} and {@code 1f}.
        @param chroma    the normalized chroma
                         Must be between {@code 0f} and {@code 1f}.
                         Cannot be greater than
                         {@code (1 - 2 * abs(lightness - 0.5))}.
        @param lightness the normalized lightness
                         Must be between {@code 0f} and {@code 1f}. */
    public ColorHCL(float hue,
                    float chroma,
                    float lightness)
    {
        if (hue < 0f || hue > 1f)
        {
            throw new IllegalArgumentException("Hue passed to HCL color constuctor wasn't normalized.");
        }
        if (chroma < 0f || chroma > 1f)
        {
            throw new IllegalArgumentException("Chroma passed to HCL color constuctor wasn't normalized.");
        }
        if (lightness < 0f || lightness > 1f)
        {
            throw new IllegalArgumentException("Lightness passed to HCL color constuctor wasn't normalized.");
        }
        if (((2 * abs(lightness - 0.5f)) + chroma) > 1f)
        {
            throw new IllegalArgumentException("Illegal combination of chroma and lightness." +
                                               "The chroma and twice the distance of the lightness from 0.5 cannot sum to more than 1. ");
        }

        this.hue       = hue;
        this.chroma    = chroma;
        this.lightness = lightness;
    }

}
