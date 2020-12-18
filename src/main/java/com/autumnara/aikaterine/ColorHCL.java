package com.autumnara.aikaterine;

/** A color in hue-saturation-lightness representation.
    This class is naive to singularities in HCL color space, so
    {@link #equals} will only compare the {@link #hue}, {#link chroma},
    and {#link lightness} against itself. */
public final class ColorHCL
{

    /** The normalized hue.
        Must be between 0f and 1f. */
    public final float hue;

    /** The normalized chroma of this color.
        Must be between 0f and 1f. */
    public final float chroma;

    /** The normalized lightness of this color.
        Must be between 0f and 1f. */
    public final float lightness;

    /** Constructor for an HCL color from it's components.
        @param hue       the normalized hue.
                         Must be between 0f and 1f.
        @param chroma    the normalized chroma
                         Must be between 0f and 1f.
        @param lightness the normalized lightness
                         Must be between 0f and 1f. */
    public ColorHCL(float hue,
                    float chroma,
                    float lightness)
    {
        if (hue < 0 || hue > 1)
        {
            throw new IllegalArgumentException("Hue passed to HCL color constuctor wasn't normalized.");
        }
        if (chroma < 0 || chroma > 1)
        {
            throw new IllegalArgumentException("Chroma passed to HCL color constuctor wasn't normalized.");
        }
        if (lightness < 0 || lightness > 1)
        {
            throw new IllegalArgumentException("Lightness passed to HCL color constuctor wasn't normalized.");
        }

        this.hue       = hue;
        this.chroma    = chroma;
        this.lightness = lightness;
    }

}
