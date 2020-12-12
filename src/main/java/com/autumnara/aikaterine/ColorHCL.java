package com.autumnara.aikaterine;

import java.lang.IllegalArgumentException;

/** A color in hue-saturation-lightness format. */
public final class ColorHCL
{

    /** The normalized hue of this color. */
    public final float hue;

    /** The normalized chroma of this color. */
    public final float chroma;

    /** The normalized lightness of this color. */
    public final float lightness;

    /** Constructor for an HCL color from it's components.
        @param hue       the normalized hue
        @param chroma    the normalized chroma
        @param lightness the normalized lightness */
    public ColorHCL(float hue,
                    float chroma,
                    float lightness)
    {
        if (hue < 0 || hue > 1)
        {
            throw IllegalArgumentException("Hue passed to HCL color constuctor wasn't normalized.");
        }
        if (chroma < 0 || chroma > 1)
        {
            throw IllegalArgumentException("Chroma passed to HCL color constuctor wasn't normalized.");
        }
        if (lightness < 0 || lightness > 1)
        {
            throw IllegalArgumentException("Lightness passed to HCL color constuctor wasn't normalized.");
        }

        this.hue       = hue;
        this.chroma    = chroma;
        this.lightness = lightness;
    }

}
