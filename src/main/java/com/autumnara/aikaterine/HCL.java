package com.autumnara.aikaterine;

/** A color in hue-saturation-lightness format. */
public class HCL
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
    public HCL(float hue,
               float chroma,
               float lightness)
    {
        this.hue       = hue;
        this.chroma    = chroma;
        this.lightness = lightness;
    }
}