package com.autumnara.aikaterine;

import java.lang.IllegalArgumentException;

public class HCL
{
    public final float hue;
    public final float chroma;
    public final float lightness;

    public HCL(float hue,
               float chroma,
               float lightness)
    {
        this.hue       = hue;
        this.chroma    = chroma;
        this.lightness = lightness;
    }
}