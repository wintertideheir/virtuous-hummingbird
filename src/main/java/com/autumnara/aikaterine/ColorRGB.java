package com.autumnara.aikaterine;

/** A color in red-green-blue representation. */
public final class ColorRGB
{

    /** The normalized red component.
        Must be between 0f and 1f. */
    public final float red;

    /** The normalized blue component.
        Must be between 0f and 1f. */
    public final float blue;

    /** The normalized green component.
        Must be between 0f and 1f. */
    public final float green;

    /** Constructor for an RGB color from it's components.
        @param red   the normalized red component.
                     Must be between 0f and 1f.
        @param blue  the normalized blue component.
                     Must be between 0f and 1f.
        @param green the normalized green component.
                     Must be between 0f and 1f. */
    public ColorRGB(float red,
                    float blue,
                    float green)
    {
        if (red < 0 || red > 1)
        {
            throw new IllegalArgumentException("Red passed to HCL color constuctor wasn't normalized.");
        }
        if (blue < 0 || blue > 1)
        {
            throw new IllegalArgumentException("Blue passed to HCL color constuctor wasn't normalized.");
        }
        if (green < 0 || green > 1)
        {
            throw new IllegalArgumentException("Green passed to HCL color constuctor wasn't normalized.");
        }

        this.red   = red;
        this.blue  = blue;
        this.green = green;
    }

    /** Constructor for a color in RGB representation from a color in
        HSV representation.
        @param color a color in HSV format */
    public ColorRGB(ColorHSV color)
    {
        float chroma = color.saturation * color.value;

        float a = color.hue * 6;
        float b = chroma * (1 - Math.abs((a % 2) - 1));
        float c = color.value - chroma;

        switch ((int) a)
        {
            case 0:
                this.red = chroma;
                this.green = b;
                this.blue = 0;
                break;
            case 1:
                this.red = b;
                this.green = chroma;
                this.blue = 0;
                break;
            case 2:
                this.red = 0;
                this.green = chroma;
                this.blue = b;
                break;
            case 3:
                this.red = 0;
                this.green = b;
                this.blue = chroma;
                break;
            case 4:
                this.red = b;
                this.green = 0;
                this.blue = chroma;
                break;
            case 5:
            case 6:
                this.red = chroma;
                this.green = 0;
                this.blue = b;
                break;
            default:
                throw new IllegalStateException("Color conversion from HSV to RGB failed for unknown reasons.");
        }
    }

}
