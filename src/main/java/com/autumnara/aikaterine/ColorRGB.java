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
        HCL representation.
        @param color a color in HCL format */
    public ColorRGB(ColorHCL color)
    {
        float a = color.hue * 6;
        float b = color.chroma * (1 - Math.abs((a % 2) - 1));
        float c = color.lightness - (color.chroma / 2);

        switch ((int) a)
        {
            case 0:
                this.red = c + color.chroma;
                this.green = c + b;
                this.blue = c;
                break;
            case 1:
                this.red = c + b;
                this.green = c + color.chroma;
                this.blue = c;
                break;
            case 2:
                this.red = c;
                this.green = c + color.chroma;
                this.blue = c + b;
                break;
            case 3:
                this.red = c;
                this.green = c + b;
                this.blue = c + color.chroma;
                break;
            case 4:
                this.red = c + b;
                this.green = c;
                this.blue = c + color.chroma;
                break;
            case 5:
            case 6:
                this.red = c + color.chroma;
                this.green = c;
                this.blue = c + b;
                break;
            default:
                throw new IllegalArgumentException("HCL to RBG conversion failed because the hue was not normalized.");
        }
    }

}
