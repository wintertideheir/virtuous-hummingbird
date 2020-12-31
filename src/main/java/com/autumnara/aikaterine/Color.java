package com.autumnara.aikaterine;

/** A color in red-green-blue representation.
  */
public final class Color
{

    /** The normalized red component.
      *
      * Must be between 0f and 1f.
      */
    public final float red;

    /** The normalized blue component.
      *
      * Must be between 0f and 1f.
      */
    public final float blue;

    /** The normalized green component.
      *
      * Must be between 0f and 1f.
      */
    public final float green;

    /** Constructor for a color from RGB components.
      *
      * @param red   the normalized red component.
      *              Must be between 0f and 1f.
      * @param blue  the normalized blue component.
      *              Must be between 0f and 1f.
      * @param green the normalized green component.
      *              Must be between 0f and 1f.
      */
    public Color(float red,
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

    /** Create a RGB color from hue, saturation, and value.
      *
      * @param hue        the normalized hue.
      *                   Must be between {@code 0f} and {@code 1f}.
      * @param saturation the normalized saturation. 
      *                   Must be between {@code 0f} and {@code 1f}.
      * @param value      the normalized value.
      *                   Must be between {@code 0f} and {@code 1f}.
      */
    public static Color fromHSV(float hue,
                                float saturation,
                                float value)
    {
        if (hue < 0f || hue > 1f)
        {
            throw new IllegalArgumentException("Hue is not between 0 and 1.");
        }
        if (saturation < 0f || saturation > 1f)
        {
            throw new IllegalArgumentException("Saturation is not between 0 and 1.");
        }
        if (value < 0f || value > 1f)
        {
            throw new IllegalArgumentException("Value is not between 0 and 1.");
        }

        float chroma = saturation * value;

        float a = hue * 6;
        float b = chroma * (1 - Math.abs((a % 2) - 1));
        float c = value - chroma;

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
