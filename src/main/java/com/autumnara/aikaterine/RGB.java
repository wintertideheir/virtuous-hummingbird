package com.autumnara.aikaterine;

import java.lang.IllegalArgumentException;

public class RGB
{
    public final float red;
    public final float blue;
    public final float green;

    public RGB(float red,
               float blue,
               float green)
    {
        this.red   = red;
        this.blue  = blue;
        this.green = green;
    }

    public RGB(HCL color)
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