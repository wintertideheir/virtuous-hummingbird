package com.autumnara.aikaterine;

/** A component that can be drawn at specific positions. */
public abstract class UIComponent
{

    /** Make this component drawable. */
    public abstract void initialize();
    
    /** Draw this component with it's present configuration. */
    public abstract void draw();

    /** Set the allocated drawing space by OpenGL window coordinates. */
    public abstract void setDrawingArea(float xMin, float yMin,
                                        float xMax, float yMax);

    /** Make this component undrawable and free all of it's resources. */
    public abstract void terminate();

}
