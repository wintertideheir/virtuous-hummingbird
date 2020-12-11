package com.autumnara.aikaterine;

/** A component that can be drawn at specific positions. */
public interface UIComponent
{

    /** Make this component drawable. */
    public void initialize();
    
    /** Draw this component with it's present configuration. */
    public void draw();

    /** Set the allocated drawing space by OpenGL window coordinates. */
    public void setDrawingArea(float xMin, float yMin,
                               float xMax, float yMax);

    /** Make this component undrawable and free all of it's resources. */
    public void terminate();

}
