package com.autumnara.aikaterine;

import static org.lwjgl.opengl.GL33.*;

/** A view of a virtue graph.
  *
  * TODO: Complete this class.
  */
public final class VirtueGraphView extends AbstractView
{

    private final VirtueGraph graph;

    private final Position center = new Position(0f, 0f);

    private final float scale;

    public final float scaleMinimum;

    public final float scaleMaximum;

    private ShaderProgram nodeShader;

    private ShaderProgram linkShader;

    /** Constructor for a virtue graph view.
      *
      * @param graph           the virtue graph to be rendered
      * @param scaleMinimum    the minimum view scale
      * @param scale           the inital view scale. The view scale is
      *                        the ratio of the radius of drawn virtue
      *                        node (defined as 1) to the shortest
      *                        dimension of this view's viewport. This
      *                        value must always be positive.
      * @param scaleMaximum    the maximum view scale
      */
    public VirtueGraphView(VirtueGraph graph,
                           float       scaleMinimum,
                           float       scale,
                           float       scaleMaximum)
    {
        if (scaleMinimum > scaleMaximum)
        {
            throw new IllegalArgumentException("Virtue graph view scale minimum must be less than or equal to the scale maximum.");
        }
        if (scale > scaleMaximum)
        {
            throw new IllegalArgumentException("Virtue graph view initial scale must be less than or equal to the scale maximum.");
        }
        if (scaleMinimum > scaleMaximum)
        {
            throw new IllegalArgumentException("Virtue graph view initial scale must be greater than or equal to the scale minimum.");
        }
        if ((scaleMinimum <= 0) ||
            (scale        <= 0) ||
            (scaleMaximum <= 0))
        {
            throw new IllegalArgumentException("Virtue graph view scale minimum, initial scale, and scale maximum must be positive. ");
        }

        this.graph        = graph;
        this.scaleMinimum = scaleMinimum;
        this.scale        = scale;
        this.scaleMaximum = scaleMaximum;
    }

    @Override
    protected void onInitialize()
    {
        try (Shader vertex   = new Shader("NodeVertex.glsl",   GL_VERTEX_SHADER);
             Shader fragment = new Shader("NodeFragment.glsl", GL_FRAGMENT_SHADER))
        {
            this.nodeShader = new ShaderProgram(new Shader[] {vertex, fragment});
        }

        try (Shader vertex   = new Shader("LinkVertex.glsl",   GL_VERTEX_SHADER);
             Shader fragment = new Shader("LinkFragment.glsl", GL_FRAGMENT_SHADER))
        {
            this.linkShader = new ShaderProgram(new Shader[] {vertex, fragment});
        }

    }

    @Override
    protected void onTerminate()
    {
        glDeleteProgram(this.nodeShader.reference);
        glDeleteProgram(this.linkShader.reference);
    }

    @Override
    protected void onRender() {}

    @Override
    protected void onSetViewport() {}

}
