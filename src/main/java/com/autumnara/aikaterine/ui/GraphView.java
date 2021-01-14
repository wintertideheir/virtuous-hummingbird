package com.autumnara.hummingbird.ui;

import static org.lwjgl.opengl.GL33.*;

/** A view of a graph.
  */
final class GraphView extends AbstractView
{

    private final float scale;

    private ShaderProgram nodeShader;

    private ShaderProgram linkShader;

    private int nodeShader_offset;

    private int nodeShader_scale;

    private int nodeShader_color;

    /** Constructor for a virtue graph view.
      *
      * @param scale The inital view scale. The view scale is the ratio
      *              of the radius of drawn virtue node (defined as 1)
      *              to the shortest dimension of this view's viewport.
      *              This value must always be positive.
      */
    GraphView(float scale)
    {
        if (scale <= 0)
        {
            throw new IllegalArgumentException("Virtue graph view scale minimum, initial scale, and scale maximum must be positive. ");
        }

        this.scale        = scale;
    }

    @Override
    protected void onInitialize()
    {
        try (Shader vertex   = new Shader("NodeVertex.glsl",   GL_VERTEX_SHADER);
             Shader fragment = new Shader("NodeFragment.glsl", GL_FRAGMENT_SHADER))
        {
            this.nodeShader = new ShaderProgram(new Shader[] {vertex, fragment});
        }

        this.nodeShader_offset = this.nodeShader.getUniformLocation("offset");
        this.nodeShader_scale  = this.nodeShader.getUniformLocation("scale");
        this.nodeShader_color  = this.nodeShader.getUniformLocation("color");

        try (Shader vertex   = new Shader("LinkVertex.glsl",   GL_VERTEX_SHADER);
             Shader fragment = new Shader("LinkFragment.glsl", GL_FRAGMENT_SHADER))
        {
            this.linkShader = new ShaderProgram(new Shader[] {vertex, fragment});
        }
    }

    @Override
    protected void onTerminate()
    {
        this.nodeShader.terminate();
        this.linkShader.terminate();
    }

    @Override
    protected void onRender() {}

    @Override
    protected void onSetViewport() {}

    /** Draw a node at the selected position.
      *
      * @param offset the offset of the node relative to the center of
      *               the view, in world coordinates
      * @param color  the color of the node
      */
/* TODO: Replace references to the Position and Color classes.
    void drawNode(Position offset,
                  Color    color)
    {
        this.assertActive();

        float viewport_smaller_dimension =
              this.viewport.width > this.viewport.height
            ? this.viewport.height
            : this.viewport.width;
        float node_width = scale * viewport_smaller_dimension;

        this.nodeShader.uniform(this.nodeShader_offset,
                                offset.x,
                                offset.y);
        this.nodeShader.uniform(this.nodeShader_scale,
                                node_width / this.viewport.width,
                                node_width /this.viewport.height);
        this.nodeShader.uniform(this.nodeShader_color,
                                color.red,
                                color.green,
                                color.blue);
    }
*/

}
