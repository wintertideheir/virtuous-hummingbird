package com.autumnara.aikaterine;

import static org.lwjgl.glfw.GLFW.*;
import static org.lwjgl.opengl.GL33.*;

import static java.lang.Math.*;

public class UIGraph extends UIComponent
{

    /** The graph being drawn. */
    private VirtueGraph graph;

    /** Where the view is centered over the graph. */
    private PositionRectangular center;

    /** How many nodes can be drawn across the shortest window dimension.
        Each node is drawn with a square mesh. */
    private float scale;

    /** The minimum value for {@link #scale} */
    private final float scaleMin;

    /** The maximum value for {@link #scale} */
    private final float scaleMax;

    /** The vertex array object (VAO) for the rectangle mesh used to
        draw both nodes and links. */
    private int rectangleVAO;

    /** The shader program to draw nodes. */
    private int nodeShaderProgram;

    /** The shader program to draw links. */
    private int linkShaderProgram;

    /** Constructor for a UI graph component.
        @param graph the graph being drawn
        @param scale the initial scale of this component. The scale
                     determines how many nodes can be drawn across the
                     shortest window dimension.
        @param scaleMin the minimum value of the scale
        @param scaleMax the maximum value of the scale */
    public UIGraph(VirtueGraph graph,
                   float scale,
                   float scaleMin,
                   float scaleMax)
    {
        this.graph = graph;
        this.center = new PositionRectangular(0, 0);
        if (scaleMin > scaleMax)
        {
            throw new IllegalArgumentException("Minimum scale larger than maximum scale for UIGraph.");
        }
        this.scaleMin = scaleMax;
        this.scaleMax = scaleMin;
        this.setScale(scale);
    }

    /** Setter for the graph scale. Will ensure the scale lies between
        it's minimum and maximum. */
    private void setScale(float scale)
    {
        this.scale = min(this.scaleMax, max(this. scaleMin, scale));
    }

    @Override
    protected void onInitialize()
    {
        // Create a rectangular model for both nodes and links.
        float[] rectangle =
        {
            // Triangle 1
            -1,  1, // Vertex 1
             1,  1, // Vertex 2
             1, -1, // Vertex 3
            // Triangle 2
            -1,  1, // Vertex 1
             1, -1, // Vertex 3
            -1, -1, // Vertex 4
        };
        int rectangleVBO = glGenBuffers();
        this.rectangleVAO = glGenVertexArrays();
        glBindVertexArray(this.rectangleVAO);
        glBindBuffer(GL_ARRAY_BUFFER, rectangleVBO);
        glBufferData(GL_ARRAY_BUFFER, rectangleVBO, GL_STATIC_DRAW);
        glVertexAttribPointer(0, 2, GL_FLOAT, false, 0, 0);
        glEnableVertexAttribArray(0);

        // Create shaders
        int nodeVertexShader   = glCreateShader(GL_VERTEX_SHADER);
        int nodeFragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
        int linkVertexShader   = glCreateShader(GL_VERTEX_SHADER);
        int linkFragmentShader = glCreateShader(GL_FRAGMENT_SHADER);

        glShaderSource(nodeVertexShader,
                       "#version 330 core"
                     + "layout (location = 0) in vec2 pos;"
                     + "void main()"
                     + "{"
                     + "    gl_Position = vec4(pos.x, pos.y, 0.0, 1.0);"
                     + "}");
        glShaderSource(nodeFragmentShader,
                       "#version 330 core"
                     + "out vec4 color;"
                     + "void main()"
                     + "{"
                     + "    color = vec4(1.0f, 1.0f, 1.0f, 1.0f);"
                     + "}");
        glShaderSource(linkVertexShader,
                       "#version 330 core"
                     + "layout (location = 0) in vec2 pos;"
                     + "void main()"
                     + "{"
                     + "    gl_Position = vec4(pos.x, pos.y, 0.0, 1.0);"
                     + "}");
        glShaderSource(linkFragmentShader,
                       "#version 330 core"
                     + "out vec4 color;"
                     + "void main()"
                     + "{"
                     + "    color = vec4(1.0f, 1.0f, 1.0f, 1.0f);"
                     + "}");

        glCompileShader(nodeVertexShader);
        glCompileShader(nodeFragmentShader);
        glCompileShader(linkVertexShader);
        glCompileShader(linkFragmentShader);

        // Create programs
        this.nodeShaderProgram = glCreateProgram();
        glAttachShader(this.nodeShaderProgram, nodeVertexShader);
        glAttachShader(this.nodeShaderProgram, nodeFragmentShader);
        glLinkProgram(this.nodeShaderProgram);
        glDeleteShader(this.nodeShaderProgram);
        glDeleteShader(this.nodeShaderProgram);

        this.linkShaderProgram = glCreateProgram();
        glAttachShader(this.linkShaderProgram, linkVertexShader);
        glAttachShader(this.linkShaderProgram, linkFragmentShader);
        glLinkProgram(this.linkShaderProgram);
        glDeleteShader(this.linkShaderProgram);
        glDeleteShader(this.linkShaderProgram);
    }

    @Override
    protected void onDraw() {}

    @Override
    protected void onBounded() {}

    @Override
    protected void onTerminate() {}

}
