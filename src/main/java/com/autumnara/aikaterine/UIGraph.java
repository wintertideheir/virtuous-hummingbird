package com.autumnara.aikaterine;

import static org.lwjgl.glfw.GLFW.*;
import static org.lwjgl.opengl.GL33.*;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.stream.Collectors;

import static java.lang.Math.*;

/** A UI component for drawing and handling virtue graphs.
  */
public class UIGraph extends UIComponent
{

    /** The graph being drawn. */
    private VirtueGraph graph;

    /** Where the view is centered over the graph. */
    private PositionRectangular center;

    /** How many nodes can be drawn across the shortest window dimension.
        Each node is drawn with a square mesh. */
    private BoundedFloat scale;

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
                     shortest window dimension. */
    public UIGraph(VirtueGraph graph,
                   BoundedFloat scale)
    {
        this.graph = graph;
        this.center = new PositionRectangular(0, 0);
        if (scale.minimum <= 0)
        {
            throw new IllegalArgumentException("UIGraph scale minimum must always be positive.");
        }
        this.scale = scale;
    }

    private String getResourceAsString(String filename)
    {
        return new BufferedReader(new InputStreamReader(this.getClass().getResourceAsStream("/" + filename)))
            .lines()
            .collect(Collectors.joining("\n"));
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

        glShaderSource(nodeVertexShader,   this.getResourceAsString("NodeVertex.glsl"));
        glShaderSource(nodeFragmentShader, this.getResourceAsString("NodeFragment.glsl"));
        glShaderSource(linkVertexShader,   this.getResourceAsString("LinkVertex.glsl"));
        glShaderSource(linkFragmentShader, this.getResourceAsString("LinkFragment.glsl"));

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
    protected void onSetBoundaries() {}

    @Override
    protected void onSetReference() {}

    @Override
    protected void onTerminate() {}

}
