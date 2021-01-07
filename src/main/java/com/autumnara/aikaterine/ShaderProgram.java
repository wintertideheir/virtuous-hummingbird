package com.autumnara.aikaterine;

import static org.lwjgl.opengl.GL33.*;

/** An OpenGL shader program.
  *
  * This is a utility class to create and manage shaders programs safely.
  */
public final class ShaderProgram extends AbstractResource
{

    /** The shader program reference.
      */
    public int reference;

    /** Constructor for a shader program from an array of shaders.
      *
      * This constructor requires an active OpenGL context. The shader
      * will be sourced and compiled.
      *
      * @param shaders an array of non-null shaders
      */
    public ShaderProgram(Shader[] shaders)
    {
        this.reference = glCreateProgram();

        for (Shader shader : shaders)
        {
            glAttachShader(this.reference, shader.reference);
        }

        glLinkProgram(this.reference);

        for (Shader shader : shaders)
        {
            glDetachShader(this.reference, shader.reference);
        }

        this.initialize();
    }

    @Override
    protected void onInitialize() {}

    @Override
    protected void onTerminate()
    {
        glDeleteProgram(this.reference);
    }

    /** Use this shader program.
      */
    public void use()
    {
        this.assertActive();
        glUseProgram(this.reference);
    }

    /** Set a 1-dimensional uniform.
      *
      * @param location the location of the uniform
      * @param x the x value of the unifrom
      */
    public void uniform(int   location,
                        float x)
    {
        this.use();
        glUniform1f(location, x);
    }

    /** Set a 2-dimensional uniform.
      *
      * @param location the location of the uniform
      * @param x the x value of the unifrom
      * @param y the y value of the unifrom
      */
    public void uniform(int   location,
                        float x,
                        float y)
    {
        this.use();
        glUniform2f(location, x, y);
    }

    /** Set a 3-dimensional uniform.
      *
      * @param location the location of the uniform
      * @param x the x value of the unifrom
      * @param y the y value of the unifrom
      * @param z the z value of the unifrom
      */
    public void uniform(int   location,
                        float x,
                        float y,
                        float z)
    {
        this.use();
        glUniform3f(location, x, y, z);
    }

    /** Set a 4-dimensional uniform.
      *
      * @param location the location of the uniform
      * @param x the x value of the unifrom
      * @param y the y value of the unifrom
      * @param z the z value of the unifrom
      * @param w the w value of the unifrom
      */
    public void uniform(int   location,
                        float x,
                        float y,
                        float z,
                        float w)
    {
        this.use();
        glUniform4f(location, x, y, z, w);
    }

}
