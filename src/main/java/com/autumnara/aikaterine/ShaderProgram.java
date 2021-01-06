package com.autumnara.aikaterine;

import static org.lwjgl.opengl.GL33.*;

/** An OpenGL shader program.
  *
  * This is a utility class to create and manage shaders programs safely.
  */
public final class ShaderProgram
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
    }

    /** Use this shader program.
      */
    public void use()
    {
        glUseProgram(this.reference);
    }

}
