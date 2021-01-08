package com.autumnara.aikaterine;

import static org.lwjgl.opengl.GL33.*;

/** An OpenGL shader.
  *
  * This is a utility class to create and manage shaders safely.
  * Calling {@link java.lang.AutoCloseable#close} is recommended after
  * this shader is no longer needed. E.g.
  * <pre> {@code
  *     try (Shader s = new Shader(pathname, type))
  *     {
  *         \\ Use the shader here ...
  *     }
  * } </pre>
  *
  * <b>Note:</b> this class is autoinitialized.
  */
public final class Shader extends AbstractResource
{

    /** The shader reference.
      */
    private final int reference;

    /** Constructor for a shader from a resource name.
      *
      * This constructor requires an active OpenGL context. The shader
      * will be sourced and compiled.
      *
      * @param resourceFilename the filename of the resource, relative
      *                         to the resource directory. This value
      *                         should <i>not</i> begin with a
      *                         backslash.
      * @param type the type of shader.
      */
    public Shader(String resourceFilename,
                  int    type)
    {
        this.reference = glCreateShader(type);

        glShaderSource(this.reference,
                       AbstractResource.getResourceAsString(resourceFilename));
        
        glCompileShader(this.reference);
    }

    /** Get the OpenGL shader reference.
      */
    public int getReference()
    {
        this.assertActive();
        return this.reference;
    }

    @Override
    protected void onInitialize() {}

    @Override
    protected void onTerminate()
    {
        glDeleteShader(this.reference);
    }

}
