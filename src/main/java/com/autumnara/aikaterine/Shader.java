package com.autumnara.aikaterine;

import static org.lwjgl.opengl.GL33.*;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.stream.Collectors;

/** An OpenGL shader.
  *
  * This is a utility class to create and manage shaders safely.
  */
public final class Shader
{

    /** The shader reference.
      */
    public final int reference;

    /** Constructor for a shader from a resource name.
      *
      * This constructor requires an active OpenGL context. The shader
      * will be sourced and compiled.
      *
      * @param filename the filename of the resource, relative to the
      *                 resource directory. This value should
      *                 <i>not</i> begin with a backslash.
      * @param type the type of shader.
      */
    public Shader(String resourceFilename,
                  int    type)
    {
        this.reference = glCreateShader(type);

        glShaderSource(this.reference,
                       this.getResourceAsString(resourceFilename));
        
        glCompileShader(this.reference);
    }

    /** Retireve a resource included with the final JAR as a string.
      *
      * @param filename the filename of the resource, relative to the
      *                 resource directory. This value should
      *                 <i>not</i> begin with a backslash.
      *
      * @return The contents of the resource
      */
    private final String getResourceAsString(String filename)
    {
        return new BufferedReader(new InputStreamReader(this.getClass().getResourceAsStream("/" + filename)))
            .lines()
            .collect(Collectors.joining("\n"));
    }

}
