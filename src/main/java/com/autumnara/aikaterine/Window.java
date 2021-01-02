package com.autumnara.aikaterine;

import java.nio.IntBuffer;

import org.lwjgl.opengl.GL;
import static org.lwjgl.glfw.GLFW.*;
import static org.lwjgl.opengl.GL33.*;
import static org.lwjgl.system.MemoryUtil.NULL;
import static org.lwjgl.BufferUtils.*;

/** A window.
  */
public final class Window extends AbstractResource
{

    /** The window title.
      */
    public final String title;

    /** The GLFW window ID.
      */
    private long windowId;

    /** Constructor for a window.
      *
      * Does not create, initialize, or display this window.
      *
      * @param title the window title.
      */
    public Window(int minimumWidth,
                  int minimumHeight,
                  String title)
    {
        this.minimumWidth  = minimumWidth;
        this.minimumHeight = minimumHeight;
        this.title         = title;
    }

    /** Create and initialize the window with GLFW and OpenGL.
      */
    @Override
    protected void onInitialize()
    {
        glfwInit();                                    // Initialize GLFW
        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3); // Request OpenGL 3.x
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3); // Request OpenGL x.3
        glfwWindowHint(GLFW_OPENGL_PROFILE,            // Do not use OpenGL compatibility mode
                       GLFW_OPENGL_CORE_PROFILE);
		    glfwWindowHint(GLFW_MAXIMIZED, GLFW_TRUE);     // Start the window maximized

        this.windowId = glfwCreateWindow(100, 100, this.title, // Create this window
                                         NULL, NULL);
        glfwMakeContextCurrent(this.windowId);                 // Set OpenGL to use this window
        GL.createCapabilities();                               // Initialize OpenGL

        IntBuffer pixels_width = createIntBuffer(1);
        IntBuffer pixels_height = createIntBuffer(1);
        glfwGetFramebufferSize​(this.windowId, pixels_width, pixels_height); // Find out the dimensions of the window
        glViewport(0, 0, pixels_width.get(0), pixels_height.get(0));        // Set the OpenGL drawing area to the window
        glfwSetFramebufferSizeCallback(this.windowId,                       // Resize the OpenGL drawing area whenever the window changes
                                       (window, width, height) ->
        {
            glViewport(0, 0, width, height);
        });

        glClearColor(0.0f, 0.0f, 0.0f, 0.0f); // Set the OpenGL clear color to black
    }


    /** Run the application until a window close is requested.
     */
    public void loop()
    {
        this.assertInitialized();
        while (!glfwWindowShouldClose(this.windowId))
        {
			      glClear(GL_COLOR_BUFFER_BIT);
			      glfwSwapBuffers(this.windowId);
			      glfwPollEvents();
		    }
    }

    /** Destroy the window and free it's resources.
     */
    @Override
    protected void onTerminate()
    {
    		glfwTerminate();
    }

}