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

    /** The primary OpenGL viewport.
      */
    private Viewport viewport;

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

        IntBuffer width = createIntBuffer(1);
        IntBuffer height = createIntBuffer(1);
        glfwGetFramebufferSize​(windowId, width, height);        // Get the size of this window
        this.viewport = new Viewport(width.get(0), height.get(0));
        this.viewport.activate();

        glfwSetFramebufferSizeCallback(this.windowId,             // Resize the OpenGL drawing area whenever the window changes
                                       (window, width, height) ->
        {
            this.viewport = new Viewport(width, height);
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