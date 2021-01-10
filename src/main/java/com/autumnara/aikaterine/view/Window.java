package com.autumnara.aikaterine.view;

import com.autumnara.aikaterine.shared.AbstractResource;

import java.nio.IntBuffer;

import org.lwjgl.opengl.GL;
import static org.lwjgl.glfw.GLFW.*;
import static org.lwjgl.opengl.GL33.*;
import static org.lwjgl.system.MemoryUtil.NULL;
import static org.lwjgl.BufferUtils.createIntBuffer;

/** A window.
  */
final class Window extends AbstractResource
{

    /** The window title.
      */
    private final static String TITLE = "Aikaterine";

    /** The OpenGL major version.
      */
    private final static int GL_VERSION_MAJOR = 3;

    /** The OpenGL minor version.
      */
    private final static int GL_VERSION_MINOR = 3;

    /** The GLFW window ID.
      */
    private long windowId;

    /** The primary OpenGL viewport.
      */
    private Viewport viewport;

    /** Constructor for a window.
      *
      * Does not create, initialize, or display this window.
      */
    Window() {}

    /** Create and initialize the window with GLFW and OpenGL.
      */
    @Override
    protected void onInitialize()
    {
        glfwInit();
        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR,
                       GL_VERSION_MINOR);
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR,
                       GLFW_CONTEXT_VERSION_MINOR);
        glfwWindowHint(GLFW_OPENGL_PROFILE,        // Do not use OpenGL compatibility mode
                       GLFW_OPENGL_CORE_PROFILE);
		glfwWindowHint(GLFW_MAXIMIZED, GLFW_TRUE); // Start the window maximized

        this.windowId = glfwCreateWindow(100, 100, TITLE, // Create this window
                                         NULL, NULL);
        glfwMakeContextCurrent(this.windowId);             // Set OpenGL to use this window
        GL.createCapabilities();                           // Initialize OpenGL

        IntBuffer initial_width = createIntBuffer(1);
        IntBuffer initial_height = createIntBuffer(1);
        glfwGetFramebufferSize​(windowId, initial_width, initial_height); // Get the size of this window
        this.viewport = new Viewport(initial_width.get(0),
                                     initial_height.get(0));
        this.viewport.activate();

        glfwSetFramebufferSizeCallback(this.windowId,             // Resize the OpenGL drawing area whenever the window changes
                                       (window, width, height) ->
        {
            this.viewport = new Viewport(width, height);
            this.viewport.activate();
        });

        glClearColor(0.0f, 0.0f, 0.0f, 0.0f); // Set the OpenGL clear color to black
    }


    /** Run the application until a window close is requested.
     */
    void loop()
    {
        this.assertActive();
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
