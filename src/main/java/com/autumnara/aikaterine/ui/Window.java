package com.autumnara.hummingbird.ui;

import com.autumnara.hummingbird.shared.AbstractResource;

import java.nio.IntBuffer;
import java.util.concurrent.TimeUnit;

import org.lwjgl.opengl.GL;
import org.lwjgl.glfw.GLFWErrorCallback;
import org.lwjgl.system.MemoryStack;
import static org.lwjgl.glfw.GLFW.*;
import static org.lwjgl.opengl.GL33.*;
import static org.lwjgl.system.MemoryUtil.NULL;

/** A window.
  */
final class Window
{

    /** The minimum interval between frames, in nanoseconds.
      *
      * A little faster than, but approximately equal to 60 frames per
      * second. This calculation should be interpreted as the integer
      * component of the division of the number of nanoseconds per
      * second divided by the number of frames per second.
      */
    private final static long FRAME_INTERVAL = 1000000000 / 60;

    /** The GLFW window ID.
      */
    private long windowId;

    /** The primary OpenGL viewport.
      */
    private Viewport viewport;

    /** The start time of the current frame in milliseconds.
      */
    private long startTime = 0;

    /** The end time of the previous frame in milliseconds.
      */
    private long endTime = 0;

    /** Constructor for a window.
      *
      * Does not create, initialize, or display this window.
      */
    Window() {}

    /** Open this window.
      */
    final void open()
    {
        this.readyGLFW();
        this.openWindow();
        this.readyViewport();
        this.readyOpenGL();
    }

    /** Initialze and setup GLFW.
      */
    private final void readyGLFW()
    {
        // Set GLFW's error callback
        GLFWErrorCallback.createThrow().set();

        // Initialize GLFW
        glfwInit();
    }

    /** Open this window with the appropriate parameters.
      */
    private final void openWindow()
    {
        // Ready GLFW to create a window
        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3); // Request OpenGL 3.x
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3); // Request OpenGL x.3
        glfwWindowHint(GLFW_OPENGL_PROFILE,            // Do not use OpenGL compatibility mode
                       GLFW_OPENGL_CORE_PROFILE);
		glfwWindowHint(GLFW_MAXIMIZED, GLFW_TRUE);     // Start the window maximized

        // Create a window and ready it for rendering
        this.windowId =
            glfwCreateWindow(100, 100, "Virtuous Hummingbird", // Create this window
                             NULL, NULL);
        glfwMakeContextCurrent(this.windowId);       // Set OpenGL to use this window
    }

    /** Setup up the viewport.
      */
    private final void readyViewport()
    {
        // Setup the viewport
		try (MemoryStack stack = MemoryStack.stackPush())
        {
            IntBuffer initial_width  = stack.mallocInt(1);
            IntBuffer initial_height = stack.mallocInt(1);
            glfwGetFramebufferSize​(this.windowId,
                                   initial_width,
                                   initial_height);
            this.viewport = new Viewport(initial_width.get(0),
                                         initial_height.get(0));
        }

        // Set the framebuffer resize callback
        glfwSetFramebufferSizeCallback(this.windowId,
                                       (window, width, height) ->
        {
            this.viewport = new Viewport(width, height);
        });
    }

    /** Setup OpenGL.
      */
    private final void readyOpenGL()
    {
        GL.createCapabilities();              // Initialize OpenGL
        glClearColor(0.0f, 0.0f, 0.0f, 0.0f); // Set the OpenGL clear color to black
    }

    /** Run the application until a window close is requested.
     */
    final void loop()
    {
        while (!glfwWindowShouldClose(this.windowId))
        {
            this.limitFramerate();

            glClear(GL_COLOR_BUFFER_BIT);
            // Draw here ...
			glfwSwapBuffers(this.windowId);
			glfwPollEvents();

            this.resetFramerate();
		}
    }

    /** Limit the framerate.
      *
      * Sleep this thread so that the frame rate never exceeds 60
      * frames per second.
      */
    private final void limitFramerate()
    {
        try
        {
            System.out.print(" ");
            TimeUnit.NANOSECONDS.sleep(this.endTime -
                                       this.startTime -
                                       Window.FRAME_INTERVAL);
        }
        catch (InterruptedException e)
        {
            throw new Error(e);
        }

        this.startTime = System.nanoTime();
    }

    /** Reset the framerate measurement system.
      */
    private final void resetFramerate()
    {
        this.endTime = System.nanoTime();
    }

    /** Destroy the window and free it's resources.
     */
    final void close()
    {
        glfwTerminate();
    }

}
