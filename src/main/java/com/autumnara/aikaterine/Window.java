package com.autumnara.aikaterine;

import java.nio.IntBuffer;

import org.lwjgl.opengl.GL;
import static org.lwjgl.glfw.GLFW.*;
import static org.lwjgl.opengl.GL33.*;
import static org.lwjgl.system.MemoryUtil.NULL;
import static org.lwjgl.BufferUtils.*;

public final class Window
{

    /** The minimum major OpenGL version. */
    public final static int OPENGL_VERSION_MAJOR = 3;

    /** The minimum minor OpenGL version. */
    public final static int OPENGL_VERSION_MINOR = 3;

    /** The minimum width of the window in screen coordinates. */
    public final int minimumWidth;

    /** The minimum height of the window in screen coordinates. */
    public final int minimumHeight;

    /** The window title. */
    public final String title;

    /** The root component being drawn. */
    private UIComponent root;

    /** The GLFW window ID. */
    private long windowId;

    public Window(int minimumWidth,
                  int minimumHeight,
                  String title,
                  UIComponent root)
    {
        this.minimumWidth  = minimumWidth;
        this.minimumHeight = minimumHeight;
        this.title         = title;
        this.root          = root;
    }

    public void create()
    {
        // Initialize GLFW
        glfwInit();
        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, OPENGL_VERSION_MAJOR);
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, OPENGL_VERSION_MINOR);
        glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
		glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE);

        // Create and configure this window
        this.windowId = glfwCreateWindow(this.minimumWidth,
                                         this.minimumHeight,
                                         this.title, NULL, NULL);
        glfwSetWindowSizeLimits(this.windowId,
                                this.minimumWidth,
                                this.minimumHeight,
                                GLFW_DONT_CARE,
                                GLFW_DONT_CARE);
        glfwMaximizeWindow(this.windowId);
        glfwMakeContextCurrent(this.windowId);

        // Initialize OpenGL
        GL.createCapabilities();

        // Configure OpenGL
        IntBuffer pixels_width = createIntBuffer(1);
        IntBuffer pixels_height = createIntBuffer(1);
        glfwGetFramebufferSize​(this.windowId, pixels_width, pixels_height);
        glViewport(0, 0, pixels_width.get(0), pixels_height.get(0));
        glfwSetFramebufferSizeCallback(this.windowId, (window, width, height) ->
        {
            glViewport(0, 0, width, height);
            root.setDrawingArea(-1, -1, 1, 1);
        });
        glClearColor(0.0f, 0.0f, 0.0f, 0.0f);

        // Initialize the root component
        root.initialize();
        root.setDrawingArea(-1, -1, 1, 1);
    }

    public void loop()
    {
        while (!glfwWindowShouldClose(this.windowId))
        {
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
			glfwSwapBuffers(this.windowId);
			glfwPollEvents();
            root.draw();
		}
    }

    public void destroy()
    {
        root.terminate();
		glfwTerminate();
    }

}