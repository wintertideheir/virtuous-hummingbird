package com.autumnara.aikaterine;

import java.nio.IntBuffer;

import org.lwjgl.opengl.GL;
import static org.lwjgl.glfw.GLFW.*;
import static org.lwjgl.opengl.GL33.*;
import static org.lwjgl.system.MemoryUtil.NULL;
import static org.lwjgl.BufferUtils.*;

public final class Window
{

    /** The minimum major OpenGL version. OpenGL 3 was selected because
        it brings important features and a programming paradigm shift
        over OpenGL 2 while providing widespread compatibility over
        OpenGL 4. */
    public final static int VERSION_MAJOR = 3;

    /** The minimum minor OpenGL version. */
    public final static int VERSION_MINOR = 3;

    /** The default width of the window in screen coordinates (assumed
        to be equivalent to pixels). */
    public final static int MINIMUM_WITDH = 600;

    /** The default height of the window in screen coordinates (assumed
        to be equivalent to pixels). */
    public final static int MINIMUM_HEIGHT = 400;

    /** The title of the window. */
    public final static String WINDOW_TITLE = "Aikaterine";

    /** The root component being drawn. */
    private UIComponent root;

    /** The GLFW window ID. */
    private long windowId;

    public Window(UIComponent root)
    {
        this.root = root;
    }

    public void create()
    {
        // Initialize GLFW
        glfwInit();
        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, VERSION_MAJOR);
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, VERSION_MINOR);
        glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
		glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE);

        // Create and configure this window
        this.windowId = glfwCreateWindow(MINIMUM_WITDH, MINIMUM_WITDH,
                                         WINDOW_TITLE, NULL, NULL);
        glfwSetWindowSizeLimits(this.windowId, MINIMUM_WITDH, MINIMUM_HEIGHT,
                                GLFW_DONT_CARE, GLFW_DONT_CARE);
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
