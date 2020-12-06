package com.autumnara.aikaterine;

import org.lwjgl.opengl.GL;

import static org.lwjgl.glfw.GLFW.*;
import static org.lwjgl.opengl.GL11.*;
import static org.lwjgl.system.MemoryUtil.*;

public final class App {

    /** The minimum major OpenGL version. */
    public final static int VERSION_MAJOR = 3;

    /** The minimum minor OpenGL version. */
    public final static int VERSION_MINOR = 3;

    /** The default width of the window. */
    public final static int DEFAULT_WITDH = 600;

    /** The default height of the window. */
    public final static int DEFAULT_HEIGHT = 400;

    /** The title of the window. */
    public final static String WINDOW_TITLE = "Aikaterine";

    public static void main(String[] args)
    {
        // Setup GLFW

        glfwInit();
        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, VERSION_MAJOR);
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, VERSION_MINOR);
        glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
		glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE);

        // Create window

        long window = glfwCreateWindow(DEFAULT_WITDH, DEFAULT_HEIGHT,
                                       WINDOW_TITLE, NULL, NULL);
        glfwMakeContextCurrent(window);

        // Setup OpenGL

        GL.createCapabilities();

        glViewport(0, 0, DEFAULT_WITDH, DEFAULT_HEIGHT);
        glfwSetFramebufferSizeCallback(window, (window_param, width, height) ->
        {
            glViewport(0, 0, width, height);
        });
        glClearColor(0.0f, 0.0f, 0.0f, 0.0f);

        // Loop!

        while (!glfwWindowShouldClose(window))
        {
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
			glfwSwapBuffers(window);
			glfwPollEvents();
		}

        // Close GLFW

		glfwTerminate();
    }

}
