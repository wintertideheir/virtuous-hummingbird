package com.autumnara.aikaterine;

import org.lwjgl.opengl.GL;

import static org.lwjgl.glfw.GLFW.*;
import static org.lwjgl.opengl.GL11.*;
import static org.lwjgl.system.MemoryUtil.*;

public class App {

    public static void main(String[] args)
    {
        final int DEFAULT_WITDH  = 600;
        final int DEFAULT_HEIGHT = 400;

        // Setup GLFW

        glfwInit();
        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
        glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
		glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE);

        // Create window

        long window = glfwCreateWindow(DEFAULT_WITDH, DEFAULT_HEIGHT,
                                       "Aikaterine", NULL, NULL);
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
