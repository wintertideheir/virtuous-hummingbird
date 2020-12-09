package com.autumnara.aikaterine;

import java.nio.IntBuffer;

import org.lwjgl.opengl.GL;
import static org.lwjgl.glfw.GLFW.*;
import static org.lwjgl.opengl.GL33.*;
import static org.lwjgl.system.MemoryUtil.NULL;
import static org.lwjgl.BufferUtils.*;

public final class App {

    /** The minimum major OpenGL version. */
    public final static int VERSION_MAJOR = 3;

    /** The minimum minor OpenGL version. */
    public final static int VERSION_MINOR = 3;

    /** The default width of the window. */
    public final static int MINIMUM_WITDH = 600;

    /** The default height of the window. */
    public final static int MINIMUM_HEIGHT = 400;

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

        long window = glfwCreateWindow(MINIMUM_WITDH, MINIMUM_WITDH,
                                       WINDOW_TITLE, NULL, NULL);
        glfwMaximizeWindow(window);
        glfwMakeContextCurrent(window);

        // Setup OpenGL

        GL.createCapabilities();

        IntBuffer pixels_width = createIntBuffer(1);
        IntBuffer pixels_height = createIntBuffer(1);
        glfwGetFramebufferSize​(window, pixels_width, pixels_height);

        glViewport(0, 0, pixels_width.get(0), pixels_height.get(0));
        glfwSetFramebufferSizeCallback(window, (window_param, width, height) ->
        {
            glViewport(0, 0, width, height);
        });
        glClearColor(0.0f, 0.0f, 0.0f, 0.0f);

        // Setup rendering data

        final float[] RECTANGLE = {
             1f,  1f,
             1f, -1f,
            -1f,  1f, 
             1f, -1f,
            -1f, -1f,
            -1f,  1f,
        };
        int rectangleVBO = glGenBuffers();
        glBindBuffer(GL_ARRAY_BUFFER, rectangleVBO);
        glBufferData(rectangleVBO, RECTANGLE, GL_STATIC_DRAW);

        final String RECTANGLE_VERT_SHADER_SRC =
              "#version 330 core"
            + "layout (location = 0) in vec3 pos;"

            + "void main()"
            + "{"
            + "    gl_Position = vec4(pos.x, pos.y, 0.0, 1.0);"
            + "}";
        int rectangleVertexShader = glCreateShader(GL_VERTEX_SHADER);
        glShaderSource(rectangleVertexShader, RECTANGLE_VERT_SHADER_SRC);
        glCompileShader(rectangleVertexShader);

        final String RECTANGLE_FRAG_SHADER_SRC =
              "#version 330 core"
            + "out vec4 FragColor;"

            + "void main()"
            + "{"
            + "    FragColor = vec4(1.0f, 1.0f, 1.0f, 1.0f);"
            + "}";
        int rectangleFragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
        glShaderSource(rectangleFragmentShader, RECTANGLE_FRAG_SHADER_SRC);
        glCompileShader(rectangleFragmentShader);

        int rectangleShaderProgram = glCreateProgram();
        glAttachShader(rectangleShaderProgram, rectangleVertexShader);
        glAttachShader(rectangleShaderProgram, rectangleFragmentShader);
        glLinkProgram(rectangleShaderProgram);
        glDeleteShader(rectangleVertexShader);
        glDeleteShader(rectangleFragmentShader);
        glUseProgram(rectangleShaderProgram);

        glVertexAttribPointer(0, 2, GL_FLOAT, false, 3 * 4, NULL);
        glEnableVertexAttribArray(0);  

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
