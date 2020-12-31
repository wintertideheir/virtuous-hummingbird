package com.autumnara.aikaterine;

import com.badlogic.gdx.backends.lwjgl.LwjglApplication;
import com.badlogic.gdx.backends.lwjgl.LwjglApplicationConfiguration;

public class ApplicationStarter
{

    public static void main(String[] argv)
    {
        LwjglApplicationConfiguration config = new LwjglApplicationConfiguration();

        // Request OpenGL version 3.3
        config.gles30ContextMajorVersion = 3;
        config.gles30ContextMinorVersion = 3;

        // Request a 600x400 window
        config.width = 600;
        config.height = 400;

        // Center the window
        config.x = -1;
        config.y = -1;

        // Set the window title
        config.title = "Aikaterine";

        // Enable window resizing
        config.resizable = true;

        new LwjglApplication(new Application(), config);
    }

}
