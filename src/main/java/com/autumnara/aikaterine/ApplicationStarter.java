package com.autumnara.aikaterine;

import com.badlogic.gdx.backends.lwjgl3.Lwjgl3Application;
import com.badlogic.gdx.backends.lwjgl3.Lwjgl3ApplicationConfiguration;

public class ApplicationStarter
{

    public static void main(String[] argv)
    {
        Lwjgl3ApplicationConfiguration config = new Lwjgl3ApplicationConfiguration();

        // Don't use OpenGL ES. Request OpenGL 3.3
        config.useOpenGL3(false, 3, 3);

        // Set the window title
        config.setTitle("Aikaterine");

        new Lwjgl3Application(new Application(), config);
    }

}
