package com.autumnara.aikaterine;

public final class App {

    public static void main(String[] args)
    {
        UIComponent root = new UIEmpty();
        Window window = new Window(600, 400, "Aikaterine", root);

        window.initialize();
        window.loop();
        window.terminate();
    }

}
