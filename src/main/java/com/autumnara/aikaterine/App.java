package com.autumnara.aikaterine;

public final class App {

    public static void main(String[] args)
    {
        UIComponent root = new UIComponent();
        Window window = new Window(600, 400, "Aikaterine", root);

        window.create();
        window.loop();
        window.destroy();
    }

}
