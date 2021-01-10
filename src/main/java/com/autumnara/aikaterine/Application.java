package com.autumnara.aikaterine;

import com.autumnara.aikaterine.model.ModelRunnable;
import com.autumnara.aikaterine.presenter.PresenterRunnable;
import com.autumnara.aikaterine.view.ViewRunnable;

public final class Application
{

    /** Constructor for an Application.
      *
      * This constructor is private to prevent the instantiation of
      * this class'. 
      */
    private Application() {}

    public static void main(String[] args)
    {

        Thread modelThread = new Thread(new ModelRunnable(), "modelThread");
        Thread viewThread  = new Thread(new ViewRunnable(),  "viewThread");

        new PresenterRunnable().run();

    }

}
