package com.autumnara.aikaterine;

import com.autumnara.aikaterine.model.ModelRunnable;
import com.autumnara.aikaterine.ui.UIRunnable;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ArrayBlockingQueue;

public final class Application
{

    /** The capacity of
      * {@link java.util.concurrent.ArrayBlockingQueue queue's}
      * used for component communication.
      */
    private final static int QUEUE_CAPACITY = 16;

    /** Constructor for an Application.
      *
      * This constructor is private to prevent the instantiation of
      * this class'. 
      */
    private Application() {}

    public static void main(String[] args)
    {

        BlockingQueue<Object> modelToUIQueue = new ArrayBlockingQueue<>(Application.QUEUE_CAPACITY);
        BlockingQueue<Object> UIToModelQueue = new ArrayBlockingQueue<>(Application.QUEUE_CAPACITY);

        Runnable modelRunnable = new ModelRunnable(modelToUIQueue,
                                                   UIToModelQueue);
        Runnable uiRunnable = new UIRunnable(UIToModelQueue,
                                             modelToUIQueue);

        new Thread(modelRunnable, "Model").start();
        uiRunnable.run();
    }

}
