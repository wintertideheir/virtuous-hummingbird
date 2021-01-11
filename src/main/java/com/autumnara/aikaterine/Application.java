package com.autumnara.aikaterine;

import com.autumnara.aikaterine.model.ModelRunnable;
import com.autumnara.aikaterine.presenter.PresenterRunnable;
import com.autumnara.aikaterine.view.ViewRunnable;

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

        BlockingQueue<Object> modelToPresenterQueue = new ArrayBlockingQueue<>(Application.QUEUE_CAPACITY);
        BlockingQueue<Object> presenterToModelQueue = new ArrayBlockingQueue<>(Application.QUEUE_CAPACITY);
        BlockingQueue<Object> viewToPresenterQueue  = new ArrayBlockingQueue<>(Application.QUEUE_CAPACITY);
        BlockingQueue<Object> presentertoViewQueue  = new ArrayBlockingQueue<>(Application.QUEUE_CAPACITY);

        new Thread(new ModelRunnable(presenterToModelQueue,
                                     modelToPresenterQueue)).start();
        new Thread(new ViewRunnable(presentertoViewQueue,
                                    viewToPresenterQueue)).start();
        new PresenterRunnable(modelToPresenterQueue,
                              presenterToModelQueue,
                              viewToPresenterQueue,
                              presentertoViewQueue).run();

    }

}
