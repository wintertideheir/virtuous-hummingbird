package com.autumnara.aikaterine.model;

import java.util.concurrent.BlockingQueue;

public final class ModelRunnable
implements Runnable
{

    private final BlockingQueue<Object> fromPresenter;

    private final BlockingQueue<Object> toPresenter;

    /** Constructor for a runnable execution thread  
      */
    public ModelRunnable(BlockingQueue<Object> fromPresenter,
                         BlockingQueue<Object> toPresenter)
    {
        this.fromPresenter = fromPresenter;
        this.toPresenter   = toPresenter;
    }

    @Override
    public void run() {}

}
