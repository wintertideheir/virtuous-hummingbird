package com.autumnara.aikaterine.presenter;

import java.util.concurrent.BlockingQueue;

public final class PresenterRunnable
implements Runnable
{

    private final BlockingQueue<Object> fromModel;

    private final BlockingQueue<Object> toModel;

    private final BlockingQueue<Object> fromView;

    private final BlockingQueue<Object> toView;

    /** Constructor for a runnable execution thread  
      */
    public PresenterRunnable(BlockingQueue<Object> fromModel,
                             BlockingQueue<Object> toModel,
                             BlockingQueue<Object> fromView,
                             BlockingQueue<Object> toView)
    {
        this.fromModel = fromModel;
        this.toModel   = toModel;
        this.fromView  = fromView;
        this.toView    = toView;
    }

    @Override
    public void run() {}

}
