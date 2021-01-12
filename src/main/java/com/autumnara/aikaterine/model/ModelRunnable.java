package com.autumnara.aikaterine.model;

import java.util.concurrent.BlockingQueue;

public final class ModelRunnable
implements Runnable
{

    private final BlockingQueue<Object> fromUI;

    private final BlockingQueue<Object> toUI;

    /** Constructor for a runnable execution thread  
      */
    public ModelRunnable(BlockingQueue<Object> fromUI,
                         BlockingQueue<Object> toUI)
    {
        this.fromUI = fromUI;
        this.toUI   = toUI;
    }

    @Override
    public void run() {}

}
