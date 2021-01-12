package com.autumnara.aikaterine.ui;

import java.util.concurrent.BlockingQueue;

public final class UIRunnable
implements Runnable
{

    private final BlockingQueue<Object> fromModel;

    private final BlockingQueue<Object> toModel;

    /** Constructor for a runnable execution thread  
      */
    public UIRunnable(BlockingQueue<Object> fromModel,
                      BlockingQueue<Object> toModel)
    {
        this.fromModel = fromModel;
        this.toModel   = toModel;
    }

    @Override
    public void run()
    {
        Window window = new Window();
        window.initialize();
        window.loop();
        window.terminate();
    }

}
