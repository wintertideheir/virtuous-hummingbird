package com.autumnara.aikaterine.view;

import java.util.concurrent.BlockingQueue;

public final class ViewRunnable
implements Runnable
{

    private final BlockingQueue<Object> fromPresenter;

    private final BlockingQueue<Object> toPresenter;

    /** Constructor for a runnable execution thread  
      */
    public ViewRunnable(BlockingQueue<Object> fromPresenter,
                        BlockingQueue<Object> toPresenter)
    {
        this.fromPresenter = fromPresenter;
        this.toPresenter   = toPresenter;
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
