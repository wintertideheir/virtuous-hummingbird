package com.autumnara.hummingbird.ui;

import java.util.concurrent.BlockingQueue;

/** A runnable to manage the model.
  *
  * <p> The model-view-presenter pattern divides a program with a UI
  * into three components based on seperation of concerns. These
  * components can be seperated by language-level safety constructs
  * like packages, and they can be seperated into threads. The latter
  * allows for each component to independently favor throughput or
  * latency.
  *
  * <p> This runnable begins a UI manager when it's {@link #run} method
  * is called. The UI consists of the presenter and view. The UI
  * requires a low latency for responsiveness.
  */
public final class UIRunnable
implements Runnable
{

    /** Message queue for messages sent from the model.
      */
    private final BlockingQueue<Object> fromModel;

    /** Message queue for messages sent to the model.
      */
    private final BlockingQueue<Object> toModel;

    /** Constructor for a runnable execution thread.
      *
      * @param fromModel message queue for messages sent from the model
      * @param toModel   message queue for messages sent to the model
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
        window.open();
        window.loop();
        window.close();
    }

}
