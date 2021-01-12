package com.autumnara.aikaterine.model;

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
  * <p> This runnable begins a model manager when it's {@link #run}
  * method is called. The model favors the high throughput required to
  * perform certain calculations.
  */
public final class ModelRunnable
implements Runnable
{

    /** Message queue for messages sent from the UI.
      */
    private final BlockingQueue<Object> fromUI;

    /** Message queue for messages sent to the UI.
      */
    private final BlockingQueue<Object> toUI;

    /** Constructor for a runnable execution thread.
      *
      * @param fromUI message queue for messages sent from the UI
      * @param toUI   message queue for messages sent to the UI
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
