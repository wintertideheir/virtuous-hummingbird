package com.autumnara.hummingbird.ui;

import com.autumnara.hummingbird.shared.AbstractResource;

/** An abstract view.
  *
  * The view is one component of the model-view-presenter (MVP) model.
  * It's responsibility is to draw the model to the screen and pass
  * input to the presenter.
  */
abstract class AbstractView extends AbstractResource
{

    /** This view's OpenGL viewport.
      */
    protected Viewport viewport;

    /** Render this view.
      *
      * This method makes sure this view is active, activates the
      * viewport, and then calls {#onRender}.
      */
    final void render()
    {
        this.assertActive();
        this.viewport.activate();
        this.onRender();
    }

    /** Render this view.
      *
      * Subclasses should provide rendering code here.
      */
    protected abstract void onRender();

    /** Set the viewport.
      *
      * This function will call {@link #onSetViewport} the after the
      * first time the viewport has been set. This function should be
      * called at least once before {@link #initialize}.
      *
      * @param viewport the new viewport
      */
    final void setViewport(Viewport viewport)
    {
        if (this.isActive())
        {
            this.viewport = viewport;
            this.onSetViewport();
        } else {
            this.viewport = viewport;
        }
    }

    /** Adjust the view to the viewport.
      */
    protected abstract void onSetViewport();

}
