package com.autumnara.aikaterine;

/** An abstract view.
  *
  * The view is one component of the model-view-presenter (MVP) model.
  * It's responsibility is to draw the model to the screen and pass
  * input to the presenter.
  */
public abstract class AbstractView extends AbstractResource
{

    /** This view's OpenGL viewport.
      */
    private Viewport viewport;

    /** A promise for this view's OpenGL viewport.
      */
    private Promise<Viewport> viewportPromise;

    /** Constructor for a view.
      *
      * This constructor ensures that the viewport is promised before
      * the view can be initialized.
      *
      * @param viewportPromise A promised OpenGL non-root viewport to
      *                        draw this view through
      */
    public AbstractView(Promise<Viewport> viewportPromise)
    {
        this.viewportPromise = viewportPromise;
    }

    /** Get the viewport.
      *
      * This method will get the viewport from a viewport promise if it
      * doesn't exist.
      */
    protected final Viewport getViewport()
    {
        if (this.viewport == null)
        {
            this.viewport = this.viewportPromise.get();

            this.viewportPromise = null;

            if (this.viewport.isRoot)
            {
                throw new IllegalArgumentException("View cannot use a viewport belonging to a window.");
            }
        }

        return this.viewport;
    }

    /** Render this view.
      *
      * This method makes sure this view is initialized, activates the
      * viewport, and then calls {#onRender}.
      */
    public final void render()
    {
        this.assertInitialized();
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
      * @param viewport the new viewport
      */
    public final void setViewport(Viewport viewport)
    {
        if (viewport.isRoot)
        {
            throw new IllegalArgumentException("View cannot use a viewport belonging to a window.");
        }

        this.viewport = viewport;

        this.onSetViewport();
    }

    /** Adjust the view to the viewport.
      */
    protected abstract void onSetViewport();

}
