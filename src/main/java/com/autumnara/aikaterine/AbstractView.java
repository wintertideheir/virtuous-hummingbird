package com.autumnara.aikaterine;

/** An abstract view.
  *
  * The view is one component of the model-view-presenter (MVP) model.
  * It's responsibility is to draw the model to the screen and pass
  * input to the presenter.
  */
public abstract class AbstractView extends AbstractResource
{

    /** The OpenGL viewport this view should be drawn through.
      */
    private Viewport viewport;

    /** Constructor for a view.
      *
      * This constructor ensures that the viewport is set before the
      * view can be initialized.
      *
      * @param viewport The OpenGL non-root viewport to draw this view
      *                 through 
      */
    public AbstractView(Viewport viewport)
    {
        if (viewport.isRoot)
        {
            throw new IllegalArgumentException("View cannot be initialized with a viewport belonging to a window.");
        }

        this.viewport = viewport;
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

    /** Get the width of the viewport.
      */
    protected final int getViewportWidth()
    {
        return this.viewport.width;
    }

    /** Get the height of the viewport.
      */
    protected final int getViewportHeight()
    {
        return this.viewport.height;
    }

}
