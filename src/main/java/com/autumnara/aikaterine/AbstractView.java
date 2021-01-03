package com.autumnara.aikaterine;

public abstract class AbstractView extends AbstractResource
{

    private Viewport viewport;

    public AbstractView(Viewport viewport)
    {
        this.setViewport(viewport);
    }

    public final void render()
    {
        this.assertInitialized();
        this.viewport.activate();
        this.onRender();
    }

    protected abstract void onRender();

    public final void setViewport(Viewport viewport)
    {
        if (viewport.isRoot)
        {
            throw new IllegalArgumentException("View cannot be initialized with a viewport belonging to a window.");
        }

        this.viewport = viewport;

        this.onSetViewport();
    }

    protected final int getViewportWidth()
    {
        return this.viewport.width;
    }

    protected final int getViewportHeight()
    {
        return this.viewport.height;
    }

}
