package com.autumnara.aikaterine;

public abstract class AbstractView extends AbstractResource
{

    public final void render()
    {
        this.assertInitialized();
        this.onRender();
    }

    protected abstract void onRender();

}
