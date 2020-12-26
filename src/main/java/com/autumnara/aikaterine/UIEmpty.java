package com.autumnara.aikaterine;

/** A component that does nothing.
    Useful for prototyping or debugging. */
public final class UIEmpty extends UIComponent
{

    @Override
    protected void onInitialize() {}

    @Override
    protected void onDraw() {}

    @Override
    protected void onBounded() {}

    @Override
    protected void onTerminate() {}

}