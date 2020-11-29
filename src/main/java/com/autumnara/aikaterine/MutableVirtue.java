package com.autumnara.aikaterine;

import com.autumnara.aikaterine.Virtue;

/** A mutable {@link com.autumnara.aikaterine.Virtue}. */
public class MutableVirtue extends Virtue {

    public MutableVirtue(String  name,
                         String  description,
                         boolean primary)
    {
        super(name, description, primary);
    }

    public void setName(String name)
    {
        this.name = name;
    }

    public void setDescription(String description)
    {
        this.description = description;
    }

    public void setPrimary(boolean primary)
    {
        this.primary = primary;
    }

}
