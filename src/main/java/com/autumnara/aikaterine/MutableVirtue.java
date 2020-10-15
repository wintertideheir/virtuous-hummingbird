package com.autumnara.aikaterine;

import com.autumnara.aikaterine.Virtue;

/** A mutable {@link com.autumnara.aikaterine.Virtue}. */
public class MutableVirtue extends Virtue {

    public MutableVirtue(String name,
                         String description)
    {
        super(name, description);
    }

    public void setName(String name)
    {
        this.name = name;
    }

    public void setDescription(String description)
    {
        this.description = description;
    }

}
