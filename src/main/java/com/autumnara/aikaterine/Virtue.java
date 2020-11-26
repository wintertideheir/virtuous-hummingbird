package com.autumnara.aikaterine;

import java.io.Serializable;

/** A virtue. A trait or quality deemed morally good or desirable.
    Anything belonging with a possessive. */
public class Virtue implements Serializable {

    /** The name of the virtue. Ought to be a few words for higher
        virtues. */
    protected String name;
    /** A description of the virtue. Ought to be a sentence for higher
        virtues. */
    protected String description;

    /** The distance of of the virtue to the root virtue, when drawn.
        */
    public transient float distance = 0;
    /** The angle of the virtue relative to an arbitrary ray from the
        root virtue. */
    public transient float angle = 0;

    public Virtue(String name,
                  String description)
    {
        this.name        = name;
        this.description = description;
    }

    public String getName()
    {
        return this.name;
    }

    public String getDescription()
    {
        return this.description;
    }

    /** Override of the equality method that only compared the name of
        the virtue, which ought to be unique in a single virtue tree.
        */
    @Override
    public boolean equals(Object obj)
    {
        if (this == obj)
        {
            return true;
        }
        if (null == obj)
        {
            return false;
        }
        if (obj instanceof Virtue)
        {
            return ((Virtue) obj).name == this.name;
        }
        return false;
    }

    /** Override of hash method to reflect the overrided equality
        method. */
    @Override
    public int hashCode()
    {
        return this.name.hashCode();
    }

}
