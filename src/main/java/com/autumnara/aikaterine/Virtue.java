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

    /** Constructor for a virtue given a name and description. */
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

}
