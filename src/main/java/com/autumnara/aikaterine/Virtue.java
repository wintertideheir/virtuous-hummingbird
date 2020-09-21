package com.autumnara.aikaterine;

import com.autumnara.aikaterine.IllegalCycleException;
import com.autumnara.aikaterine.DuplicateElementException;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;

/** A virtue. A trait or quality deemed morally good or desirable.
    Anything belonging with a possessive. */
public class Virtue implements Serializable {

    /** The name of the virtue. Ought to be a few words for higher
        virtues. */
    public String            name;
    /** A description of the virtue. Ought to be a sentence for higher
        virtues. */
    public String            description;

    /** Constructor for a virtue given a name and description. */
    public Virtue(String name,
                  String description)
    {
        this.name        = name;
        this.description = description;
    }

}
