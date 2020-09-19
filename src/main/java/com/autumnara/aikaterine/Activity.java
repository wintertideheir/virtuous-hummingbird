package com.autumnara.com;

import java.io.Serializable;

/** An activity which can be performed to further a virtue. */
class Activity implements Serializable {

    /** The name of the activity. */
    public String name;
    /** The description of the activity. Should describe in two
        sentences what is done and why it's done. */
    public String description;
    /** Instructions for performing this activity. May be as long as a
        few paragraphs, although it ought to be as brief as possible.
        Anything longer ought to be broken up, recombined, or
        reduced. */
    public String instructions;

    /** Constructor for an activity. */
    public Activity(String name,
                    String description,
                    String instructions)
    {
        this.name         = name;
        this.description  = description;
        this.instructions = instructions;
    }

}
