package com.autumnara.aikaterine;

import java.util.ArrayList;

/** A virtue. A trait or quality deemed morally good or desirable.
    Anything belonging with a possessive. Virtues are implicitly part
    of a heirarchical tree, the virtue tree, where directed links
    between nodes indicate which virtue is pursued for the sake of the
    other. This relationship can be viewed as relating parts to the
    whole. */
public class Virtue {

    /** The name of the virtue. Ought to be a few words for higher
        virtues. */
    public String            name;
    /** A description of the virtue. Ought to be a sentence for higher
        virtues. */
    public String            description;
    /** The virtue for which we are pursuing this virtue. */
    public Virtue            higher;
    /** The virtues which are pursued for this virtue. */
    public ArrayList<Virtue> lower;

    /** Constructor for a virtue given a name and description. Intended
        for user interfaces where the virtue is constructed before
        being placed. */
    public Virtue(String name,
                  String description)
    {
        this.name        = name;
        this.description = description;
        this.higher      = null;
        this.lower       = new ArrayList<Virtue>();
    }

    /** Constructor for a virtue given a name, description, and higher
        virtue. Intended for the creation of new virtues in user
        interfaces. */
    public Virtue(String name,
                  String description,
                  Virtue higher)
    {
        this.name        = name;
        this.description = description;
        this.higher      = higher;
        this.lower       = new ArrayList<Virtue>();
    }

    /** Complete constructor for a quality. */
    public Virtue(String            name,
                  String            description,
                  Virtue            higher,
                  ArrayList<Virtue> lower)
    {
        this.name        = name;
        this.description = description;
        this.higher      = higher;
        this.lower       = lower;
    }

    /** Constructor for the highest virtue from Aristotlean virtue
        ethics, εὐδαιμονία, often anglicized to eudiamonia. I literally
        copied the name and description from Wikipedia. */
    public static Virtue constructHighest()
    {
        return new Virtue("εὐδαιμονία",
                          "Flourishing, prosperity, or blessedness");
    }

}
